#pragma once

#include <algorithm>
#include <stdexcept>
#include <vector>

#include "core.hpp"
#include "exceptions.hpp"
#include "io.hpp"

#ifdef OPENMP
#include <omp.h>
#endif

namespace FunKit
{
  FTerm functionalD(const Setup &setup, const FTerm &term, Idx fdop_idx)
  {
    // Apply the functional derivative to the object at index idx
    const Object &fdop = term[fdop_idx];
    const Object &obj = term[fdop_idx + 1];

    // Check if the object is a correlation function
    const bool has_derivative = setup.is_correlationFunction(obj.type) || obj.type == ObjectType::Field;
    if (!has_derivative) return {};

    if (setup.debug_level > 1) {
      std::cout << "------ Taking derivative of object ";
      print(setup, obj);
      std::cout << "\n";
    }

    // Fill result with all objects up to idx
    FTerm result;

    // ====================================================================================================
    // If it's a propagator: d/dPhi^f G^ba = (-1)(-1)^{bf}(-1)^{dd}G^{bc}GammaN_{cfd}G^{da}
    // ====================================================================================================
    if (obj.type == ObjectType::Propagator) {
      // We need b,a,f and fresh indices c/d
      const auto b = obj.legs[0];
      const auto a = obj.legs[1];
      const auto f = fdop.legs[0];
      const auto c = fresh_sidx(term, f);
      const auto d = fresh_sidx(term, f, c);

      // Sanity checks
      assert_upper_index(b);
      assert_upper_index(a);
      assert_index(f);

      // Preallocate the right size
      result.reserve(term.size() - 2 + 5);

      // Construct the new elements
      result.insert(result.end(), term.begin(), term.begin() + fdop_idx);   // prefix
      result.push_back({ObjectType::FMinus, {b, f}, -1});                   // absorb the first -1 into (-1)^{bf}
      result.push_back({ObjectType::FMinus, {d, d}});                       // (-1)^{dd}
      result.push_back({ObjectType::Propagator, {b, c}});                   // G^{bc}
      result.push_back({ObjectType::GammaN, {-c, -f, -d}});                 // GammaN_{cfd}
      result.push_back({ObjectType::Propagator, {d, a}});                   // G^{da}
      result.insert(result.end(), term.begin() + fdop_idx + 2, term.end()); // suffix
    }
    // ====================================================================================================
    // It's a field: d/dPhi^a Phi^b -> gamma_{a}^{b}
    // ====================================================================================================
    else if (obj.type == ObjectType::Field) {
      const auto a = fdop.legs[0];
      const auto b = obj.legs[0];

      // Sanity checks
      assert_index(a);
      assert_upper_index(b);

      // Preallocate the right size
      result.reserve(term.size() - 2 + 1);

      // Construct the new elements
      result.insert(result.end(), term.begin(), term.begin() + fdop_idx);   // prefix
      result.push_back({ObjectType::gamma, {-a, b}});                       // gamma^{da}
      result.insert(result.end(), term.begin() + fdop_idx + 2, term.end()); // suffix
    }
    // ====================================================================================================
    // Otherwise, it's a correlation function, i.e. d/dPhi_f F_{ab...} -> F_{fab...}
    // ====================================================================================================
    else {
      const auto f = fdop.legs[0];

      // Sanity checks
      assert_index(f);

      // Preallocate the right size
      result.reserve(term.size() - 1);

      // Construct the new elements
      result.insert(result.end(), term.begin(), term.begin() + fdop_idx);   // prefix
      result.insert(result.end(), term.begin() + fdop_idx + 1, term.end()); // suffix

      // Add the derivative leg to the correlation function
      result[fdop_idx].legs.insert(result[fdop_idx].legs.begin(), -f);
    }

    return result;
  }

  FTerm &reduce(FTerm &fterm)
  {
    if (fterm.size() == 0) return fterm;
    if (fterm.size() == 1) {
      if (is_close(fterm[0].value, 0.)) fterm.clear();
      return fterm;
    }

    long double factor = 1;
    for (auto &object : fterm) {
      factor *= object.value;
      object.value = 1;
    }

    if (is_close(factor, 0.)) {
      fterm.clear();
      return fterm;
    }

    // Then prune all numeric terms:
    std::erase_if(fterm, [](const Object &object) { return object.type == ObjectType::Numeric; });
    // Put the factor into a (new) very first object
    Object object;
    object.type = ObjectType::Numeric;
    object.value = factor;
    fterm.insert(fterm.begin(), object);

    return fterm;
  }

  FEq &reduce(FEq &feq)
  {
    // Reduce all FTerm inside the FEq
    for (auto &fterm : feq)
      fterm = reduce(fterm);
    // Prune a FEq of all empty FTerm:
    std::erase_if(feq, [](const FTerm &term) { return term.empty(); });
    return feq;
  }

  Object commute_sign(const Setup &setup, const LegT &leg1, const LegT &leg2)
  {
    Object result;
    if (leg1.first == AnyField || leg2.first == AnyField) {
      result.type = ObjectType::FMinus;
      result.legs = {leg1, leg2};
    } else if (setup.is_gField(leg1.first) && setup.is_gField(leg2.first)) {
      result.type = ObjectType::Numeric;
      result.value = -1;
    } else {
      result.type = ObjectType::Numeric;
      result.value = 1;
    }
    return result;
  }

  FTerm &commute_forward(const Setup &setup, FTerm &term, Idx i1)
  {
    if (i1 >= Idx(term.size()) - 1) throw std::runtime_error("Can't commute beyond the term!");

    // First, swap the objects
    std::swap(term[i1], term[i1 + 1]);

    const Object &obj1 = term[i1 + 1];
    const Object &obj2 = term[i1];

    // Are these even nonCommuting objects?
    if (!(setup.is_nonCommutingObject(obj1.type) && setup.is_nonCommutingObject(obj2.type))) return term;

    // If not, calculate the effect of commuting
    FTerm commutation_signs(obj1.legs.size() * obj2.legs.size());
    for (Idx i = 0; i < obj1.legs.size(); ++i) {
      for (Idx j = 0; j < obj2.legs.size(); ++j) {
        commutation_signs[i * obj2.legs.size() + j] = commute_sign(setup, obj1.legs[i], obj2.legs[j]);
      }
    }
    commutation_signs = reduce(commutation_signs);

    // Insert the commutation signs
    term.insert(term.begin(), commutation_signs.begin(), commutation_signs.end());

    term = reduce(term);

    return term;
  }

  FEq resolve_fdop(const Setup &setup, FTerm in_term)
  {
    // Find the last FDOp object in the term
    auto it =
        std::find_if(in_term.rbegin(), in_term.rend(), [](const Object &obj) { return obj.type == ObjectType::FDOp; });

    // No FDOp found, nothing to do
    if (it == in_term.rend()) return {std::move(in_term)};

    // Otherwise, we have found an FDOp object. Quick sanity check: it should not have a factor in front of it
    {
      assert_no_factor(*it);
    }

    // Get the index of the FDOp object
    Idx fdop_idx = std::distance(in_term.begin(), it.base()) - 1;

    // Pre-allocate the space we'll need
    FEq result_terms(in_term.size() - 1 - fdop_idx);

    if (setup.debug_level > 1) {
      std::cout << "-- Taking derivative ";
      print(setup, *it);
      std::cout << "\n";
    }

    Idx mem_idx = 0;
    while (fdop_idx < in_term.size() - 1) {

      if (setup.debug_level > 1) {
        std::cout << "---- Taking derivative at idx=" << fdop_idx << "\n";
      }

      result_terms[mem_idx] = functionalD(setup, in_term, fdop_idx);
      ++mem_idx;

      // Commute the FDOp towards the next object
      if (fdop_idx < in_term.size() - 2) {
        in_term = commute_forward(setup, in_term, fdop_idx);

        it = std::find_if(in_term.rbegin(), in_term.rend(),
                          [](const Object &obj) { return obj.type == ObjectType::FDOp; });
        if (setup.debug_level > 1) {
          std::cout << "---- Commuted FDOp forward from position " << fdop_idx << ". New term length is "
                    << in_term.size() << ", new fdop_idx=" << std::distance(in_term.begin(), it.base()) - 1 << "\n";
        }
        fdop_idx = std::distance(in_term.begin(), it.base()) - 1;
      } else {
        // If the FDOp is already at the last element, the next commutation will simply yield a term that evaluates to 0
        break;
      }
    }

    return reduce(result_terms);
  }

  FEq &resolve_derivatives(const Setup &setup, FEq &feq)
  {
    while (has_FDOp(feq)) {
      std::vector<FEq> results(feq.size());
#pragma omp parallel for schedule(dynamic, 64)
      for (Idx i = 0; i < feq.size(); ++i) {
        results[i] = resolve_fdop(setup, std::move(feq[i]));
      }
      feq = merge_feq(std::move(results));
    }
    return feq;
  }
} // namespace FunKit