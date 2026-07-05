#include "derivatives.hpp"

#include <algorithm>
#include <vector>

#include "core.hpp"
#include "exceptions.hpp"
#include "io.hpp"
#include "transformations.hpp"

#ifdef _OPENMP
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
    result.value = term.value;

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

      if (setup.in_deriv_trunc) {
        // new GammaN has 3 legs
        const auto max_truncation_order = setup.truncation.max_truncation(ObjectType::GammaN);
        if (max_truncation_order < 3) {
          if (setup.debug_level > 1) {
            std::cout << "------ Skipping derivative of ";
            print(setup, obj);
            std::cout << " because GammaN would exceed the truncation order.\n";
          }
          return {};
        }
      }

      // Preallocate the right size
      result.reserve(term.size() - 2 + 5);

      // Construct the new elements
      result.insert(result.end(), term.begin(), term.begin() + fdop_idx); // prefix
      result.value *= -1;                                                 // absorb the first -1

      // (-1)^{bf}: resolves to a numeric sign if both fields are concrete,
      // stays a symbolic FMinus if either is an AnyField
      const auto [sign_bf, fminus_bf] = commute_sign(setup, b, f);
      result.value *= sign_bf;
      if (fminus_bf.type != ObjectType::None) result.push_back(fminus_bf);

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

      if (setup.in_deriv_trunc) {
        // We check if the correlation function would exceed the truncation order after taking the derivative
        const auto corr_func_idx = obj.type;
        const auto corr_func_new_order = term[fdop_idx + 1].legs.size() + 1;
        const auto max_truncation_order = setup.truncation.max_truncation(corr_func_idx);
        if (corr_func_new_order > max_truncation_order) {
          if (setup.debug_level > 1) {
            std::cout << "------ Skipping derivative of object ";
            print(setup, obj);
            std::cout << " because it would exceed the truncation order.\n";
          }
          return {};
        }
      }
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

  FEq resolve_fdop(const Setup &setup, FTerm in_term)
  {
    // Find the last FDOp object in the term
    auto it =
        std::find_if(in_term.rbegin(), in_term.rend(), [](const Object &obj) { return obj.type == ObjectType::FDOp; });

    // No FDOp found, nothing to do
    if (it == in_term.rend()) {
      FEq result;
      result.push_back(std::move(in_term));
      return result;
    }

    // Otherwise, we have found an FDOp object.
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
    if (setup.debug_level > 0) std::cout << "\n===========> Resolving derivatives..." << std::endl;

    while (has_FDOp(feq)) {
      std::vector<FEq> results(feq.size());

      if (setup.debug_level <= 0) {
#pragma omp parallel for schedule(dynamic, 64)
        for (Idx i = 0; i < feq.size(); ++i) {
          results[i] = resolve_fdop(setup, std::move(feq[i]));
        }
      } else {
        for (Idx i = 0; i < feq.size(); ++i) {
          results[i] = resolve_fdop(setup, std::move(feq[i]));
        }
      }

      feq = merge_feq(std::move(results));
    }
    return feq;
  }
} // namespace FunKit
