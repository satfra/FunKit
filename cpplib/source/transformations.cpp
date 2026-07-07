#include "transformations.hpp"

#include <algorithm>
#include <stdexcept>
#include <vector>

#include "core.hpp"
#include "exceptions.hpp"

namespace FunKit
{
  double normalize(const Setup &setup, Object &obj)
  {
    // Refuse to normalize objects with AnyField legs, since they are symbolic and cannot be resolved
    if (has_AnyField(obj)) return 1;

    // Sort the legs of the object by field index and then by leg index using a
    // bubble sort, so that every swap is a nearest-neighbour transposition. Each
    // such adjacent commutation contributes its own sign factor, which correctly
    // accumulates the Grassmann sign (a non-adjacent swap would pick up signs from
    // every leg it jumps over, not just the swapped pair).
    // Types with unordered trailing legs (e.g. Phidot's "field" slot) keep those
    // legs pinned in place: only the head range takes part in the sort.
    double sign = 1;
    const Idx n = Idx(obj.legs.size());
    const Idx n_sort = n - std::min<Idx>(n, setup.unordered_legs(obj.type));
    if (obj.type != ObjectType::Propagator)
      for (Idx i = 0; i < n_sort; ++i) {
        for (Idx j = 0; j + 1 < n_sort - i; ++j) {
          if (obj.legs[j] > obj.legs[j + 1]) {
            sign *= std::get<0>(commute_sign(setup, obj.legs[j], obj.legs[j + 1]));
            std::swap(obj.legs[j], obj.legs[j + 1]);
          }
        }
      }
    else
      for (Idx i = 0; i < n; ++i) {
        for (Idx j = 0; j + 1 < n - i; ++j) {
          if (obj.legs[j] < obj.legs[j + 1]) {
            sign *= std::get<0>(commute_sign(setup, obj.legs[j], obj.legs[j + 1]));
            std::swap(obj.legs[j], obj.legs[j + 1]);
          }
        }
      }
    return sign;
  }

  void normalize(const Setup &setup, FTerm &fterm)
  {
    // Normalize all Objects inside the FTerm
    for (auto &obj : fterm)
      fterm.value *= normalize(setup, obj);
  }

  void normalize(const Setup &setup, FEq &feq)
  {
    // Normalize all FTerm inside the FEq
    for (auto &fterm : feq)
      normalize(setup, fterm);
  }

  void reduce(FTerm &fterm)
  {
    if (fterm.size() == 0) return;
    if (is_close(fterm.value, 0.)) {
      fterm.clear();
      return;
    }
    // erase all objects of type None
    std::erase_if(fterm, [](const Object &obj) { return obj.type == ObjectType::None; });
  }

  void reduce(FEq &feq)
  {
    // Reduce all FTerm inside the FEq
    for (auto &fterm : feq)
      reduce(fterm);
    // Prune a FEq of all empty FTerm:
    std::erase_if(feq, [](const FTerm &term) { return term.empty(); });
  }

  std::tuple<double, Object> commute_sign(const Setup &setup, const LegT &leg1, const LegT &leg2)
  {
    double value = 1.0;
    Object result;
    if (leg1.first == AnyField || leg2.first == AnyField) {
      result.type = ObjectType::FMinus;
      result.legs = {leg1, leg2};
    } else if (setup.is_gField(leg1.first) && setup.is_gField(leg2.first)) {
      value = -1;
    }
    return std::make_tuple(value, result);
  }

  FTerm &commute_forward(const Setup &setup, FTerm &term, Idx i1)
  {
    if (i1 >= Idx(term.size()) - 1) loud_throw("Can't commute beyond the term!");

    // First, swap the objects
    std::swap(term[i1], term[i1 + 1]);

    const Object &obj1 = term[i1 + 1];
    const Object &obj2 = term[i1];

    // Are these even nonCommuting objects?
    if (!(setup.is_nonCommutingObject(obj1.type) && setup.is_nonCommutingObject(obj2.type))) return term;

    // If not, calculate the effect of commuting
    FTerm commutation_signs;
    commutation_signs.reserve(obj1.legs.size() * obj2.legs.size());
    for (Idx i = 0; i < obj1.legs.size(); ++i) {
      for (Idx j = 0; j < obj2.legs.size(); ++j) {
        auto [value, obj] = commute_sign(setup, obj1.legs[i], obj2.legs[j]);
        commutation_signs.value *= value;
        if (obj.type != ObjectType::None) commutation_signs.push_back(obj);
      }
    }
    reduce(commutation_signs);

    // Insert the commutation signs
    term.insert(term.begin(), commutation_signs.begin(), commutation_signs.end());
    term.value *= commutation_signs.value;
    reduce(term);

    return term;
  }

  FEq merge_feq(std::vector<FEq> &&list)
  {
    size_t total_size = 0;
    for (const auto &feq : list)
      total_size += feq.size();
    FEq result(total_size);

    size_t cur_idx = 0;
    for (size_t i = 0; i < list.size(); ++i) {
      for (size_t j = 0; j < list[i].size(); ++j) {
        result[cur_idx] = std::move(list[i][j]);
        ++cur_idx;
      }
    }
    return result;
  }
} // namespace FunKit