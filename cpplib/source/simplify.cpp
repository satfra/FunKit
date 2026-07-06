#include "simplify.hpp"

#include <algorithm>

#include "exceptions.hpp"

namespace FunKit
{
  namespace
  {
    void hash_combine(std::uint64_t &seed, std::uint64_t value)
    {
      // From boost::hash_combine
      seed ^= value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }

  } // namespace

  void canonicalize_indices(FTerm &term)
  {
    // Per-distinct-index table. Terms are small, so a linear-scanned stack vector
    // beats a hashing map: no heap allocation, cache-hot, and every scan is over a
    // handful of entries. `label == 0` is the "not yet assigned" sentinel — fresh
    // labels are always >= base >= 1.
    struct IdxInfo {
      Idx name;
      int count;
      Idx label = 0;
    };
    gch::small_vector<IdxInfo, 16> table;
    const auto find = [&table](Idx name) {
      return std::find_if(table.begin(), table.end(), [name](const IdxInfo &e) { return e.name == name; });
    };

    // Pass 1: tally occurrences of each index *name* |leg.second|.
    for (const auto &obj : term)
      for (const auto &leg : obj.legs) {
        const Idx name = std::abs(leg.second);
        const auto it = find(name);
        if (it == table.end())
          table.push_back({name, 1});
        else
          ++it->count;
      }

    // Classify + validate, and derive base = 1 + max over open (count-1) legs.
    // Open labels are externally visible and never renumbered, so closed labels
    // must start above all of them.
    Idx base = 1;
    for (const auto &e : table) {
      if (e.count == 1)
        base = std::max(base, e.name + 1);
      else if (e.count != 2)
        // Stricter than Mathematica's even/odd rule: 3+ occurrences are never
        // meaningful in a resolved equation.
        loud_throw("index " + std::to_string(e.name) + " appears " + std::to_string(e.count) +
                   " times — equation malformed or derivatives unresolved");
    }

    // Pass 2: walk objects in order, legs in order; the first time a closed label
    // is seen assign it the next fresh value, rewriting both occurrences and
    // preserving each occurrence's own sign. Open legs pass through untouched.
    Idx next = base;
    for (auto &obj : term)
      for (auto &leg : obj.legs) {
        IdxInfo &e = *find(std::abs(leg.second));
        if (e.count != 2) continue; // open leg: untouched
        if (e.label == 0) e.label = next++;
        leg.second = (leg.second > 0 ? 1 : -1) * e.label;
      }
  }

  void canonicalize_indices(FEq &feq)
  {
    for (auto &term : feq)
      canonicalize_indices(term);
  }

  void simplify(const Setup &setup, FEq &feq)
  {
    // Implement simplification logic here
  }
} // namespace FunKit