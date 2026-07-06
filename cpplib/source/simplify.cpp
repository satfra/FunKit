#include "simplify.hpp"

#include <algorithm>
#include <numeric>

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

    // Negative values (ObjectType keys, lower indices) widen through int64_t so
    // that the modular conversion to uint64_t is the same on every platform.
    std::uint64_t to_hashable(std::int64_t value) { return static_cast<std::uint64_t>(value); }

    // Hash of (type, sorted field multiset), indices excluded — the fieldKey of
    // StartPoints/PrecomputeTermData. Identifies "a GammaN with fields {phi,psibar,psi}"
    // regardless of index names and leg order.
    std::uint64_t obj_key(const Object &obj)
    {
      gch::small_vector<FieldIdx, 8> fields;
      for (const auto &leg : obj.legs)
        fields.push_back(leg.first);
      std::sort(fields.begin(), fields.end());

      std::uint64_t key = to_hashable(obj.type);
      for (const FieldIdx field : fields)
        hash_combine(key, to_hashable(field));
      return key;
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

  bool same_objects(const FTerm &t1, const FTerm &t2)
  {
    // Slice down to the object vectors so FTerm::value stays out of the comparison.
    return static_cast<const std::vector<Object> &>(t1) == static_cast<const std::vector<Object> &>(t2);
  }

  TermData precompute_term_data(const Setup &setup, const FTerm &term)
  {
    TermData data;
    const Idx n_objs = static_cast<Idx>(term.size());

    // Occurrence scan: for each distinct index name, its count, its (object, leg
    // position) endpoints, and the leg of the first occurrence (used for open legs).
    // Same linear-scanned stack table rationale as in canonicalize_indices.
    struct IdxOcc {
      Idx name;
      int count;
      std::array<std::pair<Idx, Idx>, 2> ends;
      LegT leg;
    };
    gch::small_vector<IdxOcc, 16> occ;
    for (Idx o = 0; o < n_objs; ++o)
      for (Idx l = 0; l < static_cast<Idx>(term[o].legs.size()); ++l) {
        const LegT &leg = term[o].legs[l];
        const Idx name = std::abs(leg.second);
        const auto it = std::find_if(occ.begin(), occ.end(), [name](const IdxOcc &e) { return e.name == name; });
        if (it == occ.end())
          occ.push_back({name, 1, {{{o, l}, {}}}, leg});
        else if (it->count == 1) {
          it->ends[1] = {o, l};
          ++it->count;
        } else
          loud_throw("index " + std::to_string(name) +
                     " appears more than twice — equation malformed or derivatives unresolved");
      }

    // Open / closed split. closed_labels must end up sorted (terms_equal resolves
    // labels to compact ids via lower_bound), so sort the table by name first.
    std::sort(occ.begin(), occ.end(), [](const IdxOcc &a, const IdxOcc &b) { return a.name < b.name; });
    for (const auto &e : occ) {
      if (e.count == 1)
        data.open_legs.push_back(e.leg);
      else {
        data.closed_labels.push_back(e.name);
        data.adj.push_back(e.ends);
      }
    }
    std::sort(data.open_legs.begin(), data.open_legs.end());

    // Per-object content keys.
    data.obj_keys.reserve(term.size());
    for (const auto &obj : term)
      data.obj_keys.push_back(obj_key(obj));

    // Fingerprint: (object count, closed count, sorted obj_keys, sorted open legs).
    // Terms with different fingerprints can never be merged; including the open
    // legs is a strict refinement of the Mathematica fp (external legs of equal
    // terms must agree exactly), which only makes the pairwise buckets smaller.
    std::uint64_t fp = 0;
    hash_combine(fp, static_cast<std::uint64_t>(term.size()));
    hash_combine(fp, static_cast<std::uint64_t>(data.closed_labels.size()));
    auto sorted_keys = data.obj_keys;
    std::sort(sorted_keys.begin(), sorted_keys.end());
    for (const std::uint64_t key : sorted_keys)
      hash_combine(fp, key);
    for (const auto &leg : data.open_legs) {
      hash_combine(fp, to_hashable(leg.first));
      hash_combine(fp, to_hashable(leg.second));
    }
    data.fingerprint = fp;

    // Connected components: union-find over objects, uniting the two endpoints of
    // every closed index. Objects with no closed legs stay singletons.
    std::vector<Idx> parent(n_objs);
    std::iota(parent.begin(), parent.end(), Idx(0));
    const auto root = [&parent](Idx o) {
      while (parent[o] != o) {
        parent[o] = parent[parent[o]]; // path halving
        o = parent[o];
      }
      return o;
    };
    for (const auto &ends : data.adj)
      parent[root(ends[0].first)] = root(ends[1].first);

    // Compact component ids, numbered by first appearance.
    data.component.assign(n_objs, -1);
    std::vector<Idx> root_id(n_objs, -1);
    Idx n_comp = 0;
    for (Idx o = 0; o < n_objs; ++o) {
      const Idx r = root(o);
      if (root_id[r] == -1) root_id[r] = n_comp++;
      data.component[o] = root_id[r];
    }
    data.n_components = n_comp;

    // GrassmannCount analog: bare Field objects carrying a Grassmann field. Used
    // as the merge guard for connected terms and as the component parity in the
    // disconnected matcher. AnyField legs are skipped — their Grassmann nature is
    // undetermined, and terms containing them are refused by the matcher anyway.
    data.grassmann_field_count = 0;
    for (const auto &obj : term)
      if (obj.type == ObjectType::Field)
        for (const auto &leg : obj.legs)
          if (leg.first != AnyField && setup.field_props(leg.first).grassmann) ++data.grassmann_field_count;

    return data;
  }

  void simplify(const Setup &setup, FEq &feq)
  {
    // Implement simplification logic here
  }
} // namespace FunKit