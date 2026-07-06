#include "simplify.hpp"

#include <algorithm>
#include <iostream>
#include <numeric>
#include <tuple>
#include <unordered_map>

#include "exceptions.hpp"
#include "transformations.hpp"
#include "truncation.hpp"

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

  void canonicalize_indices(FTerm &term, Idx min_base)
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
    Idx base = std::max<Idx>(1, min_base);
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
    // Common base above every open label of the equation: a symmetry
    // permutation of open labels can then never collide with a closed label,
    // and structurally equal terms get identical closed ranges regardless of
    // which open legs they carry.
    Idx min_base = 1;
    for (const auto &term : feq) {
      gch::small_vector<std::pair<Idx, int>, 16> counts;
      for (const auto &obj : term)
        for (const auto &leg : obj.legs) {
          const Idx name = std::abs(leg.second);
          const auto it =
              std::find_if(counts.begin(), counts.end(), [name](const auto &e) { return e.first == name; });
          if (it == counts.end())
            counts.push_back({name, 1});
          else
            ++it->second;
        }
      for (const auto &[name, count] : counts)
        if (count == 1) min_base = std::max(min_base, name + 1);
    }
    for (auto &term : feq)
      canonicalize_indices(term, min_base);
  }

  namespace
  {
    // Orbits of freely interchangeable open labels (identical commuting
    // derivative legs). Labels not in any orbit are only equal to themselves.
    struct OpenOrbits {
      std::vector<std::pair<Idx, Idx>> label_to_orbit; // sorted by label

      bool empty() const { return label_to_orbit.empty(); }

      // Class of an open label: a shared negative id for orbit members, the
      // label itself otherwise. Classes of different kinds never collide since
      // labels are positive.
      Idx class_of(Idx label) const
      {
        const auto it = std::lower_bound(label_to_orbit.begin(), label_to_orbit.end(), label,
                                         [](const auto &e, Idx l) { return e.first < l; });
        if (it != label_to_orbit.end() && it->first == label) return -1 - it->second;
        return label;
      }
    };

    // (field, position sign, class): the equality class of an open leg under
    // the orbit symmetries. Two open legs may be matched iff their signatures
    // are equal — with empty orbits this is exactly the old exact matching.
    using OpenSig = std::tuple<FieldIdx, Idx, Idx>;

    OpenSig open_sig_of(const LegT &leg, const OpenOrbits &orbits)
    {
      return {leg.first, leg.second > 0 ? 1 : -1, orbits.class_of(std::abs(leg.second))};
    }

    // Per-term lookup tables for the matcher, derived from an FTerm + its
    // TermData (+ the active orbits).
    struct WalkData {
      // cids[o][p]: compact closed-index id of leg p of object o, -1 if open
      std::vector<gch::small_vector<Idx, 4>> cids;
      // ocls[o][p]: open-leg class of leg p (only meaningful where cids == -1)
      std::vector<gch::small_vector<Idx, 4>> ocls;
      // open_sig[o]: sorted open-leg signatures of object o (multiset compare)
      std::vector<gch::small_vector<OpenSig, 4>> open_sig;
      // sorted signatures of ALL open legs, and sorted object keys (cheap rejects)
      std::vector<OpenSig> global_sig;
      std::vector<std::uint64_t> sorted_keys;
    };

    WalkData make_walk_data(const FTerm &term, const TermData &data, const OpenOrbits &orbits)
    {
      WalkData w;
      w.cids.resize(term.size());
      w.ocls.resize(term.size());
      w.open_sig.resize(term.size());
      for (std::size_t o = 0; o < term.size(); ++o) {
        for (const auto &leg : term[o].legs) {
          const Idx name = std::abs(leg.second);
          const auto it = std::lower_bound(data.closed_labels.begin(), data.closed_labels.end(), name);
          if (it != data.closed_labels.end() && *it == name) {
            w.cids[o].push_back(static_cast<Idx>(it - data.closed_labels.begin()));
            w.ocls[o].push_back(0);
          } else {
            const OpenSig sig = open_sig_of(leg, orbits);
            w.cids[o].push_back(-1);
            w.ocls[o].push_back(std::get<2>(sig));
            w.open_sig[o].push_back(sig);
            w.global_sig.push_back(sig);
          }
        }
        std::sort(w.open_sig[o].begin(), w.open_sig[o].end());
      }
      std::sort(w.global_sig.begin(), w.global_sig.end());
      w.sorted_keys = data.obj_keys;
      std::sort(w.sorted_keys.begin(), w.sorted_keys.end());
      return w;
    }

    // The endpoint of a closed index that is NOT the given (object, leg position).
    std::pair<Idx, Idx> other_end(const std::array<std::pair<Idx, Idx>, 2> &ends, Idx obj, Idx pos)
    {
      return ends[0] == std::make_pair(obj, pos) ? ends[1] : ends[0];
    }

    // Grassmann sign of aligning object o2's legs with o1's, given a complete
    // closed-index map imap (compact t1 id -> compact t2 id). Builds the leg
    // correspondence sigma (o1 leg -> o2 leg position) and counts inversions of
    // sigma restricted to Grassmann legs. This equals the sign Mathematica's
    // RearrangeFields accumulates one transposition at a time: each adjacent swap
    // of two Grassmann legs is -1, everything else +1, and the parity of the
    // Grassmann-restricted permutation does not depend on the transposition
    // sequence chosen to realize it. Open legs pair within their equality class;
    // orbit classes only ever contain commuting legs, so the pairing choice
    // cannot affect the sign.
    double object_alignment_sign(const Setup &setup, const Object &o1, const Object &o2,
                                 const gch::small_vector<Idx, 4> &cids1, const gch::small_vector<Idx, 4> &ocls1,
                                 const gch::small_vector<Idx, 4> &cids2, const gch::small_vector<Idx, 4> &ocls2,
                                 const std::vector<Idx> &imap, const TermData &d2)
    {
      const Idx k = static_cast<Idx>(o1.legs.size());
      gch::small_vector<Idx, 4> sigma(k, -1);
      gch::small_vector<char, 4> consumed(o2.legs.size(), 0);

      for (Idx a = 0; a < k; ++a) {
        const LegT &leg1 = o1.legs[a];
        Idx q = -1;
        if (cids1[a] >= 0) {
          // Closed leg: its partner in o2 carries the mapped label. If the label
          // occurs twice within o2 (self-loop), disambiguate by position sign,
          // then by order.
          const Idx label2 = d2.closed_labels[imap[cids1[a]]];
          for (Idx p = 0; p < static_cast<Idx>(o2.legs.size()); ++p)
            if (!consumed[p] && std::abs(o2.legs[p].second) == label2 &&
                (o2.legs[p].second > 0) == (leg1.second > 0)) {
              q = p;
              break;
            }
          if (q < 0)
            for (Idx p = 0; p < static_cast<Idx>(o2.legs.size()); ++p)
              if (!consumed[p] && std::abs(o2.legs[p].second) == label2) {
                q = p;
                break;
              }
        } else {
          // Open leg: the first unconsumed leg of the same equality class
          // (field, position sign, orbit class / exact label).
          for (Idx p = 0; p < static_cast<Idx>(o2.legs.size()); ++p)
            if (!consumed[p] && cids2[p] == -1 && o2.legs[p].first == leg1.first &&
                (o2.legs[p].second > 0) == (leg1.second > 0) && ocls2[p] == ocls1[a]) {
              q = p;
              break;
            }
        }
        if (q < 0) loud_throw("internal error: no aligned leg for a matched object pair");
        consumed[q] = 1;
        sigma[a] = q;
      }

      // Parity of the permutation that reorders o2's legs into alignment,
      // counted on o2's own fields (each adjacent swap of two Grassmann legs of
      // o2 is one CommuteSign factor). Matched legs carry equal fields, so the
      // parity is the same counted on either side.
      double sign = 1;
      for (Idx a = 0; a < k; ++a)
        for (Idx b = a + 1; b < k; ++b)
          if (sigma[a] > sigma[b] && setup.field_props(o2.legs[sigma[a]].first).grassmann &&
              setup.field_props(o2.legs[sigma[b]].first).grassmann)
            sign = -sign;
      return sign;
    }

    // The graph-isomorphism walk over two connected terms (TermsEqualAndSum
    // analog, restructured as backtracking DFS over immutable inputs). All cheap
    // rejects have already run in the caller. Objects are nodes, closed indices
    // are edges; the search builds an object map and a closed-index map, and the
    // Grassmann sign is computed once afterwards from the completed maps.
    std::optional<double> match_connected(const Setup &setup, const FTerm &t1, const TermData &d1, const WalkData &w1,
                                          const FTerm &t2, const TermData &d2, const WalkData &w2)
    {
      const Idx n = static_cast<Idx>(t1.size());
      const Idx m = static_cast<Idx>(d1.closed_labels.size());

      std::vector<Idx> omap(n, -1), imap(m, -1); // t1 object -> t2 object, t1 cid -> t2 cid
      std::vector<char> oused(n, 0), iused(m, 0);
      std::vector<std::pair<Idx, Idx>> stack; // matched pairs not yet fully processed

      // One DFS step: take the top pair, map its first unmapped closed leg to
      // every compatible candidate leg of the partner object, recurse, undo on
      // failure. Enumerating candidates one leg at a time with backtracking
      // covers all branch pairings Mathematica builds explicitly (its Case 3);
      // a pair with no unmapped legs is popped (its Case 2); a single candidate
      // is its Case 1. On failure the state is restored before returning.
      const auto extend = [&](auto &&self) -> bool {
        if (stack.empty()) return true;
        const auto [o1, o2] = stack.back();

        Idx p = -1;
        for (Idx a = 0; a < static_cast<Idx>(t1[o1].legs.size()); ++a)
          if (w1.cids[o1][a] >= 0 && imap[w1.cids[o1][a]] == -1) {
            p = a;
            break;
          }

        if (p == -1) { // fully processed
          stack.pop_back();
          if (self(self)) return true;
          stack.push_back({o1, o2});
          return false;
        }

        const LegT &entry = t1[o1].legs[p];
        const Idx c1 = w1.cids[o1][p];
        const auto [n1, p1] = other_end(d1.adj[c1], o1, p);
        const LegT &end1 = t1[n1].legs[p1];

        gch::small_vector<Idx, 4> tried; // a self-loop label occurs at two legs of o2
        for (Idx q = 0; q < static_cast<Idx>(t2[o2].legs.size()); ++q) {
          const Idx c2 = w2.cids[o2][q];
          if (c2 < 0 || iused[c2]) continue;
          // Matched closed legs must carry the same field at both endpoints.
          // Upper/lower position is ignored (Mathematica strips it via
          // makePosIdx throughout the walk). Deliberately NO field-blind
          // matching: identifying e.g. the two orientations of a fermion loop
          // is the job of explicit symmetry transformations, not of the plain
          // matcher — cross-validated against FSimplify with the Symmetries
          // annotations stripped.
          const LegT &cand = t2[o2].legs[q];
          if (cand.first != entry.first) continue;
          if (std::find(tried.begin(), tried.end(), c2) != tried.end()) continue;
          tried.push_back(c2);

          const auto [n2, p2] = other_end(d2.adj[c2], o2, q);
          const LegT &end2 = t2[n2].legs[p2];
          if (end1.first != end2.first) continue;

          bool pushed = false;
          if (omap[n1] != -1) {
            // Loop closure: both walks must arrive at the same mapped object.
            if (omap[n1] != n2) continue;
          } else {
            if (oused[n2]) continue;
            if (d1.obj_keys[n1] != d2.obj_keys[n2]) continue;
            if (w1.open_sig[n1] != w2.open_sig[n2]) continue;
            omap[n1] = n2;
            oused[n2] = 1;
            stack.push_back({n1, n2});
            pushed = true;
          }
          imap[c1] = c2;
          iused[c2] = 1;

          if (self(self)) return true;

          imap[c1] = -1;
          iused[c2] = 0;
          if (pushed) {
            stack.pop_back();
            omap[n1] = -1;
            oused[n2] = 0;
          }
        }
        return false;
      };

      // Seed at the rarest object key in t1 (fewest candidates in t2, least
      // branching — the StartPoints heuristic), aligned against every compatible
      // t2 object.
      Idx s1 = 0;
      {
        Idx best = n + 1;
        for (Idx i = 0; i < n; ++i) {
          Idx cnt = 0;
          for (Idx j = 0; j < n; ++j)
            cnt += (d1.obj_keys[j] == d1.obj_keys[i]);
          if (cnt < best) {
            best = cnt;
            s1 = i;
          }
        }
      }

      for (Idx s2 = 0; s2 < n; ++s2) {
        if (d2.obj_keys[s2] != d1.obj_keys[s1]) continue;
        if (w2.open_sig[s2] != w1.open_sig[s1]) continue;
        omap[s1] = s2;
        oused[s2] = 1;
        stack.assign(1, {s1, s2});
        if (extend(extend)) {
          // Connected input: the walk must have reached every object.
          if (std::find(omap.begin(), omap.end(), Idx(-1)) != omap.end())
            loud_throw("internal error: incomplete object map on a connected term");
          double sign = 1;
          for (Idx o = 0; o < n; ++o)
            sign *= object_alignment_sign(setup, t1[o], t2[omap[o]], w1.cids[o], w1.ocls[o], w2.cids[omap[o]],
                                          w2.ocls[omap[o]], imap, d2);
          return sign;
        }
        omap[s1] = -1;
        oused[s2] = 0;
        stack.clear();
      }
      return std::nullopt;
    }

    // TermsEqualPre analog over precomputed data: guards, cheap rejects,
    // dispatch. Open legs compare by their equality class — exact labels when
    // no orbits are active.
    std::optional<double> terms_equal_impl(const Setup &setup, const FTerm &t1, const TermData &d1,
                                           const WalkData &w1, const FTerm &t2, const TermData &d2,
                                           const WalkData &w2)
    {
      // Bit-identical object lists are trivially equal — always sound, including
      // for bare-Grassmann terms (a deliberate divergence from Mathematica's
      // blanket GrassmannCount guard, which refuses even t1 === t2).
      if (same_objects(t1, t2)) return 1.;

      // Cheap rejects.
      if (t1.size() != t2.size()) return std::nullopt;
      if (d1.closed_labels.size() != d2.closed_labels.size()) return std::nullopt;
      if (w1.global_sig != w2.global_sig) return std::nullopt;
      if (w1.sorted_keys != w2.sorted_keys) return std::nullopt;
      // No edges to walk and not bit-identical: with orbit matching the open
      // legs may still pair up freely, but only for a single object.
      if (d1.closed_labels.empty()) {
        if (t1.size() != 1) return std::nullopt;
        if (d1.obj_keys[0] != d2.obj_keys[0]) return std::nullopt;
        if (w1.open_sig[0] != w2.open_sig[0]) return std::nullopt;
        std::vector<Idx> imap; // no closed indices
        return object_alignment_sign(setup, t1[0], t2[0], w1.cids[0], w1.ocls[0], w2.cids[0], w2.ocls[0], imap, d2);
      }

      // Disconnected terms need the per-component matcher (Phase 4) — the plain
      // walk would map one component and wrongly declare success. Until it lands,
      // conservatively refuse: unmerged terms are never wrong.
      const bool disc1 = d1.n_components > 1 && t1.size() > 1;
      const bool disc2 = d2.n_components > 1 && t2.size() > 1;
      if (disc1 || disc2) return std::nullopt;

      // Bare-Grassmann guard (mirrors TermsEqualAndSumPre): the walk maps objects
      // regardless of their position in the term product, so the sign of commuting
      // bare Grassmann fields past each other is not tracked. Such terms are only
      // mergeable via the per-component path (Phase 4).
      if (d1.grassmann_field_count != 0 || d2.grassmann_field_count != 0) return std::nullopt;

      return match_connected(setup, t1, d1, w1, t2, d2, w2);
    }
  } // namespace

  std::optional<double> terms_equal(const Setup &setup, const FTerm &t1, const TermData &d1, const FTerm &t2,
                                    const TermData &d2)
  {
    if (has_AnyField(t1) || has_AnyField(t2))
      loud_throw("terms_equal requires fully truncated terms (no AnyField); run truncate first");

    // No orbits: open legs must match exactly.
    const OpenOrbits none;
    const WalkData w1 = make_walk_data(t1, d1, none);
    const WalkData w2 = make_walk_data(t2, d2, none);
    return terms_equal_impl(setup, t1, d1, w1, t2, d2, w2);
  }

  std::optional<double> terms_equal(const Setup &setup, FTerm t1, FTerm t2)
  {
    if (has_AnyField(t1) || has_AnyField(t2))
      loud_throw("terms_equal requires fully truncated terms (no AnyField); run truncate first");

    // Compare the tensor structures only: reset the coefficients, then fold the
    // normalization signs (Grassmann leg sorting) into them. The relative sign
    // of the two structures is n1 * n2 * s, since each n is +-1.
    t1.value = 1;
    t2.value = 1;
    normalize(setup, t1);
    normalize(setup, t2);
    canonicalize_indices(t1);
    canonicalize_indices(t2);

    const TermData d1 = precompute_term_data(setup, t1);
    const TermData d2 = precompute_term_data(setup, t2);
    const auto sign = terms_equal(setup, t1, d1, t2, d2);
    if (!sign) return std::nullopt;
    return t1.value * t2.value * *sign;
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

    // Open / closed split. closed_labels must end up sorted (the matcher resolves
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

  namespace
  {
    // Order-sensitive structural hash over the full object list including leg
    // indices (coefficient excluded). Used by the exact-duplicate pre-pass;
    // always verified with same_objects before merging.
    std::uint64_t term_hash(const FTerm &term)
    {
      std::uint64_t h = static_cast<std::uint64_t>(term.size());
      for (const auto &obj : term) {
        hash_combine(h, to_hashable(obj.type));
        for (const auto &leg : obj.legs) {
          hash_combine(h, to_hashable(leg.first));
          hash_combine(h, to_hashable(leg.second));
        }
      }
      return h;
    }

    // Symmetry-invariant bucket key: like TermData::fingerprint but without
    // the open-leg index labels (their field multiset is kept — a symmetry
    // only permutes labels among same-field legs). Terms related by a
    // symmetry differ exactly in those labels and must land in one bucket,
    // cf. the "Fingerprint is symmetry-invariant" comment in SubFSimplify.
    std::uint64_t symmetry_blind_fingerprint(const TermData &data)
    {
      std::uint64_t fp = 1; // != the seed of the exact fingerprint
      hash_combine(fp, static_cast<std::uint64_t>(data.obj_keys.size()));
      hash_combine(fp, static_cast<std::uint64_t>(data.closed_labels.size()));
      auto sorted_keys = data.obj_keys;
      std::sort(sorted_keys.begin(), sorted_keys.end());
      for (const std::uint64_t key : sorted_keys)
        hash_combine(fp, key);
      for (const auto &leg : data.open_legs) // sorted by (field, idx): fields are in order
        hash_combine(fp, to_hashable(leg.first));
      return fp;
    }

    Idx apply_symmetry(Idx signed_idx, const CompiledSymmetry &sym)
    {
      const Idx name = std::abs(signed_idx);
      for (const auto &[from, to] : sym.rules)
        if (from == name) return signed_idx > 0 ? to : -to;
      return signed_idx;
    }

    // Symmetry-transformed copy for a retry comparison. Rules only permute open
    // labels, and canonicalize_indices(FEq&) keeps all closed labels above every
    // open label, so the TermData (adjacency, object keys, components) stays
    // valid; only the term and its WalkData need rebuilding.
    FTerm apply_symmetry(const FTerm &term, const CompiledSymmetry &sym)
    {
      FTerm ret = term;
      for (auto &obj : ret)
        for (auto &leg : obj.legs)
          leg.second = apply_symmetry(leg.second, sym);
      return ret;
    }
  } // namespace

  void simplify(const Setup &setup, FEq &feq)
  {
    if (setup.debug_level > 0) std::cout << "\n===========> Simplifying..." << std::endl;

    for (const auto &term : feq)
      if (has_AnyField(term))
        loud_throw("simplify requires a fully truncated equation (no AnyField); run truncate first");

    // Bring every term into normal form. prune resolves leftover symbolic
    // factors (gamma/FMinus/SymmFactor — the ReduceIndicesBatch analog) and must
    // run BEFORE canonicalize_indices, since gamma contraction renames indices.
    prune(setup, feq);
    normalize(setup, feq);
    reduce(feq);
    canonicalize_indices(feq);

    const std::size_t before = feq.size();
    const Idx n = static_cast<Idx>(feq.size());
    // Unlike truncate, nothing inside the parallel regions prints, and the
    // leader scan is deterministic regardless of thread count — so parallelism
    // stays on even at higher debug levels.
    const bool parallel = true;

    std::vector<TermData> td(feq.size());
#pragma omp parallel for schedule(dynamic, 64) if (parallel)
    for (Idx i = 0; i < n; ++i)
      td[i] = precompute_term_data(setup, feq[i]);

    // --- Symmetry preparation -------------------------------------------------
    // Gather the equation's external legs once.
    std::vector<LegT> external;
    for (const auto &data : td)
      for (const auto &leg : data.open_legs)
        if (std::find(external.begin(), external.end(), leg) == external.end()) external.push_back(leg);

    // Identical commuting derivative legs form orbits: their labels are freely
    // interchangeable inside the matcher (no sign, no enumeration of the k!
    // permutations). Identical Grassmann derivative legs expand into pair
    // swaps with factor -1 (make_symmetry_list on the Grassmann subset), and
    // explicit Setup::symmetries cycles are compiled as-is; both are handled
    // by transform-and-retry.
    OpenOrbits orbits;
    std::vector<CompiledSymmetry> rules;
    if (!setup.derivatives.empty()) {
      const auto external_field_of = [&](Idx label, FieldIdx expected) {
        bool found = false;
        for (const auto &leg : external)
          if (std::abs(leg.second) == label) {
            if (leg.first != expected)
              loud_throw("Derivative leg " + std::to_string(label) + " carries field " +
                         setup.idx_to_field(leg.first) + " in the equation, not " + setup.idx_to_field(expected) +
                         ".");
            found = true;
          }
        if (!found)
          loud_throw("Derivative leg " + std::to_string(label) + " is not an external leg of the equation.");
      };

      std::vector<std::pair<FieldIdx, std::vector<Idx>>> groups;
      for (const auto &leg : setup.derivatives) {
        if (leg.first == AnyField) loud_throw("Derivative legs must carry concrete fields, not AnyField.");
        const auto it =
            std::find_if(groups.begin(), groups.end(), [&](const auto &g) { return g.first == leg.first; });
        if (it == groups.end())
          groups.push_back({leg.first, {leg.second}});
        else
          it->second.push_back(leg.second);
      }
      std::vector<LegT> grassmann_legs;
      Idx orbit_id = 0;
      for (const auto &[field, labels] : groups) {
        if (setup.field_props(field).grassmann) {
          for (const Idx label : labels)
            grassmann_legs.push_back({field, label});
          continue;
        }
        if (labels.size() < 2) continue;
        for (const Idx label : labels) {
          external_field_of(label, field);
          orbits.label_to_orbit.push_back({label, orbit_id});
        }
        ++orbit_id;
      }
      std::sort(orbits.label_to_orbit.begin(), orbits.label_to_orbit.end());

      if (!grassmann_legs.empty()) {
        Symmetries gsyms;
        for (auto &sym : make_symmetry_list(setup, grassmann_legs))
          gsyms.add(std::move(sym));
        gsyms.finalize();
        rules = gsyms.build(setup, external);
      }
    }
    if (!setup.symmetries.empty()) {
      auto explicit_rules = setup.symmetries.build(setup, external);
      rules.insert(rules.end(), std::make_move_iterator(explicit_rules.begin()),
                   std::make_move_iterator(explicit_rules.end()));
    }
    const bool symmetric = !orbits.empty() || !rules.empty();

    std::vector<WalkData> wd(feq.size());
#pragma omp parallel for schedule(dynamic, 64) if (parallel)
    for (Idx i = 0; i < n; ++i)
      wd[i] = make_walk_data(feq[i], td[i], orbits);

    // --- Bucketing --------------------------------------------------------
    // Terms in different buckets can never merge. With symmetries active the
    // key must be blind to the open-leg labels, since symmetry-related terms
    // differ exactly there.
    std::unordered_map<std::uint64_t, std::vector<Idx>> buckets;
    for (Idx i = 0; i < n; ++i)
      buckets[symmetric ? symmetry_blind_fingerprint(td[i]) : td[i].fingerprint].push_back(i);

    std::vector<char> alive(feq.size(), 1);

    // --- Exact-duplicate pre-pass ------------------------------------------
    // Catches terms the canonical form already made bit-identical (sound for
    // any Grassmann content). One task per bucket; buckets are disjoint index
    // sets, so tasks never touch shared state.
    {
      std::vector<std::vector<Idx> *> work;
      for (auto &[fp, bucket] : buckets)
        if (bucket.size() > 1) work.push_back(&bucket);
#pragma omp parallel shared(feq, alive, work) if (parallel)
#pragma omp single
      for (std::size_t w = 0; w < work.size(); ++w) {
#pragma omp task shared(feq, alive, work) firstprivate(w) if (parallel)
        {
          std::unordered_map<std::uint64_t, Idx> seen;
          for (const Idx i : *work[w]) {
            const std::uint64_t h = term_hash(feq[i]);
            const auto it = seen.find(h);
            if (it != seen.end() && same_objects(feq[it->second], feq[i])) {
              feq[it->second].value += feq[i].value;
              alive[i] = 0;
            } else if (it == seen.end())
              seen.emplace(h, i);
            // hash collision with different objects: left to the leader scan
          }
        }
      }
    }

    // --- Parallel leader scan ------------------------------------------------
    // For every term j, find the first surviving earlier term i of its bucket
    // it matches (identity first, then each symmetry transformation). This is
    // read-only and therefore embarrassingly parallel over j, and independent
    // of the thread count. The merges are applied afterwards in one serial
    // pass: leader chains resolve to their root with composed signs (equality
    // is transitive — t_j = s1 t_i and t_i = s2 t_r give t_j = s1 s2 t_r).
    const auto try_match = [&](Idx i, Idx j) -> std::optional<double> {
      auto sign = terms_equal_impl(setup, feq[i], td[i], wd[i], feq[j], td[j], wd[j]);
      if (sign) return sign;
      for (const auto &sym : rules) {
        const FTerm tj = apply_symmetry(feq[j], sym);
        const WalkData wj = make_walk_data(tj, td[j], orbits);
        sign = terms_equal_impl(setup, feq[i], td[i], wd[i], tj, td[j], wj);
        if (sign) return *sign * sym.factor;
      }
      return std::nullopt;
    };

    std::vector<std::vector<Idx>> scan_lists;
    std::vector<std::pair<std::size_t, std::size_t>> items; // (list, position)
    for (const auto &[fp, bucket] : buckets) {
      std::vector<Idx> alive_terms;
      for (const Idx i : bucket)
        if (alive[i]) alive_terms.push_back(i);
      if (alive_terms.size() < 2) continue;
      scan_lists.push_back(std::move(alive_terms));
      for (std::size_t pos = 1; pos < scan_lists.back().size(); ++pos)
        items.push_back({scan_lists.size() - 1, pos});
    }

    std::vector<Idx> leader(feq.size(), -1);
    std::vector<double> lsign(feq.size(), 1.);
    const std::int64_t n_items = static_cast<std::int64_t>(items.size());
#pragma omp parallel for schedule(dynamic, 8) if (parallel)
    for (std::int64_t k = 0; k < n_items; ++k) {
      const auto &[list, pos] = items[k];
      const Idx j = scan_lists[list][pos];
      for (std::size_t a = 0; a < pos; ++a) {
        const Idx i = scan_lists[list][a];
        const auto sign = try_match(i, j);
        if (sign) {
          leader[j] = i;
          lsign[j] = *sign;
          break;
        }
      }
    }

    // Serial resolve: leaders always point to smaller indices, so one ascending
    // pass accumulates every term into its root with the composed sign.
    std::vector<Idx> root(feq.size());
    std::vector<double> rsign(feq.size(), 1.);
    std::iota(root.begin(), root.end(), Idx(0));
    for (Idx j = 0; j < n; ++j) {
      if (leader[j] == -1) continue;
      root[j] = root[leader[j]];
      rsign[j] = lsign[j] * rsign[leader[j]];
      feq[root[j]].value += rsign[j] * feq[j].value;
      alive[j] = 0;
    }

    // Compact to the surviving terms with non-vanishing coefficients.
    Idx write = 0;
    for (Idx i = 0; i < n; ++i) {
      if (!alive[i] || is_close(feq[i].value, 0.)) continue;
      if (write != i) feq[write] = std::move(feq[i]);
      ++write;
    }
    feq.resize(write);

    if (setup.debug_level > 0)
      std::cout << "Simplified " << before << " terms to " << feq.size() << " terms." << std::endl;
  }
} // namespace FunKit
