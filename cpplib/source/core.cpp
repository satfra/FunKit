#include "core.hpp"

#include "exceptions.hpp"
#include <algorithm>
#include <iostream>
#include <limits>
#include <numeric>

namespace FunKit
{
  void Truncation::update_max_sizes()
  {
    m_max_truncation_size.clear();
    m_max_truncation_size.resize(m_truncation_table.size(), 0);
    for (size_t i = 0; i < m_truncation_table.size(); ++i) {
      // If there are no truncation rules for this type, set the max size to the maximum possible value
      if (m_truncation_table[i].empty()) {
        m_max_truncation_size[i] = std::numeric_limits<KeyT>::max();
        continue;
      }
      for (const auto &rule : m_truncation_table[i]) {
        m_max_truncation_size[i] = std::max(m_max_truncation_size[i], static_cast<KeyT>(rule.size()));
      }
    }
  }

  void Truncation::update_order_truncation_table()
  {
    m_order_truncation_table.clear();
    m_order_truncation_table.resize(m_truncation_table.size() - 1);
    // The first entry corresponds to the Field type, which is handled separately
    for (size_t type_idx = 0; type_idx < m_truncation_table.size() - 1; ++type_idx) {
      if (m_max_truncation_size[1 + type_idx] == 0) {
        // This type is fully truncated, so we add one empty rule
        m_order_truncation_table[type_idx].push_back({std::vector<FieldIdx>{}});
      } else if (m_max_truncation_size[1 + type_idx] < std::numeric_limits<KeyT>::max()) {
        m_order_truncation_table[type_idx].resize(m_max_truncation_size[1 + type_idx]);
        for (Idx order = 1; order <= m_max_truncation_size[1 + type_idx]; ++order) {
          // Filter the truncation rules for the given type index based on the specified order
          std::vector<std::vector<FieldIdx>> filtered_rules;
          for (const auto &rule : m_truncation_table[1 + type_idx]) {
            if (rule.size() == static_cast<size_t>(order)) {
              filtered_rules.push_back(rule);
            }
          }

          // Now build all distinct permutations of the filtered rules
          std::vector<std::vector<FieldIdx>> distinct_permutations;
          for (const auto &rule : filtered_rules) {
            std::vector<FieldIdx> perm = rule;
            do {
              distinct_permutations.push_back(perm);
            } while (std::next_permutation(perm.begin(), perm.end()));
          }

          // If the order is empty, we should add an empty rule to represent the fully truncated case
          if (distinct_permutations.empty()) distinct_permutations.emplace_back();

          m_order_truncation_table[type_idx][order - 1] = std::move(distinct_permutations);
        }
      } else {
        // If it's unrestricted, just keep it empty.
      }
    }
  }

  void Truncation::initialize(const Setup &setup)
  {
    // Wipe the truncation table and resize it to accommodate all object types plus the predefined correlation functions
    m_truncation_table.clear();
    m_max_truncation_size.clear();
    m_order_truncation_table.clear();
    m_truncation_table.resize(setup.objects.size() + 1 + predef_correlation_functions);

    // Precompute all ordered field pairs, used to expand 2pt functions of unrestricted types
    m_all_field_pairs.clear();
    const auto fields = setup.all_fields();
    m_all_field_pairs.reserve(fields.size() * fields.size());
    for (const auto f1 : fields)
      for (const auto f2 : fields)
        m_all_field_pairs.push_back({f1, f2});

    finalized = false;
  }

  void Truncation::add_rule(KeyT type_idx, std::vector<FieldIdx> field_indices)
  {
    if (finalized) loud_throw("Cannot add a rule to a finalized Truncation object.");
    if (m_truncation_table.empty()) loud_throw("truncation table is empty, call initialize() first.");
    for (const auto field_idx : field_indices)
      if (field_idx == AnyField) loud_throw("Truncation rules must not contain AnyField.");
    if (type_idx == ObjectType::Field) {
      if (field_indices.size() != 1) loud_throw("Truncation rule for Field must have exactly one field index.");
      auto &rules = m_truncation_table[0];
      if (std::find(rules.begin(), rules.end(), field_indices) == rules.end())
        rules.emplace_back(std::move(field_indices));
    } else if (type_idx >= 0 && 1 + type_idx < m_truncation_table.size()) {
      // Rules are stored sorted; rules differing only by ordering are the same rule
      std::sort(field_indices.begin(), field_indices.end());
      auto &rules = m_truncation_table[1 + type_idx];
      // An empty rule means "nothing is accepted" and must be the only rule of its type
      if (!rules.empty() && field_indices.empty() != rules.front().empty())
        loud_throw("An empty truncation rule (nothing accepted) cannot be combined with other rules for object type " +
                   std::to_string(type_idx) + ".");
      if (std::find(rules.begin(), rules.end(), field_indices) == rules.end())
        rules.emplace_back(std::move(field_indices));
    } else
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
  }

  void Truncation::finalize()
  {
    if (m_truncation_table.empty()) loud_throw("Truncation is not initialized, call initialize() before finalizing.");
    if (finalized) loud_throw("Truncation object is already finalized.");
    update_max_sizes();
    update_order_truncation_table();
    finalized = true;
  }

  bool Truncation::in_truncation(const Object &object) const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Truncation object.");

    // Extract the type index and field indices from the object, and sort the field indices for comparison
    const KeyT type_idx = object.type;
    std::vector<FieldIdx> field_indices;
    field_indices.reserve(object.legs.size());
    for (const auto &leg : object.legs) {
      if (leg.first == AnyField) return true; // AnyField is always allowed
      field_indices.emplace_back(leg.first);
    }
    std::sort(field_indices.begin(), field_indices.end());

    if (type_idx == ObjectType::Field) {
      if (m_truncation_table[0].empty()) return true; // No truncation rules means all are allowed
      for (const auto &rule : m_truncation_table[0]) {
        if (rule == field_indices) return true;
      }
    } else if (type_idx >= 0 && 1 + type_idx < m_truncation_table.size()) {
      if (m_truncation_table[1 + type_idx].empty()) return true; // No truncation rules means all are allowed
      // Rules are stored sorted, so the query must be sorted as well
      for (const auto &rule : m_truncation_table[1 + type_idx]) {
        if (rule == field_indices) return true;
      }
    } else {
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
    }
    return false;
  }

  Idx Truncation::max_truncation(KeyT type_idx) const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Truncation object.");
    if (type_idx == ObjectType::Field) return 1;
    if (type_idx < 0 || type_idx >= m_truncation_table.size() - 1)
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
    return m_max_truncation_size[1 + type_idx];
  }

  const std::vector<std::vector<FieldIdx>> &Truncation::truncation_rules(KeyT type_idx) const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Truncation object.");
    if (type_idx == ObjectType::Field) return m_truncation_table[0];
    if (type_idx < 0 || type_idx >= m_truncation_table.size() - 1)
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
    return m_truncation_table[1 + type_idx];
  }

  bool Truncation::has_rules(KeyT type_idx, Idx order) const
  {
    const auto &rules = truncation_rules(type_idx, order);
    // An empty list means "unrestricted"; a single empty rule means "nothing accepted at this order"
    return !rules.empty() && !rules.front().empty();
  }

  const std::vector<std::vector<FieldIdx>> &Truncation::truncation_rules(KeyT type_idx, Idx order) const
  {
    static const std::vector<std::vector<FieldIdx>> empty_list_of_empty_list(1, std::vector<FieldIdx>{});
    static const std::vector<std::vector<FieldIdx>> empty_list;
    if (!finalized) loud_throw("Cannot query a non-finalized Truncation object.");
    if (type_idx == ObjectType::Field) {
      if (order == 1)
        return m_truncation_table[0];
      else
        return empty_list_of_empty_list; // Fields only exist at order 1: nothing is accepted
    }
    if (type_idx < 0 || type_idx >= m_truncation_table.size() - 1)
      loud_throw("Unknown object type index: " + std::to_string(type_idx));

    if (order < 1 || order > m_max_truncation_size[1 + type_idx])
      return empty_list_of_empty_list; // Orders outside the truncation: nothing is accepted

    if (m_max_truncation_size[1 + type_idx] == std::numeric_limits<KeyT>::max())
      return empty_list; // Return an empty vector for unrestricted types

    return m_order_truncation_table[type_idx][order - 1];
  }

  const std::vector<std::vector<FieldIdx>> &Truncation::all_field_pairs() const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Truncation object.");
    return m_all_field_pairs;
  }

  void Symmetries::add(Symmetry sym)
  {
    if (finalized) loud_throw("Cannot add a symmetry to a finalized Symmetries object.");
    if (sym.factor != 1 && sym.factor != -1)
      loud_throw("Symmetry factor must be +1 or -1, got " + std::to_string(sym.factor) + ".");
    if (sym.cycles.empty()) loud_throw("A symmetry must contain at least one cycle.");

    // Every label must be a positive external-leg index, every cycle non-trivial,
    // and the cycles of one symmetry must be disjoint (cf. the ContainsAny check
    // in FBuildSymmetryList, AnSEL/Simplify.m).
    std::vector<Idx> seen;
    for (const auto &cycle : sym.cycles) {
      if (cycle.size() < 2) loud_throw("A symmetry cycle must contain at least two legs.");
      for (const Idx label : cycle) {
        if (label <= 0) loud_throw("Symmetry leg labels must be positive, got " + std::to_string(label) + ".");
        if (std::find(seen.begin(), seen.end(), label) != seen.end())
          loud_throw("Symmetry leg label " + std::to_string(label) + " appears in more than one cycle.");
        seen.push_back(label);
      }
    }

    m_symmetries.push_back(std::move(sym));
  }

  void Symmetries::finalize()
  {
    if (finalized) loud_throw("Symmetries object is already finalized.");
    // Drop exact duplicates while preserving order
    std::vector<Symmetry> unique;
    for (auto &sym : m_symmetries)
      if (std::find(unique.begin(), unique.end(), sym) == unique.end()) unique.push_back(std::move(sym));
    m_symmetries = std::move(unique);
    finalized = true;
  }

  bool Symmetries::empty() const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Symmetries object.");
    return m_symmetries.empty();
  }

  std::size_t Symmetries::size() const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Symmetries object.");
    return m_symmetries.size();
  }

  const std::vector<Symmetry> &Symmetries::all() const
  {
    if (!finalized) loud_throw("Cannot query a non-finalized Symmetries object.");
    return m_symmetries;
  }

  std::vector<CompiledSymmetry> Symmetries::build(const Setup &setup, const std::vector<LegT> &external_legs) const
  {
    if (!finalized) loud_throw("Cannot build a non-finalized Symmetries object.");

    // Field carried by an external-leg label; throws on unknown labels and on
    // labels that appear with two different fields (a malformed equation).
    const auto field_of = [&](Idx label) {
      FieldIdx field = AnyField;
      bool found = false;
      for (const auto &leg : external_legs)
        if (std::abs(leg.second) == label) {
          if (found && leg.first != field)
            loud_throw("External leg label " + std::to_string(label) + " carries two different fields (" +
                       setup.idx_to_field(field) + ", " + setup.idx_to_field(leg.first) + ").");
          field = leg.first;
          found = true;
        }
      if (!found)
        loud_throw("Symmetry references label " + std::to_string(label) +
                   ", which is not an external leg of the equation.");
      return field;
    };

    std::vector<CompiledSymmetry> compiled;
    compiled.reserve(m_symmetries.size());
    for (const auto &sym : m_symmetries) {
      CompiledSymmetry cs;
      cs.factor = sym.factor;
      for (const auto &cycle : sym.cycles) {
        // An index-only permutation can only relate equal terms when all legs
        // of the cycle carry the same field.
        const FieldIdx field = field_of(cycle.front());
        for (const Idx label : cycle)
          if (field_of(label) != field)
            loud_throw("Symmetry cycle mixes legs of different fields (" + setup.idx_to_field(field) + ", " +
                       setup.idx_to_field(field_of(label)) + ").");
        // (a b c) -> a->b, b->c, c->a, cf. buildCycle in FBuildSymmetryList.
        for (std::size_t k = 0; k < cycle.size(); ++k)
          cs.rules.emplace_back(cycle[k], cycle[(k + 1) % cycle.size()]);
      }
      compiled.push_back(std::move(cs));
    }
    return compiled;
  }

  std::vector<Symmetry> make_symmetry_list(const Setup &setup, const std::vector<LegT> &derivative_legs)
  {
    for (const auto &leg : derivative_legs) {
      if (leg.first == AnyField) loud_throw("Derivative legs must carry concrete fields, not AnyField.");
      if (leg.second <= 0)
        loud_throw("Derivative leg labels must be positive, got " + std::to_string(leg.second) + ".");
    }

    // Group the labels by field, preserving order of appearance.
    std::vector<std::pair<FieldIdx, std::vector<Idx>>> groups;
    for (const auto &leg : derivative_legs) {
      const auto it = std::find_if(groups.begin(), groups.end(), [&](const auto &g) { return g.first == leg.first; });
      if (it == groups.end())
        groups.push_back({leg.first, {leg.second}});
      else
        it->second.push_back(leg.second);
    }

    // Per-group symmetries, always including the identity (empty cycle set) so
    // the outer product below composes correctly.
    const auto group_symmetries = [&](const std::pair<FieldIdx, std::vector<Idx>> &group) {
      const auto &labels = group.second;
      const std::size_t k = labels.size();
      std::vector<Symmetry> syms = {{{}, 1}};
      if (k < 2) return syms;

      if (setup.field_props(group.first).grassmann) {
        // Grassmann: pairwise swaps only, each with a factor -1.
        for (std::size_t i = 0; i < k; ++i)
          for (std::size_t j = i + 1; j < k; ++j)
            syms.push_back({{{labels[i], labels[j]}}, -1});
        return syms;
      }

      // Commuting: every non-identity permutation, in disjoint-cycle form.
      std::vector<std::size_t> perm(k);
      std::iota(perm.begin(), perm.end(), std::size_t{0});
      while (std::next_permutation(perm.begin(), perm.end())) {
        Symmetry sym{{}, 1};
        std::vector<char> used(k, 0);
        for (std::size_t i = 0; i < k; ++i) {
          if (used[i] || perm[i] == i) continue;
          std::vector<Idx> cycle;
          for (std::size_t j = i; !used[j]; j = perm[j]) {
            used[j] = 1;
            cycle.push_back(labels[j]);
          }
          sym.cycles.push_back(std::move(cycle));
        }
        syms.push_back(std::move(sym));
      }
      return syms;
    };

    // Outer product across the field groups: cycles are unioned (they act on
    // disjoint labels), factors multiply — the symCombine of FMakeSymmetryList.
    std::vector<Symmetry> combined = {{{}, 1}};
    for (const auto &group : groups) {
      const std::vector<Symmetry> subs = group_symmetries(group);
      std::vector<Symmetry> next;
      next.reserve(combined.size() * subs.size());
      for (const auto &a : combined)
        for (const auto &b : subs) {
          Symmetry sym;
          sym.cycles = a.cycles;
          sym.cycles.insert(sym.cycles.end(), b.cycles.begin(), b.cycles.end());
          sym.factor = a.factor * b.factor;
          next.push_back(std::move(sym));
        }
      combined = std::move(next);
    }

    // Drop the identity; the simplify driver always tries it first anyway.
    std::erase_if(combined, [](const Symmetry &sym) { return sym.cycles.empty(); });
    return combined;
  }

  std::string sidx_to_string(KeyT _idx)
  {
    if (_idx == 0) loud_throw("Got zero index while parsing");

    bool pos = _idx > 0;
    KeyT idx = pos ? _idx : -1 * _idx;

    // Just go through the alphabet
    char pref = 'a' + ((idx - 1) % 26);
    std::string out = pos ? "" : "-";
    out += std::string(1, pref);
    if (idx > 26) out += std::to_string(idx - 26);
    return out;
  }

  bool Setup::is_correlationFunction(KeyT type_idx) const { return type_idx >= 0 && type_idx < correlationFunctions; }
  bool Setup::is_orderedObject(KeyT type_idx) const { return type_idx >= 0 && type_idx < orderedObjects; }
  bool Setup::is_indexedObject(KeyT type_idx) const { return type_idx >= -5 && type_idx < indexedObjects; }
  bool Setup::is_nonCommutingObject(KeyT type_idx) const
  {
    return type_idx == ObjectType::Field || type_idx == ObjectType::FDOp || is_correlationFunction(type_idx);
  }

  void Setup::finalize_fields()
  {
    m_field_props.assign(2 * (cFields.size() + gFields.size()), FieldProps{});
    for (Idx i = 0; i < Idx(cFields.size()); ++i) {
      const bool paired = !cFields[i].second.name.empty();
      const bool source = i >= Idx(cFields.size()) - cSourceCount;
      m_field_props[2 * i] = {true, false, source, paired ? FieldIdx(2 * i + 1) : FieldIdx(2 * i)};
      if (paired) m_field_props[2 * i + 1] = {true, false, source, FieldIdx(2 * i)};
    }
    const FieldIdx offset = 2 * cFields.size();
    for (Idx i = 0; i < Idx(gFields.size()); ++i) {
      const bool paired = !gFields[i].second.name.empty();
      const bool source = i >= Idx(gFields.size()) - gSourceCount;
      m_field_props[offset + 2 * i] = {true, true, source,
                                       paired ? FieldIdx(offset + 2 * i + 1) : FieldIdx(offset + 2 * i)};
      if (paired) m_field_props[offset + 2 * i + 1] = {true, true, source, FieldIdx(offset + 2 * i)};
    }
  }

  const FieldProps &Setup::field_props(FieldIdx field_idx) const
  {
    if (m_field_props.size() != 2 * (cFields.size() + gFields.size()))
      loud_throw("Field property table is stale, call finalize_fields() after changing the fields.");
    if (field_idx < 0 || field_idx >= FieldIdx(m_field_props.size()))
      loud_throw("Unknown field index " + std::to_string(field_idx) + ", only have " +
                 std::to_string(m_field_props.size()) + " fields.");
    const auto &props = m_field_props[field_idx];
    if (!props.valid) {
      // The padding index of an unpaired field: fetch the field name for the error message
      const auto &pair = field_idx < 2 * FieldIdx(cFields.size()) ? cFields[field_idx / 2]
                                                                  : gFields[(field_idx - 2 * cFields.size()) / 2];
      loud_throw("Field index " + std::to_string(field_idx) + " is the missing partner of the unpaired field " +
                 pair.first.name + ".");
    }
    return props;
  }

  bool Setup::is_cField(FieldIdx field_idx) const { return !field_props(field_idx).grassmann; }

  bool Setup::is_gField(FieldIdx field_idx) const { return field_props(field_idx).grassmann; }

  bool Setup::is_external_label(Idx label) const
  {
    const Idx name = std::abs(label);
    return std::find(external_labels.begin(), external_labels.end(), name) != external_labels.end();
  }

  bool Setup::is_source(FieldIdx field_idx) const { return field_props(field_idx).source; }

  std::pair<int, FieldIdx> Setup::leg_sort_key(FieldIdx field_idx) const
  {
    if (field_idx == AnyField) return {0, field_idx};
    return {field_props(field_idx).source ? 1 : 0, field_idx};
  }

  FieldIdx Setup::field_to_idx(const std::string &field_name) const
  {
    if (field_name == "") loud_throw("Got empty field name while parsing");
    if (field_name == "AnyField") return AnyField;
    for (FieldIdx i = 0; i < cFields.size(); ++i) {
      if (cFields[i].first.name == field_name) return 2 * i;
      if (cFields[i].second.name == field_name) return 2 * i + 1;
    }
    for (FieldIdx i = 0; i < gFields.size(); ++i) {
      if (gFields[i].first.name == field_name) return 2 * cFields.size() + 2 * i;
      if (gFields[i].second.name == field_name) return 2 * cFields.size() + 2 * i + 1;
    }
    loud_throw("Unknown field name: " + field_name);
  }

  std::string Setup::idx_to_field(FieldIdx field_idx) const
  {
    if (field_idx == AnyField)
      return "AnyField";
    else if (field_idx >= 0 && field_idx < cFields.size() * 2) {
      if (field_idx % 2 == 0) return cFields[field_idx / 2].first.name;
      if (cFields[field_idx / 2].second.name.empty())
        loud_throw("Field index " + std::to_string(field_idx) + " is the missing partner of the unpaired field " +
                   cFields[field_idx / 2].first.name + ".");
      return cFields[field_idx / 2].second.name;
    } else if (field_idx >= 2 * cFields.size() && field_idx < 2 * (cFields.size() + gFields.size())) {
      FieldIdx g_idx = field_idx - 2 * cFields.size();
      if (g_idx % 2 == 0) return gFields[g_idx / 2].first.name;
      if (gFields[g_idx / 2].second.name.empty())
        loud_throw("Field index " + std::to_string(field_idx) + " is the missing partner of the unpaired field " +
                   gFields[g_idx / 2].first.name + ".");
      return gFields[g_idx / 2].second.name;
    }
    loud_throw("Unknown field index: " + std::to_string(field_idx));
  }

  KeyT Setup::type_to_idx(const std::string &type_name) const
  {
    if (type_name == "FDOp") return ObjectType::FDOp;
    if (type_name == "FMinus") return ObjectType::FMinus;
    if (type_name == "Field") return ObjectType::Field;
    if (type_name == "gamma") return ObjectType::gamma;
    if (type_name == "SymmFactor") return ObjectType::SymmFactor;
    if (type_name == "Propagator") return ObjectType::Propagator;
    if (type_name == "GammaN") return ObjectType::GammaN;
    for (KeyT i = 0; i < objects.size(); ++i) {
      if (objects[i] == type_name) return predef_correlation_functions + i;
    }
    loud_throw("Unknown object name: " + type_name);
  }

  Idx Setup::unordered_legs(KeyT type_idx) const
  {
    const KeyT i = type_idx - predef_correlation_functions;
    if (i < 0 || i >= KeyT(unordered_leg_counts.size())) return 0;
    return unordered_leg_counts[i];
  }

  std::string Setup::idx_to_type(KeyT type_idx) const
  {
    if (type_idx == ObjectType::FDOp) return "FDOp";
    if (type_idx == ObjectType::FMinus) return "FMinus";
    if (type_idx == ObjectType::Field) return "Field";
    if (type_idx == ObjectType::gamma) return "gamma";
    if (type_idx == ObjectType::SymmFactor) return "SymmFactor";
    if (type_idx == ObjectType::Propagator) return "Propagator";
    if (type_idx == ObjectType::GammaN) return "GammaN";
    if (type_idx >= predef_correlation_functions && type_idx < predef_correlation_functions + objects.size())
      return objects[type_idx - predef_correlation_functions];
    loud_throw("Unknown object index: " + std::to_string(type_idx));
  }

  bool Setup::has_partner(FieldIdx field_idx) const { return field_props(field_idx).partner != field_idx; }

  FieldIdx Setup::partner_field(FieldIdx field_idx) const
  {
    // The conjugate partner is the other slot of the same pair, or the field itself if unpaired
    return field_props(field_idx).partner;
  }

  Idx Setup::gamma(const LegT &leg1, const LegT &leg2) const
  {
    // Field-space metric, following metric[] in FEDeriK/Metric.m. The index signs encode the positions.
    const FieldIdx f1 = leg1.first, f2 = leg2.first;
    const bool lower1 = leg1.second < 0, lower2 = leg2.second < 0;
    const auto &props2 = field_props(f2);
    const FieldIdx f2p = props2.partner;
    if (f1 != f2 && f1 != f2p) return 0;
    // gamma_a^b = delta_a^b
    if (f1 == f2 && lower1 && !lower2) return 1;
    // gamma^a_b = (-1)^{ab} delta^a_b
    if (f1 == f2 && !lower1 && lower2) return props2.grassmann ? -1 : 1;
    // gamma_ab = gamma^ab: nonzero only between partner fields
    if (f1 == f2p && lower1 == lower2) {
      if (!props2.grassmann) return 1;
      if (f1 == f2) return 1; // an unpaired Grassmann field is its own partner
      // Pairs are stored (antifield, field): (antifield, field) ordering gives +1, the reverse -1
      return f1 % 2 == 0 ? 1 : -1;
    }
    return 0;
  }

  std::vector<FieldIdx> Setup::all_fields() const
  {
    // All valid field indices, skipping the padding indices of unpaired fields
    // and all source fields (sources never enter the AnyField expansion)
    std::vector<FieldIdx> fields;
    fields.reserve(2 * (cFields.size() + gFields.size()));
    for (Idx i = 0; i < Idx(cFields.size()) - cSourceCount; ++i) {
      fields.push_back(2 * i);
      if (!cFields[i].second.name.empty()) fields.push_back(2 * i + 1);
    }
    const FieldIdx offset = 2 * cFields.size();
    for (Idx i = 0; i < Idx(gFields.size()) - gSourceCount; ++i) {
      fields.push_back(offset + 2 * i);
      if (!gFields[i].second.name.empty()) fields.push_back(offset + 2 * i + 1);
    }
    return fields;
  }

  bool has_FDOp(const FTerm &term)
  {
    for (const auto &obj : term)
      if (obj.type == ObjectType::FDOp) return true;
    return false;
  }

  bool has_FDOp(const FEq &feq)
  {
    for (const auto &term : feq)
      if (has_FDOp(term)) return true;
    return false;
  }

  bool has_AnyField(const Object &obj)
  {
    for (const auto &leg : obj.legs)
      if (leg.first == AnyField) return true;
    return false;
  }

  bool has_AnyField(const FTerm &term)
  {
    for (const auto &obj : term)
      if (has_AnyField(obj)) return true;
    return false;
  }
} // namespace FunKit
