#include "core.hpp"

#include "exceptions.hpp"
#include <iostream>
#include <limits>

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
      m_field_props[2 * i] = {true, false, paired ? FieldIdx(2 * i + 1) : FieldIdx(2 * i)};
      if (paired) m_field_props[2 * i + 1] = {true, false, FieldIdx(2 * i)};
    }
    const FieldIdx offset = 2 * cFields.size();
    for (Idx i = 0; i < Idx(gFields.size()); ++i) {
      const bool paired = !gFields[i].second.name.empty();
      m_field_props[offset + 2 * i] = {true, true, paired ? FieldIdx(offset + 2 * i + 1) : FieldIdx(offset + 2 * i)};
      if (paired) m_field_props[offset + 2 * i + 1] = {true, true, FieldIdx(offset + 2 * i)};
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
    if (type_name == "Propagator") return ObjectType::Propagator;
    if (type_name == "GammaN") return ObjectType::GammaN;
    for (KeyT i = 0; i < objects.size(); ++i) {
      if (objects[i] == type_name) return predef_correlation_functions + i;
    }
    loud_throw("Unknown object name: " + type_name);
  }

  std::string Setup::idx_to_type(KeyT type_idx) const
  {
    if (type_idx == ObjectType::FDOp) return "FDOp";
    if (type_idx == ObjectType::FMinus) return "FMinus";
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
    std::vector<FieldIdx> fields;
    fields.reserve(2 * (cFields.size() + gFields.size()));
    for (Idx i = 0; i < Idx(cFields.size()); ++i) {
      fields.push_back(2 * i);
      if (!cFields[i].second.name.empty()) fields.push_back(2 * i + 1);
    }
    const FieldIdx offset = 2 * cFields.size();
    for (Idx i = 0; i < Idx(gFields.size()); ++i) {
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
