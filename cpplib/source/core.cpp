#include "core.hpp"

#include "exceptions.hpp"

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

  void Truncation::update(const Setup &setup)
  {
    m_truncation_table.clear();
    m_truncation_table.resize(setup.objects.size() + 1 + predef_correlation_functions);
    update_max_sizes();
  }

  void Truncation::add_rule(KeyT type_idx, const std::vector<FieldIdx> &field_indices)
  {
    if (m_truncation_table.empty()) loud_throw("Truncation table is empty, call update() first.");
    if (type_idx == ObjectType::Field)
      m_truncation_table[0].emplace_back(field_indices);
    else if (type_idx >= 0 && 1 + type_idx < m_truncation_table.size())
      m_truncation_table[1 + type_idx].emplace_back(field_indices);
    else
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
    update_max_sizes();
  }

  bool Truncation::in_truncation(KeyT type_idx, const std::vector<FieldIdx> &field_indices) const
  {
    if (m_truncation_table.empty()) loud_throw("Truncation table is empty, call update() first.");
    if (type_idx == ObjectType::Field) {
      if (m_truncation_table[0].empty()) return true; // No truncation rules means all are allowed
      for (const auto &rule : m_truncation_table[0]) {
        if (rule == field_indices) return true;
      }
    } else if (type_idx >= 0 && 1 + type_idx < m_truncation_table.size()) {
      if (m_truncation_table[1 + type_idx].empty()) return true; // No truncation rules means all are allowed
      for (const auto &rule : m_truncation_table[1 + type_idx]) {
        if (rule == field_indices) return true;
      }
    } else {
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
    }
    return false;
  }

  KeyT Truncation::max_truncation(KeyT type_idx) const
  {
    if (m_truncation_table.empty()) loud_throw("Truncation table is empty, call update() first.");
    if (type_idx == ObjectType::Field) return 1;
    if (type_idx < 0 || type_idx >= m_truncation_table.size() - 1)
      loud_throw("Unknown object type index: " + std::to_string(type_idx));
    return m_max_truncation_size[1 + type_idx];
  }

  std::string sidx_to_string(KeyT _idx)
  {
    if (_idx == 0) loud_throw("Got zero index while parsing");

    bool pos = _idx > 0;
    KeyT idx = pos ? _idx : -1 * _idx;

    // Just go through the alphabet
    char pref = 'a' + ((idx - 1) % 26);
    std::string out = std::string(1, pref);
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

  bool Setup::is_cField(KeyT field_idx) const
  {
    if (field_idx < 2 * cFields.size()) {
      if (field_idx % 2 == 1 && cFields[field_idx / 2].second.name.empty())
        loud_throw("Field index " + std::to_string(field_idx) + " is the missing partner of the unpaired field " +
                   cFields[field_idx / 2].first.name + ".");
      return true;
    } else if (field_idx < 2 * cFields.size() + 2 * gFields.size()) {
      const KeyT g_idx = field_idx - 2 * cFields.size();
      if (g_idx % 2 == 1 && gFields[g_idx / 2].second.name.empty())
        loud_throw("Field index " + std::to_string(field_idx) + " is the missing partner of the unpaired field " +
                   gFields[g_idx / 2].first.name + ".");
      return false;
    }
    loud_throw("Unknown field index " + std::to_string(field_idx) + ", only have " +
               std::to_string(2 * cFields.size() + 2 * gFields.size()) + " fields.");
  }

  bool Setup::is_gField(KeyT field_idx) const { return !is_cField(field_idx); }

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
} // namespace FunKit
