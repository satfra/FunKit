#pragma once

#include <cmath>
#include <cstdint>
#include <exception>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "gch/small_vector.hpp"

namespace FunKit
{
  using KeyT = std::int32_t;

  using FieldIdx = KeyT;
  using Idx = KeyT;

  static constexpr FieldIdx AnyField = -1;

  struct Field {
    std::string name = "";
    std::vector<std::string> indices;
  };

  namespace ObjectType
  {
    enum : KeyT {
      Numeric = -6,
      FMinus = -5,
      SymmFactor = -4,
      gamma = -3,
      Field = -2,
      FDOp = -1,
      Propagator = 0,
      GammaN = 1
    };
  }
  constexpr KeyT predef_obj = 2;

  std::string sidx_to_string(KeyT _idx)
  {
    if (_idx == 0) throw std::runtime_error("Got zero index while parsing");

    bool pos = _idx > 0;
    KeyT idx = pos ? _idx : -1 * _idx;

    // Just go through the alphabet
    char pref = 'a' + ((idx - 1) % 26);
    std::string out = std::string(1, pref);
    if (idx > 26) out += std::to_string(idx - 26);
    return out;
  }

  struct Setup {
    std::string input_file;
    int debug_level = 0;
    std::string outputFile = "";

    std::vector<std::string> objects;

    Idx correlationFunctions = 2;
    Idx orderedObjects = 2;
    Idx indexedObjects = 2;
    bool is_correlationFunction(KeyT type_idx) const { return type_idx >= 0 && type_idx < correlationFunctions; }
    bool is_orderedObject(KeyT type_idx) const { return type_idx >= 0 && type_idx < orderedObjects; }
    bool is_indexedObject(KeyT type_idx) const { return type_idx >= -5 && type_idx < indexedObjects; }
    bool is_nonCommutingObject(KeyT type_idx) const
    {
      return type_idx == ObjectType::Field || type_idx == ObjectType::FDOp || is_correlationFunction(type_idx);
    }

    std::vector<std::pair<Field, Field>> cFields;
    std::vector<std::pair<Field, Field>> gFields;

    bool is_cField(KeyT field_idx) const
    {
      if (field_idx < 2 * cFields.size())
        return true;
      else if (field_idx < 2 * cFields.size() + 2 * gFields.size())
        return false;
      throw std::runtime_error("Unknown field index " + std::to_string(field_idx) + ", only have " +
                               std::to_string(2 * cFields.size() + 2 * gFields.size()) + " fields.");
    }

    bool is_gField(KeyT field_idx) const { return !is_cField(field_idx); }

    inline FieldIdx field_to_idx(const std::string &field_name) const
    {
      for (FieldIdx i = 0; i < cFields.size(); ++i) {
        if (cFields[i].first.name == field_name) return 2 * i;
        if (cFields[i].second.name == field_name) return 2 * i + 1;
      }
      return AnyField;
    }

    inline std::string idx_to_field(FieldIdx field_idx) const
    {
      if (field_idx == AnyField) return "AnyField";
      if (field_idx >= 0 && field_idx <= cFields.size() * 2) {
        if (field_idx % 2 == 0) return cFields[field_idx / 2].first.name;
        return cFields[field_idx / 2].second.name;
      }
      throw std::runtime_error("Unknown field index: " + std::to_string(field_idx));
    }

    inline KeyT type_to_idx(const std::string &type_name) const
    {
      if (type_name == "number") return ObjectType::Numeric;
      if (type_name == "FDOp") return ObjectType::FDOp;
      if (type_name == "FMinus") return ObjectType::FMinus;
      if (type_name == "Propagator") return ObjectType::Propagator;
      if (type_name == "GammaN") return ObjectType::GammaN;
      for (KeyT i = 0; i < objects.size(); ++i) {
        if (objects[i] == type_name) return predef_obj + i;
      }
      throw std::runtime_error("Unknown object name: " + type_name);
    }

    inline std::string idx_to_type(KeyT type_idx) const
    {
      if (type_idx == ObjectType::Numeric) return "number";
      if (type_idx == ObjectType::FDOp) return "FDOp";
      if (type_idx == ObjectType::FMinus) return "FMinus";
      if (type_idx == ObjectType::Propagator) return "Propagator";
      if (type_idx == ObjectType::GammaN) return "GammaN";
      if (type_idx >= predef_obj && type_idx < objects.size()) return objects[type_idx - predef_obj];
      throw std::runtime_error("Unknown object index: " + std::to_string(type_idx));
    }
  };

  using LegT = std::pair<FieldIdx, Idx>;

  inline LegT operator-(const LegT &leg) { return std::make_pair(leg.first, -leg.second); }

  struct Object {
    KeyT type;
    gch::small_vector<LegT, 4> legs;
    double value = 1;
  };

  using FTerm = std::vector<Object>;
  using FEq = std::vector<FTerm>;

  template <typename... O> LegT fresh_sidx(const FTerm &term, const O &...other)
  {
    // Simply do an std::max over all indices and choose the highest value
    Idx max_idx = -1;
    (..., (max_idx = std::max(max_idx, other.second)));
    // Also iterate over all elements in term
    for (const auto &obj : term)
      for (const auto &leg : obj.legs)
        max_idx = std::max(max_idx, leg.second);
    // Return an AnyField
    return {AnyField, max_idx};
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

  FEq merge_feq(std::vector<FEq> &&list)
  {
    Idx total_size = 0;
    for (const auto &feq : list)
      total_size += feq.size();
    FEq result(total_size);

    Idx cur_idx = 0;
    for (Idx i = 0; i < list.size(); ++i) {
      for (Idx j = 0; j < list[i].size(); ++j) {
        result[cur_idx] = std::move(list[i][j]);
        ++cur_idx;
      }
    }
    return result;
  }

  template <typename T1, typename T2>
    requires(std::is_floating_point<T1>::value && std::is_floating_point<T2>::value)
  bool is_close(T1 a, T2 b)
  {
    using T = decltype(a * b);
    constexpr auto eps_ = std::max(static_cast<T>(std::numeric_limits<T1>::epsilon()),
                                   static_cast<T>(std::numeric_limits<T2>::epsilon()));
    const T1 diff = std::fabs(a - b);
    if (diff <= eps_) return true;
    if (diff <= std::fmax(std::fabs(a), std::fabs(b)) * eps_) return true;
    return false;
  }
} // namespace FunKit