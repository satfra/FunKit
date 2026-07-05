#pragma once

#include <cmath>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "gch/small_vector.hpp"

namespace FunKit
{
  using KeyT = std::int32_t;
  using FieldIdx = KeyT;
  using Idx = std::int32_t;

  static constexpr FieldIdx AnyField = -1;

  struct Field {
    std::string name = "";
    std::vector<std::string> indices;
  };

  // Precomputed per-field-index properties, built by Setup::finalize_fields()
  struct FieldProps {
    bool valid = false; // false for the padding index of an unpaired field
    bool grassmann = false;
    FieldIdx partner = 0; // the conjugate partner, or the field itself if unpaired
  };

  namespace ObjectType
  {
    enum : KeyT {
      None = -10,
      FMinus = -5,
      SymmFactor = -4,
      gamma = -3,
      Field = -2,
      FDOp = -1,
      Propagator = 0,
      GammaN = 1
    };
  }
  constexpr KeyT predef_correlation_functions = 2;

  using LegT = std::pair<FieldIdx, Idx>;

  inline LegT operator-(const LegT &leg) { return std::make_pair(leg.first, -leg.second); }

  std::string sidx_to_string(KeyT _idx);

  struct Setup;
  struct Object;

  class Truncation
  {
  private:
    std::vector<std::vector<std::vector<FieldIdx>>> m_truncation_table;
    std::vector<std::vector<std::vector<std::vector<FieldIdx>>>> m_order_truncation_table;
    std::vector<Idx> m_max_truncation_size;
    std::vector<std::vector<FieldIdx>> m_all_field_pairs;

    void update_max_sizes();
    void update_order_truncation_table();

    bool finalized = false;

  public:
    void initialize(const Setup &setup);
    void add_rule(KeyT type_idx, std::vector<FieldIdx> field_indices);
    void finalize();

    bool in_truncation(KeyT type_idx, const std::vector<FieldIdx> &field_indices) const;
    Idx max_truncation(KeyT type_idx) const;
    const std::vector<std::vector<FieldIdx>> &truncation_rules(KeyT type_idx) const;
    const std::vector<std::vector<FieldIdx>> &truncation_rules(KeyT type_idx, Idx order) const;
    const std::vector<std::vector<FieldIdx>> &all_field_pairs() const;

    friend void print(const Setup &setup, std::ostream &os);
  };

  struct Setup {
    std::string input_file;
    int debug_level = 0;
    std::string outputFile = "";
    bool in_deriv_trunc = true;

    std::vector<std::string> objects;

    Idx correlationFunctions = 2;
    Idx orderedObjects = 2;
    Idx indexedObjects = 2;

    bool is_correlationFunction(KeyT type_idx) const;
    bool is_orderedObject(KeyT type_idx) const;
    bool is_indexedObject(KeyT type_idx) const;
    bool is_nonCommutingObject(KeyT type_idx) const;

    std::vector<std::pair<Field, Field>> cFields;
    std::vector<std::pair<Field, Field>> gFields;

    // Build the per-field property table; must be called after cFields/gFields are filled
    void finalize_fields();
    const FieldProps &field_props(FieldIdx field_idx) const;

    bool is_cField(FieldIdx field_idx) const;
    bool is_gField(FieldIdx field_idx) const;

    bool has_partner(FieldIdx field_idx) const;
    FieldIdx partner_field(FieldIdx field_idx) const;
    Idx gamma(const LegT &leg1, const LegT &leg2) const;

    std::vector<FieldIdx> all_fields() const;

    FieldIdx field_to_idx(const std::string &field_name) const;
    std::string idx_to_field(FieldIdx field_idx) const;

    KeyT type_to_idx(const std::string &type_name) const;
    std::string idx_to_type(KeyT type_idx) const;

    Truncation truncation;

  private:
    std::vector<FieldProps> m_field_props;
  };

  struct Object {
    KeyT type = ObjectType::None;
    gch::small_vector<LegT, 4> legs;
  };

  struct FTerm : std::vector<Object> {
    double value = 1;
  };

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
    return {AnyField, max_idx + 1};
  }

  bool has_FDOp(const FTerm &term);
  bool has_FDOp(const FEq &feq);

  bool has_AnyField(const Object &obj);
  bool has_AnyField(const FTerm &term);

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