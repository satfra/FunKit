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

  // Minimum number of terms in a loop before it is worth entering an OpenMP
  // parallel region. Below this, spinning up the (per-process, first-touch)
  // thread pool costs far more than the loop saves -- so small derivations run
  // serially and never pay the pool-creation overhead, which dominates the
  // fixed cost of a C++-backend call for small problems.
  static constexpr Idx FUNKIT_OMP_MIN = 256;

  struct Field {
    std::string name = "";
    std::vector<std::string> indices;
  };

  // Precomputed per-field-index properties, built by Setup::finalize_fields()
  struct FieldProps {
    bool valid = false; // false for the padding index of an unpaired field
    bool grassmann = false;
    bool source = false;  // source fields never enter the AnyField expansion
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

    bool in_truncation(const Object &obj) const;
    // True if this type carries an actual, restricted rule list at this order. False both for
    // unrestricted types (no rules at all) and for orders outside the truncation, so that callers
    // can use it to decide whether expanding over the rules is meaningful.
    bool has_rules(KeyT type_idx, Idx order) const;
    Idx max_truncation(KeyT type_idx) const;
    const std::vector<std::vector<FieldIdx>> &truncation_rules(KeyT type_idx) const;
    const std::vector<std::vector<FieldIdx>> &truncation_rules(KeyT type_idx, Idx order) const;
    const std::vector<std::vector<FieldIdx>> &all_field_pairs() const;

    friend void print(const Setup &setup, std::ostream &os);
  };

  // One user-specified symmetry: disjoint cycles over external-leg labels + a sign.
  // The cycle entries are the positive integer index labels that appear on the
  // equation's external (FDOp) legs; cf. the cycle-notation input of
  // FBuildSymmetryList (AnSEL/Simplify.m).
  struct Symmetry {
    std::vector<std::vector<Idx>> cycles; // each cycle: >=2 external-leg labels
    int factor = 1;                       // +1 or -1

    friend bool operator==(const Symmetry &, const Symmetry &) = default;
  };

  // A symmetry expanded into a directly applicable form: a simultaneous
  // permutation of external-leg labels plus the sign it contributes.
  struct CompiledSymmetry {
    std::vector<std::pair<Idx, Idx>> rules; // label -> label, applied all at once
    double factor = 1;
  };

  class Symmetries
  {
  private:
    std::vector<Symmetry> m_symmetries;
    bool finalized = false;

  public:
    // Structurally validate a single entry and store it, cf. FBuildSymmetryList's
    // per-symmetry checks (AnSEL/Simplify.m). The upper bound on the labels (they
    // must match real external legs) is not known at parse time and is deferred
    // to the later build step.
    void add(Symmetry sym);
    void finalize(); // de-dups and locks the container

    bool empty() const;
    std::size_t size() const;
    const std::vector<Symmetry> &all() const;

    // Bind the label-based cycles to the equation's actual external legs and
    // expand each symmetry into its {label permutation, sign} form — the
    // FBuildSymmetryList analog. Requires the external legs, so it cannot run
    // at parse time. Throws if a cycle references an unknown label or mixes
    // legs of different fields (an index-only permutation across different
    // fields can never produce a matching term, so it indicates user error).
    // The identity is not included; the driver tries it implicitly first.
    std::vector<CompiledSymmetry> build(const Setup &setup, const std::vector<LegT> &external_legs) const;

    friend void print(const Setup &setup, std::ostream &os);
  };

  // FMakeSymmetryList analog: generate the symmetry group of an equation from
  // its derivative list (one (field, open label) leg per derivative). Legs of
  // the same commuting field may be permuted arbitrarily (full S_k, factor +1);
  // legs of the same Grassmann field may only be swapped pairwise, each swap
  // contributing a factor -1. Groups of different fields combine by outer
  // product (union of cycles, product of factors). The identity is omitted.
  // Passing the derivative list is an analytic statement that the underlying
  // functional is (graded-)symmetric in these derivatives — it cannot be
  // inferred from the equation itself.
  std::vector<Symmetry> make_symmetry_list(const Setup &setup, const std::vector<LegT> &derivative_legs);

  struct Setup {
    std::string input_file;
    int debug_level = 0;
    std::string outputFile = "";
    // "json" selects structured JSON output; empty means Mathematica-syntax
    // text, unless outputFile ends in ".json"
    std::string output_format = "";
    bool in_deriv_trunc = true;

    bool do_truncate = true;
    bool do_simplify = true;

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

    // Source fields (external sources, e.g. the BRST sources of an mSTI) are
    // stored as the LAST cSourceCount/gSourceCount unpaired entries of
    // cFields/gFields: they share the whole field-index machinery but are
    // excluded from all_fields(), so AnyField expansion never produces them.
    Idx cSourceCount = 0;
    Idx gSourceCount = 0;

    // Build the per-field property table; must be called after cFields/gFields are filled
    void finalize_fields();
    const FieldProps &field_props(FieldIdx field_idx) const;

    // Index labels that are externally visible: the equation's open legs (the derivative indices
    // and any index left open by the input expression, e.g. the field index of a DSE). They name
    // the result's external legs, so they must survive every rewriting step -- unlike closed
    // labels, which are private to the term and may be renamed at will. Empty when the caller did
    // not declare any, in which case nothing is treated as external.
    std::vector<Idx> external_labels;

    bool is_external_label(Idx label) const;

    bool is_cField(FieldIdx field_idx) const;
    bool is_gField(FieldIdx field_idx) const;
    bool is_source(FieldIdx field_idx) const;

    // Leg sort key mirroring OrderFields/FieldOrderLess (FEDeriK/Ordering.m). Ordinary fields keep
    // their raw field-index order, which already reproduces FunKit's convention: cFields occupy the
    // low indices and carry the highest FieldOrderLess weight, and within a Grassmann pair the
    // anti-field precedes the field. Sources are the exception -- they are appended at the end of
    // cFields/gFields and so sit at LOW raw indices relative to the Grassmann block, while
    // FieldOrderLess gives them the lowest weight of all, i.e. they must sort last. Hence the
    // explicit source rank; cSources before gSources then falls out of the raw index order.
    std::pair<int, FieldIdx> leg_sort_key(FieldIdx field_idx) const;

    bool has_partner(FieldIdx field_idx) const;
    FieldIdx partner_field(FieldIdx field_idx) const;
    Idx gamma(const LegT &leg1, const LegT &leg2) const;

    std::vector<FieldIdx> all_fields() const;

    FieldIdx field_to_idx(const std::string &field_name) const;
    std::string idx_to_field(FieldIdx field_idx) const;

    KeyT type_to_idx(const std::string &type_name) const;
    std::string idx_to_type(KeyT type_idx) const;

    // Per user type: number of trailing legs pinned in place (never reordered
    // by normalize) -- e.g. Phidot's "field" slot. Parallel to `objects`;
    // built-in types always have 0.
    std::vector<Idx> unordered_leg_counts;
    Idx unordered_legs(KeyT type_idx) const;

    Truncation truncation;
    Symmetries symmetries;

    // The derivative legs the equation's FDOps were built from (one (field,
    // open label) leg per derivative) — an analytic statement that the
    // underlying functional is (graded-)symmetric in them. simplify() treats
    // identical commuting legs as freely interchangeable (orbit matching, no
    // enumeration — full S_k would explode for many identical legs) and
    // expands identical Grassmann legs into pair swaps with factor -1.
    std::vector<LegT> derivatives;

  private:
    std::vector<FieldProps> m_field_props;
  };

  struct Object {
    KeyT type = ObjectType::None;
    gch::small_vector<LegT, 4> legs;

    friend bool operator==(const Object &, const Object &) = default;
  };

  struct FTerm : std::vector<Object> {
    double value = 1;
  };

  using FEq = std::vector<FTerm>;

  template <typename... O> LegT fresh_sidx(const FTerm &term, const O &...other)
  {
    // Simply do an std::max over all indices and choose the highest value.
    // Index *names* are the magnitudes — the sign only encodes the leg position — so the
    // max must be taken over |leg.second|. Comparing signed values would miss a name that
    // currently occurs only as a lower index and hand it out again as "fresh".
    Idx max_idx = 0;
    (..., (max_idx = std::max<Idx>(max_idx, std::abs(other.second))));
    // Also iterate over all elements in term
    for (const auto &obj : term)
      for (const auto &leg : obj.legs)
        max_idx = std::max<Idx>(max_idx, std::abs(leg.second));
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