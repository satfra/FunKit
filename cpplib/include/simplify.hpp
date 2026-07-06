#pragma once

#include "core.hpp"

#include <array>
#include <optional>

namespace FunKit
{
  // FixIndices analog: renumber the closed (dummy) indices of a term to a
  // compact deterministic range disjoint from the open indices.
  // Precondition: call AFTER normalize (leg order feeds first-appearance order).
  void canonicalize_indices(FTerm &term);
  void canonicalize_indices(FEq &feq);

  // Per-term cached analysis — the PrecomputeTermData analog.
  // Precondition (like canonicalize_indices): every index appears at most twice,
  // i.e. symbolic FMinus/SymmFactor objects have been resolved (prune) first —
  // their legs duplicate indices that already occur elsewhere in the term.
  struct TermData {
    std::vector<Idx> closed_labels; // sorted positive labels of closed indices
    std::vector<LegT> open_legs;    // sorted (field, signed idx) of open legs
    // adjacency: for closed index closed_labels[c], its two endpoints:
    std::vector<std::array<std::pair<Idx, Idx>, 2>> adj; // {object idx, leg pos}
    std::vector<std::uint64_t> obj_keys;                 // per object: hash of (type, sorted fields)
    std::uint64_t fingerprint = 0;
    std::vector<Idx> component; // per object: connected-component id
    Idx n_components = 1;
    Idx grassmann_field_count = 0; // # bare Field objects with a Grassmann field
  };
  TermData precompute_term_data(const Setup &setup, const FTerm &term);

  // Structural equality of the object lists, ignoring the coefficient.
  bool same_objects(const FTerm &t1, const FTerm &t2);

  // TermsEqualPre analog: if t1 and t2 are the same diagram up to closed-index
  // relabeling and leg reordering, returns the relative sign; else nullopt.
  std::optional<double> terms_equal(const Setup &setup, const FTerm &t1, const TermData &d1, const FTerm &t2,
                                    const TermData &d2);
  // Convenience overload for tests/users: copies, normalizes, canonicalizes,
  // precomputes, and dispatches to the connected or disconnected matcher.
  std::optional<double> terms_equal(const Setup &setup, FTerm t1, FTerm t2);

  // FSimplifyNoSym analog. In-place.
  void simplify(const Setup &setup, FEq &feq);
} // namespace FunKit