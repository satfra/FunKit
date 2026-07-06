#include <stdexcept>

#include <catch2/catch_test_macros.hpp>

#include "funkit.hpp"

using namespace FunKit;

// canonicalize_indices renumbers only the *closed* (dummy) indices of a term to
// a compact deterministic range that starts just above the largest open index,
// leaving every open (externally visible) label untouched. See simplify.hpp.
//
//   1. Count occurrences of each index *name* |leg.second|.
//   2. count 1 -> open, count 2 -> closed, anything else -> throw.
//   3. base = 1 + max over open legs of |index|   (1 when there are no open legs).
//   4. Walk objects in order, legs in order; the first time a closed label is
//      seen assign it base, base+1, ... and rewrite BOTH occurrences, preserving
//      each occurrence's own sign (upper/lower position).

TEST_CASE("Canonicalize: prompt example", "[simplify][canonicalize]")
{
  // Gamma_{a c d} G^{d c} phi_b  with open a=1, b=2 and closed c=5, d=6.
  // Closed legs carry opposite signs on their two occurrences.
  FTerm term;
  term.push_back({ObjectType::GammaN, {{0, 1}, {0, 5}, {0, 6}}}); // Gamma_{a c d}
  term.push_back({ObjectType::Propagator, {{0, -6}, {0, -5}}});   // G^{d c}
  term.push_back({ObjectType::Field, {{0, 2}}});                  // phi_b

  canonicalize_indices(term);

  // base = 1 + max(|open|) = 1 + max(1, 2) = 3.
  // c (first seen +5 in Gamma) -> 3, d (first seen +6 in Gamma) -> 4.
  // Open a=1, b=2 are never touched.
  REQUIRE(term.size() == 3);
  REQUIRE(term[0].legs == decltype(term[0].legs){{0, 1}, {0, 3}, {0, 4}}); // Gamma_{a 3 4}
  REQUIRE(term[1].legs == decltype(term[1].legs){{0, -4}, {0, -3}});       // G^{4 3}
  REQUIRE(term[2].legs == decltype(term[2].legs){{0, 2}});                 // phi_b
}

TEST_CASE("Canonicalize: relabeling invariance", "[simplify][canonicalize]")
{
  // The same diagram written with different closed labels must canonicalize to
  // the identical bit pattern (this is what lets the hash pre-pass dedupe it).
  FTerm a;
  a.push_back({ObjectType::GammaN, {{0, 1}, {0, 5}, {0, 6}}});
  a.push_back({ObjectType::Propagator, {{0, -6}, {0, -5}}});
  a.push_back({ObjectType::Field, {{0, 2}}});

  FTerm b; // closed labels x=8, y=9 instead of c=5, d=6
  b.push_back({ObjectType::GammaN, {{0, 1}, {0, 8}, {0, 9}}});
  b.push_back({ObjectType::Propagator, {{0, -9}, {0, -8}}});
  b.push_back({ObjectType::Field, {{0, 2}}});

  canonicalize_indices(a);
  canonicalize_indices(b);

  REQUIRE(a.size() == b.size());
  for (std::size_t i = 0; i < a.size(); ++i)
    REQUIRE(a[i] == b[i]);
}

TEST_CASE("Canonicalize: open labels untouched, base above max open", "[simplify][canonicalize]")
{
  // Open indices 10 and 7, one closed index c=3. base = 1 + max(10, 7) = 11.
  FTerm term;
  term.push_back({ObjectType::Propagator, {{0, 10}, {0, 3}}});  // open a=10, closed c=+3
  term.push_back({ObjectType::Propagator, {{0, -3}, {0, 7}}});  // closed c=-3, open b=7

  canonicalize_indices(term);

  REQUIRE(term[0].legs == decltype(term[0].legs){{0, 10}, {0, 11}}); // a untouched, c -> 11
  REQUIRE(term[1].legs == decltype(term[1].legs){{0, -11}, {0, 7}}); // c -> -11, b untouched
}

TEST_CASE("Canonicalize: no open legs -> base is 1", "[simplify][canonicalize]")
{
  // A closed self-contraction with no open legs: base falls back to 1.
  FTerm term;
  term.push_back({ObjectType::Propagator, {{0, 4}, {0, -4}}}); // c appears +4 and -4

  canonicalize_indices(term);

  REQUIRE(term[0].legs == decltype(term[0].legs){{0, 1}, {0, -1}});
}

TEST_CASE("Canonicalize: first occurrence sign is preserved", "[simplify][canonicalize]")
{
  // The closed label c is first encountered in its *upper* (negative) position;
  // the fresh value must inherit that sign, and the partner the opposite one.
  FTerm term;
  term.push_back({ObjectType::Propagator, {{0, 1}, {0, -5}}}); // open a=1, closed c=-5 first
  term.push_back({ObjectType::Propagator, {{0, 5}, {0, 2}}});  // closed c=+5, open b=2

  canonicalize_indices(term);

  // base = 1 + max(1, 2) = 3; first (negative) occurrence -> -3, partner -> +3.
  REQUIRE(term[0].legs == decltype(term[0].legs){{0, 1}, {0, -3}});
  REQUIRE(term[1].legs == decltype(term[1].legs){{0, 3}, {0, 2}});
}

TEST_CASE("Canonicalize: FEq overload canonicalizes each term", "[simplify][canonicalize]")
{
  // Each term is canonicalized independently; two copies of the same diagram
  // written with different closed labels must collapse to the same bit pattern.
  FEq feq;
  feq.push_back({});
  feq[0].push_back({ObjectType::GammaN, {{0, 1}, {0, 5}, {0, 6}}});
  feq[0].push_back({ObjectType::Propagator, {{0, -6}, {0, -5}}});
  feq[0].push_back({ObjectType::Field, {{0, 2}}});
  feq.push_back({});
  feq[1].push_back({ObjectType::GammaN, {{0, 1}, {0, 8}, {0, 9}}});
  feq[1].push_back({ObjectType::Propagator, {{0, -9}, {0, -8}}});
  feq[1].push_back({ObjectType::Field, {{0, 2}}});

  canonicalize_indices(feq);

  REQUIRE(feq.size() == 2);
  REQUIRE(feq[0].size() == feq[1].size());
  for (std::size_t i = 0; i < feq[0].size(); ++i)
    REQUIRE(feq[0][i] == feq[1][i]);
  // ...and the shared canonical form is the expected one.
  REQUIRE(feq[0][0].legs == decltype(feq[0][0].legs){{0, 1}, {0, 3}, {0, 4}});
  REQUIRE(feq[0][1].legs == decltype(feq[0][1].legs){{0, -4}, {0, -3}});
}

TEST_CASE("Canonicalize: index appearing 3+ times throws", "[simplify][canonicalize]")
{
  // Deliberately stricter than Mathematica's even/odd rule: an index appearing
  // three or more times can never be meaningful in a resolved equation.
  FTerm term;
  term.push_back({ObjectType::GammaN, {{0, 5}, {0, 5}, {0, 5}}}); // index 5 appears 3x

  REQUIRE_THROWS_AS(canonicalize_indices(term), std::runtime_error);
}
