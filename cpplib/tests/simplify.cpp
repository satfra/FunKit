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

// precompute_term_data caches everything the matcher needs about a term: the
// sorted closed labels with their two (object, leg) endpoints, the sorted open
// legs, per-object content keys, a bucket fingerprint, connected components,
// and the bare-Grassmann-field count. See simplify.hpp.

TEST_CASE("TermData: classification, adjacency, components", "[simplify][termdata]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  // Gamma_{a c d} G^{d c} phi_b: open a=1, b=2, closed c=5, d=6.
  FTerm term;
  term.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 5}, {phi, 6}}});
  term.push_back({ObjectType::Propagator, {{phi, -6}, {phi, -5}}});
  term.push_back({ObjectType::Field, {{phi, 2}}});

  const TermData data = precompute_term_data(setup, term);

  REQUIRE(data.closed_labels == std::vector<Idx>{5, 6});
  // Endpoints of c=5: leg 1 of the GammaN and leg 1 of the Propagator;
  // endpoints of d=6: leg 2 of the GammaN and leg 0 of the Propagator.
  REQUIRE(data.adj.size() == 2);
  REQUIRE(data.adj[0] == std::array<std::pair<Idx, Idx>, 2>{{{0, 1}, {1, 1}}});
  REQUIRE(data.adj[1] == std::array<std::pair<Idx, Idx>, 2>{{{0, 2}, {1, 0}}});
  REQUIRE(data.open_legs == std::vector<LegT>{{phi, 1}, {phi, 2}});
  REQUIRE(data.obj_keys.size() == 3);

  // GammaN and Propagator are joined by c and d; the bare field is a singleton.
  REQUIRE(data.n_components == 2);
  REQUIRE(data.component == std::vector<Idx>{0, 0, 1});
  REQUIRE(data.grassmann_field_count == 0);
}

TEST_CASE("TermData: fingerprint invariance and separation", "[simplify][termdata]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");

  FTerm a;
  a.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 5}, {phi, 6}}});
  a.push_back({ObjectType::Propagator, {{phi, -6}, {phi, -5}}});
  a.push_back({ObjectType::Field, {{phi, 2}}});

  // Same diagram: objects permuted, closed indices relabeled, legs entered in a
  // different order. The fingerprint keys only on index-free content + open legs,
  // so it must not change.
  FTerm b;
  b.push_back({ObjectType::Field, {{phi, 2}}});
  b.push_back({ObjectType::Propagator, {{phi, -8}, {phi, -9}}});
  b.push_back({ObjectType::GammaN, {{phi, 9}, {phi, 8}, {phi, 1}}});

  const TermData da = precompute_term_data(setup, a);
  const TermData db = precompute_term_data(setup, b);
  REQUIRE(da.fingerprint == db.fingerprint);

  // Different field content -> different bucket.
  FTerm c = a;
  c[2].legs[0].first = psi;
  REQUIRE(precompute_term_data(setup, c).fingerprint != da.fingerprint);

  // Different open leg -> different bucket (refinement over the Mathematica fp:
  // external legs of mergeable terms must agree exactly).
  FTerm d = a;
  d[2].legs[0].second = 3;
  REQUIRE(precompute_term_data(setup, d).fingerprint != da.fingerprint);
}

TEST_CASE("TermData: connected term has one component", "[simplify][termdata]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  // A single closed loop: G^{c}{}_{c} traced against a 2-point vertex.
  FTerm term;
  term.push_back({ObjectType::Propagator, {{phi, 1}, {phi, 2}}});
  term.push_back({ObjectType::GammaN, {{phi, -1}, {phi, -2}}});

  const TermData data = precompute_term_data(setup, term);
  REQUIRE(data.n_components == 1);
  REQUIRE(data.component == std::vector<Idx>{0, 0});
  REQUIRE(data.open_legs.empty());
}

TEST_CASE("TermData: product of two loops", "[simplify][termdata]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  // Two disjoint self-contractions.
  FTerm term;
  term.push_back({ObjectType::Propagator, {{phi, 1}, {phi, -1}}});
  term.push_back({ObjectType::Propagator, {{phi, 2}, {phi, -2}}});

  const TermData data = precompute_term_data(setup, term);
  REQUIRE(data.n_components == 2);
  REQUIRE(data.component == std::vector<Idx>{0, 1});
  // Self-loop adjacency: both endpoints on the same object.
  REQUIRE(data.adj[0] == std::array<std::pair<Idx, Idx>, 2>{{{0, 0}, {0, 1}}});
  REQUIRE(data.adj[1] == std::array<std::pair<Idx, Idx>, 2>{{{1, 0}, {1, 1}}});
}

TEST_CASE("TermData: bare Grassmann field count", "[simplify][termdata]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");

  FTerm term;
  term.push_back({ObjectType::Field, {{psi, 1}}});
  term.push_back({ObjectType::Field, {{psibar, 2}}});
  term.push_back({ObjectType::Field, {{phi, 3}}});
  // Grassmann legs inside correlation functions do NOT count — only bare fields.
  term.push_back({ObjectType::Propagator, {{psi, 4}, {psibar, -4}}});
  // AnyField bare fields are skipped: their Grassmann nature is undetermined.
  term.push_back({ObjectType::Field, {{AnyField, 5}}});

  const TermData data = precompute_term_data(setup, term);
  REQUIRE(data.grassmann_field_count == 2);
}

TEST_CASE("TermData: index appearing 3+ times throws", "[simplify][termdata]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  FTerm term;
  term.push_back({ObjectType::GammaN, {{phi, 5}, {phi, 5}, {phi, 5}}});
  REQUIRE_THROWS_AS(precompute_term_data(setup, term), std::runtime_error);
}

TEST_CASE("same_objects ignores the coefficient", "[simplify]")
{
  FTerm a;
  a.push_back({ObjectType::Propagator, {{0, 1}, {0, 2}}});
  a.value = 1.;

  FTerm b = a;
  b.value = -3.5;
  REQUIRE(same_objects(a, b));

  // Any structural difference breaks equality: leg index, field, type, count.
  FTerm c = a;
  c[0].legs[1].second = 3;
  REQUIRE_FALSE(same_objects(a, c));

  FTerm d = a;
  d.push_back({ObjectType::Field, {{0, 3}}});
  REQUIRE_FALSE(same_objects(a, d));
}
