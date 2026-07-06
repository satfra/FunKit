#include <algorithm>
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

// terms_equal decides whether two terms are the same diagram up to renaming of
// closed indices and reordering of legs within objects, and returns the
// relative Grassmann sign. The convenience overload normalizes and
// canonicalizes copies first, so tests can write terms in any equivalent form.

TEST_CASE("terms_equal: bit-identical terms match with +1", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");

  FTerm t;
  t.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 5}, {phi, 6}}});
  t.push_back({ObjectType::Propagator, {{phi, -6}, {phi, -5}}});

  auto res = terms_equal(setup, t, t);
  REQUIRE(res.has_value());
  REQUIRE(*res == 1.);

  // The fast path is sound even for bare-Grassmann terms: identical strings
  // are trivially equal (deliberate divergence from Mathematica's guard).
  FTerm g;
  g.push_back({ObjectType::Field, {{psi, 5}}});
  g.push_back({ObjectType::GammaN, {{phi, 1}, {setup.field_to_idx("psibar"), -5}}});
  res = terms_equal(setup, g, g);
  REQUIRE(res.has_value());
  REQUIRE(*res == 1.);
}

TEST_CASE("terms_equal: relabeled and permuted tadpole matches with +1", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  FTerm t1;
  t1.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 2}, {phi, 5}, {phi, 6}}});
  t1.push_back({ObjectType::Propagator, {{phi, -5}, {phi, -6}}});

  // Same diagram: objects permuted, closed indices relabeled, legs entered in
  // a different order. Bosonic, so no signs anywhere.
  FTerm t2;
  t2.push_back({ObjectType::Propagator, {{phi, -8}, {phi, -7}}});
  t2.push_back({ObjectType::GammaN, {{phi, 7}, {phi, 8}, {phi, 1}, {phi, 2}}});

  const auto res = terms_equal(setup, t1, t2);
  REQUIRE(res.has_value());
  REQUIRE(*res == 1.);
}

TEST_CASE("terms_equal: swapped Grassmann legs give -1", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");
  const KeyT rdot = setup.type_to_idx("Rdot");

  // G^{ab} Rdot_{ab} with a fermionic loop.
  FTerm t1;
  t1.push_back({ObjectType::Propagator, {{psi, 5}, {psibar, 6}}});
  t1.push_back({rdot, {{psi, -5}, {psibar, -6}}});

  // The same term with the propagator's two Grassmann legs written in swapped
  // order: as a tensor structure this is -t1.
  FTerm t2;
  t2.push_back({ObjectType::Propagator, {{psibar, 6}, {psi, 5}}});
  t2.push_back({rdot, {{psi, -5}, {psibar, -6}}});

  const auto res = terms_equal(setup, t1, t2);
  REQUIRE(res.has_value());
  REQUIRE(*res == -1.);
}

TEST_CASE("terms_equal: the two fermionic flow channels merge with +1", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");

  // Truncating 1/2 G^{AB} Rdot_{BA} yields one bosonic and two fermionic
  // channels; the fermionic pair is the same diagram with the loop index
  // relabeled, i.e. G^{ab} Rdot_{ba} summed in the two possible orders.
  const FEq out = truncate(setup, feq[0]);
  REQUIRE(out.size() == 3);

  std::vector<Idx> fermionic, bosonic;
  for (Idx i = 0; i < Idx(out.size()); ++i) {
    const auto &prop = *std::find_if(out[i].begin(), out[i].end(),
                                     [](const Object &o) { return o.type == ObjectType::Propagator; });
    (setup.is_gField(prop.legs[0].first) ? fermionic : bosonic).push_back(i);
  }
  REQUIRE(fermionic.size() == 2);
  REQUIRE(bosonic.size() == 1);

  const auto res = terms_equal(setup, out[fermionic[0]], out[fermionic[1]]);
  REQUIRE(res.has_value());
  REQUIRE(*res == 1.);

  // Different field content never matches.
  REQUIRE_FALSE(terms_equal(setup, out[bosonic[0]], out[fermionic[0]]).has_value());
}

TEST_CASE("terms_equal: crossed Grassmann edges give -1 through the walk", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");
  const KeyT rdot = setup.type_to_idx("Rdot");

  // A vertex with two psi legs, one connected to a Propagator and one to an
  // Rdot. In t2 the two edges are crossed: the psi leg that reached the
  // Propagator now reaches the Rdot and vice versa. Relabeling t2 back onto t1
  // swaps the vertex's two Grassmann legs, so the terms are equal with sign -1
  // — and here the sign comes from the walk's alignment permutation, not from
  // normalize (both terms are already in canonical leg order).
  FTerm t1;
  t1.push_back({ObjectType::GammaN, {{psi, 5}, {psi, 6}}});
  t1.push_back({ObjectType::Propagator, {{psibar, -5}, {phi, 1}}});
  t1.push_back({rdot, {{phi, 2}, {psibar, -6}}});

  FTerm t2;
  t2.push_back({ObjectType::GammaN, {{psi, 7}, {psi, 8}}});
  t2.push_back({ObjectType::Propagator, {{psibar, -8}, {phi, 1}}});
  t2.push_back({rdot, {{phi, 2}, {psibar, -7}}});

  const auto res = terms_equal(setup, t1, t2);
  REQUIRE(res.has_value());
  REQUIRE(*res == -1.);
}

TEST_CASE("terms_equal: sunset does not match double-bubble", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  // Same fingerprint (two GammaN{phi,phi,phi}, three closed indices, no open
  // legs) but different topology: three parallel edges vs. one edge plus a
  // self-loop on each vertex.
  FTerm sunset;
  sunset.push_back({ObjectType::GammaN, {{phi, 5}, {phi, 6}, {phi, 7}}});
  sunset.push_back({ObjectType::GammaN, {{phi, -5}, {phi, -6}, {phi, -7}}});

  FTerm bubbles;
  bubbles.push_back({ObjectType::GammaN, {{phi, 5}, {phi, -5}, {phi, 7}}});
  bubbles.push_back({ObjectType::GammaN, {{phi, 6}, {phi, -6}, {phi, -7}}});

  REQUIRE_FALSE(terms_equal(setup, sunset, bubbles).has_value());

  // Control: the sunset matches its own permuted, relabeled, scrambled copy —
  // this exercises the backtracking over the 3! pairings of parallel edges.
  FTerm sunset2;
  sunset2.push_back({ObjectType::GammaN, {{phi, -11}, {phi, -9}, {phi, -10}}});
  sunset2.push_back({ObjectType::GammaN, {{phi, 9}, {phi, 10}, {phi, 11}}});

  const auto res = terms_equal(setup, sunset, sunset2);
  REQUIRE(res.has_value());
  REQUIRE(*res == 1.);
}

TEST_CASE("terms_equal: same open legs on different objects do not match", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  // Both terms carry open legs {1, 2} and identical object content, but leg 1
  // sits on the vertex in t1 and on the propagator in t2.
  FTerm t1;
  t1.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 5}}});
  t1.push_back({ObjectType::Propagator, {{phi, 2}, {phi, -5}}});

  FTerm t2;
  t2.push_back({ObjectType::GammaN, {{phi, 2}, {phi, 5}}});
  t2.push_back({ObjectType::Propagator, {{phi, 1}, {phi, -5}}});

  REQUIRE_FALSE(terms_equal(setup, t1, t2).has_value());
}

TEST_CASE("terms_equal: multi-edge bubble matches its permuted copy", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  FTerm t1;
  t1.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 5}, {phi, 6}}});
  t1.push_back({ObjectType::GammaN, {{phi, 2}, {phi, -5}, {phi, -6}}});

  FTerm t2;
  t2.push_back({ObjectType::GammaN, {{phi, 2}, {phi, -7}, {phi, -8}}});
  t2.push_back({ObjectType::GammaN, {{phi, 1}, {phi, 7}, {phi, 8}}});

  const auto res = terms_equal(setup, t1, t2);
  REQUIRE(res.has_value());
  REQUIRE(*res == 1.);
}

TEST_CASE("terms_equal: guards", "[simplify][match]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");

  // AnyField makes equality undecidable: require full truncation first.
  FTerm any;
  any.push_back({ObjectType::Propagator, {{AnyField, 1}, {AnyField, 2}}});
  REQUIRE_THROWS_AS(terms_equal(setup, any, any), std::runtime_error);

  // Connected terms with bare Grassmann fields are refused unless bit-identical:
  // the walk does not track the sign of commuting bare fields past each other.
  FTerm g1;
  g1.push_back({ObjectType::Field, {{psi, 5}}});
  g1.push_back({ObjectType::GammaN, {{phi, 1}, {psibar, -5}}});
  FTerm g2;
  g2.push_back({ObjectType::GammaN, {{phi, 1}, {psibar, -5}}});
  g2.push_back({ObjectType::Field, {{psi, 5}}});
  REQUIRE_FALSE(terms_equal(setup, g1, g2).has_value());

  // Disconnected terms are conservatively refused until the per-component
  // matcher (Phase 4) lands.
  FTerm d1;
  d1.push_back({ObjectType::Propagator, {{phi, 5}, {phi, -5}}});
  d1.push_back({ObjectType::GammaN, {{phi, 6}, {phi, 7}}});
  d1.push_back({ObjectType::Propagator, {{phi, -6}, {phi, -7}}});
  FTerm d2;
  d2.push_back({ObjectType::GammaN, {{phi, 8}, {phi, 9}}});
  d2.push_back({ObjectType::Propagator, {{phi, -8}, {phi, -9}}});
  d2.push_back({ObjectType::Propagator, {{phi, 10}, {phi, -10}}});
  REQUIRE_FALSE(terms_equal(setup, d1, d2).has_value());
}

// simplify(setup, feq) is the driver: prune -> normalize -> canonicalize ->
// fingerprint buckets -> per-bucket merging (exact-duplicate pre-pass + the
// pairwise matcher), then compaction of survivors with non-vanishing
// coefficients.

TEST_CASE("simplify: relabeled copies merge into one term", "[simplify][driver]")
{
  // Scalar fixture: its truncation contains the pure-phi vertices used below
  // (simplify prunes anything outside the truncation).
  auto [setup, feq] = parse(BOILERPLATE_DIR + "scalar.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  // Three copies of the same tadpole: closed indices relabeled, objects
  // permuted, legs scrambled.
  FEq eq;
  eq.push_back({});
  eq[0].push_back({ObjectType::GammaN, {{phi, 1}, {phi, 2}, {phi, 5}, {phi, 6}}});
  eq[0].push_back({ObjectType::Propagator, {{phi, -5}, {phi, -6}}});
  eq[0].value = 0.5;
  eq.push_back({});
  eq[1].push_back({ObjectType::GammaN, {{phi, 1}, {phi, 2}, {phi, 8}, {phi, 7}}});
  eq[1].push_back({ObjectType::Propagator, {{phi, -7}, {phi, -8}}});
  eq[1].value = 0.25;
  eq.push_back({});
  eq[2].push_back({ObjectType::Propagator, {{phi, -9}, {phi, -11}}});
  eq[2].push_back({ObjectType::GammaN, {{phi, 2}, {phi, 11}, {phi, 1}, {phi, 9}}});
  eq[2].value = 0.125;

  simplify(setup, eq);
  REQUIRE(eq.size() == 1);
  REQUIRE(eq[0].value == 0.875);
}

TEST_CASE("simplify: cancelling terms vanish", "[simplify][driver]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "scalar.toml");
  const FieldIdx phi = setup.field_to_idx("phi");

  FEq eq;
  eq.push_back({});
  eq[0].push_back({ObjectType::GammaN, {{phi, 1}, {phi, 5}, {phi, 6}}});
  eq[0].push_back({ObjectType::Propagator, {{phi, -5}, {phi, -6}}});
  eq[0].value = 0.75;
  eq.push_back({});
  eq[1].push_back({ObjectType::Propagator, {{phi, -7}, {phi, -8}}});
  eq[1].push_back({ObjectType::GammaN, {{phi, 1}, {phi, 8}, {phi, 7}}});
  eq[1].value = -0.75;

  simplify(setup, eq);
  REQUIRE(eq.empty());
}

TEST_CASE("simplify: throws on an untruncated equation", "[simplify][driver]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  // The master equation carries AnyField trace legs.
  REQUIRE_THROWS_AS(simplify(setup, feq), std::runtime_error);
}

TEST_CASE("simplify: bare-Grassmann terms merge only when bit-identical", "[simplify][driver]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");

  // Two identical connected terms with a bare Grassmann field: the canonical
  // form makes them bit-identical, so the exact-duplicate pre-pass sums them.
  // The GammaN carries fields {psibar, psi} — inside the yukawa truncation.
  FEq eq;
  eq.push_back({});
  eq[0].push_back({ObjectType::Field, {{psi, 5}}});
  eq[0].push_back({ObjectType::GammaN, {{psibar, -5}, {psi, 1}}});
  eq[0].value = 1.;
  eq.push_back({});
  eq[1].push_back({ObjectType::Field, {{psi, 7}}});
  eq[1].push_back({ObjectType::GammaN, {{psibar, -7}, {psi, 1}}});
  eq[1].value = 2.;

  // The same content with the objects in swapped order is NOT merged: the
  // matcher cannot track the sign of commuting bare Grassmann fields.
  eq.push_back({});
  eq[2].push_back({ObjectType::GammaN, {{psibar, -6}, {psi, 1}}});
  eq[2].push_back({ObjectType::Field, {{psi, 6}}});
  eq[2].value = 4.;

  simplify(setup, eq);
  REQUIRE(eq.size() == 2);
  REQUIRE(eq[0].value == 3.);
  REQUIRE(eq[1].value == 4.);
}

TEST_CASE("simplify: scalar 2-point flow", "[simplify][driver][integration]")
{
  // Full pipeline on the scalar fixture: two phi-derivatives of the Wetterich
  // RHS 1/2 G^{ab} Rdot_{ba}, truncated, then simplified. The two polarization
  // diagrams (P G3 P G3 P Rdot, +1/2 each) merge to coefficient 1; the tadpole
  // (P G4 P Rdot, -1/2) stays. This reproduces the textbook flow of the scalar
  // two-point function, Tr[G G3 G G3 G Rdot] - 1/2 Tr[G G4 G Rdot].
  auto [setup, feq] = parse(BOILERPLATE_DIR + "scalar.toml");
  FTerm &term = feq[0];
  const FieldIdx phi = setup.field_to_idx("phi");
  term.insert(term.begin(), {ObjectType::FDOp, {{phi, 4}}});
  term.insert(term.begin(), {ObjectType::FDOp, {{phi, 3}}});

  resolve_derivatives(setup, feq);
  truncate(setup, feq);
  REQUIRE(feq.size() == 3);

  simplify(setup, feq);
  REQUIRE(feq.size() == 2);

  std::vector<double> values;
  std::vector<std::size_t> sizes;
  for (const auto &t : feq) {
    values.push_back(t.value);
    sizes.push_back(t.size());
  }
  std::sort(values.begin(), values.end());
  std::sort(sizes.begin(), sizes.end());
  REQUIRE(values == std::vector<double>{-0.5, 1.});
  REQUIRE(sizes == std::vector<std::size_t>{4, 6}); // tadpole, polarization

  // Idempotence, semantically: a second pass merges nothing further and keeps
  // every term equal to its snapshot up to the canonical form. (Bit-level
  // identity is not guaranteed: renumbering can change the relative order of
  // equal-field legs, which the next normalize re-sorts.)
  const FEq snapshot = feq;
  simplify(setup, feq);
  REQUIRE(feq.size() == snapshot.size());
  for (std::size_t i = 0; i < feq.size(); ++i) {
    REQUIRE(feq[i].value == snapshot[i].value);
    const auto res = terms_equal(setup, feq[i], snapshot[i]);
    REQUIRE(res.has_value());
    REQUIRE(*res == 1.);
  }
}

TEST_CASE("simplify: yukawa 2-point flow", "[simplify][driver][integration]")
{
  // Two phi-derivatives of the yukawa flow: the four fermion-loop polarization
  // terms (-1/2 each) merge pairwise into the two loop orientations at -1
  // each. The orientations are genuinely different index structures with the
  // external legs i1, i2 fixed; identifying them requires the external-leg
  // exchange symmetry i1 <-> i2, which is Phase 5 (symmetry-aware simplify),
  // NOT the plain matcher.
  //
  // The full Mathematica pipeline outputs 1 term at -2 instead — because
  // FTakeDerivatives attaches an S_2 "Symmetries" annotation and FTruncate
  // internally re-runs FSimplify with it (Truncation.m:910). With the
  // annotations stripped from the raw derivative output, the Mathematica
  // no-symmetry result is the same as here.
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  FTerm &term = feq[0];
  const FieldIdx phi = setup.field_to_idx("phi");
  term.insert(term.begin(), {ObjectType::FDOp, {{phi, 4}}});
  term.insert(term.begin(), {ObjectType::FDOp, {{phi, 3}}});

  resolve_derivatives(setup, feq);
  truncate(setup, feq);
  REQUIRE(feq.size() == 4);

  simplify(setup, feq);
  REQUIRE(feq.size() == 2);
  for (const auto &t : feq) {
    REQUIRE(t.value == -1.);
    REQUIRE(t.size() == 6);
  }
}

TEST_CASE("simplify: flow matrix cross-validated against Mathematica", "[simplify][driver][integration]")
{
  // Full pipeline (derivatives -> truncate -> simplify) for 2-, 3-, and
  // 4-point Wetterich flows of all three fixtures, checked against the
  // NO-SYMMETRY Mathematica reference: FTakeDerivatives, then the Symmetries
  // annotations stripped BEFORE FTruncate (FTruncate internally re-runs
  // FSimplify with any attached annotations, Truncation.m:910), then
  // FSimplify.
  //
  // With Mathematica's derivative-time symmetry features additionally
  // disabled (FSetAutoBuildSymmetryList[False]; FSetAutoSimplify[False]),
  // EVERY row below matches the reference exactly — term counts and
  // coefficient multisets. With those features at their defaults, the
  // same-field 2-/3-point flows come out collapsed instead (representative
  // terms with the S_n orbit multiplicity in the coefficient, e.g.
  // yukawa-phi2 as one term at -2): FTakeDerivatives auto-builds the
  // external-leg symmetry list and re-runs FSimplify with it after each
  // derivative pass, guarded by Length[ret] < 32 && Length[symmetries] <= 6
  // (Derivatives.m:173) — which is why 4-point flows (S_4 = 24 rules) stay
  // expanded and match here even with the defaults on. Reproducing the
  // collapsed form is Phase 5 (symmetry-aware simplify).
  struct Flow {
    std::string file;
    std::vector<std::string> derivs;
    std::size_t truncated;      // terms after truncate
    std::vector<double> coeffs; // sorted coefficient multiset after simplify
  };
  const std::vector<Flow> flows = {
      // scalar
      {"scalar.toml", {"phi", "phi"}, 3, {-0.5, 1.}},
      {"scalar.toml", {"phi", "phi", "phi"}, 12, {-1., -1., -1., 1., 1., 1.}},
      {"scalar.toml", {"phi", "phi", "phi", "phi"}, 66,
       {-1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1.,
        1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.}},
      // yukawa
      {"yukawa.toml", {"phi", "phi"}, 4, {-1., -1.}},
      {"yukawa.toml", {"psibar", "psi"}, 4, {-1., -1.}},
      {"yukawa.toml", {"psibar", "psi", "phi"}, 6, {-1., -1., -1.}},
      {"yukawa.toml", {"phi", "phi", "phi"}, 12, {-1., -1., -1., -1., -1., -1.}},
      {"yukawa.toml", {"phi", "phi", "phi", "phi"}, 48,
       {-1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1.,
        -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1.}},
      // yang-mills
      {"yang-mills.toml", {"A", "A"}, 7, {-1., -1., -0.5, 1.}},
      {"yang-mills.toml", {"cb", "c"}, 4, {-1., -1.}},
      {"yang-mills.toml", {"cb", "c", "A"}, 12, {-1., -1., -1., 1., 1., 1.}},
      {"yang-mills.toml", {"A", "A", "A"}, 24,
       {-1., -1., -1., -1., -1., -1., -1., -1., -1., 1., 1., 1.}},
      {"yang-mills.toml", {"A", "A", "A", "A"}, 114,
       {-1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1.,
        -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1., -1.,
        -1., -1., -1., -1., 1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.,  1.}},
  };

  for (const auto &flow : flows) {
    CAPTURE(flow.file, flow.derivs);
    auto [setup, feq] = parse(BOILERPLATE_DIR + flow.file);
    FTerm &term = feq[0];
    for (std::size_t i = flow.derivs.size(); i-- > 0;)
      term.insert(term.begin(),
                  {ObjectType::FDOp, {{setup.field_to_idx(flow.derivs[i]), Idx(101 + i)}}});

    resolve_derivatives(setup, feq);
    truncate(setup, feq);
    REQUIRE(feq.size() == flow.truncated);

    simplify(setup, feq);
    std::vector<double> coeffs;
    for (const auto &t : feq)
      coeffs.push_back(t.value);
    std::sort(coeffs.begin(), coeffs.end());
    REQUIRE(coeffs == flow.coeffs);
  }
}

// ---- Symmetry-aware simplification (Phase 5) --------------------------------
// Symmetries are user-supplied (Setup::symmetries, parsed from the input file):
// whether the equation is invariant under permuting its external legs is
// analytic information about where the master equation came from and cannot be
// decided from the equation alone. simplify() compiles them against the
// equation's external legs (Symmetries::build) and retries failed pair
// comparisons under each transformation, multiplying in the symmetry factor.

namespace
{
  Symmetries make_symmetries(const std::vector<Symmetry> &list)
  {
    Symmetries syms;
    for (const auto &sym : list)
      syms.add(sym);
    syms.finalize();
    return syms;
  }
} // namespace

TEST_CASE("make_symmetry_list generates the graded symmetry group", "[simplify][symmetry]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yang-mills.toml");
  const FieldIdx A = setup.field_to_idx("A");
  const FieldIdx cb = setup.field_to_idx("cb");
  const FieldIdx c = setup.field_to_idx("c");

  // Three identical bosons: the full S_3, i.e. 5 non-identity permutations,
  // all with factor +1.
  const auto s3 = make_symmetry_list(setup, {{A, 1}, {A, 2}, {A, 3}});
  REQUIRE(s3.size() == 5);
  for (const auto &sym : s3)
    REQUIRE(sym.factor == 1);

  // A Grassmann pair of the same field: one swap, factor -1.
  const auto gswap = make_symmetry_list(setup, {{cb, 1}, {cb, 2}});
  REQUIRE(gswap.size() == 1);
  REQUIRE(gswap[0].cycles == std::vector<std::vector<Idx>>{{1, 2}});
  REQUIRE(gswap[0].factor == -1);

  // The mixed example A, cb, c, cb, c, A: S_2(A) x swap(cb) x swap(c) minus
  // the identity = 2*2*2 - 1 = 7 entries; the four with an odd number of
  // Grassmann swaps carry -1.
  const auto mixed = make_symmetry_list(setup, {{A, 1}, {cb, 2}, {c, 3}, {cb, 4}, {c, 5}, {A, 6}});
  REQUIRE(mixed.size() == 7);
  REQUIRE(std::count_if(mixed.begin(), mixed.end(), [](const Symmetry &s) { return s.factor == -1; }) == 4);

  // Pairwise-distinct fields admit no permutation symmetry.
  REQUIRE(make_symmetry_list(setup, {{A, 1}, {cb, 2}, {c, 3}}).empty());

  // AnyField legs and non-positive labels are rejected.
  REQUIRE_THROWS_AS(make_symmetry_list(setup, {{AnyField, 1}, {A, 2}}), std::runtime_error);
  REQUIRE_THROWS_AS(make_symmetry_list(setup, {{A, -1}, {A, 2}}), std::runtime_error);
}

TEST_CASE("Symmetries::build compiles cycles against external legs", "[simplify][symmetry]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx phi = setup.field_to_idx("phi");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");

  const std::vector<LegT> external = {{phi, 1}, {phi, -2}, {phi, 3}, {psibar, 4}, {psi, 5}};

  // (1 2 3) -> 1->2, 2->3, 3->1; position signs of the legs are irrelevant.
  const Symmetries syms = make_symmetries({{{{1, 2, 3}}, 1}, {{{1, 2}}, -1}});
  const auto compiled = syms.build(setup, external);
  REQUIRE(compiled.size() == 2);
  REQUIRE(compiled[0].rules == std::vector<std::pair<Idx, Idx>>{{1, 2}, {2, 3}, {3, 1}});
  REQUIRE(compiled[0].factor == 1.);
  REQUIRE(compiled[1].rules == std::vector<std::pair<Idx, Idx>>{{1, 2}, {2, 1}});
  REQUIRE(compiled[1].factor == -1.);

  // Unknown label and mixed-field cycles are user errors.
  REQUIRE_THROWS_AS(make_symmetries({{{{1, 9}}, 1}}).build(setup, external), std::runtime_error);
  REQUIRE_THROWS_AS(make_symmetries({{{{4, 5}}, -1}}).build(setup, external), std::runtime_error);
}

TEST_CASE("simplify: symmetry merge carries the factor", "[simplify][symmetry][driver]")
{
  auto [setup, feq] = parse(BOILERPLATE_DIR + "yukawa.toml");
  const FieldIdx psi = setup.field_to_idx("psi");
  const FieldIdx psibar = setup.field_to_idx("psibar");

  // Two terms related exactly by the external swap 101 <-> 102 (both legs psi,
  // so the swap is field-consistent). The distinct object types (Rdot vs
  // GammaN) pin which external leg sits on which object, so they do NOT merge
  // without the symmetry.
  const KeyT rdot = setup.type_to_idx("Rdot");
  const auto build_eq = [&]() {
    FEq eq;
    eq.push_back({});
    eq[0].push_back({rdot, {{psi, 101}, {psibar, 6}}});
    eq[0].push_back({ObjectType::GammaN, {{psi, 102}, {psibar, -6}}});
    eq[0].value = 1.;
    eq.push_back({});
    eq[1].push_back({rdot, {{psi, 102}, {psibar, 7}}});
    eq[1].push_back({ObjectType::GammaN, {{psi, 101}, {psibar, -7}}});
    eq[1].value = 1.;
    return eq;
  };

  FEq no_sym = build_eq();
  simplify(setup, no_sym);
  REQUIRE(no_sym.size() == 2);

  // Antisymmetric under the swap: the pair cancels.
  Setup odd = setup;
  odd.symmetries = make_symmetries({{{{101, 102}}, -1}});
  FEq cancels = build_eq();
  simplify(odd, cancels);
  REQUIRE(cancels.empty());

  // Symmetric under the swap: the pair merges to coefficient 2.
  Setup even = setup;
  even.symmetries = make_symmetries({{{{101, 102}}, 1}});
  FEq merges = build_eq();
  simplify(even, merges);
  REQUIRE(merges.size() == 1);
  REQUIRE(merges[0].value == 2.);
}

TEST_CASE("simplify: flow matrix with symmetries matches the default Mathematica pipeline",
          "[simplify][symmetry][driver][integration]")
{
  // Same flows as the no-symmetry matrix, but with the symmetry group
  // generated from the derivative list (make_symmetry_list) — the same
  // information FTakeDerivatives auto-builds and the Mathematica pipeline
  // consumes by default. Reference values:
  //   FTakeDerivatives[setup, WetterichEquation, dlist] // FTruncate // FSimplify
  struct Flow {
    std::string file;
    std::vector<std::string> derivs;
    std::vector<double> coeffs; // sorted coefficient multiset after simplify
  };
  const std::vector<Flow> flows = {
      {"scalar.toml", {"phi", "phi"}, {-0.5, 1.}},
      {"scalar.toml", {"phi", "phi", "phi"}, {-3., 3.}},
      {"scalar.toml", {"phi", "phi", "phi", "phi"}, {-12., -6., 3., 12.}},
      {"yukawa.toml", {"phi", "phi"}, {-2.}},
      {"yukawa.toml", {"phi", "phi", "phi"}, {-6.}},
      {"yukawa.toml", {"phi", "phi", "phi", "phi"}, {-24.}},
      {"yang-mills.toml", {"A", "A"}, {-2., -0.5, 1.}},
      {"yang-mills.toml", {"A", "A", "A"}, {-6., -3., 3.}},
      {"yang-mills.toml", {"A", "A", "A", "A"}, {-24., -12., -6., 3., 12.}},
  };

  // Each flow runs twice: once with the derivative legs declared directly
  // (Setup::derivatives — the orbit fast path), once with the explicitly
  // expanded permutation group (Setup::symmetries — the transform-and-retry
  // path). Both must give the reference result.
  for (const bool use_orbits : {true, false}) {
    for (const auto &flow : flows) {
      CAPTURE(use_orbits, flow.file, flow.derivs);
      auto [setup, feq] = parse(BOILERPLATE_DIR + flow.file);
      FTerm &term = feq[0];
      std::vector<LegT> derivative_legs;
      for (std::size_t i = flow.derivs.size(); i-- > 0;) {
        const LegT leg = {setup.field_to_idx(flow.derivs[i]), Idx(101 + i)};
        term.insert(term.begin(), {ObjectType::FDOp, {leg}});
        derivative_legs.push_back(leg);
      }
      if (use_orbits)
        setup.derivatives = derivative_legs;
      else
        setup.symmetries = make_symmetries(make_symmetry_list(setup, derivative_legs));

      resolve_derivatives(setup, feq);
      truncate(setup, feq);
      simplify(setup, feq);

      std::vector<double> coeffs;
      for (const auto &t : feq)
        coeffs.push_back(t.value);
      std::sort(coeffs.begin(), coeffs.end());
      REQUIRE(coeffs == flow.coeffs);
    }
  }
}

TEST_CASE("simplify: fully file-driven flow with derivative symmetries", "[simplify][symmetry][integration]")
{
  // scalar-flow.toml carries the FDOps in the equation AND a "derivatives"
  // list; the two identical commuting legs 101, 102 form an orbit that
  // simplify's matcher treats as freely interchangeable. No programmatic
  // setup needed.
  auto [setup, feq] = parse(BOILERPLATE_DIR + "scalar-flow.toml");
  REQUIRE(setup.symmetries.empty());
  const FieldIdx phi = setup.field_to_idx("phi");
  REQUIRE(setup.derivatives == std::vector<LegT>{{phi, 101}, {phi, 102}});

  resolve_derivatives(setup, feq);
  truncate(setup, feq);
  simplify(setup, feq);

  std::vector<double> coeffs;
  for (const auto &t : feq)
    coeffs.push_back(t.value);
  std::sort(coeffs.begin(), coeffs.end());
  REQUIRE(coeffs == std::vector<double>{-0.5, 1.});
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
