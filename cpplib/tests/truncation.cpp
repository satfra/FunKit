#include <catch2/catch_test_macros.hpp>

#include <algorithm>
#include <set>

#include "funkit.hpp"

namespace
{
  bool contains_type(const FunKit::FTerm &term, FunKit::KeyT type)
  {
    return std::any_of(term.begin(), term.end(), [&](const FunKit::Object &o) { return o.type == type; });
  }

  // Fields of the first object of the given type in the term, in leg order
  std::vector<FunKit::FieldIdx> fields_of(const FunKit::FTerm &term, FunKit::KeyT type)
  {
    for (const auto &obj : term) {
      if (obj.type != type) continue;
      std::vector<FunKit::FieldIdx> fields;
      for (const auto &leg : obj.legs)
        fields.push_back(leg.first);
      return fields;
    }
    return {};
  }

  // Fields of the first object of the given type, ordered by abs leg index. Stable against
  // normalize(), which reorders the legs within an object but keeps the index assignment.
  std::vector<FunKit::FieldIdx> fields_by_index(const FunKit::FTerm &term, FunKit::KeyT type)
  {
    for (const auto &obj : term) {
      if (obj.type != type) continue;
      std::vector<FunKit::LegT> legs(obj.legs.begin(), obj.legs.end());
      std::sort(legs.begin(), legs.end(),
                [](const FunKit::LegT &a, const FunKit::LegT &b) { return std::abs(a.second) < std::abs(b.second); });
      std::vector<FunKit::FieldIdx> fields;
      for (const auto &leg : legs)
        fields.push_back(leg.first);
      return fields;
    }
    return {};
  }
} // namespace

TEST_CASE("Prune resolves FMinus factors", "[truncation][prune]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // Commuting fields: factor removed, no sign
  FunKit::FTerm cterm;
  cterm.push_back({FunKit::ObjectType::FMinus, {{phi, 1}, {psi, 2}}});
  cterm.push_back({FunKit::ObjectType::Propagator, {{phi, 3}, {phi, 4}}});
  FunKit::prune(setup, cterm);
  REQUIRE(cterm.size() == 1);
  REQUIRE(cterm[0].type == FunKit::ObjectType::Propagator);
  REQUIRE(cterm.value == 1.);

  // Two Grassmann fields: factor removed with a sign flip
  FunKit::FTerm gterm;
  gterm.push_back({FunKit::ObjectType::FMinus, {{psi, 1}, {psibar, 2}}});
  gterm.push_back({FunKit::ObjectType::Propagator, {{phi, 3}, {phi, 4}}});
  FunKit::prune(setup, gterm);
  REQUIRE(gterm.size() == 1);
  REQUIRE(gterm.value == -1.);

  // (-1)^{dd} with a single Grassmann field
  FunKit::FTerm dterm;
  dterm.push_back({FunKit::ObjectType::FMinus, {{psi, 1}, {psi, 1}}});
  FunKit::prune(setup, dterm);
  REQUIRE(dterm.value == -1.);

  // FMinus with an AnyField leg must stay symbolic
  FunKit::FTerm aterm;
  aterm.push_back({FunKit::ObjectType::FMinus, {{FunKit::AnyField, 1}, {psi, 2}}});
  aterm.push_back({FunKit::ObjectType::Propagator, {{phi, 3}, {phi, 4}}});
  FunKit::prune(setup, aterm);
  REQUIRE(aterm.size() == 2);
  REQUIRE(aterm[0].type == FunKit::ObjectType::FMinus);
  REQUIRE(aterm.value == 1.);
}

TEST_CASE("Prune resolves gamma factors", "[truncation][prune]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // gamma_a^b with the same field contracts to 1 and renames the closed index
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::gamma, {{phi, -1}, {phi, 2}}});
  term.push_back({FunKit::ObjectType::GammaN, {{phi, -2}, {phi, -3}}});
  FunKit::prune(setup, term);
  REQUIRE(term.size() == 1);
  REQUIRE(term[0].type == FunKit::ObjectType::GammaN);
  REQUIRE(term.value == 1.);
  REQUIRE(term[0].legs[0] == FunKit::LegT{phi, -1});
  REQUIRE(term[0].legs[1] == FunKit::LegT{phi, -3});

  // Same Grassmann field: also a plain delta, no sign. The GammaN must be inside the yukawa
  // truncation, since prune drops terms with out-of-truncation correlation functions.
  FunKit::FTerm gsame;
  gsame.push_back({FunKit::ObjectType::gamma, {{psi, -1}, {psi, 2}}});
  gsame.push_back({FunKit::ObjectType::GammaN, {{psibar, -2}, {psi, -3}}});
  FunKit::prune(setup, gsame);
  REQUIRE(gsame.size() == 1);
  REQUIRE(gsame.value == 1.);
  REQUIRE(gsame[0].legs[0] == FunKit::LegT{psibar, -1});

  // Mixed positions with different fields (even partners) vanish and kill the term
  FunKit::FTerm gpart;
  gpart.push_back({FunKit::ObjectType::gamma, {{psibar, -1}, {psi, 2}}});
  gpart.push_back({FunKit::ObjectType::GammaN, {{phi, -2}, {phi, -3}}});
  FunKit::prune(setup, gpart);
  REQUIRE(gpart.empty());

  // gamma with an AnyField leg must stay symbolic
  FunKit::FTerm gany;
  gany.push_back({FunKit::ObjectType::gamma, {{FunKit::AnyField, -1}, {phi, 2}}});
  FunKit::prune(setup, gany);
  REQUIRE(gany.size() == 1);
  REQUIRE(gany[0].type == FunKit::ObjectType::gamma);
}

TEST_CASE("Prune resolves SymmetryFactor objects", "[truncation][prune]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  // 1/n! for each group of identical fields, cf. SymmetryFactorFromList (FEDeriK/Metric.m):
  // (phi, phi, psi) -> 1/2!
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::SymmFactor, {{phi, 1}, {phi, 2}, {psi, 3}}});
  term.push_back({FunKit::ObjectType::Propagator, {{phi, 4}, {phi, 5}}});
  FunKit::prune(setup, term);
  REQUIRE(term.size() == 1);
  REQUIRE(term[0].type == FunKit::ObjectType::Propagator);
  REQUIRE(term.value == 0.5);

  // (phi, phi, phi) -> 1/3!
  FunKit::FTerm t3;
  t3.push_back({FunKit::ObjectType::SymmFactor, {{phi, 1}, {phi, 2}, {phi, 3}}});
  FunKit::prune(setup, t3);
  REQUIRE(t3.value == 1. / 6.);

  // SymmetryFactor with an AnyField leg must stay symbolic
  FunKit::FTerm aterm;
  aterm.push_back({FunKit::ObjectType::SymmFactor, {{FunKit::AnyField, 1}, {phi, 2}}});
  FunKit::prune(setup, aterm);
  REQUIRE(aterm.size() == 1);
  REQUIRE(aterm[0].type == FunKit::ObjectType::SymmFactor);
}

TEST_CASE("Truncate resolves SymmetryFactor objects after field assignment", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  // The SymmetryFactor legs share the propagator's indices
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::SymmFactor, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});
  term.push_back({FunKit::ObjectType::Propagator, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 3);
  for (const auto &t : out) {
    REQUIRE(t.size() == 1); // SymmetryFactor resolved into the prefactor
    REQUIRE(t[0].type == FunKit::ObjectType::Propagator);
    // (phi, phi) gives 1/2!; the Grassmann channels have distinct fields, so factor 1, with a
    // -1 from normalize when it sorts a (psi, psibar) propagator into canonical order
    const FunKit::FieldIdx f1 = fields_by_index(t, FunKit::ObjectType::Propagator)[0];
    const double expected = f1 == phi ? 0.5 : (f1 == psi ? -1. : 1.);
    REQUIRE(t.value == expected);
  }
}

TEST_CASE("Prune erases vanished terms from an FEq", "[truncation][prune]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  FunKit::FTerm kept;
  kept.push_back({FunKit::ObjectType::Propagator, {{phi, 1}, {phi, 2}}});
  FunKit::FTerm dropped;
  dropped.push_back({FunKit::ObjectType::gamma, {{psibar, -1}, {psi, 2}}});

  FunKit::FEq eq = {kept, dropped};
  FunKit::prune(setup, eq);
  REQUIRE(eq.size() == 1);
  REQUIRE(eq[0][0].type == FunKit::ObjectType::Propagator);
}

TEST_CASE("Truncate keeps terms without AnyField unchanged", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  REQUIRE(feq.size() == 1);

  // truncate normalizes its output, so the term is unchanged up to the canonical leg order
  FunKit::FTerm expected = feq[0];
  FunKit::normalize(setup, expected);

  const FunKit::FEq out = FunKit::truncate(setup, feq[0]);
  REQUIRE(out.size() == 1);
  REQUIRE(out[0].size() == expected.size());
  REQUIRE(out[0].value == expected.value);
  for (FunKit::Idx i = 0; i < out[0].size(); ++i) {
    REQUIRE(out[0][i].type == expected[i].type);
    REQUIRE(out[0][i].legs == expected[i].legs);
  }
}

TEST_CASE("Truncate prunes terms without open correlation functions", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // A concrete gamma factor is resolved even though nothing needs to be expanded
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::gamma, {{phi, -1}, {phi, 2}}});
  term.push_back({FunKit::ObjectType::GammaN, {{phi, -2}, {phi, -3}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 1);
  REQUIRE(out[0].size() == 1);
  REQUIRE(out[0][0].type == FunKit::ObjectType::GammaN);

  // A concrete correlation function outside the truncation drops the term
  FunKit::FTerm bad;
  bad.push_back({FunKit::ObjectType::GammaN, {{psibar, -1}, {phi, -2}}});
  REQUIRE(FunKit::truncate(setup, bad).empty());
}

TEST_CASE("Truncate expands open Field objects", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Field, {{FunKit::AnyField, 1}}});

  // No Field truncation rules: expand over all fields
  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 3);
  for (const auto &t : out)
    REQUIRE_FALSE(FunKit::has_AnyField(t));

  // With Field rules, only the listed fields are kept
  FunKit::Setup restricted = setup;
  restricted.truncation.initialize(restricted);
  restricted.truncation.add_rule(FunKit::ObjectType::Field, {phi});
  restricted.truncation.finalize();

  const FunKit::FEq rout = FunKit::truncate(restricted, term);
  REQUIRE(rout.size() == 1);
  REQUIRE(rout[0][0].legs[0].first == phi);
}

TEST_CASE("Truncate expands an AnyField propagator over the truncation rules", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");
  const FunKit::KeyT rdot = setup.type_to_idx("Rdot");

  // The master term is 1/2 G^{AB} Rdot_{BA} with all legs AnyField
  const FunKit::FEq out = FunKit::truncate(setup, feq[0]);
  REQUIRE(out.size() == 3);

  std::set<std::vector<FunKit::FieldIdx>> prop_legs;
  for (const auto &term : out) {
    REQUIRE_FALSE(FunKit::has_AnyField(term));
    REQUIRE(term.value == 0.5);
    // The contracted Rdot legs must carry the same fields as the propagator
    REQUIRE(fields_by_index(term, FunKit::ObjectType::Propagator) == fields_by_index(term, rdot));
    prop_legs.insert(fields_by_index(term, FunKit::ObjectType::Propagator));
  }
  const std::set<std::vector<FunKit::FieldIdx>> expected = {{phi, phi}, {psibar, psi}, {psi, psibar}};
  REQUIRE(prop_legs == expected);
}

TEST_CASE("Truncate filters rules by concrete legs", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{psibar, 1}, {FunKit::AnyField, 2}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 1);
  REQUIRE(out[0][0].legs[0].first == psibar);
  REQUIRE(out[0][0].legs[1].first == psi);
}

TEST_CASE("Truncate drops terms with no matching rule", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // Restrict the propagator truncation to (phi, phi) only
  FunKit::Setup restricted = setup;
  restricted.truncation.initialize(restricted);
  restricted.truncation.add_rule(FunKit::ObjectType::Propagator, {phi, phi});
  restricted.truncation.finalize();

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{psibar, 1}, {FunKit::AnyField, 2}}});

  const FunKit::FEq out = FunKit::truncate(restricted, term);
  REQUIRE(out.empty());
}

TEST_CASE("Truncate of a type with only higher-order rules drops the term", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  FunKit::Setup restricted = setup;
  restricted.truncation.initialize(restricted);
  restricted.truncation.add_rule(FunKit::ObjectType::GammaN, {phi, phi, phi});
  restricted.truncation.finalize();

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}, {FunKit::AnyField, -2}}});

  const FunKit::FEq out = FunKit::truncate(restricted, term);
  REQUIRE(out.empty());
}

TEST_CASE("Truncate drops 2pt functions of a type with only order-1 rules", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // GammaN truncated to order 1 only: a 2-leg GammaN accepts nothing
  FunKit::Setup restricted = setup;
  restricted.truncation.initialize(restricted);
  restricted.truncation.add_rule(FunKit::ObjectType::GammaN, {phi});
  restricted.truncation.finalize();

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}, {FunKit::AnyField, -2}}});

  const FunKit::FEq out = FunKit::truncate(restricted, term);
  REQUIRE(out.empty());
}

TEST_CASE("Truncate drops 2pt functions of a fully truncated type", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  // A single empty rule means nothing is accepted for this type
  FunKit::Setup full = setup;
  full.truncation.initialize(full);
  full.truncation.add_rule(FunKit::ObjectType::Propagator, {});
  full.truncation.finalize();

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});

  const FunKit::FEq out = FunKit::truncate(full, term);
  REQUIRE(out.empty());
}

TEST_CASE("Truncate without any rules expands over all fields", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  // Wipe the truncation entirely: no rules for any type means everything is allowed
  FunKit::Setup open = setup;
  open.truncation.initialize(open);
  open.truncation.finalize();

  const FunKit::FEq out = FunKit::truncate(open, feq[0]);
  // All ordered pairs of {phi, psibar, psi}
  REQUIRE(out.size() == 9);
  for (const auto &term : out)
    REQUIRE_FALSE(FunKit::has_AnyField(term));
}

TEST_CASE("Truncate resolves FMinus factors after field assignment", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  // (-1)^{aa} G^{ab}: the FMinus legs share the propagator's first index
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::FMinus, {{FunKit::AnyField, 1}, {FunKit::AnyField, 1}}});
  term.push_back({FunKit::ObjectType::Propagator, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 3);
  for (const auto &t : out) {
    REQUIRE(t.size() == 1); // FMinus resolved into the prefactor
    REQUIRE(t[0].type == FunKit::ObjectType::Propagator);
    // The FMinus contributes -1 for a Grassmann field on index 1; normalize contributes another
    // -1 when it sorts a (psi, psibar) propagator into canonical order
    const FunKit::FieldIdx f1 = fields_by_index(t, FunKit::ObjectType::Propagator)[0];
    const double fminus = setup.is_gField(f1) ? -1. : 1.;
    const double norm = f1 == psi ? -1. : 1.;
    REQUIRE(t.value == fminus * norm);
  }
}

TEST_CASE("Truncate an FEq merges expansions and is idempotent", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  FunKit::truncate(setup, feq);
  REQUIRE(feq.size() == 3);

  const FunKit::FEq snapshot = feq;
  FunKit::truncate(setup, feq);
  REQUIRE(feq.size() == snapshot.size());
  for (FunKit::Idx i = 0; i < feq.size(); ++i) {
    REQUIRE(feq[i].value == snapshot[i].value);
    REQUIRE(feq[i].size() == snapshot[i].size());
  }
}

TEST_CASE("Derivatives followed by truncation close the scalar flow", "[truncation][integration]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // Two-point flow: d/dphi d/dphi of 1/2 G^{ab} Rdot_{ba}
  FunKit::FTerm master = feq[0];
  master.insert(master.begin(), {FunKit::ObjectType::FDOp, {{phi, 4}}});
  master.insert(master.begin(), {FunKit::ObjectType::FDOp, {{phi, 3}}});
  FunKit::FEq eq = {master};

  FunKit::resolve_derivatives(setup, eq);
  FunKit::truncate(setup, eq);

  REQUIRE_FALSE(eq.empty());
  for (const auto &term : eq) {
    REQUIRE_FALSE(FunKit::has_AnyField(term));
    REQUIRE_FALSE(contains_type(term, FunKit::ObjectType::FMinus));
    REQUIRE_FALSE(contains_type(term, FunKit::ObjectType::gamma));
    REQUIRE_FALSE(contains_type(term, FunKit::ObjectType::FDOp));
    // Every correlation function must be inside the truncation
    for (const auto &obj : term) {
      if (!setup.is_correlationFunction(obj.type)) continue;
      REQUIRE(setup.truncation.in_truncation(obj));
    }
  }
}

TEST_CASE("Truncate without 2pt functions", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // GammaN restricted to the single order-3 rule (phi, phi, phi)
  FunKit::Setup restricted = setup;
  restricted.truncation.initialize(restricted);
  restricted.truncation.add_rule(FunKit::ObjectType::GammaN, {phi, phi, phi});
  restricted.truncation.finalize();

  // Two 3-leg vertices contracted on two legs; no 2pt function anywhere in the term
  FunKit::FTerm term;
  term.push_back(
      {FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}, {FunKit::AnyField, -2}, {FunKit::AnyField, -3}}});
  term.push_back({FunKit::ObjectType::GammaN, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}, {FunKit::AnyField, -4}}});

  const FunKit::FEq out = FunKit::truncate(restricted, term);

  REQUIRE(out.size() == 1);
  REQUIRE_FALSE(FunKit::has_AnyField(out[0]));
  REQUIRE(out[0].value == 1.);
  REQUIRE(out[0].size() == 2);
  for (const auto &obj : out[0]) {
    REQUIRE(obj.type == FunKit::ObjectType::GammaN);
    for (const auto &leg : obj.legs)
      REQUIRE(leg.first == phi);
  }
}

TEST_CASE("Truncate expands 1pt functions over the order-1 rules", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // The yukawa truncation contains the single order-1 rule GammaN = (phi)
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 1);
  REQUIRE(out[0].size() == 1);
  REQUIRE(out[0][0].legs.size() == 1);
  REQUIRE(out[0][0].legs[0].first == phi);
}

TEST_CASE("Truncate drops 1pt functions of a type without order-1 rules", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // GammaN only has an order-2 rule: a 1-leg GammaN accepts nothing
  FunKit::Setup restricted = setup;
  restricted.truncation.initialize(restricted);
  restricted.truncation.add_rule(FunKit::ObjectType::GammaN, {phi, phi});
  restricted.truncation.finalize();

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}}});

  const FunKit::FEq out = FunKit::truncate(restricted, term);
  REQUIRE(out.empty());
}

TEST_CASE("Truncate filters npt permutations by concrete legs", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // The yukawa order-3 rule is (psibar, psi, phi); with the first leg fixed to psibar,
  // only the permutations starting with psibar survive
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::GammaN, {{psibar, -1}, {FunKit::AnyField, -2}, {FunKit::AnyField, -3}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 2);

  std::set<std::vector<FunKit::FieldIdx>> legs;
  for (const auto &t : out) {
    REQUIRE_FALSE(FunKit::has_AnyField(t));
    legs.insert(fields_by_index(t, FunKit::ObjectType::GammaN));
  }
  const std::set<std::vector<FunKit::FieldIdx>> expected = {{psibar, psi, phi}, {psibar, phi, psi}};
  REQUIRE(legs == expected);
}

TEST_CASE("Truncate npt functions without any rules expands over all fields", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  // Wipe the truncation entirely: no rules for any type means everything is allowed
  FunKit::Setup open = setup;
  open.truncation.initialize(open);
  open.truncation.finalize();

  FunKit::FTerm term;
  term.push_back(
      {FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}, {FunKit::AnyField, -2}, {FunKit::AnyField, -3}}});

  const FunKit::FEq out = FunKit::truncate(open, term);
  // All ordered triples of {phi, psibar, psi}
  REQUIRE(out.size() == 27);
  std::set<std::vector<FunKit::FieldIdx>> legs;
  for (const auto &t : out) {
    REQUIRE_FALSE(FunKit::has_AnyField(t));
    legs.insert(fields_by_index(t, FunKit::ObjectType::GammaN));
  }
  REQUIRE(legs.size() == 27);
}

TEST_CASE("Truncate expands 2pt functions before npt functions", "[truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  // A propagator contracted with two legs of a 3-leg vertex: the 2pt expansion runs first
  // and its fields propagate into the vertex, filtering the order-3 permutations
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});
  term.push_back(
      {FunKit::ObjectType::GammaN, {{FunKit::AnyField, -1}, {FunKit::AnyField, -2}, {FunKit::AnyField, -3}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE_FALSE(out.empty());
  for (const auto &t : out) {
    REQUIRE_FALSE(FunKit::has_AnyField(t));
    const auto prop = fields_by_index(t, FunKit::ObjectType::Propagator);
    const auto vertex = fields_by_index(t, FunKit::ObjectType::GammaN);
    REQUIRE(vertex.size() == 3);
    // The contracted vertex legs (indices 1, 2) must carry the propagator's fields
    REQUIRE(prop == std::vector<FunKit::FieldIdx>{vertex[0], vertex[1]});
    for (const auto &obj : t) {
      if (!setup.is_correlationFunction(obj.type)) continue;
      REQUIRE(setup.truncation.in_truncation(obj));
    }
  }
}

TEST_CASE("Derivatives followed by truncation close the yukawa flow", "[truncation][integration]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // Quark two-point flow: d/dpsibar d/dpsi of 1/2 G^{ab} Rdot_{ba}
  FunKit::FTerm master = feq[0];
  master.insert(master.begin(), {FunKit::ObjectType::FDOp, {{psi, 4}}});
  master.insert(master.begin(), {FunKit::ObjectType::FDOp, {{psibar, 3}}});
  FunKit::FEq eq = {master};

  FunKit::resolve_derivatives(setup, eq);
  FunKit::truncate(setup, eq);

  REQUIRE_FALSE(eq.empty());
  for (const auto &term : eq) {
    REQUIRE_FALSE(FunKit::has_AnyField(term));
    REQUIRE_FALSE(contains_type(term, FunKit::ObjectType::FMinus));
    REQUIRE_FALSE(contains_type(term, FunKit::ObjectType::gamma));
    REQUIRE_FALSE(contains_type(term, FunKit::ObjectType::FDOp));
    for (const auto &obj : term) {
      if (!setup.is_correlationFunction(obj.type)) continue;
      REQUIRE(setup.truncation.in_truncation(obj));
    }
  }
}
