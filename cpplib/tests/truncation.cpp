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

  // Same Grassmann field: also a plain delta, no sign
  FunKit::FTerm gsame;
  gsame.push_back({FunKit::ObjectType::gamma, {{psi, -1}, {psi, 2}}});
  gsame.push_back({FunKit::ObjectType::GammaN, {{psibar, -2}, {phi, -3}}});
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

  const FunKit::FEq out = FunKit::truncate(setup, feq[0]);
  REQUIRE(out.size() == 1);
  REQUIRE(out[0].size() == feq[0].size());
  REQUIRE(out[0].value == feq[0].value);
  for (FunKit::Idx i = 0; i < out[0].size(); ++i) {
    REQUIRE(out[0][i].type == feq[0][i].type);
    REQUIRE(out[0][i].legs == feq[0][i].legs);
  }
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
    REQUIRE(fields_of(term, FunKit::ObjectType::Propagator) == fields_of(term, rdot));
    prop_legs.insert(fields_of(term, FunKit::ObjectType::Propagator));
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
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // (-1)^{aa} G^{ab}: the FMinus legs share the propagator's first index
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::FMinus, {{FunKit::AnyField, 1}, {FunKit::AnyField, 1}}});
  term.push_back({FunKit::ObjectType::Propagator, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});

  const FunKit::FEq out = FunKit::truncate(setup, term);
  REQUIRE(out.size() == 3);
  for (const auto &t : out) {
    REQUIRE(t.size() == 1); // FMinus resolved into the prefactor
    REQUIRE(t[0].type == FunKit::ObjectType::Propagator);
    const double expected = t[0].legs[0].first == phi ? 1. : -1.;
    REQUIRE(t.value == expected);
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
