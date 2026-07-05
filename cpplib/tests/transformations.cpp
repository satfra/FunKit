#include <catch2/catch_test_macros.hpp>

#include "funkit.hpp"

TEST_CASE("Reduce", "[transformations]")
{
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{0, 1}, {0, 2}}});

  // Nonzero terms are untouched
  FunKit::FTerm kept = term;
  FunKit::reduce(kept);
  REQUIRE(kept.size() == 1);

  // Zero-prefactor terms are cleared
  FunKit::FTerm dropped = term;
  dropped.value = 0.;
  FunKit::reduce(dropped);
  REQUIRE(dropped.empty());

  // Cleared terms are pruned from the FEq
  FunKit::FEq feq = {term, term};
  feq[1].value = 0.;
  FunKit::reduce(feq);
  REQUIRE(feq.size() == 1);
}

TEST_CASE("Commutation signs", "[transformations]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // Commuting fields: no sign, no symbolic object
  auto [v_cc, o_cc] = FunKit::commute_sign(setup, {phi, 1}, {phi, 2});
  REQUIRE(v_cc == 1.);
  REQUIRE(o_cc.type == FunKit::ObjectType::None);

  // Two Grassmann fields: sign flip
  auto [v_gg, o_gg] = FunKit::commute_sign(setup, {psi, 1}, {psibar, 2});
  REQUIRE(v_gg == -1.);
  REQUIRE(o_gg.type == FunKit::ObjectType::None);

  // AnyField legs stay symbolic
  auto [v_any, o_any] = FunKit::commute_sign(setup, {FunKit::AnyField, 1}, {phi, 2});
  REQUIRE(v_any == 1.);
  REQUIRE(o_any.type == FunKit::ObjectType::FMinus);
  REQUIRE(o_any.legs.size() == 2);
}

TEST_CASE("Commute forward", "[transformations]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");

  // Commuting fields: plain swap
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{phi, 1}, {phi, 2}}});
  term.push_back({FunKit::ObjectType::GammaN, {{phi, 3}}});
  FunKit::commute_forward(setup, term, 0);
  REQUIRE(term.size() == 2);
  REQUIRE(term[0].type == FunKit::ObjectType::GammaN);
  REQUIRE(term.value == 1.);

  // Two Grassmann fields: swap picks up a sign
  FunKit::FTerm gterm;
  gterm.push_back({FunKit::ObjectType::Field, {{psi, 1}}});
  gterm.push_back({FunKit::ObjectType::Field, {{psibar, 2}}});
  FunKit::commute_forward(setup, gterm, 0);
  REQUIRE(gterm[0].legs[0].first == psibar);
  REQUIRE(gterm.value == -1.);

  // Ordered (commuting) objects pick up no sign, even with Grassmann legs
  FunKit::FTerm oterm;
  oterm.push_back({setup.type_to_idx("Rdot"), {{psi, -1}, {psibar, -2}}});
  oterm.push_back({FunKit::ObjectType::Field, {{psi, 1}}});
  FunKit::commute_forward(setup, oterm, 0);
  REQUIRE(oterm.value == 1.);

  // AnyField legs produce symbolic FMinus factors
  FunKit::FTerm aterm;
  aterm.push_back({FunKit::ObjectType::Propagator, {{FunKit::AnyField, 1}, {FunKit::AnyField, 2}}});
  aterm.push_back({FunKit::ObjectType::GammaN, {{FunKit::AnyField, 3}, {FunKit::AnyField, 4}}});
  FunKit::commute_forward(setup, aterm, 0);
  REQUIRE(aterm.size() == 6);
  REQUIRE(aterm[0].type == FunKit::ObjectType::FMinus);
  REQUIRE(aterm.value == 1.);

  // Can't commute past the end
  REQUIRE_THROWS(FunKit::commute_forward(setup, term, term.size() - 1));
}

TEST_CASE("Merge FEqs", "[transformations]")
{
  FunKit::FEq a(2), b(3);
  a[0].value = 1;
  a[1].value = 2;
  b[0].value = 3;
  b[1].value = 4;
  b[2].value = 5;

  FunKit::FEq merged = FunKit::merge_feq({a, b});
  REQUIRE(merged.size() == 5);
  for (size_t i = 0; i < merged.size(); ++i)
    REQUIRE(merged[i].value == i + 1);
}
