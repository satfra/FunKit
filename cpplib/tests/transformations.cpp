#include <catch2/catch_test_macros.hpp>

#include <filesystem>
#include <fstream>

#include "funkit.hpp"

namespace
{
  std::string write_tmp(const std::string &name, const std::string &content)
  {
    const auto path = (std::filesystem::temp_directory_path() / name).string();
    std::ofstream file(path);
    file << content;
    return path;
  }
} // namespace

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

TEST_CASE("Normalize keeps unordered trailing legs pinned", "[transformations][unordered]")
{
  auto [setup, feq] = FunKit::parse(write_tmp("funkit_norm_unordered.toml", R"(
    equation = [ ]
    [setup]
    correlators = [ "Phidot" ]
    ordered = [ "R" ]
    [setup.unordered]
    Phidot = 1
    [[setup.cFields]]
    phi = [ ]
    [[setup.gFields]]
    psibar = [ ]
    psi = [ ]
  )"));
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");
  const FunKit::KeyT phidot = setup.type_to_idx("Phidot");

  // The head legs sort canonically, the tail leg stays last even though it
  // would sort first
  FunKit::Object pd = {phidot, {{phi, -5}, {phi, -3}, {phi, 1}}};
  REQUIRE(FunKit::normalize(setup, pd) == 1.);
  REQUIRE(pd.legs[0] == FunKit::LegT{phi, -5});
  REQUIRE(pd.legs[1] == FunKit::LegT{phi, -3});
  REQUIRE(pd.legs[2] == FunKit::LegT{phi, 1});

  FunKit::Object pd2 = {phidot, {{phi, -3}, {phi, -5}, {phi, 1}}};
  REQUIRE(FunKit::normalize(setup, pd2) == 1.);
  REQUIRE(pd2.legs[0] == FunKit::LegT{phi, -5});
  REQUIRE(pd2.legs[2] == FunKit::LegT{phi, 1});

  // Grassmann head legs still accumulate their commutation sign
  FunKit::Object gpd = {phidot, {{psi, -3}, {psibar, -5}, {phi, 1}}};
  const double sign = FunKit::normalize(setup, gpd);
  REQUIRE(sign == -1.);
  REQUIRE(gpd.legs[0] == FunKit::LegT{psibar, -5});
  REQUIRE(gpd.legs[1] == FunKit::LegT{psi, -3});
  REQUIRE(gpd.legs[2] == FunKit::LegT{phi, 1});

  // A single-leg Phidot (the tail is the only leg) is untouched
  FunKit::Object pd1 = {phidot, {{phi, 7}}};
  REQUIRE(FunKit::normalize(setup, pd1) == 1.);
  REQUIRE(pd1.legs[0] == FunKit::LegT{phi, 7});

  // Fully ordered types are unaffected
  FunKit::Object r = {setup.type_to_idx("R"), {{phi, -3}, {phi, -5}}};
  FunKit::normalize(setup, r);
  REQUIRE(r.legs[0] == FunKit::LegT{phi, -5});
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
