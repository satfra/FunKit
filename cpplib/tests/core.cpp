#include <catch2/catch_test_macros.hpp>

#include <algorithm>

#include "funkit.hpp"

TEST_CASE("Field indexing", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  // Roundtrip for all fields; phi is unpaired, so index 1 must not resolve
  REQUIRE(setup.field_to_idx("phi") == 0);
  REQUIRE(setup.field_to_idx("psibar") == 2);
  REQUIRE(setup.field_to_idx("psi") == 3);
  REQUIRE(setup.idx_to_field(0) == "phi");
  REQUIRE(setup.idx_to_field(2) == "psibar");
  REQUIRE(setup.idx_to_field(3) == "psi");
  REQUIRE_THROWS(setup.idx_to_field(1));

  // AnyField
  REQUIRE(setup.field_to_idx("AnyField") == FunKit::AnyField);
  REQUIRE(setup.idx_to_field(FunKit::AnyField) == "AnyField");

  // Unknown fields
  REQUIRE_THROWS(setup.field_to_idx("chi"));
  REQUIRE_THROWS(setup.idx_to_field(4));
}

TEST_CASE("cField/gField classification", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  REQUIRE(setup.is_cField(0));
  REQUIRE_FALSE(setup.is_cField(2));
  REQUIRE(setup.is_gField(2));
  REQUIRE(setup.is_gField(3));

  // phi has no partner, so its padding index is invalid
  REQUIRE_THROWS(setup.is_cField(1));
  // Out of range
  REQUIRE_THROWS(setup.is_cField(4));
}

TEST_CASE("Type indexing", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");

  // Predefined types
  REQUIRE(setup.type_to_idx("FDOp") == FunKit::ObjectType::FDOp);
  REQUIRE(setup.type_to_idx("FMinus") == FunKit::ObjectType::FMinus);
  REQUIRE(setup.type_to_idx("Propagator") == FunKit::ObjectType::Propagator);
  REQUIRE(setup.type_to_idx("GammaN") == FunKit::ObjectType::GammaN);
  // Custom types start after the predefined correlation functions
  REQUIRE(setup.type_to_idx("Rdot") == FunKit::predef_correlation_functions);
  REQUIRE(setup.idx_to_type(setup.type_to_idx("Rdot")) == "Rdot");
  REQUIRE(setup.idx_to_type(FunKit::ObjectType::GammaN) == "GammaN");

  REQUIRE_THROWS(setup.type_to_idx("Unknown"));
  REQUIRE_THROWS(setup.idx_to_type(99));

  // Rdot is ordered and indexed, but not a correlation function
  const FunKit::KeyT rdot = setup.type_to_idx("Rdot");
  REQUIRE_FALSE(setup.is_correlationFunction(rdot));
  REQUIRE(setup.is_orderedObject(rdot));
  REQUIRE(setup.is_indexedObject(rdot));

  REQUIRE(setup.is_nonCommutingObject(FunKit::ObjectType::Field));
  REQUIRE(setup.is_nonCommutingObject(FunKit::ObjectType::FDOp));
  REQUIRE(setup.is_nonCommutingObject(FunKit::ObjectType::Propagator));
  REQUIRE(setup.is_nonCommutingObject(FunKit::ObjectType::GammaN));
  REQUIRE_FALSE(setup.is_nonCommutingObject(rdot));
  REQUIRE_FALSE(setup.is_nonCommutingObject(FunKit::ObjectType::FMinus));
}

TEST_CASE("sidx_to_string", "[core]")
{
  REQUIRE(FunKit::sidx_to_string(1) == "a");
  REQUIRE(FunKit::sidx_to_string(26) == "z");
  REQUIRE(FunKit::sidx_to_string(27) == "a1");
  // Lower indices are prefixed with a minus
  REQUIRE(FunKit::sidx_to_string(-3) == "-c");
  REQUIRE_THROWS(FunKit::sidx_to_string(0));
}

TEST_CASE("fresh_sidx", "[core]")
{
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{0, 1}, {0, 4}}});

  REQUIRE(FunKit::fresh_sidx(term) == FunKit::LegT{FunKit::AnyField, 5});
  // Extra legs are taken into account
  REQUIRE(FunKit::fresh_sidx(term, FunKit::LegT{0, 7}) == FunKit::LegT{FunKit::AnyField, 8});

  // Index names are the magnitudes: a name that currently occurs only as a lower
  // index still blocks that value. Handing out 10 here would silently contract the
  // fresh index with the existing -10 leg.
  FunKit::FTerm lower;
  lower.push_back({FunKit::ObjectType::GammaN, {{0, -9}, {0, -10}}});
  REQUIRE(FunKit::fresh_sidx(lower) == FunKit::LegT{FunKit::AnyField, 11});
  REQUIRE(FunKit::fresh_sidx(lower, FunKit::LegT{0, -12}) == FunKit::LegT{FunKit::AnyField, 13});

  // The all-lower case must still produce a usable (non-zero) index
  FunKit::FTerm single;
  single.push_back({FunKit::ObjectType::Field, {{0, -1}}});
  REQUIRE(FunKit::fresh_sidx(single).second > 0);
}

TEST_CASE("has_FDOp", "[core]")
{
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{0, 1}, {0, 2}}});
  REQUIRE_FALSE(FunKit::has_FDOp(term));

  FunKit::FTerm fdop_term = term;
  fdop_term.insert(fdop_term.begin(), {FunKit::ObjectType::FDOp, {{0, 3}}});
  REQUIRE(FunKit::has_FDOp(fdop_term));

  FunKit::FEq feq = {term};
  REQUIRE_FALSE(FunKit::has_FDOp(feq));
  feq.push_back(fdop_term);
  REQUIRE(FunKit::has_FDOp(feq));
}

TEST_CASE("Truncation queries", "[core][truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  REQUIRE(setup.truncation.in_truncation({FunKit::ObjectType::GammaN, {{phi, 0}, {phi, 1}, {phi, 2}}}));
  REQUIRE_FALSE(setup.truncation.in_truncation(
      {FunKit::ObjectType::GammaN, {{phi, 0}, {phi, 1}, {phi, 2}, {phi, 3}, {phi, 4}}}));
  REQUIRE(setup.truncation.in_truncation({setup.type_to_idx("Rdot"), {{phi, 0}, {phi, 0}}}));
  // No rules for a type means everything is allowed
  REQUIRE(setup.truncation.in_truncation({FunKit::ObjectType::Field, {{phi, 0}}}));

  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::GammaN) == 4);
  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::Propagator) == 2);
  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::Field) == 1);

  REQUIRE_THROWS(setup.truncation.in_truncation({99, {{phi, 0}}}));
  REQUIRE_THROWS(setup.truncation.max_truncation(99));

  // Un-updated truncation tables must not be usable
  FunKit::Truncation empty;
  REQUIRE_THROWS(empty.add_rule(FunKit::ObjectType::GammaN, {phi}));
  REQUIRE_THROWS(empty.in_truncation({FunKit::ObjectType::GammaN, {{phi, 0}}}));
}

TEST_CASE("has_AnyField", "[core]")
{
  FunKit::Object obj = {FunKit::ObjectType::Propagator, {{0, 1}, {0, 2}}};
  REQUIRE_FALSE(FunKit::has_AnyField(obj));
  obj.legs[1].first = FunKit::AnyField;
  REQUIRE(FunKit::has_AnyField(obj));

  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{0, 1}, {0, 2}}});
  REQUIRE_FALSE(FunKit::has_AnyField(term));
  term.push_back(obj);
  REQUIRE(FunKit::has_AnyField(term));
}

TEST_CASE("has_partner", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  REQUIRE_FALSE(setup.has_partner(setup.field_to_idx("phi")));
  REQUIRE(setup.has_partner(setup.field_to_idx("psibar")));
  REQUIRE(setup.has_partner(setup.field_to_idx("psi")));

  // Padding index of the unpaired phi and out-of-range indices must throw
  REQUIRE_THROWS(setup.has_partner(1));
  REQUIRE_THROWS(setup.has_partner(4));
  REQUIRE_THROWS(setup.has_partner(FunKit::AnyField));

  // A paired cField followed by an unpaired one: the lookup must stay within the pair
  FunKit::Setup s;
  s.cFields = {{FunKit::Field{"a", {}}, FunKit::Field{"abar", {}}}, {FunKit::Field{"b", {}}, FunKit::Field{}}};
  // Hand-built setups must build the field property table before lookups
  REQUIRE_THROWS(s.has_partner(0));
  s.finalize_fields();
  REQUIRE(s.has_partner(0));
  REQUIRE(s.has_partner(1));
  REQUIRE_FALSE(s.has_partner(2));
  REQUIRE_THROWS(s.has_partner(3));
}

TEST_CASE("partner_field", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  // Unpaired fields are their own partner
  REQUIRE(setup.partner_field(phi) == phi);
  REQUIRE(setup.partner_field(psibar) == psi);
  REQUIRE(setup.partner_field(psi) == psibar);
}

TEST_CASE("Field-space metric gamma", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  // gamma_a^b = delta_a^b: 1 for the same field, 0 otherwise (also for partners)
  REQUIRE(setup.gamma({phi, -1}, {phi, 2}) == 1);
  REQUIRE(setup.gamma({psi, -1}, {psi, 2}) == 1);
  REQUIRE(setup.gamma({psibar, -1}, {psibar, 2}) == 1);
  REQUIRE(setup.gamma({psibar, -1}, {psi, 2}) == 0);
  REQUIRE(setup.gamma({psi, -1}, {psibar, 2}) == 0);
  REQUIRE(setup.gamma({phi, -1}, {psi, 2}) == 0);

  // gamma^a_b = (-1)^{ab} delta^a_b: a sign for a Grassmann field
  REQUIRE(setup.gamma({psi, 1}, {psi, -2}) == -1);
  REQUIRE(setup.gamma({phi, 1}, {phi, -2}) == 1);

  // gamma_ab = gamma^ab: only partner fields, with the Grassmann ordering sign
  // Convention: gamma^{psibar psi} = gamma_{psibar psi} = +1
  REQUIRE(setup.gamma({psibar, 1}, {psi, 2}) == 1);
  REQUIRE(setup.gamma({psi, 1}, {psibar, 2}) == -1);
  REQUIRE(setup.gamma({psibar, -1}, {psi, -2}) == 1);
  REQUIRE(setup.gamma({psi, -1}, {psibar, -2}) == -1);
  // An unpaired commuting field is its own partner
  REQUIRE(setup.gamma({phi, 1}, {phi, 2}) == 1);
  REQUIRE(setup.gamma({phi, -1}, {phi, -2}) == 1);
  // A paired field is not partnered with itself at equal positions
  REQUIRE(setup.gamma({psi, 1}, {psi, 2}) == 0);
  // Partner fields with mixed positions vanish
  REQUIRE(setup.gamma({psibar, 1}, {psi, -2}) == 0);

  // Paired commuting fields behave like the Grassmann pair without signs
  FunKit::Setup paired;
  paired.cFields = {{FunKit::Field{"a", {}}, FunKit::Field{"abar", {}}}};
  paired.finalize_fields();
  REQUIRE(paired.gamma({0, -1}, {0, 2}) == 1);
  REQUIRE(paired.gamma({0, 1}, {1, 2}) == 1);
  REQUIRE(paired.gamma({1, -1}, {0, -2}) == 1);
  REQUIRE(paired.gamma({0, -1}, {1, 2}) == 0);
  REQUIRE(paired.gamma({0, 1}, {0, 2}) == 0);
}

TEST_CASE("all_fields", "[core][setup]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  // The padding index of the unpaired phi must be skipped
  REQUIRE(setup.all_fields() == std::vector<FunKit::FieldIdx>{0, 2, 3});
}

TEST_CASE("Truncation rule tables", "[core][truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  // Rules are stored sorted by field index
  const auto &prop_rules = setup.truncation.truncation_rules(FunKit::ObjectType::Propagator);
  REQUIRE(prop_rules.size() == 2);
  for (const auto &rule : prop_rules)
    REQUIRE(std::is_sorted(rule.begin(), rule.end()));

  // Order-filtered rules contain all distinct permutations
  const auto perms = setup.truncation.truncation_rules(FunKit::ObjectType::Propagator, 2);
  REQUIRE(perms.size() == 3);
  REQUIRE(std::find(perms.begin(), perms.end(), std::vector<FunKit::KeyT>{phi, phi}) != perms.end());
  REQUIRE(std::find(perms.begin(), perms.end(), std::vector<FunKit::KeyT>{psibar, psi}) != perms.end());
  REQUIRE(std::find(perms.begin(), perms.end(), std::vector<FunKit::KeyT>{psi, psibar}) != perms.end());

  REQUIRE(setup.truncation.truncation_rules(FunKit::ObjectType::GammaN, 1) ==
          std::vector<std::vector<FunKit::KeyT>>{{phi}});
  // {psi, psibar, phi} has three distinct fields: 6 permutations
  REQUIRE(setup.truncation.truncation_rules(FunKit::ObjectType::GammaN, 3).size() == 6);

  // A single empty rule means "nothing accepted": orders beyond the truncation, gap orders, and Fields
  // of order != 1. An empty rule list means "everything accepted" (unrestricted).
  const std::vector<std::vector<FunKit::FieldIdx>> nothing = {{}};
  REQUIRE(setup.truncation.truncation_rules(FunKit::ObjectType::GammaN, 5) == nothing);
  REQUIRE(setup.truncation.truncation_rules(FunKit::ObjectType::Field, 2) == nothing);

  // Gap order: rules of orders 1 and 3 only, so order 2 accepts nothing
  FunKit::Truncation gap;
  gap.initialize(setup);
  gap.add_rule(FunKit::ObjectType::GammaN, {phi});
  gap.add_rule(FunKit::ObjectType::GammaN, {phi, phi, phi});
  gap.finalize();
  REQUIRE(gap.truncation_rules(FunKit::ObjectType::GammaN, 2) == nothing);
  // Unrestricted type: empty rule list
  REQUIRE(gap.truncation_rules(FunKit::ObjectType::Propagator, 2).empty());
  // All ordered field pairs for the unrestricted expansion are precomputed
  REQUIRE(gap.all_field_pairs().size() == 9);

  REQUIRE_THROWS(setup.truncation.truncation_rules(99));
  FunKit::Truncation empty;
  REQUIRE_THROWS(empty.truncation_rules(FunKit::ObjectType::GammaN));

  // in_truncation must not depend on the query order
  REQUIRE(setup.truncation.in_truncation({FunKit::ObjectType::GammaN, {{psi, 0}, {psibar, 1}, {phi, 2}}}));
}

TEST_CASE("Truncation add_rule validation", "[core][truncation]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx psibar = setup.field_to_idx("psibar");
  const FunKit::FieldIdx psi = setup.field_to_idx("psi");

  FunKit::Truncation tr;
  tr.initialize(setup);

  // Rules that only differ by ordering are the same rule and must not double-count
  tr.add_rule(FunKit::ObjectType::Propagator, {psi, psibar});
  tr.add_rule(FunKit::ObjectType::Propagator, {psibar, psi});

  auto _tr = tr;
  _tr.finalize();
  REQUIRE(_tr.truncation_rules(FunKit::ObjectType::Propagator).size() == 1);

  // Field rules take exactly one field
  REQUIRE_THROWS(tr.add_rule(FunKit::ObjectType::Field, {phi, phi}));
  tr.add_rule(FunKit::ObjectType::Field, {phi});
  _tr = tr;
  _tr.finalize();
  REQUIRE(_tr.truncation_rules(FunKit::ObjectType::Field).size() == 1);

  // AnyField makes no sense in a truncation rule
  REQUIRE_THROWS(tr.add_rule(FunKit::ObjectType::Propagator, {FunKit::AnyField, phi}));

  // Cannot add rules to a finalized truncation
  REQUIRE_THROWS(_tr.add_rule(FunKit::ObjectType::Propagator, {phi, phi}));

  // An empty rule ("nothing accepted") cannot be combined with other rules
  REQUIRE_THROWS(tr.add_rule(FunKit::ObjectType::Propagator, {}));
  FunKit::Truncation full;
  full.initialize(setup);
  full.add_rule(FunKit::ObjectType::GammaN, {});
  REQUIRE_THROWS(full.add_rule(FunKit::ObjectType::GammaN, {phi}));
}

TEST_CASE("Symmetries add/finalize/accessors", "[core][symmetries]")
{
  FunKit::Symmetries syms;

  // Querying before finalize() must throw
  REQUIRE_THROWS(syms.empty());
  REQUIRE_THROWS(syms.size());
  REQUIRE_THROWS(syms.all());

  syms.add(FunKit::Symmetry{{{1, 2}}, -1});
  syms.add(FunKit::Symmetry{{{1, 2, 3}}, 1});
  syms.add(FunKit::Symmetry{{{1, 2}}, -1}); // exact duplicate, dropped on finalize
  syms.finalize();

  REQUIRE_FALSE(syms.empty());
  REQUIRE(syms.size() == 2); // duplicate removed
  REQUIRE(syms.all()[0].factor == -1);
  REQUIRE(syms.all()[1].cycles == std::vector<std::vector<FunKit::Idx>>{{1, 2, 3}});

  // No adding after finalize, no double finalize
  REQUIRE_THROWS(syms.add(FunKit::Symmetry{{{4, 5}}, 1}));
  REQUIRE_THROWS(syms.finalize());
}

TEST_CASE("Symmetries validation", "[core][symmetries]")
{
  const auto rejects = [](FunKit::Symmetry sym) {
    FunKit::Symmetries s;
    REQUIRE_THROWS(s.add(std::move(sym)));
  };

  rejects(FunKit::Symmetry{{{1, 2}}, 2});     // factor not +-1
  rejects(FunKit::Symmetry{{{1, 2}}, 0});     // factor not +-1
  rejects(FunKit::Symmetry{{}, 1});           // empty cycles list
  rejects(FunKit::Symmetry{{{1}}, 1});        // singleton cycle
  rejects(FunKit::Symmetry{{{0, 1}}, 1});     // non-positive label
  rejects(FunKit::Symmetry{{{-1, 1}}, 1});    // non-positive label
  rejects(FunKit::Symmetry{{{1, 2}, {2, 3}}, 1}); // overlapping cycles

  // A symmetry with several disjoint cycles is fine
  FunKit::Symmetries ok;
  ok.add(FunKit::Symmetry{{{1, 2}, {3, 4}}, 1});
  ok.finalize();
  REQUIRE(ok.size() == 1);
}

TEST_CASE("is_close", "[core]")
{
  REQUIRE(FunKit::is_close(1., 1.));
  REQUIRE(FunKit::is_close(0.1 + 0.2, 0.3));
  REQUIRE(FunKit::is_close(1., std::nextafter(1., 2.)));
  REQUIRE_FALSE(FunKit::is_close(1., 2.));
  REQUIRE_FALSE(FunKit::is_close(0., 1e-10));
}
