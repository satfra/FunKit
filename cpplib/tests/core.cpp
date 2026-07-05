#include <catch2/catch_test_macros.hpp>

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
  // Lower indices map to the same letters
  REQUIRE(FunKit::sidx_to_string(-3) == "c");
  REQUIRE_THROWS(FunKit::sidx_to_string(0));
}

TEST_CASE("fresh_sidx", "[core]")
{
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::Propagator, {{0, 1}, {0, 4}}});

  REQUIRE(FunKit::fresh_sidx(term) == FunKit::LegT{FunKit::AnyField, 5});
  // Extra legs are taken into account
  REQUIRE(FunKit::fresh_sidx(term, FunKit::LegT{0, 7}) == FunKit::LegT{FunKit::AnyField, 8});
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

  REQUIRE(setup.truncation.in_truncation(FunKit::ObjectType::GammaN, {phi, phi, phi}));
  REQUIRE_FALSE(setup.truncation.in_truncation(FunKit::ObjectType::GammaN, {phi, phi, phi, phi, phi}));
  REQUIRE(setup.truncation.in_truncation(setup.type_to_idx("Rdot"), {phi, phi}));
  // No rules for a type means everything is allowed
  REQUIRE(setup.truncation.in_truncation(FunKit::ObjectType::Field, {phi}));

  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::GammaN) == 4);
  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::Propagator) == 2);
  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::Field) == 1);

  REQUIRE_THROWS(setup.truncation.in_truncation(99, {phi}));
  REQUIRE_THROWS(setup.truncation.max_truncation(99));

  // Un-updated truncation tables must not be usable
  FunKit::Truncation empty;
  REQUIRE_THROWS(empty.add_rule(FunKit::ObjectType::GammaN, {phi}));
  REQUIRE_THROWS(empty.in_truncation(FunKit::ObjectType::GammaN, {phi}));
}

TEST_CASE("is_close", "[core]")
{
  REQUIRE(FunKit::is_close(1., 1.));
  REQUIRE(FunKit::is_close(0.1 + 0.2, 0.3));
  REQUIRE(FunKit::is_close(1., std::nextafter(1., 2.)));
  REQUIRE_FALSE(FunKit::is_close(1., 2.));
  REQUIRE_FALSE(FunKit::is_close(0., 1e-10));
}
