#include <catch2/catch_test_macros.hpp>

#include "funkit.hpp"

TEST_CASE("2-point scalar test", "[2-point][scalar][derivative]")
{
  // Parse the scalar setup
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");

  // Pre-pend the derivative operators to the first term of the FEq
  FunKit::FTerm &derivative_term = feq[0];
  FunKit::Object fdop1 = {FunKit::ObjectType::FDOp, {{setup.field_to_idx("phi"), 3}}};
  FunKit::Object fdop2 = {FunKit::ObjectType::FDOp, {{setup.field_to_idx("phi"), 4}}};
  derivative_term.insert(derivative_term.begin(), fdop2);
  derivative_term.insert(derivative_term.begin(), fdop1);

  // Compute the derivatives
  FunKit::FEq derivative_feq = FunKit::resolve_derivatives(setup, feq);

  // In general, this generates 2 polarization diagrams and 1 tadpole diagram.
  REQUIRE(derivative_feq.size() == 3);
}