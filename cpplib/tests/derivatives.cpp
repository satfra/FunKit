#include <catch2/catch_test_macros.hpp>
#include <map>

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

TEST_CASE("propagator derivative picks indices that are fresh in magnitude", "[derivative][indices]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const auto phi = setup.field_to_idx("phi");

  // A term in which the largest index name, 5, occurs only as a *lower* index —
  // exactly what an earlier derivative leaves behind (d/dphi^f Phi^b -> gamma_f^b
  // puts the derivative label f on a lower leg). The two fresh indices of the
  // propagator derivative must not reuse 4 or 5.
  FunKit::FTerm term;
  term.push_back({FunKit::ObjectType::FDOp, {{phi, 3}}});
  term.push_back({FunKit::ObjectType::Propagator, {{phi, 1}, {phi, 2}}});
  term.push_back({FunKit::ObjectType::gamma, {{phi, -4}, {phi, -5}}});

  const FunKit::FTerm result = FunKit::functionalD(setup, term, 0);
  REQUIRE_FALSE(result.empty());

  // No index name may occur more than twice: one upper, one lower. The symbolic
  // sign factors are not contractions — their legs only reference indices carried
  // elsewhere — so they are excluded from the tally, as in prune/canonicalize.
  std::map<FunKit::Idx, int> counts;
  for (const auto &obj : result) {
    if (obj.type == FunKit::ObjectType::FMinus || obj.type == FunKit::ObjectType::SymmFactor) continue;
    for (const auto &leg : obj.legs)
      ++counts[std::abs(leg.second)];
  }
  for (const auto &[name, count] : counts)
    REQUIRE(count <= 2);

  // Concretely: the fresh pair must sit above the existing name 5, not on it.
  REQUIRE(counts.count(4) == 1); // still just the gamma leg
  REQUIRE(counts[4] == 1);
  REQUIRE(counts[5] == 1);
}