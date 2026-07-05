#include <catch2/catch_test_macros.hpp>

#include <sstream>

#include "funkit.hpp"

TEST_CASE("Print objects", "[io]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  FunKit::Object prop = {FunKit::ObjectType::Propagator, {{phi, 1}, {phi, 2}}};
  std::string str;
  FunKit::print(setup, prop, str);
  REQUIRE(str == "Propagator[{phi,phi},{a,b}]");

  // Custom types and lower indices (lower indices print with a minus)
  FunKit::Object rdot = {setup.type_to_idx("Rdot"), {{phi, -1}, {phi, -2}}};
  str.clear();
  FunKit::print(setup, rdot, str);
  REQUIRE(str == "Rdot[{phi,phi},{-a,-b}]");

  // The ostream overload must produce the same output
  std::ostringstream oss;
  FunKit::print(setup, prop, oss);
  REQUIRE(oss.str() == "Propagator[{phi,phi},{a,b}]");

  FunKit::Object unknown = {99, {{phi, 1}}};
  REQUIRE_THROWS(FunKit::print(setup, unknown, str));
}

TEST_CASE("Print terms and equations", "[io]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  FunKit::FTerm term;
  term.value = 0.5;
  term.push_back({FunKit::ObjectType::Propagator, {{phi, 1}, {phi, 2}}});
  term.push_back({setup.type_to_idx("Rdot"), {{phi, -1}, {phi, -2}}});

  std::string term_str;
  FunKit::print(setup, term, term_str);
  REQUIRE(term_str == "FTerm[0.5,Propagator[{phi,phi},{a,b}],Rdot[{phi,phi},{-a,-b}]]");

  FunKit::FEq eq = {term, term};
  std::string feq_str;
  FunKit::print(setup, eq, feq_str);
  REQUIRE(feq_str == "FEq[\n  " + term_str + ",\n  " + term_str + "\n ]");

  // The ostream overload must produce the same output
  std::ostringstream oss;
  FunKit::print(setup, eq, oss);
  REQUIRE(oss.str() == feq_str);
}

TEST_CASE("Print setup", "[io]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  std::ostringstream oss;
  FunKit::print(setup, oss);
  const std::string str = oss.str();

  REQUIRE_FALSE(str.empty());
  REQUIRE(str.find("phi") != std::string::npos);
  REQUIRE(str.find("psibar") != std::string::npos);
  REQUIRE(str.find("Rdot") != std::string::npos);
}
