#include <catch2/catch_test_macros.hpp>

#include <algorithm>
#include <sstream>

#include "funkit.hpp"
#include "nlohmann/json.hpp"

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

TEST_CASE("Print Field and gamma objects", "[io][field]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  // The name maps expose the built-in internal types
  REQUIRE(setup.type_to_idx("Field") == FunKit::ObjectType::Field);
  REQUIRE(setup.idx_to_type(FunKit::ObjectType::Field) == "Field");
  REQUIRE(setup.idx_to_type(FunKit::ObjectType::gamma) == "gamma");
  REQUIRE(setup.idx_to_type(FunKit::ObjectType::SymmFactor) == "SymmFactor");

  FunKit::Object field = {FunKit::ObjectType::Field, {{phi, 1}}};
  std::string str;
  FunKit::print(setup, field, str);
  REQUIRE(str == "Field[{phi},{a}]");

  FunKit::Object gamma = {FunKit::ObjectType::gamma, {{phi, -1}, {phi, 2}}};
  str.clear();
  FunKit::print(setup, gamma, str);
  REQUIRE(str == "gamma[{phi,phi},{-a,b}]");
}

TEST_CASE("Print JSON equations", "[io][json]")
{
  auto [setup, feq_in] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  const FunKit::FieldIdx phi = setup.field_to_idx("phi");

  FunKit::FTerm term;
  term.value = 0.5;
  term.push_back({FunKit::ObjectType::Propagator, {{phi, 1}, {phi, 2}}});
  term.push_back({setup.type_to_idx("Rdot"), {{phi, -1}, {phi, -2}}});
  FunKit::FEq eq = {term, term};

  std::string str;
  FunKit::print_json(setup, eq, str);

  // Must be valid JSON with the documented output schema
  const auto data = nlohmann::json::parse(str);
  REQUIRE(data["funkit_output_version"] == 1);
  REQUIRE(data["input_file"].is_string());
  REQUIRE(data["stages"]["derivatives"] == true);
  REQUIRE(data["stages"]["truncate"] == setup.do_truncate);
  REQUIRE(data["stages"]["simplify"] == setup.do_simplify);
  REQUIRE(data["equation"].size() == 2);
  const auto &t = data["equation"][0];
  REQUIRE(t[0]["prefactor"] == 0.5);
  REQUIRE(t[1]["type"] == "Propagator");
  REQUIRE(t[1]["legs"] == nlohmann::json::parse(R"([["phi",1],["phi",2]])"));
  REQUIRE(t[2]["type"] == "Rdot");
  REQUIRE(t[2]["legs"] == nlohmann::json::parse(R"([["phi",-1],["phi",-2]])"));

  // The ostream overload must produce the same output
  std::ostringstream oss;
  FunKit::print_json(setup, eq, oss);
  REQUIRE(oss.str() == str);

  // An empty equation is still valid JSON
  str.clear();
  FunKit::print_json(setup, FunKit::FEq{}, str);
  REQUIRE(nlohmann::json::parse(str)["equation"].empty());
}

TEST_CASE("JSON output round-trips the full scalar flow", "[io][json]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar-flow.toml");
  FunKit::resolve_derivatives(setup, feq);
  FunKit::truncate(setup, feq);
  FunKit::simplify(setup, feq);

  std::string out;
  FunKit::print_json(setup, feq, out);
  const auto data = nlohmann::json::parse(out);

  // Scalar 2-point flow: polarization (+1) and tadpole (-1/2)
  REQUIRE(data["equation"].size() == 2);
  std::vector<double> prefs;
  for (const auto &t : data["equation"])
    prefs.push_back(t[0]["prefactor"].get<double>());
  std::sort(prefs.begin(), prefs.end());
  REQUIRE(prefs == std::vector<double>{-0.5, 1.0});

  // Semantic round-trip: mapping every emitted name back through the setup's
  // name maps must reproduce the in-memory equation exactly
  const auto &eq = data["equation"];
  REQUIRE(eq.size() == feq.size());
  for (size_t i = 0; i < feq.size(); ++i) {
    REQUIRE(eq[i][0]["prefactor"].get<double>() == feq[i].value);
    REQUIRE(eq[i].size() == feq[i].size() + 1);
    for (size_t j = 0; j < feq[i].size(); ++j) {
      const auto &obj = eq[i][j + 1];
      REQUIRE(setup.type_to_idx(obj["type"].get<std::string>()) == feq[i][j].type);
      REQUIRE(obj["legs"].size() == feq[i][j].legs.size());
      for (size_t k = 0; k < feq[i][j].legs.size(); ++k) {
        REQUIRE(setup.field_to_idx(obj["legs"][k][0].get<std::string>()) == feq[i][j].legs[k].first);
        REQUIRE(obj["legs"][k][1].get<FunKit::Idx>() == feq[i][j].legs[k].second);
      }
    }
  }
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
