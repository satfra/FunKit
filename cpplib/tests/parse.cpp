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

TEST_CASE("Parse scalar TOML", "[parse][toml]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");

  REQUIRE(setup.cFields.size() == 1);
  REQUIRE(setup.cFields[0].first.name == "phi");
  REQUIRE(setup.gFields.empty());
  REQUIRE(setup.objects == std::vector<std::string>{"Rdot"});
  REQUIRE(setup.is_orderedObject(setup.type_to_idx("Rdot")));

  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  REQUIRE(feq.size() == 1);
  REQUIRE(feq[0].value == 0.5);
  REQUIRE(feq[0].size() == 2);
  REQUIRE(feq[0][0].type == FunKit::ObjectType::Propagator);
  REQUIRE(feq[0][0].legs[0] == FunKit::LegT{phi, 1});
  REQUIRE(feq[0][0].legs[1] == FunKit::LegT{phi, 2});
  REQUIRE(feq[0][1].type == setup.type_to_idx("Rdot"));
  REQUIRE(feq[0][1].legs[0] == FunKit::LegT{phi, -1});
  REQUIRE(feq[0][1].legs[1] == FunKit::LegT{phi, -2});

  REQUIRE(setup.truncation.max_truncation(FunKit::ObjectType::GammaN) == 4);
}

TEST_CASE("Parse Yukawa TOML", "[parse][toml]")
{
  auto [setup, feq] = FunKit::parse(BOILERPLATE_DIR + "yukawa.toml");

  REQUIRE(setup.gFields.size() == 1);
  REQUIRE(setup.gFields[0].first.name == "psibar");
  REQUIRE(setup.gFields[0].second.name == "psi");

  // The trace legs run over the full field space
  REQUIRE(feq[0][0].legs[0].first == FunKit::AnyField);
  REQUIRE(feq[0][1].legs[0].first == FunKit::AnyField);
}

TEST_CASE("Parse scalar JSON", "[parse][json]")
{
  const auto path = write_tmp("funkit_scalar.json", R"({
    "setup": {
      "debug": 0,
      "ordered": [ "Rdot" ],
      "cFields": [ { "phi": [] } ],
      "truncation": {
        "Rdot": [ [ "phi", "phi" ] ],
        "Propagator": [ [ "phi", "phi" ] ],
        "GammaN": [ [ "phi" ], [ "phi", "phi" ], [ "phi", "phi", "phi" ], [ "phi", "phi", "phi", "phi" ] ]
      }
    },
    "equation": [ [
      { "prefactor": 0.5 },
      { "type": "Propagator", "legs": [ [ "phi", 1 ], [ "phi", 2 ] ] },
      { "type": "Rdot", "legs": [ [ "phi", -1 ], [ "phi", -2 ] ] }
    ] ]
  })");
  auto [setup_j, feq_j] = FunKit::parse(path);
  auto [setup_t, feq_t] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");

  // The JSON mirror must parse to the same structure as the TOML original
  REQUIRE(setup_j.objects == setup_t.objects);
  REQUIRE(setup_j.cFields.size() == setup_t.cFields.size());
  REQUIRE(setup_j.truncation.max_truncation(FunKit::ObjectType::GammaN) ==
          setup_t.truncation.max_truncation(FunKit::ObjectType::GammaN));

  REQUIRE(feq_j.size() == feq_t.size());
  for (size_t i = 0; i < feq_j.size(); ++i) {
    REQUIRE(feq_j[i].value == feq_t[i].value);
    REQUIRE(feq_j[i].size() == feq_t[i].size());
    for (size_t j = 0; j < feq_j[i].size(); ++j) {
      REQUIRE(feq_j[i][j].type == feq_t[i][j].type);
      REQUIRE(feq_j[i][j].legs == feq_t[i][j].legs);
    }
  }
}

TEST_CASE("Parse rejects malformed input", "[parse][robustness]")
{
  // Missing sections
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_no_setup.toml", R"(
    equation = [ [ { type = "Propagator", legs = [ [ "phi", 1 ], [ "phi", 2 ] ] } ] ]
  )")));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_no_eq.toml", R"(
    [setup]
    debug = 0
  )")));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_no_setup.json", R"({ "equation": [] })")));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_no_eq.json", R"({ "setup": {} })")));

  const std::string setup_str = R"(
    [setup]
    debug = 0
    [[setup.cFields]]
    phi = [ ]
  )";

  // Objects without type or legs, malformed legs
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_no_type.toml",
    "equation = [ [ { legs = [ [ \"phi\", 1 ] ] } ] ]\n" + setup_str)));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_no_legs.toml",
    "equation = [ [ { type = \"Propagator\" } ] ]\n" + setup_str)));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_bad_legs.toml",
    "equation = [ [ { type = \"Propagator\", legs = [ [ \"phi\" ] ] } ] ]\n" + setup_str)));

  // Malformed truncation rules
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_bad_trunc.toml",
    "equation = [ ]\n" + setup_str + "[setup.truncation]\nGammaN = 5\n")));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_unknown_field_trunc.toml",
    "equation = [ ]\n" + setup_str + "[setup.truncation]\nGammaN = [ [ \"chi\" ] ]\n")));

  // Unsupported file format
  REQUIRE_THROWS(FunKit::parse("setup.yaml"));
}
