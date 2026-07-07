#include <catch2/catch_test_macros.hpp>

#include <algorithm>
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

TEST_CASE("Parse output_format", "[parse]")
{
  const auto tpath = write_tmp("funkit_outfmt.toml", R"(
    equation = [ ]
    [setup]
    output_format = "json"
    [[setup.cFields]]
    phi = [ ]
  )");
  auto [setup_t, feq_t] = FunKit::parse(tpath);
  REQUIRE(setup_t.output_format == "json");

  const auto jpath = write_tmp("funkit_outfmt.json", R"({
    "setup": { "output_format": "json", "cFields": [ { "phi": [] } ] },
    "equation": []
  })");
  auto [setup_j, feq_j] = FunKit::parse(jpath);
  REQUIRE(setup_j.output_format == "json");

  // Default is empty (text output, unless outputFile ends in .json)
  auto [setup_d, feq_d] = FunKit::parse(BOILERPLATE_DIR + "scalar.toml");
  REQUIRE(setup_d.output_format.empty());
}

TEST_CASE("Parse and differentiate bare Field objects", "[parse][field]")
{
  // DSE-shaped input: a functional derivative acting on a bare field
  const auto path = write_tmp("funkit_field.toml", R"(
    equation = [ [
      { type = "FDOp", legs = [ [ "phi", 101 ] ] },
      { type = "Field", legs = [ [ "phi", 1 ] ] },
      { type = "Rdot", legs = [ [ "phi", -1 ], [ "phi", -2 ] ] }
    ] ]
    [setup]
    do_simplify = false
    ordered = [ "Rdot" ]
    [[setup.cFields]]
    phi = [ ]
    [setup.truncation]
    Rdot = [ [ "phi", "phi" ] ]
  )");
  auto [setup, feq] = FunKit::parse(path);

  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  REQUIRE(feq[0][1].type == FunKit::ObjectType::Field);
  REQUIRE(feq[0][1].legs[0] == FunKit::LegT{phi, 1});

  // d/dphi^101 (phi^1 Rdot_{12}): the Field derivative leaves a gamma...
  FunKit::resolve_derivatives(setup, feq);
  REQUIRE(feq.size() == 1);
  REQUIRE(feq[0].size() == 2);
  REQUIRE(feq[0][0].type == FunKit::ObjectType::gamma);
  REQUIRE(feq[0][0].legs[0] == FunKit::LegT{phi, -101});
  REQUIRE(feq[0][0].legs[1] == FunKit::LegT{phi, 1});

  // ...which truncation's prune contracts: gamma_101^1 Rdot_{12} = Rdot_{101,2}
  FunKit::truncate(setup, feq);
  REQUIRE(feq.size() == 1);
  REQUIRE(feq[0].value == 1.0);
  REQUIRE(feq[0].size() == 1);
  REQUIRE(feq[0][0].type == setup.type_to_idx("Rdot"));
  REQUIRE(feq[0][0].legs[0] == FunKit::LegT{phi, -101});
  REQUIRE(feq[0][0].legs[1] == FunKit::LegT{phi, -2});
}

TEST_CASE("Parse unordered leg counts", "[parse][unordered]")
{
  // Phidot-like objects: the trailing leg(s) are pinned and never reordered
  const auto tpath = write_tmp("funkit_unordered.toml", R"(
    equation = [ ]
    [setup]
    correlators = [ "Phidot" ]
    ordered = [ "R", "Rdot" ]
    [setup.unordered]
    Phidot = 1
    [[setup.cFields]]
    phi = [ ]
  )");
  auto [setup_t, feq_t] = FunKit::parse(tpath);
  REQUIRE(setup_t.unordered_legs(setup_t.type_to_idx("Phidot")) == 1);
  REQUIRE(setup_t.unordered_legs(setup_t.type_to_idx("R")) == 0);
  REQUIRE(setup_t.unordered_legs(FunKit::ObjectType::Propagator) == 0);
  REQUIRE(setup_t.unordered_legs(FunKit::ObjectType::GammaN) == 0);

  const auto jpath = write_tmp("funkit_unordered.json", R"({
    "setup": {
      "correlators": [ "Phidot" ],
      "ordered": [ "R", "Rdot" ],
      "unordered": { "Phidot": 1 },
      "cFields": [ { "phi": [] } ]
    },
    "equation": []
  })");
  auto [setup_j, feq_j] = FunKit::parse(jpath);
  REQUIRE(setup_j.unordered_legs(setup_j.type_to_idx("Phidot")) == 1);

  // Unknown type names and negative counts are rejected
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_unordered_bad.toml", R"(
    equation = [ ]
    [setup]
    [setup.unordered]
    Nope = 1
    [[setup.cFields]]
    phi = [ ]
  )")));
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_unordered_neg.toml", R"(
    equation = [ ]
    [setup]
    correlators = [ "Phidot" ]
    [setup.unordered]
    Phidot = -1
    [[setup.cFields]]
    phi = [ ]
  )")));
}

TEST_CASE("Parse source fields", "[parse][sources]")
{
  const auto tpath = write_tmp("funkit_sources.toml", R"(
    equation = [ ]
    [setup]
    [[setup.cFields]]
    phi = [ ]
    [[setup.cSources]]
    Q = [ ]
    [[setup.gSources]]
    QA = [ "v" ]
  )");
  auto [setup, feq] = FunKit::parse(tpath);

  const FunKit::FieldIdx phi = setup.field_to_idx("phi");
  const FunKit::FieldIdx q = setup.field_to_idx("Q");
  const FunKit::FieldIdx qa = setup.field_to_idx("QA");

  REQUIRE_FALSE(setup.field_props(phi).source);
  REQUIRE(setup.field_props(q).source);
  REQUIRE_FALSE(setup.field_props(q).grassmann);
  REQUIRE(setup.field_props(qa).source);
  REQUIRE(setup.field_props(qa).grassmann);

  // Sources never appear in the AnyField expansion universe
  const auto fields = setup.all_fields();
  REQUIRE(std::find(fields.begin(), fields.end(), phi) != fields.end());
  REQUIRE(std::find(fields.begin(), fields.end(), q) == fields.end());
  REQUIRE(std::find(fields.begin(), fields.end(), qa) == fields.end());

  // The JSON mirror parses identically
  const auto jpath = write_tmp("funkit_sources.json", R"({
    "setup": {
      "cFields": [ { "phi": [] } ],
      "cSources": [ { "Q": [] } ],
      "gSources": [ { "QA": ["v"] } ]
    },
    "equation": []
  })");
  auto [setup_j, feq_j] = FunKit::parse(jpath);
  REQUIRE(setup_j.field_props(setup_j.field_to_idx("Q")).source);

  // Sources are single fields, never pairs
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sources_pair.toml", R"(
    equation = [ ]
    [setup]
    [[setup.cSources]]
    Qb = [ ]
    Q = [ ]
  )")));
}

TEST_CASE("Parse symmetries TOML", "[parse][toml][symmetries]")
{
  const auto path = write_tmp("funkit_syms.toml", R"(
    equation = [ [ { type = "Propagator", legs = [ [ "phi", 1 ], [ "phi", 2 ] ] } ] ]

    [setup]
    debug = 0
    [[setup.cFields]]
    phi = [ ]

    [[symmetries]]
    cycles = [ [1, 2] ]
    factor = -1

    [[symmetries]]
    cycles = [ [1, 2, 3] ]
  )");
  auto [setup, feq] = FunKit::parse(path);

  REQUIRE(setup.symmetries.size() == 2);
  const auto &syms = setup.symmetries.all();
  REQUIRE(syms[0].cycles == std::vector<std::vector<FunKit::Idx>>{{1, 2}});
  REQUIRE(syms[0].factor == -1);
  REQUIRE(syms[1].cycles == std::vector<std::vector<FunKit::Idx>>{{1, 2, 3}});
  REQUIRE(syms[1].factor == 1); // factor defaults to +1
}

TEST_CASE("Parse symmetries JSON mirrors TOML", "[parse][json][symmetries]")
{
  const auto tpath = write_tmp("funkit_syms_mirror.toml", R"(
    equation = [ ]
    [setup]
    debug = 0
    [[setup.cFields]]
    phi = [ ]
    [[symmetries]]
    cycles = [ [1, 2] ]
    factor = -1
    [[symmetries]]
    cycles = [ [3, 4], [5, 6] ]
    factor = 1
  )");
  const auto jpath = write_tmp("funkit_syms_mirror.json", R"({
    "setup": { "debug": 0, "cFields": [ { "phi": [] } ] },
    "equation": [],
    "symmetries": [
      { "cycles": [[1, 2]], "factor": -1 },
      { "cycles": [[3, 4], [5, 6]], "factor": 1 }
    ]
  })");
  auto [setup_t, feq_t] = FunKit::parse(tpath);
  auto [setup_j, feq_j] = FunKit::parse(jpath);

  REQUIRE(setup_j.symmetries.all() == setup_t.symmetries.all());
}

TEST_CASE("Parse rejects malformed symmetries", "[parse][robustness][symmetries]")
{
  const std::string head = R"(
    equation = [ ]
    [setup]
    debug = 0
    [[setup.cFields]]
    phi = [ ]
  )";

  // factor must be +-1
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sym_factor.toml",
    head + "[[symmetries]]\ncycles = [ [1, 2] ]\nfactor = 2\n")));
  // empty cycles list
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sym_empty.toml",
    head + "[[symmetries]]\ncycles = [ ]\n")));
  // singleton cycle
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sym_single.toml",
    head + "[[symmetries]]\ncycles = [ [1] ]\n")));
  // non-positive label
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sym_zero.toml",
    head + "[[symmetries]]\ncycles = [ [0, 1] ]\n")));
  // overlapping cycles within one symmetry
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sym_overlap.toml",
    head + "[[symmetries]]\ncycles = [ [1, 2], [2, 3] ]\n")));
  // missing cycles key
  REQUIRE_THROWS(FunKit::parse(write_tmp("funkit_sym_nocycles.toml",
    head + "[[symmetries]]\nfactor = 1\n")));
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
