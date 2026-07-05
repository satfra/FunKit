#include "parse.hpp"

#include <algorithm>
#include <fstream>
#include <string>

#include "core.hpp"
#include "exceptions.hpp"
#include "io.hpp"

#include "nlohmann/json.hpp"
#include "toml11/toml.hpp"

namespace FunKit
{
  std::tuple<Setup, FEq> parse_json(const std::string &filename)
  {
    Setup setup;
    FEq feq;

    using json = nlohmann::ordered_json;

    std::ifstream file(filename);
    json data = json::parse(file);

    // Sanity: We need a "setup" and an "equation" section
    if (!data.contains("setup")) loud_throw("Missing 'setup' section in JSON file.");
    if (!data.contains("equation")) loud_throw("Missing 'equation' section in JSON file.");

    // Global info:
    setup.input_file = filename;
    if (data["setup"].contains("debug")) setup.debug_level = data["setup"]["debug"];
    if (data["setup"].contains("outputFile")) setup.outputFile = data["setup"]["outputFile"];
    if (data["setup"].contains("in_deriv_trunc")) setup.in_deriv_trunc = data["setup"]["in_deriv_trunc"];

    // Read commuting fields
    if (data["setup"].contains("cFields"))
      for (const auto &pair : data["setup"]["cFields"]) {
        std::vector<Field> entries;
        for (const auto &field : pair.items()) {
          auto &f = entries.emplace_back();
          f.name = field.key();
          for (const auto &index : field.value()) {
            f.indices.push_back(index);
          }
        }
        if (entries.size() == 1)
          setup.cFields.push_back(std::make_pair(entries[0], Field{}));
        else if (entries.size() == 2)
          setup.cFields.push_back(std::make_pair(entries[0], entries[1]));
        else
          loud_throw("Fields can be provided at most in pairs!");
      }

    // Read Grassmann fields
    if (data["setup"].contains("gFields"))
      for (const auto &pair : data["setup"]["gFields"]) {
        std::vector<Field> entries;
        for (const auto &field : pair.items()) {
          auto &f = entries.emplace_back();
          f.name = field.key();
          for (const auto &index : field.value()) {
            f.indices.push_back(index);
          }
        }
        if (entries.size() == 1)
          setup.gFields.push_back(std::make_pair(entries[0], Field{}));
        else if (entries.size() == 2)
          setup.gFields.push_back(std::make_pair(entries[0], entries[1]));
        else
          loud_throw("Fields can be provided at most in pairs!");
      }

    // Read existing correlation functions
    if (data["setup"].contains("correlators"))
      for (const auto &object : data["setup"]["correlators"]) {
        setup.objects.push_back(object);
        setup.correlationFunctions++;
        setup.orderedObjects++;
        setup.indexedObjects++;
      }

    // Read ordered functions
    if (data["setup"].contains("ordered"))
      for (const auto &object : data["setup"]["ordered"]) {
        setup.objects.push_back(object);
        setup.orderedObjects++;
        setup.indexedObjects++;
      }

    // Parse the truncation rules
    setup.truncation.update(setup);
    if (data["setup"].contains("truncation")) {
      for (const auto &rule : data["setup"]["truncation"].items()) {
        KeyT type_idx = setup.type_to_idx(rule.key());
        if (!rule.value().is_array())
          loud_throw("Truncation rule for '" + rule.key() + "' must be an array in JSON file.");
        for (const auto &field_indices : rule.value()) {
          if (!field_indices.is_array())
            loud_throw("Each truncation rule for '" + rule.key() + "' must be an array of field names in JSON file.");
          std::vector<FieldIdx> indices;
          for (const auto &field_name : field_indices) {
            indices.push_back(setup.field_to_idx(field_name));
          }
          setup.truncation.add_rule(type_idx, indices);
        }
      }
    }

    // Parse the equation
    for (const auto &term : data["equation"]) {
      FTerm fterm;
      for (const auto &object : term) {

        if (object.contains("prefactor")) {
          fterm.value *= object["prefactor"].get<double>();
          continue;
        }

        Object obj;
        obj.type = setup.type_to_idx(object["type"]);

        if (object.contains("legs")) {
          for (const auto &leg : object["legs"]) {
            obj.legs.emplace_back(setup.field_to_idx(leg[0]), leg[1]);
          }
        }

        fterm.push_back(obj);
      }
      feq.push_back(fterm);
    }

    return std::make_tuple(setup, feq);
  }

  std::tuple<Setup, FEq> parse_toml(const std::string &filename)
  {
    Setup setup;
    FEq feq;

    std::ifstream file(filename);
    const auto data = toml::parse<toml::ordered_type_config>(file);

    // Sanity: We need a "setup" and an "equation" section
    if (!data.contains("setup")) loud_throw("Missing 'setup' section in TOML file.");
    if (!data.contains("equation")) loud_throw("Missing 'equation' section in TOML file.");

    // Global info:
    setup.input_file = filename;
    if (data.at("setup").contains("debug")) setup.debug_level = data.at("setup").at("debug").as_integer();
    if (data.at("setup").contains("outputFile")) setup.outputFile = data.at("setup").at("outputFile").as_string();
    if (data.at("setup").contains("in_deriv_trunc"))
      setup.in_deriv_trunc = data.at("setup").at("in_deriv_trunc").as_boolean();

    // Read commuting fields
    if (data.at("setup").contains("cFields")) {
      // must be an array of tables
      if (!data.at("setup").at("cFields").is_array()) loud_throw("'cFields' must be an array of tables in TOML file.");

      for (const auto &field : data.at("setup").at("cFields").as_array()) {
        std::vector<Field> entries;
        for (const auto &field : field.as_table()) {
          auto &f = entries.emplace_back();
          f.name = field.first;
          for (const auto &index : field.second.as_array()) {
            f.indices.push_back(index.as_string());
          }
        }
        if (entries.size() == 1)
          setup.cFields.push_back(std::make_pair(entries[0], Field{}));
        else if (entries.size() == 2)
          setup.cFields.push_back(std::make_pair(entries[0], entries[1]));
        else
          loud_throw("Fields can be provided at most in pairs!");
      }
    }

    // Read Grassmann fields
    if (data.at("setup").contains("gFields")) {
      // must be an array of tables
      if (!data.at("setup").at("gFields").is_array()) loud_throw("'gFields' must be an array of tables in TOML file.");

      for (const auto &field : data.at("setup").at("gFields").as_array()) {
        std::vector<Field> entries;
        for (const auto &field : field.as_table()) {
          auto &f = entries.emplace_back();
          f.name = field.first;
          for (const auto &index : field.second.as_array()) {
            f.indices.push_back(index.as_string());
          }
        }
        if (entries.size() == 1)
          setup.gFields.push_back(std::make_pair(entries[0], Field{}));
        else if (entries.size() == 2)
          setup.gFields.push_back(std::make_pair(entries[0], entries[1]));
        else
          loud_throw("Fields can be provided at most in pairs!");
      }
    }

    // Read existing correlation functions
    if (data.at("setup").contains("correlators")) {
      // must be an array of strings
      if (!data.at("setup").at("correlators").is_array()) loud_throw("'correlators' must be an array in TOML file.");

      for (const auto &object : data.at("setup").at("correlators").as_array()) {
        setup.objects.push_back(object.as_string());
        setup.correlationFunctions++;
        setup.orderedObjects++;
        setup.indexedObjects++;
      }
    }

    // Read ordered functions
    if (data.at("setup").contains("ordered")) {
      // must be an array of strings
      if (!data.at("setup").at("ordered").is_array()) loud_throw("'ordered' must be an array in TOML file.");

      for (const auto &object : data.at("setup").at("ordered").as_array()) {
        setup.objects.push_back(object.as_string());
        setup.orderedObjects++;
        setup.indexedObjects++;
      }
    }

    // Parse the truncation rules
    setup.truncation.update(setup);
    if (data.at("setup").contains("truncation")) {
      // must be a table
      if (!data.at("setup").at("truncation").is_table()) loud_throw("'truncation' must be a table in TOML file.");

      for (const auto &rule : data.at("setup").at("truncation").as_table()) {
        KeyT type_idx = setup.type_to_idx(rule.first);
        if (!rule.second.is_array())
          loud_throw("Truncation rule for '" + rule.first + "' must be an array in TOML file.");
        for (const auto &field_indices : rule.second.as_array()) {
          if (!field_indices.is_array())
            loud_throw("Each truncation rule for '" + rule.first + "' must be an array of field names in TOML file.");
          std::vector<FieldIdx> indices;
          for (const auto &field_name : field_indices.as_array()) {
            indices.push_back(setup.field_to_idx(field_name.as_string()));
          }
          setup.truncation.add_rule(type_idx, indices);
        }
      }
    }

    // Parse the equation
    for (const auto &term : data.at("equation").as_array()) {
      // must be an array of tables
      if (!term.is_array()) loud_throw("Each term in 'equation' must be an array of tables in TOML file.");

      FTerm fterm;
      for (const auto &object : term.as_array()) {
        if (object.contains("prefactor")) {
          fterm.value *= object.at("prefactor").as_floating();
          continue;
        }

        // Sanity check: Each object must have a type and legs
        if (!object.contains("type")) loud_throw("Missing 'type' in object " + toml::format(object) + " in TOML file.");
        if (!object.contains("legs")) loud_throw("Missing 'legs' in object " + toml::format(object) + " in TOML file.");
        // legs must be an array of arrays with two elements each
        if (!object.at("legs").is_array())
          loud_throw("'legs' must be an array in object " + toml::format(object) + " in TOML file.");
        for (const auto &leg : object.at("legs").as_array()) {
          if (!leg.is_array() || leg.as_array().size() != 2)
            loud_throw("'legs' must be an array of arrays with two elements each in object " + toml::format(object) +
                       " in TOML file.");
        }

        Object obj;
        obj.type = setup.type_to_idx(object.at("type").as_string());

        for (const auto &leg : object.at("legs").as_array())
          obj.legs.emplace_back(setup.field_to_idx(leg.at(0).as_string()), leg.at(1).as_integer());

        fterm.push_back(obj);
      }
      feq.push_back(fterm);
    }

    return std::make_tuple(setup, feq);
  }

  std::tuple<Setup, FEq> parse(const std::string &filename)
  {
    FunKit::Setup setup;
    FunKit::FEq equation;

    if (filename.ends_with(".json")) {
      // Parse JSON file
      std::tie(setup, equation) = FunKit::parse_json(filename);
    } else if (filename.ends_with(".toml")) {
      // Parse TOML file
      std::tie(setup, equation) = FunKit::parse_toml(filename);
    } else {
      loud_throw("Unsupported file format. Please use .json or .toml files.");
    }

    return std::tuple(std::move(setup), std::move(equation));
  }
} // namespace FunKit
