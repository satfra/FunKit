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
    if (data["setup"].contains("output_format")) setup.output_format = data["setup"]["output_format"];
    if (data["setup"].contains("in_deriv_trunc")) setup.in_deriv_trunc = data["setup"]["in_deriv_trunc"];
    if (data["setup"].contains("do_truncate")) setup.do_truncate = data["setup"]["do_truncate"];
    if (data["setup"].contains("do_simplify")) setup.do_simplify = data["setup"]["do_simplify"];

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

    // Read source fields: single unpaired entries appended after the regular
    // fields, excluded from the AnyField expansion
    if (data["setup"].contains("cSources"))
      for (const auto &pair : data["setup"]["cSources"]) {
        if (pair.size() != 1) loud_throw("Source fields must be single fields, not pairs!");
        for (const auto &field : pair.items()) {
          Field f;
          f.name = field.key();
          for (const auto &index : field.value())
            f.indices.push_back(index);
          setup.cFields.push_back(std::make_pair(f, Field{}));
          setup.cSourceCount++;
        }
      }

    if (data["setup"].contains("gSources"))
      for (const auto &pair : data["setup"]["gSources"]) {
        if (pair.size() != 1) loud_throw("Source fields must be single fields, not pairs!");
        for (const auto &field : pair.items()) {
          Field f;
          f.name = field.key();
          for (const auto &index : field.value())
            f.indices.push_back(index);
          setup.gFields.push_back(std::make_pair(f, Field{}));
          setup.gSourceCount++;
        }
      }

    // All fields are known now: build the per-field property table
    setup.finalize_fields();

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

    // Read the externally visible index labels (the equation's open legs)
    if (data["setup"].contains("externals"))
      for (const auto &label : data["setup"]["externals"])
        setup.external_labels.push_back(std::abs(label.get<Idx>()));

    // Read unordered trailing-leg counts (e.g. Phidot's pinned "field" slot)
    setup.unordered_leg_counts.assign(setup.objects.size(), 0);
    if (data["setup"].contains("unordered"))
      for (const auto &entry : data["setup"]["unordered"].items()) {
        const KeyT type_idx = setup.type_to_idx(entry.key());
        if (type_idx < predef_correlation_functions)
          loud_throw("'unordered' may only be set for user object types, not '" + entry.key() + "'.");
        const Idx count = entry.value().get<Idx>();
        if (count < 0) loud_throw("'unordered' count for '" + entry.key() + "' must be non-negative.");
        setup.unordered_leg_counts[type_idx - predef_correlation_functions] = count;
      }

    // Parse the truncation rules
    setup.truncation.initialize(setup);
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
    setup.truncation.finalize();

    // Parse the symmetries (top-level, sibling of "equation"). Each entry is
    // { cycles = [[label, ...], ...], factor = ±1 }; factor defaults to +1.
    if (data.contains("symmetries")) {
      if (!data["symmetries"].is_array()) loud_throw("'symmetries' must be an array in JSON file.");
      for (const auto &entry : data["symmetries"]) {
        Symmetry sym;
        if (!entry.contains("cycles")) loud_throw("Each symmetry must have a 'cycles' array in JSON file.");
        if (!entry["cycles"].is_array()) loud_throw("A symmetry's 'cycles' must be an array in JSON file.");
        for (const auto &cycle : entry["cycles"]) {
          if (!cycle.is_array()) loud_throw("Each symmetry cycle must be an array of leg labels in JSON file.");
          std::vector<Idx> labels;
          for (const auto &label : cycle)
            labels.push_back(label.get<Idx>());
          sym.cycles.push_back(std::move(labels));
        }
        if (entry.contains("factor")) sym.factor = entry["factor"].get<int>();
        setup.symmetries.add(std::move(sym));
      }
    }
    // A "derivatives" list (top-level, sibling of "equation") declares the
    // equation's external-leg symmetry: each entry is a [field, label] pair
    // naming one derivative. Stored as-is — simplify() exploits it without
    // expanding the (possibly huge) permutation group.
    if (data.contains("derivatives")) {
      if (!data["derivatives"].is_array()) loud_throw("'derivatives' must be an array in JSON file.");
      for (const auto &entry : data["derivatives"]) {
        if (!entry.is_array() || entry.size() != 2)
          loud_throw("Each derivative must be a [field, label] pair in JSON file.");
        setup.derivatives.push_back({setup.field_to_idx(entry[0].get<std::string>()), entry[1].get<Idx>()});
      }
    }
    setup.symmetries.finalize();

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
    if (data.at("setup").contains("output_format"))
      setup.output_format = data.at("setup").at("output_format").as_string();
    if (data.at("setup").contains("in_deriv_trunc"))
      setup.in_deriv_trunc = data.at("setup").at("in_deriv_trunc").as_boolean();
    if (data.at("setup").contains("do_truncate")) setup.do_truncate = data.at("setup").at("do_truncate").as_boolean();
    if (data.at("setup").contains("do_simplify")) setup.do_simplify = data.at("setup").at("do_simplify").as_boolean();

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

    // Read source fields: single unpaired entries appended after the regular
    // fields, excluded from the AnyField expansion
    if (data.at("setup").contains("cSources")) {
      if (!data.at("setup").at("cSources").is_array()) loud_throw("'cSources' must be an array of tables in TOML file.");
      for (const auto &entry : data.at("setup").at("cSources").as_array()) {
        if (entry.as_table().size() != 1) loud_throw("Source fields must be single fields, not pairs!");
        for (const auto &field : entry.as_table()) {
          Field f;
          f.name = field.first;
          for (const auto &index : field.second.as_array())
            f.indices.push_back(index.as_string());
          setup.cFields.push_back(std::make_pair(f, Field{}));
          setup.cSourceCount++;
        }
      }
    }

    if (data.at("setup").contains("gSources")) {
      if (!data.at("setup").at("gSources").is_array()) loud_throw("'gSources' must be an array of tables in TOML file.");
      for (const auto &entry : data.at("setup").at("gSources").as_array()) {
        if (entry.as_table().size() != 1) loud_throw("Source fields must be single fields, not pairs!");
        for (const auto &field : entry.as_table()) {
          Field f;
          f.name = field.first;
          for (const auto &index : field.second.as_array())
            f.indices.push_back(index.as_string());
          setup.gFields.push_back(std::make_pair(f, Field{}));
          setup.gSourceCount++;
        }
      }
    }

    // All fields are known now: build the per-field property table
    setup.finalize_fields();

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

    // Read the externally visible index labels (the equation's open legs)
    if (data.at("setup").contains("externals")) {
      if (!data.at("setup").at("externals").is_array()) loud_throw("'externals' must be an array in TOML file.");
      for (const auto &label : data.at("setup").at("externals").as_array())
        setup.external_labels.push_back(std::abs(static_cast<Idx>(label.as_integer())));
    }

    // Read unordered trailing-leg counts (e.g. Phidot's pinned "field" slot)
    setup.unordered_leg_counts.assign(setup.objects.size(), 0);
    if (data.at("setup").contains("unordered")) {
      if (!data.at("setup").at("unordered").is_table()) loud_throw("'unordered' must be a table in TOML file.");
      for (const auto &entry : data.at("setup").at("unordered").as_table()) {
        const KeyT type_idx = setup.type_to_idx(entry.first);
        if (type_idx < predef_correlation_functions)
          loud_throw("'unordered' may only be set for user object types, not '" + entry.first + "'.");
        const Idx count = static_cast<Idx>(entry.second.as_integer());
        if (count < 0) loud_throw("'unordered' count for '" + entry.first + "' must be non-negative.");
        setup.unordered_leg_counts[type_idx - predef_correlation_functions] = count;
      }
    }

    // Parse the truncation rules
    setup.truncation.initialize(setup);
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
    setup.truncation.finalize();

    // Parse the symmetries (top-level, sibling of "equation"). Each entry is a
    // table { cycles = [[label, ...], ...], factor = ±1 }; factor defaults to +1.
    if (data.contains("symmetries")) {
      if (!data.at("symmetries").is_array()) loud_throw("'symmetries' must be an array of tables in TOML file.");
      for (const auto &entry : data.at("symmetries").as_array()) {
        Symmetry sym;
        if (!entry.contains("cycles")) loud_throw("Each symmetry must have a 'cycles' array in TOML file.");
        if (!entry.at("cycles").is_array()) loud_throw("A symmetry's 'cycles' must be an array in TOML file.");
        for (const auto &cycle : entry.at("cycles").as_array()) {
          if (!cycle.is_array()) loud_throw("Each symmetry cycle must be an array of leg labels in TOML file.");
          std::vector<Idx> labels;
          for (const auto &label : cycle.as_array())
            labels.push_back(static_cast<Idx>(label.as_integer()));
          sym.cycles.push_back(std::move(labels));
        }
        if (entry.contains("factor")) sym.factor = static_cast<int>(entry.at("factor").as_integer());
        setup.symmetries.add(std::move(sym));
      }
    }
    // A "derivatives" list (top-level, sibling of "equation") declares the
    // equation's external-leg symmetry: each entry is a [field, label] pair
    // naming one derivative. Stored as-is — simplify() exploits it without
    // expanding the (possibly huge) permutation group.
    if (data.contains("derivatives")) {
      if (!data.at("derivatives").is_array()) loud_throw("'derivatives' must be an array in TOML file.");
      for (const auto &entry : data.at("derivatives").as_array()) {
        if (!entry.is_array() || entry.as_array().size() != 2)
          loud_throw("Each derivative must be a [field, label] pair in TOML file.");
        setup.derivatives.push_back({setup.field_to_idx(std::string(entry.as_array()[0].as_string())),
                                     static_cast<Idx>(entry.as_array()[1].as_integer())});
      }
    }
    setup.symmetries.finalize();

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
