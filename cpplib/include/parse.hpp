#pragma once

#include <algorithm>
#include <fstream>
#include <string>

#include "core.hpp"
#include "io.hpp"

#include "nlohmann/json.hpp"
#include "toml11/toml.hpp"

namespace FunKit
{
  inline auto parse_json(const std::string &filename)
  {
    Setup setup;
    FEq feq;

    using json = nlohmann::json;

    std::ifstream file(filename);
    json data = json::parse(file);

    // Global info:
    setup.input_file = filename;
    setup.debug_level = data["setup"]["debug"];
    setup.outputFile = data["setup"]["outputFile"];

    // Read commuting fields
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
        throw std::runtime_error("Fields can be provided at most in pairs!");
    }

    // Read existing correlation functions
    for (const auto &object : data["setup"]["correlators"]) {
      setup.objects.push_back(object);
      setup.correlationFunctions++;
      setup.orderedObjects++;
      setup.indexedObjects++;
    }

    // Read ordered functions
    for (const auto &object : data["setup"]["ordered"]) {
      setup.objects.push_back(object);
      setup.orderedObjects++;
      setup.indexedObjects++;
    }

    // Read objects

    // Parse the equation
    for (const auto &term : data["equation"]) {
      FTerm fterm;
      for (const auto &object : term) {
        Object obj;

        obj.type = setup.type_to_idx(object["type"]);

        if (object.contains("value"))
          obj.value = object["value"];
        else
          obj.value = 1.0;

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

  inline auto parse_toml(const std::string &filename)
  {
    Setup setup;
    FEq feq;

    std::ifstream file(filename);
    const auto data = toml::parse(file);

    // Global info:
    setup.input_file = filename;
    setup.debug_level = data.at("setup").at("debug").as_integer();
    setup.outputFile = data.at("setup").at("outputFile").as_string();

    // Read commuting fields
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
        throw std::runtime_error("Fields can be provided at most in pairs!");
    }

    // Read existing correlation functions
    for (const auto &object : data.at("setup").at("correlators").as_array()) {
      setup.objects.push_back(object.as_string());
      setup.correlationFunctions++;
      setup.orderedObjects++;
      setup.indexedObjects++;
    }

    // Read ordered functions
    for (const auto &object : data.at("setup").at("ordered").as_array()) {
      setup.objects.push_back(object.as_string());
      setup.orderedObjects++;
      setup.indexedObjects++;
    }

    // Read objects

    // Parse the equation
    for (const auto &term : data.at("equation").as_array()) {
      FTerm fterm;
      for (const auto &object : term.as_array()) {
        Object obj;

        obj.type = setup.type_to_idx(object.at("type").as_string());

        if (object.contains("value"))
          obj.value = object.at("value").as_floating();
        else
          obj.value = 1.0;

        if (object.contains("legs")) {
          for (const auto &leg : object.at("legs").as_array()) {
            obj.legs.emplace_back(setup.field_to_idx(leg.at(0).as_string()), leg.at(1).as_integer());
          }
        }

        fterm.push_back(obj);
      }
      feq.push_back(fterm);
    }

    return std::make_tuple(setup, feq);
  }

  inline auto parse(const std::string &filename)
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
      throw std::runtime_error("Unsupported file format. Please use .json or .toml files.");
    }

    if (setup.debug_level > 0) {
      print(setup);
      std::cout << std::endl;

      std::cout << "Equation:\n";
      print(setup, equation);

      std::cout << "\n" << std::endl;
    }

    return std::tuple(std::move(setup), std::move(equation));
  }

} // namespace FunKit