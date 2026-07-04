#pragma once

#include "core.hpp"

#include <iostream>
#include <stdexcept>

namespace FunKit
{
  void print(const Setup &setup, const Object &object, std::ostream &os = std::cout)
  {
    if (object.type != ObjectType::Numeric && !is_close(object.value, 1.)) {
      os << object.value << "*";
    }
    // Print the head - either predefined:
    switch (object.type) {
    case ObjectType::Numeric:
      os << object.value;
      return;
    case ObjectType::FDOp:
      os << "FDOp";
      break;
    case ObjectType::FMinus:
      os << "FMinus";
      break;
    case ObjectType::Propagator:
      os << "Propagator";
      break;
    case ObjectType::GammaN:
      os << "GammaN";
      break;
    default:
      // Or custom:
      if (object.type >= predef_obj && object.type < predef_obj + setup.objects.size()) {
        os << setup.objects[object.type - predef_obj];
      } else {
        throw std::runtime_error("Unknown object type: " + std::to_string(object.type));
      }
    }

    // Print the legs
    os << "[";
    for (const auto &leg : object.legs) {
      os << "{" << setup.idx_to_field(leg.first) << "," << sidx_to_string(leg.second) << "}";
      if (&leg != &object.legs.back()) os << ",";
    }
    os << "]";
  }

  void print(const Setup &setup, const FTerm &term, std::ostream &os = std::cout)
  {
    os << "FTerm[";
    for (const auto &obj : term) {
      print(setup, obj);
      if (&obj != &term.back()) os << ",";
    }
    os << "]";
  }

  void print(const Setup &setup, const FEq &feq, std::ostream &os = std::cout)
  {
    os << "FEq[";
    for (const auto &term : feq) {
      os << "\n  ";
      print(setup, term);
      if (&term != &feq.back()) os << ",";
    }
    os << "\n ]";
  }

  void print(const Setup &setup, std::ostream &os = std::cout)
  {
    os << "Setup:";

    os << "\n  input_file:  " << setup.input_file;
    os << "\n  debug_level: " << setup.debug_level;

    os << "\n  objects:     [";
    for (Idx i = 0; i < setup.objects.size(); ++i) {
      os << setup.objects[i];
      os << " (cf:" << (int)setup.is_correlationFunction(predef_obj + i);
      os << " of:" << (int)setup.is_orderedObject(predef_obj + i) << ")";
      if (i < setup.objects.size() - 1)
        os << ",\n"
           << "                ";
      else
        os << "]";
    }

    os << "\n  cFields:     [";
    for (Idx i = 0; i < setup.cFields.size(); ++i) {
      // Only one field:
      if (setup.cFields[i].second.name == "") {
        os << setup.cFields[i].first.name << "[p";
        if (setup.cFields[i].first.indices.size() > 0) {
          os << ",{";
          for (Idx j = 0; j < setup.cFields[i].first.indices.size(); ++j) {
            os << setup.cFields[i].first.indices[j];
            if (j < setup.cFields[i].first.indices.size() - 1) os << ",";
          }
          os << "}";
        }
        os << "]";
      }
      // Pair of fields:
      else {
        os << "[" << setup.cFields[i].first.name;
        if (setup.cFields[i].first.indices.size() > 0) {
          os << ",{";
          for (Idx j = 0; j < setup.cFields[i].first.indices.size(); ++j) {
            os << setup.cFields[i].first.indices[j];
            if (j < setup.cFields[i].first.indices.size() - 1) os << ",";
          }
          os << "}";
        }
        os << "," << setup.cFields[i].second.name;
        if (setup.cFields[i].second.indices.size() > 0) {
          os << ",{";
          for (Idx j = 0; j < setup.cFields[i].second.indices.size(); ++j) {
            os << setup.cFields[i].second.indices[j];
            if (j < setup.cFields[i].second.indices.size() - 1) os << ",";
          }
          os << "}";
        }
        os << "]";
      }
      if (i < setup.cFields.size() - 1)
        os << ",\n"
           << "                ";
      else
        os << "]";
    }
    if (setup.cFields.size() == 0) os << "]";

    os << "\n  gFields:     [";
    for (Idx i = 0; i < setup.gFields.size(); ++i) {
      // Only one field:
      if (setup.gFields[i].second.name == "") {
        os << setup.gFields[i].first.name << "[p";
        if (setup.gFields[i].first.indices.size() > 0) {
          os << ",{";
          for (Idx j = 0; j < setup.gFields[i].first.indices.size(); ++j) {
            os << setup.gFields[i].first.indices[j];
            if (j < setup.gFields[i].first.indices.size() - 1) os << ",";
          }
          os << "}";
        }
        os << "]";
      }
      // Pair of fields:
      else {
        os << "[" << setup.gFields[i].first.name;
        if (setup.gFields[i].first.indices.size() > 0) {
          os << ",{";
          for (Idx j = 0; j < setup.gFields[i].first.indices.size(); ++j) {
            os << setup.gFields[i].first.indices[j];
            if (j < setup.gFields[i].first.indices.size() - 1) os << ",";
          }
          os << "}";
        }
        os << "," << setup.gFields[i].second.name;
        if (setup.gFields[i].second.indices.size() > 0) {
          os << ",{";
          for (Idx j = 0; j < setup.gFields[i].second.indices.size(); ++j) {
            os << setup.gFields[i].second.indices[j];
            if (j < setup.gFields[i].second.indices.size() - 1) os << ",";
          }
          os << "}";
        }
        os << "]";
      }
      if (i < setup.gFields.size() - 1)
        os << ",\n"
           << "                ";
      else
        os << "]";
    }
    if (setup.gFields.size() == 0) os << "]";
  }
} // namespace FunKit