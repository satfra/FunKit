#include "io.hpp"

#include <format>
#include <string>

#include "exceptions.hpp"

namespace FunKit
{
  void print(const Setup &setup, const Object &object, std::string &os)
  {
    // Print the head - either predefined:
    switch (object.type) {
    case ObjectType::FDOp:
      os += "FDOp";
      break;
    case ObjectType::FMinus:
      os += "FMinus";
      break;
    case ObjectType::Propagator:
      os += "Propagator";
      break;
    case ObjectType::GammaN:
      os += "GammaN";
      break;
    default:
      // Or custom:
      if (object.type >= predef_correlation_functions &&
          object.type < predef_correlation_functions + setup.objects.size()) {
        os += setup.objects[object.type - predef_correlation_functions];
      } else {
        loud_throw("Unknown object type: " + std::to_string(object.type));
      }
    }

    // Print the legs
    os += "[{";
    for (const auto &leg : object.legs) {
      os += setup.idx_to_field(leg.first);
      if (&leg != &object.legs.back()) os += ",";
    }
    os += "},{";
    for (const auto &leg : object.legs) {
      os += sidx_to_string(leg.second);
      if (&leg != &object.legs.back()) os += ",";
    }
    os += "}]";
  }
  void print(const Setup &setup, const Object &object, std::ostream &os)
  {
    std::string os_str;
    print(setup, object, os_str);
    os << os_str;
  }

  void print(const Setup &setup, const FTerm &term, std::string &os)
  {
    os += "FTerm[";
    std::format_to(std::back_inserter(os), "{}", term.value);
    os += ",";
    for (const auto &obj : term) {
      print(setup, obj, os);
      if (&obj != &term.back()) os += ",";
    }
    os += "]";
  }
  void print(const Setup &setup, const FTerm &term, std::ostream &os)
  {
    std::string os_str;
    print(setup, term, os_str);
    os << os_str;
  }

  void print(const Setup &setup, const FEq &feq, std::string &os)
  {
    os += "FEq[";
    for (const auto &term : feq) {
      os += "\n  ";
      print(setup, term, os);
      if (&term != &feq.back()) os += ",";
    }
    os += "\n ]";
  }
  void print(const Setup &setup, const FEq &feq, std::ostream &os)
  {
    std::string buf;
    buf += "FEq[";
    for (const auto &term : feq) {
      buf += "\n  ";
      print(setup, term, buf);
      if (&term != &feq.back()) buf += ",";

      // Flush the buffer if it gets > 8MB to avoid excessive memory usage
      if (buf.size() > 8e6) {
        os << buf;
        buf.clear();
      }
    }
    buf += "\n ]";
    os << buf;
  }

  void print(const Setup &setup, std::ostream &os)
  {
    std::string os_str;
    os_str += "Setup:";

    os_str += "\n  input_file:     " + setup.input_file;
    os_str += "\n  debug_level:    " + std::to_string(setup.debug_level);

    os_str += "\n  objects:        [";
    for (Idx i = 0; i < setup.objects.size(); ++i) {
      os_str += setup.objects[i];
      os_str += " (cf:" + std::to_string((int)setup.is_correlationFunction(predef_correlation_functions + i));
      os_str += " of:" + std::to_string((int)setup.is_orderedObject(predef_correlation_functions + i)) +
                " io:" + std::to_string((int)setup.is_indexedObject(predef_correlation_functions + i)) + ")";
      if (i < setup.objects.size() - 1)
        os_str += ",\n"
                  "                   ";
      else
        os_str += "]";
    }

    os_str += "\n  cFields:        [";
    for (Idx i = 0; i < setup.cFields.size(); ++i) {
      // Only one field:
      if (setup.cFields[i].second.name == "") {
        os_str += setup.cFields[i].first.name + "[p";
        if (setup.cFields[i].first.indices.size() > 0) {
          os_str += ",{";
          for (Idx j = 0; j < setup.cFields[i].first.indices.size(); ++j) {
            os_str += setup.cFields[i].first.indices[j];
            if (j < setup.cFields[i].first.indices.size() - 1) os_str += ",";
          }
          os_str += "}";
        }
        os_str += "]";
      }
      // Pair of fields:
      else {
        os_str += "[" + setup.cFields[i].first.name;
        if (setup.cFields[i].first.indices.size() > 0) {
          os_str += ",{";
          for (Idx j = 0; j < setup.cFields[i].first.indices.size(); ++j) {
            os_str += setup.cFields[i].first.indices[j];
            if (j < setup.cFields[i].first.indices.size() - 1) os_str += ",";
          }
          os_str += "}";
        }
        os_str += "," + setup.cFields[i].second.name;
        if (setup.cFields[i].second.indices.size() > 0) {
          os_str += ",{";
          for (Idx j = 0; j < setup.cFields[i].second.indices.size(); ++j) {
            os_str += setup.cFields[i].second.indices[j];
            if (j < setup.cFields[i].second.indices.size() - 1) os_str += ",";
          }
          os_str += "}";
        }
        os_str += "]";
      }
      if (i < setup.cFields.size() - 1)
        os_str += ",\n"
                  "                   ";
      else
        os_str += "]";
    }
    if (setup.cFields.size() == 0) os_str += "]";

    os_str += "\n  gFields:        [";
    for (Idx i = 0; i < setup.gFields.size(); ++i) {
      // Only one field:
      if (setup.gFields[i].second.name == "") {
        os_str += setup.gFields[i].first.name + "[p";
        if (setup.gFields[i].first.indices.size() > 0) {
          os_str += ",{";
          for (Idx j = 0; j < setup.gFields[i].first.indices.size(); ++j) {
            os_str += setup.gFields[i].first.indices[j];
            if (j < setup.gFields[i].first.indices.size() - 1) os_str += ",";
          }
          os_str += "}";
        }
        os_str += "]";
      }
      // Pair of fields:
      else {
        os_str += "[" + setup.gFields[i].first.name;
        if (setup.gFields[i].first.indices.size() > 0) {
          os_str += ",{";
          for (Idx j = 0; j < setup.gFields[i].first.indices.size(); ++j) {
            os_str += setup.gFields[i].first.indices[j];
            if (j < setup.gFields[i].first.indices.size() - 1) os_str += ",";
          }
          os_str += "}";
        }
        os_str += "," + setup.gFields[i].second.name;
        if (setup.gFields[i].second.indices.size() > 0) {
          os_str += ",{";
          for (Idx j = 0; j < setup.gFields[i].second.indices.size(); ++j) {
            os_str += setup.gFields[i].second.indices[j];
            if (j < setup.gFields[i].second.indices.size() - 1) os_str += ",";
          }
          os_str += "}";
        }
        os_str += "]";
      }
      if (i < setup.gFields.size() - 1)
        os_str += ",\n"
                  "                   ";
      else
        os_str += "]";
    }
    if (setup.gFields.size() == 0) os_str += "]";

    if (setup.truncation.m_truncation_table.size() > 0) {
      os_str += "\n  Truncation:";
      os_str += "\n    Field:        ";
      if (setup.truncation.m_truncation_table[0].size() == 0) {
        os_str += "None";
      }
      for (Idx i = 0; i < setup.truncation.m_truncation_table[0].size(); ++i) {
        os_str += setup.idx_to_field(setup.truncation.m_truncation_table[0][i][0]);
        if (i < setup.truncation.m_truncation_table[0].size() - 1) os_str += ",";
      }
      for (Idx i = 0; i < setup.truncation.m_truncation_table.size() - 1; ++i) {
        // Make sure this has 15 spaces for alignment
        os_str += "\n    " + setup.idx_to_type(i) + ": " +
                  std::string(std::max(18 - (Idx)(6 + setup.idx_to_type(i).size()), Idx{}), ' ');
        std::string type_list;
        for (Idx j = 0; j < setup.truncation.m_truncation_table[1 + i].size(); ++j) {
          type_list += "[";
          for (Idx k = 0; k < setup.truncation.m_truncation_table[1 + i][j].size(); ++k) {
            type_list += setup.idx_to_field(setup.truncation.m_truncation_table[1 + i][j][k]);
            if (k < setup.truncation.m_truncation_table[1 + i][j].size() - 1) type_list += ",";
          }
          type_list += "]";
          if (j < setup.truncation.m_truncation_table[1 + i].size() - 1) type_list += ",";
          // Add a newline when type_list gets too long
          if (type_list.size() > 80) {
            os_str += type_list + "\n               ";
            type_list = "";
          }
        }
        os_str += type_list;
      }
    }

    os << os_str;
  }
} // namespace FunKit
