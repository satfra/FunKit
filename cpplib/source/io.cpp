#include "io.hpp"

#include <format>
#include <string>

#include "exceptions.hpp"

namespace FunKit
{
  void print(const Setup &setup, const Object &object, std::string &os)
  {
    // Print the head (idx_to_type covers built-in and custom types)
    os += setup.idx_to_type(object.type);

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

  namespace
  {
    // Minimal JSON string escaping (quote, backslash, control characters);
    // field/type names are plain identifiers, but input_file is a user path
    std::string escape_json(const std::string &s)
    {
      std::string out;
      out.reserve(s.size());
      for (const char c : s) {
        switch (c) {
        case '"':
          out += "\\\"";
          break;
        case '\\':
          out += "\\\\";
          break;
        case '\n':
          out += "\\n";
          break;
        case '\t':
          out += "\\t";
          break;
        case '\r':
          out += "\\r";
          break;
        default:
          if (static_cast<unsigned char>(c) < 0x20)
            std::format_to(std::back_inserter(out), "\\u{:04x}", static_cast<unsigned>(c));
          else
            out += c;
        }
      }
      return out;
    }

    void append_json_header(const Setup &setup, std::string &os)
    {
      os += "{\n \"funkit_output_version\": 1,\n";
      os += " \"input_file\": \"" + escape_json(setup.input_file) + "\",\n";
      std::format_to(std::back_inserter(os),
                     " \"stages\": {{\"derivatives\": true, \"truncate\": {}, \"simplify\": {}}},\n", setup.do_truncate,
                     setup.do_simplify);
      os += " \"equation\": [";
    }

    void append_json_term(const Setup &setup, const FTerm &term, std::string &os)
    {
      os += "\n  [";
      // std::format emits the shortest representation that round-trips the
      // double exactly — lossless coefficient transport
      std::format_to(std::back_inserter(os), "{{\"prefactor\": {}}}", term.value);
      for (const auto &obj : term) {
        os += ",\n   {\"type\": \"" + setup.idx_to_type(obj.type) + "\", \"legs\": [";
        for (const auto &leg : obj.legs) {
          std::format_to(std::back_inserter(os), "[\"{}\",{}]", setup.idx_to_field(leg.first), leg.second);
          if (&leg != &obj.legs.back()) os += ",";
        }
        os += "]}";
      }
      os += "]";
    }
  } // namespace

  void print_json(const Setup &setup, const FEq &feq, std::string &os)
  {
    append_json_header(setup, os);
    for (const auto &term : feq) {
      append_json_term(setup, term, os);
      if (&term != &feq.back()) os += ",";
    }
    os += "\n ]\n}\n";
  }

  void print_json(const Setup &setup, const FEq &feq, std::ostream &os)
  {
    std::string buf;
    append_json_header(setup, buf);
    for (const auto &term : feq) {
      append_json_term(setup, term, buf);
      if (&term != &feq.back()) buf += ",";

      // Flush the buffer if it gets > 8MB to avoid excessive memory usage
      if (buf.size() > 8e6) {
        os << buf;
        buf.clear();
      }
    }
    buf += "\n ]\n}\n";
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

    if (!setup.symmetries.m_symmetries.empty()) {
      os_str += "\n  Symmetries:";
      for (const auto &sym : setup.symmetries.m_symmetries) {
        os_str += "\n    ";
        for (const auto &cycle : sym.cycles) {
          os_str += "(";
          for (Idx k = 0; k < (Idx)cycle.size(); ++k) {
            os_str += std::to_string(cycle[k]);
            if (k < (Idx)cycle.size() - 1) os_str += " ";
          }
          os_str += ")";
        }
        os_str += " -> " + std::string(sym.factor < 0 ? "-1" : "+1");
      }
    }

    os << os_str;
  }
} // namespace FunKit
