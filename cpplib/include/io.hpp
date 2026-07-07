#pragma once

#include "core.hpp"

#include <iostream>

namespace FunKit
{
  void print(const Setup &setup, const Object &object, std::string &os);
  void print(const Setup &setup, const Object &object, std::ostream &os = std::cout);

  void print(const Setup &setup, const FTerm &term, std::string &os);
  void print(const Setup &setup, const FTerm &term, std::ostream &os = std::cout);

  void print(const Setup &setup, const FEq &feq, std::string &os);
  void print(const Setup &setup, const FEq &feq, std::ostream &os = std::cout);

  void print(const Setup &setup, std::ostream &os = std::cout);

  // Structured JSON output: input-schema-compatible "equation" (prefactor /
  // type / legs tables) plus metadata (funkit_output_version, input_file,
  // stages). The ostream overload streams with a bounded buffer.
  void print_json(const Setup &setup, const FEq &feq, std::string &os);
  void print_json(const Setup &setup, const FEq &feq, std::ostream &os = std::cout);
} // namespace FunKit