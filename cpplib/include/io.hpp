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
} // namespace FunKit