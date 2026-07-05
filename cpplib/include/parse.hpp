#pragma once

#include "core.hpp"

namespace FunKit
{
  std::tuple<Setup, FEq> parse_json(const std::string &filename);
  std::tuple<Setup, FEq> parse_toml(const std::string &filename);

  std::tuple<Setup, FEq> parse(const std::string &filename);
} // namespace FunKit
