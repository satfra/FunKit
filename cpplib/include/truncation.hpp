#pragma once

#include "core.hpp"

namespace FunKit
{
  void prune(const Setup &setup, FTerm &term);
  void prune(const Setup &setup, FEq &feq);

  FEq truncate(const Setup &setup, FTerm fterm);
  void truncate(const Setup &setup, FEq &feq);
} // namespace FunKit
