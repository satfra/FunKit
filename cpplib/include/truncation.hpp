#pragma once

#include "core.hpp"

namespace FunKit
{
  FTerm &truncate(const Setup &setup, FTerm &fterm);
  FEq &truncate(const Setup &setup, FEq &feq);
} // namespace FunKit