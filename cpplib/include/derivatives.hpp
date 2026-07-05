#pragma once

#include "core.hpp"

namespace FunKit
{
  FTerm functionalD(const Setup &setup, const FTerm &term, Idx fdop_idx);
  FEq resolve_fdop(const Setup &setup, FTerm in_term);
  FEq &resolve_derivatives(const Setup &setup, FEq &feq);
} // namespace FunKit
