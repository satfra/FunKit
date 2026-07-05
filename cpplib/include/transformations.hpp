#pragma once

#include "core.hpp"

namespace FunKit
{
  FTerm &reduce(FTerm &fterm);
  FEq &reduce(FEq &feq);

  std::tuple<double, Object> commute_sign(const Setup &setup, const LegT &leg1, const LegT &leg2);
  FTerm &commute_forward(const Setup &setup, FTerm &term, Idx i1);

  FEq merge_feq(std::vector<FEq> &&list);
} // namespace FunKit