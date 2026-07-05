#pragma once

#include "core.hpp"

namespace FunKit
{
  double normalize(const Setup &setup, Object &obj);
  void normalize(const Setup &setup, FTerm &fterm);
  void normalize(const Setup &setup, FEq &feq);

  void reduce(FTerm &fterm);
  void reduce(FEq &feq);

  std::tuple<double, Object> commute_sign(const Setup &setup, const LegT &leg1, const LegT &leg2);
  FTerm &commute_forward(const Setup &setup, FTerm &term, Idx i1);

  FEq merge_feq(std::vector<FEq> &&list);
} // namespace FunKit