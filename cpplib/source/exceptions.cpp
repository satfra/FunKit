#include "exceptions.hpp"

namespace FunKit
{
  namespace Exc
  {
    ZeroIndex::ZeroIndex(const LegT &leg) : leg(leg)
    {
      msg = "The leg (" + std::to_string(leg.first) + "," + std::to_string(leg.second) + ") has zero index!";
    }

    const char *ZeroIndex::what() const noexcept { return msg.c_str(); }

    UpperIndex::UpperIndex(const LegT &leg) : leg(leg)
    {
      msg = "Expected upper index in leg (" + std::to_string(leg.first) + "," + std::to_string(leg.second) +
            "), but got lower!";
    }

    const char *UpperIndex::what() const noexcept { return msg.c_str(); }

    LowerIndex::LowerIndex(const LegT &leg) : leg(leg)
    {
      msg = "Expected lower index in leg (" + std::to_string(leg.first) + "," + std::to_string(leg.second) +
            "), but got upper!";
    }

    const char *LowerIndex::what() const noexcept { return msg.c_str(); }
  } // namespace Exc

  void assert_upper_index(const LegT &leg)
  {
    if (leg.second == 0) throw Exc::ZeroIndex(leg);
    if (leg.second < 0) throw Exc::UpperIndex(leg);
  }

  void assert_lower_index(const LegT &leg)
  {
    if (leg.second == 0) throw Exc::ZeroIndex(leg);
    if (leg.second > 0) throw Exc::LowerIndex(leg);
  }

  void assert_index(const LegT &leg)
  {
    if (leg.second == 0) throw Exc::ZeroIndex(leg);
  }
} // namespace FunKit
