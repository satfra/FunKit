#include "core.hpp"
#include <exception>

namespace FunKit
{
  namespace Exc
  {
    class ZeroIndex : public std::exception
    {
    private:
      LegT leg;
      std::string msg;

    public:
      ZeroIndex(const LegT &leg) : leg(leg)
      {
        msg = "The leg (" + std::to_string(leg.first) + "," + std::to_string(leg.second) + ") has zero index!";
      }

      const char *what() const noexcept override { return msg.c_str(); }
    };

    class UpperIndex : public std::exception
    {
    private:
      LegT leg;
      std::string msg;

    public:
      UpperIndex(const LegT &leg) : leg(leg)
      {
        msg = "Expected upper index in leg (" + std::to_string(leg.first) + "," + std::to_string(leg.second) +
              "), but got lower!";
      }

      const char *what() const noexcept override { return msg.c_str(); }
    };

    class LowerIndex : public std::exception
    {
    private:
      LegT leg;
      std::string msg;

    public:
      LowerIndex(const LegT &leg) : leg(leg)
      {
        msg = "Expected lower index in leg (" + std::to_string(leg.first) + "," + std::to_string(leg.second) +
              "), but got upper!";
      }

      const char *what() const noexcept override { return msg.c_str(); }
    };

    class ExpectedNoFactor : public std::exception
    {
    private:
      Object object;
      std::string msg;

    public:
      ExpectedNoFactor(const Object &object) : object(object)
      {
        msg = "No factor expected for object of type " + std::to_string(object.type) + ", got " +
              std::to_string(object.value) + ".";
      }

      const char *what() const noexcept override { return msg.c_str(); }
    };
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

  void assert_no_factor(const Object &object)
  {
    if (!is_close(object.value, 1.)) throw Exc::ExpectedNoFactor(object);
  }
} // namespace FunKit