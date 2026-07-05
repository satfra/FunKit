#pragma once

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
      ZeroIndex(const LegT &leg);
      const char *what() const noexcept override;
    };

    class UpperIndex : public std::exception
    {
    private:
      LegT leg;
      std::string msg;

    public:
      UpperIndex(const LegT &leg);
      const char *what() const noexcept override;
    };

    class LowerIndex : public std::exception
    {
    private:
      LegT leg;
      std::string msg;

    public:
      LowerIndex(const LegT &leg);

      const char *what() const noexcept override;
    };
  } // namespace Exc

  void assert_upper_index(const LegT &leg);
  void assert_lower_index(const LegT &leg);
  void assert_index(const LegT &leg);
} // namespace FunKit