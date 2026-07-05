#pragma once

#include "core.hpp"
#include <exception>

// custom throw that also prints the file and line number of the throw
#define loud_throw(msg)                                                                                                \
  throw std::runtime_error(std::string("In ") + __FILE__ + ":" + std::to_string(__LINE__) + ":\n    " + msg);

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