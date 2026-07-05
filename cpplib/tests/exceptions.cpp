#include <catch2/catch_test_macros.hpp>

#include "funkit.hpp"

TEST_CASE("Index assertions", "[exceptions]")
{
  const FunKit::LegT upper = {0, 1};
  const FunKit::LegT lower = {0, -1};
  const FunKit::LegT zero = {0, 0};

  REQUIRE_NOTHROW(FunKit::assert_upper_index(upper));
  REQUIRE_THROWS_AS(FunKit::assert_upper_index(zero), FunKit::Exc::ZeroIndex);
  REQUIRE_THROWS_AS(FunKit::assert_upper_index(lower), FunKit::Exc::UpperIndex);

  REQUIRE_NOTHROW(FunKit::assert_lower_index(lower));
  REQUIRE_THROWS_AS(FunKit::assert_lower_index(zero), FunKit::Exc::ZeroIndex);
  REQUIRE_THROWS_AS(FunKit::assert_lower_index(upper), FunKit::Exc::LowerIndex);

  REQUIRE_NOTHROW(FunKit::assert_index(upper));
  REQUIRE_NOTHROW(FunKit::assert_index(lower));
  REQUIRE_THROWS_AS(FunKit::assert_index(zero), FunKit::Exc::ZeroIndex);
}

TEST_CASE("Exception messages", "[exceptions]")
{
  REQUIRE_FALSE(std::string(FunKit::Exc::ZeroIndex({0, 0}).what()).empty());
  REQUIRE_FALSE(std::string(FunKit::Exc::UpperIndex({0, -1}).what()).empty());
  REQUIRE_FALSE(std::string(FunKit::Exc::LowerIndex({0, 1}).what()).empty());
}
