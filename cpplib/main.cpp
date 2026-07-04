#include <chrono>
#include <iostream>

#include "io.hpp"
#include "parse.hpp"
#include "transformations.hpp"

int main(int argc, char **argv)
{
  // You must pass an input file as the first argument
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <input_file>" << std::endl;
    return 1;
  }

  // I am speed
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);

  // Parse file
  auto [setup, equation] = FunKit::parse(argv[1]);

  if (setup.debug_level > 0) std::cout << "Debug info:" << std::endl;

  const auto start = std::chrono::high_resolution_clock::now();

  auto result = FunKit::resolve_derivatives(setup, equation);

  const auto end = std::chrono::high_resolution_clock::now();

  if (setup.debug_level > 0) {
    std::cout << "Time taken: ";
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    if (ms < 1000)
      std::cout << ms << " ms" << std::endl;
    else
      // output with 1 decimal place
      std::cout << std::fixed << std::setprecision(1) << (ms / 1000.0) << " s" << std::endl;
  }

  if (setup.debug_level > 0) std::cout << "\nOutput:" << std::endl;

  if (setup.outputFile != "") {
    std::ofstream ofs(setup.outputFile);
    // FunKit::print(setup, result, ofs);
  } else {
    // FunKit::print(setup, result);
  }

  return 0;
};
