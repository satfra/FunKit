#include <chrono>
#include <fstream>
#include <iomanip>

#include "derivatives.hpp"
#include "io.hpp"
#include "parse.hpp"

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

  if (setup.debug_level > 0) {
    std::cout << "\n=================================";
    std::cout << "\n========= Configuration: ========";
    std::cout << "\n=================================\n\n";
    print(setup);
    std::cout << std::endl;

    std::cout << "Equation:\n";
    std::string eq_str;
    print(setup, equation, eq_str);
    // Print the equation with indentation
    size_t pos = 0;
    while ((pos = eq_str.find("\n", pos)) != std::string::npos) {
      eq_str.replace(pos, 1, "\n  ");
      pos += 2;
    }
    std::cout << "  " << eq_str << std::endl;
  }

  if (setup.debug_level > 0) {
    std::cout << "\n\n=================================";
    std::cout << "\n========= Processing: ===========";
    std::cout << "\n=================================\n\n";
  }

  const auto start = std::chrono::high_resolution_clock::now();

  auto result = FunKit::resolve_derivatives(setup, equation);

  const auto deriv_end = std::chrono::high_resolution_clock::now();

  if (setup.debug_level > 0) {
    std::cout << "Time taken for derivatives: ";
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(deriv_end - start).count();
    if (ms < 1000)
      std::cout << ms << " ms" << std::endl;
    else
      // output with 1 decimal place
      std::cout << std::fixed << std::setprecision(1) << (ms / 1000.0) << " s" << std::endl;
  }

  if (setup.debug_level > 0) {
    std::cout << "\n\n=================================";
    std::cout << "\n========= Output: ===============";
    std::cout << "\n=================================\n\n";
  }

  if (setup.outputFile != "") {
    std::ofstream ofs(setup.outputFile);
    FunKit::print(setup, result, ofs);
    const auto out_end = std::chrono::high_resolution_clock::now();
    if (setup.debug_level > 0) {
      std::cout << "\nOutput written to " << setup.outputFile << std::endl;

      std::cout << "Time taken for output: ";
      const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(out_end - deriv_end).count();
      if (ms < 1000)
        std::cout << ms << " ms" << std::endl;
      else
        // output with 1 decimal place
        std::cout << std::fixed << std::setprecision(1) << (ms / 1000.0) << " s" << std::endl;
    }
  } else {
    FunKit::print(setup, result);
  }

  return 0;
};
