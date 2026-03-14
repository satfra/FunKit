tests = {};

(**********************************************************************************
    Setup
**********************************************************************************)

Needs["CCompilerDriver`"];

CppCompiler =
  If[Run["command -v g++ &> /dev/null"] == 0,
    "g++"
    ,
    If[Run["command -v clang++ &> /dev/null"] == 0,
      "clang++"
      ,
      ""
    ]
  ];

If[CppCompiler == "",
  Print["C++ compiler not found, skipping tests."];
  Return[];
];

powrCode = "
template<int n, typename NumberType>
  requires requires(NumberType x) {
    x*x;
    NumberType(1.)/x;
  }
constexpr
NumberType powr(const NumberType x)
{
  if constexpr (n == 0)
    return NumberType(1.);
  else if constexpr (n < 0)
    return NumberType(1.) / powr<-n>(x);
  else if constexpr (n > 1)
    return x * powr<n-1>(x);
  else
    return x;
}";

(**********************************************************************************
    Basic C++ Function Test  (MakeCppFunction)  
**********************************************************************************)

funBody1 = MakeCppFunction[a, "Name" -> "fun", "Body" -> "const auto a = in;", "Parameters" -> {"in"}];

exec1 = CreateExecutable["
#include <iostream>
" <> funBody1 <> "
int main(){
  std::cout << fun(42) << std::endl;
}
", "FunKitCppTest1", "CompilerName" -> CppCompiler];

output1 = Import["!" <> QuoteFile[exec1], "Text"];

AppendTo[tests, TestCreate[exec1 =!= $Failed, True, TestID -> "Verify compilation of basic C++ function"]];

AppendTo[tests, TestCreate[output1, "42", TestID -> "Verify return value of basic C++ function"]];

(**********************************************************************************
    Testing typical arithmetic operations in C++ functions    
**********************************************************************************)

expr = (Cos[a] + Sin[a] ^ 2 - Log[a] / Sqrt[a]) / (Exp[a] + Tan[a]);

funBody2 = MakeCppFunction[expr, "Name" -> "fun", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];

exec2 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBody2 <> "

int main () {
  std::cout << std::setprecision (10) << fun (1.5) << std::endl;
}
", "FunKitCppTest2", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output2 = Import["!" <> QuoteFile[exec2], "Text"];

expected = ToString[NumberForm[expr /. a -> 1.5, 10]];

AppendTo[tests, TestCreate[exec2 =!= $Failed, True, TestID -> "Verify compilation of C++ function with arithmetic operations"]];

AppendTo[tests, TestCreate[output2, expected, TestID -> "Verify return value of C++ function with arithmetic operations"]];

(**********************************************************************************
    Optimization level 0 (legacy) produces valid C++ code
**********************************************************************************)

Block[{FunKit`Private`$codeOptimizationLevel = 0},
    funBody3 = MakeCppFunction[expr, "Name" -> "fun0", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec3 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBody3 <> "

int main () {
  std::cout << std::setprecision (10) << fun0 (1.5) << std::endl;
}
", "FunKitCppTest3", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output3 = Import["!" <> QuoteFile[exec3], "Text"];

AppendTo[tests, TestCreate[exec3 =!= $Failed, True, TestID -> "Verify compilation at optimization level 0"]];

AppendTo[tests, TestCreate[output3, expected, TestID -> "Verify numerical agreement at optimization level 0"]];

(**********************************************************************************
    Optimization level 1 produces valid C++ code
**********************************************************************************)

Block[{FunKit`Private`$codeOptimizationLevel = 1},
    funBody4 = MakeCppFunction[expr, "Name" -> "fun1", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec4 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBody4 <> "

int main () {
  std::cout << std::setprecision (10) << fun1 (1.5) << std::endl;
}
", "FunKitCppTest4", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output4 = Import["!" <> QuoteFile[exec4], "Text"];

AppendTo[tests, TestCreate[exec4 =!= $Failed, True, TestID -> "Verify compilation at optimization level 1"]];

AppendTo[tests, TestCreate[output4, expected, TestID -> "Verify numerical agreement at optimization level 1"]];

(**********************************************************************************
    Optimization level 2 (default) produces valid C++ code
**********************************************************************************)

Block[{FunKit`Private`$codeOptimizationLevel = 2},
    funBody5 = MakeCppFunction[expr, "Name" -> "fun2", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec5 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBody5 <> "

int main () {
  std::cout << std::setprecision (10) << fun2 (1.5) << std::endl;
}
", "FunKitCppTest5", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output5 = Import["!" <> QuoteFile[exec5], "Text"];

AppendTo[tests, TestCreate[exec5 =!= $Failed, True, TestID -> "Verify compilation at optimization level 2"]];

AppendTo[tests, TestCreate[output5, expected, TestID -> "Verify numerical agreement at optimization level 2"]];

(**********************************************************************************
    Large expression with accumulator pattern compiles correctly
**********************************************************************************)

largeExpr = Sum[Sin[a + i] * Cos[a - i] / (1 + i * a), {i, 1, 80}];

Block[{FunKit`Private`$codeOptimizationLevel = 2, FunKit`Private`$codeMaxChunkSize = 10, FunKit`Private`$availableRegisters = 8},
    funBody6 = MakeCppFunction[largeExpr, "Name" -> "funLarge", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec6 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>
using NumberType = double;

" <> powrCode <> "
" <> funBody6 <> "

int main () {
  std::cout << std::setprecision (10) << funLarge (1.5) << std::endl;
}
", "FunKitCppTest6", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output6 = Import["!" <> QuoteFile[exec6], "Text"];

expectedLarge = ToString[NumberForm[largeExpr /. a -> 1.5, 10]];

AppendTo[tests, TestCreate[exec6 =!= $Failed, True, TestID -> "Verify compilation of large expression with accumulator"]];

AppendTo[tests, TestCreate[output6, expectedLarge, TestID -> "Verify numerical agreement of large expression with accumulator"]];

(**********************************************************************************
    Optimization level 3 produces valid C++ code
**********************************************************************************)

fmaCode = "
#include <cmath>
auto fma(auto a, auto b, auto c) { return std::fma(a, b, c); }
";

Block[{FunKit`Private`$codeOptimizationLevel = 3, FunKit`Private`$codeGPURegisterBudget = 32},
    funBody7 = MakeCppFunction[expr, "Name" -> "fun3", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec7 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> fmaCode <> "
" <> powrCode <> "
" <> funBody7 <> "

int main () {
  std::cout << std::setprecision (10) << fun3 (1.5) << std::endl;
}
", "FunKitCppTest7", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output7 = Import["!" <> QuoteFile[exec7], "Text"];

AppendTo[tests, TestCreate[exec7 =!= $Failed, True, TestID -> "Verify compilation at optimization level 3"]];

AppendTo[tests, TestCreate[output7, expected, TestID -> "Verify numerical agreement at optimization level 3"]];

(**********************************************************************************
    FMA detection: verify fma() appears in output
**********************************************************************************)

Block[{FunKit`Private`$codeOptimizationLevel = 3, FunKit`Private`$codeFMARestructure = True, FunKit`Private`$codeGPURegisterBudget = 32},
    fmaTestCode = CppCode[a * b + c * d + e];
];

AppendTo[tests, TestCreate[StringContainsQ[fmaTestCode, "fma("], True, TestID -> "Verify FMA detection in level 3 output"]];

(**********************************************************************************
    Fast-math intrinsics emission
**********************************************************************************)

Block[{FunKit`Private`$codeOptimizationLevel = 3, FunKit`Private`$codeFastMath = True, FunKit`Private`$codePrecision = "single", FunKit`Private`$codeGPURegisterBudget = 32},
    fastMathCode = CppCode[Exp[x] + Log[x]];
];

AppendTo[tests, TestCreate[StringContainsQ[fastMathCode, "__expf("], True, TestID -> "Verify __expf in fast-math output"]];
AppendTo[tests, TestCreate[StringContainsQ[fastMathCode, "__logf("], True, TestID -> "Verify __logf in fast-math output"]];

(* Fast-math should NOT emit intrinsics when precision is double *)
Block[{FunKit`Private`$codeOptimizationLevel = 3, FunKit`Private`$codeFastMath = True, FunKit`Private`$codePrecision = "double", FunKit`Private`$codeGPURegisterBudget = 32},
    noFastMathCode = CppCode[Exp[x] + Log[x]];
];

AppendTo[tests, TestCreate[StringFreeQ[noFastMathCode, "__expf("], True, TestID -> "Verify no __expf when precision is double"]];

(**********************************************************************************
    Sub-kernel splitting
**********************************************************************************)

largeExprSplit = Sum[Sin[a + i] * Cos[a - i] / (1 + i * a), {i, 1, 600}];

Block[{FunKit`Private`$codeOptimizationLevel = 3, FunKit`Private`$codeMaxKernelTerms = 200, FunKit`Private`$codeGPURegisterBudget = 64},
    splitCode = CppCode[largeExprSplit];
];

AppendTo[tests, TestCreate[StringContainsQ[splitCode, "// subkernel 1"], True, TestID -> "Verify sub-kernel splitting produces multiple blocks"]];
AppendTo[tests, TestCreate[StringContainsQ[splitCode, "// subkernel 2"], True, TestID -> "Verify sub-kernel splitting produces at least 2 blocks"]];

(**********************************************************************************
    Transcendental hoisting
**********************************************************************************)

Block[{FunKit`Private`$codeOptimizationLevel = 3, FunKit`Private`$codeGPURegisterBudget = 32},
    tranCode = CppCode[Exp[a + b * c] + 2 * Exp[a + b * c]];
];

AppendTo[tests, TestCreate[StringContainsQ[tranCode, "_tran"], True, TestID -> "Verify transcendental hoisting creates _tran variables"]];
