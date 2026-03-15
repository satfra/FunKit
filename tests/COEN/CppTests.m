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
    Optimization enabled (True) produces valid C++ code
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True},
    funBodyOpt = MakeCppFunction[expr, "Name" -> "funOpt", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

execOpt = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBodyOpt <> "

int main () {
  std::cout << std::setprecision (10) << funOpt (1.5) << std::endl;
}
", "FunKitCppTestOpt", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

outputOpt = Import["!" <> QuoteFile[execOpt], "Text"];

AppendTo[tests, TestCreate[execOpt =!= $Failed, True, TestID -> "Verify compilation with optimization enabled"]];

AppendTo[tests, TestCreate[outputOpt, expected, TestID -> "Verify numerical agreement with optimization enabled"]];

(**********************************************************************************
    Optimization disabled (False) produces valid C++ code
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = False},
    funBodyNoOpt = MakeCppFunction[expr, "Name" -> "funNoOpt", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

execNoOpt = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBodyNoOpt <> "

int main () {
  std::cout << std::setprecision (10) << funNoOpt (1.5) << std::endl;
}
", "FunKitCppTestNoOpt", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

outputNoOpt = Import["!" <> QuoteFile[execNoOpt], "Text"];

AppendTo[tests, TestCreate[execNoOpt =!= $Failed, True, TestID -> "Verify compilation with optimization disabled"]];

AppendTo[tests, TestCreate[outputNoOpt, expected, TestID -> "Verify numerical agreement with optimization disabled"]];

(**********************************************************************************
    FMA helper (needed by all tests below since all passes now run)
**********************************************************************************)

fmaCode = "
#include <cmath>
auto fma(auto a, auto b, auto c) { return std::fma(a, b, c); }
";

(**********************************************************************************
    Large expression with accumulator pattern compiles correctly
**********************************************************************************)

largeExpr = Sum[Sin[a + i] * Cos[a - i] / (1 + i * a), {i, 1, 80}];

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxChunkSize = 10, FunKit`Private`$availableRegisters = 8, FunKit`Private`$codeMaxKernelTerms = 10000},
    funBody6 = MakeCppFunction[largeExpr, "Name" -> "funLarge", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec6 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>
using NumberType = double;

" <> fmaCode <> "
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
    FMA-enabled optimization produces valid C++ code
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True},
    funBody7 = MakeCppFunction[expr, "Name" -> "funFMA", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

exec7 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> fmaCode <> "
" <> powrCode <> "
" <> funBody7 <> "

int main () {
  std::cout << std::setprecision (10) << funFMA (1.5) << std::endl;
}
", "FunKitCppTest7", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

output7 = Import["!" <> QuoteFile[exec7], "Text"];

AppendTo[tests, TestCreate[exec7 =!= $Failed, True, TestID -> "Verify compilation with FMA optimization"]];

AppendTo[tests, TestCreate[output7, expected, TestID -> "Verify numerical agreement with FMA optimization"]];

(**********************************************************************************
    FMA detection: verify fma() appears in output
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeFMARestructure = True},
    fmaTestCode = CppCode[a * b + c * d + e];
];

AppendTo[tests, TestCreate[StringContainsQ[fmaTestCode, "fma("], True, TestID -> "Verify FMA detection in optimized output"]];

(**********************************************************************************
    Fast-math intrinsics emission
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeFastMath = True, FunKit`Private`$codePrecision = "single"},
    fastMathCode = CppCode[Exp[x] + Log[x]];
];

AppendTo[tests, TestCreate[StringContainsQ[fastMathCode, "__expf("], True, TestID -> "Verify __expf in fast-math output"]];
AppendTo[tests, TestCreate[StringContainsQ[fastMathCode, "__logf("], True, TestID -> "Verify __logf in fast-math output"]];

(* Fast-math should NOT emit intrinsics when precision is double *)
Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeFastMath = True, FunKit`Private`$codePrecision = "double"},
    noFastMathCode = CppCode[Exp[x] + Log[x]];
];

AppendTo[tests, TestCreate[StringFreeQ[noFastMathCode, "__expf("], True, TestID -> "Verify no __expf when precision is double"]];

(**********************************************************************************
    Sub-kernel splitting
**********************************************************************************)

largeExprSplit = Sum[Sin[a + i] * Cos[a - i] / (1 + i * a), {i, 1, 600}];

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    splitCode = CppCode[largeExprSplit];
];

AppendTo[tests, TestCreate[StringContainsQ[splitCode, "// subkernel 1"], True, TestID -> "Verify sub-kernel splitting produces multiple blocks"]];
AppendTo[tests, TestCreate[StringContainsQ[splitCode, "// subkernel 2"], True, TestID -> "Verify sub-kernel splitting produces at least 2 blocks"]];

(**********************************************************************************
    Transcendental hoisting
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True},
    tranCode = CppCode[Exp[a + b * c] + 2 * Exp[a + b * c]];
];

AppendTo[tests, TestCreate[StringContainsQ[tranCode, "_tran"], True, TestID -> "Verify transcendental hoisting creates _tran variables"]];
