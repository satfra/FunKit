tests = {};

(**********************************************************************************
    Setup
**********************************************************************************)

Needs["CCompilerDriver`"];

CppCompiler =
  If[Quiet[RunProcess[{"g++", "--version"}]] =!= $Failed,
    "g++"
    ,
    If[Quiet[RunProcess[{"clang++", "--version"}]] =!= $Failed,
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

AppendTo[tests, VerificationTest[exec1 =!= $Failed, True, TestID -> "Verify compilation of basic C++ function"]];

AppendTo[tests, VerificationTest[output1, "42", TestID -> "Verify return value of basic C++ function"]];

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

AppendTo[tests, VerificationTest[exec2 =!= $Failed, True, TestID -> "Verify compilation of C++ function with arithmetic operations"]];

AppendTo[tests, VerificationTest[output2, expected, TestID -> "Verify return value of C++ function with arithmetic operations"]];

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

AppendTo[tests, VerificationTest[execOpt =!= $Failed, True, TestID -> "Verify compilation with optimization enabled"]];

AppendTo[tests, VerificationTest[outputOpt, expected, TestID -> "Verify numerical agreement with optimization enabled"]];

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

AppendTo[tests, VerificationTest[execNoOpt =!= $Failed, True, TestID -> "Verify compilation with optimization disabled"]];

AppendTo[tests, VerificationTest[outputNoOpt, expected, TestID -> "Verify numerical agreement with optimization disabled"]];

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

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$availableRegisters = 8, FunKit`Private`$codeMaxKernelTerms = 200},
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

AppendTo[tests, VerificationTest[exec6 =!= $Failed, True, TestID -> "Verify compilation of large expression with sub-kernels"]];

AppendTo[tests, VerificationTest[output6, expectedLarge, TestID -> "Verify numerical agreement of large expression with sub-kernels"]];

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

AppendTo[tests, VerificationTest[exec7 =!= $Failed, True, TestID -> "Verify compilation with FMA optimization"]];

AppendTo[tests, VerificationTest[output7, expected, TestID -> "Verify numerical agreement with FMA optimization"]];

(**********************************************************************************
    FMA detection: verify fma() appears in output
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeFMARestructure = True},
    fmaTestCode = CppCode[a * b + c * d + e];
];

AppendTo[tests, VerificationTest[StringContainsQ[fmaTestCode, "fma("], True, TestID -> "Verify FMA detection in optimized output"]];

(**********************************************************************************
    Fast-math intrinsics emission
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeFastMath = True, FunKit`Private`$codePrecision = "single"},
    fastMathCode = CppCode[Exp[x] + Log[x]];
];

AppendTo[tests, VerificationTest[StringContainsQ[fastMathCode, "__expf("], True, TestID -> "Verify __expf in fast-math output"]];
AppendTo[tests, VerificationTest[StringContainsQ[fastMathCode, "__logf("], True, TestID -> "Verify __logf in fast-math output"]];

(* Fast-math should NOT emit intrinsics when precision is double *)
Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeFastMath = True, FunKit`Private`$codePrecision = "double"},
    noFastMathCode = CppCode[Exp[x] + Log[x]];
];

AppendTo[tests, VerificationTest[StringFreeQ[noFastMathCode, "__expf("], True, TestID -> "Verify no __expf when precision is double"]];

(**********************************************************************************
    Sub-kernel splitting
**********************************************************************************)

largeExprSplit = Sum[Sin[a + i] * Cos[a - i] / (1 + i * a), {i, 1, 600}];

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    splitCode = CppCode[largeExprSplit];
];

AppendTo[tests, VerificationTest[StringContainsQ[splitCode, "// subkernel 1"], True, TestID -> "Verify sub-kernel splitting produces multiple blocks"]];
AppendTo[tests, VerificationTest[StringContainsQ[splitCode, "// subkernel 2"], True, TestID -> "Verify sub-kernel splitting produces at least 2 blocks"]];

(**********************************************************************************
    Transcendental hoisting
**********************************************************************************)

Block[{FunKit`Private`$codeOptimize = True},
    tranCode = CppCode[Exp[a + b * c] + 2 * Exp[a + b * c]];
];

AppendTo[tests, VerificationTest[StringContainsQ[tranCode, "_tran"], True, TestID -> "Verify transcendental hoisting creates _tran variables"]];

(**********************************************************************************
    ReturnTransform option (post-processing injection)
**********************************************************************************)

ClearAll[a, b];
exprRT = (Cos[a] + Sin[a]^2) / (1 + a);

(* Identity transform on standard path: byte-identical to default *)
AppendTo[tests, VerificationTest[
    CppCode[exprRT, "ReturnTransform" -> Identity],
    CppCode[exprRT],
    TestID -> "ReturnTransform Identity matches default (standard path)"
]];

(* Re wrap on standard path: CppForm[Re[_]] = real(...) *)
AppendTo[tests, VerificationTest[
    StringContainsQ[CppCode[exprRT, "ReturnTransform" -> Re], "return real("],
    True,
    TestID -> "ReturnTransform Re wraps return on standard path"
]];

(* Identity transform on sub-kernel path: byte-identical to default *)
Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    largeDefaultRT = CppCode[largeExprSplit];
    largeIdentityRT = CppCode[largeExprSplit, "ReturnTransform" -> Identity];
];
AppendTo[tests, VerificationTest[largeIdentityRT, largeDefaultRT, TestID -> "ReturnTransform Identity matches default (sub-kernel path)"]];

(* Re wrap on sub-kernel path: real(_acc) at the return *)
Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    largeReRT = CppCode[largeExprSplit, "ReturnTransform" -> Re];
];
AppendTo[tests, VerificationTest[
    StringContainsQ[largeReRT, "return real(_acc)"],
    True,
    TestID -> "ReturnTransform Re wraps _acc on sub-kernel path"
]];

(* MakeCppFunction forwards the option *)
funBodyRT = MakeCppFunction[exprRT, "Name" -> "fun", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}, "ReturnTransform" -> Re];
AppendTo[tests, VerificationTest[StringContainsQ[funBodyRT, "real("], True, TestID -> "MakeCppFunction forwards ReturnTransform"]];

(* Constant transform: ignores input, returns a literal *)
AppendTo[tests, VerificationTest[
    StringContainsQ[CppCode[a + b, "ReturnTransform" -> (42 &)], "return 42."],
    True,
    TestID -> "ReturnTransform constant function applies"
]];

(**********************************************************************************
    Regression: long function signature must not eat following statements
    (`// clang-format on` was emitted without a trailing newline, so the next
    `;`-split chunk landed on the same line and was dropped by
    fixClangFormatOffIndentation; see Cpp.m:248)
**********************************************************************************)

(* Build a parameter list long enough to push the signature past
   $codeFormatStatementLimit (1000 chars) and trigger the clang-format-off wrap. *)
longInterpType = "SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>";
longParams = Join[
    {"k", "Nf"},
    Map[<|"Name" -> #, "Type" -> longInterpType, "Const" -> True, "Reference" -> True|>&,
        {"ZA3", "ZAcbc", "ZA4", "ZAqbq1", "dtZc", "Zc", "dtZA", "ZA", "dtZq", "Zq", "Mq", "lambda4F1"}]
];

(* expr references an interpolator-like call so _interp1 is generated. *)
longSigBody = MakeCppFunction[dtZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2] * ZA[l1],
    "Name" -> "kernel",
    "Prefix" -> "static",
    "Body" -> "using namespace DiFfRG;using namespace DiFfRG::compute;\n",
    "Parameters" -> longParams
];

interpRefs = DeleteDuplicates @ StringCases[longSigBody, "_interp" ~~ DigitCharacter ..];
interpDefNames = DeleteDuplicates @ Flatten @ StringCases[longSigBody,
    "const auto " ~~ x:("_interp" ~~ DigitCharacter ..) :> x];

AppendTo[tests, VerificationTest[
    Complement[interpRefs, interpDefNames],
    {},
    TestID -> "Long signature: every referenced _interp has a definition"
]];

AppendTo[tests, VerificationTest[
    StringContainsQ[longSigBody, "using namespace DiFfRG::compute;"],
    True,
    TestID -> "Long signature: trailing using-statement is not eaten by clang-format-on"
]];
