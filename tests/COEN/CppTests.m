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
", "FunKitCppTest1", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

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
    Two-argument ArcTan -> atan2

    Mathematica's ArcTan[x, y] takes the x-part first, C's atan2(y, x) the y-part first.
    Emitting the arguments positionally reflects the angle about the pi/4 axis, which agrees
    with the truth only on that axis -- so the test has to probe all four quadrants.
**********************************************************************************)

atan2Pts = {{1., 2.}, {-1., 2.}, {-1., -2.}, {1., -2.}};

funBodyAtan2 = MakeCppFunction[ArcTan[a, b], "Name" -> "funAtan2",
    "Body" -> "using namespace std; const auto a = x; const auto b = y;", "Parameters" -> {"x", "y"}];

execAtan2 = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>

" <> powrCode <> "
" <> funBodyAtan2 <> "

int main () {
  std::cout << std::setprecision (10);
" <> StringJoin[("  std::cout << funAtan2 (" <> ToString[NumberForm[#[[1]], 10]] <> ", " <>
    ToString[NumberForm[#[[2]], 10]] <> ") << std::endl;\n") & /@ atan2Pts] <> "}
", "FunKitCppTestAtan2", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

outputAtan2 = Import["!" <> QuoteFile[execAtan2], "Text"];

expectedAtan2 = StringRiffle[
    ToString[NumberForm[N@ArcTan[#[[1]], #[[2]]], 10]] & /@ atan2Pts, "\n"];

AppendTo[tests, VerificationTest[execAtan2 =!= $Failed, True, TestID -> "Verify compilation of C++ function with two-argument ArcTan"]];

AppendTo[tests, VerificationTest[outputAtan2, expectedAtan2, TestID -> "Verify two-argument ArcTan maps to atan2 with the correct argument order"]];

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
    Composite-denominator hoisting (Pass 1b)
    Negative integer powers of a Plus/Times base — the regulated-propagator
    denominators 1/(M^2 + q*(...)^2) — are hoisted to shared "_den" defs so a
    denominator shared across split sub-kernels is computed once, not recomputed
    per chunk by the per-sub-kernel CSE. See CppOptimize.m hoistDivisions and
    Tools.m $codeHoistDivisions.
**********************************************************************************)

ClearAll[a, mm, b, c];

(* A composite denominator is hoisted to a _den variable. *)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeHoistDivisions = True},
    denHoistCode = CppCode[Sin[a] / (mm + a^2) + Cos[a] / (mm + a^2)];
];

AppendTo[tests, VerificationTest[StringContainsQ[denHoistCode, "_den"], True, TestID -> "Composite denominator hoisting creates _den variables"]];

(* With hoisting disabled the same denominator falls to the per-kernel CSE, so no
   _den appears — guards the $codeHoistDivisions toggle. *)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeHoistDivisions = False},
    denNoHoistCode = CppCode[Sin[a] / (mm + a^2) + Cos[a] / (mm + a^2)];
];

AppendTo[tests, VerificationTest[StringFreeQ[denNoHoistCode, "_den"], True, TestID -> "No _den variables when $codeHoistDivisions is False"]];

(* A denominator shared across many terms that split into sub-kernels must be
   hoisted to exactly ONE shared def (the efficiency invariant: not recomputed
   once per chunk). Exercises the transitive reference tracking in earlySplit. *)

ClearAll[a];
denSharedExpr = Sum[Sin[a + i] Cos[a - i] / (3 + a^2), {i, 1, 600}];

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeHoistDivisions = True, FunKit`Private`$codeMaxKernelTerms = 200},
    denSharedCode = CppCode[denSharedExpr];
];

AppendTo[tests, VerificationTest[StringContainsQ[denSharedCode, "// subkernel 1"], True, TestID -> "Shared denominator: expression does split into sub-kernels"]];

AppendTo[tests, VerificationTest[Length @ StringCases[denSharedCode, "const auto _den" ~~ DigitCharacter.. ~~ " ="], 1, TestID -> "Shared denominator hoisted to a single def across sub-kernels"]];

(* ...and the split kernel still computes the right value. *)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeHoistDivisions = True, FunKit`Private`$codeMaxKernelTerms = 200},
    funBodyDen = MakeCppFunction[denSharedExpr, "Name" -> "funDen", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

execDen = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>
using NumberType = double;

" <> fmaCode <> "
" <> powrCode <> "
" <> funBodyDen <> "

int main () {
  std::cout << std::setprecision (10) << funDen (1.5) << std::endl;
}
", "FunKitCppTestDen", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

outputDen = Import["!" <> QuoteFile[execDen], "Text"];

expectedDen = ToString[NumberForm[denSharedExpr /. a -> 1.5, 10]];

AppendTo[tests, VerificationTest[execDen =!= $Failed, True, TestID -> "Shared denominator hoisting compiles with sub-kernel splitting"]];

AppendTo[tests, VerificationTest[outputDen, expectedDen, TestID -> "Shared denominator hoisting preserves numerical value across sub-kernels"]];

(* Nested denominators: inner is hoisted first and the outer def references it
   (topological ordering by LeafCount + nested substitution). Compile & run to
   confirm the dependency-ordered defs produce the correct value. *)

ClearAll[a, b, c];
denNestedExpr = 1 / (b + 1 / (c + a^2)) + a / (c + a^2);

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeHoistDivisions = True},
    funBodyNested = MakeCppFunction[denNestedExpr /. {b -> 0.7, c -> 1.3}, "Name" -> "funNested", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

execNested = CreateExecutable["
#include <iostream>
#include <iomanip>
#include <cmath>
using NumberType = double;

" <> fmaCode <> "
" <> powrCode <> "
" <> funBodyNested <> "

int main () {
  std::cout << std::setprecision (10) << funNested (1.5) << std::endl;
}
", "FunKitCppTestNested", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

outputNested = Import["!" <> QuoteFile[execNested], "Text"];

expectedNested = ToString[NumberForm[denNestedExpr /. {b -> 0.7, c -> 1.3, a -> 1.5}, 10]];

AppendTo[tests, VerificationTest[execNested =!= $Failed, True, TestID -> "Nested denominator hoisting compiles"]];

AppendTo[tests, VerificationTest[outputNested, expectedNested, TestID -> "Nested denominator hoisting preserves numerical value"]];

(**********************************************************************************
    ReturnTransform option (post-processing injection)
**********************************************************************************)

ClearAll[a, b];

exprRT = (Cos[a] + Sin[a] ^ 2) / (1 + a);

(* Identity transform on standard path: byte-identical to default *)

AppendTo[tests, VerificationTest[CppCode[exprRT, "ReturnTransform" -> Identity], CppCode[exprRT], TestID -> "ReturnTransform Identity matches default (standard path)"]];

(* Re wrap on standard path: CppForm[Re[_]] = real(...) *)

AppendTo[tests, VerificationTest[StringContainsQ[CppCode[exprRT, "ReturnTransform" -> Re], "return real("], True, TestID -> "ReturnTransform Re wraps return on standard path"]];

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

AppendTo[tests, VerificationTest[StringContainsQ[largeReRT, "return real(_acc)"], True, TestID -> "ReturnTransform Re wraps _acc on sub-kernel path"]];

(* MakeCppFunction forwards the option *)

funBodyRT = MakeCppFunction[exprRT, "Name" -> "fun", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}, "ReturnTransform" -> Re];

AppendTo[tests, VerificationTest[StringContainsQ[funBodyRT, "real("], True, TestID -> "MakeCppFunction forwards ReturnTransform"]];

(* Constant transform: ignores input, returns a literal *)

AppendTo[tests, VerificationTest[StringContainsQ[CppCode[a + b, "ReturnTransform" -> (42&)], "return 42."], True, TestID -> "ReturnTransform constant function applies"]];

(**********************************************************************************
    Regression: long function signature must not eat following statements
    (`// clang-format on` was emitted without a trailing newline, so the next
    `;`-split chunk landed on the same line and was dropped by
    fixClangFormatOffIndentation; see Cpp.m:248)
**********************************************************************************)

(* Build a parameter list long enough to push the signature past
   $codeFormatStatementLimit (1000 chars) and trigger the clang-format-off wrap. *)

longInterpType = "SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>";

longParams = Join[{"k", "Nf"}, Map[<|"Name" -> #, "Type" -> longInterpType, "Const" -> True, "Reference" -> True|>&, {"ZA3", "ZAcbc", "ZA4", "ZAqbq1", "dtZc", "Zc", "dtZA", "ZA", "dtZq", "Zq", "Mq", "lambda4F1"}]];

(* expr references an interpolator-like call so _interp1 is generated. *)

longSigBody = MakeCppFunction[dtZA[(1 + k^6) ^ (1/6)] + RB[k^2, l1^2] * ZA[l1], "Name" -> "kernel", "Prefix" -> "static", "Body" -> "using namespace DiFfRG;using namespace DiFfRG::compute;\n", "Parameters" -> longParams];

interpRefs = DeleteDuplicates @ StringCases[longSigBody, "_interp" ~~ DigitCharacter..];

interpDefNames = DeleteDuplicates @ Flatten @ StringCases[longSigBody, "const auto " ~~ x : ("_interp" ~~ DigitCharacter..) :> x];

AppendTo[tests, VerificationTest[Complement[interpRefs, interpDefNames], {}, TestID -> "Long signature: every referenced _interp has a definition"]];

AppendTo[tests, VerificationTest[StringContainsQ[longSigBody, "using namespace DiFfRG::compute;"], True, TestID -> "Long signature: trailing using-statement is not eaten by clang-format-on"]];

(**********************************************************************************
    Regression: a sum (Plus) appearing as a factor in a product must keep its
    grouping parentheses. Times is Orderless, so a single Plus factor is always
    matched and parenthesized; but with >= 2 Plus factors the trailing sum used
    to lose its parentheses, e.g. (a+b)*(c+d) was printed as "(a + b) * c + d".
    This silently corrupted generated kernels (the ZA quark-loop angular factor;
    see FUNKIT_KERNEL_PRINTER_BUG). For power-free expressions the emitted C++ is
    also valid Wolfram syntax, so we re-parse it and compare numerically — this
    catches the dropped grouping without needing a compiler.
**********************************************************************************)

parenRTSample = {za -> 1.3, zb -> 0.7, zc -> 2.1, zd -> 0.4, k -> 1.9, m1 -> 0.5, m2 -> 1.1, d1 -> 0.8, d2 -> 1.7, l1 -> 2.3, p -> 0.6, r -> 1.4};

parenRTExprs =
    {
        (za + zb) (zc + zd)
        ,(* two pure sum factors *)
        k (za + zb) (zc + zd)
        , (* prefactor + two sum factors *)
        (m1 + m2) (d1 + d2) (l1 + 2 r - 3 p)(* mirrors the ZA quark-loop term *)
    };

Do[AppendTo[tests, VerificationTest[ToExpression[FunKit`CppForm[e]] /. parenRTSample, e /. parenRTSample, SameTest -> (Abs[#1 - #2] < 10. ^ -9&), TestID -> "Sum factor keeps parentheses in product (round-trip): " <> ToString[e, InputForm]]], {e, parenRTExprs}];

(**********************************************************************************
    Regression: Cot/Coth/ArcCot/ArcCoth must respect $CppPowr. With powr disabled
    they previously still emitted powr<-1>(...) unconditionally, referencing an
    undefined template (compile error) when integer powers correctly used pow().
**********************************************************************************)

Block[{FunKit`Private`$CppPowr = False},
    Do[AppendTo[tests, VerificationTest[StringFreeQ[FunKit`CppForm[fn[x]], "powr"], True, TestID -> "Cotangent family is powr-free when $CppPowr is False: " <> ToString[fn]]], {fn, {Cot, Coth, ArcCot, ArcCoth}}]
];

(* And still uses powr<-1> by default (regression guard the other way). *)

AppendTo[tests, VerificationTest[StringContainsQ[FunKit`CppForm[Cot[x]], "powr<-1>"], True, TestID -> "Cot uses powr<-1> when $CppPowr is True (default)"]];

(**********************************************************************************
    expm1: exp(x)-1 should emit expm1 (more accurate near x=0). The patterns must
    match the -1./1. that N[] produces, not the literal integer -1/1.
**********************************************************************************)

AppendTo[tests, VerificationTest[StringContainsQ[FunKit`CppForm[Exp[x] - 1], "expm1"], True, TestID -> "exp(x)-1 emits expm1"]];

AppendTo[tests, VerificationTest[StringContainsQ[FunKit`CppForm[1 - Exp[x]], "expm1"], True, TestID -> "1-exp(x) emits expm1"]];

(**********************************************************************************
    Regression: newline/brace formatting of MakeCppFunction output.
    "\n\n" -> "" in the body assembly glued the opening brace to the first
    statement ("{const auto ..."), and the ";"-based clang-format wrapper glued
    its markers mid-line (";// clang-format off") while leaving spurious blank
    lines around the fenced region; see Cpp.m wrapLargeStatementsForClangFormat
    and MakeCppFunction.
**********************************************************************************)

ClearAll[a, b];

(* -- small default function: brace on its own line, nothing glued, no blank lines *)

fmtSmall = MakeCppFunction[a + b, "Name" -> "fmtSmall", "Body" -> "const auto a = in;\nconst auto b = in;", "Parameters" -> {"in"}];

AppendTo[tests, VerificationTest[StringContainsQ[fmtSmall, "\n{\n"], True, TestID -> "Formatting: opening brace sits on its own line"]];

AppendTo[tests, VerificationTest[StringFreeQ[fmtSmall, "{const"], True, TestID -> "Formatting: opening brace is not glued to the first statement"]];

AppendTo[tests, VerificationTest[StringFreeQ[fmtSmall, "\n\n"], True, TestID -> "Formatting: no blank lines inside a small function"]];

AppendTo[tests, VerificationTest[StringStartsQ[fmtSmall, " "], False, TestID -> "Formatting: empty Prefix leaves no leading space in the signature"]];

(* -- long return statement (> $codeFormatStatementLimit) triggers the fence; markers
      must land on their own lines, with nothing glued and no blank lines *)

ClearAll[a];

Block[{FunKit`Private`$codeOptimize = False},
    longRetBody = MakeCppFunction[Sum[Sin[a + i], {i, 1, 200}], "Name" -> "longRet", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

AppendTo[tests, VerificationTest[StringContainsQ[longRetBody, "// clang-format off"], True, TestID -> "Long statement: over-long return line is fenced"]];

AppendTo[tests, VerificationTest[StringFreeQ[longRetBody, ";// clang-format"], True, TestID -> "Long statement: fence marker is not glued to the previous statement"]];

AppendTo[tests, VerificationTest[StringFreeQ[longRetBody, "\n\n"], True, TestID -> "Long statement: fencing introduces no blank lines"]];

AppendTo[tests, VerificationTest[
    AllTrue[Select[StringSplit[longRetBody, "\n"], StringContainsQ[#, "// clang-format"]&],
        StringMatchQ[StringTrim[#], "// clang-format " ~~ ("off" | "on")]&],
    True, TestID -> "Long statement: every clang-format marker occupies its own line"]];

(* -- existing long-signature scenario: additionally assert clean formatting *)

AppendTo[tests, VerificationTest[StringFreeQ[longSigBody, ";// clang-format"], True, TestID -> "Long signature: fence marker not glued mid-line"]];

AppendTo[tests, VerificationTest[StringFreeQ[longSigBody, "\n\n"], True, TestID -> "Long signature: no blank lines in output"]];

(* -- sub-kernel path: the decltype fence emitted by CppCode must not be
      double-wrapped; off/on markers must strictly alternate *)

ClearAll[a];

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    subkBody = MakeCppFunction[largeExprSplit, "Name" -> "subkFmt", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

subkMarkers = Select[StringTrim /@ StringSplit[subkBody, "\n"], StringStartsQ[#, "// clang-format"]&];

AppendTo[tests, VerificationTest[
    EvenQ[Length[subkMarkers]] && subkMarkers === Flatten[ConstantArray[{"// clang-format off", "// clang-format on"}, Length[subkMarkers] / 2]],
    True, TestID -> "Sub-kernel path: clang-format markers alternate off/on (no double-wrapping)"]];

AppendTo[tests, VerificationTest[StringFreeQ[subkBody, "\n\n"], True, TestID -> "Sub-kernel path: no blank lines inside the function"]];

(* -- MakeCppFunctionSplit: the per-function newline collapsing must not eat the
      "\n\n" separators BETWEEN the emitted sub-functions *)

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    splitFns = MakeCppFunctionSplit[largeExprSplit, "Name" -> "splitFmt", "Body" -> "using namespace std; const auto a = in;", "Parameters" -> {"in"}];
];

AppendTo[tests, VerificationTest[StringContainsQ[splitFns, "splitFmt_sub1"] && StringContainsQ[splitFns, "}\n\n"], True, TestID -> "MakeCppFunctionSplit: blank-line separators between sub-functions survive"]];

(* -- "Body" -> None: pure declaration, no braces *)

declOnly = MakeCppFunction["Name" -> "declOnly", "Parameters" -> {"in"}, "Body" -> None];

AppendTo[tests, VerificationTest[StringFreeQ[declOnly, "{"] && StringEndsQ[StringTrim[declOnly], ";"], True, TestID -> "Body None: emits a brace-free declaration ending in a semicolon"]];

(* -- Class/Templates/Prefix/Suffix signature shape *)

shapeFn = MakeCppFunction[a + b, "Name" -> "shape", "Class" -> "MyClass", "Templates" -> {"T"}, "Prefix" -> "static", "Suffix" -> "const", "Body" -> "const auto a = in;\nconst auto b = in;", "Parameters" -> {"in"}];

AppendTo[tests, VerificationTest[
    StringContainsQ[shapeFn, "template<typename T"] && StringContainsQ[shapeFn, "static auto MyClass::shape"] && StringContainsQ[shapeFn, ") const"] && StringFreeQ[shapeFn, "\n\n"],
    True, TestID -> "Signature shapes: Class/Templates/Prefix/Suffix compose without blank lines"]];

(* -- multi-line user Body with blank lines and comments (Yang-Mills DSEAcbc1Loop
      style): blank lines collapse instead of gluing, comments survive *)

ClearAll[k, l1];

ymUserBody = "using namespace DiFfRG;\nusing namespace DiFfRG::compute;\n\n// shorthand definitions\nconst double p = 1.;\n";

ymFn = MakeCppFunction[dtZA[(1 + k^6) ^ (1/6)] + RB[k^2, l1^2] * ZA[l1], "Name" -> "ymKernel", "Parameters" -> {"k", "l1"}, "Body" -> ymUserBody];

AppendTo[tests, VerificationTest[StringFreeQ[ymFn, "{using"] && StringFreeQ[ymFn, "{const"], True, TestID -> "User Body: first body line not glued to opening brace"]];

AppendTo[tests, VerificationTest[StringContainsQ[ymFn, "// shorthand definitions"], True, TestID -> "User Body: comments survive newline collapsing"]];

AppendTo[tests, VerificationTest[StringFreeQ[ymFn, "\n\n"], True, TestID -> "User Body: blank lines in user Body are collapsed, not glued"]];
