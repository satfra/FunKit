tests = {};

(**********************************************************************************
    Kernels with several outputs ("MultiReturn") and shared coordinate transforms of
    interpolator lookups ("SharedGrids").

    The generated code is compiled and run, and its numbers are compared against Mathematica,
    so the tests check what the code computes, not how it is spelled.
**********************************************************************************)

Needs["CCompilerDriver`"];

multiCompiler =
    Which[
        Quiet[RunProcess[{"g++", "--version"}]] =!= $Failed, "g++",
        Quiet[RunProcess[{"clang++", "--version"}]] =!= $Failed, "clang++",
        True, ""
    ];

(* powr as DiFfRG has it, a three-value return type, and an "interpolator" that counts how often
   its coordinate transform index() runs. *)
multiPreamble = "
#include <cmath>
#include <cstdio>
using std::sqrt; using std::exp; using std::log; using std::fma;
template<int n, typename T> constexpr T powr(const T x)
{
  if constexpr (n == 0) return T(1.);
  else if constexpr (n < 0) return T(1.) / powr<-n>(x);
  else if constexpr (n > 1) return x * powr<n-1>(x);
  else return x;
}
struct MV3 { double v[3]; MV3(double a, double b, double c) : v{a, b, c} {} };
static int index_calls = 0;
struct Grid1D {
  double scale;
  double index(double x) const { ++index_calls; return log(x); }
  double at(double i) const { return scale * i; }
  double operator()(double x) const { return at(index(x)); }
};
const Grid1D f{2.}, g{3.}, h{5.};
";

(* Compile `function` and print fun(x, y)'s three components (and the index() count). *)
runMulti[function_String, name_String] :=
    Module[{exe},
        exe = CreateExecutable[multiPreamble <> function <> "
int main() {
  const MV3 r = fun(1.7, 0.6);
  std::printf(\"%.17g %.17g %.17g %d\\n\", r.v[0], r.v[1], r.v[2], index_calls);
}", name, "CompilerName" -> multiCompiler, "SystemCompileOptions" -> "-std=c++20"];
        If[exe === $Failed, $Failed, ToExpression /@ StringSplit[Import["!" <> QuoteFile[exe], "Text"]]]
    ];

(* Mathematica's values of the three outputs, with the "interpolators" of multiPreamble *)
multiReference[exprs_, xv_, yv_] :=
    N[exprs /. {f[a_] :> 2 Log[a], g[a_] :> 3 Log[a], h[a_] :> 5 Log[a]} /. {x -> xv, y -> yv}];

closeQ[a_List, b_List] := Max[Abs[a - b] / (1 + Abs[b])] < 10^-12;

(* three projections of one integrand: a shared part times a projection-specific one *)
multiShared = Exp[-x^2] / (1 + x y + f[x]);
multiExprs = {multiShared * (1 + y^2), multiShared * g[x] * x, multiShared * (x - y) + h[y]};

(**********************************************************************************
    One output as a list = the scalar path, apart from the return line
**********************************************************************************)

AppendTo[tests, VerificationTest[
    StringReplace[CppCode[{multiExprs[[1]]}, "MultiReturn" -> "MV"], "return MV(" ~~ v : Shortest[__] ~~ ");" :> "return " <> v <> ";"],
    CppCode[multiExprs[[1]]],
    TestID -> "COEN-Multi-OneOutputIsScalar"]
];

(**********************************************************************************
    Three outputs in one kernel
**********************************************************************************)

If[multiCompiler =!= "",
    multiOut = runMulti[MakeCppFunction[multiExprs, "Name" -> "fun", "Parameters" -> {"x", "y"}, "MultiReturn" -> "MV3"], "FunKitMultiSingle"];
    AppendTo[tests, VerificationTest[
        closeQ[Take[multiOut, 3], multiReference[multiExprs, 1.7, 0.6]],
        True,
        TestID -> "COEN-Multi-SingleKernelValues"]
    ];

    (* forced sub-kernel split: every output is accumulated in its own _accJ *)
    multiSplitCode = Block[{FunKit`Private`$codeMaxKernelTerms = 4},
        MakeCppFunction[Expand /@ (multiExprs * (1 + x + x^2 + y + y^2 + x y)), "Name" -> "fun", "Parameters" -> {"x", "y"},
            "MultiReturn" -> "MV3", "ReturnTransform" -> {Identity, 2 #&, Identity}]];
    AppendTo[tests, VerificationTest[
        StringContainsQ[multiSplitCode, "_acc0 +="] && StringContainsQ[multiSplitCode, "_acc2 +="],
        True,
        TestID -> "COEN-Multi-SplitHasAccumulators"]
    ];
    multiSplitOut = runMulti[multiSplitCode, "FunKitMultiSplit"];
    AppendTo[tests, VerificationTest[
        closeQ[Take[multiSplitOut, 3], {1, 2, 1} * multiReference[multiExprs * (1 + x + x^2 + y + y^2 + x y), 1.7, 0.6]],
        True,
        TestID -> "COEN-Multi-SplitValuesAndTransforms"]
    ];
];

AppendTo[tests, VerificationTest[
    CheckAbort[CppCode[{x, y}], $Aborted],
    $Aborted,
    {CppCode::multireturn},
    TestID -> "COEN-Multi-NeedsMultiReturn"]
];

AppendTo[tests, VerificationTest[
    CheckAbort[CppCode[{x, y}, "MultiReturn" -> "MV", "ReturnTransform" -> {Identity}], $Aborted],
    $Aborted,
    {CppCode::transforms},
    TestID -> "COEN-Multi-TransformCount"]
];

(**********************************************************************************
    Shared grids
**********************************************************************************)

sharedExpr = f[x] * g[x] + h[x] * f[y];

AppendTo[tests, VerificationTest[
    Module[{code = CppCode[sharedExpr, "SharedGrids" -> {{f, g}}]},
        {StringCount[code, ".index(x)"], StringCount[code, "f.at(_gix1)"], StringCount[code, "g.at(_gix1)"],
         StringContainsQ[code, "h(x)"], StringContainsQ[code, "f(y)"]}
    ],
    {1, 1, 1, True, True},
    TestID -> "COEN-SharedGrids-OneIndexPerGroupAndArgument"]
];

AppendTo[tests, VerificationTest[
    CppCode[sharedExpr, "SharedGrids" -> {}],
    CppCode[sharedExpr],
    TestID -> "COEN-SharedGrids-EmptyIsUnchanged"]
];

(* group members given as strings work the same *)
AppendTo[tests, VerificationTest[
    CppCode[sharedExpr, "SharedGrids" -> {{"f", "g"}}],
    CppCode[sharedExpr, "SharedGrids" -> {{f, g}}],
    TestID -> "COEN-SharedGrids-NamesAsStrings"]
];

If[multiCompiler =!= "",
    (* f, g and h all on one grid: x is transformed once, y once *)
    sharedOut = runMulti[MakeCppFunction[{sharedExpr, sharedExpr * x, f[x] + g[y]}, "Name" -> "fun", "Parameters" -> {"x", "y"},
        "MultiReturn" -> "MV3", "SharedGrids" -> {{f, g, h}}], "FunKitSharedGrids"];
    AppendTo[tests, VerificationTest[
        {closeQ[Take[sharedOut, 3], multiReference[{sharedExpr, sharedExpr * x, f[x] + g[y]}, 1.7, 0.6]], Last[sharedOut]},
        {True, 2},
        TestID -> "COEN-SharedGrids-ValuesAndIndexCount"]
    ];

    (* a split kernel: the _gix indices must stay out of the accumulator type *)
    sharedSplitOut = runMulti[Block[{FunKit`Private`$codeMaxKernelTerms = 4},
        MakeCppFunction[Expand /@ ({sharedExpr, sharedExpr * x, f[x] + g[y]} * (1 + x + x^2 + y + y^2)), "Name" -> "fun",
            "Parameters" -> {"x", "y"}, "MultiReturn" -> "MV3", "SharedGrids" -> {{f, g, h}}]], "FunKitSharedGridsSplit"];
    AppendTo[tests, VerificationTest[
        closeQ[Take[sharedSplitOut, 3], multiReference[{sharedExpr, sharedExpr * x, f[x] + g[y]} * (1 + x + x^2 + y + y^2), 1.7, 0.6]],
        True,
        TestID -> "COEN-SharedGrids-SplitKernel"]
    ];
];

(* a one-element transform list on a single output acts like the function itself *)
AppendTo[tests, VerificationTest[
    CppCode[multiExprs[[1]], "ReturnTransform" -> {2 #&}],
    CppCode[multiExprs[[1]], "ReturnTransform" -> (2 #&)],
    TestID -> "COEN-Multi-OneTransformList"]
];
