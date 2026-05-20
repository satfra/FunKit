tests = {};

(**********************************************************************************
    DifferentialTests.m -- backend-agnostic differential correctness harness.

    The bug that motivated this suite (a sum factor in a product losing its
    parentheses) shipped because no test compared generated code against an
    INDEPENDENT ground truth. Existing tests check that code is produced, or
    compare optimize-on vs optimize-off (the same printer against itself, which
    agrees even when both are wrong).

    This file fixes that gap: for a corpus that deliberately contains the
    dangerous structural shapes (>=2 sum factors, negated sums, sums in
    denominators, powers of sums, large sums that force sub-kernel splitting), it
    generates code with C++, Fortran and Julia under several optimization settings
    and asserts the COMPILED result equals Mathematica's own numeric value at
    several sample points. Each backend is guarded by a toolchain check and simply
    skipped if its compiler/interpreter is absent.
**********************************************************************************)

Needs["CCompilerDriver`"];

cppCompiler =
    Which[
        Quiet[RunProcess[{"g++", "--version"}]] =!= $Failed, "g++",
        Quiet[RunProcess[{"clang++", "--version"}]] =!= $Failed, "clang++",
        True, ""
    ];
hasCpp = cppCompiler =!= "";
hasFortran = Quiet[RunProcess[{"gfortran", "--version"}]] =!= $Failed;
hasJulia = Quiet[RunProcess[{"julia", "--version"}]] =!= $Failed;

If[!hasCpp, Print["  [DifferentialTests] no C++ compiler — C++ cases skipped."]];
If[!hasFortran, Print["  [DifferentialTests] no gfortran — Fortran cases skipped."]];
If[!hasJulia, Print["  [DifferentialTests] no julia — Julia cases skipped."]];

ClearAll[a, b, c, d, e];

(* powr<N> template and an fma() shim, as used by the existing CppTests. *)
powrCode = "
template<int n, typename NumberType>
  requires requires(NumberType x) { x*x; NumberType(1.)/x; }
constexpr NumberType powr(const NumberType x) {
  if constexpr (n == 0) return NumberType(1.);
  else if constexpr (n < 0) return NumberType(1.) / powr<-n>(x);
  else if constexpr (n > 1) return x * powr<n-1>(x);
  else return x;
}";
fmaCode = "auto fma(auto a, auto b, auto c) { return std::fma(a, b, c); }";

(**********************************************************************************
    Corpus and sample points
**********************************************************************************)

(* {id, expression, FORM-eligible? (FORM = polynomial/rational only)} *)
corpus = {
    {"two-sums",            (a + b) (c + d),                              True},
    {"pref-two-sums",       a (b + c) (d + e),                            True},
    {"three-sums",          (a + b) (c + d) (a - e + 1),                  True},
    {"neg-product-of-sums", -(a + b) (c + d),                            True},
    {"sum-over-sum",        (a + b)/(c + d),                              True},
    {"diff-of-divisions",   a/(b + c) - d/(e + a),                        True},
    {"coeffs",              (a + 2 b - 3 c) (d - e),                      True},
    {"mixed",               a b c + (a + b) (c + d) - e,                  True},
    {"pow-of-sum",          (a + b)^2 (c - d),                            True},
    {"reciprocal-sums",     1/(a + b) + 1/(c + d) + 1/(a + c),            True},
    {"transcendental",      Sin[a + b] (c + d) + Exp[a]/(b + c),          False},
    {"big-sum",             Sum[(a + i b) (c - i d/10), {i, 1, 14}],      True}
};

vars = {a, b, c, d, e};
points = {
    {a -> 1.3, b -> 0.7, c -> 2.1, d -> 0.9, e -> 1.5},
    {a -> 0.6, b -> 1.9, c -> 0.8, d -> 2.2, e -> 1.1}
};

(* {name, $codeOptimize, $codeFMARestructure, $availableRegisters, $codeMaxKernelTerms} *)
cppSettings = {
    {"raw",       False, True,  64, 100000},
    {"opt",       True,  True,  64, 100000},
    {"opt-noFMA", True,  False, 64, 100000},
    {"tight-reg", True,  True,  2,  100000},
    {"split",     True,  True,  64, 3}
};
(* Fortran/Julia expand fmaGroup back to a*b+c, so noFMA is redundant there. *)
genSettings = {
    {"raw",       False, True,  64, 100000},
    {"opt",       True,  True,  64, 100000},
    {"tight-reg", True,  True,  2,  100000},
    {"split",     True,  True,  64, 3}
};

(**********************************************************************************
    Helpers
**********************************************************************************)

parseNum[s_String] := Quiet @ Check[Read[StringToStream[StringTrim[s]], Number], $Failed];
parseNum[_] := $Failed;

(* Compare a list of backend values to the expected ground-truth list. *)
closeAllQ[got_, expected_] :=
    ListQ[got] && Length[got] === Length[expected] &&
    AllTrue[
        Transpose[{got, expected}],
        NumericQ[#[[1]]] && Abs[#[[1]] - #[[2]]] <= 10.^-6 (1 + Abs[#[[2]]]) &
    ];

groundTruth[expr_, pts_] := Map[N[expr /. #] &, pts];

(* Argument strings (one per sample point) for a given parameter list. *)
argStrings[params_, pts_, suffix_String] :=
    Map[
        Function[pt, StringRiffle[Map[ToString[CForm[N[# /. pt]]] <> suffix &, params], ", "]],
        pts
    ];

(* ----- C++ ----- *)
cppCompileRun[body_String, argStrs_List, extraDefs_String] :=
    Module[{src, exec, out},
        src = "#include <iostream>\n#include <iomanip>\n#include <cmath>\nusing namespace std;\n" <>
            extraDefs <> "\n" <> powrCode <> "\n" <> fmaCode <> "\n" <> body <>
            "\nint main(){ std::cout << std::setprecision(17);\n" <>
            StringJoin[Map["std::cout << fun(" <> # <> ") << \"\\n\";\n" &, argStrs]] <>
            "return 0;}\n";
        exec = Quiet @ CreateExecutable[src, "FunKitDiffCpp", "CompilerName" -> cppCompiler, "SystemCompileOptions" -> "-std=c++20"];
        If[exec === $Failed, Return[$Failed]];
        out = Quiet @ Import["!" <> QuoteFile[exec], "Text"];
        If[!StringQ[out], Return[$Failed]];
        parseNum /@ Select[StringSplit[StringTrim[out], "\n"], # =!= "" &]
    ];

cppValues[expr_, setting_, params_, argStrs_, extraDefs_:""] :=
    Module[{body},
        body = Block[
            {FunKit`Private`$codeOptimize = setting[[2]], FunKit`Private`$codeFMARestructure = setting[[3]],
             FunKit`Private`$availableRegisters = setting[[4]], FunKit`Private`$codeMaxKernelTerms = setting[[5]]},
            Quiet @ Check[MakeCppFunction[expr, "Name" -> "fun", "Body" -> "using namespace std;", "Parameters" -> (ToString /@ params)], $Failed]
        ];
        If[!StringQ[body], Return[$Failed]];
        cppCompileRun[body, argStrs, extraDefs]
    ];

(* ----- Fortran ----- *)
fortranValues[expr_, setting_, params_, argStrs_] :=
    Module[{body, src, f90, exe, comp, out},
        body = Block[
            {FunKit`Private`$codeOptimize = setting[[2]], FunKit`Private`$codeFMARestructure = setting[[3]],
             FunKit`Private`$availableRegisters = setting[[4]], FunKit`Private`$codeMaxKernelTerms = setting[[5]]},
            Quiet @ Check[MakeFortranFunction[expr, "Name" -> "fun", "Parameters" -> (ToString /@ params)], $Failed]
        ];
        If[!StringQ[body], Return[$Failed]];
        src = body <> "\n\nprogram main\n  implicit none\n  double precision :: fun\n" <>
            StringJoin[Map["  write(*,'(ES30.18E3)') fun(" <> # <> ")\n" &, argStrs]] <>
            "end program main\n";
        f90 = $TemporaryDirectory <> "/FunKitDiff.f90";
        exe = $TemporaryDirectory <> "/FunKitDiffF";
        Export[f90, src, "Text"];
        comp = RunProcess[{"gfortran", "-ffree-form", "-o", exe, f90}];
        If[comp["ExitCode"] =!= 0, Return[$Failed]];
        out = RunProcess[{exe}];
        If[out["ExitCode"] =!= 0, Return[$Failed]];
        parseNum /@ Select[StringSplit[StringTrim[out["StandardOutput"]], "\n"], # =!= "" &]
    ];

(* ----- Julia ----- *)
juliaValues[expr_, setting_, params_, argStrs_] :=
    Module[{body, src, jl, out},
        body = Block[
            {FunKit`Private`$codeOptimize = setting[[2]], FunKit`Private`$codeFMARestructure = setting[[3]],
             FunKit`Private`$availableRegisters = setting[[4]], FunKit`Private`$codeMaxKernelTerms = setting[[5]]},
            Quiet @ Check[MakeJuliaFunction[expr, "Name" -> "fun", "Parameters" -> (ToString /@ params)], $Failed]
        ];
        If[!StringQ[body], Return[$Failed]];
        src = body <> "\n" <> StringJoin[Map["println(fun(" <> # <> "))\n" &, argStrs]];
        jl = Export[$TemporaryDirectory <> "/FunKitDiff.jl", src, "Text"];
        out = RunProcess[{"julia", jl}];
        If[out["ExitCode"] =!= 0, Return[$Failed]];
        parseNum /@ Select[StringSplit[StringTrim[out["StandardOutput"]], "\n"], # =!= "" &]
    ];

(**********************************************************************************
    Core: corpus x settings x backends, compared to Mathematica ground truth
**********************************************************************************)

cppArgs = argStrings[vars, points, ""];
fortranArgs = argStrings[vars, points, "d0"];
juliaArgs = argStrings[vars, points, ""];
(* Results are computed EAGERLY and injected as literals via With: VerificationTest
   holds its first argument, so building it around a call that references the Do
   iterator would defer evaluation to TestReport time, when the iterator is no
   longer bound. The corpus iterator is `cc`, NOT `c` — `c` is one of the
   expression variables {a,b,c,d,e}, so iterating with `c` would rebind the `c`
   inside the corpus expressions and recurse via self-substitution. *)
Do[
    Module[{id = cc[[1]], expr = cc[[2]], gt = groundTruth[cc[[2]], points]},
        If[hasCpp,
            Do[With[{res = closeAllQ[cppValues[expr, s, vars, cppArgs], gt], tid = "C++ [" <> s[[1]] <> "] " <> id},
                AppendTo[tests, VerificationTest[res, True, TestID -> tid]]], {s, cppSettings}]];
        If[hasFortran,
            Do[With[{res = closeAllQ[fortranValues[expr, s, vars, fortranArgs], gt], tid = "Fortran [" <> s[[1]] <> "] " <> id},
                AppendTo[tests, VerificationTest[res, True, TestID -> tid]]], {s, genSettings}]];
        If[hasJulia,
            Do[With[{res = closeAllQ[juliaValues[expr, s, vars, juliaArgs], gt], tid = "Julia [" <> s[[1]] <> "] " <> id},
                AppendTo[tests, VerificationTest[res, True, TestID -> tid]]], {s, genSettings}]];
    ],
    {cc, corpus}
];

(**********************************************************************************
    Real flow expression (FlowA4[1]) through the full C++ pipeline.
    Ground truth = the optimize=False ("raw") C++ output, which uses only CppForm.
    The corpus above independently validates CppForm against Mathematica, so any
    disagreement here pins a bug in the optimizer (CSE / splitting / FMA / hoisting)
    on a realistic expression with interpolator calls and many terms.
**********************************************************************************)

If[hasCpp,
    Module[{flowA4, flowParams, flowPts, flowArgs, stubDefs, refVals},
        flowA4 = Import[$FunKitDirectory <> "/tests/boilerplate/FlowA4.m"];
        flowParams = {l1, p, k, cosl1p1, cosl1p2, cosl1p3};
        flowPts = {
            {l1 -> 2.5, p -> 1.0, k -> 3.0, cosl1p1 -> 0.2, cosl1p2 -> 0.3, cosl1p3 -> 0.4},
            {l1 -> 1.7, p -> 2.3, k -> 1.1, cosl1p1 -> -0.3, cosl1p2 -> 0.5, cosl1p3 -> -0.1}
        };
        flowArgs = argStrings[flowParams, flowPts, ""];
        stubDefs = "
using NumberType = double;
auto ZAcbc(auto x){return 1.0+0.1*x;} auto ZA(auto x){return 1.0+0.05*x;}
auto ZA3(auto x){return 1.0+0.02*x;} auto ZA4(auto x){return 1.0+0.03*x;}
auto Zc(auto x){return 1.0+0.01*x;} auto dtZc(auto x){return 0.01;} auto dtZA(auto x){return 0.02;}
auto RB(auto k2,auto p2){return k2/(1.0+p2/k2);} auto RBdot(auto k2,auto p2){return 2.0*RB(k2,p2)/k2;}
auto RF(auto k2,auto p2){return sqrt(k2*p2)/(1.0+p2/k2);} auto RFdot(auto k2,auto p2){return 2.0*RF(k2,p2)/k2;}
auto dq2RB(auto k2,auto p2){return -k2/pow(1.0+p2/k2,2)/k2;}
auto dq2RF(auto k2,auto p2){return 0.5*sqrt(k2/p2)/(1.0+p2/k2)-sqrt(k2*p2)/pow(1.0+p2/k2,2)/k2;}
";
        refVals = cppValues[flowA4[[1]], {"raw", False, True, 64, 100000}, flowParams, flowArgs, stubDefs];
        With[{ok = ListQ[refVals] && AllTrue[refVals, NumericQ]},
            AppendTo[tests, VerificationTest[ok, True, TestID -> "FlowA4[1] C++ raw produces numeric reference"]]];
        Do[
            With[{res = closeAllQ[cppValues[flowA4[[1]], s, flowParams, flowArgs, stubDefs], refVals],
                  tid = "FlowA4[1] C++ [" <> s[[1]] <> "] matches raw"},
                AppendTo[tests, VerificationTest[res, True, TestID -> tid]]],
            {s, {{"opt", True, True, 64, 100000}, {"opt-noFMA", True, False, 64, 100000},
                 {"tight-reg", True, True, 4, 100000}, {"split", True, True, 64, 3}}}
        ];
    ]
];

(**********************************************************************************
    FORM code path (CppCodeFORM / CodeParser->"FORM"), if FORM is available.
    Algebraic corpus entries only (FORM does not handle Sin/Exp/Log).
**********************************************************************************)

hasFORM = TrueQ @ Quiet @ CheckAbort[StringQ @ TimeConstrained[CppCodeFORM[a + b], 60, $Failed], False];
If[!hasFORM, Print["  [DifferentialTests] FORM not available — FORM cases skipped."]];

If[hasCpp && hasFORM,
    Do[
        If[cc[[3]],
            Module[{expr = cc[[2]], gt = groundTruth[cc[[2]], points], body},
                body = Quiet @ CheckAbort[
                    Check[MakeCppFunction[expr, "Name" -> "fun", "CodeParser" -> "FORM",
                        "Body" -> "using namespace std;", "Parameters" -> (ToString /@ vars)], $Failed],
                    $Failed];
                With[{res = closeAllQ[If[StringQ[body], cppCompileRun[body, cppArgs, ""], $Failed], gt],
                      tid = "C++ [FORM] " <> cc[[1]]},
                    AppendTo[tests, VerificationTest[res, True, TestID -> tid]]];
            ]
        ],
        {cc, corpus}
    ]
];
