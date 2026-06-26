tests = {};

(**********************************************************************************
    Setup
**********************************************************************************)

hasFortran = Quiet[RunProcess[{"gfortran", "--version"}]] =!= $Failed;

(* gfortran may be absent (e.g. a fresh machine). Code-generation tests below run
   regardless; the compile/run blocks are each guarded by If[hasFortran, ...] so
   they are skipped rather than reported as failures. A bare Return[] here does
   NOT work: the runner evaluates the file via ToExpression, so Return[] would not
   short-circuit the subsequent test definitions. *)
If[!hasFortran,
    Print["Fortran compiler not found, skipping compiler-dependent tests."];
];

ClearAll[a]

(**********************************************************************************
    Basic Fortran Function Test (MakeFortranFunction)
**********************************************************************************)

funBody1 = MakeFortranFunction[a, "Name" -> "fun", "Body" -> "double precision :: a\na = inp", "Parameters" -> {"inp"}];

code1 = funBody1 <> "

program main
  implicit none
  double precision :: res, fun
  res = fun(42.0d0)
  write(*,'(I0)') nint(res)
end program main
";

If[hasFortran,
    execFile1 = $TemporaryDirectory <> "/FunKitFortranTest1.f90";
    execPath1 = $TemporaryDirectory <> "/FunKitFortranTest1";
    Export[execFile1, code1, "Text"];

    compile1 = RunProcess[{"gfortran", "-ffree-form", "-o", execPath1, execFile1}];

    AppendTo[tests, VerificationTest[compile1["ExitCode"], 0, TestID -> "Verify compilation of basic Fortran function"]];

    output1 = If[compile1["ExitCode"] === 0, RunProcess[{execPath1}], <|"StandardOutput" -> ""|>];

    AppendTo[tests, VerificationTest[StringTrim[output1["StandardOutput"]], "42", TestID -> "Verify return value of basic Fortran function"]];
];

(**********************************************************************************
    Testing typical arithmetic operations in Fortran functions
**********************************************************************************)

expr = (Cos[a] + Sin[a] ^ 2 - Log[a] / Sqrt[a]) / (Exp[a] + Tan[a]);

funBody2 = MakeFortranFunction[expr, "Name" -> "fun2", "Body" -> "double precision :: a\na = inp", "Parameters" -> {"inp"}];

code2 = funBody2 <> "

program main
  implicit none
  double precision :: res, fun2
  res = fun2(1.5d0)
  write(*,'(F25.15)') res
end program main
";

If[hasFortran,
    execFile2 = $TemporaryDirectory <> "/FunKitFortranTest2.f90";
    execPath2 = $TemporaryDirectory <> "/FunKitFortranTest2";
    Export[execFile2, code2, "Text"];

    compile2 = RunProcess[{"gfortran", "-ffree-form", "-o", execPath2, execFile2}];

    AppendTo[tests, VerificationTest[compile2["ExitCode"], 0, TestID -> "Verify compilation of Fortran function with arithmetic operations"]];

    output2 = If[compile2["ExitCode"] === 0, RunProcess[{execPath2}], <|"StandardOutput" -> ""|>];

    expectedVal = N[expr /. a -> 1.5, 15];
    fortranVal = ToExpression[StringTrim[output2["StandardOutput"]]];

    AppendTo[tests, VerificationTest[Abs[fortranVal - expectedVal] < 1*^-8, True, TestID -> "Verify return value of Fortran function with arithmetic operations"]];
];

(**********************************************************************************
    Optimization pipeline test: CSE variables appear in output
**********************************************************************************)

ClearAll[a, b]
exprOpt = Sin[a + b]^2 + Cos[a + b]^3 + Sin[a + b]*Log[b + 1] + Cos[a + b]*Exp[b];

fortranCodeOpt = FortranCode[exprOpt];
AppendTo[tests, VerificationTest[
    StringContainsQ[fortranCodeOpt, "fkcse"] || StringContainsQ[fortranCodeOpt, "fkinterp"],
    True,
    TestID -> "Optimization pipeline produces CSE variables for Fortran"
]];

(**********************************************************************************
    Optimized function computes correct numerical result
**********************************************************************************)

funBodyOpt = MakeFortranFunction[exprOpt, "Name" -> "funopt",
    "Body" -> "double precision :: a, b\na = in1\nb = in2",
    "Parameters" -> {"in1", "in2"}];

codeOpt = funBodyOpt <> "

program main
  implicit none
  double precision :: res, funopt
  res = funopt(1.2d0, 0.7d0)
  write(*,'(F25.15)') res
end program main
";

If[hasFortran,
    execFileOpt = $TemporaryDirectory <> "/FunKitFortranTestOpt.f90";
    execPathOpt = $TemporaryDirectory <> "/FunKitFortranTestOpt";
    Export[execFileOpt, codeOpt, "Text"];

    compileOpt = RunProcess[{"gfortran", "-ffree-form", "-o", execPathOpt, execFileOpt}];

    AppendTo[tests, VerificationTest[compileOpt["ExitCode"], 0, TestID -> "Verify compilation of optimized Fortran function"]];

    outputOpt = If[compileOpt["ExitCode"] === 0, RunProcess[{execPathOpt}], <|"StandardOutput" -> ""|>];

    expectedOptVal = N[exprOpt /. {a -> 1.2, b -> 0.7}, 15];
    fortranOptVal = ToExpression[StringTrim[outputOpt["StandardOutput"]]];

    AppendTo[tests, VerificationTest[Abs[fortranOptVal - expectedOptVal] < 1*^-8, True, TestID -> "Optimized Fortran function returns correct value"]];
];

(**********************************************************************************
    Composite-denominator hoisting: _den names must be coerced to valid Fortran
    identifiers (fkden), since Fortran identifiers cannot start with "_". See
    Fortran.m fortranFixNames / fortranVarDeclarations and CppOptimize.m
    hoistDivisions.
**********************************************************************************)

ClearAll[a, mm]

Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeHoistDivisions = True},
    fortranDenCode = FortranCode[Sin[a] / (mm + a^2) + Cos[a] / (mm + a^2)];
];

AppendTo[tests, VerificationTest[
    StringContainsQ[fortranDenCode, "fkden"],
    True,
    TestID -> "Composite denominator hoisting produces fkden variables in Fortran"
]];

(* No leading-underscore _den survives the rename (would be invalid Fortran). *)

AppendTo[tests, VerificationTest[
    StringFreeQ[fortranDenCode, "_den"],
    True,
    TestID -> "No invalid _den identifiers remain in Fortran output"
]];

(* ...and the result is numerically correct end-to-end. *)

funBodyDen = MakeFortranFunction[Sin[a] / (mm + a^2) + Cos[a] / (mm + a^2), "Name" -> "funden",
    "Body" -> "double precision :: a, mm\na = in1\nmm = in2",
    "Parameters" -> {"in1", "in2"}];

(* The hoisted denominator is declared with double precision (required: the
   generated function uses implicit none). *)

AppendTo[tests, VerificationTest[
    StringContainsQ[funBodyDen, "double precision" ~~ Shortest[__] ~~ "fkden"],
    True,
    TestID -> "Hoisted denominator gets a double precision declaration in Fortran"
]];

codeDen = funBodyDen <> "

program main
  implicit none
  double precision :: res, funden
  res = funden(1.2d0, 0.7d0)
  write(*,'(F25.15)') res
end program main
";

If[hasFortran,
    execFileDen = $TemporaryDirectory <> "/FunKitFortranTestDen.f90";
    execPathDen = $TemporaryDirectory <> "/FunKitFortranTestDen";
    Export[execFileDen, codeDen, "Text"];

    compileDen = RunProcess[{"gfortran", "-ffree-form", "-o", execPathDen, execFileDen}];

    AppendTo[tests, VerificationTest[compileDen["ExitCode"], 0, TestID -> "Verify compilation of Fortran function with hoisted denominator"]];

    outputDen = If[compileDen["ExitCode"] === 0, RunProcess[{execPathDen}], <|"StandardOutput" -> ""|>];

    expectedDenVal = N[(Sin[a] / (mm + a^2) + Cos[a] / (mm + a^2)) /. {a -> 1.2, mm -> 0.7}, 15];
    fortranDenVal = ToExpression[StringTrim[outputDen["StandardOutput"]]];

    AppendTo[tests, VerificationTest[Abs[fortranDenVal - expectedDenVal] < 1*^-8, True, TestID -> "Fortran function with hoisted denominator returns correct value"]];
];

(**********************************************************************************
    Simple expression: plain return, no CSE
**********************************************************************************)

ClearAll[x, y]
simpleCode = FortranCode[x + y];

AppendTo[tests, VerificationTest[
    StringStartsQ[simpleCode, "kernel"],
    True,
    TestID -> "Simple expression produces plain result assignment without definitions"
]];

AppendTo[tests, VerificationTest[
    !StringContainsQ[simpleCode, "fkcse"] && !StringContainsQ[simpleCode, "fkinterp"],
    True,
    TestID -> "Simple expression produces no CSE variables"
]];

(**********************************************************************************
    ReturnTransform option (post-processing injection)
**********************************************************************************)

ClearAll[a, b];
exprRT = (Cos[a] + Sin[a]^2) / (1 + a);

(* Identity transform on standard path: byte-identical to default *)
AppendTo[tests, VerificationTest[
    FortranCode[exprRT, "kernel", "ReturnTransform" -> Identity],
    FortranCode[exprRT],
    TestID -> "ReturnTransform Identity matches default Fortran (standard path)"
]];

(* Custom wrapper: rendered as myWrap(...) via FortranForm fallback *)
AppendTo[tests, VerificationTest[
    StringContainsQ[
        FortranCode[exprRT, "kernel", "ReturnTransform" -> Function[v, Global`myWrap[v]]],
        "myWrap("
    ],
    True,
    TestID -> "ReturnTransform custom wrapper appears in Fortran output"
]];

(* Identity on sub-kernel path *)
ClearAll[a];
largeFortranExpr = Sum[Sin[a + i] * Cos[a - i] / (1 + i * a), {i, 1, 600}];
Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    largeDefaultF = FortranCode[largeFortranExpr];
    largeIdentityF = FortranCode[largeFortranExpr, "kernel", "ReturnTransform" -> Identity];
];
AppendTo[tests, VerificationTest[largeIdentityF, largeDefaultF, TestID -> "ReturnTransform Identity matches default Fortran (sub-kernel path)"]];

(* Custom wrapper on sub-kernel path *)
Block[{FunKit`Private`$codeOptimize = True, FunKit`Private`$codeMaxKernelTerms = 200},
    largeWrapF = FortranCode[largeFortranExpr, "kernel", "ReturnTransform" -> Function[v, Global`myWrap[v]]];
];
AppendTo[tests, VerificationTest[
    StringContainsQ[largeWrapF, "myWrap("],
    True,
    TestID -> "ReturnTransform wraps accumulator on Fortran sub-kernel path"
]];

(* MakeFortranFunction forwards the option *)
funBodyRT = MakeFortranFunction[exprRT, "Name" -> "fun3", "Body" -> "double precision :: a\na = inp", "Parameters" -> {"inp"}, "ReturnTransform" -> Function[v, Global`myWrap[v]]];
AppendTo[tests, VerificationTest[StringContainsQ[funBodyRT, "myWrap("], True, TestID -> "MakeFortranFunction forwards ReturnTransform"]];
