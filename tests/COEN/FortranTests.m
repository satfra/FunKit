tests = {};

(**********************************************************************************
    Setup
**********************************************************************************)

hasFortran = Quiet[RunProcess[{"gfortran", "--version"}]] =!= $Failed;

If[!hasFortran,
    Print["Fortran compiler not found, skipping tests."];
    Return[];
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

execFile1 = $TemporaryDirectory <> "/FunKitFortranTest1.f90";
execPath1 = $TemporaryDirectory <> "/FunKitFortranTest1";
Export[execFile1, code1, "Text"];

compile1 = RunProcess[{"gfortran", "-ffree-form", "-o", execPath1, execFile1}];

AppendTo[tests, TestCreate[compile1["ExitCode"], 0, TestID -> "Verify compilation of basic Fortran function"]];

output1 = If[compile1["ExitCode"] === 0, RunProcess[{execPath1}], <|"StandardOutput" -> ""|>];

AppendTo[tests, TestCreate[StringTrim[output1["StandardOutput"]], "42", TestID -> "Verify return value of basic Fortran function"]];

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

execFile2 = $TemporaryDirectory <> "/FunKitFortranTest2.f90";
execPath2 = $TemporaryDirectory <> "/FunKitFortranTest2";
Export[execFile2, code2, "Text"];

compile2 = RunProcess[{"gfortran", "-ffree-form", "-o", execPath2, execFile2}];

AppendTo[tests, TestCreate[compile2["ExitCode"], 0, TestID -> "Verify compilation of Fortran function with arithmetic operations"]];

output2 = If[compile2["ExitCode"] === 0, RunProcess[{execPath2}], <|"StandardOutput" -> ""|>];

expectedVal = N[expr /. a -> 1.5, 15];
fortranVal = ToExpression[StringTrim[output2["StandardOutput"]]];

AppendTo[tests, TestCreate[Abs[fortranVal - expectedVal] < 1*^-8, True, TestID -> "Verify return value of Fortran function with arithmetic operations"]];

(**********************************************************************************
    Optimization pipeline test: CSE variables appear in output
**********************************************************************************)

ClearAll[a, b]
exprOpt = Sin[a + b]^2 + Cos[a + b]^3 + Sin[a + b]*Log[b + 1] + Cos[a + b]*Exp[b];

fortranCodeOpt = FortranCode[exprOpt];
AppendTo[tests, TestCreate[
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

execFileOpt = $TemporaryDirectory <> "/FunKitFortranTestOpt.f90";
execPathOpt = $TemporaryDirectory <> "/FunKitFortranTestOpt";
Export[execFileOpt, codeOpt, "Text"];

compileOpt = RunProcess[{"gfortran", "-ffree-form", "-o", execPathOpt, execFileOpt}];

AppendTo[tests, TestCreate[compileOpt["ExitCode"], 0, TestID -> "Verify compilation of optimized Fortran function"]];

outputOpt = If[compileOpt["ExitCode"] === 0, RunProcess[{execPathOpt}], <|"StandardOutput" -> ""|>];

expectedOptVal = N[exprOpt /. {a -> 1.2, b -> 0.7}, 15];
fortranOptVal = ToExpression[StringTrim[outputOpt["StandardOutput"]]];

AppendTo[tests, TestCreate[Abs[fortranOptVal - expectedOptVal] < 1*^-8, True, TestID -> "Optimized Fortran function returns correct value"]];

(**********************************************************************************
    Simple expression: plain return, no CSE
**********************************************************************************)

ClearAll[x, y]
simpleCode = FortranCode[x + y];

AppendTo[tests, TestCreate[
    StringStartsQ[simpleCode, "kernel"],
    True,
    TestID -> "Simple expression produces plain result assignment without definitions"
]];

AppendTo[tests, TestCreate[
    !StringContainsQ[simpleCode, "fkcse"] && !StringContainsQ[simpleCode, "fkinterp"],
    True,
    TestID -> "Simple expression produces no CSE variables"
]];
