tests = {};

(**********************************************************************************
    Setup
**********************************************************************************)

hasJulia = Quiet[RunProcess[{"julia", "--version"}]] =!= $Failed;

If[hasJulia == "",
    Print["Julia compiler not found, skipping tests."];
    Return[];
];

ClearAll[a]

(**********************************************************************************
    Basic Julia Function Test (MakeJuliaFunction)  
**********************************************************************************)

funBody1 = MakeJuliaFunction[a, "Name" -> "fun", "Body" -> "a = in", "Parameters" -> {"in"}];

code1 = funBody1 <> "
print(fun(42))
";

execFile1 = Export[$TemporaryDirectory <> "/FunKitJuliaTest1.jl", code1, "Text"];

output1 = RunProcess[{"julia", execFile1}];

AppendTo[tests, VerificationTest[output1["StandardError"], "", TestID -> "Verify correctness of basic Julia function"]];

AppendTo[tests, VerificationTest[output1["StandardOutput"], "42", TestID -> "Verify return value of basic Julia function"]];

(**********************************************************************************
    Testing typical arithmetic operations in Julia functions    
**********************************************************************************)

expr = (Cos[a] + Sin[a] ^ 2 - Log[a] / Sqrt[a]) / (Exp[a] + Tan[a]);

funBody2 = MakeJuliaFunction[expr, "Name" -> "fun", "Body" -> "a = in", "Parameters" -> {"in"}];

code2 = funBody2 <> "
print(round(fun(1.5),digits=11))
"

execFile2 = Export[$TemporaryDirectory <> "/FunKitJuliaTest2.jl", code2, "Text"];

output2 = RunProcess[{"julia", execFile2}]

expected = ToString[NumberForm[expr /. a -> 1.5, 10]];

AppendTo[tests, VerificationTest[output2["StandardError"], "", TestID -> "Verify correctness of Julia function with arithmetic operations"]];

AppendTo[tests, VerificationTest[output2["StandardOutput"], expected, TestID -> "Verify return value of Julia function with arithmetic operations"]];

(**********************************************************************************
    Optimization pipeline test: CSE variables appear in output, result still correct
**********************************************************************************)

(* An expression with repeated subexpressions — the optimizer should hoist them into _cse* vars *)
ClearAll[a, b]
exprOpt = Sin[a + b]^2 + Cos[a + b]^3 + Sin[a + b]*Log[b + 1] + Cos[a + b]*Exp[b];

(* Verify the code contains at least one optimizer-generated variable *)
juliaCodeOpt = JuliaCode[exprOpt];
AppendTo[tests, VerificationTest[
    StringContainsQ[juliaCodeOpt, "_cse"] || StringContainsQ[juliaCodeOpt, "_interp"],
    True,
    TestID -> "Optimization pipeline produces CSE variables for Julia"
]];

(* Verify the optimized function computes the correct numerical result *)
funBodyOpt = MakeJuliaFunction[exprOpt, "Name" -> "fun", "Body" -> "a = in1\nb = in2", "Parameters" -> {"in1", "in2"}];

codeOpt = funBodyOpt <> "
print(round(fun(1.2, 0.7), digits=10))
";

execFileOpt = Export[$TemporaryDirectory <> "/FunKitJuliaTestOpt.jl", codeOpt, "Text"];
outputOpt = RunProcess[{"julia", execFileOpt}];

expectedOpt = ToString[NumberForm[N[exprOpt /. {a -> 1.2, b -> 0.7}, 10], 10]];

AppendTo[tests, VerificationTest[outputOpt["StandardError"], "", TestID -> "Optimization pipeline produces no Julia runtime errors"]];
AppendTo[tests, VerificationTest[outputOpt["StandardOutput"], expectedOpt, TestID -> "Optimized Julia function returns correct value"]];

(**********************************************************************************
    $codeOptimize = False: plain expression path produces just a return statement
**********************************************************************************)

(* A trivially simple expression cannot trigger CSE, so JuliaCode should produce
   just a return statement with no definition lines regardless of $codeOptimize.
   This verifies the "no definitions" code path works correctly. *)
ClearAll[x, y]
simpleCode = JuliaCode[x + y];

AppendTo[tests, VerificationTest[
    StringStartsQ[simpleCode, "return"],
    True,
    TestID -> "Simple expression produces plain return statement without definitions"
]];

AppendTo[tests, VerificationTest[
    !StringContainsQ[simpleCode, "_cse"] && !StringContainsQ[simpleCode, "_interp"],
    True,
    TestID -> "Simple expression produces no CSE variables"
]];
