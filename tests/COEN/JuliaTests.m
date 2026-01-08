tests = {};

(**********************************************************************************
    Setup
**********************************************************************************)

hasJulia = Run["command -v julia &> /dev/null"] == 0;

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

AppendTo[tests, TestCreate[output1["StandardError"], "", TestID -> "Verify correctness of basic Julia function"]];

AppendTo[tests, TestCreate[output1["StandardOutput"], "42", TestID -> "Verify return value of basic Julia function"]];

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

AppendTo[tests, TestCreate[output2["StandardError"], "", TestID -> "Verify correctness of Julia function with arithmetic operations"]];

AppendTo[tests, TestCreate[output2["StandardOutput"], expected, TestID -> "Verify return value of Julia function with arithmetic operations"]];
