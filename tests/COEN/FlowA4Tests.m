tests = {};

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
    Load the FlowA4 expression
**********************************************************************************)

flowA4 = Import[$FunKitDirectory <> "/tests/boilerplate/FlowA4.m"];

(**********************************************************************************
    Test: Pipeline completes for all FlowA4 elements at each opt level
**********************************************************************************)

Do[
    Do[
        Block[{FunKit`Private`$codeOptimizationLevel = lvl},
            {timing, body} = AbsoluteTiming[
                MakeCppFunction[flowA4[[elem]],
                    "Name" -> "flow_" <> ToString[elem] <> "_lvl" <> ToString[lvl],
                    "Body" -> "",
                    "Parameters" -> {"l1", "p", "k", "cosl1p1", "cosl1p2", "cosl1p3"}
                ]
            ];
            AppendTo[tests, TestCreate[
                StringQ[body],
                True,
                TestID -> "FlowA4[" <> ToString[elem] <> "] opt level " <> ToString[lvl] <> " generates valid code (took " <> ToString[NumberForm[timing, 3]] <> "s)"
            ]];
        ];
        ,
        {lvl, {0, 1, 2}}
    ];
    ,
    {elem, 1, Length[flowA4]}
];

(**********************************************************************************
    Test: FlowA4[1] compiles and produces correct numerical output
**********************************************************************************)

(* Define stub functions for the interpolators *)
stubDefs = "
#include <cmath>
using namespace std;
using NumberType = double;

auto ZAcbc(auto x) { return 1.0 + 0.1*x; }
auto ZA(auto x) { return 1.0 + 0.05*x; }
auto ZA3(auto x) { return 1.0 + 0.02*x; }
auto ZA4(auto x) { return 1.0 + 0.03*x; }
auto Zc(auto x) { return 1.0 + 0.01*x; }
auto dtZc(auto x) { return 0.01; }
auto dtZA(auto x) { return 0.02; }
auto RB(auto k2, auto p2) { return k2 / (1.0 + p2/k2); }
auto RBdot(auto k2, auto p2) { return 2.0 * RB(k2, p2) / k2; }
auto RF(auto k2, auto p2) { return sqrt(k2 * p2) / (1.0 + p2/k2); }
auto RFdot(auto k2, auto p2) { return 2.0 * RF(k2, p2) / k2; }
auto dq2RB(auto k2, auto p2) { return -k2 / pow(1.0 + p2/k2, 2) / k2; }
auto dq2RF(auto k2, auto p2) { return 0.5 * sqrt(k2/p2) / (1.0 + p2/k2) - sqrt(k2*p2) / pow(1.0 + p2/k2, 2) / k2; }
";

funBodyCompile = MakeCppFunction[flowA4[[1]],
    "Name" -> "flowA4_1",
    "Body" -> "",
    "Parameters" -> {"l1", "p", "k", "cosl1p1", "cosl1p2", "cosl1p3"}
];

execCompile = CreateExecutable["
#include <iostream>
#include <iomanip>
" <> stubDefs <> "
" <> powrCode <> "
" <> funBodyCompile <> "

int main () {
  std::cout << std::setprecision (15) << flowA4_1 (2.5, 1.0, 3.0, 0.2, 0.3, 0.4) << std::endl;
}
", "FunKitFlowA4Test", "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];

AppendTo[tests, TestCreate[execCompile =!= $Failed, True, TestID -> "FlowA4[1] compiles to executable"]];

If[execCompile =!= $Failed,
    outputCompile = Import["!" <> QuoteFile[execCompile], "Text"];
    numericOutputCompile = Quiet @ Read[StringToStream[outputCompile], Number];
    AppendTo[tests, TestCreate[
        NumberQ[numericOutputCompile],
        True,
        TestID -> "FlowA4[1] produces numeric output"
    ]];
];

(**********************************************************************************
    Test: All opt levels produce same numerical result for FlowA4[1]
**********************************************************************************)

Do[
    Block[{FunKit`Private`$codeOptimizationLevel = lvl},
        funBodyLvl = MakeCppFunction[flowA4[[1]],
            "Name" -> "flowA4_lvl" <> ToString[lvl],
            "Body" -> "",
            "Parameters" -> {"l1", "p", "k", "cosl1p1", "cosl1p2", "cosl1p3"}
        ];
    ];
    execLvl = CreateExecutable["
#include <iostream>
#include <iomanip>
" <> stubDefs <> "
" <> powrCode <> "
" <> funBodyLvl <> "

int main () {
  std::cout << std::setprecision (12) << flowA4_lvl" <> ToString[lvl] <> " (2.5, 1.0, 3.0, 0.2, 0.3, 0.4) << std::endl;
}
", "FunKitFlowA4Lvl" <> ToString[lvl], "CompilerName" -> CppCompiler, "SystemCompileOptions" -> "-std=c++20"];
    If[execLvl =!= $Failed,
        outputLvl = Import["!" <> QuoteFile[execLvl], "Text"];
        numericOutputLvl = Quiet @ Read[StringToStream[outputLvl], Number];
        AppendTo[tests, TestCreate[
            NumberQ[numericOutputLvl] && Abs[numericOutputLvl - numericOutputCompile] < 1*^-8,
            True,
            TestID -> "FlowA4[1] opt level " <> ToString[lvl] <> " matches default output"
        ]];
    ];
    ,
    {lvl, {0, 1}}
];
