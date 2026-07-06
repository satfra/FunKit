tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    FMinus Tests
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[FMinus[{Phi, Psi}, {i1, i2}]]]
        ]
        ,
        FTerm[]
        ,
        TestID -> "FMinus basic test 1"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[FMinus[{Phi, Phi}, {i1, i2}]]]
        ]
        ,
        FTerm[] (* = 1*)
        ,
        TestID -> "FMinus basic test 2"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[FMinus[{Psi, Psibar}, {i1, i2}]]]
        ]
        ,
        FTerm[-1]
        ,
        TestID -> "FMinus basic test 3"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[FMinus[{Psi, Psi}, {i1, i2}]]]
        ]
        ,
        FTerm[-1]
        ,
        TestID -> "FMinus basic test 4"
    ]
];

(**********************************************************************************
    Metric Tests

    Convention: \[Gamma]^{Psibar Psi} = \[Gamma]_{Psibar Psi} = +1 (antifield first),
    cf. GrassOrder in FEDeriK/Metric.m.
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[AnyField[-i1], \[Gamma][{Psi, Psi}, {i1, i2}], AnyField[-i2]]]
        ]
        ,
        FTerm[0]
        ,
        TestID -> "Metric test {Psi, Psi}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[AnyField[-i1], \[Gamma][{Psi, Psibar}, {i1, i2}], AnyField[-i2]]]
        ]
        ,
        FTerm[-1, AnyField[i2], AnyField[-i2]]
        ,
        TestID -> "Metric test {Psi, Psibar}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[AnyField[-i1], \[Gamma][{Psibar, Psibar}, {i1, i2}], AnyField[-i2]]]
        ]
        ,
        FTerm[0]
        ,
        TestID -> "Metric test {Psibar, Psibar}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{ySet = GetFunKitSetupYukawa[]},
            FunKit`Private`ReduceIndices[ySet, FTerm[AnyField[-i1], \[Gamma][{Psibar, Psi}, {i1, i2}], AnyField[-i2]]]
        ]
        ,
        FTerm[AnyField[i2], AnyField[-i2]]
        ,
        TestID -> "Metric test {Psibar, Psi}"
    ]
];
