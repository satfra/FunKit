tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    c>ag>g
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`GetOrder[yFunKitSetup, {Psi, Psibar}]
        ]
        ,
        {-1, {2, 1}}
        ,
        TestID -> "GetOrder {Psi, Psibar}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`GetOrder[yFunKitSetup, {Psibar, Psi}]
        ]
        ,
        {1, {1, 2}}
        ,
        TestID -> "GetOrder {Psibar, Psi}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`GetOrder[yFunKitSetup, {Psi, Psibar, Phi}]
        ]
        ,
        {-1, {3, 2, 1}}
        ,
        TestID -> "GetOrder {Psi, Psibar, Phi}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`GetOrder[yFunKitSetup, {Psi, Psibar, Psi, Psibar, Phi}]
        ]
        ,
        {-1, {5, 2, 4, 1, 3}}
        ,
        TestID -> "GetOrder {Psi, Psibar, Psi, Psibar, Phi}"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`OrderObject[yFunKitSetup, GammaN[{Psibar, Psi}, {i1, i2}]]
        ]
        ,
        GammaN[{Psibar, Psi}, {i1, i2}]
        ,
        TestID -> "OrderObject Gamma[{Psibar, Psi}, {i1, i2}]"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`OrderObject[yFunKitSetup, GammaN[{Psi, Psibar}, {i1, i2}]]
        ]
        ,
        -GammaN[{Psibar, Psi}, {i2, i1}]
        ,
        TestID -> "OrderObject Gamma[{Psi, Psibar}, {i1, i2}]"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{yFunKitSetup},
            FSetCanonicalOrdering["c>ag>g"];
            yFunKitSetup = GetFunKitSetupYukawa[];
            FunKit`Private`OrderObject[yFunKitSetup, GammaN[{Psi, Psibar, Psi, Psibar, Phi}, {i1, i2, i3, i4, i5}]]
        ]
        ,
        -GammaN[{Phi, Psibar, Psibar, Psi, Psi}, {i5, i4, i2, i3, i1}]
        ,
        TestID -> "OrderObject Gamma[{Psi, Psibar, Psi, Psibar, Phi}, {i1, i2, i3, i4, i5}]"
    ]
];
