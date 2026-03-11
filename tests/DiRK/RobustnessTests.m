tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    DiRK: Global setup fallback tests (C3, C4)
**********************************************************************************)

FunKit`FSetGlobalSetup[];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeDiagrammaticRules[], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FMakeDiagrammaticRules without global setup should abort"
]];

(**********************************************************************************
    DiRK: AssertFSetup tests (D2)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeDiagrammaticRules[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D2: FMakeDiagrammaticRules with non-Association setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeDiagrammaticRules["not a setup"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D2: FMakeDiagrammaticRules with string setup should abort"
]];

(**********************************************************************************
    DiRK: Missing FeynmanRules key (D3)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[
        FunKit`FMakeDiagrammaticRules[
            <|"FieldSpace" -> <|"Commuting" -> {Phi[p]}, "Grassmann" -> {}|>|>
        ],
        "AbortTriggered"
    ],
    "AbortTriggered",
    {FunKit`FMakeDiagrammaticRules::noFeynmanRules},
    TestID -> "D3: FMakeDiagrammaticRules without FeynmanRules key should abort"
]];

(**********************************************************************************
    DiRK: Catch-all tests (E3, E4)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetSymmetricDressing[42, 42, 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E3: FSetSymmetricDressing with wrong args should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeDiagrammaticRules[42, 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E4: FMakeDiagrammaticRules with two non-setup args should abort"
]];
