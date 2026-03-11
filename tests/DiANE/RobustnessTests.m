tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

testSetup = GetFunKitSetupScalar[];

(**********************************************************************************
    DiANE: AssertFSetup tests (D4)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FPlot[42, FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D4: FPlot FTerm with non-Association setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FPlot["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D4: FPlot FEx with non-Association setup should abort"
]];

(**********************************************************************************
    DiANE: FAddTexStyles / FSetTexStyles validation (H6, E5, E6)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddTexStyles[Phi -> 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddTexStyles::invalidRule},
    TestID -> "FAddTexStyles with non-string value should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetTexStyles[Phi -> 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetTexStyles::invalidRule},
    TestID -> "H6: FSetTexStyles with non-string value should abort (using correct symbol)"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddTexStyles[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E5: FAddTexStyles with non-rule arg should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetTexStyles[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E6: FSetTexStyles with non-rule arg should abort"
]];

(* Test that FSetTexStyles[] (no args) works without error *)
AppendTo[tests, TestCreate[
    FunKit`FSetTexStyles[],
    Null,
    TestID -> "FSetTexStyles[] should clear styles without error"
]];

(* Test that valid rules work *)
AppendTo[tests, TestCreate[
    (FunKit`FAddTexStyles[Phi -> "\\phi"]; True),
    True,
    TestID -> "FAddTexStyles with valid rule should succeed"
]];

AppendTo[tests, TestCreate[
    (FunKit`FSetTexStyles[Phi -> "\\phi"]; True),
    True,
    TestID -> "FSetTexStyles with valid rule should succeed"
]];
