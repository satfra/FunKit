tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    AnSEL: Global setup fallback tests (C4)
    When $GlobalSetup is not set, convenience wrappers should abort.
**********************************************************************************)

FunKit`FSetGlobalSetup[];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FRoute[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FRoute FEx without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FRoute[FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FRoute FTerm without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FUnroute[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FUnroute FEx without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FUnroute[FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FUnroute FTerm without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSimplify[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FSimplify FEx without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSimplify[FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FSimplify FTerm without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeSymmetryList[{Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "C4: FMakeSymmetryList without global setup should abort"
]];

(**********************************************************************************
    AnSEL: Catch-all tests (E1, E2)
**********************************************************************************)

testSetup = GetFunKitSetupScalar[];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FRoute[testSetup, "not valid"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E1: FRoute with wrong expr type should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FRoute[42, FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D1: FRoute with non-Association setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FUnroute[testSetup, "not valid"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E2: FUnroute with wrong expr type should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FUnroute[42, FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D1: FUnroute with non-Association setup should abort"
]];

(**********************************************************************************
    AnSEL: AssertFSetup tests (D1)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSimplify[42, FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D1: FSimplify with non-Association setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSimplify["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D1: FSimplify with string setup should abort"
]];

(**********************************************************************************
    AnSEL: FMakeSymmetryList validation (B1, C1, F1)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeSymmetryList[testSetup, "not a list"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "C1: FMakeSymmetryList with non-list fields should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeSymmetryList[42, {Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D1: FMakeSymmetryList with non-Association setup should abort"
]];

(**********************************************************************************
    AnSEL: FSetLoopMomentumName catch-all
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetLoopMomentumName[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetLoopMomentumName with non-string should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetLoopMomentumName[], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetLoopMomentumName with no args should abort"
]];
