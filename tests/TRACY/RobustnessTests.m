tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    TRACY: FSetAlwaysExpandLorentzTensors catch-all (E7)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetAlwaysExpandLorentzTensors["not a bool"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E7: FSetAlwaysExpandLorentzTensors with non-boolean should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetAlwaysExpandLorentzTensors[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E7: FSetAlwaysExpandLorentzTensors with integer should abort"
]];

(* Test that valid booleans work *)
AppendTo[tests, TestCreate[
    (FunKit`FSetAlwaysExpandLorentzTensors[True]; True),
    True,
    TestID -> "FSetAlwaysExpandLorentzTensors with True should succeed"
]];

AppendTo[tests, TestCreate[
    (FunKit`FSetAlwaysExpandLorentzTensors[False]; True),
    True,
    TestID -> "FSetAlwaysExpandLorentzTensors with False should succeed"
]];

(**********************************************************************************
    TRACY: FIterativelySum catch-all (E9)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FIterativelySum["not a list"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E9: FIterativelySum with non-list should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FIterativelySum[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E9: FIterativelySum with integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FIterativelySum[{1, 2}, "not an integer"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E9: FIterativelySum with non-integer finalSize should abort"
]];

(**********************************************************************************
    TRACY: FSetCacheDirectory catch-all (E8)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetCacheDirectory[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E8: FSetCacheDirectory with non-string should abort"
]];

(* Test that default reset works *)
AppendTo[tests, TestCreate[
    (FunKit`FSetCacheDirectory[]; True),
    True,
    TestID -> "FSetCacheDirectory[] should reset without error"
]];

(**********************************************************************************
    TRACY: FClearTraceCache path sanitization (H7)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FClearTraceCache["../../etc"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FClearTraceCache::invalidPath},
    TestID -> "H7: FClearTraceCache with path traversal should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FClearTraceCache["/absolute/path"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FClearTraceCache::invalidPath},
    TestID -> "H7: FClearTraceCache with absolute path should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FClearTraceCache["foo/../bar"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FClearTraceCache::invalidPath},
    TestID -> "H7: FClearTraceCache with embedded .. should abort"
]];

(**********************************************************************************
    TRACY: FMakeFormMomentumExpansion stub (H1)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeFormMomentumExpansion[q1, q2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FMakeFormMomentumExpansion::notImplemented},
    TestID -> "H1: FMakeFormMomentumExpansion with args should abort (not implemented)"
]];

(* Zero-arg call should succeed (initialization) *)
AppendTo[tests, TestCreate[
    (FunKit`FMakeFormMomentumExpansion[]; True),
    True,
    TestID -> "H1: FMakeFormMomentumExpansion[] should succeed as initialization"
]];
