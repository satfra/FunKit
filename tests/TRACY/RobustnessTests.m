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
    TRACY: IterativelySum catch-all (E9)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`IterativelySum["not a list"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E9: IterativelySum with non-list should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`IterativelySum[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E9: IterativelySum with integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`IterativelySum[{1, 2}, "not an integer"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E9: IterativelySum with non-integer finalSize should abort"
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
    TRACY: ClearTraceCache path sanitization (H7)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`ClearTraceCache["../../etc"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`ClearTraceCache::invalidPath},
    TestID -> "H7: ClearTraceCache with path traversal should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`ClearTraceCache["/absolute/path"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`ClearTraceCache::invalidPath},
    TestID -> "H7: ClearTraceCache with absolute path should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`ClearTraceCache["foo/../bar"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`ClearTraceCache::invalidPath},
    TestID -> "H7: ClearTraceCache with embedded .. should abort"
]];

(**********************************************************************************
    TRACY: FormMomentumExpansion stub (H1)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FormMomentumExpansion[q1, q2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FormMomentumExpansion::notImplemented},
    TestID -> "H1: FormMomentumExpansion with args should abort (not implemented)"
]];

(* Zero-arg call should succeed (initialization) *)
AppendTo[tests, TestCreate[
    (FunKit`FormMomentumExpansion[]; True),
    True,
    TestID -> "H1: FormMomentumExpansion[] should succeed as initialization"
]];
