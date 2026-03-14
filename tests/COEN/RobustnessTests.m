tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    COEN: FSetRegisterSize catch-all (E10)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetRegisterSize["not a number"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E10: FSetRegisterSize with non-integer should abort"
]];

(* Test that valid integer works *)
AppendTo[tests, TestCreate[
    (FunKit`FSetRegisterSize[64]; True),
    True,
    TestID -> "FSetRegisterSize with valid integer should succeed"
]];

(* Reset to default *)
AppendTo[tests, TestCreate[
    (FunKit`FSetRegisterSize[32]; True),
    True,
    TestID -> "FSetRegisterSize reset to default should succeed"
]];

(**********************************************************************************
    COEN: FormatCppCode catch-all (E11)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FormatCppCode[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E11: FormatCppCode with non-string should abort"
]];

(**********************************************************************************
    COEN: MakeCppClass catch-all (E12)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`MakeCppClass[42, 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E12: MakeCppClass with positional args should abort"
]];

(**********************************************************************************
    COEN: MakeCppHeader catch-all (E13)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`MakeCppHeader[42, 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E13: MakeCppHeader with positional args should abort"
]];

(**********************************************************************************
    COEN: prepParam catch-all (F2)
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`Private`prepParam[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`prepParam::invalid},
    TestID -> "F2: prepParam with integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`Private`prepParam[{1, 2}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`prepParam::invalid},
    TestID -> "F2: prepParam with list should abort"
]];

(* Test that valid inputs work *)
AppendTo[tests, TestCreate[
    Head[FunKit`Private`prepParam["x"]],
    Association,
    TestID -> "prepParam with string should return Association"
]];

AppendTo[tests, TestCreate[
    Head[FunKit`Private`prepParam[<|"Name" -> "x", "Type" -> "double"|>]],
    Association,
    TestID -> "prepParam with Association should return Association"
]];

(**********************************************************************************
    COEN: FSetCodeOptimizationLevel validation
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetCodeOptimizationLevel["not a number"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeOptimizationLevel with non-integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetCodeOptimizationLevel[5], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeOptimizationLevel with out-of-range value should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetCodeOptimizationLevel[-1], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeOptimizationLevel with negative value should abort"
]];

AppendTo[tests, TestCreate[
    (FunKit`FSetCodeOptimizationLevel[0]; True),
    True,
    TestID -> "FSetCodeOptimizationLevel with 0 should succeed"
]];

AppendTo[tests, TestCreate[
    (FunKit`FSetCodeOptimizationLevel[1]; True),
    True,
    TestID -> "FSetCodeOptimizationLevel with 1 should succeed"
]];

AppendTo[tests, TestCreate[
    (FunKit`FSetCodeOptimizationLevel[2]; True),
    True,
    TestID -> "FSetCodeOptimizationLevel with 2 should succeed"
]];

(**********************************************************************************
    COEN: FSetCodeChunkSize validation
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetCodeChunkSize["not a number"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeChunkSize with non-integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetCodeChunkSize[-5], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeChunkSize with negative value should abort"
]];

AppendTo[tests, TestCreate[
    (FunKit`FSetCodeChunkSize[25]; True),
    True,
    TestID -> "FSetCodeChunkSize with valid integer should succeed"
]];

(* Reset to defaults *)
FunKit`FSetCodeOptimizationLevel[2];
FunKit`FSetCodeChunkSize[50];

(**********************************************************************************
    COEN: CppForm basic test
**********************************************************************************)

AppendTo[tests, TestCreate[
    Head[FunKit`CppForm[x + y]],
    String,
    TestID -> "CppForm should return a string"
]];
