tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    COEN: FSetRegisterSize catch-all (E10)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetRegisterSize["not a number"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E10: FSetRegisterSize with non-integer should abort"
]];

(* Test that valid integer works *)
AppendTo[tests, VerificationTest[
    (FunKit`FSetRegisterSize[64]; True),
    True,
    TestID -> "FSetRegisterSize with valid integer should succeed"
]];

(* Reset to default *)
AppendTo[tests, VerificationTest[
    (FunKit`FSetRegisterSize[32]; True),
    True,
    TestID -> "FSetRegisterSize reset to default should succeed"
]];

(**********************************************************************************
    COEN: FormatCppCode catch-all (E11)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FormatCppCode[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E11: FormatCppCode with non-string should abort"
]];

(**********************************************************************************
    COEN: MakeCppClass catch-all (E12)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`MakeCppClass[42, 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E12: MakeCppClass with positional args should abort"
]];

(**********************************************************************************
    COEN: MakeCppHeader catch-all (E13)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`MakeCppHeader[42, 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E13: MakeCppHeader with positional args should abort"
]];

(**********************************************************************************
    COEN: prepParam catch-all (F2)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`Private`prepParam[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`prepParam::invalid},
    TestID -> "F2: prepParam with integer should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`Private`prepParam[{1, 2}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`prepParam::invalid},
    TestID -> "F2: prepParam with list should abort"
]];

(* Test that valid inputs work *)
AppendTo[tests, VerificationTest[
    Head[FunKit`Private`prepParam["x"]],
    Association,
    TestID -> "prepParam with string should return Association"
]];

AppendTo[tests, VerificationTest[
    Head[FunKit`Private`prepParam[<|"Name" -> "x", "Type" -> "double"|>]],
    Association,
    TestID -> "prepParam with Association should return Association"
]];

(**********************************************************************************
    COEN: FSetCodeOptimization validation
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetCodeOptimization["not a boolean"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeOptimization with non-boolean should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetCodeOptimization[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodeOptimization with integer should abort"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetCodeOptimization[True]; True),
    True,
    TestID -> "FSetCodeOptimization with True should succeed"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetCodeOptimization[False]; True),
    True,
    TestID -> "FSetCodeOptimization with False should succeed"
]];

(* Reset to default *)
FunKit`FSetCodeOptimization[True];

(**********************************************************************************
    COEN: FSetFastMath validation
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetFastMath[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetFastMath with integer should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetFastMath["yes"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetFastMath with string should abort"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetFastMath[True]; True),
    True,
    TestID -> "FSetFastMath with True should succeed"
]];

FunKit`FSetFastMath[False];

(**********************************************************************************
    COEN: FSetMaxKernelTerms validation
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetMaxKernelTerms[-1], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetMaxKernelTerms with negative value should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetMaxKernelTerms["not a number"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetMaxKernelTerms with string should abort"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetMaxKernelTerms[200]; True),
    True,
    TestID -> "FSetMaxKernelTerms with valid integer should succeed"
]];

FunKit`FSetMaxKernelTerms[500];

(**********************************************************************************
    COEN: FSetCodePrecision validation
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetCodePrecision["invalid"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodePrecision with invalid string should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetCodePrecision[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "FSetCodePrecision with integer should abort"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetCodePrecision["single"]; True),
    True,
    TestID -> "FSetCodePrecision with single should succeed"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetCodePrecision["double"]; True),
    True,
    TestID -> "FSetCodePrecision with double should succeed"
]];

(**********************************************************************************
    COEN: CppForm basic test
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Head[FunKit`CppForm[x + y]],
    String,
    TestID -> "CppForm should return a string"
]];
