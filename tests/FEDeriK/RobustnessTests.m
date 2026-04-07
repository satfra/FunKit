tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    A2: Global setup fallback tests
    When $GlobalSetup is not set, convenience wrappers should abort with a message.
**********************************************************************************)

(* Make sure $GlobalSetup is cleared *)

FunKit`FSetGlobalSetup[];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FTruncate[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FTruncate without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FTruncateOpenIndices[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FTruncateOpenIndices without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FTakeDerivatives[FEx[FTerm[1]], {Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FTakeDerivatives without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`QMeSForm[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: QMeSForm without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FExpand[FEx[FTerm[1]], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FExpand without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`DExpand[FEx[FTerm[1]], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: DExpand without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FMakeClassicalAction[], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FMakeClassicalAction without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FMakeDSE[Phi], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FMakeDSE without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FResolveDerivatives[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FResolveDerivatives without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FResolveFDOp[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FResolveFDOp without global setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FOrderFields[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FOrderFields without global setup should abort"
]];

(**********************************************************************************
    A1: Catch-all error definitions for wrong argument types
**********************************************************************************)

testSetup = GetFunKitSetupScalar[];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FTruncate[testSetup, "not an FEx"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FTruncate::wrongExpr},
    TestID -> "A1: FTruncate with wrong type should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FTruncateOpenIndices[testSetup, "not an FEx"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FTruncate::wrongExpr},
    TestID -> "A1: FTruncateOpenIndices with wrong type should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FResolveFDOp[testSetup, "not an FEx or FTerm"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "A1: FResolveFDOp with wrong type should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FExpand[testSetup, "not an FTerm", 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "A1: FExpand with wrong type should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`DExpand[testSetup, "not an FTerm", 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "A1: DExpand with wrong type should abort"
]];

(**********************************************************************************
    B1: FSetCanonicalOrdering should abort on invalid input
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`Private`FSetCanonicalOrdering[99], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`CanonicalOrdering::unknownInteger},
    TestID -> "B1: FSetCanonicalOrdering with invalid integer should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`Private`FSetCanonicalOrdering["invalid"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`CanonicalOrdering::unknownString},
    TestID -> "B1: FSetCanonicalOrdering with invalid string should abort"
]];

(* Verify valid orderings still work *)

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`Private`FSetCanonicalOrdering["c>ag>g"];
        FunKit`Private`$CanonicalOrdering
    ],
    "c>ag>g",
    TestID -> "B1: FSetCanonicalOrdering with valid string should succeed"
]];

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`Private`FSetCanonicalOrdering[1];
        FunKit`Private`$CanonicalOrdering
    ],
    "g>ag>c",
    TestID -> "B1: FSetCanonicalOrdering with valid integer should succeed"
]];

(**********************************************************************************
    C1-C4: AssertFSetup validation
    Passing a non-setup value should abort.
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FExpand["not a setup", FTerm[1], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C1: FExpand with invalid setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`DExpand["not a setup", FTerm[1], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C1: DExpand with invalid setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FResolveFDOp["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C2: FResolveFDOp with invalid setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FOrderFields["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C3: FOrderFields with invalid setup should abort"
]];

(* Restore canonical ordering for subsequent tests *)

FunKit`Private`FSetCanonicalOrdering["c>ag>g"];

(**********************************************************************************
    A3: FSetUnorderedIndices catch-all
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetUnorderedIndices[Propagator, -1], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetUnorderedIndices::invalidArgs},
    TestID -> "A3: FSetUnorderedIndices with negative n should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetUnorderedIndices[FooBarNotRegistered, 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetUnorderedIndices::invalidArgs},
    TestID -> "A3: FSetUnorderedIndices with unregistered object should abort"
]];

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`FSetUnorderedIndices[Propagator, 1];
        FunKit`Private`$unorderedIndices[Propagator]
    ],
    1,
    TestID -> "A3: FSetUnorderedIndices with valid args should succeed"
]];

(**********************************************************************************
    A4: FSetSymmetricObject empty fields catch-all
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetSymmetricObject[GammaN, {}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetSymmetricObject::emptyFields},
    TestID -> "A4: FSetSymmetricObject with empty fields should abort"
]];

(**********************************************************************************
    A5: FAddObject/FAddIndexedObject/FAddOrderedObject/FAddCorrelationFunction
    catch-alls for non-Symbol arguments
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddObject["NotASymbol"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddObject::notSymbol},
    TestID -> "A5: FAddObject with string should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddIndexedObject["NotASymbol"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddIndexedObject::notSymbol},
    TestID -> "A5: FAddIndexedObject with string should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddOrderedObject[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddOrderedObject::notSymbol},
    TestID -> "A5: FAddOrderedObject with integer should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddCorrelationFunction["NotASymbol"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddCorrelationFunction::notSymbol},
    TestID -> "A5: FAddCorrelationFunction with string should abort"
]];

(**********************************************************************************
    A6: FSetAutoSimplify/FSetAutoBuildSymmetryList Boolean validation
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetAutoSimplify[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetAutoSimplify::notBoolean},
    TestID -> "A6: FSetAutoSimplify with non-Boolean should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetAutoBuildSymmetryList["yes"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetAutoBuildSymmetryList::notBoolean},
    TestID -> "A6: FSetAutoBuildSymmetryList with non-Boolean should abort"
]];

(* Verify valid usage still works *)

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`FSetAutoSimplify[False];
        FunKit`Private`$AutoSimplify
    ],
    False,
    TestID -> "A6: FSetAutoSimplify with False should succeed"
]];

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`FSetAutoSimplify[];
        FunKit`Private`$AutoSimplify
    ],
    True,
    TestID -> "A6: FSetAutoSimplify with no args should default to True"
]];

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`FSetAutoBuildSymmetryList[False];
        FunKit`Private`$AutoBuildSymmetryList
    ],
    False,
    TestID -> "A6: FSetAutoBuildSymmetryList with False should succeed"
]];

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`FSetAutoBuildSymmetryList[];
        FunKit`Private`$AutoBuildSymmetryList
    ],
    True,
    TestID -> "A6: FSetAutoBuildSymmetryList with no args should default to True"
]];

(**********************************************************************************
    D1: FTruncate/FTruncateOpenIndices FTerm convenience overloads
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`FTruncate[testSetup, FTerm[GammaN[{Phi, Phi}, {i1, i2}]]];
        Head[result]
    ],
    FEx,
    TestID -> "D1: FTruncate with FTerm should return FEx"
]];

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`FTruncateOpenIndices[testSetup, FTerm[GammaN[{Phi, Phi}, {i1, i2}]]];
        Head[result]
    ],
    FEx,
    TestID -> "D1: FTruncateOpenIndices with FTerm should return FEx"
]];

(**********************************************************************************
    D2: NormalizeSuperIndices FEx overload
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`Private`NormalizeSuperIndices[testSetup, FEx[FTerm[GammaN[{Phi, Phi}, {i1, i2}]]]];
        Head[result]
    ],
    FEx,
    TestID -> "D2: NormalizeSuperIndices with FEx should return FEx"
]];

(**********************************************************************************
    D3: FExpand/DExpand missing order catch-all
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FExpand[testSetup, FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FExpand::missingOrder},
    TestID -> "D3: FExpand without order should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`DExpand[testSetup, FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`DExpand::missingOrder},
    TestID -> "D3: DExpand without order should abort"
]];

(**********************************************************************************
    G1: FieldOrderLess crash on unrecognized fields
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`Private`FieldOrderLess[testSetup, UnknownFieldXYZ, Phi], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FieldOrderLess::unknownField},
    TestID -> "G1: FieldOrderLess with unrecognized first field should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`Private`FieldOrderLess[testSetup, Phi, UnknownFieldXYZ2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FieldOrderLess::unknownField},
    TestID -> "G1: FieldOrderLess with unrecognized second field should abort"
]];

(* Positive test: FieldOrderLess with valid fields should work *)

AppendTo[tests, VerificationTest[
    BooleanQ[FunKit`Private`FieldOrderLess[testSetup, Phi, Phi]],
    True,
    TestID -> "G1: FieldOrderLess with valid fields should return Boolean"
]];

(**********************************************************************************
    G3: FMakeDSE guarded FSimplify call
    We verify FMakeClassicalAction works (positive test) to confirm the
    setup is valid. The G3 code change ensures FMakeDSE only calls FSimplify
    when AnSEL is loaded and $AutoSimplify is True, matching the pattern
    used in Truncation.m and Derivatives.m.
**********************************************************************************)

testSetupDSE = Module[{p},
    <|"FieldSpace" -> <|"Commuting" -> {Phi[p]}, "Grassmann" -> {}|>,
      "Truncation" -> <|S -> {{Phi, Phi}, {Phi, Phi, Phi, Phi}}, Rdot -> {{Phi, Phi}}, Propagator -> {{Phi, Phi}}, GammaN -> {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}|>|>
];

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`FMakeClassicalAction[testSetupDSE];
        Head[result]
    ],
    FEx,
    TestID -> "G3: FMakeClassicalAction should return FEx"
]];

(**********************************************************************************
    G4: GetPartnerField positive test
**********************************************************************************)

testSetupYukawa = GetFunKitSetupYukawa[];

AppendTo[tests, VerificationTest[
    FunKit`Private`GetPartnerField[testSetupYukawa, Psi],
    Psibar,
    TestID -> "G4: GetPartnerField should find partner of Psi"
]];

AppendTo[tests, VerificationTest[
    FunKit`Private`GetPartnerField[testSetupYukawa, Psibar],
    Psi,
    TestID -> "G4: GetPartnerField should find partner of Psibar"
]];

(* For a field without a partner, GetPartnerField should return the field itself *)

AppendTo[tests, VerificationTest[
    FunKit`Private`GetPartnerField[testSetupYukawa, Phi],
    Phi,
    TestID -> "G4: GetPartnerField for field without partner should return same field"
]];

(**********************************************************************************
    F2: Empty truncation warning in FTruncate
    When setup["Truncation"] is <||>, a warning message should fire
    but truncation should still proceed (no abort).
**********************************************************************************)

f2Setup = Module[{p},
    <|"FieldSpace" -> <|"Commuting" -> {Phi[p]}, "Grassmann" -> {}|>,
      "Truncation" -> <||>|>
];

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`FTruncate[f2Setup, FEx[FTerm[1]]];
        Head[result]
    ],
    FEx,
    {FunKit`FTruncate::emptyTruncation},
    TestID -> "F2: FTruncate with empty truncation should warn but not abort"
]];

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`FTruncateOpenIndices[f2Setup, FEx[FTerm[1]]];
        Head[result]
    ],
    FEx,
    {FunKit`FTruncate::emptyTruncation},
    TestID -> "F2: FTruncateOpenIndices with empty truncation should warn but not abort"
]];

(**********************************************************************************
    C5: FAddFDRule catch-all for wrong arity
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddFDRule[obj, wrt], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddFDRule::invalidArgs},
    TestID -> "C5: FAddFDRule with 2 args should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddFDRule[obj], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddFDRule::invalidArgs},
    TestID -> "C5: FAddFDRule with 1 arg should abort"
]];

(* Positive test: 3 args should work *)

AppendTo[tests, VerificationTest[
    Module[{},
        FunKit`FClearFDRules[];
        FunKit`FAddFDRule[myObj, myWrt, myRes];
        Length[FunKit`Private`$userRules]
    ],
    1,
    TestID -> "C5: FAddFDRule with 3 args should succeed"
]];

(* Clean up *)

FunKit`FClearFDRules[];

(**********************************************************************************
    C6: FExpand/DExpand negative order check
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FExpand[testSetup, FTerm[1], -1], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FExpand::negativeOrder},
    TestID -> "C6: FExpand with negative order should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`DExpand[testSetup, FTerm[1], -2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`DExpand::negativeOrder},
    TestID -> "C6: DExpand with negative order should abort"
]];

(* Positive test: order 0 should work *)

AppendTo[tests, VerificationTest[
    Module[{result},
        result = FunKit`FExpand[testSetup, FTerm[1], 0];
        Head[result]
    ],
    FTerm,
    TestID -> "C6: FExpand with order 0 should succeed"
]];
