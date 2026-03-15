tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    A2: Global setup fallback tests
    When $GlobalSetup is not set, convenience wrappers should abort with a message.
**********************************************************************************)

(* Make sure $GlobalSetup is cleared *)

FunKit`FSetGlobalSetup[];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FTruncate[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FTruncate without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FTruncateOpenIndices[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FTruncateOpenIndices without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FTakeDerivatives[FEx[FTerm[1]], {Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FTakeDerivatives without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`QMeSForm[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: QMeSForm without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FExpand[FEx[FTerm[1]], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FExpand without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`DExpand[FEx[FTerm[1]], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: DExpand without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`MakeClassicalAction[], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: MakeClassicalAction without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FMakeDSE[Phi], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FMakeDSE without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FResolveDerivatives[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FResolveDerivatives without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FResolveFDOp[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FResolveFDOp without global setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FOrderFields[FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::noGlobalSetup},
    TestID -> "A2: FOrderFields without global setup should abort"
]];

(**********************************************************************************
    A1: Catch-all error definitions for wrong argument types
**********************************************************************************)

testSetup = GetFunKitSetupScalar[];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FTruncate[testSetup, "not an FEx"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FTruncate::wrongExpr},
    TestID -> "A1: FTruncate with wrong type should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FTruncateOpenIndices[testSetup, "not an FEx"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FTruncate::wrongExpr},
    TestID -> "A1: FTruncateOpenIndices with wrong type should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FResolveFDOp[testSetup, "not an FEx or FTerm"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "A1: FResolveFDOp with wrong type should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FExpand[testSetup, "not an FTerm", 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "A1: FExpand with wrong type should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`DExpand[testSetup, "not an FTerm", 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "A1: DExpand with wrong type should abort"
]];

(**********************************************************************************
    B1: FSetCanonicalOrdering should abort on invalid input
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`Private`FSetCanonicalOrdering[99], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`CanonicalOrdering::unknownInteger},
    TestID -> "B1: FSetCanonicalOrdering with invalid integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`Private`FSetCanonicalOrdering["invalid"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`CanonicalOrdering::unknownString},
    TestID -> "B1: FSetCanonicalOrdering with invalid string should abort"
]];

(* Verify valid orderings still work *)

AppendTo[tests, TestCreate[
    Module[{},
        FunKit`Private`FSetCanonicalOrdering["c>ag>g"];
        FunKit`Private`$CanonicalOrdering
    ],
    "c>ag>g",
    TestID -> "B1: FSetCanonicalOrdering with valid string should succeed"
]];

AppendTo[tests, TestCreate[
    Module[{},
        FunKit`Private`FSetCanonicalOrdering[1];
        FunKit`Private`$CanonicalOrdering
    ],
    "g>ag>c",
    TestID -> "B1: FSetCanonicalOrdering with valid integer should succeed"
]];

(**********************************************************************************
    D4: DoFunForm stub should abort with not-implemented message
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`DoFunForm[testSetup, FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`DoFunForm::notImplemented},
    TestID -> "D4: DoFunForm should abort as not implemented"
]];

(**********************************************************************************
    C1-C4: AssertFSetup validation
    Passing a non-setup value should abort.
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FExpand["not a setup", FTerm[1], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C1: FExpand with invalid setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`DExpand["not a setup", FTerm[1], 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C1: DExpand with invalid setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FResolveFDOp["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C2: FResolveFDOp with invalid setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FOrderFields["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "C3: FOrderFields with invalid setup should abort"
]];

(* Restore canonical ordering for subsequent tests *)

FunKit`Private`FSetCanonicalOrdering["c>ag>g"];

(**********************************************************************************
    F1: SeDecA AddVertex/AddVertexBasis object validation
    Passing an unregistered object should abort with a message.
**********************************************************************************)

(* Create a minimal setup with a field for the AddVertex tests *)

f1Setup = FunKit`MakeSetup[];
FunKit`AddCField[f1Setup, Phi[p]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddVertex[f1Setup, Propogator, {Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::unknownObject},
    TestID -> "F1: AddVertex with misspelled object should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddVertex[f1Setup, FooBar, {Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::unknownObject},
    TestID -> "F1: AddVertex with unknown object should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddVertexBasis[f1Setup, Propogator, {Phi} -> "someBasis"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::unknownObject},
    TestID -> "F1: AddVertexBasis with misspelled object should abort"
]];

(* Verify that valid objects still work *)

AppendTo[tests, TestCreate[
    Module[{s},
        s = FunKit`MakeSetup[];
        FunKit`AddCField[s, Phi[p]];
        FunKit`AddVertex[s, Propagator, {Phi, Phi}];
        KeyExistsQ[s["Truncation"], Propagator]
    ],
    True,
    TestID -> "F1: AddVertex with valid object should succeed"
]];

AppendTo[tests, TestCreate[
    Module[{s},
        s = FunKit`MakeSetup[];
        FunKit`AddCField[s, Phi[p]];
        FunKit`AddVertex[s, GammaN, {Phi, Phi, Phi}];
        KeyExistsQ[s["Truncation"], GammaN]
    ],
    True,
    TestID -> "F1: AddVertex with GammaN should succeed"
]];

(**********************************************************************************
    A3: FSetUnorderedIndices catch-all
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetUnorderedIndices[Propagator, -1], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetUnorderedIndices::invalidArgs},
    TestID -> "A3: FSetUnorderedIndices with negative n should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetUnorderedIndices[FooBarNotRegistered, 2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetUnorderedIndices::invalidArgs},
    TestID -> "A3: FSetUnorderedIndices with unregistered object should abort"
]];

AppendTo[tests, TestCreate[
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

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetSymmetricObject[GammaN, {}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetSymmetricObject::emptyFields},
    TestID -> "A4: FSetSymmetricObject with empty fields should abort"
]];

(**********************************************************************************
    A5: FAddObject/FAddIndexedObject/FAddOrderedObject/FAddCorrelationFunction
    catch-alls for non-Symbol arguments
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddObject["NotASymbol"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddObject::notSymbol},
    TestID -> "A5: FAddObject with string should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddIndexedObject["NotASymbol"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddIndexedObject::notSymbol},
    TestID -> "A5: FAddIndexedObject with string should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddOrderedObject[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddOrderedObject::notSymbol},
    TestID -> "A5: FAddOrderedObject with integer should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddCorrelationFunction["NotASymbol"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddCorrelationFunction::notSymbol},
    TestID -> "A5: FAddCorrelationFunction with string should abort"
]];

(**********************************************************************************
    A6: FSetAutoSimplify/FSetAutoBuildSymmetryList Boolean validation
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetAutoSimplify[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetAutoSimplify::notBoolean},
    TestID -> "A6: FSetAutoSimplify with non-Boolean should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FSetAutoBuildSymmetryList["yes"], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetAutoBuildSymmetryList::notBoolean},
    TestID -> "A6: FSetAutoBuildSymmetryList with non-Boolean should abort"
]];

(* Verify valid usage still works *)

AppendTo[tests, TestCreate[
    Module[{},
        FunKit`FSetAutoSimplify[False];
        FunKit`Private`$AutoSimplify
    ],
    False,
    TestID -> "A6: FSetAutoSimplify with False should succeed"
]];

AppendTo[tests, TestCreate[
    Module[{},
        FunKit`FSetAutoSimplify[];
        FunKit`Private`$AutoSimplify
    ],
    True,
    TestID -> "A6: FSetAutoSimplify with no args should default to True"
]];

AppendTo[tests, TestCreate[
    Module[{},
        FunKit`FSetAutoBuildSymmetryList[False];
        FunKit`Private`$AutoBuildSymmetryList
    ],
    False,
    TestID -> "A6: FSetAutoBuildSymmetryList with False should succeed"
]];

AppendTo[tests, TestCreate[
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

AppendTo[tests, TestCreate[
    Module[{result},
        result = FunKit`FTruncate[testSetup, FTerm[GammaN[{Phi, Phi}, {i1, i2}]]];
        Head[result]
    ],
    FEx,
    TestID -> "D1: FTruncate with FTerm should return FEx"
]];

AppendTo[tests, TestCreate[
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

AppendTo[tests, TestCreate[
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

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FExpand[testSetup, FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FExpand::missingOrder},
    TestID -> "D3: FExpand without order should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`DExpand[testSetup, FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`DExpand::missingOrder},
    TestID -> "D3: DExpand without order should abort"
]];

(**********************************************************************************
    G1: FieldOrderLess crash on unrecognized fields
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`Private`FieldOrderLess[testSetup, UnknownFieldXYZ, Phi], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FieldOrderLess::unknownField},
    TestID -> "G1: FieldOrderLess with unrecognized first field should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`Private`FieldOrderLess[testSetup, Phi, UnknownFieldXYZ2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FieldOrderLess::unknownField},
    TestID -> "G1: FieldOrderLess with unrecognized second field should abort"
]];

(* Positive test: FieldOrderLess with valid fields should work *)

AppendTo[tests, TestCreate[
    BooleanQ[FunKit`Private`FieldOrderLess[testSetup, Phi, Phi]],
    True,
    TestID -> "G1: FieldOrderLess with valid fields should return Boolean"
]];

(**********************************************************************************
    G3: FMakeDSE guarded FSimplify call
    We verify MakeClassicalAction works (positive test) to confirm the
    setup is valid. The G3 code change ensures FMakeDSE only calls FSimplify
    when AnSEL is loaded and $AutoSimplify is True, matching the pattern
    used in Truncation.m and Derivatives.m.
**********************************************************************************)

testSetupDSE = Module[{p},
    <|"FieldSpace" -> <|"Commuting" -> {Phi[p]}, "Grassmann" -> {}|>,
      "Truncation" -> <|S -> {{Phi, Phi}, {Phi, Phi, Phi, Phi}}, Rdot -> {{Phi, Phi}}, Propagator -> {{Phi, Phi}}, GammaN -> {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}|>|>
];

AppendTo[tests, TestCreate[
    Module[{result},
        result = FunKit`MakeClassicalAction[testSetupDSE];
        Head[result]
    ],
    FEx,
    TestID -> "G3: MakeClassicalAction should return FEx"
]];

(**********************************************************************************
    G4: GetPartnerField positive test
**********************************************************************************)

testSetupYukawa = GetFunKitSetupYukawa[];

AppendTo[tests, TestCreate[
    FunKit`Private`GetPartnerField[testSetupYukawa, Psi],
    Psibar,
    TestID -> "G4: GetPartnerField should find partner of Psi"
]];

AppendTo[tests, TestCreate[
    FunKit`Private`GetPartnerField[testSetupYukawa, Psibar],
    Psi,
    TestID -> "G4: GetPartnerField should find partner of Psibar"
]];

(* For a field without a partner, GetPartnerField should return the field itself *)

AppendTo[tests, TestCreate[
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

AppendTo[tests, TestCreate[
    Module[{result},
        result = FunKit`FTruncate[f2Setup, FEx[FTerm[1]]];
        Head[result]
    ],
    FEx,
    {FunKit`FTruncate::emptyTruncation},
    TestID -> "F2: FTruncate with empty truncation should warn but not abort"
]];

AppendTo[tests, TestCreate[
    Module[{result},
        result = FunKit`FTruncateOpenIndices[f2Setup, FEx[FTerm[1]]];
        Head[result]
    ],
    FEx,
    {FunKit`FTruncate::emptyTruncation},
    TestID -> "F2: FTruncateOpenIndices with empty truncation should warn but not abort"
]];

(**********************************************************************************
    F3: AddCField/AddGrassmann should detect duplicates with source fields
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[
        Module[{s},
            s = FunKit`MakeSetup[];
            FunKit`AddCSource[s, J[p]];
            FunKit`AddCField[s, J[p]];
        ],
        "AbortTriggered"
    ],
    "AbortTriggered",
    {FunKit`Private`SeDecA::FieldExists},
    TestID -> "F3: AddCField should abort when field name matches existing source"
]];

AppendTo[tests, TestCreate[
    CheckAbort[
        Module[{s},
            s = FunKit`MakeSetup[];
            FunKit`AddGrassmannSource[s, eta[p]];
            FunKit`AddGrassmann[s, eta[p]];
        ],
        "AbortTriggered"
    ],
    "AbortTriggered",
    {FunKit`Private`SeDecA::FieldExists},
    TestID -> "F3: AddGrassmann should abort when field name matches existing Grassmann source"
]];

(* Positive test: fields and sources with different names should work *)

AppendTo[tests, TestCreate[
    Module[{s},
        s = FunKit`MakeSetup[];
        FunKit`AddCSource[s, J[p]];
        FunKit`AddCField[s, Phi[p]];
        Length[s["FieldSpace"]["Commuting"]]
    ],
    1,
    TestID -> "F3: AddCField with different name than source should succeed"
]];

(**********************************************************************************
    F4: SeDecA functions should validate setup
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddCField["not a setup", Phi[p]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::notASetup},
    TestID -> "F4: AddCField with non-setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddGrassmann[42, Psi[p]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::notASetup},
    TestID -> "F4: AddGrassmann with non-setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddCSource["bad", J[p]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::notASetup},
    TestID -> "F4: AddCSource with non-setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddGrassmannSource["bad", eta[p]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::notASetup},
    TestID -> "F4: AddGrassmannSource with non-setup should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[
        Module[{s = <|"NoFieldSpace" -> True|>},
            FunKit`AddCField[s, Phi[p]]
        ],
        "AbortTriggered"
    ],
    "AbortTriggered",
    {FunKit`Private`SeDecA::notASetup},
    TestID -> "F4: AddCField with Association missing FieldSpace should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`AddVertex["bad", Propagator, {Phi}], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`SeDecA::notASetup},
    TestID -> "F4: AddVertex with non-setup should abort"
]];

(**********************************************************************************
    F5: AddVertexBasis cross-validation
    AddVertexBasis auto-adds the truncation entry if missing.
    Note: Full testing of AddVertexBasis requires a registered TensorBases basis,
    which is not available in this test file. The cross-validation behavior is
    exercised by tests that use AddVertexBasis in full setup scenarios (e.g.,
    CrossTests). Here we verify that AddVertex alone still works as expected.
**********************************************************************************)

AppendTo[tests, TestCreate[
    Module[{s},
        s = FunKit`MakeSetup[];
        FunKit`AddCField[s, Phi[p]];
        FunKit`AddVertex[s, Propagator, {Phi, Phi}];
        MemberQ[s["Truncation"][Propagator], {Phi, Phi}]
    ],
    True,
    TestID -> "F5: AddVertex creates truncation entry (prerequisite for cross-validation)"
]];

(**********************************************************************************
    C5: FAddFDRule catch-all for wrong arity
**********************************************************************************)

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddFDRule[obj, wrt], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddFDRule::invalidArgs},
    TestID -> "C5: FAddFDRule with 2 args should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FAddFDRule[obj], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddFDRule::invalidArgs},
    TestID -> "C5: FAddFDRule with 1 arg should abort"
]];

(* Positive test: 3 args should work *)

AppendTo[tests, TestCreate[
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

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`FExpand[testSetup, FTerm[1], -1], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FExpand::negativeOrder},
    TestID -> "C6: FExpand with negative order should abort"
]];

AppendTo[tests, TestCreate[
    CheckAbort[FunKit`DExpand[testSetup, FTerm[1], -2], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`DExpand::negativeOrder},
    TestID -> "C6: DExpand with negative order should abort"
]];

(* Positive test: order 0 should work *)

AppendTo[tests, TestCreate[
    Module[{result},
        result = FunKit`FExpand[testSetup, FTerm[1], 0];
        Head[result]
    ],
    FTerm,
    TestID -> "C6: FExpand with order 0 should succeed"
]];
