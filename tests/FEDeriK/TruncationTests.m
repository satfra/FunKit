tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Setup helpers
**********************************************************************************)

srcSetup = GetFunKitSetupWithSources[];

ySetup = GetFunKitSetupYukawa[];

(**********************************************************************************
    Source fields NOT in AnyField expansion
**********************************************************************************)

(* When AnyField is expanded in a closed-index truncation, source fields
   should NOT appear among the expanded field values. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {i1, i2}], Rdot[{AnyField, AnyField}, {-i2, -i1}]]];
            result = FTruncate[srcSetup, expr];
            (* J and eta should not appear from AnyField expansion *)
            FreeQ[result, J] && FreeQ[result, eta]
        ]
        ,
        True
        ,
        TestID -> "FTruncate source: AnyField expansion excludes source fields"
    ]
];

(**********************************************************************************
    Source fields surviving truncation when in truncation table
**********************************************************************************)

(* When source fields appear explicitly in an expression and are in the
   truncation table, they should survive truncation. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[GammaN[{J, Phi}, {-i1, -i2}]]];
            result = FTruncate[srcSetup, expr];
            Not @ (result === FEx[])
        ]
        ,
        True
        ,
        TestID -> "FTruncate source: explicit source field terms survive truncation"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[GammaN[{J}, {-i1}]]];
            result = FTruncate[srcSetup, expr];
            Not @ (result === FEx[])
        ]
        ,
        True
        ,
        TestID -> "FTruncate source: single source field vertex survives truncation"
    ]
];

(**********************************************************************************
    Mixed expressions: derivatives then truncation
**********************************************************************************)

(* Taking a derivative w.r.t. source field and then truncating *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, derived, result},
            expr = FEx[FTerm[GammaN[{Phi}, {-i1}]]];
            derived = FTakeDerivatives[srcSetup, expr, {J[i2]}];
            result = FTruncate[srcSetup, derived];
            (* GammaN[{J, Phi}, ...] is in the truncation table, so it should survive *)
            Not @ (result === FEx[])
        ]
        ,
        True
        ,
        TestID -> "FTruncate source: derivative w.r.t. source then truncate"
    ]
];

(**********************************************************************************
    Backward compatibility: setup without source keys
**********************************************************************************)

(* A setup without CommutingSource/GrassmannSource should still work *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {i1, i2}], Rdot[{AnyField, AnyField}, {-i2, -i1}]]];
            result = FTruncate[ySetup, expr];
            Head[result] === FEx
        ]
        ,
        True
        ,
        TestID -> "FTruncate backward compat: setup without source keys works"
    ]
];

AppendTo[tests, TestCreate[FunKit`Private`GetNonSourceFields[ySetup], FunKit`Private`GetAllFields[ySetup], TestID -> "Backward compat: GetNonSourceFields equals GetAllFields without sources"]];

AppendTo[tests, TestCreate[FunKit`Private`GetAllSourceFields[ySetup], {}, TestID -> "Backward compat: GetAllSourceFields empty for setup without source keys"]];

(**********************************************************************************
    Basic scalar FTruncate (closed indices)
**********************************************************************************)

scalarSetup = GetFunKitSetupScalar[];

(* A propagator term present in the truncation table must survive truncation. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i2, -i1}]]];
            result = FTruncate[scalarSetup, expr];
            Not @ (result === FEx[])
        ]
        ,
        True
        ,
        TestID -> "FTruncate basic: term in truncation table survives"
    ]
];

(* A 5-point vertex is not in the scalar truncation table and must be zeroed. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[GammaN[{Phi, Phi, Phi, Phi, Phi}, {-i1, -i2, -i3, -i4, -i5}]]];
            result = FTruncate[scalarSetup, expr];
            result === FEx[]
        ]
        ,
        True
        ,
        TestID -> "FTruncate basic: 5-point vertex not in table is zeroed"
    ]
];

(* A contracted AnyField propagator (closed indices) must be expanded into concrete fields.
   Bare open-index AnyField is not expanded by LTrunc — contraction is required. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            (* Wetterich-like trace: contracted indices force AnyField expansion *)
            expr = FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {i1, i2}], Rdot[{AnyField, AnyField}, {-i2, -i1}]]];
            result = FTruncate[scalarSetup, expr];
            FreeQ[result, AnyField, Infinity] && Not @ FreeQ[result, Phi, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTruncate basic: AnyField with contracted indices expands to Phi"
    ]
];

(* FEx annotations are passed through FTruncate unchanged. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result, annots},
            expr = FEx[FTerm[Propagator[{Phi, Phi}, {i1, i2}]], "TestKey" -> "testval"];
            result = FTruncate[scalarSetup, expr];
            annots = FunKit`Private`SeparateFExAnnotations[result][[2]];
            annots["TestKey"] === "testval"
        ]
        ,
        True
        ,
        TestID -> "FTruncate basic: FEx annotations preserved"
    ]
];

(**********************************************************************************
    FTruncateOpenIndices
**********************************************************************************)

(* An AnyField 1-point function with an open index must be expanded to Phi. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[GammaN[{AnyField}, {-i1}]]];
            result = FTruncateOpenIndices[scalarSetup, expr];
            FreeQ[result, AnyField, Infinity] && Not @ FreeQ[result, Phi, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTruncateOpenIndices: AnyField 1-point expands to Phi"
    ]
];

(* A 2-point vertex with one concrete field and one AnyField (open index) expands the AnyField.
   The resulting concrete vertex is in the truncation table and must survive. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[GammaN[{Phi, AnyField}, {-i1, -i2}]]];
            result = FTruncateOpenIndices[scalarSetup, expr];
            FreeQ[result, AnyField, Infinity] && Not @ (result === FEx[])
        ]
        ,
        True
        ,
        TestID -> "FTruncateOpenIndices: mixed Phi/AnyField 2-point expands AnyField"
    ]
];
