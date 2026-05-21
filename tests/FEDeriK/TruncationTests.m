tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Setup helpers
**********************************************************************************)

srcSetup = GetFunKitSetupWithSources[];

ySetup = GetFunKitSetupYukawa[];

ymSetup = GetFunKitSetupYangMills[];

(**********************************************************************************
    Source fields NOT in AnyField expansion
**********************************************************************************)

(* When AnyField is expanded in a closed-index truncation, source fields
   should NOT appear among the expanded field values. *)

AppendTo[
    tests
    ,
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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

AppendTo[tests, VerificationTest[FunKit`Private`GetNonSourceFields[ySetup], FunKit`Private`GetAllFields[ySetup], TestID -> "Backward compat: GetNonSourceFields equals GetAllFields without sources"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetAllSourceFields[ySetup], {}, TestID -> "Backward compat: GetAllSourceFields empty for setup without source keys"]];

(**********************************************************************************
    Basic scalar FTruncate (closed indices)
**********************************************************************************)

scalarSetup = GetFunKitSetupScalar[];

(* A propagator term present in the truncation table must survive truncation. *)

AppendTo[
    tests
    ,
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
        Module[
            {expr, result}
            ,
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

(* An explicit GammaN vertex with mixed concrete + AnyField fields must expand
   correctly: the concrete field stays, AnyField is expanded, and the result
   survives if the combination is in the truncation table. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[
            {expr, result}
            ,
(* Free-standing explicit field Phi[i1] alongside Propagator with AnyField.
   The truncation includes Field -> {{Phi}} so the free field survives.
   The contracted AnyField in the Propagator expands normally. *)
            Module[{extSetup},
                extSetup = ySetup;
                extSetup["Truncation", Field] = {{Phi}, {Psi}, {Psibar}};
                expr = FEx[FTerm[Phi[i1], Propagator[{AnyField, AnyField}, {-i1, i2}], Rdot[{AnyField, AnyField}, {-i2, -i3}]]];
                result = FTruncate[extSetup, expr];
                (* Phi[i1] must survive, result must not be empty *)
                Not @ (result === FEx[]) && Not @ FreeQ[result, Phi, Infinity]
            ]
        ]
        ,
        True
        ,
        TestID -> "FTruncate basic: mixed concrete+AnyField fields expand correctly"
    ]
];

(* FEx annotations are passed through FTruncate unchanged. *)

AppendTo[
    tests
    ,
    VerificationTest[
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
    Field truncation: bare fields killed or kept based on Field key
**********************************************************************************)

(* Field -> {{}} means no bare fields survive — Phi[i1] next to GammaN should be killed *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result, fieldSetup},
            fieldSetup = scalarSetup;
            fieldSetup["Truncation", Field] = {{}};
            expr = FEx[FTerm[Phi[i1], GammaN[{Phi, Phi}, {-i1, i2}]]];
            result = FTruncate[fieldSetup, expr];
            result === FEx[]
        ]
        ,
        True
        ,
        TestID -> "FTruncate Field: empty Field key kills bare fields"
    ]
];

(* Field -> {{Phi}} means bare Phi fields survive *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result, fieldSetup},
            fieldSetup = scalarSetup;
            fieldSetup["Truncation", Field] = {{Phi}};
            expr = FEx[FTerm[Phi[i1], GammaN[{Phi, Phi}, {-i1, i2}]]];
            result = FTruncate[fieldSetup, expr];
            result =!= FEx[]
        ]
        ,
        True
        ,
        TestID -> "FTruncate Field: Phi in Field key keeps bare Phi"
    ]
];

(* Regression: a 1-leg Field truncation must round-trip cleanly through CTrunc's
   list-notation conversion. Length-1 list-notation objects (Field[{A, ci}]) were
   previously excluded by a Length[obj] >= 2 guard during convert-back, leaving
   malformed list-notation Field objects in the result. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result, fieldSetup},
            fieldSetup = scalarSetup;
            fieldSetup["Truncation", Field] = {{Phi}};
            expr = FEx[FTerm[Field[{Phi}, {i1}], Propagator[{AnyField, AnyField}, {-i1, i2}], GammaN[{Phi, Phi, Phi}, {-i2, i3, i4}]]];
            result = FTruncate[fieldSetup, expr];
            FreeQ[result, Field[{_, _}], Infinity] && result =!= FEx[]
        ]
        ,
        True
        ,
        TestID -> "FTruncate Field: 1-leg Field survives CTrunc list-notation round-trip"
    ]
];

(**********************************************************************************
    FTruncateOpenIndices
**********************************************************************************)

(* An AnyField 1-point function with an open index must be expanded to Phi. *)

AppendTo[
    tests
    ,
    VerificationTest[
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
    VerificationTest[
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

(* Two open indices on the same object (both propagator slots AnyField) must BOTH
   be expanded, with no spurious coefficients, and the operation must be idempotent
   and message-free. Regression test for the stale-subObj / getIndex[0,..] bug. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, once, twice, content, coeffs},
            expr = FEx[FTerm[Propagator[{AnyField, AnyField}, {i1, i2}]]];
            once = FTruncateOpenIndices[ymSetup, expr];
            (* Re-running must not error (Part::partd regression) and must be stable. *)
            twice = FTruncateOpenIndices[ymSetup, once];
            content[r_] := Sort[Cases[r, Propagator[f_List, _] :> Sort[f], Infinity]];
            coeffs[r_] := Cases[List @@ r, FTerm[c_?NumberQ, ___] :> Abs[c]];
            (* Both open slots expand: gluon {A,A} plus the two ghost leg assignments. *)
            FreeQ[once, AnyField, Infinity]
              && FreeQ[twice, AnyField, Infinity]
              && content[once] === Sort[{Sort[{A, A}], Sort[{c, cb}], Sort[{c, cb}]}]
              && content[twice] === content[once]
              && Max[Append[coeffs[once], 1]] === 1
        ]
        ,
        True
        ,
        TestID -> "FTruncateOpenIndices: AnyField in both propagator slots fully expands"
    ]
];
