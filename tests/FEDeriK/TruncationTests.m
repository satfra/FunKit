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

AppendTo[tests, TestCreate[
    Module[{expr, result},
        expr = FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {i1, i2}], Rdot[{AnyField, AnyField}, {-i2, -i1}]]];
        result = FTruncate[srcSetup, expr];
        (* J and eta should not appear from AnyField expansion *)
        FreeQ[result, J] && FreeQ[result, eta]
    ],
    True,
    TestID -> "FTruncate source: AnyField expansion excludes source fields"]
];

(**********************************************************************************
    Source fields surviving truncation when in truncation table
**********************************************************************************)

(* When source fields appear explicitly in an expression and are in the
   truncation table, they should survive truncation. *)

AppendTo[tests, TestCreate[
    Module[{expr, result},
        expr = FEx[FTerm[GammaN[{J, Phi}, {-i1, -i2}]]];
        result = FTruncate[srcSetup, expr];
        Not @ (result === FEx[])
    ],
    True,
    TestID -> "FTruncate source: explicit source field terms survive truncation"]
];

AppendTo[tests, TestCreate[
    Module[{expr, result},
        expr = FEx[FTerm[GammaN[{J}, {-i1}]]];
        result = FTruncate[srcSetup, expr];
        Not @ (result === FEx[])
    ],
    True,
    TestID -> "FTruncate source: single source field vertex survives truncation"]
];

(**********************************************************************************
    Source fields in functional derivatives
**********************************************************************************)

(* Taking a functional derivative w.r.t. a source field should work *)

AppendTo[tests, TestCreate[
    FunKit`Private`FunctionalD[srcSetup, J[i1], J[i2]],
    \[Gamma][{J, J}, {-i2, i1}],
    TestID -> "FunctionalD source: derivative of J w.r.t. J"]
];

AppendTo[tests, TestCreate[
    FunKit`Private`FunctionalD[srcSetup, AnyField[i1], J[i2]],
    \[Gamma][{J, AnyField}, {-i2, i1}],
    TestID -> "FunctionalD source: derivative of AnyField w.r.t. J"]
];

(**********************************************************************************
    Mixed expressions: derivatives then truncation
**********************************************************************************)

(* Taking a derivative w.r.t. source field and then truncating *)

AppendTo[tests, TestCreate[
    Module[{expr, derived, result},
        expr = FEx[FTerm[GammaN[{Phi}, {-i1}]]];
        derived = FTakeDerivatives[srcSetup, expr, {J[i2]}];
        result = FTruncate[srcSetup, derived];
        (* GammaN[{J, Phi}, ...] is in the truncation table, so it should survive *)
        Not @ (result === FEx[])
    ],
    True,
    TestID -> "FTruncate source: derivative w.r.t. source then truncate"]
];

(**********************************************************************************
    Backward compatibility: setup without source keys
**********************************************************************************)

(* A setup without CommutingSource/GrassmannSource should still work *)

AppendTo[tests, TestCreate[
    Module[{expr, result},
        expr = FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {i1, i2}], Rdot[{AnyField, AnyField}, {-i2, -i1}]]];
        result = FTruncate[ySetup, expr];
        Head[result] === FEx
    ],
    True,
    TestID -> "FTruncate backward compat: setup without source keys works"]
];

AppendTo[tests, TestCreate[
    FunKit`Private`GetNonSourceFields[ySetup],
    FunKit`Private`GetAllFields[ySetup],
    TestID -> "Backward compat: GetNonSourceFields equals GetAllFields without sources"]
];

AppendTo[tests, TestCreate[
    FunKit`Private`GetAllSourceFields[ySetup],
    {},
    TestID -> "Backward compat: GetAllSourceFields empty for setup without source keys"]
];
