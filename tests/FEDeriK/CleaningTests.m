(* ::Package:: *)

(**********************************************************************************
    Tests for FEDeriK Cleaning module
    Covers: FixIndices, FDOpCount, ReduceFTerm, ReduceFEx
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

scalarSetup = GetFunKitSetupScalar[];
yukawaSetup = GetFunKitSetupYukawa[];

(**********************************************************************************
    FixIndices: relabel closed indices to canonical form
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result, closedBefore, closedAfter},
            (* Build a term with non-canonical closed indices (x, y instead of i1, i2) *)
            expr = FTerm[Propagator[{Phi, Phi}, {x, y}], Propagator[{Phi, Phi}, {-y, -x}]];
            result = FunKit`Private`FixIndices[scalarSetup, expr];
            (* After FixIndices, the closed indices should be renamed but the structure preserved *)
            closedBefore = FunKit`Private`GetClosedSuperIndices[scalarSetup, expr];
            closedAfter = FunKit`Private`GetClosedSuperIndices[scalarSetup, result];
            (* Same number of closed indices, but names changed *)
            Length[closedBefore] === Length[closedAfter] && closedBefore =!= closedAfter
        ]
        ,
        True
        ,
        TestID -> "FixIndices: relabels closed indices to new unique names"
    ]
];

(* FixIndices applied to an FEx should map over each term. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result},
            expr = FEx[
                FTerm[Propagator[{Phi, Phi}, {x, y}], Propagator[{Phi, Phi}, {-y, -x}]],
                FTerm[Propagator[{Phi, Phi}, {x, y}], Propagator[{Phi, Phi}, {-y, -x}]]
            ];
            result = FunKit`Private`FixIndices[scalarSetup, expr];
            Head[result] === FEx && Length[result] === 2
        ]
        ,
        True
        ,
        TestID -> "FixIndices: FEx overload maps over terms"
    ]
];

(**********************************************************************************
    FDOpCount: count derivative operators in expressions
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        FunKit`Private`FDOpCount[FTerm[FDOp[Phi[i1]], Propagator[{Phi, Phi}, {i2, i3}]]]
        ,
        1
        ,
        TestID -> "FDOpCount: single FDOp in FTerm gives 1"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FunKit`Private`FDOpCount[FTerm[Propagator[{Phi, Phi}, {i1, i2}]]]
        ,
        0
        ,
        TestID -> "FDOpCount: no FDOp gives 0"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FunKit`Private`FDOpCount[FTerm[FDOp[Phi[i1]], FDOp[Phi[i2]]]]
        ,
        2
        ,
        TestID -> "FDOpCount: two FDOps in FTerm gives 2"
    ]
];

(**********************************************************************************
    ReduceFTerm: merge numeric prefactors and simplify
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result},
            (* A term with numeric factors that should be merged *)
            expr = FTerm[2, 3, Propagator[{Phi, Phi}, {i1, -i1}]];
            result = FunKit`Private`ReduceFTerm[scalarSetup, expr];
            Head[result] === FTerm && (List @@ result)[[1]] === 6
        ]
        ,
        True
        ,
        TestID -> "ReduceFTerm: merges numeric prefactors"
    ]
];

(**********************************************************************************
    ReduceFEx: filters zero terms and relabels indices
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, result},
            expr = FEx[FTerm[0], FTerm[Propagator[{Phi, Phi}, {i1, -i1}]]];
            result = FunKit`Private`ReduceFEx[scalarSetup, expr];
            Head[result] === FEx && Length[result] === 1
        ]
        ,
        True
        ,
        TestID -> "ReduceFEx: filters out zero terms"
    ]
];
