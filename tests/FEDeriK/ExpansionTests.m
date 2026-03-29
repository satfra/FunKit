(* ::Package:: *)

(**********************************************************************************
    Tests for FEDeriK Expansions module
    Covers: FExpand, DExpand — positive (happy) paths
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

scalarSetup = GetFunKitSetupScalar[];

(**********************************************************************************
    FExpand: Power[FTerm, n] expansion
**********************************************************************************)

(* Build a term with a Power structure and expand to order 2. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            (* A simple FTerm raised to a power — the kind FExpand is designed to handle *)
            expr = FTerm[Power[FTerm[Propagator[{Phi, Phi}, {i1, i2}]], 2]];
            result = FExpand[scalarSetup, expr, 2];
            FreeQ[result, Power, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FExpand: order 2 expansion removes Power"
    ]
];

(* FExpand at order 0 should give a single-term result. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FTerm[Power[FTerm[Propagator[{Phi, Phi}, {i1, i2}]], 2]];
            result = FExpand[scalarSetup, expr, 0];
            Head[result] === FTerm
        ]
        ,
        True
        ,
        TestID -> "FExpand: order 0 returns FTerm"
    ]
];

(* FExpand on an FEx maps over each term. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[
                FTerm[Power[FTerm[Propagator[{Phi, Phi}, {i1, i2}]], 2]],
                FTerm[Propagator[{Phi, Phi}, {i3, i4}]]
            ];
            result = FExpand[scalarSetup, expr, 1];
            Head[result] === FEx && FreeQ[result, Power, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FExpand: FEx overload maps over terms and removes Power"
    ]
];

(**********************************************************************************
    DExpand: Power[FTerm with FDOp, n] expansion
**********************************************************************************)

(* DExpand should expand powers of derivative operators. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            (* A term with FDOp raised to a power *)
            expr = FTerm[Power[FTerm[FDOp[Phi[i1]]], 2]];
            result = DExpand[scalarSetup, expr, 2];
            FreeQ[result, Power, Infinity]
        ]
        ,
        True
        ,
        TestID -> "DExpand: order 2 expansion of FDOp power removes Power"
    ]
];

(* DExpand at order 0 should give a single-term result. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FTerm[Power[FTerm[FDOp[Phi[i1]]], 2]];
            result = DExpand[scalarSetup, expr, 0];
            Head[result] === FTerm
        ]
        ,
        True
        ,
        TestID -> "DExpand: order 0 returns FTerm"
    ]
];

(* DExpand on an FEx maps over each term. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{expr, result},
            expr = FEx[FTerm[Power[FTerm[FDOp[Phi[i1]]], 2]]];
            result = DExpand[scalarSetup, expr, 1];
            Head[result] === FEx && FreeQ[result, Power, Infinity]
        ]
        ,
        True
        ,
        TestID -> "DExpand: FEx overload maps over terms and removes Power"
    ]
];
