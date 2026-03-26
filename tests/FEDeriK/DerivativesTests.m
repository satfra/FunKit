tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

scalarSetup = GetFunKitSetupScalar[];
yukawaSetup = GetFunKitSetupYukawa[];

(**********************************************************************************
    FResolveFDOp tests
**********************************************************************************)

(* When the term contains no FDOp, FResolveFDOp wraps it in an FEx and returns. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FResolveFDOp[scalarSetup, FTerm[GammaN[{Phi, Phi}, {i1, i2}]]]},
            Head[res] === FEx && FreeQ[res, FDOp, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp: no FDOp returns FEx unchanged"
    ]
];

(* A trailing FDOp (acting on nothing to its right) gives zero. *)

AppendTo[
    tests
    ,
    TestCreate[
        FResolveFDOp[scalarSetup, FTerm[GammaN[{Phi}, {-i1}], FDOp[Phi[i2]]]]
        ,
        FEx[]
        ,
        TestID -> "FResolveFDOp: trailing FDOp gives zero"
    ]
];

(* Product rule: derivative acting on a product of two factors produces two terms. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FResolveFDOp[scalarSetup, FTerm[FDOp[Phi[k]], GammaN[{Phi}, {-i1}], GammaN[{Phi}, {-i2}]]]},
            FreeQ[res, FDOp, Infinity] && Length[res] === 2
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp: product rule gives two terms"
    ]
];

(* A Yukawa mixed vertex differentiated w.r.t. a bosonic field yields a 4-legged vertex.
   Field ordering is not checked — just confirm a GammaN with 4 fields appears. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FTakeDerivatives[yukawaSetup, FEx[FTerm[GammaN[{Psi, Psibar, Phi}, {i1, i2, i3}]]], {Phi[k]}]},
            Head[res] === FEx && Not @ FreeQ[res, GammaN[{_, _, _, _}, _], Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTakeDerivatives: Yukawa 3-point gives 4-point after bosonic derivative"
    ]
];

(* Passing a non-FTerm/FEx argument hits the fallback rule and aborts. *)

AppendTo[
    tests
    ,
    TestCreate[
        CheckAbort[FResolveFDOp[scalarSetup, 42], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        TestID -> "FResolveFDOp: invalid input type aborts"
    ]
];

(**********************************************************************************
    FResolveDerivatives tests
**********************************************************************************)

(* An expression already free of FDOp is returned as-is (wrapped in FEx). *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FResolveDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi}, {i1, i2}]]]]},
            Head[res] === FEx && FreeQ[res, FDOp, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveDerivatives: no FDOp passes through"
    ]
];

(* Passing a non-FTerm/FEx argument hits the fallback rule and aborts. *)

AppendTo[
    tests
    ,
    TestCreate[
        CheckAbort[FResolveDerivatives[scalarSetup, 42], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        TestID -> "FResolveDerivatives: invalid argument aborts"
    ]
];

(**********************************************************************************
    FTakeDerivatives tests
**********************************************************************************)

(* A single derivative of a 2-point function produces a 3-point function. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi}, {i1, i2}]]], {Phi[k]}]},
            Head[res] === FEx && Not @ FreeQ[res, GammaN[{Phi, Phi, Phi}, _], Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTakeDerivatives: single derivative gives 3-point vertex"
    ]
];

(* Two derivatives of a 3-point function produce a 5-point function (each derivative adds one leg). *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]]], {Phi[k], Phi[l]}]},
            Head[res] === FEx && Not @ FreeQ[res, GammaN[{Phi, Phi, Phi, Phi, Phi}, _], Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTakeDerivatives: two derivatives give 5-point vertex"
    ]
];

(* A single derivative of a 1-point function produces a 2-point function. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi}, {-i1}]]], {Phi[k]}]},
            Head[res] === FEx && Not @ FreeQ[res, GammaN[{Phi, Phi}, _], Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTakeDerivatives: derivative of 1-point gives 2-point vertex"
    ]
];

(* After taking derivatives the result must never contain unresolved FDOp. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{res = FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi, Phi, Phi}, {i1, i2, i3, i4}]]], {Phi[k], Phi[l], Phi[m]}]},
            FreeQ[res, FDOp, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTakeDerivatives: result has no unresolved FDOp"
    ]
];
