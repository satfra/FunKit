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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
        Module[{res = FEvaluate[FTakeDerivatives[yukawaSetup, FEx[FTerm[GammaN[{Psi, Psibar, Phi}, {i1, i2, i3}]]], {Phi[k]}]]},
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
    VerificationTest[
        CheckAbort[FResolveFDOp[scalarSetup, 42], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        {FunKit`FunKit::invalidArguments}
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
    VerificationTest[
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
    VerificationTest[
        CheckAbort[FResolveDerivatives[scalarSetup, 42], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        {FunKit`FResolveDerivatives::argument}
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
    VerificationTest[
        Module[{res = FEvaluate[FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi}, {i1, i2}]]], {Phi[k]}]]},
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
    VerificationTest[
        Module[{res = FEvaluate[FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]]], {Phi[k], Phi[l]}]]},
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
    VerificationTest[
        Module[{res = FEvaluate[FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi}, {-i1}]]], {Phi[k]}]]},
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
    VerificationTest[
        Module[{res = FEvaluate[FTakeDerivatives[scalarSetup, FEx[FTerm[GammaN[{Phi, Phi, Phi, Phi}, {i1, i2, i3, i4}]]], {Phi[k], Phi[l], Phi[m]}]]},
            FreeQ[res, FDOp, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FTakeDerivatives: result has no unresolved FDOp"
    ]
];

(**********************************************************************************
    Multi-index FDOp (user-registered correlation function — 3PI scenario)
    Regression: G appearing as a field, nested-list indices.
**********************************************************************************)

FAddCorrelationFunction[G];

(* The 3PI case from examples/ScalarTheory.nb: differentiating
   S[{Phi,Phi},{i,j}] G[{Phi,Phi},{-i,-j}] w.r.t. G[{Phi,Phi},{g,h}].
   Result must be a well-formed FEx with no FDOp left, no G-as-field,
   no nested-list indices. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = FResolveFDOp[scalarSetup, FTerm[FDOp[G[{Phi, Phi}, {g, h}]], S[{Phi, Phi}, {i, j}], G[{Phi, Phi}, {-i, -j}]]]},
            Head[res] === FEx
                && FreeQ[res, FDOp, Infinity]
                && FreeQ[res, G[{___, G, ___}, _], Infinity]
                && FreeQ[res, _[_, {___, _List, ___}], Infinity]
                && FreeQ[res, FunKit`Private`ReduceGamma, Infinity]
                && FreeQ[res, FunKit`Private`ReduceIndices, Infinity]
                && FreeQ[res, FunKit`Private`ReduceIndicesBatch, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp 3PI: derivative w.r.t. G is well-formed"
    ]
];

(* Nested FDOp[G[...]] is one of the inputs the C++ engine cannot represent, so under that
   backend these two emit FunKit::cppFallback and run through the Mathematica implementation.
   That is the intended behaviour and the result is the same either way, but the message would
   otherwise put the test in TestReport's "failed with messages" bucket. *)

(* Derivative through the entropy term Log[FTerm[G[...]]]. The chain rule
   in FunctionalD inserts an inner FDOp; FResolveDerivatives should
   fully resolve it. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = Quiet[FResolveDerivatives[scalarSetup, FEx[FTerm[FDOp[G[{Phi, Phi}, {g, h}]], -1/2, Log[FTerm[G[{Phi, Phi}, {i, -i}]]]]]], FunKit::cppFallback]},
            Head[res] === FEx
                && FreeQ[res, FDOp, Infinity]
                && FreeQ[res, G[{___, G, ___}, _], Infinity]
                && FreeQ[res, _[_, {___, _List, ___}], Infinity]
                && FreeQ[res, FunKit`Private`ReduceGamma, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveDerivatives 3PI: derivative through Log[FTerm[G]]"
    ]
];

(* The full ScalarTheory.nb 3PI scenario: differentiate the combined
   entropy + interaction Gamma w.r.t. G, fully resolved via
   FResolveDerivatives. Result must be a well-formed FEx with no
   leftover ReduceGamma / FDOp / G-as-field / nested-list-index. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{Gamma3PIInteraction, res},
            Gamma3PIInteraction = FEx[
                FTerm[-(1/2), Log[FTerm[G[{Phi, Phi}, {i, -i}]]]],
                FTerm[S[{Phi, Phi}, {i, j}], G[{Phi, Phi}, {-i, -j}]]
            ];
            res = Quiet[
                FResolveDerivatives[scalarSetup,
                    FEx[FTerm[FDOp[G[{Phi, Phi}, {g, h}]]]] ** Gamma3PIInteraction],
                FunKit::cppFallback];
            Head[res] === FEx
                && FreeQ[res, FDOp, Infinity]
                && FreeQ[res, G[{___, G, ___}, _], Infinity]
                && FreeQ[res, _[_, {___, _List, ___}], Infinity]
                && FreeQ[res, FunKit`Private`ReduceGamma, Infinity]
                && FreeQ[res, FunKit`Private`ReduceIndices, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveDerivatives 3PI: full ScalarTheory.nb scenario"
    ]
];

(**********************************************************************************
    Multi-leg dF parity (Yukawa + G) — regression for malformed FMinus
    factors emitted by the product-rule loop in FResolveFDOpInternal.

    For dF = head[{f_1,..,f_n},{i_1,..,i_n}] commuted past an object with
    leg (f_p, i_p), the sign is (-1)^(parity(dF)*parity(f_p))
    = Product_l CommuteSign(f_l, f_p). The fix emits one FMinus per
    (leg-of-dF, pair) combination; for single-leg dF this reduces to the
    pre-existing single-FMinus-per-pair behaviour.
**********************************************************************************)

(* (a) Well-formedness for mixed-leg G in Yukawa setup. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = FResolveFDOp[yukawaSetup, FTerm[FDOp[G[{Phi, Psi}, {g, h}]], Psi[k], G[{Phi, Psi}, {i, j}]]]},
            Head[res] === FEx
                && FreeQ[res, FDOp, Infinity]
                && FreeQ[res, FMinus[{_List, _}, _], Infinity]
                && FreeQ[res, FMinus[{_, _List}, _], Infinity]
                && FreeQ[res, _[_, {___, _List, ___}], Infinity]
                && FreeQ[res, FMinus, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp Yukawa G: mixed-leg G is well-formed, no leftover FMinus"
    ]
];

(* (b) Single-leg G written in multi-leg form: dF = G[{Psi},{g}] is
   Grassmann-odd. Commuting past Psi[k] picks up CommuteSign(Psi,Psi) = -1. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = FResolveFDOp[yukawaSetup, FTerm[FDOp[G[{Psi}, {g}]], Psi[k], G[{Psi}, {i}]]]},
            Not @ FreeQ[res, FTerm[___, -1, ___], Infinity] || Not @ FreeQ[res, -1 * _FTerm, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp Yukawa G: single-leg G[{Psi},{g}] past Psi gives -1"
    ]
];

(* (c) Bosonic 2-leg G[{Phi,Phi},...] is Grassmann-even (0+0=0).
   Commuting past Psi[k] gives +1 — no spurious -1 factor. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = FResolveFDOp[yukawaSetup, FTerm[FDOp[G[{Phi, Phi}, {g, h}]], Psi[k], G[{Phi, Phi}, {i, j}]]]},
            Head[res] === FEx
                && FreeQ[res, FDOp, Infinity]
                && FreeQ[res, FMinus, Infinity]
                && FreeQ[res, FTerm[___, -1, ___], Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp Yukawa G: bosonic 2-leg G past Psi gives +1 (no -1 factor)"
    ]
];

(* (d) Mixed-leg G[{Phi,Psi},...] is Grassmann-odd (0+1=1).
   Commuting past Psi[k] gives (+1)*(-1) = -1. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = FResolveFDOp[yukawaSetup, FTerm[FDOp[G[{Phi, Psi}, {g, h}]], Psi[k], G[{Phi, Psi}, {i, j}]]]},
            Not @ FreeQ[res, FTerm[___, -1, ___], Infinity] || Not @ FreeQ[res, -1 * _FTerm, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp Yukawa G: mixed-leg G past Psi gives -1"
    ]
];

(* (e) Both-Grassmann 2-leg G[{Psi,Psibar},...] is Grassmann-even (1+1=0).
   Commuting past Psi[k] gives (-1)*(-1) = +1.
   Pre-fix: malformed FMinus survives, FreeQ[res, FMinus, Infinity] FAILs.
   Post-fix: two well-formed factors whose product reduces to +1. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{res = FResolveFDOp[yukawaSetup, FTerm[FDOp[G[{Psi, Psibar}, {g, h}]], Psi[k], G[{Psi, Psibar}, {i, j}]]]},
            Head[res] === FEx
                && FreeQ[res, FDOp, Infinity]
                && FreeQ[res, FMinus, Infinity]
                && FreeQ[res, FTerm[___, -1, ___], Infinity]
        ]
        ,
        True
        ,
        TestID -> "FResolveFDOp Yukawa G: both-Grassmann 2-leg G past Psi gives +1"
    ]
];

$userCorrelationFunctions = DeleteCases[$userCorrelationFunctions, G];
$CorrelationFunctions    = Join[{Propagator, GammaN}, $userCorrelationFunctions];
