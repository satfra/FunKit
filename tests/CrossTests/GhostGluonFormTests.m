(* ::Package:: *)

(**********************************************************************************
    Cross-module regression: the Yang-Mills ghost-gluon vertex flow must survive a
    FORM trace.

    Covers the full pipeline FEDeriK -> AnSEL -> DiRK -> TRACY:
        FTakeDerivatives -> FTruncate -> FSimplify -> FRoute
            -> FMakeDiagrammaticRules -> FormTrace.

    WHY THIS TEST EXISTS
    --------------------
    FRoute now puts the d_t R insertion on the bare loop momentum (see
    modules/AnSEL/Routing.m). For the ghost-gluon vertex that makes the gluon exchange
    propagator carry l - p1 - p2, and FormTracer then ABORTS in ExpandLorentzStructures:

        FormTracer`ExpandLorentzStructures::failedconsistencycheck:
          ... you either entered an ill-defined expression or
          lorentzTensorProductToSortedLists has a serious bug!

    The expression FunKit hands it is well-formed: momentum is conserved at every vertex
    and every line carries the Matsubara parity of its field (FRoute asserts both). The
    fault is in FormTracer's Lorentz-index census, getOpenIndices (FormTracer.m:1490-1500):

        getOpenIndices[expr_Times, rules] :=
            getOpenIndices[List @@ (expr //. Plus[f_, __] :> f // removeSquaredTensors), rules];
        getOpenIndices[expr_List,  rules] := ... Select[expr, Head[#] =!= Power &] ...

    It reduces every sum to its FIRST SUMMAND across the whole product, and then discards
    any Power factor. For this diagram the ghost-gluon vertex  (l[m] - p1[m])  and the
    external-leg projector  (delta[n,m] - p1[n] p1[m]/p1^2)  both collapse onto p1[m];
    Times fuses them into FTxvec[p1,m]^2; the Power is dropped; the index m vanishes from
    the census; the contraction bookkeeping is then wrong and the sort aborts.

    That is why the failure is sensitive to a PURE RELABELLING of the loop momentum:
    l -> l + p1 traces fine, l -> l does not. Whether two reduced factors collide depends
    on which momenta happen to appear, i.e. on the routing -- but no routing is more
    "correct" than another here, so this can only be fixed in FormTracer.

    This test is therefore a guard on the DEPENDENCY, not on FunKit: it pins a diagram we
    know to be well-formed and asserts that the trace goes through. If FormTracer is
    updated and regresses, or if a patched FormTracer is dropped, this goes red.
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

$FORMAvailable = Quiet[RunProcess[{"form", "-v"}]] =!= $Failed;

If[$FORMAvailable,

    (* Yang-Mills with the tensor bases the diagrammatic rules need. The field space is the
       shared fixture (which declares the ghost's Bose statistics); only "FeynmanRules" is
       added here, because tests/boilerplate/setups.m deliberately keeps the fixtures free
       of TensorBases dependencies. *)

    ghostGluonSetup =
        Append[
            GetFunKitSetupYangMills[]
            ,
            "FeynmanRules" -> <|
                GammaN -> {
                    {A, A} -> {"AA", 1}, {A, A, A} -> "AAAClass", {A, A, A, A} -> "AAAAClass",
                    {A, cb, c} -> {"Acbc", 1}, {cb, c} -> "cbc"
                },
                Propagator -> {{A, A} -> {"AA", 1}, {cb, c} -> "cbc"},
                Rdot -> {{A, A} -> {"AA", 1}, {cb, c} -> "cbc"}
            |>
        ];

    FSetGlobalSetup[ghostGluonSetup];

    (* The ghost-gluon vertex flow, routed. *)

    ghostGluonFlow =
        FTakeDerivatives[ghostGluonSetup, WetterichEquation, {A[i1], cb[i2], c[i3]}] //
        FTruncate //
        FSimplify //
        FRoute;

    (* Pre-condition, and the thing that makes the FORM failure a DEPENDENCY bug rather than
       ours: the routed expression is well-formed. FRoute aborts on a momentum-conservation
       or a Matsubara-statistics violation, so producing a "1-Loop" association at all is the
       assertion. Restate it explicitly so a reader of a red FORM test below knows where to
       look -- and where not to. *)

    AppendTo[
        tests
        ,
        VerificationTest[
            Module[{ex, vertices},
                ex = ghostGluonFlow["1-Loop"]["Expression"];
                vertices = Cases[ex, (Propagator | Rdot | GammaN)[_, idxs_] :> idxs[[All, 1]], Infinity];
                (* momentum is conserved on every object with more than one leg *)
                AllTrue[Select[vertices, Length[#] > 1&], Simplify[Total[#]] === 0&]
            ]
            ,
            True
            ,
            TestID -> "GhostGluon: the routed ghost-gluon vertex flow conserves momentum"
        ]
    ];

    (* And d_t R sits on the bare loop momentum, which is what pushes p1 + p2 onto the gluon
       exchange propagator and thereby triggers the FormTracer bug. Pinned so that if the
       routing ever stops doing this, we notice that the test below started passing for the
       wrong reason. *)

    AppendTo[
        tests
        ,
        VerificationTest[
            Module[{rdotMoms},
                rdotMoms = (#[[2, All, 1]])& /@ Cases[ghostGluonFlow["1-Loop"]["Expression"], _Rdot, Infinity];
                Length[rdotMoms] > 0 && AllTrue[rdotMoms, MatchQ[Sort[#], {-l1, l1} | {-lf1, lf1}]&]
            ]
            ,
            True
            ,
            TestID -> "GhostGluon: d_t R carries the bare loop momentum"
        ]
    ];

    (* THE REGRESSION. Trace the whole flow through FORM. This aborts against an unpatched
       FormTracer -- see the header. *)

    AppendTo[
        tests
        ,
        VerificationTest[
            Module[{projector, traceExpr, result},
                projector = FTerm[TBGetProjector["Acbc", 1, {i1, i2, i3} /. ghostGluonFlow["1-Loop"]["ExternalIndices"]]];
                traceExpr = projector ** (ghostGluonFlow["1-Loop"]["Expression"] /. FMakeDiagrammaticRules[]);
                result = CheckAbort[FormTrace["GhostGluonRegression", traceExpr], "AbortTriggered"];
                result =!= "AbortTriggered" && result =!= $Failed && FreeQ[result, _String]
            ]
            ,
            True
            ,
            TestID -> "GhostGluon: the ghost-gluon vertex flow survives a FORM trace (FORM)"
        ]
    ];
];
