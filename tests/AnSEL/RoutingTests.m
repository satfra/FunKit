(* ::Package:: *)

(**********************************************************************************
    Tests for AnSEL Routing module
    Covers: FRoute, FUnroute, FSetLoopMomentumName — correctness tests
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Setup: derive scalar and Yukawa flows for routing tests
**********************************************************************************)

scalarSetup = GetFunKitSetupScalar[];
yukawaSetup = GetFunKitSetupYukawa[];

(* Scalar flows — use global setup pattern like CrossTests *)
FSetGlobalSetup[scalarSetup];

scalar2ptFlow =
    FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2]}] //
    FTruncate //
    FSimplify;

scalar4ptFlow =
    FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
    FTruncate //
    FSimplify;

(* The same flow WITHOUT the final FSimplify. This is what a production pipeline hands to FRoute
   (cf. NumTracer: FTakeDerivatives // FTruncate // FRoute), and it is the input the leg-order tests
   below need: FSimplify re-canonicalises the legs of a vertex, so permuting them on a simplified
   flow is undone before FRoute ever sees it, and any leg-order test built on it passes vacuously. *)

scalar4ptFlowRaw =
    FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
    FTruncate;

(* Yukawa flows *)
FSetGlobalSetup[yukawaSetup];

yukawa2ptFlow =
    FTakeDerivatives[yukawaSetup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
    FTruncate //
    FSimplify;

yukawaVertexFlow =
    FTakeDerivatives[yukawaSetup, WetterichEquation, {Psi[i1], Psibar[i2], Phi[i3]}] //
    FTruncate //
    FSimplify;

(**********************************************************************************
    FRoute: scalar 2-point flow
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result},
            result = FRoute[scalarSetup, scalar2ptFlow];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point flow gives Association with 1-Loop key"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, oneLoop},
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            KeyExistsQ[oneLoop, "Expression"] && KeyExistsQ[oneLoop, "ExternalIndices"] && KeyExistsQ[oneLoop, "LoopMomenta"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point 1-Loop has Expression, ExternalIndices, LoopMomenta"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, oneLoop},
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            Head[oneLoop["Expression"]] === FEx && Length[oneLoop["LoopMomenta"]] === 1
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point has 1 loop momentum"
    ]
];

(**********************************************************************************
    FRoute: scalar 4-point flow
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result},
            result = FRoute[scalarSetup, scalar4ptFlow];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 4-point flow is 1-loop"
    ]
];

(**********************************************************************************
    FRoute: single FTerm routing
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{term, result},
            term = (List @@ scalar2ptFlow)[[1]];
            result = FRoute[scalarSetup, term];
            Head[result] === Association && KeyExistsQ[result, "Expression"] && KeyExistsQ[result, "ExternalIndices"] && KeyExistsQ[result, "LoopMomenta"]
        ]
        ,
        True
        ,
        TestID -> "FRoute FTerm: single term returns Association with correct keys"
    ]
];

(**********************************************************************************
    FRoute: external indices structure
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, oneLoop, extIdx},
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            extIdx = oneLoop["ExternalIndices"];
            (* Scalar 2-point should have exactly 2 external indices *)
            Length[extIdx] === 2
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point has 2 external indices"
    ]
];

(**********************************************************************************
    FRoute: Yukawa flows
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result},
            result = FRoute[yukawaSetup, yukawaVertexFlow];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: Yukawa vertex flow is 1-loop"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, oneLoop},
            result = FRoute[yukawaSetup, yukawa2ptFlow];
            oneLoop = result["1-Loop"];
            Length[oneLoop["LoopMomenta"]] === 1
        ]
        ,
        True
        ,
        TestID -> "FRoute: Yukawa fermion 2-point has 1 loop momentum"
    ]
];

(**********************************************************************************
    FSetLoopMomentumName: custom name propagates to routing
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, oneLoop, expr},
            FSetLoopMomentumName["q"];
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            expr = oneLoop["Expression"];
            FSetLoopMomentumName["l"]; (* restore default *)
            Not @ FreeQ[expr, q1, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FSetLoopMomentumName: custom name q used in routing result"
    ]
];

(**********************************************************************************
    FSetRoutingAlgorithm["Regulator"]: regulator carries pure loop momentum {l, -l}
    (no external momenta leak into Rdot via chained vertex conservation)
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, rdotInstances},
            FSetRoutingAlgorithm["Regulator"];
            result = FRoute[scalarSetup, scalar4ptFlow];
            FSetRoutingAlgorithm["Default"];
            expr = result["1-Loop"]["Expression"];
            rdotInstances = Cases[expr, _Rdot, Infinity];
            Length[rdotInstances] > 0 && FreeQ[rdotInstances, p1] && FreeQ[rdotInstances, p2] && FreeQ[rdotInstances, p3]
        ]
        ,
        True
        ,
        TestID -> "FRoute Regulator: scalar 4-point Rdot is free of external momenta"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result},
            FSetRoutingAlgorithm["Regulator"];
            result = FRoute[scalarSetup, scalar4ptFlow];
            FSetRoutingAlgorithm["Canonical"];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"] && Length[result["1-Loop"]["LoopMomenta"]] === 1
        ]
        ,
        True
        ,
        TestID -> "FRoute Regulator: scalar 4-point still yields valid 1-loop association"
    ]
];

(* THE HAZARD, pinned. "Regulator" mode freezes the Rdot's loop momentum with
   FirstCase[rdotMoms, loopMomentum[__, _]] — the `_` matches EITHER Grassmann tag — so with
   fermionic external legs it can hand a wrong-parity momentum to a line, i.e. route a fermionic
   Matsubara frequency through a bosonic propagator. That is wrong at finite T. The statistics
   post-condition in FRoute now catches it and aborts, rather than silently emitting a wrong
   kernel. This test exists so that nobody promotes "Regulator" to the default. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result},
            FSetRoutingAlgorithm["Regulator"];
            result = CheckAbort[FRoute[yukawaSetup, yukawaVertexFlow], "AbortTriggered"];
            FSetRoutingAlgorithm["Canonical"];
            result
        ]
        ,
        "AbortTriggered"
        ,
        {FRoute::statistics}
        ,
        TestID -> "FRoute Regulator: Yukawa vertex aborts — it violates Matsubara statistics"
    ]
];

(* Discriminating sanity: Default mode for the Yukawa vertex flow DOES leak p1 (or p2)
   into at least one Rdot — confirms Regulator mode is doing meaningful work, not
   passing vacuously. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, rdotInstances},
            (* Default mode — explicitly assert no algorithm change is in effect *)
            FSetRoutingAlgorithm["Default"];
            result = FRoute[yukawaSetup, yukawaVertexFlow];
            FSetRoutingAlgorithm["Canonical"];
            expr = result["1-Loop"]["Expression"];
            rdotInstances = Cases[expr, _Rdot, Infinity];
            Or @@ (Not @ FreeQ[rdotInstances, #]& /@ {p1, p2, p3})
        ]
        ,
        True
        ,
        TestID -> "FRoute Default sanity: Yukawa vertex Rdot does carry external momenta"
    ]
];

(**********************************************************************************
    FSetRoutingAlgorithm["Canonical"] — the default.

    A routed diagram still carries the relabelling freedom l -> +-l + Delta (a change of
    integration variable). "Default" resolves it by accident: it eliminates whichever momentum
    its vertex-by-vertex solver happens to meet first, which inherits Mathematica's canonical
    order over Unique-generated symbol names and the order of the objects inside the FTerm.
    "Canonical" fixes it on physical grounds instead — see modules/AnSEL/Routing.m.
**********************************************************************************)

(* Helper: the physical fingerprint of a routing — the multiset, per term, of the {field,
   momentum} of every internal line. A line is a {l, -l} pair with no intrinsic direction, and
   the two backends order those two legs differently, so orient each momentum by making the
   loop-momentum coefficient positive. What survives is exactly the integrand's identity. *)

routingFingerprint[ex_] :=
    Sort @ Map[
        Function[term,
            Sort @ Cases[
                term
                ,
                (Propagator | Rdot | R)[flds_, idxs_] :>
                    Module[{m = idxs[[1, 1]]},
                        {
                            flds[[1]]
                            ,
                            If[Coefficient[m, l1] < 0 || Coefficient[m, lf1] < 0,
                                -m
                                ,
                                m
                            ]
                        }
                    ]
                ,
                Infinity
            ]
        ]
        ,
        List @@ ex
    ];

routedFingerprint[setup_, flow_] := routingFingerprint[FRoute[setup, flow]["1-Loop"]["Expression"]];

(* Put $FunKitBackend back to a previously saved value. "Cpp" and "Mathematica" have public
   setters; "Automatic" (the package default) has none, so it is restored directly. *)

restoreBackend["Cpp"] := FSetBackendCpp[];

restoreBackend["Mathematica"] := FSetBackendMathematica[];

restoreBackend[other_] :=
    (
        Unprotect[FunKit`$FunKitBackend];
        FunKit`$FunKitBackend = other;
        Protect[FunKit`$FunKitBackend];
        If[Length[Kernels[]] > 0,
            DistributeDefinitions[FunKit`$FunKitBackend]
        ];
    );

(* (1) THE GUARANTEE: d_t R always carries the bare loop momentum.

   This is what keeps the regulator shell centred on the radial integration variable, and hence what
   keeps the kernel cheap to integrate: d_t R_k is a shell of radius ~k, loop integrals are done in
   spherical coordinates centred at l = 0, so a d_t R sitting on l + P with |P| >> k becomes a thin
   off-centre shell, sharply peaked in the angles.

   "Bare" means the Rdot's two legs are a pure {l, -l} pair, on EITHER a bosonic l_i or a fermionic
   lf_i — which one is decided by the statistics of the regulated field, see (2). Checked across
   every fixture, including the ones with fermionic external legs where this used to be impossible. *)

allRdotsBareQ[setup_, flow_] :=
    Module[{rdotMoms},
        rdotMoms = (#[[2, All, 1]])& /@ Cases[FRoute[setup, flow]["1-Loop"]["Expression"], _Rdot, Infinity];
        Length[rdotMoms] > 0 &&
            AllTrue[rdotMoms, MatchQ[Sort[#], {-l1, l1} | {-lf1, lf1}]&]
    ];

AppendTo[
    tests
    ,
    VerificationTest[
        {
            allRdotsBareQ[scalarSetup, scalar2ptFlow],
            allRdotsBareQ[scalarSetup, scalar4ptFlow],
            allRdotsBareQ[yukawaSetup, yukawa2ptFlow],
            allRdotsBareQ[yukawaSetup, yukawaVertexFlow]
        }
        ,
        {True, True, True, True}
        ,
        TestID -> "FRoute Canonical: d_t R always carries the bare loop momentum"
    ]
];

(* (2) The rule, stated exactly:

       THE LOOP MOMENTUM IS THE MOMENTUM FLOWING THROUGH d_t R, AND ITS STATISTICS IS THAT OF THE
       REGULATED FIELD.

   The Yukawa vertex flow is the sharp test, because it has fermionic external legs. Reaching a bare
   loop momentum on a fermion-regulated term REQUIRES shifting by a fermionic external, which flips
   the loop momentum's statistics — so the boson-regulated terms must come out on a bosonic l1 and
   the fermion-regulated ones on a fermionic lf1, and both must be BARE. An earlier version of this
   canonicaliser only admitted statistics-preserving shifts, which forced it to leave d_t R
   displaced on exactly these terms. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, rdots},
            expr = FRoute[yukawaSetup, yukawaVertexFlow]["1-Loop"]["Expression"];
            rdots = Cases[expr, Rdot[flds_, idxs_] :> {flds[[1]], idxs[[1, 1]]}, Infinity];
            Length[rdots] > 0 &&
                AllTrue[rdots, MatchQ[#, {Phi, l1 | -l1} | {Psi | Psibar, lf1 | -lf1}]&]
        ]
        ,
        True
        ,
        TestID -> "FRoute Canonical: loop momentum statistics follows the regulated field"
    ]
];

(* ...and both statistics really do occur in that flow, so the assertion above is not vacuous. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr},
            expr = FRoute[yukawaSetup, yukawaVertexFlow]["1-Loop"]["Expression"];
            {Not @ FreeQ[expr, l1], Not @ FreeQ[expr, lf1]}
        ]
        ,
        {True, True}
        ,
        TestID -> "FRoute Canonical: Yukawa vertex flow carries both a bosonic and a fermionic loop"
    ]
];

(* (3) Invariance under the three things that can move the routing today.

   (3a) Backend. The C++ and the Mathematica backend emit the legs of identical-field vertices in
        opposite slot order, which feeds straight into FRoute's greedy object reordering. *)

AppendTo[
    tests
    ,
    VerificationTest[
        (* The backend is global state, so it must be put back exactly as it was found: this file
           runs third alphabetically, and leaving it pinned silently forced every later suite in
           the same kernel onto that backend regardless of what tests/init.m had selected. *)
        Module[{flowMma, flowCpp, savedBackend = FunKit`$FunKitBackend},
            FSetGlobalSetup[scalarSetup];   (* FTruncate reads $GlobalSetup *)
            FSetBackendMathematica[];
            flowMma = FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] // FTruncate;
            FSetBackendCpp[];
            flowCpp = FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] // FTruncate;
            restoreBackend[savedBackend];
            FSetGlobalSetup[yukawaSetup];
            routedFingerprint[scalarSetup, flowMma] === routedFingerprint[scalarSetup, flowCpp]
        ]
        ,
        True
        ,
        TestID -> "FRoute Canonical: routing is independent of the evaluation backend"
    ]
];

(* (3b) Leg order inside identical-field vertices — the mechanism behind (3a) — lives in
        tests/CoBra/BackendTests.m ("CoBra-Routing-LegOrder*"). It needs the C++ backend to have
        any teeth: the Mathematica backend's canonical leg order happens to be a fixed point of the
        vertex-leg rotations, so under it even "Default" is (accidentally) invariant, and a test
        placed here would pass vacuously. The C++ leg order is not, and that is precisely the
        configuration NumTracer ships. *)

(* (3c) The Unique counter. FRoute names the internal momenta with Unique[], and its solve-for
        choice inherits Mathematica's canonical order over those names — which compares them as
        STRINGS, so p$1000 sorts before p$998. The routing must not depend on where in the
        session the counter happens to sit. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{before, after},
            before = routedFingerprint[scalarSetup, scalar4ptFlow];
            Do[Unique["p"], {2000}];
            after = routedFingerprint[scalarSetup, scalar4ptFlow];
            before === after
        ]
        ,
        True
        ,
        TestID -> "FRoute Canonical: routing is independent of the Unique counter state"
    ]
];

(* (4) The statistics post-condition itself, on the routings we actually ship: every line must
   carry the Matsubara parity of its field — Grassmann lines odd, commuting lines even. FRoute
   aborts if not, so reaching a result at all is the assertion; check a fermionic setup where the
   condition has teeth. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, lines, parity, required},
            expr = FRoute[yukawaSetup, yukawaVertexFlow]["1-Loop"]["Expression"];
            (* Externals of {Psi[i1], Psibar[i2], Phi[i3]} are p1 (Grassmann), p2 (Grassmann) and
               the dependent leg -p1-p2 (commuting). Loop momenta are l1 (commuting) and lf1
               (Grassmann). A momentum's Matsubara parity is the Z2 sum of the coefficients of the
               Grassmann momenta among them. *)
            parity[m_] := Mod[Total[Coefficient[m, #]& /@ {lf1, p1, p2}], 2];
            required = <|Psi -> 1, Psibar -> 1, Phi -> 0|>;
            lines = Cases[expr, (Propagator | Rdot)[flds_, idxs_] :> {flds[[1]], idxs[[1, 1]]}, Infinity];
            Length[lines] > 0 && AllTrue[lines, parity[#[[2]]] === required[#[[1]]]&]
        ]
        ,
        True
        ,
        TestID -> "FRoute Canonical: every line carries the Matsubara parity of its field"
    ]
];

(**********************************************************************************
    Statistics are NOT Grassmann parity: ghosts.

    Faddeev-Popov ghosts anticommute (Grassmann) but obey PERIODIC boundary conditions in
    imaginary time, so they carry BOSONIC Matsubara frequencies. FunKit has no notion of a
    "ghost" — only of Grassmann — so this has to be declared, via the field space's
    "BoseStatistics". Routing keys off that trait (HasFermiStatistics), while signs keep
    keying off IsGrassmann, so ghosts still anticommute.
**********************************************************************************)

ymSetup = GetFunKitSetupYangMills[];   (* declares "BoseStatistics" -> {c} *)

(* The ghost loop of the gluon 2-point flow must be routed with a BOSONIC loop momentum. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{expr, ghostLines},
            FSetGlobalSetup[ymSetup];
            expr = FRoute[ymSetup,
                FTakeDerivatives[ymSetup, WetterichEquation, {A[i1], A[i2]}] // FTruncate
            ]["1-Loop"]["Expression"];
            ghostLines = Cases[expr, (Propagator | Rdot)[flds_, idxs_] /; MemberQ[{c, cb}, flds[[1]]] :> idxs[[1, 1]], Infinity];
            Length[ghostLines] > 0 && AllTrue[ghostLines, FreeQ[#, lf1]&]
        ]
        ,
        True
        ,
        TestID -> "FRoute Canonical: ghost lines carry bosonic momenta (BoseStatistics)"
    ]
];

(* Discriminating sanity: drop the declaration and the ghost loop goes back to a FERMIONIC lf1 —
   the old, finite-T-wrong behaviour. This shows the trait is doing the work, and pins the
   back-compat fallback (undeclared Grassmann => Fermi) at the same time. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{bare, expr, ghostLines},
            bare = ymSetup;
            bare["FieldSpace"] = KeyDrop[bare["FieldSpace"], "BoseStatistics"];
            FSetGlobalSetup[bare];
            expr = FRoute[bare,
                FTakeDerivatives[bare, WetterichEquation, {A[i1], A[i2]}] // FTruncate
            ]["1-Loop"]["Expression"];
            FSetGlobalSetup[ymSetup];
            ghostLines = Cases[expr, (Propagator | Rdot)[flds_, idxs_] /; MemberQ[{c, cb}, flds[[1]]] :> idxs[[1, 1]], Infinity];
            Length[ghostLines] > 0 && AllTrue[ghostLines, Not @ FreeQ[#, lf1]&]
        ]
        ,
        True
        ,
        TestID -> "FRoute Canonical: without BoseStatistics ghosts fall back to fermionic momenta"
    ]
];

(**********************************************************************************
    FRoute: disconnected FTerms — the term is split into connected components,
    each routed independently with its own external momenta, then merged.
**********************************************************************************)

(* Two disjoint scalar propagators: each leg pair gets its own conserved
   momentum (p1, -p1) and (p2, -p2). *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, ext, expr, propMoms},
            result = FRoute[scalarSetup,
                FTerm[Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}]]
            ];
            ext = result["ExternalIndices"];
            expr = result["Expression"];
            propMoms = Cases[expr, Propagator[_, momPair_] :> momPair[[All, 1]], Infinity];
            And[
                Length[ext] === 4,
                Length[result["LoopMomenta"]] === 0,
                AllTrue[propMoms, Simplify[Total[#]] === 0&]
            ]
        ]
        ,
        True
        ,
        TestID -> "FRoute disconnected: two scalar propagators route to (p1,-p1)+(p2,-p2)"
    ]
];

(* The original failing case from the CompositeOperators.nb work: two disjoint
   fermion propagators. *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, propMoms},
            result = FRoute[yukawaSetup,
                FTerm[Propagator[{Psi, Psibar}, {i12, i11}], Propagator[{Psi, Psibar}, {i22, i21}]]
            ];
            expr = result["Expression"];
            propMoms = Cases[expr, Propagator[_, momPair_] :> momPair[[All, 1]], Infinity];
            And[
                Length[result["ExternalIndices"]] === 4,
                Length[result["LoopMomenta"]] === 0,
                AllTrue[propMoms, Simplify[Total[#]] === 0&]
            ]
        ]
        ,
        True
        ,
        TestID -> "FRoute disconnected: two fermion propagators (CompositeOperators repro)"
    ]
];

(* Three disjoint propagators: confirm the renumbering keeps externals collision-free
   and momenta locally conserved on every component. *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, ext, expr, propMoms, momSymbols},
            result = FRoute[scalarSetup,
                FTerm[
                    Propagator[{Phi, Phi}, {i1, i2}],
                    Propagator[{Phi, Phi}, {i3, i4}],
                    Propagator[{Phi, Phi}, {i5, i6}]
                ]
            ];
            ext = result["ExternalIndices"];
            expr = result["Expression"];
            propMoms = Cases[expr, Propagator[_, momPair_] :> momPair[[All, 1]], Infinity];
            momSymbols = DeleteDuplicates @ Cases[propMoms, _Symbol, Infinity];
            And[
                Length[ext] === 6,
                Length[result["LoopMomenta"]] === 0,
                AllTrue[propMoms, Simplify[Total[#]] === 0&],
                Sort[momSymbols] === {p1, p2, p3}
            ]
        ]
        ,
        True
        ,
        TestID -> "FRoute disconnected: three scalar propagators get distinct externals"
    ]
];

(* Sanity: a single connected propagator is unchanged by the disconnected guard. *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{guarded, plain},
            guarded = FRoute[scalarSetup, FTerm[Propagator[{Phi, Phi}, {i1, i2}]]];
            plain = guarded["Expression"];
            And[
                Length[guarded["ExternalIndices"]] === 2,
                Length[guarded["LoopMomenta"]] === 0,
                MatchQ[plain, FEx[FTerm[Propagator[{Phi, Phi}, _]]]]
            ]
        ]
        ,
        True
        ,
        TestID -> "FRoute disconnected guard: connected single propagator unchanged"
    ]
];
