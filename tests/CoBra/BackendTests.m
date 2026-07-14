tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Deployment guard: build/activate the backend once; when no toolchain is
    available every test degrades to a skipped placeholder.
**********************************************************************************)

$toolchainPresent = Quiet[RunProcess[{"cmake", "--version"}]] =!= $Failed;

$cppAvailable = $toolchainPresent && Quiet[CheckAbort[Check[FSetBackendCpp[], $Failed], $Failed]] =!= $Failed;

If[$cppAvailable,
    FSetBackendMathematica[]
];

(*If a toolchain exists, activation must have succeeded*)

AppendTo[tests,
    VerificationTest[
        !$toolchainPresent || $cppAvailable
        ,
        True
        ,
        TestID -> "CoBra-Backend-DeploymentSmoke"
    ]
];

cppTest[body_, expected_, id_] :=
    If[$cppAvailable,
        VerificationTest[body, expected, TestID -> id]
        ,
        VerificationTest[True, True, TestID -> id <> "-SkippedNoToolchain"]
    ];

SetAttributes[cppTest, HoldAll];

(**********************************************************************************
    Helpers
**********************************************************************************)

scalarSetup = GetFunKitSetupScalar[];

yukawaSetup = GetFunKitSetupYukawa[];

ymSetup = GetFunKitSetupYangMills[];

qcdSetup = GetFunKitSetupQCD[];

srcSetup = GetFunKitSetupWithSources[];

wetterich := FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]];

negateTerm[t_FTerm] := FTerm[-1, ##]& @@ t;

coeffOf[t_FTerm] :=
    If[Length[t] > 0 && NumericQ[First[t]],
        First[t]
        ,
        1
    ];

(*a - b must simplify to zero, using the external-leg symmetries of derivList*)

equivalentQ[setup_, aEx_FEx, bEx_FEx, derivList_:{}] :=
    Module[{la, lb, syms},
        la = List @@ FunKit`Private`DropFExAnnotations[aEx];
        lb = negateTerm /@ (List @@ FunKit`Private`DropFExAnnotations[bEx]);
        syms =
            If[derivList === {},
                {}
                ,
                FMakeSymmetryList[setup, derivList]
            ];
        FSimplify[setup, FEx @@ Join[la, lb], "Symmetries" -> syms] === FEx[]
    ];

exactCoefficientsQ[ex_FEx] :=
    AllTrue[List @@ FunKit`Private`DropFExAnnotations[ex], MatchQ[coeffOf[#], _Integer | _Rational]&];

(**********************************************************************************
    Laziness semantics
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{h},
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"];
            {FDeferredQ[h], Head[FTruncate[scalarSetup, h]]}
        ]
        ,
        {True, FEx}
        ,
        "CoBra-Backend-Laziness"
    ]
];

(*Per-call opt-out returns a concrete FEx even with the global backend active*)

AppendTo[tests,
    cppTest[
        Module[{res},
            FSetBackendCpp[];
            res = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Mathematica"];
            FSetBackendMathematica[];
            {FDeferredQ[res], Head[res]}
        ]
        ,
        {False, FEx}
        ,
        "CoBra-Backend-PerCallOptOut"
    ]
];

(**********************************************************************************
    Parity: scalar Wetterich 2-point flow
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{h, cpp, native},
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"];
            cpp = FTruncate[scalarSetup, h];
            native = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}]];
            {
                Length[FunKit`Private`DropFExAnnotations[cpp]],
                Sort[coeffOf /@ (List @@ FunKit`Private`DropFExAnnotations[cpp])],
                exactCoefficientsQ[cpp],
                equivalentQ[scalarSetup, cpp, native, {Phi[i1], Phi[i2]}]
            }
        ]
        ,
        {2, {-1/2, 1}, True, True}
        ,
        "CoBra-Parity-Scalar2Point"
    ]
];

(*Derivatives-only evaluation, then native truncation of the C++ result*)

AppendTo[tests,
    cppTest[
        Module[{h, cppEval, native},
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"];
            cppEval = FEvaluate[h];
            native = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}]];
            {
                Head[cppEval] === FEx && !FDeferredQ[cppEval],
                equivalentQ[scalarSetup, FTruncate[scalarSetup, cppEval], native, {Phi[i1], Phi[i2]}]
            }
        ]
        ,
        {True, True}
        ,
        "CoBra-Parity-EvaluateThenNativeTruncate"
    ]
];

(*The C++ result carries the "Symmetries" annotation like the native one*)

AppendTo[tests,
    cppTest[
        Module[{h, cpp, ann},
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"];
            cpp = FTruncate[scalarSetup, h];
            ann = FunKit`Private`SeparateFExAnnotations[cpp][[2]];
            KeyExistsQ[ann, "Symmetries"]
        ]
        ,
        True
        ,
        "CoBra-Parity-SymmetryAnnotation"
    ]
];

(**********************************************************************************
    Parity: scalar 4-point flow (equivalence; term counts may legitimately
    differ between the two simplification algorithms)
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cpp, native},
            derivs = {Phi[i1], Phi[i2], Phi[i3], Phi[i4]};
            h = FTakeDerivatives[scalarSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[scalarSetup, h];
            native = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, wetterich, derivs]];
            {exactCoefficientsQ[cpp], equivalentQ[scalarSetup, cpp, native, derivs]}
        ]
        ,
        {True, True}
        ,
        "CoBra-Parity-Scalar4Point"
    ]
];

(**********************************************************************************
    Parity: Yukawa 2-point flows (Grassmann signs)
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cpp, native},
            derivs = {Psibar[i1], Psi[i2]};
            h = FTakeDerivatives[yukawaSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[yukawaSetup, h];
            native = FTruncate[yukawaSetup, FTakeDerivatives[yukawaSetup, wetterich, derivs]];
            {exactCoefficientsQ[cpp], equivalentQ[yukawaSetup, cpp, native, derivs]}
        ]
        ,
        {True, True}
        ,
        "CoBra-Parity-Yukawa2PointFermion"
    ]
];

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cpp, native},
            derivs = {Phi[i1], Phi[i2]};
            h = FTakeDerivatives[yukawaSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[yukawaSetup, h];
            native = FTruncate[yukawaSetup, FTakeDerivatives[yukawaSetup, wetterich, derivs]];
            {exactCoefficientsQ[cpp], equivalentQ[yukawaSetup, cpp, native, derivs]}
        ]
        ,
        {True, True}
        ,
        "CoBra-Parity-Yukawa2PointBoson"
    ]
];

(**********************************************************************************
    Parity: Yang-Mills gluon 2-point (ghost loop sign)
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cpp, native},
            derivs = {A[i1], A[i2]};
            h = FTakeDerivatives[ymSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[ymSetup, h];
            native = FTruncate[ymSetup, FTakeDerivatives[ymSetup, wetterich, derivs]];
            {exactCoefficientsQ[cpp], equivalentQ[ymSetup, cpp, native, derivs]}
        ]
        ,
        {True, True}
        ,
        "CoBra-Parity-YangMillsGluon2Point"
    ]
];

(**********************************************************************************
    Parity: QCD vertex flows.

    These are the two flows the NumTracer generators derive (ZA4 and ZAqbq1), and
    they cover the two cases the engine's auto-symmetry machinery treats by
    *different* algorithms (cpplib/source/simplify.cpp:686):

      {A,A,A,A} -- four identical commuting legs collapse into one orbit, and the
                   matcher becomes blind to which of them it is looking at, instead
                   of enumerating the permutation group the way FMakeSymmetryList
                   does on the native side.
      {A,qb,q}  -- no orbit at all (single A leg; qb and q are distinct fields), so
                   the engine falls back to exact open-leg matching.

    A wrong merge in either case is invisible to cpplib/tests/simplify.cpp, which
    only compares a sorted multiset of coefficients.
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cpp, native},
            derivs = {A[i1], qb[i2], q[i3]};
            h = FTakeDerivatives[qcdSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[qcdSetup, h];
            native = FTruncate[qcdSetup, FTakeDerivatives[qcdSetup, wetterich, derivs]];
            {
                Length[FunKit`Private`DropFExAnnotations[cpp]],
                exactCoefficientsQ[cpp],
                equivalentQ[qcdSetup, cpp, native, derivs]
            }
        ]
        ,
        {6, True, True}
        ,
        "CoBra-Parity-QCDQuarkGluonVertex"
    ]
];

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cpp, native},
            derivs = {A[i1], A[i2], A[i3], A[i4]};
            h = FTakeDerivatives[qcdSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[qcdSetup, h];
            native = FTruncate[qcdSetup, FTakeDerivatives[qcdSetup, wetterich, derivs]];
            {
                Length[FunKit`Private`DropFExAnnotations[cpp]],
                exactCoefficientsQ[cpp],
                equivalentQ[qcdSetup, cpp, native, derivs]
            }
        ]
        ,
        {6, True, True}
        ,
        "CoBra-Parity-QCDFourGluonVertex"
    ]
];

(**********************************************************************************
    Backend parity of the ROUTED expression.

    equivalentQ above cannot see a routing difference: it is applied BEFORE FRoute and knows
    nothing about loop-momentum shifts, so two backends can agree on the derivation and still
    hand FRoute inputs whose legs are ordered differently — which used to change the loop-momentum
    routing, and with it the integrand at fixed |l| (the integral is unchanged; the pointwise
    kernel is not). That is what forced NumTracer to pin the backend.

    With FSetRoutingAlgorithm["Canonical"] the routing is fixed by the diagram alone, so the two
    backends must now produce the same integrand. Compare the physical fingerprint: the multiset,
    per term, of the {field, momentum} of every internal line. A line is a {l, -l} pair with no
    intrinsic direction and the backends order those two legs differently, so orient each momentum
    by making the loop-momentum coefficient positive.
**********************************************************************************)

routedFingerprint[setup_, flow_] :=
    Sort @ Map[
        Function[term,
            Sort @ Cases[
                term
                ,
                (Propagator | Rdot | R)[flds_, idxs_] :>
                    Module[{m = idxs[[1, 1]]},
                        {
                            flds
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
        List @@ FRoute[setup, flow]["1-Loop"]["Expression"]
    ];

Do[
    AppendTo[tests,
        cppTest[
            Module[{h, cpp, native},
                h = FTakeDerivatives[qcdSetup, wetterich, derivs, "Backend" -> "Cpp"];
                cpp = FTruncate[qcdSetup, h];
                native = FTruncate[qcdSetup, FTakeDerivatives[qcdSetup, wetterich, derivs]];
                routedFingerprint[qcdSetup, cpp] === routedFingerprint[qcdSetup, native]
            ]
            ,
            True
            ,
            "CoBra-Parity-Routing-" <> StringJoin[ToString /@ (Head /@ derivs)]
        ]
    ]
    ,
    {derivs, {{A[i1], A[i2], A[i3]}, {A[i1], A[i2], A[i3], A[i4]}, {A[i1], qb[i2], q[i3]}}}
];

(**********************************************************************************
    Determinism of the routing — the actual defect behind the NumTracer report.

    FRoute never *chooses* a routing on physical grounds. It solves momentum conservation
    vertex-by-vertex and eliminates whichever momentum comes first:

        mom = availMomenta[[1]]                       (modules/AnSEL/Routing.m)

    availMomenta is built by flattening a Plus, so "first" means Mathematica's canonical order over
    the Unique-generated symbol names of the internal momenta (p$127, p$99, ...) — and those are
    compared as STRINGS, so p$1000 sorts before p$998. Every FRoute call allocates fresh names, so
    as the session's Unique counter crosses a digit boundary the choice flips.

    The consequence is stark: routing THE SAME EXPRESSION TWICE IN ONE SESSION can give two
    different routings. The integral is unchanged (it is a relabelling of the integration variable)
    but the integrand at fixed |l| is not, and the routing it drifts to can be far more sharply
    peaked. That is what NumTracer saw, and it is why pinning the backend did not actually fix it:
    the routing was never a function of the diagram in the first place.

    Under FSetRoutingAlgorithm["Canonical"] the leftover freedom is resolved by a physical criterion
    instead, so the routing is a function of the diagram alone and this cannot happen.

    Guarded to the C++ backend because that is where it bites — and the C++ backend is the one
    FunKit auto-activates ($FunKitBackend = "Automatic"), i.e. what production actually runs. The
    Mathematica backend's momentum names happen to sit away from a digit boundary for this diagram,
    so "Default" comes out stable under it and the test would pass vacuously.
**********************************************************************************)

(* The C++-derived scalar 4-point flow. Built through the pinned-C++ fused pipeline, i.e. the way a
   production script gets it. *)

cppScalar4ptFlow[] :=
    Module[{flow},
        FSetBackendCpp[];
        flow = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}]];
        FSetBackendMathematica[];
        flow
    ];

(* The global Unique counter, and a burn that parks it just short of the next power of ten. Reading
   it is the only way to make this test deterministic: the defect only fires when the momentum names
   FRoute allocates straddle a digit boundary, and where the counter sits at this point in the suite
   depends on everything that ran before. *)

uniqueCounter[] :=
    ToExpression @ StringJoin @ StringCases[SymbolName[Unique["fkCtr"]], DigitCharacter];

burnUniqueToDigitBoundary[] :=
    Module[{target},
        target = 10^Ceiling[Log10[uniqueCounter[] + 20]];
        Do[Unique["fkBurn"], {Max[0, target - uniqueCounter[] - 6]}];
    ];

(* Route the same expression on both sides of the boundary. Same diagram, same input, so the same
   routing must come out. *)

routingSurvivesCounterBoundary[] :=
    Module[{flow, before, after},
        flow = cppScalar4ptFlow[];
        before = routedFingerprint[scalarSetup, flow];
        burnUniqueToDigitBoundary[];
        after = routedFingerprint[scalarSetup, flow];
        before === after
    ];

AppendTo[tests,
    cppTest[
        Module[{res},
            FSetRoutingAlgorithm["Canonical"];
            res = routingSurvivesCounterBoundary[];
            res
        ]
        ,
        True
        ,
        "CoBra-Routing-DeterministicAcrossUniqueBoundary"
    ]
];

(* Discriminating sanity: "Default" really does re-route the SAME expression when the Unique counter
   crosses a digit boundary. Without this, the determinism asserted above could be a property every
   mode has, and would prove nothing. This is the NumTracer bug, reduced: the routing was never a
   function of the diagram, so pinning the backend could not have fixed it. *)

AppendTo[tests,
    cppTest[
        Module[{res},
            FSetRoutingAlgorithm["Default"];
            res = routingSurvivesCounterBoundary[];
            FSetRoutingAlgorithm["Canonical"];
            res
        ]
        ,
        False
        ,
        "CoBra-Routing-NondeterminismHazard"
    ]
];

(*The engine only simplifies when it also truncates in the same call
  (CppRunPipelineCore, modules/CoBra/Deferred.m:130). Running the four-gluon flow
  with the engine's simplify switched off and the native FSimplify applied instead
  pins the orbit matcher specifically: if it ever over-merges, this disagrees with
  the fully-C++ result above.*)

AppendTo[tests,
    cppTest[
        Module[{derivs, h, cppFull, cppNativeSimplify},
            derivs = {A[i1], A[i2], A[i3], A[i4]};
            cppFull = FTruncate[qcdSetup, FTakeDerivatives[qcdSetup, wetterich, derivs, "Backend" -> "Cpp"]];
            h = FTakeDerivatives[qcdSetup, wetterich, derivs, "Backend" -> "Cpp"];
            cppNativeSimplify =
                FSimplify[
                    qcdSetup,
                    FEvaluate[qcdSetup, h, "Truncate" -> True, "Simplify" -> False],
                    "Symmetries" -> FMakeSymmetryList[qcdSetup, derivs]
                ];
            equivalentQ[qcdSetup, cppFull, cppNativeSimplify, derivs]
        ]
        ,
        True
        ,
        "CoBra-Parity-QCDFourGluonOrbitSimplify"
    ]
];

(**********************************************************************************
    Parity: GeneralizedFlowEquation (Phidot, unordered trailing leg)
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{setup = scalarSetup, derivs, h, cpp, native, phidotTailsUpper},
            setup["Truncation"] = Join[setup["Truncation"], <|Phidot -> {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}}, R -> {{Phi, Phi}}|>];
            derivs = {Phi[i1], Phi[i2]};
            h = FTakeDerivatives[setup, GeneralizedFlowEquation, derivs, "Backend" -> "Cpp"];
            cpp = FTruncate[setup, h];
            native = FTruncate[setup, FTakeDerivatives[setup, GeneralizedFlowEquation, derivs]];
            (*Every Phidot in the result must keep its pinned "field" slot as
              the last leg, which is the upper one*)
            phidotTailsUpper =
                AllTrue[
                    Cases[FunKit`Private`DropFExAnnotations[cpp], _Phidot, Infinity]
                    ,
                    !FunKit`Private`isNeg[Last[FunKit`Private`getIndices[#]]]&
                ];
            {
                Head[cpp] === FEx,
                exactCoefficientsQ[cpp],
                phidotTailsUpper,
                equivalentQ[setup, cpp, native, derivs]
            }
        ]
        ,
        {True, True, True, True}
        ,
        "CoBra-Parity-GeneralizedFlow2Point"
    ]
];

(**********************************************************************************
    Parity: FMakeDSE runs through the C++ backend out of the box
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{cppDSE, nativeDSE},
            FSetBackendCpp[];
            cppDSE = CheckAbort[FMakeDSE[scalarSetup, Phi[i1]], $Aborted];
            FSetBackendMathematica[];
            nativeDSE = FMakeDSE[scalarSetup, Phi[i1]];
            (*The untruncated DSEs carry unresolved symbolic FMinus factors
              written relative to each engine's own commutation order, which
              FSimplify cannot identify syntactically -- compare after
              truncation resolves AnyField and the sign factors*)
            {
                cppDSE =!= $Aborted,
                Head[cppDSE] === FEx,
                equivalentQ[scalarSetup, FTruncate[scalarSetup, cppDSE], FTruncate[scalarSetup, nativeDSE]],
                exactCoefficientsQ[FTruncate[scalarSetup, cppDSE]]
            }
        ]
        ,
        {True, True, True, True}
        ,
        "CoBra-Parity-ScalarDSE"
    ]
];

(*Further derivatives of a DSE (whose untruncated form carries gamma/FMinus
  objects) also run through the C++ backend*)

AppendTo[tests,
    cppTest[
        Module[{cpp, native},
            FSetBackendCpp[];
            cpp =
                CheckAbort[
                    Module[{d},
                        d = FMakeDSE[scalarSetup, Phi[i1]];
                        d = FTakeDerivatives[scalarSetup, d, {Phi[i2]}];
                        FTruncate[scalarSetup, d]
                    ]
                    ,
                    $Aborted
                ];
            FSetBackendMathematica[];
            native = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, FMakeDSE[scalarSetup, Phi[i1]], {Phi[i2]}]];
            {
                cpp =!= $Aborted,
                exactCoefficientsQ[cpp],
                equivalentQ[scalarSetup, cpp, native]
            }
        ]
        ,
        {True, True, True}
        ,
        "CoBra-Parity-ScalarDSE2Point"
    ]
];

(**********************************************************************************
    Parity: symbolic prefactors (per-group engine runs)
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{eq, derivs, cpp, native},
            (*a coupling-dressed Wetterich equation: g * (1/2 G Rdot)*)
            eq = FEx[FTerm[Global`g, 1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]];
            derivs = {Phi[i1], Phi[i2]};
            cpp = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, eq, derivs, "Backend" -> "Cpp"]];
            native = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, eq, derivs]];
            {
                (*every term carries the exact symbolic factor g*)
                AllTrue[List @@ FunKit`Private`DropFExAnnotations[cpp], MemberQ[List @@ #, Global`g]&],
                equivalentQ[scalarSetup, cpp, native, derivs]
            }
        ]
        ,
        {True, True}
        ,
        "CoBra-Parity-SymbolicPrefactor"
    ]
];

(**********************************************************************************
    Parity: source fields (spectators in the flow, excluded from AnyField)
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{derivs, cpp, native},
            derivs = {Phi[i1], Phi[i2]};
            cpp = FTruncate[srcSetup, FTakeDerivatives[srcSetup, wetterich, derivs, "Backend" -> "Cpp"]];
            native = FTruncate[srcSetup, FTakeDerivatives[srcSetup, wetterich, derivs]];
            {
                (*AnyField expansion must not produce source fields*)
                FreeQ[cpp, J] && FreeQ[cpp, eta],
                exactCoefficientsQ[cpp],
                equivalentQ[srcSetup, cpp, native, derivs]
            }
        ]
        ,
        {True, True, True}
        ,
        "CoBra-Parity-SourceFieldFlow"
    ]
];

(**********************************************************************************
    Caching: identical runs reuse the cached result
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{h, first, second, nFiles},
            FClearCppCache[];
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"];
            first = FTruncate[scalarSetup, h];
            nFiles = Length[FileNames["*.json", FunKit`Private`$CppCacheDir]];
            second = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"]];
            (*fresh internal index symbols are drawn per ingestion, so results
              of two reads agree up to closed-index relabeling*)
            {nFiles, Length[FileNames["*.json", FunKit`Private`$CppCacheDir]], equivalentQ[scalarSetup, first, second, {Phi[i1], Phi[i2]}]}
        ]
        ,
        {2, 2, True}
        ,
        "CoBra-Backend-CacheHit"
    ]
];

(**********************************************************************************
    The "Automatic" default: first pipeline use activates the C++ backend
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{h, activated},
            Unprotect[FunKit`$FunKitBackend];
            FunKit`$FunKitBackend = "Automatic";
            Protect[FunKit`$FunKitBackend];
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}];
            activated = FunKit`$FunKitBackend;
            FSetBackendMathematica[];
            {FDeferredQ[h], activated}
        ]
        ,
        {True, "Cpp"}
        ,
        "CoBra-Backend-AutomaticDefault"
    ]
];

(**********************************************************************************
    Warn-and-fallback: unsupported input runs natively instead of aborting
**********************************************************************************)

(*Deferral time: the branch falls through to the native implementation*)

AppendTo[tests,
    cppTest[
        Module[{res, native},
            FSetBackendCpp[];
            FAddFDRule[Global`X[{f1_}, {j1_}], Phi[Global`jj_], 0];
            res = Quiet @ CheckAbort[FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}], $Aborted];
            FClearFDRules[];
            FSetBackendMathematica[];
            native = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}];
            {res =!= $Aborted, FDeferredQ[res], Head[res], equivalentQ[scalarSetup, FTruncate[scalarSetup, res], FTruncate[scalarSetup, native], {Phi[i1], Phi[i2]}]}
        ]
        ,
        {True, False, FEx, True}
        ,
        "CoBra-Fallback-DeferralTime"
    ]
];

(*Force time: the handle's instructions are replayed through the native path*)

AppendTo[tests,
    cppTest[
        Module[{h, res, native},
            FSetBackendCpp[];
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}];
            FAddFDRule[Global`X[{f1_}, {j1_}], Phi[Global`jj_], 0];
            res = Quiet @ CheckAbort[FTruncate[scalarSetup, h], $Aborted];
            FClearFDRules[];
            FSetBackendMathematica[];
            native = FTruncate[scalarSetup, FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}]];
            {res =!= $Aborted, Head[res], equivalentQ[scalarSetup, res, native, {Phi[i1], Phi[i2]}]}
        ]
        ,
        {True, FEx, True}
        ,
        "CoBra-Fallback-ForceTime"
    ]
];

(**********************************************************************************
    FDeferred traps: incompatible consumers give an actionable hard error
**********************************************************************************)

AppendTo[tests,
    cppTest[
        Module[{h},
            h = FTakeDerivatives[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, "Backend" -> "Cpp"];
            {
                CheckAbort[Quiet @ QMeSForm[scalarSetup, h], $Aborted],
                CheckAbort[Quiet @ DExpand[scalarSetup, h, 2], $Aborted],
                CheckAbort[Quiet[h ** h], $Aborted],
                CheckAbort[Quiet[2 * h], $Aborted]
            }
        ]
        ,
        {$Aborted, $Aborted, $Aborted, $Aborted}
        ,
        "CoBra-Backend-DeferredTraps"
    ]
];
