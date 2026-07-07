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
