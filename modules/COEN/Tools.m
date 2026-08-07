(**********************************************************************************
    Tools.m -- Code generation settings and shared utilities

    Public API:
      FSetRegisterSize           -- Sets register budget for CSE optimization
      FSetCodeOptimization       -- Enables/disables code optimization pipeline
      FSetFastMath               -- Enables/disables fast math GPU intrinsics
      FSetMaxKernelTerms         -- Sets max terms per sub-kernel before splitting
      FSetCodePrecision          -- Sets code precision ("single" or "double")
      FSetFullSimplifyLimit      -- LeafCount above which Simplify replaces FullSimplify

    Internal:
      parallelSimplify           -- Parallel simplify (FullSimplify for small exprs,
                                    Simplify above $codeFullSimplifyMaxLeafCount) with
                                    automatic serial fallback
                                    (used by Cpp, CppOptimize, Fortran, Julia)

    Variables:
      $codeOptimizeFunctions     -- Patterns for subexpressions eligible for CSE
      $codeOptimizeInterps       -- Patterns for interpolator calls to hoist
      $availableRegisters        -- Register budget for CSE (default 32)
      $codeOptimize              -- Master optimization toggle (default True)
      $codeFactorTerms           -- Whether to apply FactorTerms (default True)
      $codeFastMath              -- Fast math intrinsics toggle (default False)
      $codePrecision             -- "single" or "double" (default "double")
      $codeMaxKernelTerms        -- Max terms per sub-kernel (default 500)
      $codeFormatStatementLimit  -- Max chars before disabling clang-format
      $codeFMARestructure        -- FMA pattern restructuring toggle (default True)
      $codeParallelThreshold     -- Min items for parallel evaluation (default 4)
      $codeFullSimplifyMaxLeafCount -- LeafCount above which Simplify replaces
                                    FullSimplify in parallelSimplify (default 100)
**********************************************************************************)

(**********************************************************************************
    bashrc, zshrc and zshprofile sourcing to infer PATH
    (Unix only -- on Windows, PATH is already correct from the environment)
**********************************************************************************)

If[$OperatingSystem =!= "Windows",
    Module[{haveBashrc, haveZshrc, haveZshProfile, inferredPATH},
        haveBashrc = 0 == RunProcess[$SystemShell, All, "source ~/.bashrc"]["ExitCode"];
        haveZshrc = 0 == RunProcess[$SystemShell, All, "source ~/.zshrc"]["ExitCode"];
        haveZshProfile = 0 == RunProcess[$SystemShell, All, "source ~/.zprofile"]["ExitCode"];
        inferredPATH = RunProcess[
                $SystemShell
                ,
                All
                ,
                If[haveBashrc,
                        "source ~/.bashrc;"
                        ,
                        ""
                    ] <>
                    If[haveZshrc,
                        "source ~/.zshrc;"
                        ,
                        ""
                    ] <>
                    If[haveZshProfile,
                        "source ~/.zprofile;"
                        ,
                        ""
                    ] <> " echo $PATH"
            ]["StandardOutput"];
        SetEnvironment["PATH" -> inferredPATH];
    ];
];

(**********************************************************************************
    Parallel map with automatic fallback
**********************************************************************************)

(* Only use parallel evaluation when there are enough items to amortize overhead *)

$codeParallelThreshold = 4;

(* FullSimplify is by far the dominant cost in code generation. It is worth it on
   small subexpressions, but on larger ones it explodes (and dominates suite/kernel
   generation time) while buying little over the much cheaper Simplify. Above this
   LeafCount, fall back to Simplify. Both are value-preserving, so this only trades
   simplification aggressiveness for speed. Tunable via FSetFullSimplifyLimit. *)

$codeFullSimplifyMaxLeafCount = 100;

parallelSimplify[exprs_List] :=
(* Bake the limit in as a literal so the mapped function carries no FunKit-context
   dependency onto subkernels (only LeafCount/Simplify/FullSimplify, all built-in). *)
    With[{lim = $codeFullSimplifyMaxLeafCount},
        With[{simplifyOne = (If[LeafCount[#] > lim, Simplify[#], FullSimplify[#]]&)},
            (* Profiling: record the size distribution that drives the cost. Guarded by
               the master switch, so it is completely inert (no LeafCount sweep) unless
               ResetCodegenProfile[] has been called. See PrintCodegenProfile. *)
            If[TrueQ[$ProfileCodegenOn],
                Module[{lcs = LeafCount /@ exprs},
                    $ProfileCgSimplifyCount += Length[exprs];
                    $ProfileCgSimplifyFull += Count[lcs, l_ /; l <= lim];
                    $ProfileCgSimplifySimple += Count[lcs, l_ /; l > lim];
                    $ProfileCgSimplifyTotLeaf += Total[lcs];
                    $ProfileCgSimplifyMaxLeaf = Max[$ProfileCgSimplifyMaxLeaf, If[lcs === {}, 0, Max[lcs]]];
                ];
            ];
            cgTimed[$ProfileCgSimplify,
                If[Length[exprs] >= $codeParallelThreshold && Length[Kernels[]] > 0,
                    FunKitDebug[2, "Parallelizing simplify over ", Length[exprs], " expressions on ", Length[Kernels[]], " kernels (FullSimplify <= ", lim, " leaves, else Simplify)"];
                    ParallelMap[simplifyOne, exprs, DistributedContexts -> Automatic]
                    ,
                    Map[simplifyOne, exprs]
                ]
            ]
        ]
    ];

(**********************************************************************************
    Codegen profiling instrumentation (zero overhead unless enabled)

    All COEN passes are wrapped in cgTimed[counter, body], which times `body` and
    accumulates into `counter` ONLY when $ProfileCodegenOn is True. By default the
    switch is unset (TrueQ -> False), so cgTimed runs `body` and skips the two
    AbsoluteTime[] calls and the accumulation: no measurable runtime impact.

    Enable with ResetCodegenProfile[] (zeroes counters + flips the switch), run code
    generation, then PrintCodegenProfile[] for the breakdown. Mirrors the
    Reset/PrintFSimplifyProfile convention in AnSEL/Simplify.m.
**********************************************************************************)

SetAttributes[cgTimed, HoldAll];

cgTimed[counter_, body_] :=
    If[TrueQ[$ProfileCodegenOn],
        Module[{t0 = AbsoluteTime[], r},
            r = body;
            counter += AbsoluteTime[] - t0;
            r
        ]
        ,
        body
    ];

ResetCodegenProfile[] :=
    (
        $ProfileCgHoist = 0.; $ProfileCgSplit = 0.; $ProfileCgCSE = 0.;
        $ProfileCgOptExpr = 0.; $ProfileCgFallbackCSE = 0.; $ProfileCgPowerNorm = 0.;
        $ProfileCgFactor = 0.; $ProfileCgTranscendental = 0.; $ProfileCgFMA = 0.;
        $ProfileCgSimplify = 0.; $ProfileCgCppForm = 0.; $ProfileCgClangFormat = 0.;
        $ProfileCgSimplifyCount = 0; $ProfileCgSimplifyFull = 0;
        $ProfileCgSimplifySimple = 0; $ProfileCgSimplifyTotLeaf = 0;
        $ProfileCgSimplifyMaxLeaf = 0;
        $ProfileCgCppFormCount = 0; $ProfileCgClangFormatCount = 0;
        $ProfileCgTopoSort = 0.; $ProfileCgStripNames = 0.; $ProfileCgEmitScaffold = 0.;
        $ProfileCgHoistDiv = 0.;
        $ProfileCgSubKernelTimes = {};
        $ProfileCodegenOn = True;
    );

DisableCodegenProfile[] :=
    ($ProfileCodegenOn = False);

PrintCodegenProfile[] :=
    Module[{fmt, pct, total},
        fmt[t_] := ToString[NumberForm[N[t], {7, 4}]];
        (* "Wallclock" reference = the disjoint top-level passes that make up CppCode. *)
        total = $ProfileCgHoist + $ProfileCgHoistDiv + $ProfileCgSplit + $ProfileCgCSE + $ProfileCgPowerNorm +
            $ProfileCgTopoSort + $ProfileCgFactor + $ProfileCgTranscendental + $ProfileCgFMA +
            $ProfileCgSimplify + $ProfileCgCppForm + $ProfileCgStripNames + $ProfileCgClangFormat;
        pct[t_] := If[total > 0, ToString[NumberForm[100. t / total, {4, 1}]] <> "%", "-"];
        Print["  --- optimizeExpression passes ---"];
        Print["  hoistInterpolators:    ", fmt[$ProfileCgHoist], " s  (", pct[$ProfileCgHoist], ")"];
        Print["  hoistDivisions:        ", fmt[$ProfileCgHoistDiv], " s  (", pct[$ProfileCgHoistDiv], ")"];
        Print["  earlySplit/split:      ", fmt[$ProfileCgSplit], " s  (", pct[$ProfileCgSplit], ")"];
        Print["  dagCSE (total):        ", fmt[$ProfileCgCSE], " s  (", pct[$ProfileCgCSE], ")"];
        Print["    OptimizeExpression:  ", fmt[$ProfileCgOptExpr], " s"];
        Print["    fallbackCSE:         ", fmt[$ProfileCgFallbackCSE], " s"];
        Print["  normalizePowerBases:   ", fmt[$ProfileCgPowerNorm], " s  (", pct[$ProfileCgPowerNorm], ")"];
        Print["  topological re-sort:   ", fmt[$ProfileCgTopoSort], " s  (", pct[$ProfileCgTopoSort], ")"];
        Print["  algebraicFactor:       ", fmt[$ProfileCgFactor], " s  (", pct[$ProfileCgFactor], ")"];
        Print["  hoistTranscendentals:  ", fmt[$ProfileCgTranscendental], " s  (", pct[$ProfileCgTranscendental], ")"];
        Print["  fmaRestructure:        ", fmt[$ProfileCgFMA], " s  (", pct[$ProfileCgFMA], ")"];
        Print["  --- formatting ---"];
        Print["  parallelSimplify:      ", fmt[$ProfileCgSimplify], " s  (", pct[$ProfileCgSimplify], ")"];
        Print["    calls/exprs:         ", $ProfileCgSimplifyCount, "  (FullSimplify: ", $ProfileCgSimplifyFull, ", Simplify: ", $ProfileCgSimplifySimple, ")"];
        Print["    leaf total/max:      ", $ProfileCgSimplifyTotLeaf, " / ", $ProfileCgSimplifyMaxLeaf];
        Print["  CppForm:               ", fmt[$ProfileCgCppForm], " s  (", pct[$ProfileCgCppForm], ")  [", $ProfileCgCppFormCount, " calls]"];
        Print["  stripQuotedNames:      ", fmt[$ProfileCgStripNames], " s  (", pct[$ProfileCgStripNames], ")"];
        Print["  clang-format:          ", fmt[$ProfileCgClangFormat], " s  (", pct[$ProfileCgClangFormat], ")  [", $ProfileCgClangFormatCount, " calls]"];
        Print["  ----------------------------------"];
        Print["  Sum of passes:         ", fmt[total], " s"];
(* Sub-kernel work is the unit any parallelisation of the emission would map over, so its COUNT
   bounds the achievable speedup and its SKEW bounds it again: earlySplit chunks by term count,
   not by cost, so one dominant sub-kernel caps the win no matter how many kernels are up. *)
        If[Length[$ProfileCgSubKernelTimes] > 0,
            Print["  --- sub-kernels ---"];
            Print["  count / emission time: ", Length[$ProfileCgSubKernelTimes], " / ",
                fmt[$ProfileCgEmitScaffold], " s"];
            Print["  optimise total/max:    ", fmt[Total[$ProfileCgSubKernelTimes]], " s / ",
                fmt[Max[$ProfileCgSubKernelTimes]], " s  (max/total = ",
                ToString[NumberForm[N[Max[$ProfileCgSubKernelTimes] / Total[$ProfileCgSubKernelTimes]], {4, 3}]],
                ", parallel ceiling ",
                ToString[NumberForm[N[Total[$ProfileCgSubKernelTimes] / Max[$ProfileCgSubKernelTimes]], {4, 2}]],
                "x)"];
        ];
    ];

(**********************************************************************************
    Code generation settings and tools
**********************************************************************************)

$codeOptimizeFunctions = {a_Symbol[__] /; Not @ MatchQ[a, Times | Plus | Power | Rational | Complex | Real | Integer], Power[a_, b_Integer] /; (b > 1 || b < -1) && !NumberQ[a]};

$codeOptimizeInterps = {a_Symbol[__] /; Not @ MatchQ[a, Times | Plus | Power | Rational | Complex | Real | Integer]};

(* Composite-denominator (negative integer power of a Plus/Times base) global hoisting.
   These reciprocals are NOT interpolator leaves, so without this they fall to the
   PER-SUB-KERNEL CSE and get recomputed once per chunk when the term-sum is split.
   Hoisting them to shared defs (like interpolators) computes each once across chunks. *)
$codeHoistDivisions = True;

$availableRegisters = 32;

$codeOptimize = True;

$codeFactorTerms = True;

$codeFastMath = False;

$codePrecision = "double";

$codeMaxKernelTerms = 500;

(* Depth to which the kernel splitter recurses into nested sums. Level 1 (default)
   splits only the top-level Plus; level >= 2 also splits the largest nested Plus inside
   each heavy term (halving per extra level), enabling finer sub-kernels for integrands
   that are a single nested term (prefactor * big-product) rather than a wide sum. *)

$kernelSplitLevel = 1;

(* Max character length of a single C++ statement (from ; to ;) before
   clang-format is disabled for that statement to avoid OOM. *)

$codeFormatStatementLimit = 1000;

$codeFMARestructure = True;

(* Register-pressure-aware CSE. A subexpression is promoted to a (long-lived) CSE
   temporary only when its recompute cost EXCEEDS $cseCostThreshold; cheaper ones
   (small positive integer powers, a single add or multiply) are left inline so the backend
   rematerializes them at use instead of holding them in a register across a large
   basic block. ON by default — a ~1-op value is never worth a register on a
   register-starved GPU. The flag exists to A/B the effect, not to gate the behaviour. *)

$codeCSECostFilter = True;

(* Recompute-cost cutoff (in ~arithmetic ops): atoms are free; +,* cost 1 each; a small
   positive integer power (powr<n>, n<=8) costs 1; negative/fractional powers (division,
   pow()) and other calls (sqrt/exp/log/interpolators) cost ~8 and are always kept.
   Threshold 2 drops pure monomials and trivial 1-2 op combos. *)

$cseCostThreshold = 2;

FSetRegisterSize[n_Integer?Positive] :=
    Module[{},
        $availableRegisters = n;
    ];

FSetRegisterSize[___] :=
    (
        Message[FunKit::invalidArguments, FSetRegisterSize];
        Abort[]
    );

FSetCodeOptimization[b_?BooleanQ] :=
    Set[$codeOptimize, b];

FSetCodeOptimization[___] :=
    (
        Message[FunKit::invalidArguments, FSetCodeOptimization];
        Abort[]
    );

FSetFastMath[b_?BooleanQ] :=
    Module[{},
        $codeFastMath = b;
    ];

FSetFastMath[___] :=
    (
        Message[FunKit::invalidArguments, FSetFastMath];
        Abort[]
    );

FSetMaxKernelTerms[n_Integer?Positive] :=
    Module[{},
        $codeMaxKernelTerms = n;
    ];

FSetKernelSplitLevel[n_Integer?Positive] :=
    Module[{},
        $kernelSplitLevel = n;
    ];

FSetKernelSplitLevel[___] :=
    (
        Message[FunKit::invalidArguments, FSetKernelSplitLevel];
        Abort[]
    );

FSetCSECostFilter[b_?BooleanQ] :=
    Set[$codeCSECostFilter, b];

FSetCSECostFilter[___] :=
    (
        Message[FunKit::invalidArguments, FSetCSECostFilter];
        Abort[]
    );

FSetCSECostThreshold[n_Integer?NonNegative] :=
    Module[{},
        $cseCostThreshold = n;
    ];

FSetCSECostThreshold[___] :=
    (
        Message[FunKit::invalidArguments, FSetCSECostThreshold];
        Abort[]
    );

FSetMaxKernelTerms[___] :=
    (
        Message[FunKit::invalidArguments, FSetMaxKernelTerms];
        Abort[]
    );

FSetCodePrecision[p_String] /; MemberQ[{"single", "double"}, p] :=
    Module[{},
        $codePrecision = p;
    ];

FSetCodePrecision[___] :=
    (
        Message[FunKit::invalidArguments, FSetCodePrecision];
        Abort[]
    );

FSetFullSimplifyLimit[n_Integer?Positive] :=
    Set[$codeFullSimplifyMaxLeafCount, n];

FSetFullSimplifyLimit[___] :=
    (
        Message[FunKit::invalidArguments, FSetFullSimplifyLimit];
        Abort[]
    );
