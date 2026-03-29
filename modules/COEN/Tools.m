(**********************************************************************************
    Tools.m -- Code generation settings and shared utilities

    Public API:
      FSetRegisterSize           -- Sets register budget for CSE optimization
      FSetCodeOptimization       -- Enables/disables code optimization pipeline
      FSetFastMath               -- Enables/disables fast math GPU intrinsics
      FSetMaxKernelTerms         -- Sets max terms per sub-kernel before splitting
      FSetCodePrecision          -- Sets code precision ("single" or "double")

    Internal:
      balancedBracesQ            -- Checks balanced parentheses in a string
                                    (used by Cpp, CppOptimize)
      balancedRBracesQ           -- Checks balanced square brackets in a string
                                    (used by Cpp)
      hasNoOperators             -- Tests if a string contains no operators
                                    (used by Cpp)
      parallelSimplify           -- Parallel FullSimplify with automatic fallback
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
**********************************************************************************)

(**********************************************************************************
    bashrc, zshrc and zshprofile sourcing to infer PATH
**********************************************************************************)

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

(**********************************************************************************
    Brace detection
**********************************************************************************)

balancedBracesQ[str_String] :=
    Module[{cases, idx},
        If[Not @ (StringCount[str, "("] === StringCount[str, ")"]),
            Return[False]
        ];
        cases = StringCases[str, "(" | ")"];
        For[idx = 1, idx <= Length[cases], idx++,
            If[(Count[cases[[ ;; idx]], "("] < Count[cases[[ ;; idx]], ")"]),
                Return[False]
            ];
        ];
        Return[True];
    ];

balancedRBracesQ[str_String] :=
    StringCount[str, "["] === StringCount[str, "]"]

hasNoOperators[str_String] :=
    StringFreeQ[str, ")"] && StringFreeQ[str, "("] && StringFreeQ[str, "["] && StringFreeQ[str, "]"] && StringFreeQ[str, "*"] && StringFreeQ[str, "/"] && StringFreeQ[str, "+"] && StringFreeQ[str, "-"] && StringFreeQ[str, "%"] && StringFreeQ[str, "&"]

(**********************************************************************************
    Parallel map with automatic fallback
**********************************************************************************)

(* Only use parallel evaluation when there are enough items to amortize overhead *)
$codeParallelThreshold = 4;

parallelSimplify[exprs_List] :=
    If[Length[exprs] >= $codeParallelThreshold && Length[Kernels[]] > 0,
        FunKitDebug[2, "Parallelizing FullSimplify over ", Length[exprs], " expressions on ", Length[Kernels[]], " kernels"];
        ParallelMap[FullSimplify, exprs, DistributedContexts -> Automatic]
        ,
        Map[FullSimplify, exprs]
    ];

(**********************************************************************************
    Code generation settings and tools
**********************************************************************************)

$codeOptimizeFunctions = {a_Symbol[__] /; Not @ MatchQ[a, Times | Plus | Power | Rational | Complex | Real | Integer], Power[a_, b_Integer] /; (b > 1 || b < -1) && !NumberQ[a]};

$codeOptimizeInterps = {a_Symbol[__] /; Not @ MatchQ[a, Times | Plus | Power | Rational | Complex | Real | Integer]};

$availableRegisters = 32;

$codeOptimize = True;

$codeFactorTerms = True;

$codeFastMath = False;

$codePrecision = "double";

$codeMaxKernelTerms = 500;

(* Max character length of a single C++ statement (from ; to ;) before
   clang-format is disabled for that statement to avoid OOM. *)
$codeFormatStatementLimit = 1000;

$codeFMARestructure = True;

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

