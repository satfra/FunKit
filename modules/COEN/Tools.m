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
    Code generation settings and tools
**********************************************************************************)

$codeOptimizeFunctions = {a_Symbol[__] /; Not @ MatchQ[a, Times | Plus | Power | Rational | Complex | Real | Integer], Power[a_, b_Integer] /; (b > 1 || b < -1) && !NumberQ[a]};

$codeOptimizeInterps = {a_Symbol[__] /; Not @ MatchQ[a, Times | Plus | Power | Rational | Complex | Real | Integer]};

$availableRegisters = 32;

SetRegisterSize[n_Integer?Positive] :=
    Module[{},
        $availableRegisters = n;
    ];
