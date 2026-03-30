(*
  Benchmark.m
  Discovers and runs all Mathematica benchmark files ending in "Bench.m"
  in this directory.

  Usage:
    wolframscript -script benchmarks/Benchmark.m                    (* run all *)
    wolframscript -script benchmarks/Benchmark.m ScalarBench.m      (* single file *)
    wolframscript -script benchmarks/Benchmark.m Scalar             (* directory *)
*)

(* Hide styling if we are in a CLI context. *)

If[$FrontEnd === Null,
    Unprotect[Style];
    Unprotect[StyleBox];
    Unprotect[Print];
    Style[expr_, opts___] := expr;
    StyleBox[expr_, opts___] := expr;
    Print[expr__] := Write[$Output, StringJoin[ToString[#, OutputForm]& /@ {expr}]];
    Protect[Print];
    Protect[StyleBox];
    Protect[Style];
];

(* Perform initialization *)

Print["Performing initialization tasks..."];

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

Print["Initialization complete.\n"];

(* Function to run benchmarks from a file *)

RunAndReportBenchmarks[exprText_String, benchFileName_String] :=
    Module[{},
        ToExpression[exprText];
        Print["  " <> benchFileName <> ": " <> ToString[Length[benchmarks]] <> " benchmark case(s)\n"];
        Scan[PrintBenchmarkTable, benchmarks];
        Return[benchmarks];
    ];

(* Main script *)

Module[{benchDir, filterArg, benchFiles, exprText, allResults = {},
        mOrange = RGBColor[0.8, 0.4, 0], resultsDir},
    benchDir = DirectoryName[$InputFileName];
    AppendTo[$Path, benchDir];
    AppendTo[$Path, FileNameJoin[{benchDir, "..", "modules"}]];

    (* Parse filter argument *)
    filterArg =
        Which[
            Length[$ScriptCommandLine] >= 2,
                $ScriptCommandLine[[2]]
            ,
            Length[$CommandLine] >= 1,
                Module[{pos},
                    pos = Position[$CommandLine, "-script"];
                    If[pos =!= {} && Length[$CommandLine] >= pos[[-1, 1]] + 2,
                        $CommandLine[[pos[[-1, 1]] + 2]]
                        ,
                        ""
                    ]
                ]
            ,
            True,
                ""
        ];

    If[filterArg =!= "",
        Module[{filterPath},
            filterPath = FileNameJoin[{benchDir, filterArg}];
            If[FileExistsQ[filterPath] && !DirectoryQ[filterPath],
                benchFiles = {filterPath};
                ,
                If[DirectoryQ[filterPath],
                    benchFiles = FileNames["*Bench.m", filterPath, 2];
                    ,
                    benchFiles = FileNames[filterArg, benchDir, 2];
                    If[benchFiles === {},
                        Print["ERROR: No benchmark files matched: " <> filterArg];
                        Exit[1];
                    ];
                ]
            ];
        ];
        Print[Style["Running filtered benchmarks (" <> filterArg <> ")...", Bold, mOrange]];
        ,
        benchFiles = FileNames["*Bench.m", benchDir, 1];
        Print[Style["Discovering and running benchmarks...", Bold, mOrange]];
    ];

    Print[Style["=================================", Bold, mOrange]];

    Scan[
        (
            Print[Style["Running: " <> FileNameTake[#], Bold, mOrange]];
            Print[""];
            exprText = Import[#, "Text"];
            If[StringContainsQ[exprText, "benchmarks ="] || StringContainsQ[exprText, "benchmarks="],
                Module[{results},
                    results = RunAndReportBenchmarks[exprText, FileNameTake[#]];
                    allResults = Join[allResults, results];
                ]
                ,
                Print["  ERROR: Benchmark file " <> FileNameTake[#] <> " does not define a 'benchmarks' variable."];
            ]
        )&
        ,
        benchFiles
    ];

    Print[Style["=================================", Bold, mOrange]];

    (* Export CSV *)
    If[Length[allResults] > 0,
        resultsDir = FileNameJoin[{benchDir, "results"}];
        ExportBenchmarkCSV[allResults, resultsDir];
    ];

    Print[Style["All benchmarks complete.", Bold, mOrange]];
];

Exit[0];
