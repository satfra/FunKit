(* BenchmarkLib.m — Shared benchmark utilities *)

(**********************************************************************************
    Timing
**********************************************************************************)

BenchmarkThunk[label_String, thunk_, nWarm_Integer:2, nRuns_Integer:5] :=
    Module[{ts, t},
        WriteString[$Output, "    " <> label <> ": warming up..."];
        Do[thunk[], {nWarm}];
        ts =
            Table[
                WriteString[$Output, "\r    " <> label <> ": run " <> ToString[i] <> "/" <> ToString[nRuns] <> "...    "];
                t = First @ AbsoluteTiming[thunk[]];
                t
                ,
                {i, nRuns}
            ];
        WriteString[$Output, "\r    " <> label <> ": " <> ToString[NumberForm[Mean[ts], {5, 3}]] <> " s           \n"];
        <|
            "Label" -> label
            ,
            "Mean" -> Mean[ts]
            ,
            "StdDev" ->
                If[Length[ts] > 1,
                    StandardDeviation[ts]
                    ,
                    0.
                ]
            ,
            "Runs" -> ts
        |>
    ];

(**********************************************************************************
    Wetterich / Flow Equation Benchmarks
**********************************************************************************)

BenchmarkCase[title_String, derivList_List, fkSetup_Association, qmesSetup_, doFunSetupStr_, doFunFields_String, nWarm_Integer:2, nRuns_Integer:5] :=
    Module[{fkDerivRes, fkTruncRes, results = <||>},
        FSetGlobalSetup[fkSetup];
        Print["  [" <> title <> "]"];
        (* Disable AutoSimplify for fair comparison: QMeS does not reduce diagrams *)
        FSetAutoSimplify[False];
        (* Pre-compute intermediate results for stage isolation *)
        WriteString[$Output, "    Pre-computing..."];
        fkDerivRes = FTakeDerivatives[fkSetup, WetterichEquation, derivList];
        fkTruncRes = FTruncate[fkSetup, fkDerivRes];
        WriteString[$Output, " done\n"];
        (* FunKit individual stages *)
        results["FunKit-FTakeDerivatives"] =
            BenchmarkThunk[
                "FTakeDerivatives"
                ,
                Function[{},
                    FTakeDerivatives[fkSetup, WetterichEquation, derivList]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        results["FunKit-FTruncate"] =
            BenchmarkThunk[
                "FTruncate"
                ,
                Function[{},
                    FTruncate[fkSetup, fkDerivRes]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        results["FunKit-FRoute"] =
            BenchmarkThunk[
                "FRoute"
                ,
                Function[{},
                    FRoute[fkSetup, fkTruncRes]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        (* FunKit full derivation *)
        results["FunKit-Full"] =
            BenchmarkThunk[
                "FunKit Full"
                ,
                Function[{},
                    FTakeDerivatives[fkSetup, WetterichEquation, derivList] // FTruncate
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        (* Restore AutoSimplify *)
        FSetAutoSimplify[True];
        (* QMeS — without ReduceIdenticalFlowDiagrams for fair comparison *)
        If[qmesSetup =!= None,
            results["QMeS-Full"] =
                BenchmarkThunk[
                    "QMeS Full"
                    ,
                    Function[{},
                        DeriveFunctionalEquation[qmesSetup, derivList, "OutputLevel" -> "SuperindexDiagrams"]
                    ]
                    ,
                    nWarm
                    ,
                    nRuns
                ];
        ];
        (* DoFun — with identify->False to skip diagram reduction for fair comparison *)
        If[doFunSetupStr =!= None,
            results["DoFun-Full"] =
                BenchmarkThunk[
                    "DoFun Full"
                    ,
                    Function[{},
                        wrapDoFun[doFunSetupStr <> "doRGE[" <> doFunFields <> ", identify->False]"]
                    ]
                    ,
                    nWarm
                    ,
                    nRuns
                ];
        ];
        <|"Title" -> title, "Results" -> results|>
    ];

(**********************************************************************************
    DSE Benchmarks
**********************************************************************************)

BenchmarkDSECase[title_String, field_, derivList_List, fkSetup_Association, qmesSetup_, doFunSetupStr_, doFunCmd_String, nWarm_Integer:2, nRuns_Integer:5] :=
    Module[{dse, fkDerivRes, fkTruncRes, results = <||>, qmesDerivList},
        FSetGlobalSetup[fkSetup];
        Print["  [" <> title <> "]"];
        (* Disable AutoSimplify for fair comparison: QMeS does not reduce diagrams *)
        FSetAutoSimplify[False];
        (* Pre-compute intermediate results *)
        WriteString[$Output, "    Pre-computing..."];
        dse = FMakeDSE[fkSetup, field];
        If[Length[derivList] > 0,
            fkDerivRes = FTakeDerivatives[fkSetup, dse, derivList];
            ,
            fkDerivRes = dse;
        ];
        fkTruncRes = FTruncate[fkSetup, fkDerivRes];
        WriteString[$Output, " done\n"];
        (* FunKit individual stages *)
        results["FunKit-FMakeDSE"] =
            BenchmarkThunk[
                "FMakeDSE"
                ,
                Function[{},
                    FMakeDSE[fkSetup, field]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        If[Length[derivList] > 0,
            results["FunKit-FTakeDerivatives"] =
                BenchmarkThunk[
                    "FTakeDerivatives"
                    ,
                    Function[{},
                        FTakeDerivatives[fkSetup, dse, derivList]
                    ]
                    ,
                    nWarm
                    ,
                    nRuns
                ];
        ];
        results["FunKit-FTruncate"] =
            BenchmarkThunk[
                "FTruncate"
                ,
                Function[{},
                    FTruncate[fkSetup, fkDerivRes]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        results["FunKit-FRoute"] =
            BenchmarkThunk[
                "FRoute"
                ,
                Function[{},
                    FRoute[fkSetup, fkTruncRes]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        (* FunKit full *)
        results["FunKit-Full"] =
            BenchmarkThunk[
                "FunKit Full"
                ,
                Function[{},
                    Module[{d},
                        d = FMakeDSE[fkSetup, field];
                        If[Length[derivList] > 0,
                            d = FTakeDerivatives[fkSetup, d, derivList]
                        ];
                        d // FTruncate
                    ]
                ]
                ,
                nWarm
                ,
                nRuns
            ];
        (* Restore AutoSimplify *)
        FSetAutoSimplify[True];
        (* QMeS — for DSE, the derivative list includes the DSE field as the last entry *)
        If[qmesSetup =!= None,
            qmesDerivList = Join[derivList, {field}];
            results["QMeS-Full"] =
                BenchmarkThunk[
                    "QMeS Full"
                    ,
                    Function[{},
                        Module[{diag},
                            diag = DeriveFunctionalEquation[qmesSetup, qmesDerivList, "OutputLevel" -> "SuperindexDiagrams"];
                            diag
                        ]
                    ]
                    ,
                    nWarm
                    ,
                    nRuns
                ];
        ];
        (* DoFun — with identify->False to skip diagram reduction for fair comparison *)
        If[doFunSetupStr =!= None,
            results["DoFun-Full"] =
                BenchmarkThunk[
                    "DoFun Full"
                    ,
                    Function[{},
                        wrapDoFun[doFunSetupStr <> StringReplace[doFunCmd, "]" ~~ EndOfString :> ", identify->False]"]]
                    ]
                    ,
                    nWarm
                    ,
                    nRuns
                ];
        ];
        <|"Title" -> title, "Results" -> results, "IsDSE" -> True|>
    ];

(**********************************************************************************
    Table Formatting
**********************************************************************************)

FormatTime[assoc_Association] :=
    ToString[NumberForm[assoc["Mean"], {5, 3}]] <> " s";

FormatTime[_] :=
    "    -    ";

FormatTimeFull[assoc_Association] :=
    ToString[NumberForm[assoc["Mean"], {5, 3}]] <> " +/- " <> ToString[NumberForm[assoc["StdDev"], {4, 3}]] <> " s";

FormatTimeFull[_] :=
    "    -    ";

padRight[str_String, width_Integer] :=
    StringPadRight[str, width];

PrintBenchmarkTable[bench_Association] :=
    Module[{title, res, isDSE, hasQMeS, hasDoFun, rows, col1W, col2W, col3W, col4W, topLine, midLine, botLine, sepLine, headerLine, titleLine, fmtRow},
        title = bench["Title"];
        res = bench["Results"];
        isDSE = TrueQ[bench["IsDSE"]];
        hasQMeS = KeyExistsQ[res, "QMeS-Full"];
        hasDoFun = KeyExistsQ[res, "DoFun-Full"];
        (* Build rows: {label, funkit, qmes, dofun} *)
        rows =
            {
                {
                    "Full derivation"
                    ,
                    FormatTime[res["FunKit-Full"]]
                    ,
                    If[hasQMeS,
                        FormatTime[res["QMeS-Full"]]
                        ,
                        "    -    "
                    ]
                    ,
                    If[hasDoFun,
                        FormatTime[res["DoFun-Full"]]
                        ,
                        "    -    "
                    ]
                }
            };
        (* Separator marker *)
        AppendTo[rows, "sep"];
        (* Individual FunKit stages *)
        If[isDSE && KeyExistsQ[res, "FunKit-FMakeDSE"],
            AppendTo[rows, {"FMakeDSE", FormatTime[res["FunKit-FMakeDSE"]], "    -    ", "    -    "}];
        ];
        If[KeyExistsQ[res, "FunKit-FTakeDerivatives"],
            AppendTo[rows, {"FTakeDerivatives", FormatTime[res["FunKit-FTakeDerivatives"]], "    -    ", "    -    "}];
        ];
        AppendTo[rows, {"FTruncate", FormatTime[res["FunKit-FTruncate"]], "    -    ", "    -    "}];
        AppendTo[rows, {"FRoute", FormatTime[res["FunKit-FRoute"]], "    -    ", "    -    "}];
        (* Column widths *)
        col1W = Max[StringLength /@ Select[rows, ListQ][[All, 1]], StringLength["Stage"]] + 2;
        col2W = Max[StringLength /@ Select[rows, ListQ][[All, 2]], StringLength["FunKit"]] + 2;
        col3W = Max[StringLength /@ Select[rows, ListQ][[All, 3]], StringLength["QMeS"]] + 2;
        col4W = Max[StringLength /@ Select[rows, ListQ][[All, 4]], StringLength["DoFun"]] + 2;
        (* Line builders *)
        topLine = "+" <> StringJoin[Table["=", col1W + col2W + col3W + col4W + 3]] <> "+";
        midLine = "+" <> StringPadRight["", col1W, "-"] <> "+" <> StringPadRight["", col2W, "-"] <> "+" <> StringPadRight["", col3W, "-"] <> "+" <> StringPadRight["", col4W, "-"] <> "+";
        botLine = midLine;
        sepLine = midLine;
        titleLine = "| " <> padRight[title, col1W + col2W + col3W + col4W + 2] <> "|";
        headerLine = "| " <> padRight["Stage", col1W - 1] <> "| " <> padRight["FunKit", col2W - 1] <> "| " <> padRight["QMeS", col3W - 1] <> "| " <> padRight["DoFun", col4W - 1] <> "|";
        fmtRow[r_List] := "| " <> padRight[r[[1]], col1W - 1] <> "| " <> padRight[r[[2]], col2W - 1] <> "| " <> padRight[r[[3]], col3W - 1] <> "| " <> padRight[r[[4]], col4W - 1] <> "|";
        (* Print *)
        Print[topLine];
        Print[titleLine];
        Print[midLine];
        Print[headerLine];
        Print[midLine];
        Scan[
            If[# === "sep",
                Print[sepLine]
                ,
                Print[fmtRow[#]]
            ]&
            ,
            rows
        ];
        Print[botLine];
        Print[""];
    ];

(**********************************************************************************
    CSV Export
**********************************************************************************)

BenchmarkResultToCSVRows[bench_Association] :=
    Module[{title, res, isDSE, rows = {}, addRow},
        title = bench["Title"];
        res = bench["Results"];
        isDSE = TrueQ[bench["IsDSE"]];
        addRow[stage_String, fkKey_, qmesKey_, doFunKey_] :=
            AppendTo[
                rows
                ,
                {
                    title
                    ,
                    stage
                    ,
                    If[KeyExistsQ[res, fkKey],
                        res[fkKey]["Mean"]
                        ,
                        ""
                    ]
                    ,
                    If[KeyExistsQ[res, fkKey],
                        res[fkKey]["StdDev"]
                        ,
                        ""
                    ]
                    ,
                    If[KeyExistsQ[res, qmesKey],
                        res[qmesKey]["Mean"]
                        ,
                        ""
                    ]
                    ,
                    If[KeyExistsQ[res, qmesKey],
                        res[qmesKey]["StdDev"]
                        ,
                        ""
                    ]
                    ,
                    If[KeyExistsQ[res, doFunKey],
                        res[doFunKey]["Mean"]
                        ,
                        ""
                    ]
                    ,
                    If[KeyExistsQ[res, doFunKey],
                        res[doFunKey]["StdDev"]
                        ,
                        ""
                    ]
                }
            ];
        addRow["Full derivation", "FunKit-Full", "QMeS-Full", "DoFun-Full"];
        If[isDSE,
            addRow["FMakeDSE", "FunKit-FMakeDSE", None, None]
        ];
        addRow["FTakeDerivatives", "FunKit-FTakeDerivatives", None, None];
        addRow["FTruncate", "FunKit-FTruncate", None, None];
        addRow["FRoute", "FunKit-FRoute", None, None];
        rows
    ];

ExportBenchmarkCSV[allBenchmarks_List, outputDir_String] :=
    Module[{header, csvRows, csvString, filePath},
        If[!DirectoryQ[outputDir],
            CreateDirectory[outputDir]
        ];
        header = {"Title", "Stage", "FunKit_Mean", "FunKit_StdDev", "QMeS_Mean", "QMeS_StdDev", "DoFun_Mean", "DoFun_StdDev"};
        csvRows = Join @@ (BenchmarkResultToCSVRows /@ allBenchmarks);
        csvString =
            StringJoin[
                StringRiffle[ToString /@ header, ","] <> "\n"
                ,
                StringRiffle[
                    (
                            StringRiffle[
                                (
                                        If[StringQ[#],
                                            "\"" <> # <> "\""
                                            ,
                                            ToString[#]
                                        ]
                                    )& /@ #
                                ,
                                ","
                            ]
                        )& /@ csvRows
                    ,
                    "\n"
                ]
            ];
        filePath = FileNameJoin[{outputDir, "benchmark_" <> DateString[{"Year", "Month", "Day", "_", "Hour", "Minute"}] <> ".csv"}];
        Export[filePath, csvString, "Text"];
        Print["Results exported to: " <> filePath];
    ];
