(* 
  Test.m
  This script discovers and runs all Mathematica test files ending in "Tests.m" 
  in its directory.
*)

(* Hide styling if we are in a CLI context. *)

If[$FrontEnd === Null,
    Unprotect[Style];
    Unprotect[StyleBox];
    Unprotect[Print];
    Style[expr_, opts___] := expr;
    StyleBox[expr_, opts___] := expr;
    Print[expr__] := Write[$Output, StringJoin[ToString[#, OutputForm
        ]& /@ {expr}]];
    Protect[Print];
    Protect[StyleBox];
    Protect[Style];
];

(* Perform user-specified initialization tasks *)

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

(* Function to run and report tests *)

RunAndReportTests[tests_List, testFileName_String] :=
    Module[{result, successCount, failureCount, mGreen = RGBColor[0.0235294,
         0.235294, 0.0235294], mRed = RGBColor[0.435294, 0, 0]},
        Print["Running tests from: " <> testFileName];
        result = TestReport[tests];
        successCount = Length[result["TestsSucceededKeys"]];
        failureCount = Length[result["TestsFailedWrongResultsKeys"]];
            
        Print[Style["  ✓ " <> ToString[successCount] <> " passed", mGreen
            ], "    ", Style["x " <> ToString[failureCount] <> " failed", mRed]];
            
        If[successCount > 0,
            Print["\n", Style["  Successful Tests Details:", mGreen, 
                Bold]];
            Scan[(Print["\n", Style["  Test:", mGreen, Bold], " ", #[
                "TestID"]];)&, Values[KeyTake[result["TestResults"], result["TestsSucceededKeys"
                ]]]]
        ];
        If[failureCount > 0,
            Print["\n", Style["  Failed Tests Details:", mRed, Bold]]
                ;
            Scan[
                (
                    Print["\n", Style["  Test:", mRed, Bold], " ", #[
                        "TestID"]];
                    Print["    Expected: ", #["ExpectedOutput"]];
                    Print["    Actual:   ", #["ActualOutput"]];
                )&
                ,
                Values[KeyTake[result["TestResults"], result["TestsFailedWrongResultsKeys"
                    ]]]
            ]
        ];
        Return[{successCount, failureCount}];
    ];

(* Main script execution logic *)

Module[{testFiles, totalSuccesses = 0, totalFailures = 0, mOrange = RGBColor[
    0.8, 0.4, 0], mRed = RGBColor[0.435294, 0, 0], mGreen = RGBColor[0.0235294,
     0.235294, 0.0235294]},
    AppendTo[$Path, DirectoryName[$InputFileName]];
    AppendTo[$Path, FileNameJoin[{DirectoryName[$InputFileName], "..",
         "modules"}]];
    testFiles = FileNames["*Tests.m", DirectoryName[$InputFileName]];
        
    Print[Style["Discovering and running tests...", Bold, mOrange]];
    Print[Style["---------------------------------", Bold, mOrange]];
        
    Scan[
        (
            Get[#];
            If[ValueQ[tests],
                Module[{results},
                    results = RunAndReportTests[tests, FileNameTake[#
                        ]];
                    totalSuccesses += results[[1]];
                    totalFailures += results[[2]];
                    Print[""]; (* newline separator *)
                ]
                ,
                (
                    Print["  ERROR: Test file ", FileNameTake[#], " does not define a 'tests' variable."
                        ];
                    totalFailures++;
                )
            ]
        )&
        ,
        testFiles
    ];
    Print[Style["---------------------------------", Bold, mOrange]];
        
    Print[Style["Overall Test Summary", Bold, mOrange]];
    Print[Style["---------------------------------", Bold, mOrange]];
        
    Print[Style["✓ " <> ToString[totalSuccesses] <> " passed", mGreen,
         Bold], "    ", Style["x " <> ToString[totalFailures] <> " failed", mRed,
         Bold]];
    Print[Style["---------------------------------", Bold, mOrange]];
        
    Return[totalFailures];
];
