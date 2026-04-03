(* ::Package:: *)

(**********************************************************************************
    Tests for TRACY module
    Covers: FIterativelySum, FDiagramSimplify, cos, FSetCacheDirectory,
            FClearTraceCache, and FORM-dependent tests (guarded)
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* Detect FORM availability for guarded tests *)
$FORMAvailable = Quiet[RunProcess[{"form", "-v"}]] =!= $Failed;

(**********************************************************************************
    cos: Orderless attribute and self-contraction
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        cos[p, p]
        ,
        1
        ,
        TestID -> "cos: self-contraction cos[p,p] is 1"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        cos[p2, p1] === cos[p1, p2]
        ,
        True
        ,
        TestID -> "cos: Orderless attribute ensures cos[p2,p1] === cos[p1,p2]"
    ]
];

(**********************************************************************************
    FIterativelySum: happy path
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result},
            (* FIterativelySum calls FFormSimplify internally; for numeric input
               the total should equal 15 regardless of intermediate simplification *)
            result = FIterativelySum[{1, 2, 3, 4, 5}];
            result === 15
        ]
        ,
        True
        ,
        TestID -> "FIterativelySum: sums list of numbers to 15"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        FIterativelySum[{42}]
        ,
        {42}
        ,
        TestID -> "FIterativelySum: single element returns {element}"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result},
            result = FIterativelySum[Range[8], 2];
            Length[result] === 2
        ]
        ,
        True
        ,
        TestID -> "FIterativelySum: with finalSize 2 returns list of length 2"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result},
            (* finalSize equal to input length should return input unchanged *)
            result = FIterativelySum[{1, 2, 3}, 3];
            result === {1, 2, 3}
        ]
        ,
        True
        ,
        TestID -> "FIterativelySum: finalSize equals input length returns input"
    ]
];

(**********************************************************************************
    FDiagramSimplify: collects by coupling structure
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{g, expr, result},
            (* Build a simple expression with symbolic coupling structure *)
            expr = g[1] * x + g[1] * y + g[2] * z;
            result = FDiagramSimplify[expr];
            (* The result should be simplified: g[1]*(x+y) + g[2]*z *)
            Expand[result - expr] === 0
        ]
        ,
        True
        ,
        TestID -> "FDiagramSimplify: result is algebraically equal to input"
    ]
];

(**********************************************************************************
    FMakeFormMomentumExpansion: initialization
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{},
            FMakeFormMomentumExpansion[];
            (* After init, the standard rules should be an empty list *)
            FunKit`Private`$standardFORMmomentumRules === {}
        ]
        ,
        True
        ,
        TestID -> "FMakeFormMomentumExpansion: init resets standard rules to empty list"
    ]
];

(**********************************************************************************
    FSetCacheDirectory: sets and normalizes path
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{},
            FSetCacheDirectory[FileNameJoin[{$TemporaryDirectory, "FunKitTestCache"}]];
            FunKit`Private`$TraceCacheDir === FileNameJoin[{$TemporaryDirectory, "FunKitTestCache"}] <> $PathnameSeparator
        ]
        ,
        True
        ,
        TestID -> "FSetCacheDirectory: appends trailing slash"
    ]
];

(* Restore default *)
FSetCacheDirectory[];

(**********************************************************************************
    FClearTraceCache: zero-arg creates fresh directory
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{cacheDir},
            FSetCacheDirectory[FileNameJoin[{$TemporaryDirectory, "FunKitTestCacheClear"}]];
            cacheDir = FunKit`Private`$TraceCacheDir;
            FClearTraceCache[];
            DirectoryQ[cacheDir]
        ]
        ,
        True
        ,
        TestID -> "FClearTraceCache: zero-arg recreates cache directory"
    ]
];

(* Restore default *)
FSetCacheDirectory[];

(**********************************************************************************
    FORM-dependent tests (guarded)
**********************************************************************************)

If[$FORMAvailable,

    (* FFormSimplify: simplifies a scalar expression *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{result},
                result = FFormSimplify[a + b + c];
                (* Should return a simplified expression, not $Failed or $Aborted *)
                result =!= $Failed && result =!= $Aborted
            ]
            ,
            True
            ,
            TestID -> "FFormSimplify: simplifies scalar expression (FORM)"
        ]
    ];

    (* FFormSimplify: caching — second call should return same result *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{result1, result2},
                result1 = FFormSimplify[x^2 + 2 x y + y^2];
                result2 = FFormSimplify[x^2 + 2 x y + y^2];
                result1 === result2
            ]
            ,
            True
            ,
            TestID -> "FFormSimplify: caching returns identical result (FORM)"
        ]
    ];

    (* FMakeP0Rule: temporal projection rule *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{rules},
                rules = FMakeP0Rule[{p1, p2}, {p10, p20}];
                (* Should produce replacement rules for vec[p1,0] and vec[p2,0] *)
                MatchQ[rules, {(_ -> _) ..}] && Length[rules] === 2
            ]
            ,
            True
            ,
            TestID -> "FMakeP0Rule: generates replacement rules for temporal projections (FORM)"
        ]
    ];

    (* FMakeSPFormRule: symmetric point FORM code *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{code},
                code = FMakeSPFormRule[{l1}, p, {p1, p2}];
                (* Should return a list of strings containing ProjSP *)
                MatchQ[code, {_String ..}] && StringContainsQ[code[[1]], "ProjSP"]
            ]
            ,
            True
            ,
            TestID -> "FMakeSPFormRule: generates FORM code with ProjSP (FORM)"
        ]
    ];

    (* FMakeFiniteTFormMomentumExpansion: returns FORM code *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{code},
                code = FMakeFiniteTFormMomentumExpansion[l1, p1];
                MatchQ[code, {_String ..}] && StringContainsQ[code[[1]], "ExpandFiniteT"]
            ]
            ,
            True
            ,
            TestID -> "FMakeFiniteTFormMomentumExpansion: generates FORM code with ExpandFiniteT (FORM)"
        ]
    ];

    (* FMakeSPFiniteTFormRule: combined SP + finite T *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{code},
                code = FMakeSPFiniteTFormRule[{l1}, p, {p1, p2}];
                MatchQ[code, {_String ..}] && StringContainsQ[code[[1]], "ProjSPFiniteT"]
            ]
            ,
            True
            ,
            TestID -> "FMakeSPFiniteTFormRule: generates FORM code with ProjSPFiniteT (FORM)"
        ]
    ];

    (* FMakeP0FormRule: temporal projection FORM code *)
    AppendTo[
        tests
        ,
        TestCreate[
            Module[{code},
                code = FMakeP0FormRule[{p1, p2}, {p10, p20}];
                MatchQ[code, {_String ..}] && StringContainsQ[code[[1]], "ProjP0"]
            ]
            ,
            True
            ,
            TestID -> "FMakeP0FormRule: generates FORM code with ProjP0 (FORM)"
        ]
    ];
];
