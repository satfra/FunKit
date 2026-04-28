tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

testSetup = GetFunKitSetupScalar[];

(**********************************************************************************
    DiANE: AssertFSetup tests (D4)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FPlot[42, FTerm[1]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D4: FPlot FTerm with non-Association setup should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FPlot["not a setup", FEx[FTerm[1]]], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`Private`FSetupQ::association},
    TestID -> "D4: FPlot FEx with non-Association setup should abort"
]];

(**********************************************************************************
    DiANE: FAddTexStyles / FSetTexStyles validation (H6, E5, E6)
**********************************************************************************)

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddTexStyles[Phi -> 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FAddTexStyles::invalidRule},
    TestID -> "FAddTexStyles with non-string value should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetTexStyles[Phi -> 42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FSetTexStyles::invalidRule},
    TestID -> "H6: FSetTexStyles with non-string value should abort (using correct symbol)"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FAddTexStyles[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E5: FAddTexStyles with non-rule arg should abort"
]];

AppendTo[tests, VerificationTest[
    CheckAbort[FunKit`FSetTexStyles[42], "AbortTriggered"],
    "AbortTriggered",
    {FunKit`FunKit::invalidArguments},
    TestID -> "E6: FSetTexStyles with non-rule arg should abort"
]];

(* Test that FSetTexStyles[] (no args) works without error *)
AppendTo[tests, VerificationTest[
    FunKit`FSetTexStyles[],
    Null,
    TestID -> "FSetTexStyles[] should clear styles without error"
]];

(* Test that valid rules work *)
AppendTo[tests, VerificationTest[
    (FunKit`FAddTexStyles[Phi -> "\\phi"]; True),
    True,
    TestID -> "FAddTexStyles with valid rule should succeed"
]];

AppendTo[tests, VerificationTest[
    (FunKit`FSetTexStyles[Phi -> "\\phi"]; True),
    True,
    TestID -> "FSetTexStyles with valid rule should succeed"
]];

(**********************************************************************************
    DiANE: DiagramStyling VertexStyles / VertexSizes overrides
**********************************************************************************)

(* A minimal tadpole-like FTerm with a 4-point GammaN vertex and one propagator loop. *)
tadpoleFTerm = FTerm[1/2,
    Propagator[{Phi, Phi}, {-i1, -i2}],
    GammaN[{Phi, Phi, Phi, Phi}, {i1, i2, -ex1, -ex2}]
];

AppendTo[tests, VerificationTest[
    Module[{marker, s, graph, shapeRules},
        marker = Graphics[{Red, Disk[{0, 0}, 1]}];
        s = Append[testSetup, "DiagramStyling" -> <|
            "VertexStyles" -> {GammaN -> marker}
        |>];
        graph = FunKit`Private`FGetDiagram[s, tadpoleFTerm][[2]];
        shapeRules = VertexShape /. Options[graph, VertexShape];
        MemberQ[shapeRules, _ -> marker]
    ],
    True,
    TestID -> "DiagramStyling: VertexStyles overrides built-in GammaN shape"
]];

AppendTo[tests, VerificationTest[
    Module[{s, graph, sizeRules},
        s = Append[testSetup, "DiagramStyling" -> <|
            "VertexSizes" -> {GammaN -> 0.42}
        |>];
        graph = FunKit`Private`FGetDiagram[s, tadpoleFTerm][[2]];
        sizeRules = VertexSize /. Options[graph, VertexSize];
        MemberQ[sizeRules, _ -> 0.42]
    ],
    True,
    TestID -> "DiagramStyling: VertexSizes overrides built-in GammaN size"
]];

AppendTo[tests, VerificationTest[
    Module[{graph, sizeRules},
        (* No DiagramStyling at all: built-in GammaN size (0.15) must still apply. *)
        graph = FunKit`Private`FGetDiagram[testSetup, tadpoleFTerm][[2]];
        sizeRules = VertexSize /. Options[graph, VertexSize];
        MemberQ[sizeRules, _ -> 0.15]
    ],
    True,
    TestID -> "DiagramStyling: built-in VertexSize fallback preserved when key absent"
]];
