(* ::Package:: *)

(**********************************************************************************
    Tests for user-supplied symmetries.

    Covers: FSymmetry, FMakeSymmetryList[s1, s2, ...], FSymmetrise, FCheckSymmetry,
    and the fact that $AutoBuildSymmetryList defaults to False so that the output of
    a derivation is exact for any consumer.

    Background: reducing with the full permutation group of a correlator is only
    correct if the contraction the user applies is covariant under every element of
    it. The group is a property of the correlation function, not of an individual
    diagram, so the reduced expression equals the original only after symmetrisation.
    See SYMMETRY-REDUCTION-DESIGN.md and scripts/fsimplify_symmetry_bug.m.
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

identitySym = <|"Rule" -> {}, "Factor" -> 1|>;

nTerms[ex_] := Count[List @@ ex, _FTerm];

(**********************************************************************************
    Section 1: FSymmetry lowering
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}]]
        ,
        {identitySym, <|"Rule" -> {i1 -> i2, i2 -> i1}, "Factor" -> 1|>}
        ,
        TestID -> "FSymmetry: Symmetric two-cycle lowers to factor +1"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[FSymmetry[Antisymmetric, {i1, i2}]]
        ,
        {identitySym, <|"Rule" -> {i1 -> i2, i2 -> i1}, "Factor" -> -1|>}
        ,
        TestID -> "FSymmetry: Antisymmetric two-cycle lowers to factor -1"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}, {i3, i4}]]
        ,
        {identitySym, <|"Rule" -> {i1 -> i2, i2 -> i1, i3 -> i4, i4 -> i3}, "Factor" -> 1|>}
        ,
        TestID -> "FSymmetry: two disjoint cycles are applied simultaneously"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2, i3}]]
        ,
        {identitySym, <|"Rule" -> {i1 -> i2, i2 -> i3, i3 -> i1}, "Factor" -> 1|>}
        ,
        TestID -> "FSymmetry: three-cycle i1->i2->i3->i1"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[FSymmetry[-1, {i1, i2}]]
        ,
        {identitySym, <|"Rule" -> {i1 -> i2, i2 -> i1}, "Factor" -> -1|>}
        ,
        TestID -> "FSymmetry: an explicit numeric factor is accepted"
    ]
];

(**********************************************************************************
    Section 2: FMakeSymmetryList assembly
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Count[FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}], FSymmetry[Antisymmetric, {i3, i4}]], identitySym]
        ,
        1
        ,
        TestID -> "FMakeSymmetryList: the identity is added exactly once"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}], FSymmetry[Symmetric, {i1, i2}]]
        ,
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}]]
        ,
        TestID -> "FMakeSymmetryList: duplicate symmetries are removed"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        FMakeSymmetryList[{FSymmetry[Symmetric, {i1, i2}], FSymmetry[Antisymmetric, {i3, i4}]}]
        ,
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}], FSymmetry[Antisymmetric, {i3, i4}]]
        ,
        TestID -> "FMakeSymmetryList: the list form agrees with the sequence form"
    ]
];

(*The hand-written group must reproduce exactly what the field-based constructor builds.*)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupFourFermion[];
            Sort @ FMakeSymmetryList[setup, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}] ===
                Sort @ FMakeSymmetryList[
                    FSymmetry[Antisymmetric, {i1, i3}],
                    FSymmetry[Antisymmetric, {i2, i4}],
                    FSymmetry[Symmetric, {i1, i3}, {i2, i4}]]
        ]
        ,
        True
        ,
        TestID -> "FMakeSymmetryList: hand-written group equals the auto-built one"
    ]
];

(**********************************************************************************
    Section 3: input validation
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        CheckAbort[FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}, {i2, i3}]], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        {FSymmetry::notDisjoint}
        ,
        TestID -> "FSymmetry: overlapping cycles abort"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        CheckAbort[FMakeSymmetryList[FSymmetry[Symmetric, {i1}]], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        {FSymmetry::badCycle}
        ,
        TestID -> "FSymmetry: a cycle of length one aborts"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        CheckAbort[FMakeSymmetryList[FSymmetry[NotASymmetry, {i1, i2}]], "AbortTriggered"]
        ,
        "AbortTriggered"
        ,
        {FSymmetry::badFactor}
        ,
        TestID -> "FSymmetry: an unknown head aborts"
    ]
];

(**********************************************************************************
    Section 4: the default is exact, symmetries are opt-in
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        TrueQ[FunKit`Private`$AutoBuildSymmetryList]
        ,
        False
        ,
        TestID -> "$AutoBuildSymmetryList defaults to False"
    ]
];

(*The scalar four-point flow: 33 diagrams exactly, 4 once the full permutation group of
  the four identical external legs is declared.*)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FSetGlobalSetup[setup];
            nTerms[FTruncate[FTakeDerivatives[setup, WetterichEquation,
                {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}]]] >
                nTerms[FTruncate[FTakeDerivatives[setup, WetterichEquation,
                    {Phi[i1], Phi[i2], Phi[i3], Phi[i4]},
                    "Symmetries" -> FMakeSymmetryList[setup, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}]]]]
        ]
        ,
        True
        ,
        TestID -> "Declaring symmetries reduces the diagram count; the default does not"
    ]
];

(**********************************************************************************
    Section 5: FSymmetrise / FCheckSymmetry

    The regression for the bug this whole mechanism exists for: a symmetry-reduced
    expression is NOT equal to the exact one, and symmetrising it recovers the exact
    one. Equality is decided with FSimplify without symmetries, which is exact.
**********************************************************************************)

fourFermionCase[] :=
    Module[{setup, syms, exact, reduced},
        setup = GetFunKitSetupFourFermion[];
        FSetGlobalSetup[setup];
        syms = FMakeSymmetryList[setup, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}];
        exact = FTruncate[FTakeDerivatives[setup, WetterichEquation,
            {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}]];
        reduced = FTruncate[FTakeDerivatives[setup, WetterichEquation,
            {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}, "Symmetries" -> syms]];
        {setup, syms, FEx @@ Cases[List @@ exact, _FTerm], FEx @@ Cases[List @@ reduced, _FTerm]}
    ];

exprsEqualQ[setup_, a_, b_] :=
    Length @ FSimplify[setup,
        FEx @@ Join[List @@ a, (FTerm[-1] ** #)& /@ (List @@ b)]] === 0;

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, syms, exact, reduced},
            {setup, syms, exact, reduced} = fourFermionCase[];
            FCheckSymmetry[setup, exact, syms]
        ]
        ,
        True
        ,
        TestID -> "FCheckSymmetry: the exact derivative has the correlator's symmetry"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, syms, exact, reduced},
            {setup, syms, exact, reduced} = fourFermionCase[];
            exprsEqualQ[setup, exact, reduced]
        ]
        ,
        False
        ,
        TestID -> "A symmetry-reduced expression is NOT equal to the exact one"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, syms, exact, reduced},
            {setup, syms, exact, reduced} = fourFermionCase[];
            exprsEqualQ[setup, exact,
                FEx @@ Cases[List @@ FSymmetrise[setup, reduced, syms], _FTerm]]
        ]
        ,
        True
        ,
        TestID -> "FSymmetrise recovers the exact expression from the reduced one"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, syms, exact, reduced},
            {setup, syms, exact, reduced} = fourFermionCase[];
            FCheckSymmetry[setup, reduced, syms]
        ]
        ,
        False
        ,
        TestID -> "FCheckSymmetry: a symmetry-reduced expression no longer has the symmetry"
    ]
];

tests
