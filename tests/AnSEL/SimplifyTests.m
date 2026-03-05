(* ::Package:: *)

(**********************************************************************************
    Tests for AnSEL Simplify module
    Covers: FSimplify, TermsEqualAndSum, FTermContent, FMakeSymmetryList, BuildSymmetryList
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Section 1: FTermContent (grouping/hashing) — 4 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{t1, t2, setup},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            FunKit`Private`FTermContent[setup, t1] === FunKit`Private`FTermContent[setup, t2]
        ]
        ,
        True
        ,
        TestID -> "FTermContent: identical terms same hash"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{t1, t2, setup},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {a, b}], Propagator[{Phi, Phi}, {b, c}]];
            FunKit`Private`FTermContent[setup, t1] === FunKit`Private`FTermContent[setup, t2]
        ]
        ,
        True
        ,
        TestID -> "FTermContent: different index names same hash"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{t1, t2, setup},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]];
            t2 = FTerm[1, GammaN[{Phi, Phi, Phi, Phi}, {i1, i2, i3, i4}]];
            FunKit`Private`FTermContent[setup, t1] =!= FunKit`Private`FTermContent[setup, t2]
        ]
        ,
        True
        ,
        TestID -> "FTermContent: different object types different hash"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{t1, t2, setup},
            setup = GetFunKitSetupYukawa[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]];
            t2 = FTerm[1, Propagator[{Psi, Psibar}, {i1, i2}]];
            FunKit`Private`FTermContent[setup, t1] =!= FunKit`Private`FTermContent[setup, t2]
        ]
        ,
        True
        ,
        TestID -> "FTermContent: different field content different hash"
    ]
];

(**********************************************************************************
    Section 2: TermsEqualAndSum — bosonic terms — 6 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        FTerm[2, Propagator[{Phi, Phi}, {i2, i1}], Propagator[{Phi, Phi}, {i3, i2}]]
        ,
        TestID -> "TermsEqualAndSum: identical bosonic terms"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, b}], Propagator[{Phi, Phi}, {b, i3}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        FTerm[2, Propagator[{Phi, Phi}, {i2, i1}], Propagator[{Phi, Phi}, {i3, i2}]]
        ,
        TestID -> "TermsEqualAndSum: different index names"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[3, Propagator[{Phi, Phi}, {i1, i2}]];
            t2 = FTerm[2, Propagator[{Phi, Phi}, {i1, i2}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        FTerm[5, Propagator[{Phi, Phi}, {i2, i1}]]
        ,
        TestID -> "TermsEqualAndSum: different prefactors"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        False
        ,
        TestID -> "TermsEqualAndSum: different topology"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}], Propagator[{Phi, Phi}, {i3, i4}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i4}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        False
        ,
        TestID -> "TermsEqualAndSum: different closed index count"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}], GammaN[{Phi, Phi}, {-i3, -i1}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}], GammaN[{Phi, Phi}, {-i4, -i1}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        False
        ,
        TestID -> "TermsEqualAndSum: different open indices"
    ]
];

(**********************************************************************************
    Section 3: TermsEqualAndSum — fermionic terms (post-truncation) — 3 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupYukawa[];
            t1 = FTerm[1, Propagator[{Psi, Psibar}, {i1, i2}], GammaN[{Psi, Psibar, Phi}, {-i1, -i3, -i4}], GammaN[{Psi, Psibar, Phi}, {-i5, -i2, -i6}], Propagator[{Psi, Psibar}, {i3, i5}], Propagator[{Phi, Phi}, {i4, i6}]];
            t2 = FTerm[1, Propagator[{Psi, Psibar}, {a, b}], GammaN[{Psi, Psibar, Phi}, {-a, -c, -d}], GammaN[{Psi, Psibar, Phi}, {-e, -b, -f}], Propagator[{Psi, Psibar}, {c, e}], Propagator[{Phi, Phi}, {d, f}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        FTerm[2, Propagator[{Psi, Psibar}, {i1, i2}], GammaN[{Phi, Psibar, Psi}, {-i4, -i3, -i1}], GammaN[{Phi, Psibar, Psi}, {-i6, -i2, -i5}], Propagator[{Psi, Psibar}, {i3, i5}], Propagator[{Phi, Phi}, {i6, i4}]]
        ,
        TestID -> "TermsEqualAndSum: identical fermionic diagrams"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupYukawa[];
            t1 = FTerm[1, Propagator[{Psi, Psibar}, {i1, i2}], GammaN[{Psi, Psibar, Phi}, {-i1, -i3, -i4}], GammaN[{Psi, Psibar, Phi}, {-i5, -i2, -i6}], Propagator[{Psi, Psibar}, {i3, i5}], Propagator[{Phi, Phi}, {i4, i6}]];
            t2 = FTerm[-1, Propagator[{Psibar, Psi}, {b, a}], GammaN[{Psibar, Phi, Psi}, {-c, -d, -a}], GammaN[{Phi, Psi, Psibar}, {-f, -e, -b}], Propagator[{Psibar, Psi}, {e, c}], Propagator[{Phi, Phi}, {d, f}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        FTerm[2, Propagator[{Psi, Psibar}, {i1, i2}], GammaN[{Phi, Psibar, Psi}, {-i4, -i3, -i1}], GammaN[{Phi, Psibar, Psi}, {-i6, -i2, -i5}], Propagator[{Psi, Psibar}, {i3, i5}], Propagator[{Phi, Phi}, {i6, i4}]]
        ,
        TestID -> "TermsEqualAndSum: reordered fermionic fields"
    ]
];

(**********************************************************************************
    Section 4: FSimplify — bosonic simplification — 5 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupScalar[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1 === 2
        ]
        ,
        True
        ,
        TestID -> "FSimplify: scalar 2-point flow"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupScalar[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1
        ]
        ,
        4
        ,
        TestID -> "FSimplify: scalar 4-point flow"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result1, result2},
            setup = GetFunKitSetupScalar[];
            FSetGlobalSetup[setup];
            result1 =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
                FTruncate //
                FSimplify;
            result2 = FSimplify[setup, result1];
            Length[result1] === Length[result2]
        ]
        ,
        True
        ,
        TestID -> "FSimplify: idempotent"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, singleTerm, result},
            setup = GetFunKitSetupScalar[];
            singleTerm = FEx[FTerm[1, Propagator[{Phi, Phi}, {i2, i1}]]];
            FSimplify[setup, singleTerm]
        ]
        ,
        FEx[FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]]]
        ,
        TestID -> "FSimplify: single FTerm passthrough"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, emptyEx, result},
            setup = GetFunKitSetupScalar[];
            emptyEx = FEx[];
            FSimplify[setup, emptyEx]
        ]
        ,
        FEx[]
        ,
        TestID -> "FSimplify: empty FEx"
    ]
];

(**********************************************************************************
    Section 5: FSimplify — fermionic simplification (Yukawa theory) — 3 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupYukawa[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1
        ]
        ,
        2
        ,
        TestID -> "FSimplify Yukawa: fermion propagator flow has 2 terms (polarization with a boson, different regulator insertions)"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupYukawa[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1
        ]
        ,
        1
        ,
        TestID -> "FSimplify Yukawa: scalar propagator flow has 1 term (tadpole)"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupYukawa[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2], Phi[i3]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1
        ]
        ,
        3
        ,
        TestID -> "FSimplify Yukawa: Yukawa vertex flow (3 graphs, two fermion regulator insertions and one boson regulator insertion)"
    ]
];

(**********************************************************************************
    Section 6: FSimplify — four-fermion interactions — 5 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupFourFermion[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1
        ]
        ,
        1
        ,
        TestID -> "FSimplify 4F: fermion 2-point flow (only tadpole diagram)"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupFourFermion[];
            FSetGlobalSetup[setup];
            result =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}] //
                FTruncate //
                FSimplify;
            Length[result] - 1
        ]
        ,
        2
        ,
        TestID -> "FSimplify 4F: fermion 4-point flow term count (two diagrams with different topologies)"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result1, result2},
            setup = GetFunKitSetupFourFermion[];
            FSetGlobalSetup[setup];
            result1 =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}] //
                FTruncate //
                FSimplify;
            result2 = FSimplify[setup, result1];
            Length[result1] === Length[result2]
        ]
        ,
        True
        ,
        TestID -> "FSimplify 4F: fermion 4-point flow idempotent"
    ]
];

(**********************************************************************************
    Section 7: FMakeSymmetryList — 6 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, symmetries},
            setup = GetFunKitSetupScalar[];
            symmetries = FMakeSymmetryList[setup, {Phi[i1], Phi[i2]}];
            Length[symmetries]
        ]
        ,
        2
        ,
        TestID -> "FMakeSymmetryList: two identical bosons"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, symmetries},
            setup = GetFunKitSetupScalar[];
            symmetries = FMakeSymmetryList[setup, {Phi[i1], Phi[i2], Phi[i3]}];
            Length[symmetries]
        ]
        ,
        6
        ,
        TestID -> "FMakeSymmetryList: three identical bosons"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, symmetries},
            setup = GetFunKitSetupFourFermion[];
            symmetries = FMakeSymmetryList[setup, {Psi[i1], Psi[i2]}];
            Length[symmetries]
        ]
        ,
        2
        ,
        TestID -> "FMakeSymmetryList: two identical fermions"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, symmetries},
            setup = GetFunKitSetupYukawa[];
            symmetries = FMakeSymmetryList[setup, {Phi[i1], Phi[i2], Psi[i3], Psi[i4]}];
            Length[symmetries]
        ]
        ,
        4
        ,
        TestID -> "FMakeSymmetryList: mixed bosons and fermions"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, symmetries},
            setup = GetFunKitSetupYukawa[];
            symmetries = FMakeSymmetryList[setup, {Psi[i1], Psibar[i2]}];
            Length[symmetries]
        ]
        ,
        1
        ,
        TestID -> "FMakeSymmetryList: different species no symmetry"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        CheckAbort[
            Module[{setup},
                setup = GetFunKitSetupScalar[];
                FMakeSymmetryList[setup, {Phi[i1], Phi[i2]}, {i1}]
            ]
            ,
            "AbortTriggered"
        ]
        ,
        "AbortTriggered"
        ,
        TestID -> "FMakeSymmetryList: mismatched lengths aborts"
    ]
];

(**********************************************************************************
    Section 8: BuildSymmetryList — 4 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, setup},
            setup = GetFunKitSetupScalar[];
            result = FunKit`Private`BuildSymmetryList[setup, {{{1, 2}, 1}}, {Phi[i1], Phi[i2]}];
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "BuildSymmetryList: valid 2-cycle"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, setup},
            setup = GetFunKitSetupScalar[];
            result = FunKit`Private`BuildSymmetryList[setup, {{{1, 2, 3}, 1}}, {Phi[i1], Phi[i2], Phi[i3]}];
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "BuildSymmetryList: valid 3-cycle"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        CheckAbort[
            Module[{result, setup},
                setup = GetFunKitSetupScalar[];
                FunKit`Private`BuildSymmetryList[setup, {{{1, 5}, 1}}, {Phi[i1], Phi[i2]}]
            ]
            ,
            "AbortTriggered"
        ]
        ,
        "AbortTriggered"
        ,
        TestID -> "BuildSymmetryList: invalid cycle aborts"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FunKit`Private`BuildSymmetryList[setup, {}, {Phi[i1], Phi[i2]}]
        ]
        ,
        {}
        ,
        TestID -> "BuildSymmetryList: empty symmetries"
    ]
];

(**********************************************************************************
    Section 9: FSimplify with explicit symmetries — 2 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, resultNoSym, resultWithSym},
            setup = GetFunKitSetupScalar[];
            FSetGlobalSetup[setup];
            $AutoBuildSymmetryList = False;
            resultNoSym =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
                FTruncate //
                FSimplify;
            $AutoBuildSymmetryList = True;
            resultWithSym =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
                FTruncate //
                FSimplify;
            Length[resultWithSym] <= Length[resultNoSym]
        ]
        ,
        True
        ,
        TestID -> "FSimplify with symmetries: bosonic 4-point"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, resultNoSym, resultWithSym},
            setup = GetFunKitSetupFourFermion[];
            FSetGlobalSetup[setup];
            $AutoBuildSymmetryList = False;
            resultNoSym =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}] //
                FTruncate //
                FSimplify;
            $AutoBuildSymmetryList = True;
            resultWithSym =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}] //
                FTruncate //
                FSimplify;
            Length[resultWithSym] <= Length[resultNoSym]
        ]
        ,
        True
        ,
        TestID -> "FSimplify with symmetries: fermionic 4-point"
    ]
];
