(* ::Package:: *)

(**********************************************************************************
    Tests for AnSEL Simplify module
    Covers: FSimplify, TermsEqualAndSum, FTermContent, FMakeSymmetryList, FBuildSymmetryList
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
        Module[{setup, t1, t2, res},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            res = FunKit`Private`TermsEqualAndSum[setup, t1, t2];
            FunKit`Private`NormalizeSuperIndices[setup, res]
        ]
        ,
        FTerm[2, Propagator[{Phi, Phi}, {sIdx1, i1}], Propagator[{Phi, Phi}, {i3, sIdx1}]]
        ,
        TestID -> "TermsEqualAndSum: identical bosonic terms"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2, res},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i1, b}], Propagator[{Phi, Phi}, {b, i3}]];
            res = FunKit`Private`TermsEqualAndSum[setup, t1, t2];
            FunKit`Private`NormalizeSuperIndices[setup, res]
        ]
        ,
        FTerm[2, Propagator[{Phi, Phi}, {sIdx1, i1}], Propagator[{Phi, Phi}, {i3, sIdx1}]]
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
        Module[{setup, t1, t2, res},
            setup = GetFunKitSetupYukawa[];
            t1 = FTerm[1, Propagator[{Psi, Psibar}, {i1, i2}], GammaN[{Psi, Psibar, Phi}, {-i1, -i3, -i4}], GammaN[{Psi, Psibar, Phi}, {-i5, -i2, -i6}], Propagator[{Psi, Psibar}, {i3, i5}], Propagator[{Phi, Phi}, {i4, i6}]];
            t2 = FTerm[1, Propagator[{Psi, Psibar}, {a, b}], GammaN[{Psi, Psibar, Phi}, {-a, -c, -d}], GammaN[{Psi, Psibar, Phi}, {-e, -b, -f}], Propagator[{Psi, Psibar}, {c, e}], Propagator[{Phi, Phi}, {d, f}]];
            res = FunKit`Private`TermsEqualAndSum[setup, t1, t2];
            FunKit`Private`NormalizeSuperIndices[setup, res]
        ]
        ,
        FTerm[2, Propagator[{Psi, Psibar}, {sIdx1, sIdx2}], GammaN[{Phi, Psibar, Psi}, {-sIdx3, -sIdx4, -sIdx1}], GammaN[{Phi, Psibar, Psi}, {-sIdx5, -sIdx2, -sIdx6}], Propagator[{Psi, Psibar}, {sIdx4, sIdx6}], Propagator[{Phi, Phi}, {sIdx5, sIdx3}]]
        ,
        TestID -> "TermsEqualAndSum: identical fermionic diagrams"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2, res},
            setup = GetFunKitSetupYukawa[];
            t1 = FTerm[1, Propagator[{Psi, Psibar}, {i1, i2}], GammaN[{Psi, Psibar, Phi}, {-i1, -i3, -i4}], GammaN[{Psi, Psibar, Phi}, {-i5, -i2, -i6}], Propagator[{Psi, Psibar}, {i3, i5}], Propagator[{Phi, Phi}, {i4, i6}]];
            t2 = FTerm[-1, Propagator[{Psibar, Psi}, {b, a}], GammaN[{Psibar, Phi, Psi}, {-c, -d, -a}], GammaN[{Phi, Psi, Psibar}, {-f, -e, -b}], Propagator[{Psibar, Psi}, {e, c}], Propagator[{Phi, Phi}, {d, f}]];
            res = FunKit`Private`TermsEqualAndSum[setup, t1, t2];
            FunKit`Private`NormalizeSuperIndices[setup, res]
        ]
        ,
        FTerm[2, Propagator[{Psi, Psibar}, {sIdx1, sIdx2}], GammaN[{Phi, Psibar, Psi}, {-sIdx3, -sIdx4, -sIdx1}], GammaN[{Phi, Psibar, Psi}, {-sIdx5, -sIdx2, -sIdx6}], Propagator[{Psi, Psibar}, {sIdx4, sIdx6}], Propagator[{Phi, Phi}, {sIdx5, sIdx3}]]
        ,
        TestID -> "TermsEqualAndSum: reordered fermionic fields"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupFourFermion[];
            t1 = FTerm[GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i3}], Propagator[{Psi, Psibar}, {i2, i4}]];
            t2 = FTerm[GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i4}], Propagator[{Psi, Psibar}, {i2, i3}]];
            FunKit`Private`TermsEqualAndSum[setup, t1, t2]
        ]
        ,
        FTerm[0]
        ,
        TestID -> "FSimplify 4F: Identify two tadpoles with switched legs correctly (expect FEx[] after simplification)"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2, res},
            setup = GetFunKitSetupFourFermion[];
            t1 = FTerm[GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i3}], Propagator[{Psi, Psibar}, {i2, i4}]];
            t2 = FTerm[-1, GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i4}], Propagator[{Psi, Psibar}, {i2, i3}]];
            res = FunKit`Private`TermsEqualAndSum[setup, t1, t2];
            FunKit`Private`NormalizeSuperIndices[setup, res]
        ]
        ,
        FTerm[2, GammaN[{Psibar, Psibar, Psi, Psi}, {-sIdx1, -sIdx2, -sIdx3, -sIdx4}], Propagator[{Psi, Psibar}, {sIdx2, sIdx4}], Propagator[{Psi, Psibar}, {sIdx1, sIdx3}]]
        ,
        TestID -> "FSimplify 4F: Identify two tadpoles with switched legs correctly (expect NOT FEx[] after simplification)"
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
            Length[result] - 1
        ]
        ,
        2
        ,
        TestID -> "FSimplify: scalar 2-point flow has 2 terms (tadpole and polarization)"
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
        TestID -> "FSimplify: scalar 4-point flow has 4 terms (1 diagram with two 4-point vertices, 2 diagrams with one 4-point vertex, 1 diagram with no 4-point vertices)"
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
        FEx[FTerm[1, Propagator[{Phi, Phi}, {i2, i1}]]]
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
        Module[{setup, eq1, eq2},
            setup = GetFunKitSetupFourFermion[];
            eq1 = FTerm[GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i3}], Propagator[{Psi, Psibar}, {i2, i4}]];
            eq2 = FTerm[GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i4}], Propagator[{Psi, Psibar}, {i2, i3}]];
            FSimplify[setup, FEx[eq1, eq2]]
        ]
        ,
        FEx[]
        ,
        TestID -> "FSimplify 4F: Identify two tadpoles with switched legs correctly (expect FEx[] after simplification)"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, eq1, eq2},
            setup = GetFunKitSetupFourFermion[];
            eq1 = FTerm[GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i3}], Propagator[{Psi, Psibar}, {i2, i4}]];
            eq2 = FTerm[-1, GammaN[{Psibar, Psibar, Psi, Psi}, {-i1, -i2, -i3, -i4}], Propagator[{Psi, Psibar}, {i1, i4}], Propagator[{Psi, Psibar}, {i2, i3}]];
            FSimplify[setup, FEx[eq1, eq2]] =!= FEx[]
        ]
        ,
        True
        ,
        TestID -> "FSimplify 4F: Identify two tadpoles with switched legs correctly (expect NOT FEx[] after simplification)"
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
    Section 8: FBuildSymmetryList — 4 tests
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, setup},
            setup = GetFunKitSetupScalar[];
            result = FunKit`Private`FBuildSymmetryList[setup, {{{1, 2}, 1}}, {Phi[i1], Phi[i2]}];
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FBuildSymmetryList: valid 2-cycle"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, setup},
            setup = GetFunKitSetupScalar[];
            result = FunKit`Private`FBuildSymmetryList[setup, {{{1, 2, 3}, 1}}, {Phi[i1], Phi[i2], Phi[i3]}];
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FBuildSymmetryList: valid 3-cycle"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        CheckAbort[
            Module[{result, setup},
                setup = GetFunKitSetupScalar[];
                FunKit`Private`FBuildSymmetryList[setup, {{{1, 5}, 1}}, {Phi[i1], Phi[i2]}]
            ]
            ,
            "AbortTriggered"
        ]
        ,
        "AbortTriggered"
        ,
        TestID -> "FBuildSymmetryList: invalid cycle aborts"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FunKit`Private`FBuildSymmetryList[setup, {}, {Phi[i1], Phi[i2]}]
        ]
        ,
        {}
        ,
        TestID -> "FBuildSymmetryList: empty symmetries"
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
            $AutoFBuildSymmetryList = False;
            resultNoSym =
                FTakeDerivatives[setup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
                FTruncate //
                FSimplify;
            $AutoFBuildSymmetryList = True;
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
            $AutoFBuildSymmetryList = False;
            resultNoSym =
                FTakeDerivatives[setup, WetterichEquation, {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}] //
                FTruncate //
                FSimplify;
            $AutoFBuildSymmetryList = True;
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

(**********************************************************************************
    Section 10: FSimplify — disconnected diagrams — 3 tests

    Disconnected diagrams have multiple groups of objects with no shared
    closed superindices. FSimplify's graph walk only visits one connected
    component, so its results on disconnected input are unreliable.
    These tests document this limitation.
**********************************************************************************)

(* Two identical disconnected diagrams (index-renamed) — FSimplify should
   merge them to a single term with prefactor 2, but the walk may not
   traverse the second component and thus may fail to recognise equality. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2, result},
            setup = GetFunKitSetupScalar[];
            (* Two separate tadpole loops — no shared index between the two Propagator-GammaN pairs *)
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi}, {a1, a2}], Propagator[{Phi, Phi}, {a3, a4}], GammaN[{Phi, Phi}, {a3, a4}]];
            result = Quiet[FSimplify[setup, FEx[t1, t2]], FSimplify::disconnected];
            (* Disconnected terms are skipped — both kept as-is *)
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FSimplify disconnected: two identical disconnected diagrams are skipped"
    ]
];

(* A disconnected diagram and a connected diagram with the same object types
   must NOT be merged — they have different topology. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, disconnected, connected, result},
            setup = GetFunKitSetupScalar[];
            (* Disconnected: two separate tadpole loops *)
            disconnected = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]];
            (* Connected: chain linking all four objects via shared indices *)
            connected = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i2, i3}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i4, i1}]];
            result = Quiet[FSimplify[setup, FEx[disconnected, connected]], FSimplify::disconnected];
            (* Disconnected term skipped, connected term simplified — both remain *)
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FSimplify disconnected: disconnected vs connected must not merge"
    ]
];

(* Two disconnected diagrams that share one component but differ in the other.
   These must NOT be merged. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2, result},
            setup = GetFunKitSetupScalar[];
            (* Both share a Propagator-GammaN 2-point loop, but the second component differs *)
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi, Phi, Phi}, {a3, a4, a5, a6}], Propagator[{Phi, Phi}, {a3, a4}], Propagator[{Phi, Phi}, {a5, a6}]];
            result = Quiet[FSimplify[setup, FEx[t1, t2]], FSimplify::disconnected];
            (* Both disconnected terms skipped — both remain *)
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FSimplify disconnected: different second component must not merge"
    ]
];

(**********************************************************************************
    Section 11: FSimplify — non-loop and loop term cancellation — 2 tests
**********************************************************************************)

(* Bug 1: Non-loop terms with implicit vs explicit coefficient should cancel *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, result},
            setup = GetFunKitSetupScalar[];
            result = FSimplify[setup, FEx[FTerm[S[{Phi, Phi}, {-i2, -i1}]], FTerm[-1, S[{Phi, Phi}, {-i2, -i1}]]]];
            result === FEx[]
        ]
        ,
        True
        ,
        TestID -> "FSimplify: non-loop terms with implicit coefficient cancel"
    ]
];

(* Bug 2: Loop terms with identical topology but different index naming
   should be identified and cancel.
   Both have: S[AAAA] + 3 Propagator[AA] + 2 GammaN[AAA], same topology,
   but open indices at different positions within all-A vertices. *)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{setup, t1, t2, result},
            setup = GetFunKitSetupYangMills[];
            t1 = FTerm[1/2, S[{A, A, A, A}, {-i2, -a903, -a904, -a905}],
                Propagator[{A, A}, {a903, a906}],
                GammaN[{A, A, A}, {-i1, -a907, -a906}],
                Propagator[{A, A}, {a908, a907}],
                Propagator[{A, A}, {a904, a909}],
                GammaN[{A, A, A}, {-a910, -a909, -a908}],
                Propagator[{A, A}, {a905, a910}]];
            t2 = FTerm[-(1/2), S[{A, A, A, A}, {-i911, -i912, -i913, -i2}],
                Propagator[{A, A}, {i914, i911}],
                GammaN[{A, A, A}, {-i915, -i914, -i1}],
                Propagator[{A, A}, {i916, i915}],
                Propagator[{A, A}, {i917, i912}],
                GammaN[{A, A, A}, {-i918, -i917, -i916}],
                Propagator[{A, A}, {i913, i918}]];
            result = FSimplify[setup, FEx[t1, t2]];
            result === FEx[]
        ]
        ,
        True
        ,
        TestID -> "FSimplify: loop terms with branching and different index naming cancel"
    ]
];
