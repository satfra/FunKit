(* ::Package:: *)

(**********************************************************************************
    Tests for FEDeriK Compatibility module
    Covers: QMeSForm, DoFunForm, FunKitForm for superindex and routed formats
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

testSetup = GetFunKitSetupScalar[];

testSetupYukawa = GetFunKitSetupYukawa[];

(**********************************************************************************
    D4: DoFunForm superindex conversion (moved from RobustnessTests.m)
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, Propagator[{Phi, Phi}, {i1, i2}]], DoFun`DoDSERGE`P[{Phi, i1}, {Phi, i2}], TestID -> "D4a: DoFunForm single Propagator"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]], DoFun`DoDSERGE`V[{Phi, i1}, {Phi, i2}, {Phi, i3}], TestID -> "D4b: DoFunForm GammaN to V"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, S[{Phi, Phi}, {i1, i2}]], DoFun`DoDSERGE`S[{Phi, -i1}, {Phi, -i2}], TestID -> "D4c: DoFunForm S with index sign flip"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, Rdot[{Phi, Phi}, {i1, i2}]], DoFun`DoDSERGE`dR[{Phi, i1}, {Phi, i2}], TestID -> "D4d: DoFunForm Rdot to dR"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {i3, i4}]]], 1/2 * DoFun`DoDSERGE`op[DoFun`DoDSERGE`P[{Phi, i1}, {Phi, i2}], DoFun`DoDSERGE`dR[{Phi, i3}, {Phi, i4}]], TestID -> "D4e: DoFunForm FTerm with coefficient"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, FEx[FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]], FTerm[-1, GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]]]], DoFun`DoDSERGE`op[DoFun`DoDSERGE`P[{Phi, i1}, {Phi, i2}]] - DoFun`DoDSERGE`op[DoFun`DoDSERGE`V[{Phi, i1}, {Phi, i2}, {Phi, i3}]], TestID -> "D4f: DoFunForm FEx to Plus"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, FEx[]], 0, TestID -> "D4g: DoFunForm empty FEx"]];

(**********************************************************************************
    E1: QMeSForm superindex -> QMeS native Association format
**********************************************************************************)

(* Note: QMeS canonical ordering "c>ag>g" reverses index order for {Phi,Phi} *)

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, Propagator[{Phi, Phi}, {i1, i2}]], {1, <|"type" -> "Propagator", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>}, TestID -> "E1a: QMeSForm superindex Propagator"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, Rdot[{Phi, Phi}, {i1, i2}]], {1, <|"type" -> "Regulatordot", "indices" -> {{Phi, {-i2}}, {Phi, {-i1}}}|>}, TestID -> "E1b: QMeSForm superindex Rdot (negated indices)"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]], {1, <|"type" -> "nPoint", "indices" -> {{Phi, {-i3}}, {Phi, {-i2}}, {Phi, {-i1}}}, "nPoint" -> 3, "spec" -> "none"|>}, TestID -> "E1c: QMeSForm superindex GammaN"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, S[{Phi, Phi}, {i1, i2}]], {1, <|"type" -> "nPoint", "indices" -> {{Phi, {-i2}}, {Phi, {-i1}}}, "nPoint" -> 2, "spec" -> "classical"|>}, TestID -> "E1d: QMeSForm superindex S"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]], {"Prefactor" -> {1/2}, <|"type" -> "Propagator", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>, <|"type" -> "Regulatordot", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>}, TestID -> "E1e: QMeSForm superindex FTerm"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]]], {{"Prefactor" -> {1/2}, <|"type" -> "Propagator", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>, <|"type" -> "Regulatordot", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>}}, TestID -> "E1f: QMeSForm superindex FEx"]];

(**********************************************************************************
    E2: QMeSForm routed -> QMeS named-symbol format
**********************************************************************************)

(* Note: QMeS canonical ordering reverses for {Phi,Phi} *)

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, Propagator[{Phi, Phi}, {{l1}, {-l1}}]], Symbol["GPhiPhi"][{-l1, l1}], TestID -> "E2a: QMeSForm routed Propagator to named symbol"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]], 1/2 * Symbol["GPhiPhi"][{-l1, l1}] * Symbol["RdotPhiPhi"][{l1, -l1}], TestID -> "E2b: QMeSForm routed FTerm"]];

AppendTo[tests, VerificationTest[FunKit`QMeSForm[testSetup, FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]]], {1/2 * Symbol["GPhiPhi"][{-l1, l1}] * Symbol["RdotPhiPhi"][{l1, -l1}]}, TestID -> "E2c: QMeSForm routed FEx"]];

(**********************************************************************************
    E3: DoFunForm routed -> DoFun algebraic format
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, Propagator[{Phi, Phi}, {{l1}, {-l1}}]], DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False], TestID -> "E3a: DoFunForm routed Propagator to algebraic"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, GammaN[{Phi, Phi, Phi, Phi}, {{l1}, {-l1}, {l2}, {-l2}}]], DoFun`DoDSERGE`V[Phi[l1], Phi[-l1], Phi[l2], Phi[-l2], Global`explicit -> False], TestID -> "E3b: DoFunForm routed GammaN to algebraic V"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, Rdot[{Phi, Phi}, {{l1}, {-l1}}]], DoFun`DoDSERGE`dR[Phi[l1], Phi[-l1], Global`explicit -> False], TestID -> "E3c: DoFunForm routed Rdot to algebraic dR"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, S[{Phi, Phi}, {{l1}, {-l1}}]], DoFun`DoDSERGE`S[Phi[-l1], Phi[l1], Global`explicit -> False], TestID -> "E3d: DoFunForm routed S to algebraic (momentum negation)"]];

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetup, FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]], 1/2 * DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False] * DoFun`DoDSERGE`dR[Phi[-l1], Phi[l1], Global`explicit -> False], TestID -> "E3e: DoFunForm routed FTerm to algebraic product"]];

AppendTo[tests, VerificationTest[Head[FunKit`DoFunForm[testSetup, FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]]]], List, TestID -> "E3f: DoFunForm routed FEx returns List"]];

(**********************************************************************************
    E4: DoFunForm with routed Yukawa (multi-component indices)
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`DoFunForm[testSetupYukawa, Propagator[{Psi, Psibar}, {{lf1, {a1}}, {-lf1, {a1}}}]], DoFun`DoDSERGE`P[Psi[lf1, a1], Psibar[-lf1, a1], Global`explicit -> False], TestID -> "E4a: DoFunForm routed fermion Propagator"]];

(**********************************************************************************
    E5: FunKitForm from DoFun algebraic format
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False]}], FEx[FTerm[Propagator[{Phi, Phi}, {{l1}, {-l1}}]]], TestID -> "E5a: FunKitForm DoFun algebraic Propagator"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {DoFun`DoDSERGE`V[Phi[l1], Phi[-l1], Phi[l2], Phi[-l2], Global`explicit -> False]}], FEx[FTerm[GammaN[{Phi, Phi, Phi, Phi}, {{l1}, {-l1}, {l2}, {-l2}}]]], TestID -> "E5b: FunKitForm DoFun algebraic V to GammaN"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {DoFun`DoDSERGE`S[Phi[-l1], Phi[l1], Global`explicit -> False]}], FEx[FTerm[S[{Phi, Phi}, {{l1}, {-l1}}]]], TestID -> "E5c: FunKitForm DoFun algebraic S (momentum negation reversal)"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {1/2 * DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False] * DoFun`DoDSERGE`dR[Phi[-l1], Phi[l1], Global`explicit -> False]}], FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]], TestID -> "E5d: FunKitForm DoFun algebraic product with coefficient"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetupYukawa, {DoFun`DoDSERGE`P[Psi[lf1, a1], Psibar[-lf1, a1], Global`explicit -> False]}], FEx[FTerm[Propagator[{Psi, Psibar}, {{lf1, {a1}}, {-lf1, {a1}}}]]], TestID -> "E5e: FunKitForm DoFun algebraic fermion"]];

(**********************************************************************************
    E6: FunKitForm from QMeS named-symbol format
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {Symbol["GPhiPhi"][{i1, i2}]}], FEx[FTerm[Propagator[{Phi, Phi}, {{i1}, {i2}}]]], TestID -> "E6a: FunKitForm QMeS named Propagator"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {Symbol["\[CapitalGamma]PhiPhiPhi"][{i1, i2, i3}]}], FEx[FTerm[GammaN[{Phi, Phi, Phi}, {{i1}, {i2}, {i3}}]]], TestID -> "E6b: FunKitForm QMeS named GammaN"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {Symbol["RdotPhiPhi"][{i1, i2}]}], FEx[FTerm[Rdot[{Phi, Phi}, {{i1}, {i2}}]]], TestID -> "E6c: FunKitForm QMeS named Rdot"]];

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetup, {1/2 * Symbol["GPhiPhi"][{l1, -l1}] * Symbol["RdotPhiPhi"][{-l1, l1}]}], FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]], TestID -> "E6d: FunKitForm QMeS named product"]];

(* E6e/E6f: routed named-symbol reconstruction where the per-leg slot count is
   NOT the field-template count. QMeS leaves an EXTERNAL leg as a single bare
   superindex (no group index even for a fermion) while INTERNAL legs are
   {momentum, {groupIndex}}. reverseQMeSNaming must segment by classifying group
   indices (atomic symbols whose base name is a declared internal index, here
   "a") rather than chunking by template slots. Regression guard for the
   Part::take / garbage-reconstruction bug on QMeS FullDiagrams output. *)

(* Mixed case: the external Psi leg is a bare superindex {q3} while the Psibar leg
   carries its group index. This is QMeS FullDiagrams' external-leg convention, which
   reverseQMeSNaming reconstructs faithfully but flags with FunKitForm::mixedSuperindex
   (external legs should be given full index structure). The expected-message argument
   asserts the warning fires. *)

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetupYukawa, {Symbol["\[CapitalGamma]PhiPsibarPsi"][{q1, q2, a, q3}]}], FEx[FTerm[GammaN[{Phi, Psibar, Psi}, {{q1}, {q2, {a}}, {q3}}]]], {FunKit`FunKitForm::mixedSuperindex}, TestID -> "E6e: FunKitForm QMeS named vertex with external (bare) leg warns + reconstructs"]];

(* Pure routed (both legs internal, full group structure): no warning expected. *)

AppendTo[tests, VerificationTest[FunKit`FunKitForm[testSetupYukawa, {Symbol["GPsiPsibar"][{q1, Symbol["a$1"], q2, Symbol["a$2"]}]}], FEx[FTerm[Propagator[{Psi, Psibar}, {{q1, {Symbol["a$1"]}}, {q2, {Symbol["a$2"]}}}]]], TestID -> "E6f: FunKitForm QMeS named fermion propagator (both legs internal, no warning)"]];

(* Multi-group-index field with localised template names: A[p,{v,col}] declares two
   group indices (v, col). Each internal gluon leg is {momentum, {v, col}} and the
   external leg is a bare superindex. Base-name matching segments correctly regardless
   of the v$.../col$... suffixes. Regression guard for the Yang-Mills breakage. *)

AppendTo[tests, VerificationTest[FunKit`FunKitForm[GetFunKitSetupYangMills[], {Symbol["\[CapitalGamma]AAA"][{k1, Symbol["v$1"], Symbol["col$1"], k2, Symbol["v$2"], Symbol["col$2"], ext}]}], FEx[FTerm[GammaN[{A, A, A}, {{k1, {Symbol["v$1"], Symbol["col$1"]}}, {k2, {Symbol["v$2"], Symbol["col$2"]}}, {ext}}]]], {FunKit`FunKitForm::mixedSuperindex}, TestID -> "E6g: FunKitForm QMeS named gluon vertex (2 group indices/leg + external leg)"]];

(**********************************************************************************
    E7: Roundtrip tests
**********************************************************************************)

(* QMeS superindex roundtrip — note: canonical ordering reverses indices for {Phi,Phi} *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{orig, converted},
            orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]];
            converted = FunKit`QMeSForm[testSetup, orig];
            FunKit`FunKitForm[testSetup, converted]
        ]
        ,
        FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i2, i1}], Rdot[{Phi, Phi}, {-i2, -i1}]]]
        ,
        TestID -> "E7a: QMeS superindex roundtrip"
    ]
];

(* DoFun superindex roundtrip *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{orig, converted},
            orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]];
            converted = FunKit`DoFunForm[testSetup, orig];
            FunKit`FunKitForm[testSetup, converted]
        ]
        ,
        FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]]
        ,
        TestID -> "E7b: DoFun superindex roundtrip"
    ]
];

(* DoFun algebraic roundtrip *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{orig, converted},
            orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]];
            converted = FunKit`DoFunForm[testSetup, orig];
            FunKit`FunKitForm[testSetup, converted]
        ]
        ,
        FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]]
        ,
        TestID -> "E7c: DoFun algebraic roundtrip"
    ]
];

(* QMeS named-symbol roundtrip for scalar — preserves routing *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{orig, converted},
            orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]];
            converted = FunKit`QMeSForm[testSetup, orig];
            FunKit`FunKitForm[testSetup, converted]
        ]
        ,
        FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{-l1}, {l1}}], Rdot[{Phi, Phi}, {{l1}, {-l1}}]]]
        ,
        TestID -> "E7d: QMeS named-symbol roundtrip (scalar: preserves routing)"
    ]
];

(**********************************************************************************
    E8: Detection predicates
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`routedObjectQ[Propagator[{Phi, Phi}, {{l1}, {-l1}}]], True, TestID -> "E8a: routedObjectQ on routed Propagator"]];

AppendTo[tests, VerificationTest[FunKit`Private`routedObjectQ[Propagator[{Phi, Phi}, {i1, i2}]], False, TestID -> "E8b: routedObjectQ on superindex Propagator"]];

AppendTo[tests, VerificationTest[FunKit`Private`routedFExQ[FEx[FTerm[1, Propagator[{Phi, Phi}, {{l1}, {-l1}}]]]], True, TestID -> "E8c: routedFExQ on routed FEx"]];

AppendTo[tests, VerificationTest[FunKit`Private`routedFExQ[FEx[FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]]]], False, TestID -> "E8d: routedFExQ on superindex FEx"]];

(**********************************************************************************
    F: Cross-validation with derived Yukawa equations
    Derive with QMeS/DoFun (superindex and routed), convert to FunKit,
    convert back, check formal correctness of all intermediate expressions.
**********************************************************************************)

yQMeSSetup = GetQMeSWetterichSetupYukawa[];

yDoFunSetup = GetDoFunSetupYukawa[];

yFunKitSetup = GetFunKitSetupYukawa[];

FSetGlobalSetup[yFunKitSetup];

(* Structural checks *)

validFExQ[expr_FEx] :=
    AllTrue[Select[List @@ expr, Head[#] =!= Rule&], Head[#] === FTerm&];

validFExQ[_] :=
    False;

validQMeSListQ[expr_List] :=
    AllTrue[expr, FunKit`Private`QMeSSuperindexDiagramQ];

validQMeSListQ[_] :=
    False;

validDoFunSymQ[expr_] :=
    FunKit`Private`DoFunSuperindexDiagramQ[expr];

validDoFunAlgQ[expr_] :=
    FunKit`Private`DoFunAlgebraicDiagramQ[expr];

(* ---- QMeS superindex derivation ---- *)

qmesDiag2 = DeriveFunctionalEquation[yQMeSSetup, {Phi[i1], Phi[i2]}, "OutputLevel" -> "SuperindexDiagrams"];

qmesDiag2 = ReduceIdenticalFlowDiagrams[qmesDiag2, {Phi[i1], Phi[i2]}];

qmesDiag3 = DeriveFunctionalEquation[yQMeSSetup, {Psi[i1], Psibar[i2], Phi[i3]}, "OutputLevel" -> "SuperindexDiagrams"];

qmesDiag3 = ReduceIdenticalFlowDiagrams[qmesDiag3, {Psi[i1], Psibar[i2], Phi[i3]}];

(* ---- DoFun superindex derivation ---- *)

doFunDiag2 = wrapDoFun[yDoFunSetup <> "doRGE[actionYukawaSymbolic,{Phi,Phi}]"];

doFunDiag3 = wrapDoFun[yDoFunSetup <> "doRGE[actionYukawaSymbolic,{Psi,Psibar,Phi}]"];

(* ---- DoFun algebraic (routed) derivation via getAE ---- *)

doFunAlgSetup = yDoFunSetup <> "defineFieldsSpecific[{Phi[mom], Psi[mom, f], Psibar[mom, f]}];\n";

doFunAlg2 = wrapDoFun[doFunAlgSetup <> "getAE[doRGE[actionYukawaSymbolic,{Phi,Phi}], {{Phi,Global`i1,p1},{Phi,Global`i2,-p1}}, explicit->False]"];

doFunAlg3 = wrapDoFun[doFunAlgSetup <> "getAE[doRGE[actionYukawaSymbolic,{Psi,Psibar,Phi}], {{Psi,Global`i1,p1,a1},{Psibar,Global`i2,p2,a2},{Phi,Global`i3,-p1-p2}}, explicit->False]"];

(**********************************************************************************
    F1: QMeS superindex — derive, convert to FunKit, convert back, check structure
**********************************************************************************)

fQmesFK2 = FunKitForm[qmesDiag2];

fQmesFK3 = FunKitForm[qmesDiag3];

fQmesBack2 = QMeSForm[yFunKitSetup, fQmesFK2];

fQmesBack3 = QMeSForm[yFunKitSetup, fQmesFK3];

fQmesRT2 = FunKitForm[fQmesBack2];

fQmesRT3 = FunKitForm[fQmesBack3];

AppendTo[tests, VerificationTest[validFExQ[fQmesFK2], True, TestID -> "F1a: QMeS scalar propagator -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[validFExQ[fQmesFK3], True, TestID -> "F1b: QMeS Yukawa vertex -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[validQMeSListQ[fQmesBack2], True, TestID -> "F1c: QMeS scalar propagator: QMeSForm produces valid QMeS"]];

AppendTo[tests, VerificationTest[validQMeSListQ[fQmesBack3], True, TestID -> "F1d: QMeS Yukawa vertex: QMeSForm produces valid QMeS"]];

AppendTo[tests, VerificationTest[fQmesRT2 === fQmesFK2, True, TestID -> "F1e: QMeS scalar propagator: roundtrip matches original"]];

(* Yukawa vertex roundtrip changes field ordering due to QMeS c>ag>g convention — check structure *)

AppendTo[tests, VerificationTest[validFExQ[fQmesRT3] && FreeQ[fQmesRT3, Part] && Length[fQmesRT3] === Length[fQmesFK3], True, TestID -> "F1f: QMeS Yukawa vertex: roundtrip produces valid FEx with same diagram count"]];

(**********************************************************************************
    F2: DoFun superindex — derive, convert to FunKit, convert back, check structure
**********************************************************************************)

fDoFunFK2 = FunKitForm[doFunDiag2];

fDoFunFK3 = FunKitForm[doFunDiag3];

fDoFunBack2 = DoFunForm[yFunKitSetup, fDoFunFK2];

fDoFunBack3 = DoFunForm[yFunKitSetup, fDoFunFK3];

fDoFunRT2 = FunKitForm[fDoFunBack2];

fDoFunRT3 = FunKitForm[fDoFunBack3];

AppendTo[tests, VerificationTest[validFExQ[fDoFunFK2], True, TestID -> "F2a: DoFun scalar propagator -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[validFExQ[fDoFunFK3], True, TestID -> "F2b: DoFun Yukawa vertex -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[validDoFunSymQ[fDoFunBack2], True, TestID -> "F2c: DoFun scalar propagator: DoFunForm produces valid DoFun"]];

AppendTo[tests, VerificationTest[validDoFunSymQ[fDoFunBack3], True, TestID -> "F2d: DoFun Yukawa vertex: DoFunForm produces valid DoFun"]];

AppendTo[tests, VerificationTest[fDoFunRT2 === fDoFunFK2, True, TestID -> "F2e: DoFun scalar propagator: roundtrip matches original"]];

AppendTo[tests, VerificationTest[fDoFunRT3 === fDoFunFK3, True, TestID -> "F2f: DoFun Yukawa vertex: roundtrip matches original"]];

(**********************************************************************************
    F3: DoFun algebraic (routed via getAE) — derive, convert, roundtrip
**********************************************************************************)

fDoFunAlgFK2 = FunKitForm[doFunAlg2];

fDoFunAlgFK3 = FunKitForm[doFunAlg3];

fDoFunAlgBack2 = DoFunForm[yFunKitSetup, fDoFunAlgFK2];

fDoFunAlgBack3 = DoFunForm[yFunKitSetup, fDoFunAlgFK3];

fDoFunAlgRT2 = FunKitForm[fDoFunAlgBack2];

fDoFunAlgRT3 = FunKitForm[fDoFunAlgBack3];

AppendTo[tests, VerificationTest[validFExQ[fDoFunAlgFK2], True, TestID -> "F3a: DoFun getAE scalar propagator -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[validFExQ[fDoFunAlgFK3], True, TestID -> "F3b: DoFun getAE Yukawa vertex -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[validDoFunAlgQ[fDoFunAlgBack2], True, TestID -> "F3c: DoFun getAE scalar propagator: DoFunForm produces valid algebraic"]];

AppendTo[tests, VerificationTest[validDoFunAlgQ[fDoFunAlgBack3], True, TestID -> "F3d: DoFun getAE Yukawa vertex: DoFunForm produces valid algebraic"]];

AppendTo[tests, VerificationTest[fDoFunAlgRT2 === fDoFunAlgFK2, True, TestID -> "F3e: DoFun getAE scalar propagator: roundtrip matches original"]];

AppendTo[tests, VerificationTest[fDoFunAlgRT3 === fDoFunAlgFK3, True, TestID -> "F3f: DoFun getAE Yukawa vertex: roundtrip matches original"]];

(**********************************************************************************
    F4: QMeS routed ("FullDiagrams") — derive, convert, roundtrip
    QMeS "FullDiagrams" produces named-symbol format with explicit momenta,
    which is exactly the QMeS routed format.
**********************************************************************************)

qmesFullDiag2 = DeriveFunctionalEquation[yQMeSSetup, {Phi[i1], Phi[i2]}, "OutputLevel" -> "FullDiagrams"];

qmesFullDiag3 = DeriveFunctionalEquation[yQMeSSetup, {Psi[i1], Psibar[i2], Phi[i3]}, "OutputLevel" -> "FullDiagrams"];

fQmesFullFK2 = FunKitForm[yFunKitSetup, qmesFullDiag2];

fQmesFullFK3 = FunKitForm[yFunKitSetup, qmesFullDiag3];

fQmesFullBack2 = QMeSForm[yFunKitSetup, fQmesFullFK2];

fQmesFullBack3 = QMeSForm[yFunKitSetup, fQmesFullFK3];

fQmesFullRT2 = FunKitForm[yFunKitSetup, fQmesFullBack2];

fQmesFullRT3 = FunKitForm[yFunKitSetup, fQmesFullBack3];

AppendTo[tests, VerificationTest[validFExQ[fQmesFullFK2], True, TestID -> "F4a: QMeS FullDiagrams scalar propagator -> FunKitForm produces valid FEx"]];

(* FreeQ[..., Part] guards against the out-of-bounds-Part / garbage reconstruction
   regression: a failed index span leaves unevaluated Part[...] artifacts in the FEx. *)

AppendTo[tests, VerificationTest[validFExQ[fQmesFullFK3] && FreeQ[fQmesFullFK3, Part], True, TestID -> "F4b: QMeS FullDiagrams Yukawa vertex -> FunKitForm produces valid FEx"]];

AppendTo[tests, VerificationTest[Head[fQmesFullBack2] === List, True, TestID -> "F4c: QMeS FullDiagrams scalar propagator: QMeSForm produces List"]];

AppendTo[tests, VerificationTest[Head[fQmesFullBack3] === List, True, TestID -> "F4d: QMeS FullDiagrams Yukawa vertex: QMeSForm produces List"]];

AppendTo[tests, VerificationTest[validFExQ[fQmesFullRT2], True, TestID -> "F4e: QMeS FullDiagrams scalar propagator: roundtrip produces valid FEx"]];

AppendTo[tests, VerificationTest[validFExQ[fQmesFullRT3] && FreeQ[fQmesFullRT3, Part] && Length[fQmesFullRT3] === Length[fQmesFullFK3], True, TestID -> "F4f: QMeS FullDiagrams Yukawa vertex: roundtrip produces valid FEx"]];

(**********************************************************************************
    F5: FunKit-derivation origin. F1-F4 start from a QMeS/DoFun derivation and
    round-trip through FunKit (QMeS -> FunKit -> QMeS -> FunKit). F5 closes the
    other direction: take a *FunKit-native* derivation (FTakeDerivatives) and
    round-trip it through QMeS (FunKit -> QMeS -> FunKit), checking the diagrams
    are recovered. (QMeSForm strips the Symmetries annotation, so we compare the
    FTerm content rather than the annotated FEx.)
**********************************************************************************)

fkDerived = FTakeDerivatives[testSetup, FEx[FTerm[GammaN[{Phi, Phi}, {i1, i2}]]], {Phi[k]}];

fkRoundtrip = FunKitForm[testSetup, QMeSForm[testSetup, fkDerived]];

AppendTo[tests, VerificationTest[validFExQ[fkRoundtrip] && FreeQ[fkRoundtrip, Part] && Sort[Cases[fkRoundtrip, _FTerm]] === Sort[Cases[fkDerived, _FTerm]], True, TestID -> "F5: FunKit derivation -> QMeS -> FunKit recovers derived diagrams"]];
