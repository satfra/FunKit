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

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, Propagator[{Phi, Phi}, {i1, i2}]],
    DoFun`DoDSERGE`P[{Phi, i1}, {Phi, i2}],
    TestID -> "D4a: DoFunForm single Propagator"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]],
    DoFun`DoDSERGE`V[{Phi, i1}, {Phi, i2}, {Phi, i3}],
    TestID -> "D4b: DoFunForm GammaN to V"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, S[{Phi, Phi}, {i1, i2}]],
    DoFun`DoDSERGE`S[{Phi, -i1}, {Phi, -i2}],
    TestID -> "D4c: DoFunForm S with index sign flip"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, Rdot[{Phi, Phi}, {i1, i2}]],
    DoFun`DoDSERGE`dR[{Phi, i1}, {Phi, i2}],
    TestID -> "D4d: DoFunForm Rdot to dR"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup,
        FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {i3, i4}]]],
    1/2 * DoFun`DoDSERGE`op[
        DoFun`DoDSERGE`P[{Phi, i1}, {Phi, i2}],
        DoFun`DoDSERGE`dR[{Phi, i3}, {Phi, i4}]],
    TestID -> "D4e: DoFunForm FTerm with coefficient"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, FEx[
        FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]],
        FTerm[-1, GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]]]],
    DoFun`DoDSERGE`op[DoFun`DoDSERGE`P[{Phi, i1}, {Phi, i2}]]
        - DoFun`DoDSERGE`op[DoFun`DoDSERGE`V[{Phi, i1}, {Phi, i2}, {Phi, i3}]],
    TestID -> "D4f: DoFunForm FEx to Plus"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, FEx[]],
    0,
    TestID -> "D4g: DoFunForm empty FEx"
]];

(**********************************************************************************
    E1: QMeSForm superindex -> QMeS native Association format
**********************************************************************************)

(* Note: QMeS canonical ordering "c>ag>g" reverses index order for {Phi,Phi} *)

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, Propagator[{Phi, Phi}, {i1, i2}]],
    {1, <|"type" -> "Propagator", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>},
    TestID -> "E1a: QMeSForm superindex Propagator"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, Rdot[{Phi, Phi}, {i1, i2}]],
    {1, <|"type" -> "Regulatordot", "indices" -> {{Phi, {-i2}}, {Phi, {-i1}}}|>},
    TestID -> "E1b: QMeSForm superindex Rdot (negated indices)"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, GammaN[{Phi, Phi, Phi}, {i1, i2, i3}]],
    {1, <|"type" -> "nPoint", "indices" -> {{Phi, {-i3}}, {Phi, {-i2}}, {Phi, {-i1}}}, "nPoint" -> 3, "spec" -> "none"|>},
    TestID -> "E1c: QMeSForm superindex GammaN"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, S[{Phi, Phi}, {i1, i2}]],
    {1, <|"type" -> "nPoint", "indices" -> {{Phi, {-i2}}, {Phi, {-i1}}}, "nPoint" -> 2, "spec" -> "classical"|>},
    TestID -> "E1d: QMeSForm superindex S"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup,
        FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]],
    {"Prefactor" -> {1/2},
     <|"type" -> "Propagator", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>,
     <|"type" -> "Regulatordot", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>},
    TestID -> "E1e: QMeSForm superindex FTerm"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, FEx[
        FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]]],
    {{"Prefactor" -> {1/2},
      <|"type" -> "Propagator", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>,
      <|"type" -> "Regulatordot", "indices" -> {{Phi, {i2}}, {Phi, {i1}}}|>}},
    TestID -> "E1f: QMeSForm superindex FEx"
]];

(**********************************************************************************
    E2: QMeSForm routed -> QMeS named-symbol format
**********************************************************************************)

(* Note: QMeS canonical ordering reverses for {Phi,Phi} *)

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, Propagator[{Phi, Phi}, {{l1}, {-l1}}]],
    Symbol["GPhiPhi"][{-l1, l1}],
    TestID -> "E2a: QMeSForm routed Propagator to named symbol"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup,
        FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]],
    1/2 * Symbol["GPhiPhi"][{-l1, l1}] * Symbol["RdotPhiPhi"][{l1, -l1}],
    TestID -> "E2b: QMeSForm routed FTerm"
]];

AppendTo[tests, TestCreate[
    FunKit`QMeSForm[testSetup, FEx[
        FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]]],
    {1/2 * Symbol["GPhiPhi"][{-l1, l1}] * Symbol["RdotPhiPhi"][{l1, -l1}]},
    TestID -> "E2c: QMeSForm routed FEx"
]];

(**********************************************************************************
    E3: DoFunForm routed -> DoFun algebraic format
**********************************************************************************)

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, Propagator[{Phi, Phi}, {{l1}, {-l1}}]],
    DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False],
    TestID -> "E3a: DoFunForm routed Propagator to algebraic"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, GammaN[{Phi, Phi, Phi, Phi}, {{l1}, {-l1}, {l2}, {-l2}}]],
    DoFun`DoDSERGE`V[Phi[l1], Phi[-l1], Phi[l2], Phi[-l2], Global`explicit -> False],
    TestID -> "E3b: DoFunForm routed GammaN to algebraic V"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, Rdot[{Phi, Phi}, {{l1}, {-l1}}]],
    DoFun`DoDSERGE`dR[Phi[l1], Phi[-l1], Global`explicit -> False],
    TestID -> "E3c: DoFunForm routed Rdot to algebraic dR"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup, S[{Phi, Phi}, {{l1}, {-l1}}]],
    DoFun`DoDSERGE`S[Phi[-l1], Phi[l1], Global`explicit -> False],
    TestID -> "E3d: DoFunForm routed S to algebraic (momentum negation)"
]];

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetup,
        FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]],
    1/2 * DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False]
        * DoFun`DoDSERGE`dR[Phi[-l1], Phi[l1], Global`explicit -> False],
    TestID -> "E3e: DoFunForm routed FTerm to algebraic product"
]];

AppendTo[tests, TestCreate[
    Head[FunKit`DoFunForm[testSetup, FEx[
        FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]]]],
    List,
    TestID -> "E3f: DoFunForm routed FEx returns List"
]];

(**********************************************************************************
    E4: DoFunForm with routed Yukawa (multi-component indices)
**********************************************************************************)

AppendTo[tests, TestCreate[
    FunKit`DoFunForm[testSetupYukawa, Propagator[{Psi, Psibar}, {{lf1, {a1}}, {-lf1, {a1}}}]],
    DoFun`DoDSERGE`P[Psi[lf1, a1], Psibar[-lf1, a1], Global`explicit -> False],
    TestID -> "E4a: DoFunForm routed fermion Propagator"
]];

(**********************************************************************************
    E5: FunKitForm from DoFun algebraic format
**********************************************************************************)

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[{DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False]}],
    FEx[FTerm[Propagator[{Phi, Phi}, {{l1}, {-l1}}]]],
    TestID -> "E5a: FunKitForm DoFun algebraic Propagator"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[{DoFun`DoDSERGE`V[Phi[l1], Phi[-l1], Phi[l2], Phi[-l2], Global`explicit -> False]}],
    FEx[FTerm[GammaN[{Phi, Phi, Phi, Phi}, {{l1}, {-l1}, {l2}, {-l2}}]]],
    TestID -> "E5b: FunKitForm DoFun algebraic V to GammaN"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[{DoFun`DoDSERGE`S[Phi[-l1], Phi[l1], Global`explicit -> False]}],
    FEx[FTerm[S[{Phi, Phi}, {{l1}, {-l1}}]]],
    TestID -> "E5c: FunKitForm DoFun algebraic S (momentum negation reversal)"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[{
        1/2 * DoFun`DoDSERGE`P[Phi[l1], Phi[-l1], Global`explicit -> False]
            * DoFun`DoDSERGE`dR[Phi[-l1], Phi[l1], Global`explicit -> False]}],
    FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]],
    TestID -> "E5d: FunKitForm DoFun algebraic product with coefficient"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[{DoFun`DoDSERGE`P[Psi[lf1, a1], Psibar[-lf1, a1], Global`explicit -> False]}],
    FEx[FTerm[Propagator[{Psi, Psibar}, {{lf1, {a1}}, {-lf1, {a1}}}]]],
    TestID -> "E5e: FunKitForm DoFun algebraic fermion"
]];

(**********************************************************************************
    E6: FunKitForm from QMeS named-symbol format
**********************************************************************************)

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[testSetup, {Symbol["GPhiPhi"][{i1, i2}]}],
    FEx[FTerm[Propagator[{Phi, Phi}, {{i1}, {i2}}]]],
    TestID -> "E6a: FunKitForm QMeS named Propagator"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[testSetup, {Symbol["\[CapitalGamma]PhiPhiPhi"][{i1, i2, i3}]}],
    FEx[FTerm[GammaN[{Phi, Phi, Phi}, {{i1}, {i2}, {i3}}]]],
    TestID -> "E6b: FunKitForm QMeS named GammaN"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[testSetup, {Symbol["RdotPhiPhi"][{i1, i2}]}],
    FEx[FTerm[Rdot[{Phi, Phi}, {{i1}, {i2}}]]],
    TestID -> "E6c: FunKitForm QMeS named Rdot"
]];

AppendTo[tests, TestCreate[
    FunKit`FunKitForm[testSetup,
        {1/2 * Symbol["GPhiPhi"][{l1, -l1}] * Symbol["RdotPhiPhi"][{-l1, l1}]}],
    FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]],
    TestID -> "E6d: FunKitForm QMeS named product"
]];

(**********************************************************************************
    E7: Roundtrip tests
**********************************************************************************)

(* QMeS superindex roundtrip — note: canonical ordering reverses indices for {Phi,Phi} *)
AppendTo[tests, TestCreate[
    Module[{orig, converted},
        orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]];
        converted = FunKit`QMeSForm[testSetup, orig];
        FunKit`FunKitForm[converted]
    ],
    FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i2, i1}], Rdot[{Phi, Phi}, {-i2, -i1}]]],
    TestID -> "E7a: QMeS superindex roundtrip"
]];

(* DoFun superindex roundtrip *)
AppendTo[tests, TestCreate[
    Module[{orig, converted},
        orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]];
        converted = FunKit`DoFunForm[testSetup, orig];
        FunKit`FunKitForm[converted]
    ],
    FEx[FTerm[1/2, Propagator[{Phi, Phi}, {i1, i2}], Rdot[{Phi, Phi}, {-i1, -i2}]]],
    TestID -> "E7b: DoFun superindex roundtrip"
]];

(* DoFun algebraic roundtrip *)
AppendTo[tests, TestCreate[
    Module[{orig, converted},
        orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]];
        converted = FunKit`DoFunForm[testSetup, orig];
        FunKit`FunKitForm[converted]
    ],
    FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]],
    TestID -> "E7c: DoFun algebraic roundtrip"
]];

(* QMeS named-symbol roundtrip for scalar — preserves routing *)
AppendTo[tests, TestCreate[
    Module[{orig, converted},
        orig = FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{l1}, {-l1}}], Rdot[{Phi, Phi}, {{-l1}, {l1}}]]];
        converted = FunKit`QMeSForm[testSetup, orig];
        FunKit`FunKitForm[testSetup, converted]
    ],
    FEx[FTerm[1/2, Propagator[{Phi, Phi}, {{-l1}, {l1}}], Rdot[{Phi, Phi}, {{l1}, {-l1}}]]],
    TestID -> "E7d: QMeS named-symbol roundtrip (scalar: preserves routing)"
]];

(**********************************************************************************
    E8: Detection predicates
**********************************************************************************)

AppendTo[tests, TestCreate[
    FunKit`Private`routedObjectQ[Propagator[{Phi, Phi}, {{l1}, {-l1}}]],
    True,
    TestID -> "E8a: routedObjectQ on routed Propagator"
]];

AppendTo[tests, TestCreate[
    FunKit`Private`routedObjectQ[Propagator[{Phi, Phi}, {i1, i2}]],
    False,
    TestID -> "E8b: routedObjectQ on superindex Propagator"
]];

AppendTo[tests, TestCreate[
    FunKit`Private`routedFExQ[FEx[FTerm[1, Propagator[{Phi, Phi}, {{l1}, {-l1}}]]]],
    True,
    TestID -> "E8c: routedFExQ on routed FEx"
]];

AppendTo[tests, TestCreate[
    FunKit`Private`routedFExQ[FEx[FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]]]],
    False,
    TestID -> "E8d: routedFExQ on superindex FEx"
]];

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
validFExQ[expr_FEx] := AllTrue[Select[List @@ expr, Head[#] =!= Rule&], Head[#] === FTerm&];
validFExQ[_] := False;
validQMeSListQ[expr_List] := AllTrue[expr, FunKit`Private`QMeSSuperindexDiagramQ];
validQMeSListQ[_] := False;
validDoFunSymQ[expr_] := FunKit`Private`DoFunSuperindexDiagramQ[expr];
validDoFunAlgQ[expr_] := FunKit`Private`DoFunAlgebraicDiagramQ[expr];

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

AppendTo[tests, TestCreate[validFExQ[fQmesFK2], True, TestID -> "F1a: QMeS scalar propagator -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validFExQ[fQmesFK3], True, TestID -> "F1b: QMeS Yukawa vertex -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validQMeSListQ[fQmesBack2], True, TestID -> "F1c: QMeS scalar propagator: QMeSForm produces valid QMeS"]];
AppendTo[tests, TestCreate[validQMeSListQ[fQmesBack3], True, TestID -> "F1d: QMeS Yukawa vertex: QMeSForm produces valid QMeS"]];
AppendTo[tests, TestCreate[fQmesRT2 === fQmesFK2, True, TestID -> "F1e: QMeS scalar propagator: roundtrip matches original"]];
(* Yukawa vertex roundtrip changes field ordering due to QMeS c>ag>g convention — check structure *)
AppendTo[tests, TestCreate[validFExQ[fQmesRT3] && Length[fQmesRT3] === Length[fQmesFK3], True, TestID -> "F1f: QMeS Yukawa vertex: roundtrip produces valid FEx with same diagram count"]];

(**********************************************************************************
    F2: DoFun superindex — derive, convert to FunKit, convert back, check structure
**********************************************************************************)

fDoFunFK2 = FunKitForm[doFunDiag2];
fDoFunFK3 = FunKitForm[doFunDiag3];
fDoFunBack2 = DoFunForm[yFunKitSetup, fDoFunFK2];
fDoFunBack3 = DoFunForm[yFunKitSetup, fDoFunFK3];
fDoFunRT2 = FunKitForm[fDoFunBack2];
fDoFunRT3 = FunKitForm[fDoFunBack3];

AppendTo[tests, TestCreate[validFExQ[fDoFunFK2], True, TestID -> "F2a: DoFun scalar propagator -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validFExQ[fDoFunFK3], True, TestID -> "F2b: DoFun Yukawa vertex -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validDoFunSymQ[fDoFunBack2], True, TestID -> "F2c: DoFun scalar propagator: DoFunForm produces valid DoFun"]];
AppendTo[tests, TestCreate[validDoFunSymQ[fDoFunBack3], True, TestID -> "F2d: DoFun Yukawa vertex: DoFunForm produces valid DoFun"]];
AppendTo[tests, TestCreate[fDoFunRT2 === fDoFunFK2, True, TestID -> "F2e: DoFun scalar propagator: roundtrip matches original"]];
AppendTo[tests, TestCreate[fDoFunRT3 === fDoFunFK3, True, TestID -> "F2f: DoFun Yukawa vertex: roundtrip matches original"]];

(**********************************************************************************
    F3: DoFun algebraic (routed via getAE) — derive, convert, roundtrip
**********************************************************************************)

fDoFunAlgFK2 = FunKitForm[doFunAlg2];
fDoFunAlgFK3 = FunKitForm[doFunAlg3];
fDoFunAlgBack2 = DoFunForm[yFunKitSetup, fDoFunAlgFK2];
fDoFunAlgBack3 = DoFunForm[yFunKitSetup, fDoFunAlgFK3];
fDoFunAlgRT2 = FunKitForm[fDoFunAlgBack2];
fDoFunAlgRT3 = FunKitForm[fDoFunAlgBack3];

AppendTo[tests, TestCreate[validFExQ[fDoFunAlgFK2], True, TestID -> "F3a: DoFun getAE scalar propagator -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validFExQ[fDoFunAlgFK3], True, TestID -> "F3b: DoFun getAE Yukawa vertex -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validDoFunAlgQ[fDoFunAlgBack2], True, TestID -> "F3c: DoFun getAE scalar propagator: DoFunForm produces valid algebraic"]];
AppendTo[tests, TestCreate[validDoFunAlgQ[fDoFunAlgBack3], True, TestID -> "F3d: DoFun getAE Yukawa vertex: DoFunForm produces valid algebraic"]];
AppendTo[tests, TestCreate[fDoFunAlgRT2 === fDoFunAlgFK2, True, TestID -> "F3e: DoFun getAE scalar propagator: roundtrip matches original"]];
AppendTo[tests, TestCreate[fDoFunAlgRT3 === fDoFunAlgFK3, True, TestID -> "F3f: DoFun getAE Yukawa vertex: roundtrip matches original"]];

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

AppendTo[tests, TestCreate[validFExQ[fQmesFullFK2], True, TestID -> "F4a: QMeS FullDiagrams scalar propagator -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[validFExQ[fQmesFullFK3], True, TestID -> "F4b: QMeS FullDiagrams Yukawa vertex -> FunKitForm produces valid FEx"]];
AppendTo[tests, TestCreate[Head[fQmesFullBack2] === List, True, TestID -> "F4c: QMeS FullDiagrams scalar propagator: QMeSForm produces List"]];
AppendTo[tests, TestCreate[Head[fQmesFullBack3] === List, True, TestID -> "F4d: QMeS FullDiagrams Yukawa vertex: QMeSForm produces List"]];
AppendTo[tests, TestCreate[validFExQ[fQmesFullRT2], True, TestID -> "F4e: QMeS FullDiagrams scalar propagator: roundtrip produces valid FEx"]];
AppendTo[tests, TestCreate[validFExQ[fQmesFullRT3], True, TestID -> "F4f: QMeS FullDiagrams Yukawa vertex: roundtrip produces valid FEx"]];
