(* ::Package:: *)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* Utilities testing *)

(* Test scalar field theory *)

(**************** QMeS ****************)

sQMeSSetup = GetQMeSWetterichSetupScalar[];

(**** Propagator ****)

QMeSdList2 = {Phi[-p1], Phi[p1]};

QMeSdiag2Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList2, "OutputLevel"
   -> "SuperindexDiagrams"];

QMeSdiag2Idx = ReduceIdenticalFlowDiagrams[QMeSdiag2Idx, QMeSdList2];

(**** Three-Point ****)

QMeSdList3 = {Phi[p1], Phi[p2], Phi[-p1 - p2]};

QMeSdiag3Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList3, "OutputLevel"
   -> "SuperindexDiagrams"];

QMeSdiag3Idx = ReduceIdenticalFlowDiagrams[QMeSdiag3Idx, QMeSdList3];

(**************** FunKit ****************)

sFunKitSetup = GetFunKitSetupScalar[];

SetGlobalSetup[sFunKitSetup]

(**** Propagator ****)

FunKitRes2 =
  TakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2]}
    ] //
  FTruncate //
  FSimplify;

(**** Three-Point ****)

FunKitRes3 =
  TakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2],
     Phi[i3]}] //
  FTruncate //
  FSimplify;

Print[
  " QMeSdiag2Idx: "
  ,
  QMeSdiag2Idx //
  FunKitForm //
  FunKit`Private`NormalizeIndices
];

Print[" FunKitRes2: ", FunKitRes2];

(**************** Comparison and Tests ****************)

sSortMomenta[expr_] :=
  expr /. sList /. sList /. sList;

sFix[expr_] :=
  sSortMomenta[expr] //
  Flatten //
  Total //
  Simplify

QMeSRes2 = sFix[QMeSRes2 //. q -> FunKitRes2["1-Loop"]["LoopMomenta"]
  [[1, 1]]];

FunKitRes2 = sFix[FunKitRes2["1-Loop"]["Expression"] //. FunKitRes2["1-Loop"
  ]["ExternalIndices"][[1, 2, 1]] -> p1];

QMeSRes3 = sFix[QMeSRes3 //. q -> FunKitRes3["1-Loop"]["LoopMomenta"]
  [[1, 1]]];

FunKitRes3 = sFix[FunKitRes3["1-Loop"]["Expression"] //. FunKitRes3["1-Loop"
  ]["ExternalIndices"][[1, 2, 1]] -> p1 //. FunKitRes3["1-Loop"]["ExternalIndices"
  ][[2, 2, 1]] -> p2];

AppendTo[tests, TestCreate[FunKitRes2, QMeSRes2, TestID -> "Verify scalar field theory (QMeS): Propagator flow"
  ]];

AppendTo[tests, TestCreate[FunKitRes3, QMeSRes3, TestID -> "Verify scalar field theory (QMeS): Three-point flow"
  ]];
