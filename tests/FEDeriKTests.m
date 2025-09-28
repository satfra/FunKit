(* ::Package:: *)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* ::Section:: *)

(*Utilities testing*)

(*AppendTo[
  tests
  ,
  TestCreate[
      FunKit`WetterichEquation
      ,
      If[$Notebooks,
              NotebookDirectory[]
                ,
                Directory[]
            ] <> "/flows"
        ,
        TestID -> "Test flowDir default value"
    ]
];*)

(* ::Section:: *)

(*Test scalar field theory*)

sQMeSSetup = GetQMeSWetterichSetupScalar[];

sFunKitSetup = GetFunKitSetupScalar[];

QMeSDerivativeListPhiPhi = {Phi[-p1], Phi[p1]};

QMeSDiagramsPhiPhisidx = DeriveFunctionalEquation[sQMeSSetup, QMeSDerivativeListPhiPhi,
   "OutputLevel" -> "SuperindexDiagrams"];

QMeSDiagramsPhiPhisidx = ReduceIdenticalFlowDiagrams[QMeSDiagramsPhiPhisidx,
   QMeSDerivativeListPhiPhi];

QMeSResultPhiPhi = SuperindexToFullDiagrams[QMeSDiagramsPhiPhisidx, sQMeSSetup,
   QMeSDerivativeListPhiPhi];

SetGlobalSetup[sFunKitSetup]

FunKitResultPhiPhi =
  TakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2]}
    ] //
  FTruncate //
  FSimplify //
  FRoute //
  QMeSForm;

SetGlobalSetup[]

sSortMomenta[expr_] :=
  expr /. {RdotPhiPhi[{a__}] :> RdotPhiPhi[Sort @ {a}], GPhiPhi[{a__}
    ] :> GPhiPhi[Sort @ {a}], \[CapitalGamma]PhiPhiPhi[{a__}] :> \[CapitalGamma]PhiPhiPhi[
    Sort @ {a}], \[CapitalGamma]PhiPhiPhiPhi[{a__}] :> \[CapitalGamma]PhiPhiPhiPhi[
    Sort @ {a}], \[CapitalGamma]PhiPhiPhiPhiPhi[{a__}] :> \[CapitalGamma]PhiPhiPhiPhiPhi[
    Sort @ {a}]}

sFix[expr_] :=
  sSortMomenta[expr] //
  Flatten //
  Total //
  Simplify

QMeSResultPhiPhi = sFix[QMeSResultPhiPhi //. q -> FunKitResultPhiPhi[
  "1-Loop"]["LoopMomenta"][[1, 1]]];

FunKitResultPhiPhi = sFix[FunKitResultPhiPhi["1-Loop"]["Expression"] 
  //. FunKitResultPhiPhi["1-Loop"]["ExternalIndices"][[1, 2, 1]] -> p1 
  //. q -> FunKitResultPhiPhi["1-Loop"]["LoopMomenta"][[1, 1]]];

AppendTo[tests, TestCreate[FunKitResultPhiPhi, QMeSResultPhiPhi, TestID
   -> "Verify scalar field theory (QMeS): Propagator flow"]];
