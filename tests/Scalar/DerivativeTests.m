tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* Utilities testing *)

(* Test scalar field theory *)

(**************** QMeS ****************)

sQMeSSetup = GetQMeSWetterichSetupScalar[];

(**** Propagator ****)

QMeSdList2 = {Phi[i1], Phi[i2]};

QMeSdiag2Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiag2Idx = ReduceIdenticalFlowDiagrams[QMeSdiag2Idx, QMeSdList2];

QMeSRes2 = QMeSdiag2Idx // FunKitForm;

(**** Three-Point ****)

QMeSdList3 = {Phi[i1], Phi[i2], Phi[i3]};

QMeSdiag3Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList3, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiag3Idx = ReduceIdenticalFlowDiagrams[QMeSdiag3Idx, QMeSdList3];

QMeSRes3 = QMeSdiag3Idx // FunKitForm;

(**** Four-Point ****)

QMeSdList4 = {Phi[i1], Phi[i2], Phi[i3], Phi[i4]};

QMeSdiag4Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList4, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiag4Idx = ReduceIdenticalFlowDiagrams[QMeSdiag4Idx, QMeSdList4];

QMeSRes4 = QMeSdiag4Idx // FunKitForm;

(**************** FunKit ****************)

sFunKitSetup = GetFunKitSetupScalar[];

SetGlobalSetup[sFunKitSetup]

(**** Propagator ****)

FunKitRes2 =
  TakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2]}] //
  FTruncate //
  FSimplify;

(**** Three-Point ****)

FunKitRes3 =
  TakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3]}] //
  FTruncate //
  FSimplify;

(**** Four-Point ****)

FunKitRes4 =
  TakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
  FTruncate //
  FSimplify;

(**************** Comparison and Tests ****************)

result2 = FEx[FunKitRes2, FTerm[-1, QMeSRes2]] // FSimplify;

result3 = FEx[FunKitRes3, FTerm[-1, QMeSRes3]] // FSimplify;

result4 = FEx[FunKitRes4, FTerm[-1, QMeSRes4]] // FSimplify;

AppendTo[tests, TestCreate[result2, FEx[], TestID -> "Verify scalar field theory (QMeS): Propagator flow"]];

AppendTo[tests, TestCreate[result3, FEx[], TestID -> "Verify scalar field theory (QMeS): Three-point flow"]];

AppendTo[tests, TestCreate[result4, FEx[], TestID -> "Verify scalar field theory (QMeS): Four-point flow"]];
