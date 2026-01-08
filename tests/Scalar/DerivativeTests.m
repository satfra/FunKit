tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* Test scalar field theory *)

(**********************************************************************************
    QMeS
**********************************************************************************)

sQMeSSetup = GetQMeSWetterichSetupScalar[];

(**** Propagator ****)

QMeSdList2 = {Phi[i1], Phi[i2]};

QMeSdiag2Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiag2Idx = ReduceIdenticalFlowDiagrams[QMeSdiag2Idx, QMeSdList2];

QMeSRes2 = FunKitForm[QMeSdiag2Idx];

(**** Three-Point ****)

QMeSdList3 = {Phi[i1], Phi[i2], Phi[i3]};

QMeSdiag3Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList3, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiag3Idx = ReduceIdenticalFlowDiagrams[QMeSdiag3Idx, QMeSdList3];

QMeSRes3 = FunKitForm[QMeSdiag3Idx];

(**** Four-Point ****)

QMeSdList4 = {Phi[i1], Phi[i2], Phi[i3], Phi[i4]};

QMeSdiag4Idx = DeriveFunctionalEquation[sQMeSSetup, QMeSdList4, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiag4Idx = ReduceIdenticalFlowDiagrams[QMeSdiag4Idx, QMeSdList4];

QMeSRes4 = FunKitForm[QMeSdiag4Idx];

(**********************************************************************************
    DoFun
**********************************************************************************)

DoFunSetup = GetDoFunSetupScalar[];

(**** Propagator ****)

DoFundiag2Idx = wrapDoFun[DoFunSetup <> "doRGE[actionONSymbolic,{Phi,Phi}]"];

DoFunRes2 = FunKitForm[DoFundiag2Idx]

(**** Three-Point ****)

DoFundiag3Idx = wrapDoFun[DoFunSetup <> "doRGE[actionONSymbolic,{Phi,Phi,Phi}]"];

DoFunRes3 = FunKitForm[DoFundiag3Idx];

(**** Four-Point ****)

DoFundiag4Idx = wrapDoFun[DoFunSetup <> "doRGE[actionONSymbolic,{Phi,Phi,Phi,Phi}]"];

DoFunRes4 = FunKitForm[DoFundiag4Idx];

(**********************************************************************************
    FunKit
**********************************************************************************)

sFunKitSetup = GetFunKitSetupScalar[];

FSetGlobalSetup[sFunKitSetup];

(**** Propagator ****)

FunKitRes2 =
  FTakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2]}] //
  FTruncate //
  FSimplify;

(**** Three-Point ****)

FunKitRes3 =
  FTakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3]}] //
  FTruncate //
  FSimplify;

(**** Four-Point ****)

FunKitRes4 =
  FTakeDerivatives[sFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
  FTruncate //
  FSimplify;

(**********************************************************************************
    Comparison and Tests
**********************************************************************************)

result2QF = FEx[FunKitRes2, FTerm[-1, QMeSRes2]] // FSimplify;

result3QF = FEx[FunKitRes3, FTerm[-1, QMeSRes3]] // FSimplify;

result4QF = FEx[FunKitRes4, FTerm[-1, QMeSRes4]] // FSimplify;

AppendTo[tests, TestCreate[result2QF, FEx[], TestID -> "Verify scalar field theory (QMeS): Propagator flow"]];

AppendTo[tests, TestCreate[result3QF, FEx[], TestID -> "Verify scalar field theory (QMeS): Three-point flow"]];

AppendTo[tests, TestCreate[result4QF, FEx[], TestID -> "Verify scalar field theory (QMeS): Four-point flow"]];

result2DF = FEx[FunKitRes2, FTerm[-1, DoFunRes2]] // FSimplify;

result3DF = FEx[FunKitRes3, FTerm[-1, DoFunRes3]] // FSimplify;

result4DF = FEx[FunKitRes4, FTerm[-1, DoFunRes4]] // FSimplify;

AppendTo[tests, TestCreate[result2DF, FEx[], TestID -> "Verify scalar field theory (DoFun): Propagator flow"]];

AppendTo[tests, TestCreate[result3DF, FEx[], TestID -> "Verify scalar field theory (DoFun): Three-point flow"]];

AppendTo[tests, TestCreate[result4DF, FEx[], TestID -> "Verify scalar field theory (DoFun): Four-point flow"]];
