tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* Utilities testing *)

(* Test a Yukawa theory *)

(**********************************************************************************
    QMeS
**********************************************************************************)

yQMeSSetup = GetQMeSWetterichSetupYukawa[];

(**** Fermion Propagator ****)

QMeSdListF2 = {Psi[i1], Psibar[i2]};

QMeSdiagF2Idx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListF2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagF2Idx = ReduceIdenticalFlowDiagrams[QMeSdiagF2Idx, QMeSdListF2];

QMeSResF2 = FunKitForm[QMeSdiagF2Idx];

(**** Scalar Propagator ****)

QMeSdListS2 = {Phi[i1], Phi[i2]};

QMeSdiagS2Idx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListS2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagS2Idx = ReduceIdenticalFlowDiagrams[QMeSdiagS2Idx, QMeSdListS2];

QMeSResS2 = FunKitForm[QMeSdiagS2Idx];

(**** Yukawa vertex ****)

QMeSdListYuk = {Psi[i1], Psibar[i2], Phi[i3]};

QMeSdiagYukIdx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListYuk, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagYukIdx = ReduceIdenticalFlowDiagrams[QMeSdiagYukIdx, QMeSdListYuk];

QMeSResYuk = FunKitForm[QMeSdiagYukIdx];

(**********************************************************************************
    DoFun
**********************************************************************************)

DoFunSetup = GetDoFunSetupYukawa[];

(**** Fermion Propagator ****)

DoFundiagF2Idx = wrapDoFun[DoFunSetup <> "doRGE[actionYukawaSymbolic,{Psi,Psibar}]"];

DoFunResF2 = FunKitForm[DoFundiagF2Idx];

(**** Scalar Propagator ****)

DoFundiagS2Idx = wrapDoFun[DoFunSetup <> "doRGE[actionYukawaSymbolic,{Phi,Phi}]"];

DoFunResS2 = FunKitForm[DoFundiagS2Idx];

(**** Yukawa vertex ****)

DoFundiagYukIdx = wrapDoFun[DoFunSetup <> "doRGE[actionYukawaSymbolic,{Psi,Psibar,Phi}]"];

DoFunResYuk = FunKitForm[DoFundiagYukIdx];

(**********************************************************************************
    FunKit
**********************************************************************************)

yFunKitSetup = GetFunKitSetupYukawa[];

SetGlobalSetup[yFunKitSetup]

(**** Fermion Propagator ****)

FunKitResF2 =
    TakeDerivatives[yFunKitSetup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
    FTruncate //
    FSimplify;

(**** Scalar Propagator ****)

FunKitResS2 =
    TakeDerivatives[yFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2]}] //
    FTruncate //
    FSimplify;

(**** Yukawa vertex ****)

FunKitResYuk =
    TakeDerivatives[yFunKitSetup, WetterichEquation, {Psi[i1], Psibar[i2], Phi[i3]}] //
    FTruncate //
    FSimplify;

(**********************************************************************************
    Comparison and Tests
**********************************************************************************)

resultF2QF = FEx[FunKitResF2, FTerm[-1, QMeSResF2]] // FSimplify;

resultS2QF = FEx[FunKitResS2, FTerm[-1, QMeSResS2]] // FSimplify;

resultYukQF = FEx[FunKitResYuk, FTerm[-1, QMeSResYuk]] // FSimplify;

AppendTo[tests, TestCreate[resultF2QF, FEx[], TestID -> "Verify Yukawa theory (QMeS): Fermion propagator flow"]];

AppendTo[tests, TestCreate[resultS2QF, FEx[], TestID -> "Verify Yukawa theory (QMeS): Scalar propagator flow"]];

AppendTo[tests, TestCreate[resultYukQF, FEx[], TestID -> "Verify Yukawa theory (QMeS): Yukawa vertex flow"]];

resultF2DF = FEx[FunKitResF2, FTerm[-1, DoFunResF2]] // FSimplify;

resultS2DF = FEx[FunKitResS2, FTerm[-1, DoFunResS2]] // FSimplify;

AppendTo[tests, TestCreate[resultF2DF, FEx[], TestID -> "Verify Yukawa theory (DoFun): Fermion propagator flow"]];

AppendTo[tests, TestCreate[resultS2DF, FEx[], TestID -> "Verify Yukawa theory (DoFun): Scalar propagator flow"]];
