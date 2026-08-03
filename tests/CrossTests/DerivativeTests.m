(* Testing functional derivatives and simplifications: Yukawa theory *)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    QMeS
**********************************************************************************)

yFunKitSetup = GetFunKitSetupYukawa[];
yQMeSSetup = GetQMeSWetterichSetupYukawa[];

(**** Fermion Propagator ****)

QMeSdListF2 = {Psi[i1], Psibar[i2]};

QMeSdiagF2Idx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListF2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagF2Idx = ReduceIdenticalFlowDiagrams[QMeSdiagF2Idx, QMeSdListF2];

QMeSResF2 = FunKitForm[yFunKitSetup, QMeSdiagF2Idx];

(**** Scalar Propagator ****)

QMeSdListS2 = {Phi[i1], Phi[i2]};

QMeSdiagS2Idx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListS2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagS2Idx = ReduceIdenticalFlowDiagrams[QMeSdiagS2Idx, QMeSdListS2];

QMeSResS2 = FunKitForm[yFunKitSetup, QMeSdiagS2Idx];

(**** Yukawa vertex ****)

QMeSdListYuk = {Psi[i1], Psibar[i2], Phi[i3]};

QMeSdiagYukIdx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListYuk, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagYukIdx = ReduceIdenticalFlowDiagrams[QMeSdiagYukIdx, QMeSdListYuk];

QMeSResYuk = FunKitForm[yFunKitSetup, QMeSdiagYukIdx];

(**********************************************************************************
    DoFun
**********************************************************************************)

DoFunSetup = GetDoFunSetupYukawa[];

(**** Fermion Propagator ****)

DoFundiagF2Idx = wrapDoFun[DoFunSetup <> "doRGE[actionYukawaSymbolic,{Psi,Psibar}]"];

DoFunResF2 = FunKitForm[yFunKitSetup, DoFundiagF2Idx];

(**** Scalar Propagator ****)

DoFundiagS2Idx = wrapDoFun[DoFunSetup <> "doRGE[actionYukawaSymbolic,{Phi,Phi}]"];

DoFunResS2 = FunKitForm[yFunKitSetup, DoFundiagS2Idx];

(**** Yukawa vertex ****)

DoFundiagYukIdx = wrapDoFun[DoFunSetup <> "doRGE[actionYukawaSymbolic,{Psi,Psibar,Phi}]"];

DoFunResYuk = FunKitForm[yFunKitSetup, DoFundiagYukIdx];

(**********************************************************************************
    FunKit
**********************************************************************************)

FSetGlobalSetup[yFunKitSetup];

(**** Fermion Propagator ****)

FunKitResF2 =
    FTakeDerivatives[yFunKitSetup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
    FTruncate //
    FSimplify;

(**** Scalar Propagator ****)

(*Two identical external bosons. DoFun and QMeS present this flow in its
  exchange-symmetric form, so the exchange has to be declared here -- since
  $AutoBuildSymmetryList defaults to False, FunKit no longer assumes it.*)
FunKitResS2 =
    FTakeDerivatives[yFunKitSetup, WetterichEquation, {Phi[i1], Phi[i2]},
        "Symmetries" -> FMakeSymmetryList[FSymmetry[Symmetric, {i1, i2}]]] //
    FTruncate //
    FSimplify;

(**** Yukawa vertex ****)

FunKitResYuk =
    FTakeDerivatives[yFunKitSetup, WetterichEquation, {Psi[i1], Psibar[i2], Phi[i3]}] //
    FTruncate //
    FSimplify;

(**********************************************************************************
    Comparison and Tests
**********************************************************************************)

(* QMeS comparison: only run when the QMeS package is actually installed (see
   tests/util/getQMeS.m). Otherwise DeriveFunctionalEquation/ReduceIdenticalFlow-
   Diagrams stay unevaluated and these would fail spuriously. *)
If[TrueQ[$QMeSAvailable],
    resultF2QF = FEx[FunKitResF2, FTerm[-1, QMeSResF2]] // FSimplify;

    resultS2QF = FEx[FunKitResS2, FTerm[-1, QMeSResS2]] // FSimplify;

    resultYukQF = FEx[FunKitResYuk, FTerm[-1, QMeSResYuk]] // FSimplify;

    AppendTo[tests, VerificationTest[resultF2QF, FEx[], TestID -> "Verify Yukawa theory (QMeS): Fermion propagator flow"]];

    AppendTo[tests, VerificationTest[resultS2QF, FEx[], TestID -> "Verify Yukawa theory (QMeS): Scalar propagator flow"]];

    AppendTo[tests, VerificationTest[resultYukQF, FEx[], TestID -> "Verify Yukawa theory (QMeS): Yukawa vertex flow"]];
];

resultF2DF = FEx[FunKitResF2, FTerm[-1, DoFunResF2]] // FSimplify;

resultS2DF = FEx[FunKitResS2, FTerm[-1, DoFunResS2]] // FSimplify;

resultYukDF = FEx[FunKitResYuk, FTerm[-1, DoFunResYuk]] // FSimplify;

AppendTo[tests, VerificationTest[resultF2DF, FEx[], TestID -> "Verify Yukawa theory (DoFun): Fermion propagator flow"]];

AppendTo[tests, VerificationTest[resultS2DF, FEx[], TestID -> "Verify Yukawa theory (DoFun): Scalar propagator flow"]];

AppendTo[tests, VerificationTest[resultYukDF, FEx[], TestID -> "Verify Yukawa theory (DoFun): Yukawa vertex flow"]];
