tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* Utilities testing *)

(* Test a Yukawa theory *)

(**************** QMeS ****************)

yQMeSSetup = GetQMeSWetterichSetupYukawa[];

(**** Fermion Propagator ****)

QMeSdListF2 = {Psi[i1], Psibar[i2]};

QMeSdiagF2Idx = DeriveFunctionalEquation[yQMeSSetup, QMeSdListF2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagF2Idx = ReduceIdenticalFlowDiagrams[QMeSdiagF2Idx, QMeSdListF2];

QMeSResF2 = QMeSdiagF2Idx // FunKitForm;

(**************** FunKit ****************)

yFunKitSetup = GetFunKitSetupYukawa[];

SetGlobalSetup[yFunKitSetup]

(**** Fermion Propagator ****)

FunKitResF2 =
    TakeDerivatives[yFunKitSetup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
    FTruncate //
    FSimplify;

(**************** Comparison and Tests ****************)

resultF2 = FEx[FunKitResF2, FTerm[-1, QMeSResF2]] // FSimplify;

AppendTo[tests, TestCreate[resultF2, FEx[], TestID -> "Verify Yukawa theory (QMeS): Fermion propagator flow"]];

(**** End of tests ****) 