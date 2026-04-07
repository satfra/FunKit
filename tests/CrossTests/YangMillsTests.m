(* Cross-validation of Yang-Mills DSE and flow equations: FunKit vs QMeS vs DoFun *)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

ymFunKitSetup = GetFunKitSetupYangMills[];

ymQMeSDSESetup = GetQMeSDSESetupYangMills[];

ymQMeSWettSetup = GetQMeSWetterichSetupYangMills[];

ymDoFunSetup = GetDoFunSetupYangMills[];

FSetGlobalSetup[ymFunKitSetup];

(**********************************************************************************
    QMeS — DSE
**********************************************************************************)

(* Gluon propagator DSE *)

QMeSdListAA = {A[i1], A[i2]};

QMeSdiagAADSE = DeriveFunctionalEquation[ymQMeSDSESetup, QMeSdListAA, "OutputLevel" -> "SuperindexDiagrams"];

QMeSResAADSE = FunKitForm[ymFunKitSetup, QMeSdiagAADSE];

(* Ghost-gluon vertex DSE: cb DSE differentiated by c and A *)

QMeSdListAcbc = {cb[i1], c[i2], A[i3]};

QMeSdiagAcbcDSE = DeriveFunctionalEquation[ymQMeSDSESetup, QMeSdListAcbc, "OutputLevel" -> "SuperindexDiagrams"];

QMeSResAcbcDSE = FunKitForm[ymFunKitSetup, QMeSdiagAcbcDSE];

(* Ghost propagator DSE *)

QMeSdListcbc = {cb[i1], c[i2]};

QMeSdiagcbcDSE = DeriveFunctionalEquation[ymQMeSDSESetup, QMeSdListcbc, "OutputLevel" -> "SuperindexDiagrams"];

QMeSRescbcDSE = FunKitForm[ymFunKitSetup, QMeSdiagcbcDSE];

(**********************************************************************************
    QMeS — Wetterich flow
**********************************************************************************)

(* Gluon propagator flow *)

QMeSdListAA2 = {A[i1], A[i2]};

QMeSdiagAA2 = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAA2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagAA2 = ReduceIdenticalFlowDiagrams[QMeSdiagAA2, QMeSdListAA2];

QMeSResAA2 = FunKitForm[ymFunKitSetup, QMeSdiagAA2];

(* Ghost propagator flow *)

QMeSdListcbc2 = {cb[i1], c[i2]};

QMeSdiagcbc2 = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListcbc2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagcbc2 = ReduceIdenticalFlowDiagrams[QMeSdiagcbc2, QMeSdListcbc2];

QMeSRescbc2 = FunKitForm[ymFunKitSetup, QMeSdiagcbc2];

(* Ghost-gluon vertex flow *)

QMeSdListAcbc2 = {cb[i1], c[i2], A[i3]};

QMeSdiagAcbc2 = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAcbc2, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagAcbc2 = ReduceIdenticalFlowDiagrams[QMeSdiagAcbc2, QMeSdListAcbc2];

QMeSResAcbc2 = FunKitForm[ymFunKitSetup, QMeSdiagAcbc2];

(* Three-gluon vertex flow *)

QMeSdListAAA = {A[i1], A[i2], A[i3]};

QMeSdiagAAA = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAAA, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagAAA = ReduceIdenticalFlowDiagrams[QMeSdiagAAA, QMeSdListAAA];

QMeSResAAA = FunKitForm[ymFunKitSetup, QMeSdiagAAA];

(* Four-gluon vertex flow *)

QMeSdListAAAA = {A[i1], A[i2], A[i3], A[i4]};

QMeSdiagAAAA = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAAAA, "OutputLevel" -> "SuperindexDiagrams"];

QMeSdiagAAAA = ReduceIdenticalFlowDiagrams[QMeSdiagAAAA, QMeSdListAAAA];

QMeSResAAAA = FunKitForm[ymFunKitSetup, QMeSdiagAAAA];

(**********************************************************************************
    DoFun — DSE
**********************************************************************************)

(* Gluon propagator DSE *)

DoFundiagAADSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{A,A}]"];

DoFunResAADSE = FunKitForm[ymFunKitSetup, DoFundiagAADSE];

(* Ghost propagator DSE *)

DoFundiagcbcDSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{cb,c}]"];

DoFunRescbcDSE = FunKitForm[ymFunKitSetup, DoFundiagcbcDSE];

(**********************************************************************************
    DoFun — Wetterich flow
**********************************************************************************)

(* Gluon propagator flow *)

DoFundiagAA2 = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{A,A}]"];

DoFunResAA2 = FunKitForm[ymFunKitSetup, DoFundiagAA2];

(* Ghost propagator flow *)

DoFundiagcbc2 = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{cb,c}]"];

DoFunRescbc2 = FunKitForm[ymFunKitSetup, DoFundiagcbc2];

(* Ghost-gluon vertex flow *)

DoFundiagAcbc2 = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{cb, c, A}]"];

DoFunResAcbc2 = FunKitForm[ymFunKitSetup, DoFundiagAcbc2];

(* Three-gluon vertex flow *)

DoFundiagAAA = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{A,A,A}]"];

DoFunResAAA = FunKitForm[ymFunKitSetup, DoFundiagAAA];

(**********************************************************************************
    FunKit — DSE (with AutoSimplify disabled for raw diagram counts)
**********************************************************************************)

FSetAutoSimplify[False];

(* Gluon propagator DSE *)

FunKitResAADSERaw = FTakeDerivatives[ymFunKitSetup, FMakeDSE[ymFunKitSetup, A[i2]], {A[i1]}] // FTruncate;

(* Ghost-gluon vertex DSE: DSE of cb, differentiated by {c, A} *)

FunKitResAcbcDSERaw = FTakeDerivatives[ymFunKitSetup, FMakeDSE[ymFunKitSetup, A[i3]], {cb[i1], c[i2]}] // FTruncate;

(* Ghost propagator DSE *)

FunKitRescbcDSERaw = FTakeDerivatives[ymFunKitSetup, FMakeDSE[ymFunKitSetup, c[i2]], {cb[i1]}] // FTruncate;

FSetAutoSimplify[True];

FunKitResAADSE = FSimplify[FunKitResAADSERaw];

FunKitResAcbcDSE = FSimplify[FunKitResAcbcDSERaw];

FunKitRescbcDSE = FSimplify[FunKitRescbcDSERaw];

(* Symmetries for cross-test comparisons.
   FSimplify treats external indices as fixed labels — without explicit symmetries,
   it cannot match equivalent diagrams that differ by external index permutations.
   FunKit's pipeline auto-builds these via FTakeDerivatives, but the manually
   constructed comparison FEx lacks them. *)

symsAA = FMakeSymmetryList[ymFunKitSetup, {A[i1], A[i2]}];

symscbc = FunKit`Private`FBuildSymmetryList[ymFunKitSetup, {{{1, 2}, -1}}, {cb[i1], c[i2]}];

symsAcbc = FunKit`Private`FBuildSymmetryList[ymFunKitSetup, {{{1, 2}, -1}}, {cb[i1], c[i2], A[i3]}];

symsAcbc2 = FunKit`Private`FBuildSymmetryList[ymFunKitSetup, {{{2, 3}, -1}}, {cb[i1], c[i2], A[i3]}];

symsAAA = FMakeSymmetryList[ymFunKitSetup, {A[i1], A[i2], A[i3]}];

symsAAAA = FMakeSymmetryList[ymFunKitSetup, {A[i1], A[i2], A[i3], A[i4]}];

(**********************************************************************************
    FunKit — Wetterich flow
**********************************************************************************)

(* Gluon propagator flow *)

FunKitResAA2 = FTakeDerivatives[ymFunKitSetup, WetterichEquation, {A[i1], A[i2]}] // FTruncate;

(* Ghost propagator flow *)

FunKitRescbc2 = FTakeDerivatives[ymFunKitSetup, WetterichEquation, {cb[i1], c[i2]}] // FTruncate;

(* Ghost-gluon vertex flow *)

FunKitResAcbc2 = FTakeDerivatives[ymFunKitSetup, WetterichEquation, {cb[i1], c[i2], A[i3]}] // FTruncate;

(* Three-gluon vertex flow *)

FunKitResAAA = FTakeDerivatives[ymFunKitSetup, WetterichEquation, {A[i1], A[i2], A[i3]}] // FTruncate;

(* Four-gluon vertex flow *)

FunKitResAAAA = FTakeDerivatives[ymFunKitSetup, WetterichEquation, {A[i1], A[i2], A[i3], A[i4]}] // FTruncate;

(**********************************************************************************
    Diagram count cross-checks (raw, before identification/simplification)
    Compare FunKit (AutoSimplify off) vs QMeS (no ReduceIdentical) vs
    DoFun (identify->False)
**********************************************************************************)

(* DoFun raw (identify->False) for diagram count comparison *)

DoFunRawAcbcDSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{cb,c,A}, identify->False]"];

DoFuncountAcbcDSE = Length[DoFunRawAcbcDSE];

DoFunRawcbcDSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{cb,c}, identify->False]"];

DoFuncountcbcDSE = Length[DoFunRawcbcDSE];

DoFunRawAADSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{A,A}, identify->False]"];

DoFuncountAADSE = Length[DoFunRawAADSE];

(* FunKit raw counts — filter out Symmetries/annotation entries *)

FunKitcountAcbcDSE = Length[Select[List @@ FunKitResAcbcDSERaw, Head[#] === FTerm&]];

FunKitcountcbcDSE = Length[Select[List @@ FunKitRescbcDSERaw, Head[#] === FTerm&]];

FunKitcountAADSE = Length[Select[List @@ FunKitResAADSERaw, Head[#] === FTerm&]];

(* Note: QMeS DSE "SuperindexDiagrams" output is at a finer granularity
   than FunKit/DoFun (includes field permutations), so count comparison
   is only meaningful against DoFun. *)

AppendTo[tests, VerificationTest[FunKitcountcbcDSE, DoFuncountcbcDSE, TestID -> "Yang-Mills diagram count (FunKit vs DoFun): Ghost propagator DSE"]];

(**********************************************************************************
    Comparison: FunKit vs QMeS — DSE
**********************************************************************************)

resultAADSEQF = FSimplify[FEx[FunKitResAADSE, FTerm[-1, QMeSResAADSE]], "Symmetries" -> symsAA];

AppendTo[tests, VerificationTest[resultAADSEQF, FEx[], TestID -> "Verify Yang-Mills DSE (QMeS): Gluon propagator"]];

resultAcbcDSEQF = FSimplify[FEx[FunKitResAcbcDSE, FTerm[-1, QMeSResAcbcDSE]], "Symmetries" -> symsAcbc];

AppendTo[tests, VerificationTest[resultAcbcDSEQF, FEx[], TestID -> "Verify Yang-Mills DSE (QMeS): Ghost-gluon vertex"]];

resultcbcDSEQF = FSimplify[FEx[FunKitRescbcDSE, FTerm[-1, QMeSRescbcDSE]], "Symmetries" -> symscbc];

AppendTo[tests, VerificationTest[resultcbcDSEQF, FEx[], TestID -> "Verify Yang-Mills DSE (QMeS): Ghost propagator"]];

(**********************************************************************************
    Comparison: FunKit vs DoFun — DSE
    Can't really do this, as sign conventions are too different.
**********************************************************************************)

(**********************************************************************************
    Comparison: FunKit vs QMeS — Wetterich flow
**********************************************************************************)

resultAA2QF = FSimplify[FEx[FunKitResAA2, FTerm[-1, QMeSResAA2]], "Symmetries" -> symsAA];

AppendTo[tests, VerificationTest[resultAA2QF, FEx[], TestID -> "Verify Yang-Mills flow (QMeS): Gluon propagator"]];

resultcbc2QF = FSimplify[FEx[FunKitRescbc2, FTerm[-1, QMeSRescbc2]], "Symmetries" -> symscbc];

AppendTo[tests, VerificationTest[resultcbc2QF, FEx[], TestID -> "Verify Yang-Mills flow (QMeS): Ghost propagator"]];

resultAcbc2QF = FSimplify[FEx[FunKitResAcbc2, FTerm[-1, QMeSResAcbc2]], "Symmetries" -> symsAcbc2];

AppendTo[tests, VerificationTest[resultAcbc2QF, FEx[], TestID -> "Verify Yang-Mills flow (QMeS): Ghost-gluon vertex"]];

resultAAAQF = FSimplify[FEx[FunKitResAAA, FTerm[-1, QMeSResAAA]], "Symmetries" -> symsAAA];

AppendTo[tests, VerificationTest[resultAAAQF, FEx[], TestID -> "Verify Yang-Mills flow (QMeS): Three-gluon vertex"]];

resultAAAAQF = FSimplify[FEx[FunKitResAAAA, FTerm[-1, QMeSResAAAA]], "Symmetries" -> symsAAAA];

AppendTo[tests, VerificationTest[resultAAAAQF, FEx[], TestID -> "Verify Yang-Mills flow (QMeS): Four-gluon vertex"]];

(**********************************************************************************
    Comparison: FunKit vs DoFun — Wetterich flow
**********************************************************************************)

resultAA2DF = FSimplify[FEx[FunKitResAA2, FTerm[-1, DoFunResAA2]], "Symmetries" -> symsAA];

AppendTo[tests, VerificationTest[resultAA2DF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Gluon propagator"]];

resultcbc2DF = FSimplify[FEx[FunKitRescbc2, FTerm[-1, DoFunRescbc2]], "Symmetries" -> symscbc];

AppendTo[tests, VerificationTest[resultcbc2DF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Ghost propagator"]];
