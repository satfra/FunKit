(* Cross-validation of Yang-Mills DSE and flow equations: FunKit vs QMeS vs DoFun *)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

ymFunKitSetup = GetFunKitSetupYangMills[];
ymQMeSDSESetup = GetQMeSDSESetupYangMills[];
ymQMeSWettSetup = GetQMeSWetterichSetupYangMills[];
ymDoFunSetup = GetDoFunSetupYangMills[];

FSetGlobalSetup[ymFunKitSetup];

(* Helper: strip tree-level (propagator-free) terms from an FEx.
   FSimplify cannot cancel tree-level S-vertex terms because its graph-based
   comparison requires closed (loop) indices. The classical action part is
   an input, so we only need to cross-validate the loop corrections. *)
DropTreeLevel[expr_FEx] :=
    FEx @@ Select[List @@ expr, !FreeQ[#, Propagator[__], Infinity]&];

(**********************************************************************************
    QMeS — DSE
**********************************************************************************)

(* Gluon propagator DSE *)

QMeSdListAA = {A[i1], A[i2]};
QMeSdiagAADSE = DeriveFunctionalEquation[ymQMeSDSESetup, QMeSdListAA, "OutputLevel" -> "SuperindexDiagrams"];
QMeSResAADSE = FunKitForm[QMeSdiagAADSE];

(* Ghost-gluon vertex DSE: cb DSE differentiated by c and A *)

QMeSdListAcbc = {cb[i1], c[i2], A[i3]};
QMeSdiagAcbcDSE = DeriveFunctionalEquation[ymQMeSDSESetup, QMeSdListAcbc, "OutputLevel" -> "SuperindexDiagrams"];
QMeSResAcbcDSE = FunKitForm[QMeSdiagAcbcDSE];

(* Ghost propagator DSE *)

QMeSdListcbc = {cb[i1], c[i2]};
QMeSdiagcbcDSE = DeriveFunctionalEquation[ymQMeSDSESetup, QMeSdListcbc, "OutputLevel" -> "SuperindexDiagrams"];
QMeSRescbcDSE = FunKitForm[QMeSdiagcbcDSE];

(**********************************************************************************
    QMeS — Wetterich flow
**********************************************************************************)

(* Gluon propagator flow *)

QMeSdListAA2 = {A[i1], A[i2]};
QMeSdiagAA2 = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAA2, "OutputLevel" -> "SuperindexDiagrams"];
QMeSdiagAA2 = ReduceIdenticalFlowDiagrams[QMeSdiagAA2, QMeSdListAA2];
QMeSResAA2 = FunKitForm[QMeSdiagAA2];

(* Ghost propagator flow *)

QMeSdListcbc2 = {cb[i1], c[i2]};
QMeSdiagcbc2 = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListcbc2, "OutputLevel" -> "SuperindexDiagrams"];
QMeSdiagcbc2 = ReduceIdenticalFlowDiagrams[QMeSdiagcbc2, QMeSdListcbc2];
QMeSRescbc2 = FunKitForm[QMeSdiagcbc2];

(* Ghost-gluon vertex flow *)

QMeSdListAcbc2 = {A[i1], cb[i2], c[i3]};
QMeSdiagAcbc2 = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAcbc2, "OutputLevel" -> "SuperindexDiagrams"];
QMeSdiagAcbc2 = ReduceIdenticalFlowDiagrams[QMeSdiagAcbc2, QMeSdListAcbc2];
QMeSResAcbc2 = FunKitForm[QMeSdiagAcbc2];

(* Three-gluon vertex flow *)

QMeSdListAAA = {A[i1], A[i2], A[i3]};
QMeSdiagAAA = DeriveFunctionalEquation[ymQMeSWettSetup, QMeSdListAAA, "OutputLevel" -> "SuperindexDiagrams"];
QMeSdiagAAA = ReduceIdenticalFlowDiagrams[QMeSdiagAAA, QMeSdListAAA];
QMeSResAAA = FunKitForm[QMeSdiagAAA];

(**********************************************************************************
    DoFun — DSE
**********************************************************************************)

(* Gluon propagator DSE *)

DoFundiagAADSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{A,A}]"];
DoFunResAADSE = FunKitForm[DoFundiagAADSE];

(* Ghost-gluon vertex DSE *)

DoFundiagAcbcDSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{cb,c,A}]"];
DoFunResAcbcDSE = FunKitForm[DoFundiagAcbcDSE];

(* Ghost propagator DSE *)

DoFundiagcbcDSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{cb,c}]"];
DoFunRescbcDSE = FunKitForm[DoFundiagcbcDSE];

(**********************************************************************************
    DoFun — Wetterich flow
**********************************************************************************)

(* Gluon propagator flow *)

DoFundiagAA2 = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{A,A}]"];
DoFunResAA2 = FunKitForm[DoFundiagAA2];

(* Ghost propagator flow *)

DoFundiagcbc2 = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{cb,c}]"];
DoFunRescbc2 = FunKitForm[DoFundiagcbc2];

(* Ghost-gluon vertex flow *)

DoFundiagAcbc2 = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{A,cb,c}]"];
DoFunResAcbc2 = FunKitForm[DoFundiagAcbc2];

(* Three-gluon vertex flow *)

DoFundiagAAA = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{A,A,A}]"];
DoFunResAAA = FunKitForm[DoFundiagAAA];

(**********************************************************************************
    FunKit — DSE (with AutoSimplify disabled for raw diagram counts)
**********************************************************************************)

FSetAutoSimplify[False];

(* Gluon propagator DSE *)

FunKitResAADSERaw =
    FTakeDerivatives[ymFunKitSetup, FMakeDSE[ymFunKitSetup, A[i1]], {A[i2]}] //
    FTruncate;

(* Ghost-gluon vertex DSE: DSE of cb, differentiated by {c, A} *)

FunKitResAcbcDSERaw =
    FTakeDerivatives[ymFunKitSetup, FMakeDSE[ymFunKitSetup, cb[i1]], {c[i2], A[i3]}] //
    FTruncate;

(* Ghost propagator DSE *)

FunKitRescbcDSERaw =
    FTakeDerivatives[ymFunKitSetup, FMakeDSE[ymFunKitSetup, cb[i1]], {c[i2]}] //
    FTruncate;

FSetAutoSimplify[True];

FunKitResAADSE = FSimplify[FunKitResAADSERaw];
FunKitResAcbcDSE = FSimplify[FunKitResAcbcDSERaw];
FunKitRescbcDSE = FSimplify[FunKitRescbcDSERaw];

(**********************************************************************************
    FunKit — Wetterich flow
**********************************************************************************)

(* Gluon propagator flow *)

FunKitResAA2 =
    FTakeDerivatives[ymFunKitSetup, WetterichEquation, {A[i1], A[i2]}] //
    FTruncate //
    FSimplify;

(* Ghost propagator flow *)

FunKitRescbc2 =
    FTakeDerivatives[ymFunKitSetup, WetterichEquation, {cb[i1], c[i2]}] //
    FTruncate //
    FSimplify;

(* Ghost-gluon vertex flow *)

FunKitResAcbc2 =
    FTakeDerivatives[ymFunKitSetup, WetterichEquation, {A[i1], cb[i2], c[i3]}] //
    FTruncate //
    FSimplify;

(* Three-gluon vertex flow *)

FunKitResAAA =
    FTakeDerivatives[ymFunKitSetup, WetterichEquation, {A[i1], A[i2], A[i3]}] //
    FTruncate //
    FSimplify;

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

AppendTo[tests, TestCreate[FunKitcountcbcDSE, DoFuncountcbcDSE,
    TestID -> "Yang-Mills diagram count (FunKit vs DoFun): Ghost propagator DSE"]];

(**********************************************************************************
    Comparison: FunKit vs QMeS — DSE
**********************************************************************************)

resultAADSEQF = FEx[DropTreeLevel[FunKitResAADSE], FTerm[-1, DropTreeLevel[QMeSResAADSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultAADSEQF, FEx[],
    TestID -> "Verify Yang-Mills DSE (QMeS): Gluon propagator (loop corrections)"]];

resultAcbcDSEQF = FEx[DropTreeLevel[FunKitResAcbcDSE], FTerm[-1, DropTreeLevel[QMeSResAcbcDSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultAcbcDSEQF, FEx[],
    TestID -> "Verify Yang-Mills DSE (QMeS): Ghost-gluon vertex (loop corrections)"]];

resultcbcDSEQF = FEx[DropTreeLevel[FunKitRescbcDSE], FTerm[-1, DropTreeLevel[QMeSRescbcDSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultcbcDSEQF, FEx[],
    TestID -> "Verify Yang-Mills DSE (QMeS): Ghost propagator (loop corrections)"]];


(**********************************************************************************
    Comparison: FunKit vs DoFun — DSE
**********************************************************************************)

resultAADSEDF = FEx[DropTreeLevel[FunKitResAADSE], FTerm[-1, DropTreeLevel[DoFunResAADSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultAADSEDF, FEx[],
    TestID -> "Verify Yang-Mills DSE (DoFun): Gluon propagator (loop corrections)"]];

resultAcbcDSEDF = FEx[DropTreeLevel[FunKitResAcbcDSE], FTerm[-1, DropTreeLevel[DoFunResAcbcDSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultAcbcDSEDF, FEx[],
    TestID -> "Verify Yang-Mills DSE (DoFun): Ghost-gluon vertex (loop corrections)"]];

resultcbcDSEDF = FEx[DropTreeLevel[FunKitRescbcDSE], FTerm[-1, DropTreeLevel[DoFunRescbcDSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultcbcDSEDF, FEx[],
    TestID -> "Verify Yang-Mills DSE (DoFun): Ghost propagator (loop corrections)"]];


(**********************************************************************************
    Comparison: FunKit vs QMeS — Wetterich flow
**********************************************************************************)

resultAA2QF = FEx[FunKitResAA2, FTerm[-1, QMeSResAA2]] // FSimplify;
AppendTo[tests, TestCreate[resultAA2QF, FEx[],
    TestID -> "Verify Yang-Mills flow (QMeS): Gluon propagator"]];

resultcbc2QF = FEx[FunKitRescbc2, FTerm[-1, QMeSRescbc2]] // FSimplify;
AppendTo[tests, TestCreate[resultcbc2QF, FEx[],
    TestID -> "Verify Yang-Mills flow (QMeS): Ghost propagator"]];

resultAcbc2QF = FEx[FunKitResAcbc2, FTerm[-1, QMeSResAcbc2]] // FSimplify;
AppendTo[tests, TestCreate[resultAcbc2QF, FEx[],
    TestID -> "Verify Yang-Mills flow (QMeS): Ghost-gluon vertex"]];

resultAAAQF = FEx[FunKitResAAA, FTerm[-1, QMeSResAAA]] // FSimplify;
AppendTo[tests, TestCreate[resultAAAQF, FEx[],
    TestID -> "Verify Yang-Mills flow (QMeS): Three-gluon vertex"]];

(**********************************************************************************
    Comparison: FunKit vs DoFun — Wetterich flow
**********************************************************************************)

resultAA2DF = FEx[FunKitResAA2, FTerm[-1, DoFunResAA2]] // FSimplify;
AppendTo[tests, TestCreate[resultAA2DF, FEx[],
    TestID -> "Verify Yang-Mills flow (DoFun): Gluon propagator"]];

resultcbc2DF = FEx[FunKitRescbc2, FTerm[-1, DoFunRescbc2]] // FSimplify;
AppendTo[tests, TestCreate[resultcbc2DF, FEx[],
    TestID -> "Verify Yang-Mills flow (DoFun): Ghost propagator"]];

resultAcbc2DF = FEx[FunKitResAcbc2, FTerm[-1, DoFunResAcbc2]] // FSimplify;
AppendTo[tests, TestCreate[resultAcbc2DF, FEx[],
    TestID -> "Verify Yang-Mills flow (DoFun): Ghost-gluon vertex"]];

resultAAADF = FEx[FunKitResAAA, FTerm[-1, DoFunResAAA]] // FSimplify;
AppendTo[tests, TestCreate[resultAAADF, FEx[],
    TestID -> "Verify Yang-Mills flow (DoFun): Three-gluon vertex"]];

(**********************************************************************************
    Comparison: QMeS vs DoFun (independent cross-check for ghost-gluon)
**********************************************************************************)

resultAcbcDSEQD = FEx[DropTreeLevel[QMeSResAcbcDSE], FTerm[-1, DropTreeLevel[DoFunResAcbcDSE]]] // FSimplify;
AppendTo[tests, TestCreate[resultAcbcDSEQD, FEx[],
    TestID -> "Verify Yang-Mills DSE (QMeS vs DoFun): Ghost-gluon vertex (loop corrections)"]];
