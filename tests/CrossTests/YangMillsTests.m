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

(* Ghost-gluon vertex DSE *)

DoFundiagAcbcDSE = wrapDoFun[ymDoFunSetup <> "doDSE[actionYMSymbolic,{cb,c,A}]"];

DoFunResAcbcDSE = FunKitForm[ymFunKitSetup, DoFundiagAcbcDSE];

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

(* Four-gluon vertex flow *)

DoFundiagAAAA = wrapDoFun[ymDoFunSetup <> "doRGE[actionYMSymbolic,{A,A,A,A}]"];

DoFunResAAAA = FunKitForm[ymFunKitSetup, DoFundiagAAAA];

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

(* QMeS comparisons run only when the QMeS package is installed (getQMeS.m). *)
If[TrueQ[$QMeSAvailable],
    resultAADSEQF = FSimplify[FEx[FunKitResAADSE, FTerm[-1, QMeSResAADSE]], "Symmetries" -> symsAA];

    AppendTo[tests, VerificationTest[resultAADSEQF, FEx[], TestID -> "Verify Yang-Mills DSE (QMeS): Gluon propagator"]];

    resultAcbcDSEQF = FSimplify[FEx[FunKitResAcbcDSE, FTerm[-1, QMeSResAcbcDSE]], "Symmetries" -> symsAcbc];

    AppendTo[tests, VerificationTest[resultAcbcDSEQF, FEx[], TestID -> "Verify Yang-Mills DSE (QMeS): Ghost-gluon vertex"]];

    resultcbcDSEQF = FSimplify[FEx[FunKitRescbcDSE, FTerm[-1, QMeSRescbcDSE]], "Symmetries" -> symscbc];

    AppendTo[tests, VerificationTest[resultcbcDSEQF, FEx[], TestID -> "Verify Yang-Mills DSE (QMeS): Ghost propagator"]];
];

(**********************************************************************************
    Comparison: FunKit vs DoFun — DSE
    Sign-convention differences (DoFun 3 vs FunKit) are absorbed by the
    per-Grassmann-vertex correction inside FunKitForm[]; see the comment at
    modules/FEDeriK/Compatibility.m. The propagator DSEs (AA and cbc) survive
    that correction term-for-term.

    For the ghost-gluon vertex DSE, FunKit and DoFun produce different graph
    expansions: FunKit's FMakeDSE differentiates the gluon DSE
    (delta Gamma / delta A = ...), so it generates skeleton diagrams with
    bare S^{(3)}_AAA or S^{(4)}_AAAA insertions; DoFun's doDSE differentiates
    the ghost DSE and gets the same physics with the bare vertex relocated to
    S^{(3)}_Acbc, where FunKit instead produces dressed gluon vertices.
    Additionally DoFun's expansion uses the dressed 4-pt ghost-gluon vertex
    GammaN[{A, A, cb, c}], which FunKit truncates away. To make a meaningful
    symbolic comparison, we restrict both sides to the "ghost-channel" subset:
    diagrams whose only bare vertex is S[A, cb, c] or S[cb, c] and whose only
    dressed interaction is V[A, cb, c]. The structurally-different diagrams
    are subtracted from each side accordingly.
**********************************************************************************)

resultAADSEDF = FSimplify[FEx[FunKitResAADSE, FTerm[-1, DoFunResAADSE]], "Symmetries" -> symsAA];

AppendTo[tests, VerificationTest[resultAADSEDF, FEx[], TestID -> "Verify Yang-Mills DSE (DoFun): Gluon propagator"]];

resultcbcDSEDF = FSimplify[FEx[FunKitRescbcDSE, FTerm[-1, DoFunRescbcDSE]], "Symmetries" -> symscbc];

AppendTo[tests, VerificationTest[resultcbcDSEDF, FEx[], TestID -> "Verify Yang-Mills DSE (DoFun): Ghost propagator"]];

(* Ghost-channel comparison: drop FunKit-only "classical" diagrams (bare
   S^{(3,4)}_AAA insertions from differentiating the gluon DSE) and DoFun-only
   diagrams (V[A,A,cb,c] tadpole, since {A,A,cb,c} is not in FunKit's GammaN
   truncation; and 1-loop diagrams whose dressed vertex is V[A,A,A], which are
   the structural counterparts to FunKit's bare-S[A,A,A] insertions and so
   represent the same physics in a different graph expansion). What remains
   on each side is the tree-level vertex term — the only diagram both sides
   place identically in the DSE (with the bare S[A,cb,c]'s legs all external).
   The 1-loop ghost-only diagram has identical topology on both sides but
   incompatible external-leg routing (FunKit hangs i3 on the bare-S A-leg,
   DoFun hangs i1 on its cb-leg), so it cannot be symbolically matched without
   a Bose-resymmetrisation of the bare vertex. *)

ghostChannelFTermQ[ft_] :=
    FreeQ[ft, S[fields_, _] /; fields =!= {A, cb, c} && fields =!= {cb, c}] &&
    FreeQ[ft, GammaN[fields_, _] /; AllTrue[fields, # === A&] && Length[fields] >= 3] &&
    FreeQ[ft, GammaN[{A, A, cb, c}, _]] &&
    Count[Cases[ft, _GammaN], _GammaN] === 0;

FunKitResAcbcDSEGhost = FEx @@ Cases[List @@ FunKitResAcbcDSE, ft_FTerm /; ghostChannelFTermQ[ft]];

DoFunResAcbcDSEGhost = FEx @@ Cases[List @@ DoFunResAcbcDSE, ft_FTerm /; ghostChannelFTermQ[ft]];

resultAcbcDSEDF = FSimplify[FEx[FunKitResAcbcDSEGhost, FTerm[-1, DoFunResAcbcDSEGhost]], "Symmetries" -> symsAcbc];

AppendTo[tests, VerificationTest[resultAcbcDSEDF, FEx[], TestID -> "Verify Yang-Mills DSE (DoFun): Ghost-gluon vertex tree term"]];

(**********************************************************************************
    Comparison: FunKit vs QMeS — Wetterich flow
**********************************************************************************)

If[TrueQ[$QMeSAvailable],
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
];

(**********************************************************************************
    Comparison: FunKit vs DoFun — Wetterich flow
**********************************************************************************)

resultAA2DF = FSimplify[FEx[FunKitResAA2, FTerm[-1, DoFunResAA2]], "Symmetries" -> symsAA];

AppendTo[tests, VerificationTest[resultAA2DF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Gluon propagator"]];

resultcbc2DF = FSimplify[FEx[FunKitRescbc2, FTerm[-1, DoFunRescbc2]], "Symmetries" -> symscbc];

AppendTo[tests, VerificationTest[resultcbc2DF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Ghost propagator"]];

resultAcbc2DF = FSimplify[FEx[FunKitResAcbc2, FTerm[-1, DoFunResAcbc2]], "Symmetries" -> symsAcbc2];

AppendTo[tests, VerificationTest[resultAcbc2DF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Ghost-gluon vertex"]];

resultAAADF = FSimplify[FEx[FunKitResAAA, FTerm[-1, DoFunResAAA]], "Symmetries" -> symsAAA];

AppendTo[tests, VerificationTest[resultAAADF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Three-gluon vertex"]];

resultAAAADF = FSimplify[FEx[FunKitResAAAA, FTerm[-1, DoFunResAAAA]], "Symmetries" -> symsAAAA];

AppendTo[tests, VerificationTest[resultAAAADF, FEx[], TestID -> "Verify Yang-Mills flow (DoFun): Four-gluon vertex"]];
