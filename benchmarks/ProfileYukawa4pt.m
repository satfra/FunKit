(* ProfileYukawa4pt.m — Profile where time is spent in Yukawa 4-pt derivation *)

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

setup = GetFunKitSetupYukawa[];
FSetGlobalSetup[setup];
FSetAutoSimplify[False];
derivList = {Phi[i1], Phi[i2], Phi[i3], Phi[i4]};

(* Warmup *)
Print["Warmup..."];
FTakeDerivatives[setup, WetterichEquation, derivList] // FTruncate;

(* Initialize all profiling counters *)
resetCounters[] := (
    FunKit`Private`$ReduceIndicesTime = 0.; FunKit`Private`$ReduceIndicesCount = 0;
    FunKit`Private`$FixIndicesTime = 0.; FunKit`Private`$FixIndicesCount = 0;
    FunKit`Private`$ProfileFDOp = 0.;
    FunKit`Private`$ProfileDerivSimplify = 0.;
    FunKit`Private`$ProfileLTrunc = 0.;
    FunKit`Private`$ProfilePostRI = 0.;
    FunKit`Private`$ProfileFixOrder = 0.;
    FunKit`Private`$ProfileFSimplify = 0.;
    FunKit`Private`$ProfileLTruncDetail = True;
    FunKit`Private`$ProfileLTruncExtract = 0.;
    FunKit`Private`$ProfileLTruncExpand = 0.;
    FunKit`Private`$ProfileLTruncCalls = 0;
    FunKit`Private`$ProfileLTruncPairs = 0;
);

(* ========== Profile FTakeDerivatives ========== *)
FSetDebugLevel[1]; (* shows per-pass term counts *)
resetCounters[];
{tDeriv, derivRes} = AbsoluteTiming[FTakeDerivatives[setup, WetterichEquation, derivList]];
FSetDebugLevel[0];

Print[""];
Print["=== FTakeDerivatives: ", NumberForm[tDeriv, {5,3}], " s ==="];
Print["  Terms produced: ", Length[derivRes]];
Print["  FResolveFDOpInternal (all passes): ", NumberForm[FunKit`Private`$ProfileFDOp, {5,3}], " s"];
Print["  Mid-deriv FSimplify+RI:            ", NumberForm[FunKit`Private`$ProfileDerivSimplify, {5,3}], " s"];
Print["  ReduceIndices:                     ", NumberForm[FunKit`Private`$ReduceIndicesTime, {5,3}], " s  (", FunKit`Private`$ReduceIndicesCount, " calls)"];
Print["  FixIndices:                        ", NumberForm[FunKit`Private`$FixIndicesTime, {5,3}], " s  (", FunKit`Private`$FixIndicesCount, " calls)"];
Print[""];

(* ========== Profile FTruncate ========== *)
resetCounters[];
{tTrunc, truncRes} = AbsoluteTiming[FTruncate[setup, derivRes]];

Print["=== FTruncate: ", NumberForm[tTrunc, {5,3}], " s ==="];
Print["  Input terms:  ", Length[derivRes]];
Print["  Output terms: ", Length[truncRes]];
Print["  LTrunc:                           ", NumberForm[FunKit`Private`$ProfileLTrunc, {5,3}], " s"];
Print["    Extract+Pairs:                  ", NumberForm[FunKit`Private`$ProfileLTruncExtract, {5,3}], " s"];
Print["    Expansion loop:                 ", NumberForm[FunKit`Private`$ProfileLTruncExpand, {5,3}], " s"];
Print["    Calls with AnyField:            ", FunKit`Private`$ProfileLTruncCalls, "  (avg pairs: ",
      If[FunKit`Private`$ProfileLTruncCalls > 0, N[FunKit`Private`$ProfileLTruncPairs/FunKit`Private`$ProfileLTruncCalls, 3], 0], ")"];
Print["  Post-LTrunc ReduceIndices:        ", NumberForm[FunKit`Private`$ProfilePostRI, {5,3}], " s"];
Print["  FixIndices+OrderFields:            ", NumberForm[FunKit`Private`$ProfileFixOrder, {5,3}], " s"];
Print["  FSimplify ($AutoSimplify):         ", NumberForm[FunKit`Private`$ProfileFSimplify, {5,3}], " s"];
Print["  ReduceIndices total:               ", NumberForm[FunKit`Private`$ReduceIndicesTime, {5,3}], " s  (", FunKit`Private`$ReduceIndicesCount, " calls)"];
Print["  FixIndices total:                  ", NumberForm[FunKit`Private`$FixIndicesTime, {5,3}], " s  (", FunKit`Private`$FixIndicesCount, " calls)"];
Print[""];

Print["=== FunKit Grand Total: ", NumberForm[tDeriv + tTrunc, {5,3}], " s ==="];
Print[""];

(* ========== Profile QMeS for comparison ========== *)
qmesSetup = GetQMeSWetterichSetupYukawa[];
qmesDerivList = {Phi[i1], Phi[i2], Phi[i3], Phi[i4]};

(* Warmup *)
DeriveFunctionalEquation[qmesSetup, qmesDerivList, "OutputLevel" -> "SuperindexDiagrams"];

{tQMeS, qmesRes} = AbsoluteTiming[
    DeriveFunctionalEquation[qmesSetup, qmesDerivList, "OutputLevel" -> "SuperindexDiagrams"]
];
Print["=== QMeS: ", NumberForm[tQMeS, {5,3}], " s (", Length[qmesRes], " diagrams) ==="];
Print[""];
Print["=== FunKit/QMeS ratio: ", NumberForm[N[(tDeriv + tTrunc)/tQMeS], {4,2}], "x ==="];

Exit[0];
