(* ProfileScalar4pt.m — Profile where time is spent in scalar 4-pt derivation *)

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

setup = GetFunKitSetupScalar[];
FSetGlobalSetup[setup];
derivList = {Phi[i1], Phi[i2], Phi[i3], Phi[i4]};

(* Warmup *)
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
);

(* Profile FTakeDerivatives *)
resetCounters[];
{tDeriv, derivRes} = AbsoluteTiming[FTakeDerivatives[setup, WetterichEquation, derivList]];

Print["=== FTakeDerivatives: ", NumberForm[tDeriv, {5,3}], " s ==="];
Print["  FResolveFDOpInternal (all passes): ", NumberForm[FunKit`Private`$ProfileFDOp, {5,3}], " s"];
Print["  Mid-deriv FSimplify+RI:            ", NumberForm[FunKit`Private`$ProfileDerivSimplify, {5,3}], " s"];
Print["  ReduceIndices:                     ", NumberForm[FunKit`Private`$ReduceIndicesTime, {5,3}], " s  (", FunKit`Private`$ReduceIndicesCount, " calls)"];
Print["  FixIndices:                        ", NumberForm[FunKit`Private`$FixIndicesTime, {5,3}], " s  (", FunKit`Private`$FixIndicesCount, " calls)"];
Print[""];

(* Profile FTruncate *)
resetCounters[];
{tTrunc, truncRes} = AbsoluteTiming[FTruncate[setup, derivRes]];

Print["=== FTruncate: ", NumberForm[tTrunc, {5,3}], " s ==="];
Print["  LTrunc:                           ", NumberForm[FunKit`Private`$ProfileLTrunc, {5,3}], " s"];
Print["  Post-LTrunc ReduceIndices:        ", NumberForm[FunKit`Private`$ProfilePostRI, {5,3}], " s"];
Print["  FixIndices+OrderFields:            ", NumberForm[FunKit`Private`$ProfileFixOrder, {5,3}], " s"];
Print["  FSimplify ($AutoSimplify):         ", NumberForm[FunKit`Private`$ProfileFSimplify, {5,3}], " s"];
Print["  ReduceIndices total:               ", NumberForm[FunKit`Private`$ReduceIndicesTime, {5,3}], " s  (", FunKit`Private`$ReduceIndicesCount, " calls)"];
Print["  FixIndices total:                  ", NumberForm[FunKit`Private`$FixIndicesTime, {5,3}], " s  (", FunKit`Private`$FixIndicesCount, " calls)"];
Print[""];

Print["=== Grand Total: ", NumberForm[tDeriv + tTrunc, {5,3}], " s ==="];

Exit[0];
