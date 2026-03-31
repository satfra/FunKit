(* ProfileFSimplify.m — Profile FSimplify on Yukawa four-fermion vertex flow *)

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

setup = GetFunKitSetupYukawaExtended[];
FSetGlobalSetup[setup];
FSetAutoSimplify[False];
derivList = {Psibar[i1], Psibar[i2], Psi[i3], Psi[i4]};

(* Derive with AutoSimplify off *)
Print["Deriving Yukawa four-fermion vertex (AutoSimplify=False)..."];
{tDeriv, derivRes} = AbsoluteTiming[FTakeDerivatives[setup, WetterichEquation, derivList]];
Print["FTakeDerivatives: ", NumberForm[tDeriv, {5,3}], " s, ", Length[derivRes], " terms"];

{tTrunc, truncRes} = AbsoluteTiming[FTruncate[setup, derivRes]];
Print["FTruncate: ", NumberForm[tTrunc, {5,3}], " s, ", Length[truncRes], " terms"];

(* Warmup FSimplify *)
Print["Warming up FSimplify..."];
FunKit`FSimplify[setup, truncRes];

(* Profile FSimplify *)
Print[];
Print["=== FSimplify profiling ==="];

(* Without symmetries *)
{tSimp, simpRes} = AbsoluteTiming[FunKit`FSimplify[setup, truncRes]];
Print["FSimplify (no symmetries): ", NumberForm[tSimp, {5,3}], " s"];
Print["  Input terms:  ", Length[truncRes]];
Print["  Output terms: ", Length[simpRes]];

(* With symmetries from FTakeDerivatives *)
{truncResSym, annotations} = FunKit`Private`SeparateFExAnnotations[truncRes];
symmetries = If[KeyExistsQ[annotations, "Symmetries"], annotations["Symmetries"], {}];
Print["  Symmetries: ", Length[symmetries]];

If[Length[symmetries] > 0,
    {tSimpSym, simpResSym} = AbsoluteTiming[FunKit`FSimplify[setup, truncRes, "Symmetries" -> symmetries]];
    Print["FSimplify (with symmetries): ", NumberForm[tSimpSym, {5,3}], " s"];
    Print["  Output terms: ", Length[simpResSym]];
];

(* Also test on a simpler case: Yukawa scalar 4-point *)
Print[];
Print["=== Yukawa scalar 4-point ==="];
setupY = GetFunKitSetupYukawa[];
FSetGlobalSetup[setupY];
derivY = FTakeDerivatives[setupY, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}];
truncY = FTruncate[setupY, derivY];
Print["Terms after truncation: ", Length[truncY]];

FunKit`FSimplify[setupY, truncY]; (* warmup *)
{tSimpY, simpResY} = AbsoluteTiming[FunKit`FSimplify[setupY, truncY]];
Print["FSimplify: ", NumberForm[tSimpY, {5,3}], " s"];
Print["  Output terms: ", Length[simpResY]];

Exit[0];
