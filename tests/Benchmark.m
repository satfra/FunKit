(* tests/Benchmark.m — Run: wolfram -script tests/Benchmark.m *)

$mDir = SelectFirst[
    Join[
        {FileNameJoin[{$UserBaseDirectory, "Applications", "FunKit"}],
         FileNameJoin[{$BaseDirectory, "Applications", "FunKit"}],
         FileNameJoin[{$InstallationDirectory, "AddOns", "Applications", "FunKit"}]},
        Select[$Path, StringContainsQ[#, "FunKit"]&]
    ],
    DirectoryQ[#]&
] <> "/";

Block[{Print}, Needs["FunKit`"]];
Print["FunKit " <> ToString[FunKit`$FunKitVersion]];
If[Length[Kernels[]] <= 1, LaunchKernels[]];

Import[$mDir <> "tests/boilerplate/setups.m"];

BenchmarkThunk[label_String, thunk_, nWarm_:2, nRuns_:5] :=
    Module[{ts},
        Do[thunk[], {nWarm}];
        ts = Table[First @ AbsoluteTiming[thunk[]], {nRuns}];
        Print[label <> ": mean=" <> ToString[NumberForm[Mean[ts],{6,3}]]
              <> "s  runs=" <> StringRiffle[ToString[NumberForm[#,{6,3}]]& /@ ts, ", "]];
    ];

(* === Yukawa setup === *)
ySetup = GetFunKitSetupYukawa[];
FSetGlobalSetup[ySetup];
eq = WetterichEquation;

(* Pre-compute inputs for downstream stages *)
derivF2 = {Psi[i1], Psibar[i2]};
derivYuk = {Psi[i1], Psibar[i2], Phi[i3]};

resF2  = FTakeDerivatives[ySetup, eq, derivF2];
resYuk = FTakeDerivatives[ySetup, eq, derivYuk];

resF2trunc  = FTruncate[ySetup, resF2];
resYuktrunc = FTruncate[ySetup, resYuk];

resF2simp  = FSimplify[ySetup, resF2trunc];
resYuksimp = FSimplify[ySetup, resYuktrunc];

(* Stage 1: FTakeDerivatives *)
BenchmarkThunk["FTakeDerivatives 2-pt fermion",
    Function[{}, FTakeDerivatives[ySetup, eq, derivF2]]];
BenchmarkThunk["FTakeDerivatives 3-pt Yukawa",
    Function[{}, FTakeDerivatives[ySetup, eq, derivYuk]]];

(* Stage 2: FTruncate *)
BenchmarkThunk["FTruncate 2-pt fermion",
    Function[{}, FTruncate[ySetup, resF2]]];
BenchmarkThunk["FTruncate 3-pt Yukawa",
    Function[{}, FTruncate[ySetup, resYuk]]];

(* Stage 3: FRoute *)
BenchmarkThunk["FRoute 2-pt fermion",
    Function[{}, FRoute[ySetup, resF2simp]]];
BenchmarkThunk["FRoute 3-pt Yukawa",
    Function[{}, FRoute[ySetup, resYuksimp]]];

Exit[0];
