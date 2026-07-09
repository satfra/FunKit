(* benchmarks/init.m — Benchmark environment initialization *)

$mDir = SelectFirst[
    Join[
        {FileNameJoin[{$UserBaseDirectory, "Applications", "FunKit"}],
         FileNameJoin[{$BaseDirectory, "Applications", "FunKit"}],
         FileNameJoin[{$InstallationDirectory, "AddOns", "Applications", "FunKit"}],
         FileNameJoin[{$InstallationDirectory, "AddOns", "Packages", "FunKit"}],
         FileNameJoin[{$InstallationDirectory, "AddOns", "ExtraPackages", "FunKit"}]},
        Select[$Path, StringContainsQ[#, "FunKit"]&]
    ],
    DirectoryQ[#]&
] <> "/";

(* Load external dependencies *)
Import[$mDir <> "tests/util/getQMeS.m"];
Import[$mDir <> "tests/util/getDoFun.m"];

(* Load FunKit *)
Block[{Print}, Needs["FunKit`"]];
Print["FunKit " <> ToString[FunKit`$FunKitVersion]];

(* Load shared setups *)
Import[$mDir <> "tests/boilerplate/setups.m"];

(* Load benchmark utilities *)
Import[FileNameJoin[{DirectoryName[$InputFileName], "util", "BenchmarkLib.m"}]];

(* The native FunKit rows must run the Mathematica pipeline: pin the backend
   so the "Automatic" default cannot route them through the C++ engine. The
   dedicated C++ row switches explicitly. *)

FSetBackendMathematica[];

(* Launch parallel kernels *)
If[Length[Kernels[]] <= 1,
    LaunchKernels[];
    Print["Launched " <> ToString[Length[Kernels[]]] <> " parallel kernels."];
];
