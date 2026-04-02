(* TestCreate was introduced in 12.0; alias to VerificationTest for older versions *)
If[$VersionNumber < 12.0 && !ValueQ[TestCreate],
    TestCreate = VerificationTest;
];

$mDir = SelectFirst[Join[{FileNameJoin[{$UserBaseDirectory, "Applications", "FunKit"}], FileNameJoin[{$BaseDirectory, "Applications", "FunKit"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Applications", "FunKit"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Packages", "FunKit"}], FileNameJoin[{$InstallationDirectory, "AddOns", "ExtraPackages", "FunKit"}]}, Select[$Path, StringContainsQ[#, "FunKit"]&]], DirectoryQ[#]&] <> "/";

(* Take care of dependencies *)

Import[$mDir <> "/tests/util/getQMeS.m"];

Import[$mDir <> "/tests/util/getFORMTracer.m"];

Import[$mDir <> "/tests/util/getTensorBases.m"];

Import[$mDir <> "/tests/util/getDoFun.m"];

Block[{Print},
    Needs["FunKit`"];
];

Print["  Using FunKit version: " <> ToString[FunKit`$FunKitVersion]];

If[Length[Kernels[]] <= 1,
    Print["  Launching parallel kernels..."];
    LaunchKernels[];
    Print["    Parallel kernels launched: " <> ToString[Length[Kernels[]]] <> " kernels.\n"];
];
