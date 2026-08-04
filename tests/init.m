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

(* Which backend the suites run against. The default is the native pipeline, so that the
   "Automatic" default cannot route them through the C++ engine; the CoBra suites opt in
   explicitly. Set FUNKIT_TEST_BACKEND=Cpp to run the whole suite through the C++ engine
   instead -- worth doing in CI, since that is what users get by default.

   Test.m re-applies this before every test file via ApplyTestBackend: the backend is global
   state and several suites switch it deliberately, so without that a single file leaving it
   changed silently retargets the rest of the run. *)

$TestBackend =
    If[Environment["FUNKIT_TEST_BACKEND"] === "Cpp",
        "Cpp"
        ,
        "Mathematica"
    ];

ApplyTestBackend[] :=
    If[$TestBackend === "Cpp",
        FSetBackendCpp[]
        ,
        FSetBackendMathematica[]
    ];

ApplyTestBackend[];

(* Say which configuration this run is: the two suites are the same size, so the totals alone
   cannot tell a Mathematica run from a C++ one -- in a CI log or a pasted transcript that is the
   difference between "the default configuration is green" and "we tested the other one". *)

Print["  Test backend: " <> $TestBackend];

If[Length[Kernels[]] <= 1,
    Print["  Launching parallel kernels..."];
    LaunchKernels[];
    Print["    Parallel kernels launched: " <> ToString[Length[Kernels[]]] <> " kernels.\n"];
];
