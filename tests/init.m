$FunKitDirectory = SelectFirst[Join[{FileNameJoin[{$UserBaseDirectory,
     "Applications", "FunKit"}], FileNameJoin[{$BaseDirectory, "Applications",
     "FunKit"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Applications",
     "FunKit"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Packages",
     "FunKit"}], FileNameJoin[{$InstallationDirectory, "AddOns", "ExtraPackages",
     "FunKit"}]}, Select[$Path, StringContainsQ[#, "FunKit"]&]], DirectoryQ[
    #]&] <> "/";

(* Take care of dependencies *)

Import[$FunKitDirectory <> "/tests/util/getQMeS.m"];

Import[$FunKitDirectory <> "/tests/util/getFORMTracer.m"];

Import[$FunKitDirectory <> "/tests/util/getFORMTensorBases.m"];

Block[{Print},
    Needs["FunKit`"];
];

Print["Using FunKit version: " <> ToString[FunKit`$FunKitVersion], "\n"
    ];
