TensorBasesInstalled[] :=
    Module[{TensorBasesDirectory},
        TensorBasesDirectory = SelectFirst[Join[{FileNameJoin[{$UserBaseDirectory,
             "Applications", "TensorBases"}], FileNameJoin[{$BaseDirectory, "Applications",
             "TensorBases"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Applications",
             "TensorBases"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Packages",
             "TensorBases"}], FileNameJoin[{$InstallationDirectory, "AddOns", "ExtraPackages",
             "TensorBases"}]}, Select[$Path, StringContainsQ[#, "TensorBases"]&]],
             DirectoryQ[#]&] <> "/" // Quiet;
        If[Head[TensorBasesDirectory] =!= String,
            Return[False]
        ];
        Return[True];
    ];

If[Not @ TensorBasesInstalled[],
    Print["TensorBases does not seem to be installed, installing it now."
        ];
    Import["https://raw.githubusercontent.com/satfra/TensorBases/main/TensorBasesInstaller.m"
        ];
];
