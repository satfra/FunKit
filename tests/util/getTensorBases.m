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
    Print["TensorBases does not seem to be installed, installing it now."];
    If[$VersionNumber >= 11.2,
        Import["https://raw.githubusercontent.com/satfra/TensorBases/main/TensorBasesInstaller.m"],
        Module[{zipURL, archive, installDir, data},
            zipURL = "https://github.com/satfra/TensorBases/archive/refs/heads/main.zip";
            archive = FileNameJoin[{$TemporaryDirectory, "TensorBases.zip"}];
            installDir = FileNameJoin[{$UserBaseDirectory, "Applications"}];
            Print["Downloading TensorBases ..."];
            data = Quiet[Import[zipURL, "Byte"]];
            If[!ListQ[data] || Length[data] === 0,
                Print["Download of TensorBases failed."]; Abort[]
            ];
            Close[BinaryWrite[OpenWrite[archive, BinaryFormat -> True], data]];
            ExtractArchive[archive, installDir];
            RenameDirectory[FileNameJoin[{installDir, "TensorBases-main"}], FileNameJoin[{installDir, "TensorBases"}]];
            Quiet[DeleteFile[archive]];
            Print["TensorBases installed successfully."];
        ]
    ]
];
