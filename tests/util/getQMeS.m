QMeSInstalled[] :=
    Module[{QMeSDirectory},
        QMeSDirectory = SelectFirst[Join[{FileNameJoin[{$UserBaseDirectory,
             "Applications", "QMeSderivation"}], FileNameJoin[{$BaseDirectory, "Applications",
             "QMeSderivation"}], FileNameJoin[{$InstallationDirectory, "AddOns",
            "Applications", "QMeSderivation"}], FileNameJoin[{$InstallationDirectory,
             "AddOns", "Packages", "QMeSderivation"}], FileNameJoin[{$InstallationDirectory,
             "AddOns", "ExtraPackages", "QMeSderivation"}]}, Select[$Path, StringContainsQ[
            #, "QMeSderivation"]&]], DirectoryQ[#]&] <> "/" // Quiet;
        If[Head[QMeSDirectory] =!= String,
            Return[False]
        ];
        Return[True];
    ];

If[Not @ QMeSInstalled[],
    Print["FunKit's tests require QMeS to run. Installing the latest version now.
    "
        ];
    If[$VersionNumber >= 11.2,
        Block[{Print},
            Import["https://raw.githubusercontent.com/satfra/QMeS-Derivation/main/QMeSInstaller.m"]
        ],
        Module[{zipURL, archive, installDir, data},
            zipURL = "https://raw.githubusercontent.com/satfra/QMeS-Derivation/main/QMeSderivation.zip";
            archive = FileNameJoin[{$TemporaryDirectory, "QMeSderivation.zip"}];
            installDir = FileNameJoin[{$UserBaseDirectory, "Applications"}];
            Print["Downloading QMeS ..."];
            data = Quiet[Import[zipURL, "Byte"]];
            If[!ListQ[data] || Length[data] === 0,
                Print["Download of QMeS failed."]; Abort[]
            ];
            Close[BinaryWrite[OpenWrite[archive, BinaryFormat -> True], data]];
            ExtractArchive[archive, installDir];
            Quiet[DeleteFile[archive]];
            Print["QMeS installed successfully."];
        ]
    ]
];

Get["QMeSderivation`"]
