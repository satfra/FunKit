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
    Block[{Print},
        Import["https://raw.githubusercontent.com/satfra/QMeS-Derivation/main/QMeSInstaller.m"
            ]
    ]
];

Get["QMeSderivation`"]
