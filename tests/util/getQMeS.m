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
    If[ChoiceDialog["QMeS does not seem to be installed. The tests require QMeS to be installed to run. Do you want to install it?",
         WindowTitle -> "Install QMeS", WindowSize -> {Medium, All}],
        Block[{Print},
            Import["https://raw.githubusercontent.com/satfra/QMeS-Derivation/main/QMeSInstaller.m"
                ]
        ]
        ,
        Print["StyleBox[\"FunKit\",\nFontWeight->\"Bold\"]\)'s tests require \!\(\*
    StyleBox[\"QMeS\",\nFontWeight->\"Bold\"]\) to run."
            ];
        Abort[];
    ];
];

Get["QMeSderivation`"]
