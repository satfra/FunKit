FormTracerInstalled[] :=
    Module[{FTDirectory},
        FTDirectory = SelectFirst[Join[{FileNameJoin[{$UserBaseDirectory,
             "Applications", "FormTracer"}], FileNameJoin[{$BaseDirectory, "Applications",
             "FormTracer"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Applications",
             "FormTracer"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Packages",
             "FormTracer"}], FileNameJoin[{$InstallationDirectory, "AddOns", "ExtraPackages",
             "FormTracer"}]}, Select[$Path, StringContainsQ[#, "FormTracer"]&]], 
            DirectoryQ[#]&] <> "/" // Quiet;
        If[Head[FTDirectory] =!= String,
            Return[False]
        ];
        Return[True];
    ];

If[Not @ FormTracerInstalled[],
    Print["FormTracer does not seem to be installed. Installing it now."
        ];
    Import["https://raw.githubusercontent.com/FormTracer/FormTracer/master/src/FormTracerInstaller.m"
        ]
];
