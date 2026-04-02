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
    Print["FormTracer does not seem to be installed. Installing it now."];
    If[$VersionNumber >= 11.2,
        Import["https://raw.githubusercontent.com/FormTracer/FormTracer/master/src/FormTracerInstaller.m"],
        Module[{zipURL, archive, installDir, data},
            zipURL = "https://raw.githubusercontent.com/FormTracer/FormTracer/master/FormTracer.zip";
            archive = FileNameJoin[{$TemporaryDirectory, "FormTracer.zip"}];
            installDir = FileNameJoin[{$UserBaseDirectory, "Applications"}];
            Print["Downloading FormTracer ..."];
            data = Quiet[Import[zipURL, "Byte"]];
            If[!ListQ[data] || Length[data] === 0,
                Print["Download of FormTracer failed."]; Abort[]
            ];
            Close[BinaryWrite[OpenWrite[archive, BinaryFormat -> True], data]];
            ExtractArchive[archive, installDir];
            Quiet[DeleteFile[archive]];
            Print["FormTracer installed successfully."];
        ]
    ]
];
