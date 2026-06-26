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
    Print["FunKit's tests require QMeS to run. Installing the latest version now."];
    If[$VersionNumber >= 11.2,
        (* Do NOT wrap this in Block[{Print}, ...]: doing so swallows the
           installer's own diagnostics, so a failed install (no network, no write
           permission, GitHub unreachable, ...) leaves no clue and only surfaces
           later as a confusing Get::noopen on QMeSderivation`Tools`. *)
        Import["https://raw.githubusercontent.com/satfra/QMeS-Derivation/main/QMeSInstaller.m"],
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

(* Verify the install actually succeeded before relying on it. If QMeS is still
   not available, surface ONE clear, actionable message and set $QMeSAvailable so
   that QMeS-dependent tests can be skipped, instead of emitting Get::noopen and
   letting ~30 physics-comparison tests fail with unevaluated QMeS expressions. *)
$QMeSAvailable = QMeSInstalled[];

If[$QMeSAvailable,
    Get["QMeSderivation`"]
    ,
    Print[Style[StringJoin[
        "ERROR: QMeS could not be installed automatically — QMeS-dependent tests will be skipped.\n",
        "  Install it manually (e.g. run dependencies/install.sh, or unzip dependencies/QMeS.zip\n",
        "  into ", FileNameJoin[{$UserBaseDirectory, "Applications", "QMeSderivation"}], "),\n",
        "  or check network access to github.com, then re-run the tests."
    ], Red, Bold]];
]
