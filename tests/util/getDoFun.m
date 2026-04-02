DoFunInstalled[] :=
    Module[{DoFunDirectory},
        DoFunDirectory = SelectFirst[Join[{FileNameJoin[{$UserBaseDirectory, "Applications", "DoFun"}], FileNameJoin[{$BaseDirectory, "Applications", "DoFun"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Applications", "DoFun"}], FileNameJoin[{$InstallationDirectory, "AddOns", "Packages", "DoFun"}], FileNameJoin[{$InstallationDirectory, "AddOns", "ExtraPackages", "DoFun"}]}, Select[$Path, StringContainsQ[#, "DoFun"]&]], DirectoryQ[#]&] <> "/" // Quiet;
        If[Head[DoFunDirectory] =!= String,
            Return[False]
        ];
        Return[True];
    ];

If[Not @ DoFunInstalled[],
    Print["FunKit's tests require DoFun to run. Installing the latest version now.
    "];
    If[$VersionNumber >= 11.2,
        Block[{Print},
            Import["https://raw.githubusercontent.com/markusqh/DoFun/master/DoFun/DoFunInstaller.m"]
        ],
        Module[{releaseInfo, zipURL, archive, installDir, data},
            Print["Downloading DoFun ..."];
            releaseInfo = Import["https://api.github.com/repos/markusqh/DoFun/releases/latest", "JSON"];
            zipURL = First["browser_download_url" /. ("assets" /. releaseInfo)];
            archive = FileNameJoin[{$TemporaryDirectory, "DoFun.zip"}];
            installDir = FileNameJoin[{$UserBaseDirectory, "Applications"}];
            data = Quiet[Import[zipURL, "Byte"]];
            If[!ListQ[data] || Length[data] === 0,
                Print["Download of DoFun failed."]; Abort[]
            ];
            Close[BinaryWrite[OpenWrite[archive, BinaryFormat -> True], data]];
            ExtractArchive[archive, installDir];
            Quiet[DeleteFile[archive]];
            Print["DoFun installed successfully."];
        ]
    ]
];

(**********************************************************************************
    Utils
**********************************************************************************)

wrapDoFun[expr_] :=
    Module[{exprString = ToString[expr], ret, outDoFun = {a_Symbol /; StringContainsQ[ToString[a], "DoFunWrapper`Private`"] :> Symbol[SymbolName[a]]}},
        Off[General::shdw];
        BeginPackage["DoFunWrapper`"];
        Block[{Print},
            Get["DoFun`"]
        ];
        Begin["`Private`"];
        ret = ToExpression[exprString];
        End[];
        EndPackage[];
        On[General::shdw];
        Return[ret //. outDoFun];
    ];
