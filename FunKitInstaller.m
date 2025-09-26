(* ::Package:: *)

(* ::Input::Initialization:: *)

FunKit::allowinternetuse = "You have forbidden Mathematica to access the internet. Either allow Mathematica to access the internet or download FunKit from github manually.";

If[Not["AllowInternetUse" /. SystemInformation["Network"]],
    Message[FunKit::allowinternetuse];
    Abort[];
];

(* just for backwards compatibility *)

If[ToString[Context[URLDownload]] =!= "System`",
    URLDownload = URLSave
];

(* ::Input::Initialization:: *)

FunKitZipLocation = "https://github.com/satfra/FunKit/archive/refs/heads/main.zip";

FunKitInstallDir = FileNameJoin[{$UserBaseDirectory, "Applications"}];

(* ::Input::Initialization:: *)

FunKitInstaller::zipdownloadfailed = "Download from " <> FunKitZipLocation <>
     " failed.";

FunKitInstaller::installationfailed = "\nInstallation failed. Please read the error messages for more information!";

Print["Downloading FunKit ..."];

FunKitArchive = FileNameJoin[{$TemporaryDirectory, "FunKit.zip"}];

URLDownload[FunKitZipLocation, FunKitArchive]

tmpFunKitImport = Import[FunKitArchive];

If[tmpFunKitImport === "{\"error\":\"Not Found\"}" || tmpFunKitImport
     === "404: Not Found",
    Message[FunKitInstaller::zipdownloadfailed];
    Abort[];
];

newVersionString = (List @@ Import[FunKitArchive, FileNameJoin[{"FunKit",
     "PacletInfo.m"}]])[[1]]["Version"];

FunKitFiles = FileNameJoin[{FunKitInstallDir, #}]& /@ Import[FunKitArchive
    ];

FunKitFilesExist = FileExistsQ /@ FunKitFiles;

FunKitExistingInstallation = Or @@ FunKitFilesExist;

FunKitExistingPacletInfo = FileNameJoin[{FunKitInstallDir, "FunKit", 
    "PacletInfo.m"}];

FunKitExistingVersionString =
    If[FileExistsQ[FunKitExistingPacletInfo],
        (List @@ Import[FunKitArchive, FileNameJoin[{"FunKit", "PacletInfo.m"
            }]])[[1]]["Version"]
        ,
        "unknown"
    ];

(* ::Input::Initialization:: *)

deleteExisting = False;

deleteExisting =
    If[FunKitExistingInstallation,
        ChoiceDialog["The installer has found an existing FunKit installation.
Do you want to overwrite the existing installation version "
             <> FunKitExistingVersionString <> " with version " <> newVersionString
             <> "?
Otherwise the installation will be aborted.", WindowTitle -> "FunKit Installation",
             WindowSize -> {Medium, All}]
        ,
        False
    ];

If[deleteExisting,
    DeleteFile[Pick[FunKitFiles, FunKitFilesExist]]
];

If[FunKitExistingInstallation && deleteExisting === False,
    (*abort installation*)
    Print["FunKit installation aborted."];
    ,
    (*install FunKit*)
    installationSuccess =
        Check[
            ExtractArchive[FunKitArchive, FunKitInstallDir];
            Get["FunKit`"]
            ,
            $Failed
        ];
    If[installationSuccess === $Failed,
        (*installation failed*)
        Message[FunKitInstaller::installationfailed];
        ,
        (*installation successful*)
        PacletDataRebuild[];
        Print["Installation was successful."];
    ];
];

Quiet[DeleteFile[FunKitArchive]];
