(**********************************************************************************
    Cache.m -- Trace cache directory management

    Public API:
      FSetCacheDirectory         -- Sets (and creates) the trace cache directory
      FClearTraceCache           -- Clears the trace cache (all or a subdirectory)

    Variables:
      $TraceCacheDir             -- Current trace cache directory path
**********************************************************************************)

FSetCacheDirectory::invalid = "The directory \"`1` \"could not be created.";

FSetCacheDirectory[str_String] :=
    Module[{mstr = str},
        If[!StringEndsQ[mstr, "/" | "\\"],
            mstr = mstr <> $PathnameSeparator
        ];
        CreateDirectory[mstr] // Quiet;
        If[DirectoryQ[mstr],
            Set[$TraceCacheDir, mstr]
            ,
            Message[FSetCacheDirectory::invalid, mstr];
            Abort[]
        ];
    ];

FSetCacheDirectory[] :=
    FSetCacheDirectory[FileNameJoin[{$TemporaryDirectory, "TraceCache"}]];

FSetCacheDirectory[];

FClearTraceCache[] :=
    (
        DeleteDirectory[$TraceCacheDir, DeleteContents -> True];
        CreateDirectory[$TraceCacheDir]
    )

FClearTraceCache::invalidPath = "The subdirectory path \"`1`\" is invalid. It must not contain \"..\" or start with an absolute path.";

FClearTraceCache[str_String] :=
    Module[{},
        If[StringContainsQ[str, ".."] || StringMatchQ[str, ("/" | "\\" | (LetterCharacter ~~ ":")) ~~ ___],
            Message[FClearTraceCache::invalidPath, str];
            Abort[]
        ];
        DeleteDirectory[FileNameJoin[{$TraceCacheDir, str}], DeleteContents -> True]
    ]

FSetCacheDirectory[___] :=
    (
        Message[FunKit::invalidArguments, FSetCacheDirectory];
        Abort[]
    );
