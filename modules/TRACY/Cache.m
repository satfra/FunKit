FSetCacheDirectory::invalid = "The directory \"`1` \"could not be created.";

FSetCacheDirectory[str_String] :=
    Module[{mstr = str},
        If[StringTake[mstr, {-1}] =!= "/",
            mstr = StringJoin[mstr, "/"]
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
    FSetCacheDirectory["/tmp/TraceCache/"];

FSetCacheDirectory[];

ClearTraceCache[] :=
    (
        DeleteDirectory[$TraceCacheDir, DeleteContents -> True];
        CreateDirectory[$TraceCacheDir]
    )

ClearTraceCache::invalidPath = "The subdirectory path \"`1`\" is invalid. It must not contain \"..\" or start with \"/\".";

ClearTraceCache[str_String] :=
    Module[{},
        If[StringContainsQ[str, ".."] || StringMatchQ[str, "/" ~~ ___],
            Message[ClearTraceCache::invalidPath, str];
            Abort[]
        ];
        DeleteDirectory[$TraceCacheDir <> str, DeleteContents -> True]
    ]

FSetCacheDirectory[___] :=
    (
        Message[FunKit::invalidArguments, FSetCacheDirectory];
        Abort[]
    );
