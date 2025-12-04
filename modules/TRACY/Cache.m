SetCacheDirectory::invalid = "The directory \"`1` \"could not be created.";

SetCacheDirectory[str_String] :=
    Module[{mstr = str},
        If[StringTake[mstr, {-1}] =!= "/",
            mstr = StringJoin[mstr, "/"]
        ];
        CreateDirectory[mstr] // Quiet;
        If[DirectoryQ[mstr],
            Set[$TraceCacheDir, mstr]
            ,
            Message[SetCacheDirectory::invalid, mstr];
            Abort[]
        ];
    ];

SetCacheDirectory[] :=
    SetCacheDirectory["/tmp/TraceCache/"];

SetCacheDirectory[];

ClearTraceCache[] :=
    (
        DeleteDirectory[$TraceCacheDir, DeleteContents -> True];
        CreateDirectory[$TraceCacheDir]
    )

ClearTraceCache[str_String] :=
    (DeleteDirectory[$TraceCacheDir <> str, DeleteContents -> True])
