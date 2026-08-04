(**********************************************************************************
    Ingest.m -- Running the funkit executable, reading its JSON output back
    into FEx form, exact-coefficient reconstruction and result caching.

    Public API:
      FClearCppCache             -- Delete all cached C++ backend results
      FSetCppCacheDirectory      -- Set the result cache directory

    Internal:
      CppExecute                 -- Cache lookup, process run, result read
      CppReadResult              -- JSON output file -> FEx
      CppRationalize             -- double -> exact Rational/Integer coefficient
      CppAttachSymmetries        -- Re-attach the "Symmetries" annotation

    Caching follows TRACY's content-hash pattern: the key is a SHA256 over the
    canonical input JSON plus the engine build stamp, so results invalidate
    automatically on engine rebuilds. Both <hash>.in.json and <hash>.out.json
    are kept -- the input doubles as the repro artifact referenced by
    FunKit::cppRuntime.
**********************************************************************************)

FunKit::cppRuntime = "The C++ backend exited with status `1`. Output:
`2`
The input file has been kept for reproduction at
  `3`
You can rerun it manually with the funkit executable, or inspect it after converting with FExportToml.";

FunKit::cppBadOutput = "The C++ backend produced unreadable output (`1`). This indicates a version mismatch between FunKit and the funkit executable; try FSetBackendCpp[\"Rebuild\" -> True].";

FunKit::inexactCoefficient = "The coefficient `1` could not be reconstructed as an exact rational number and is kept as a machine real.";

(**********************************************************************************
    Cache management
**********************************************************************************)

$CppCacheDir = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "FunKit", "cpp-cache"}];

FSetCppCacheDirectory[dir_String] :=
    (
        $CppCacheDir = dir;
        Quiet @ CreateDirectory[$CppCacheDir, CreateIntermediateDirectories -> True];
        $CppCacheDir
    );

FSetCppCacheDirectory[a___] :=
    (
        Message[FunKit::invalidArguments, FSetCppCacheDirectory];
        Abort[]
    );

FClearCppCache[] :=
    (
        If[DirectoryQ[$CppCacheDir],
            DeleteDirectory[$CppCacheDir, DeleteContents -> True]
        ];
        CreateDirectory[$CppCacheDir, CreateIntermediateDirectories -> True];
    );

FClearCppCache[a___] :=
    (
        Message[FunKit::invalidArguments, FClearCppCache];
        Abort[]
    );

(*The hash covers the entire semantic input (equation, derivatives,
  symmetries, fields, truncation, stage flags) plus the engine build stamp;
  output location and debug level are non-semantic*)

CppInputHash[input_Association] :=
    Module[{canon = input, stamp},
        canon["setup"] = KeyDrop[canon["setup"], {"outputFile", "output_format", "debug"}];
        (*The engine build stamp is stashed at activation time (FSetBackendCpp);
          only re-read it from disk if that memoized value is missing*)
        stamp =
            Which[
                StringQ[$CppEngineStamp],
                    $CppEngineStamp
                ,
                StringQ[$CppBackendBinary],
                    Quiet @ Import[FileNameJoin[{DirectoryName[$CppBackendBinary], "funkit-source.hash"}], "Text"]
                ,
                True,
                    None
            ];
        (*Hash the canonical input expression directly -- no JSON encode; the
          full serialization happens once, in CppExecute, for the input file*)
        IntegerString[Hash[{canon, stamp}, "SHA256"], 16, 64]
    ];

(**********************************************************************************
    Process execution with cache lookup
**********************************************************************************)

CppExecute[setup_, input_Association, openInverse_Association] :=
    Module[{hash, inFile, outFile, tmpOut, runInput, res},
        hash = CppInputHash[input];
        Quiet @ CreateDirectory[$CppCacheDir, CreateIntermediateDirectories -> True];
        inFile = FileNameJoin[{$CppCacheDir, hash <> ".in.json"}];
        outFile = FileNameJoin[{$CppCacheDir, hash <> ".out.json"}];
        If[!FileExistsQ[outFile],
            (*Write output to a per-kernel temporary, then move into place, so
              concurrent kernels computing the same hash cannot collide*)
            tmpOut = outFile <> "." <> ToString[$KernelID] <> ".tmp";
            runInput = input;
            runInput["setup"] = Join[runInput["setup"], <|"outputFile" -> tmpOut, "output_format" -> "json", "debug" -> Max[0, $FunKitDebugLevel - 1]|>];
            Export[inFile, runInput, "RawJSON"];
            FunKitDebug[1, "Running the C++ backend on ", inFile];
            res = RunProcess[{$CppBackendBinary, inFile}, All];
            If[!AssociationQ[res] || res["ExitCode"] =!= 0 || !FileExistsQ[tmpOut],
                Message[
                    FunKit::cppRuntime,
                    If[AssociationQ[res], res["ExitCode"], "(the process failed to start)"],
                    If[AssociationQ[res], res["StandardOutput"] <> res["StandardError"], ""],
                    inFile
                ];
                Abort[];
            ];
            If[$FunKitDebugLevel >= 2,
                FunKitDebug[2, res["StandardOutput"]]
            ];
            If[FileExistsQ[outFile],
                DeleteFile[tmpOut]
                ,
                RenameFile[tmpOut, outFile]
            ];
            ,
            FunKitDebug[1, "C++ result cached: ", outFile];
        ];
        CppReadResult[setup, outFile, openInverse]
    ];

(**********************************************************************************
    Coefficient reconstruction. Pipeline coefficients are products of the
    master equation's rationals, integer multiplicities, +-1 signs and 1/n!
    symmetry factors -- small-denominator rationals that a double represents
    exactly, so a tight relative tolerance recovers them deterministically.
    Anything exotic is kept as a float with a warning instead of being
    silently mangled.
**********************************************************************************)

$CppRationalizeTolerance = 1.*^-10;

$CppMaxDenominator = 10^7;

CppRationalize[c_Integer] :=
    c;

CppRationalize[c_?NumericQ] :=
    Module[{r},
        If[c == Round[c],
            Return[Round[c]]
        ];
        r = Rationalize[c, Abs[c] * $CppRationalizeTolerance];
        If[MatchQ[r, _Rational] && Denominator[r] <= $CppMaxDenominator,
            r
            ,
            Message[FunKit::inexactCoefficient, c];
            c
        ]
    ];

(**********************************************************************************
    JSON output -> FEx. Open legs are restored to their original symbols via
    openInverse (label -> symbol); every other label gets a fresh symbol,
    memoized per term (closed pairs appear exactly twice within one term).
    A final ReduceIndicesBatch resolves any gamma/FMinus/SymmFactor objects a
    truncation-free run leaves behind, exactly like the native pipeline tail.
**********************************************************************************)

CppReadResult[setup_, file_, openInverse_Association] :=
    Module[{raw, nameToField, nameToHead, toTerm, terms},
        raw = Quiet @ Import[file, "RawJSON"];
        If[!AssociationQ[raw] || Lookup[raw, "funkit_output_version", 0] =!= 1,
            Message[FunKit::cppBadOutput, file];
            Abort[];
        ];
        (*Names resolve through the setup and object registry -- never through
          Symbol[] -- so output cannot inject symbols into user contexts*)
        nameToField = Association @ Append[(ToString[#] -> #)& /@ GetAllFields[setup], "AnyField" -> AnyField];
        nameToHead =
            Association @ Join[
                (ToString[#] -> #)& /@ DeleteCases[$allObjects, \[Gamma] | SymmetryFactor],
                {"gamma" -> \[Gamma], "SymmFactor" -> SymmetryFactor, "Field" -> Field, "FMinus" -> FMinus, "FDOp" -> FDOp}
            ];
        toTerm[objs_List] :=
            Module[{coeff = 1, items = {}, fresh = <||>, lookupIdx, toObj},
                lookupIdx[n_Integer] :=
                    Module[{l = Abs[n], s},
                        s =
                            Which[
                                KeyExistsQ[openInverse, l],
                                    openInverse[l]
                                ,
                                KeyExistsQ[fresh, l],
                                    fresh[l]
                                ,
                                True,
                                    (*Same "ci" prefix the Mathematica path uses for fresh closed
                                      indices (cf. Cleaning.m), so printed and plotted output looks
                                      the same whichever backend produced it. The numeric suffix
                                      still comes from the session-global Unique counter and so
                                      differs between the two -- the expressions are equal, but not
                                      literally identical.*)
                                    fresh[l] = Symbol[SymbolName[Unique["ci"]]]
                            ];
                        If[n < 0,
                            -s
                            ,
                            s
                        ]
                    ];
                toObj[o_Association] :=
                    Module[{head, fields, idxs},
                        head = Lookup[nameToHead, o["type"], $Failed];
                        fields = Lookup[nameToField, #[[1]], $Failed]& /@ o["legs"];
                        If[head === $Failed || MemberQ[fields, $Failed],
                            Message[FunKit::cppBadOutput, o["type"]];
                            Abort[];
                        ];
                        idxs = lookupIdx[#[[2]]]& /@ o["legs"];
                        If[head === Field,
                            (*bare field application, inverting the serializer*)
                            fields[[1]][First[idxs]]
                            ,
                            makeObj[head, fields, idxs]
                        ]
                    ];
                Scan[
                    If[KeyExistsQ[#, "prefactor"],
                        coeff *= #["prefactor"]
                        ,
                        AppendTo[items, toObj[#]]
                    ]&
                    ,
                    objs
                ];
                FTerm @@ Prepend[items, CppRationalize[coeff]]
            ];
        terms = toTerm /@ raw["equation"];
        terms = ReduceIndicesBatch[setup, terms];
        FEx @@ terms
    ];

(**********************************************************************************
    Re-attaching the "Symmetries" annotation. The C++ output carries none, but
    ingestion restores the original external-leg symbols, so the annotation
    the native path would attach (FMakeSymmetryList over the derivative list)
    applies verbatim. Above the permutation-enumeration limit the annotation
    is skipped -- the C++ result is already fully simplified via the engine's
    orbit matching, so it is a nice-to-have for later WL-side FSimplify calls,
    not a correctness requirement.
**********************************************************************************)

$CppAutoSymmetryLimit = 6;

CppAttachSymmetries[data_Association, result_FEx] :=
    Module[{syms = data["Symmetries"], derivs = data["DerivativeList"]},
        If[TrueQ[data["AutoSymmetries"]] && 0 < Length[derivs] <= $CppAutoSymmetryLimit,
            syms = FMergeSymmetries[FunKit`FMakeSymmetryList[data["Setup"], derivs], syms]
        ];
        If[syms === {},
            result
            ,
            MergeFExAnnotations[result, <|"Symmetries" -> syms|>]
        ]
    ];
