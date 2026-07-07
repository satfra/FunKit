(**********************************************************************************
    Backend.m -- C++ backend activation and build orchestration

    Public API:
      FSetBackendCpp             -- Build (if needed), test and activate the C++ backend
      FSetBackendMathematica     -- Deactivate the C++ backend

    Internal:
      $CppBackendBinary          -- Absolute path to the funkit executable (None if not ready)
      CppBackendSourceDir        -- Resolves the cpplib source directory
      CppBackendBuildDir         -- Resolves the persistent build directory
      CppSourceHash              -- Content hash of the cpplib sources (rebuild detection)
      CppBackendReadyQ           -- True if the backend binary is ready to run

    The backend flag itself ($FunKitBackend and CppBackendActiveQ) lives in
    FEDeriK/Global.m, so that FEDeriK works standalone without CoBra.
**********************************************************************************)

$CppBackendBinary = None;

CppBackendSourceDir[Automatic] :=
    FileNameJoin[{$FunKitDirectory, "cpplib"}];

CppBackendSourceDir[dir_String] :=
    dir;

CppBackendBuildDir[Automatic] :=
    FileNameJoin[{$UserBaseDirectory, "ApplicationData", "FunKit", "cpp-build"}];

CppBackendBuildDir[dir_String] :=
    dir;

CppBackendBinaryName :=
    If[$OperatingSystem === "Windows",
        "funkit.exe"
        ,
        "funkit"
    ];

CppBackendReadyQ[] :=
    StringQ[$CppBackendBinary] && FileExistsQ[$CppBackendBinary];

(**********************************************************************************
    Rebuild detection: a content hash over all build-relevant source files,
    stamped into the build directory. Catches both package upgrades and local
    development edits of the cpplib sources.
**********************************************************************************)

CppSourceHash[src_String] :=
    Module[{files},
        files =
            Sort @ Select[
                Join[
                    FileNames["*.cpp" | "*.hpp" | "*.h", {src}, 2],
                    FileNames["*.cpp" | "*.hpp" | "*.h", {FileNameJoin[{src, "include"}], FileNameJoin[{src, "source"}], FileNameJoin[{src, "tests"}]}, Infinity],
                    {FileNameJoin[{src, "CMakeLists.txt"}], FileNameJoin[{src, "tests", "CMakeLists.txt"}]}
                ]
                ,
                FileExistsQ
            ];
        ToString @ Hash[{FileNameTake[#, -3], FileHash[#, "SHA256"]}& /@ files, "SHA256"]
    ];

(**********************************************************************************
    Build steps. All external calls go through CppRunProcess, which captures
    exit code, stdout and stderr; failures are reported with the captured
    output and abort -- the backend is never activated after a failed step.
**********************************************************************************)

FSetBackendCpp::noSources = "The cpplib sources were not found at `1`. Reinstall FunKit (make install) or pass \"SourceDirectory\" -> \"/path/to/cpplib\".";

FSetBackendCpp::noCMake = "cmake was not found on PATH. The C++ backend requires CMake >= 3.20 and a C++20 compiler with OpenMP support.";

FSetBackendCpp::noOpenMP = "CMake could not find OpenMP. Install an OpenMP-capable C++ compiler. Configure output:\n`1`";

FSetBackendCpp::noNetwork = "Configuring the test suite failed while downloading the test framework (network access is required on the first configure). Either connect to the network once, or call FSetBackendCpp[\"RunTests\" -> False] to build without tests. Configure output:\n`1`";

FSetBackendCpp::configureFailed = "CMake configuration failed. Configure output:\n`1`";

FSetBackendCpp::buildFailed = "Building the funkit executable failed. The full build log has been written to\n  `1`";

FSetBackendCpp::testsFailed = "The cpplib test suite FAILED -- the C++ backend was NOT activated. The full test log has been written to\n  `1`";

FSetBackendCpp::noBinary = "The build reported success, but the funkit executable was not found at `1`.";

CppRunProcess[args_List, dir_:None] :=
    Module[{res},
        FunKitDebug[1, "Running: ", StringRiffle[args, " "]];
        res =
            If[dir === None,
                Quiet @ RunProcess[args, All]
                ,
                Quiet @ RunProcess[args, All, ProcessDirectory -> dir]
            ];
        FunKitDebug[3, "Exit code: ", If[AssociationQ[res], res["ExitCode"], res]];
        res
    ];

CppProcessOutput[res_] :=
    If[AssociationQ[res],
        res["StandardOutput"] <> "\n" <> res["StandardError"]
        ,
        "(the process failed to start)"
    ];

CppProcessFailedQ[res_] :=
    !AssociationQ[res] || res["ExitCode"] =!= 0;

CppWriteLog[bld_String, name_String, res_] :=
    Module[{logFile = FileNameJoin[{bld, name}]},
        Quiet @ Export[logFile, CppProcessOutput[res], "Text"];
        logFile
    ];

CppCheckToolchain[] :=
    If[CppProcessFailedQ[CppRunProcess[{"cmake", "--version"}]],
        Message[FSetBackendCpp::noCMake];
        Abort[];
    ];

CppConfigure[src_String, bld_String, tests_] :=
    Module[{res, out},
        res =
            CppRunProcess[{
                "cmake", "-S", src, "-B", bld,
                "-DCMAKE_BUILD_TYPE=Release",
                "-DFUNKIT_BUILD_TESTING=" <> If[TrueQ[tests], "ON", "OFF"]
            }];
        If[CppProcessFailedQ[res],
            out = CppProcessOutput[res];
            Which[
                StringContainsQ[out, "OpenMP", IgnoreCase -> True],
                    Message[FSetBackendCpp::noOpenMP, out]
                ,
                StringContainsQ[out, "Catch2" | "FetchContent" | "Could not resolve" | "Failed to connect"],
                    Message[FSetBackendCpp::noNetwork, out]
                ,
                True,
                    Message[FSetBackendCpp::configureFailed, out]
            ];
            Abort[];
        ];
    ];

CppBuild[bld_String, jobs_] :=
    Module[{nJobs, res},
        nJobs =
            If[jobs === Automatic,
                $ProcessorCount
                ,
                jobs
            ];
        res = CppRunProcess[{"cmake", "--build", bld, "-j", ToString[nJobs]}];
        If[CppProcessFailedQ[res],
            Message[FSetBackendCpp::buildFailed, CppWriteLog[bld, "funkit-build.log", res]];
            Abort[];
        ];
    ];

CppRunCTest[bld_String, jobs_] :=
    Module[{nJobs, res},
        nJobs =
            If[jobs === Automatic,
                $ProcessorCount
                ,
                jobs
            ];
        FunKitDebug[1, "Running the cpplib test suite"];
        res = CppRunProcess[{"ctest", "--test-dir", bld, "--output-on-failure", "-j", ToString[nJobs]}];
        If[CppProcessFailedQ[res],
            Message[FSetBackendCpp::testsFailed, CppWriteLog[bld, "funkit-ctest.log", res]];
            Abort[];
        ];
    ];

(**********************************************************************************
    FSetBackendCpp : build (if needed), test and activate the C++ backend
**********************************************************************************)

Options[FSetBackendCpp] = {
    "Rebuild" -> False,
    "RunTests" -> True,
    "SourceDirectory" -> Automatic,
    "BuildDirectory" -> Automatic,
    "Jobs" -> Automatic
};

FSetBackendCpp[OptionsPattern[]] :=
    Module[{src, bld, binPath, stampFile, hash, needBuild},
        src = CppBackendSourceDir[OptionValue["SourceDirectory"]];
        If[!DirectoryQ[src] || !FileExistsQ[FileNameJoin[{src, "CMakeLists.txt"}]],
            Message[FSetBackendCpp::noSources, src];
            Abort[];
        ];
        bld = CppBackendBuildDir[OptionValue["BuildDirectory"]];
        Quiet @ CreateDirectory[bld, CreateIntermediateDirectories -> True];
        binPath = FileNameJoin[{bld, CppBackendBinaryName}];
        stampFile = FileNameJoin[{bld, "funkit-source.hash"}];
        hash = CppSourceHash[src];
        needBuild =
            TrueQ[OptionValue["Rebuild"]] ||
            !FileExistsQ[binPath] ||
            !FileExistsQ[stampFile] ||
            Quiet[Import[stampFile, "Text"]] =!= hash;
        If[needBuild,
            FunKitDebug[1, "Building the C++ backend in ", bld];
            CppCheckToolchain[];
            CppConfigure[src, bld, OptionValue["RunTests"]];
            CppBuild[bld, OptionValue["Jobs"]];
            If[!FileExistsQ[binPath],
                Message[FSetBackendCpp::noBinary, binPath];
                Abort[];
            ];
            If[TrueQ[OptionValue["RunTests"]],
                CppRunCTest[bld, OptionValue["Jobs"]]
            ];
            Export[stampFile, hash, "Text"];
            ,
            FunKitDebug[1, "C++ backend is up to date"];
        ];
        $CppBackendBinary = binPath;
        Unprotect[FunKit`$FunKitBackend];
        FunKit`$FunKitBackend = "Cpp";
        Protect[FunKit`$FunKitBackend];
        (*Make the switch visible on any already-launched subkernels*)
        If[Length[Kernels[]] > 0,
            DistributeDefinitions[$FunKitBackend, $CppBackendBinary]
        ];
        FunKitDebug[1, "C++ backend active: ", binPath];
        binPath
    ];

FSetBackendCpp[a___] :=
    Module[{},
        Message[FunKit::invalidArguments, FSetBackendCpp];
        Abort[];
    ];

(**********************************************************************************
    FSetBackendMathematica : deactivate the C++ backend
**********************************************************************************)

FSetBackendMathematica[] :=
    Module[{},
        Unprotect[FunKit`$FunKitBackend];
        FunKit`$FunKitBackend = "Mathematica";
        Protect[FunKit`$FunKitBackend];
        If[Length[Kernels[]] > 0,
            DistributeDefinitions[$FunKitBackend]
        ];
    ];

FSetBackendMathematica[a___] :=
    Module[{},
        Message[FunKit::invalidArguments, FSetBackendMathematica];
        Abort[];
    ];
