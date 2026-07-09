(* ::Package:: *)

(* ::Title:: *)

(*CoBra - Compiled Backend for Rapid derivation Algebra*)

(* ::Section:: *)

(*Exports*)

(* ::Input::Initialization:: *)

FSetBackendCpp::usage = "FSetBackendCpp[]
Explicitly activates the C++ backend for FTakeDerivatives, FResolveDerivatives, FTruncate and FSimplify.
Note that under the default $FunKitBackend === \"Automatic\" this happens by itself on first pipeline use; calling FSetBackendCpp[] directly is mainly useful to control the build or to see why an automatic activation failed.
On first use (and whenever the cpplib sources changed) the funkit executable is compiled with CMake and its test suite is run; the backend is only activated if build and tests succeed.
Options:
  \"Rebuild\" -> False       force a full reconfigure and rebuild
  \"RunTests\" -> True       run the cpplib test suite after building (False also skips downloading the test framework, for offline builds)
  \"SourceDirectory\" -> Automatic   cpplib source location (default: the cpplib directory shipped with FunKit)
  \"BuildDirectory\" -> Automatic    persistent build directory (default: $UserBaseDirectory/ApplicationData/FunKit/cpp-build)
  \"Jobs\" -> Automatic      parallel build jobs (default: $ProcessorCount)
Term coefficients may be numeric or index-free symbolic prefactors (couplings, Z-factors, I); the symbolic part never enters C++ and stays exact. Unsupported input (e.g. FAddFDRule rules or routed indices) issues a FunKit::cppFallback warning and runs through the Mathematica implementation instead.";

FSetBackendMathematica::usage = "FSetBackendMathematica[]
Deactivates the C++ backend; all pipeline functions use the pure-Mathematica implementation again (the default).";

FEvaluate::usage = "FEvaluate[deferred]
Forces a deferred C++ backend computation (as returned by FTakeDerivatives when the C++ backend is active) and returns a concrete FEx.
Options:
  \"Truncate\" -> False      also truncate in the same C++ run
  \"Simplify\" -> Automatic  simplify in the same C++ run (Automatic follows $AutoSimplify)";

FDeferred::usage = "FDeferred[data]
An inert handle for a deferred C++ backend computation, returned by FTakeDerivatives when the C++ backend is active.
Pass it to FTruncate, FSimplify or FEvaluate to run the fused C++ pipeline and obtain a concrete FEx.";

FDeferredQ::usage = "FDeferredQ[expr]
Returns True if expr is a deferred C++ backend computation (an FDeferred handle).";

FExportCppInput::usage = "FExportCppInput[setup, expr, derivativeList, \"file.json\"]
Serializes a setup, an FEx and a derivative list into the JSON input format of the funkit C++ engine.
FExportCppInput[setup, expr, \"file.json\"] exports without derivatives.";

FExportToml::usage = "FExportToml[setup, expr, derivativeList, \"file.toml\"]
Serializes a setup, an FEx and a derivative list into the (human-readable) TOML input format of the funkit C++ engine.
FExportToml[setup, expr, \"file.toml\"] exports without derivatives.";

FClearCppCache::usage = "FClearCppCache[]
Deletes all cached C++ backend results.";

FSetCppCacheDirectory::usage = "FSetCppCacheDirectory[dir]
Sets the directory used to cache C++ backend results (default: $UserBaseDirectory/ApplicationData/FunKit/cpp-cache).";

(* ::Section:: *)

(* Begin Private *)

(* ::Input::Initialization:: *)

Begin["`Private`"];

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FEDeriK] =!= True,
  Message[ModuleLoaded::dependency, "CoBra", "FEDeriK"];
  Abort[];
];

If[ModuleLoaded[AnSEL] =!= True,
  Message[ModuleLoaded::dependency, "CoBra", "AnSEL"];
  Abort[];
];

ModuleLoaded[CoBra] = True;

(* ::Section:: *)

(* Loading components*)

(* ::Input::Initialization:: *)

(* Backend switching and build orchestration *)

Get[$FunKitDirectory <> "modules/CoBra/Backend.m"];

(* Serialization to the C++ engine's input format, eligibility checks *)

Get[$FunKitDirectory <> "modules/CoBra/Serialize.m"];

(* The FDeferred handle and pipeline dispatch *)

Get[$FunKitDirectory <> "modules/CoBra/Deferred.m"];

(* Result ingestion and caching *)

Get[$FunKitDirectory <> "modules/CoBra/Ingest.m"];

(* ::Section:: *)

(* End Private *)

End[];
