(* ProfileCodegen.m -- Profile where time is spent generating C++ code for the
   four-gluon vertex (ZA4) of the Yang-Mills + DiFfRG example.

   Loads the cached FlowA4 integrand (benchmarks/util/ZA4Flow.mx; build it once with
   benchmarks/util/BuildZA4Cache.m), then times FunKit`CppCode and -- if DiFfRG is
   available -- the full MakeKernel, attributing time across the COEN optimization
   passes via the zero-overhead counters in modules/COEN (Reset/PrintCodegenProfile).

   Run:  wolfram -script benchmarks/ProfileCodegen.m *)

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

$cacheFile = FileNameJoin[{DirectoryName[$InputFileName], "util", "ZA4Flow.mx"}];
If[!FileExistsQ[$cacheFile],
    Print["ERROR: ", $cacheFile, " missing. Build it first:"];
    Print["  wolfram -script benchmarks/util/BuildZA4Cache.m"];
    Exit[1];
];
Get[$cacheFile];
Print["Loaded FlowA4: LeafCount = ", LeafCount[FlowA4],
    ", top-level head = ", Head[FlowA4],
    ", register size = ", 64];
FSetRegisterSize[64];

reset := FunKit`Private`ResetCodegenProfile[];
report := FunKit`Private`PrintCodegenProfile[];

(* ============================================================ *)
Print[""];
Print["=== FunKit CppCode (the core codegen path) ==="];
Print["Warming up..."];
FunKit`CppCode[FlowA4];                       (* JIT / OptimizeExpression first-call cost *)

reset;
{tCpp, codeCpp} = AbsoluteTiming[FunKit`CppCode[FlowA4]];
Print["CppCode total: ", NumberForm[tCpp, {6, 3}], " s  (output ", StringLength[codeCpp], " chars)"];
report;
Print["  (clang-format is 0 here: CppCode returns a raw string; formatting happens in MakeKernel)"];

(* ============================================================ *)
Print[""];
Print["=== Full MakeKernel (end-to-end, incl. DiFfRG templating + clang-format) ==="];
diffrgOK = Quiet[Check[Get["DiFfRG`"]; True, False]];
If[TrueQ[diffrgOK],
    Module[{interpolatorType, kernelParameterList, SP4Defs, tmpDir},
        interpolatorType = "SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>";
        kernelParameterList = {
            <|"Name" -> "k", "Type" -> "double"|>,
            <|"Name" -> "ZA3", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>,
            <|"Name" -> "ZAcbc", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>,
            <|"Name" -> "ZA4", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>,
            <|"Name" -> "dtZc", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>,
            <|"Name" -> "Zc", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>,
            <|"Name" -> "dtZA", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>,
            <|"Name" -> "ZA", "Type" -> interpolatorType, "Const" -> True, "Reference" -> True|>};
        SP4Defs = DeclareSymmetricPoints4DP4[l1, p, {p1, p2, p3, p4}];
        (* write generated files to a throwaway dir so the committed example is untouched *)
        tmpDir = CreateDirectory[];
        SetDirectory[tmpDir];
        Print["Warming up MakeKernel (writing to ", tmpDir, ")..."];
        MakeKernel[FlowA4, "Name" -> "ZA4", "Integrator" -> "Integrator_p2_4D_3ang", "d" -> 4,
            "AD" -> False, "ctype" -> "double", "Device" -> "GPU", "Type" -> "double",
            "Parameters" -> kernelParameterList, "KernelBody" -> SP4Defs,
            "IntegrationVariables" -> {"l1", "cos1", "cos2", "phi"},
            "Coordinates" -> {"LogarithmicCoordinates1D<double>"}, "CoordinateArguments" -> {"p"}];

        reset;
        {tMake, _} = AbsoluteTiming[
            MakeKernel[FlowA4, "Name" -> "ZA4", "Integrator" -> "Integrator_p2_4D_3ang", "d" -> 4,
                "AD" -> False, "ctype" -> "double", "Device" -> "GPU", "Type" -> "double",
                "Parameters" -> kernelParameterList, "KernelBody" -> SP4Defs,
                "IntegrationVariables" -> {"l1", "cos1", "cos2", "phi"},
                "Coordinates" -> {"LogarithmicCoordinates1D<double>"}, "CoordinateArguments" -> {"p"}]];
        Print["MakeKernel total: ", NumberForm[tMake, {6, 3}], " s"];
        report;
        Print["  DiFfRG-side remainder (MakeKernel - summed FunKit passes): ",
            NumberForm[tMake - (FunKit`Private`$ProfileCgCSE + FunKit`Private`$ProfileCgSimplify +
                FunKit`Private`$ProfileCgCppForm + FunKit`Private`$ProfileCgClangFormat +
                FunKit`Private`$ProfileCgFactor + FunKit`Private`$ProfileCgHoist +
                FunKit`Private`$ProfileCgSplit + FunKit`Private`$ProfileCgPowerNorm +
                FunKit`Private`$ProfileCgTranscendental + FunKit`Private`$ProfileCgFMA), {6, 3}], " s"];
        ResetDirectory[];
    ];
    ,
    Print["DiFfRG not available headless -- skipping end-to-end MakeKernel timing."];
    Print["The CppCode breakdown above already covers the dominant FunKit codegen cost."];
];

Exit[0];
