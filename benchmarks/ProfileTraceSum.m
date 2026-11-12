(* ProfileTraceSum.m -- where COEN's time goes on a NumTracer-shaped kernel.

   The motivating workload for parallelising COEN emission is a NumTracer flow, where FunKit's
   C++ lowering is ~98% of the Wolfram-side generation time. That shape -- a sum over opaque
   trace references, each multiplied by dressings of nested radical momenta -- is what makes
   earlySplit chop the integrand into independent sub-kernels, and it is NOT the shape of the
   ZA4 flow that benchmarks/ProfileCodegen.m uses. tests/COEN/TraceSumFlow.m is a frozen copy of
   it; rebuild with benchmarks/util/BuildTraceSumCache.m.

   Reports three things, in this order:

     1. The per-pass breakdown, plus the RESIDUAL (wall - sum of passes). The residual is the
        serial glue no counter covers -- Table scaffolding, accTypeLeaves, getAllVarNames, the
        final StringJoin. A large residual means the passes are not where the time is and no
        amount of parallelising them will help.
     2. The sub-kernel count and the skew of their optimisation times. The count bounds any
        parallel speedup; the skew (max/total) bounds it again, since earlySplit chunks by term
        COUNT, not by cost.
     3. A serial sweep over $codeMaxKernelTerms and $kernelSplitLevel. If wall time is flat or
        improves as the chunks get smaller, the parallel-friendly shape is already available.
        If it worsens monotonically, more workers will cost more than they save.

   Run:  wolfram -script benchmarks/ProfileTraceSum.m *)

Import[FileNameJoin[{DirectoryName[$InputFileName], "init.m"}]];

$fixture = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "tests", "COEN", "TraceSumFlow.m"}];

If[!FileExistsQ[$fixture],
    Print["ERROR: ", $fixture, " missing. Rebuild it:"];
    Print["  wolfram -script benchmarks/util/BuildTraceSumCache.m"];
    Exit[1];
];

flow = Get[$fixture];

FSetRegisterSize[64];

reset := FunKit`Private`ResetCodegenProfile[];
report := FunKit`Private`PrintCodegenProfile[];
fmt[t_] := ToString[NumberForm[N[t], {7, 4}]];

Print[""];
Print["=== fixture ==="];
Print["  LeafCount:        ", LeafCount[flow]];
Print["  ByteCount:        ", ByteCount[flow]];
Print["  top-level head:   ", Head[flow]];
Print["  top-level terms:  ", If[Head[flow] === Plus, Length[flow], 1]];
Print["  register size:    ", 64];
Print["  $codeMaxKernelTerms: ", FunKit`Private`$codeMaxKernelTerms,
    ",  $kernelSplitLevel: ", FunKit`Private`$kernelSplitLevel];

(* Warm up: OptimizeExpression and the CExpression rule installation both have first-call costs
   that would otherwise land in the measured run. *)
Print[""];
Print["Warming up..."];
FunKit`CppCode[flow];

(* ============================================================ *)
Print[""];
Print["=== per-pass breakdown (serial) ==="];
reset;
{tWall, code} = AbsoluteTiming[FunKit`CppCode[flow]];
Print["CppCode wall:            ", fmt[tWall], " s   (output ", StringLength[code], " chars)"];
report;

Module[{sumPasses, residual},
    sumPasses =
        FunKit`Private`$ProfileCgHoist + FunKit`Private`$ProfileCgSplit +
        FunKit`Private`$ProfileCgCSE + FunKit`Private`$ProfileCgPowerNorm +
        FunKit`Private`$ProfileCgTopoSort + FunKit`Private`$ProfileCgFactor +
        FunKit`Private`$ProfileCgTranscendental + FunKit`Private`$ProfileCgFMA +
        FunKit`Private`$ProfileCgSimplify + FunKit`Private`$ProfileCgCppForm +
        FunKit`Private`$ProfileCgStripNames + FunKit`Private`$ProfileCgClangFormat;
    residual = tWall - sumPasses;
(* THE decision number. Above ~50% the counters are measuring a minority of the run and the
   next move is more instrumentation, not more parallelism. *)
    Print[""];
    Print["  RESIDUAL (wall - passes): ", fmt[residual], " s  (",
        ToString[NumberForm[100. residual / tWall, {4, 1}]], "% of wall)"];
];

(* ============================================================ *)
Print[""];
Print["=== split sweep (serial): $codeMaxKernelTerms ==="];
Print["  terms   subkernels   wall       CSE        emit       max/total"];
Module[{saved = FunKit`Private`$codeMaxKernelTerms},
    Do[
        FSetMaxKernelTerms[n];
        reset;
        Module[{t, nSub, skew},
            t = First @ AbsoluteTiming[FunKit`CppCode[flow]];
            nSub = Length[FunKit`Private`$ProfileCgSubKernelTimes];
            skew =
                If[nSub > 0,
                    Max[FunKit`Private`$ProfileCgSubKernelTimes] /
                        Total[FunKit`Private`$ProfileCgSubKernelTimes]
                    ,
                    1.
                ];
            Print["  ", ToString[n], "\t", nSub, "\t     ", fmt[t], "  ", fmt[FunKit`Private`$ProfileCgCSE],
                "  ", fmt[FunKit`Private`$ProfileCgEmitScaffold], "  ",
                ToString[NumberForm[N[skew], {4, 3}]]];
        ]
        ,
        {n, {4000, 2000, 1000, 500, 250, 125}}
    ];
    FSetMaxKernelTerms[saved];
];

Print[""];
Print["=== split sweep (serial): $kernelSplitLevel ==="];
Print["  level   subkernels   wall       CSE        emit       max/total"];
Module[{saved = FunKit`Private`$kernelSplitLevel},
    Do[
        FSetKernelSplitLevel[lvl];
        reset;
        Module[{t, nSub, skew},
            t = First @ AbsoluteTiming[FunKit`CppCode[flow]];
            nSub = Length[FunKit`Private`$ProfileCgSubKernelTimes];
            skew =
                If[nSub > 0,
                    Max[FunKit`Private`$ProfileCgSubKernelTimes] /
                        Total[FunKit`Private`$ProfileCgSubKernelTimes]
                    ,
                    1.
                ];
            Print["  ", ToString[lvl], "\t", nSub, "\t     ", fmt[t], "  ", fmt[FunKit`Private`$ProfileCgCSE],
                "  ", fmt[FunKit`Private`$ProfileCgEmitScaffold], "  ",
                ToString[NumberForm[N[skew], {4, 3}]]];
        ]
        ,
        {lvl, {1, 2, 3}}
    ];
    FSetKernelSplitLevel[saved];
];

Print[""];
Exit[0];
