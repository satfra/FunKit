(* BuildTraceSumCache.m -- regenerate tests/COEN/TraceSumFlow.m.

   The fixture is the integrand NumTracer hands to FunKit`MakeCppFunction on its lambda3d(small)
   general-frame flow. We capture it by hooking FunKit`Private`optimizeExpression -- the first
   thing CppCode does with the integrand -- rather than by re-deriving it here, so the fixture
   stays a faithful copy of what a real NumTracer flow presents to COEN.

   Needs a NumTracer checkout (plus DiFfRG and FORM); the committed fixture does not. Point
   $NumTracerDirectory at it, or set the NUMTRACER_DIR environment variable.

   Run:  wolfram -script benchmarks/util/BuildTraceSumCache.m
   Env:  NT_TEST_NDIAG   how many diagrams to keep (0 = all; default 0). The generator's own
                         cut-down dial -- lower it for a smaller fixture. *)

$NumTracerDirectory =
    With[{e = Environment["NUMTRACER_DIR"]},
        If[StringQ[e],
            e
            ,
            FileNameJoin[{ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]], "..", "NumTracer_bckp"}]
        ]
    ];

$out = FileNameJoin[{ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]], "tests", "COEN", "TraceSumFlow.m"}];

If[!DirectoryQ[$NumTracerDirectory],
    Print["NumTracer checkout not found at ", $NumTracerDirectory, "."];
    Print["Set NUMTRACER_DIR, or keep the committed tests/COEN/TraceSumFlow.m as is."];
    Exit[0];
];

Needs["FunKit`"];
Get["DiFfRG`"];

FSetRegisterSize[64];

$repo = $NumTracerDirectory;
SetDirectory[$repo];
DefineFormExecutable["/usr/bin/tform -w16"];
Get[FileNameJoin[{$repo, "numtracer", "tests", "qcd_lambda3d_setup.m"}]];
Get[FileNameJoin[{$repo, "numtracer", "mathematica", "NumTracer.m"}]];
SetOptions[MakeNTKernel, Sequence @@ $ntShimOpts];

SetEnvironment["NT_GEN_JOBS" -> "8"];
SetEnvironment["NT_GEN_MAXW" -> "8"];
SetEnvironment["NT_GEN_MAXW_B" -> "4"];

dress = {ZAqbq1, Zq, Mq, ZA, dtZA, dtZq};

(* The four-quark derivative, exactly as with_mesons' lambda1Raw takes it -- see
   numtracer/tests/gen/gen_lambda3d_small_numeric.wls for why only the COMBINED exchange is a
   symmetry of the channel. *)
sym = FMakeSymmetryList[FSymmetry[Symmetric, {i1, i3}, {i2, i4}]];
fRG4q = FTakeDerivatives[WetterichEquation, {qb[i1], q[i2], qb[i3], q[i4]}, "Symmetries" -> sym] //
    FTruncate // FRoute;
proj = FTerm[TBGetProjector["FierzCompleteNf2Nc34D", 1,
    {i2, i1, i4, i3} /. fRG4q["1-Loop"]["ExternalIndices"]]];
trace = FTerm[proj, fRG4q["1-Loop"]["Expression"] /. FMakeDiagrammaticRules[]];

(* Two-momentum configuration; ReplaceAll (single pass), NOT ReplaceRepeated -- the renaming is
   simultaneous, and p1 -> p2 would otherwise be rewritten again by the p2 rule. *)
trace3d = trace /. {p1 -> p2, p2 -> -p2, p3 -> -p3, p4 -> p3};

net = FromFunKit[S0Param[dressingRules[trace3d]]];
Print["lambda3d(small) net: top terms = ", If[Head[net] === Plus, Length[net], 1]];

nKeep = With[{e = Environment["NT_TEST_NDIAG"]},
    If[StringQ[e] && IntegerQ[Quiet@ToExpression[e]], ToExpression[e], 0]];
If[nKeep > 0 && Head[net] === Plus && Length[net] > nKeep,
    net = Plus @@ Take[Sort[List @@ net], nKeep];
    Print["cut down to ", nKeep, " diagrams (NT_TEST_NDIAG)"]];

ntk = NumTrace[net, "Frame" -> frameS0S1Sphi, "Args" -> argsS0S1SPhi];

$header = "(* TraceSumFlow.m -- a NumTracer-shaped integrand, frozen.

   Captured from NumTracer's lambda3d(small) GENERAL-FRAME flow (the four-quark vertex at
   two-momentum (S0,S1,SPhi) kinematics), at the point where NumTracer hands the integrand to
   FunKit`MakeCppFunction. Regenerate with benchmarks/util/BuildTraceSumCache.m.

   Why this fixture exists, and why ZA4Flow.mx cannot stand in for it: a NumTracer kernel is a
   SUM OVER TRACE REFERENCES -- opaque ntRe[\"...tr<N>(fenv)\"] calls into a separately generated
   traces header, multiplied by dressing functions of nested radical momenta. That shape is what
   makes COEN's earlySplit chop the integrand into independent sub-kernels, and it is the shape
   whose emission dominates Wolfram-side generation time on production flows (~98% of it). The
   ZA4 flow is dense polynomial algebra with few top-level terms and does not exercise it.

   The expression is inert: it needs no NumTracer, DiFfRG or FORM to load. The only heads in it
   are ntRe, RB, RBdot, RF, the dressings (ZA, dtZA, Zq, Mq, ZAqbq1) and built-ins. *)

";

(* Hook the first thing CppCode does with the integrand, dump it and stop. Prepending a
   catch-all DownValue is enough: nothing downstream of the capture is needed. *)
Unprotect[FunKit`Private`optimizeExpression];
DownValues[FunKit`Private`optimizeExpression] =
    Prepend[DownValues[FunKit`Private`optimizeExpression],
        HoldPattern[FunKit`Private`optimizeExpression[e_]] :> (
            Print["captured integrand: LeafCount = ", LeafCount[e],
                ", ByteCount = ", ByteCount[e],
                ", top-level head = ", Head[e],
                ", top-level length = ", Length[e]];
            Export[$out, e, "Package"];
            Export[$out, $header <> Import[$out, "Text"], "Text"];
            Print["wrote ", $out, " (", FileByteCount[$out], " bytes)"];
            Quit[0])];

MakeNTKernel[ntk,
    FileNameJoin[{$TemporaryDirectory, "tracesum_capture_gen.cpp"}],
    FileNameJoin[{$TemporaryDirectory, "TraceSumCapture_num_kernel.hh"}],
    FileNameJoin[{$TemporaryDirectory, "TraceSumCapture_num_kernels.hh"}],
    "Name" -> "Capture_num_kernel", "Namespace" -> "capture_num",
    "Dressings" -> dress];

Print["ERROR: the optimizeExpression hook never fired -- the capture point moved."];
Exit[1];
