(* BuildZA4Cache.m -- one-time builder for the four-gluon-vertex codegen benchmark.

   Reproduces the upstream of examples/Yang-Mills/Yang-Mills.wl (setup + the A4
   derivation/trace) to obtain FlowA4 -- the integrand handed to
   MakeKernel -- and DumpSaves it to ZA4Flow.mx so ProfileCodegen.m can time code
   generation repeatedly without re-running the expensive symbolic+FORM pipeline.

   FlowA4 itself does not need DiFfRG; only MakeKernel (in the profiler) does. FORM
   traces are cached under examples/Yang-Mills/TraceCache, so this is cheap on reruns.

   Run once:  wolfram -script benchmarks/util/BuildZA4Cache.m *)

$utilDir = DirectoryName[$InputFileName];
Import[FileNameJoin[{ParentDirectory[$utilDir], "init.m"}]];

$exampleDir = FileNameJoin[{$mDir, "examples", "Yang-Mills"}];
$cacheFile = FileNameJoin[{$utilDir, "ZA4Flow.mx"}];

SetDirectory[$exampleDir];
DefineFormExecutable["/usr/bin/tform -w16"];

(* ---- setup (Yang-Mills.m:28-119) ---- *)
fields = <|"Commuting" -> {A[p, {v, c}]}, "Grassmann" -> {{cb[p, {c}], c[p, {c}]}}|>;
truncation = <|
    GammaN -> {{A, A}, {A, A, A}, {A, A, A, A}, {A, cb, c}, {cb, c}},
    Propagator -> {{A, A}, {cb, c}}, Rdot -> {{A, A}, {cb, c}},
    S -> {{A, A}, {A, A, A}, {A, A, A, A}, {cb, c}, {cb, c, A}}, Field -> {{}}|>;
bases = <|
    GammaN -> {{A, A} -> {"AA", 1}, {A, A, A} -> "AAAClass", {A, A, A, A} -> "AAAAClass", {A, cb, c} -> {"Acbc", 1}, {cb, c} -> "cbc"},
    S -> {{A, A} -> {"AA", 1}, {A, A, A} -> "AAAClass", {A, A, A, A} -> "AAAAClass", {A, cb, c} -> {"Acbc", 1}, {cb, c} -> "cbc"},
    Propagator -> {{A, A} -> {"AA", 1}, {cb, c} -> "cbc"},
    Rdot -> {{A, A} -> {"AA", 1}, {cb, c} -> "cbc"}|>;
diagramStyling = <|"Styles" -> {A -> {Orange}, c -> {Black, Dashed}}|>;
FSetTexStyles[cb -> "\\bar{c}"];
Setup = <|"FieldSpace" -> fields, "Truncation" -> truncation, "FeynmanRules" -> bases, "DiagramStyling" -> diagramStyling|>;
FSetGlobalSetup[Setup];

SP3Patt[p1e_, p2e_, p3e_] := {Sqrt[(sp[p1, p1] + sp[p2, p2] + sp[p3, p3]) / 3]} /. {p1 :> p1e, p2 :> p2e, p3 :> p3e} // UseLorentzLinearity // FullSimplify;
SP4Patt[p1e_, p2e_, p3e_, p4e_] := {Sqrt[(sp[p1, p1] + sp[p2, p2] + sp[p3, p3] + sp[p4, p4]) / 4]} /. {p1 :> p1e, p2 :> p2e, p3 :> p3e, p4 :> p4e} // UseLorentzLinearity // FullSimplify;

dressingRules = ReplaceRepeated[#, {
    dressing[GammaN, {cb, c}, 1, {p1_, p2_}] :> -Zc[Sqrt[sp[p2, p2]]] sp[p2, p2],
    dressing[GammaN, {A, A}, 1, {p1_, p2_}] :> ZA[Sqrt[sp[p2, p2]]] sp[p2, p2],
    dressing[InverseProp, {cb, c}, 1, {p1_, p2_}] :> -(Zc[Sqrt[sp[p2, p2]]] sp[p2, p2] + RB[k^2, sp[p2, p2]] Zc[k]),
    dressing[InverseProp, {A, A}, 1, {p1_, p2_}] :> ZA[Sqrt[sp[p2, p2]]] sp[p2, p2] + RB[k^2, sp[p2, p2]] ZA[evP],
    dressing[GammaN, {A, cb, c}, 1, {p1_, p2_, p3_}] :> ZAcbc[p1, p2],
    dressing[GammaN, {A, A, A}, 1, {p1_, p2_, p3_}] :> ZA3[p1, p2],
    dressing[GammaN, {A, A, A, A}, 1, {p1_, p2_, p3_, p4_}] :> ZA4[p1, p2, p3],
    ZAcbc[p1_, p2_] :> ZAcbc @@ SP3Patt[p1, p2, -p1 - p2],
    ZA3[p1_, p2_] :> ZA3 @@ SP3Patt[p1, p2, -p1 - p2],
    ZA4[p1_, p2_, p3_] :> ZA4 @@ SP4Patt[p1, p2, p3, -p1 - p2 - p3],
    nZA -> 6,
    evP :> (k^nZA + 1)^(1/nZA),
    devP :> k^(-1 + nZA) (1 + k^nZA)^(-1 + 1/nZA),
    dressing[Rdot, {A, A}, 1, {p1_, p2_}] :> ZA[evP] RBdot[k^2, sp[p2, p2]] + RB[k^2, sp[p2, p2]] (dtZA[evP] + k*devP*(ZA[1.02 evP] - ZA[evP]) / (0.02*evP)),
    dressing[Rdot, {cb, c}, 1, {p1_, p2_}] :> Zc[k] RBdot[k^2, sp[p2, p2]] + RB[k^2, sp[p2, p2]] (dtZc[k] + k (Zc[1.02*k] - Zc[k]) / (0.02*k))
}]&;
FSetSymmetricDressing[GammaN, {A, A}];

SP4FormRule = FMakeSPFormRule[{l1, lf1}, p, {p1, p2, p3, p4}];
SPParam[expr_] := UseLorentzLinearity[expr] //. {
    lf1 -> l1, sp[p, p] -> p^2, sp[l1, l1] -> l1^2,
    sp[l1, p1] -> p l1 cos[l1, p1], sp[l1, p2] -> p l1 cos[l1, p2],
    sp[l1, p3] -> p l1 cos[l1, p3], sp[l1, p4] -> p l1 cos[l1, p4],
    Sqrt[a_^2] :> a, (a_^2)^(n_/2) :> a^n, Power[Power[l1_, 2], Rational[n_, 2]] :> l1^n,
    cos[l1, p1] :> cosl1p1, cos[l1, p2] :> cosl1p2, cos[l1, p3] :> cosl1p3, cos[l1, p4] :> cosl1p4};

SetNc[3];
$Assumptions = k > 0 && p > 0 && l1 > 0 && -1 < cos1 < 1 && -1 < cos2 < 1 && -1 < cos3 < 1;

FSetCacheDirectory[FileNameJoin[{$exampleDir, "TraceCache"}] <> "/"];
FSetRegisterSize[64];

(* ---- four-gluon vertex flow (Yang-Mills.m:242-246) ---- *)
Print["Deriving four-gluon vertex flow (this is the expensive, one-time step)..."];
{tBuild, FlowA4} = AbsoluteTiming[
    Module[{fRGA4, projectorA4, traceExprA4},
        fRGA4 = FTakeDerivatives[WetterichEquation, {A[i1], A[i2], A[i3], A[i4]}] // FTruncate // FPlot // FRoute // FPrint;
        projectorA4 = FTerm[TBGetProjector["AAAAClassTrans", 1, {i1, i2, i3, i4} /. fRGA4["1-Loop"]["ExternalIndices"]]] // TBProjectToSymmetricPoint[#, l1, p, p1, p2, p3, p4]& // Simplify;
        traceExprA4 = projectorA4 ** (fRGA4["1-Loop"]["Expression"] /. FMakeDiagrammaticRules[]) // TBProjectToSymmetricPoint[#, l1, p, p1, p2, p3, p4]&;
        FormTrace["ZA4", traceExprA4, {}, SP4FormRule] // dressingRules // TBProjectToSymmetricPoint[#, l1, p, p1, p2, p3, p4]& // SPParam // Simplify
    ]
];
Print["FlowA4 built in ", NumberForm[tBuild, {6, 2}], " s.  LeafCount = ", LeafCount[FlowA4], ", terms = ", If[Head[FlowA4] === Plus, Length[FlowA4], 1]];

DumpSave[$cacheFile, FlowA4];
Print["Cached FlowA4 -> ", $cacheFile];
Exit[0];
