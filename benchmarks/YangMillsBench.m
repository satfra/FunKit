(* YangMillsBench.m — Yang-Mills benchmarks: DSE + Flow equations, FunKit vs DoFun vs QMeS *)

benchmarks = {};

ymFK    = GetFunKitSetupYangMills[];
ymQMeSDSE  = GetQMeSDSESetupYangMills[];
ymQMeSWett = GetQMeSWetterichSetupYangMills[];
ymDoFun = GetDoFunSetupYangMills[];

(* ==================== DSE benchmarks ==================== *)

(* Gluon DSE (1-point) *)
AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Gluon DSE",
    A[i1], {},
    ymFK, ymQMeSDSE,
    ymDoFun, "doDSE[actionYMSymbolic,{A}]"
]];

(* Gluon 2-point DSE *)
AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Gluon 2-point DSE",
    A[i1], {A[i2]},
    ymFK, ymQMeSDSE,
    ymDoFun, "doDSE[actionYMSymbolic,{A,A}]"
]];

(* Three-gluon vertex DSE *)
AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Three-gluon vertex DSE",
    A[i1], {A[i2], A[i3]},
    ymFK, ymQMeSDSE,
    ymDoFun, "doDSE[actionYMSymbolic,{A,A,A}]"
]];

(* ==================== Flow equation (Wetterich) benchmarks ==================== *)

(* Gluon 2-point flow *)
AppendTo[benchmarks, BenchmarkCase[
    "Yang-Mills: Gluon 2-point (Wetterich)",
    {A[i1], A[i2]},
    ymFK, ymQMeSWett,
    ymDoFun, "actionYMSymbolic,{A,A}"
]];

(* Ghost 2-point flow — blocked by ghost derivative validation issue *)
(* AppendTo[benchmarks, BenchmarkCase[
    "Yang-Mills: Ghost 2-point (Wetterich)",
    {cb[i1], c[i2]},
    ymFK, ymQMeSWett,
    ymDoFun, "actionYMSymbolic,{cb,c}"
]]; *)

(* Three-gluon vertex flow *)
AppendTo[benchmarks, BenchmarkCase[
    "Yang-Mills: Three-gluon vertex (Wetterich)",
    {A[i1], A[i2], A[i3]},
    ymFK, ymQMeSWett,
    ymDoFun, "actionYMSymbolic,{A,A,A}"
]];

(* Ghost-gluon vertex flow — blocked by ghost derivative validation issue *)
(* AppendTo[benchmarks, BenchmarkCase[
    "Yang-Mills: Ghost-gluon vertex (Wetterich)",
    {A[i1], cb[i2], c[i3]},
    ymFK, ymQMeSWett,
    ymDoFun, "actionYMSymbolic,{A,cb,c}"
]]; *)

(* Ghost-gluon vertex DSE — blocked by ghost DSE setup issue (derivList validation) *)
(* AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Ghost-gluon vertex DSE",
    cb[i1], {c[i2], A[i3]},
    ymFK, ymQMeSDSE,
    ymDoFun, "doDSE[actionYMSymbolic,{cb,c,A}]"
]]; *)
