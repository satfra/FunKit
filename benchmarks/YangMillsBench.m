(* YangMillsBench.m — Yang-Mills DSE benchmarks: FunKit vs DoFun vs QMeS *)

benchmarks = {};

ymFK    = GetFunKitSetupYangMills[];
(* QMeS DSE integration with SuperindexDiagrams has issues for YM — skip for now *)
ymQMeS  = None;
ymDoFun = GetDoFunSetupYangMills[];

(* Gluon DSE (1-point) *)
AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Gluon DSE",
    A[i1], {},
    ymFK, ymQMeS,
    ymDoFun, "doDSE[actionYMSymbolic,{A}]"
]];

(* Gluon 2-point DSE *)
AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Gluon 2-point DSE",
    A[i1], {A[i2]},
    ymFK, ymQMeS,
    ymDoFun, "doDSE[actionYMSymbolic,{A,A}]"
]];

(* Three-gluon vertex DSE *)
AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Three-gluon vertex DSE",
    A[i1], {A[i2], A[i3]},
    ymFK, ymQMeS,
    ymDoFun, "doDSE[actionYMSymbolic,{A,A,A}]"
]];

(* Ghost-gluon vertex DSE — blocked by ghost DSE setup issue (derivList validation) *)
(* AppendTo[benchmarks, BenchmarkDSECase[
    "Yang-Mills: Ghost-gluon vertex DSE",
    cb[i1], {c[i2], A[i3]},
    ymFK, ymQMeS,
    ymDoFun, "doDSE[actionYMSymbolic,{cb,c,A}]"
]]; *)
