(* ScalarBench.m — Scalar O(N) theory benchmarks: FunKit vs DoFun vs QMeS *)

benchmarks = {};

sFK    = GetFunKitSetupScalar[];
sQMeS  = GetQMeSWetterichSetupScalar[];
sDoFun = GetDoFunSetupScalar[];

(* 2-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Scalar Theory: 2-point function (Wetterich)",
    {Phi[i1], Phi[i2]},
    sFK, sQMeS,
    sDoFun, "actionONSymbolic,{Phi,Phi}"
]];

(* 3-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Scalar Theory: 3-point function (Wetterich)",
    {Phi[i1], Phi[i2], Phi[i3]},
    sFK, sQMeS,
    sDoFun, "actionONSymbolic,{Phi,Phi,Phi}"
]];

(* 4-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Scalar Theory: 4-point function (Wetterich)",
    {Phi[i1], Phi[i2], Phi[i3], Phi[i4]},
    sFK, sQMeS,
    sDoFun, "actionONSymbolic,{Phi,Phi,Phi,Phi}"
]];
