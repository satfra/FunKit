(* YukawaBench.m — Yukawa theory benchmarks: FunKit vs DoFun vs QMeS *)

benchmarks = {};

yFK    = GetFunKitSetupYukawa[];
yQMeS  = GetQMeSWetterichSetupYukawa[];
yDoFun = GetDoFunSetupYukawa[];

(* Fermion 2-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Yukawa: Fermion 2-point (Wetterich)",
    {Psi[i1], Psibar[i2]},
    yFK, yQMeS,
    yDoFun, "actionYukawaSymbolic,{Psi,Psibar}"
]];

(* Scalar 2-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Yukawa: Scalar 2-point (Wetterich)",
    {Phi[i1], Phi[i2]},
    yFK, yQMeS,
    yDoFun, "actionYukawaSymbolic,{Phi,Phi}"
]];

(* Yukawa vertex *)
AppendTo[benchmarks, BenchmarkCase[
    "Yukawa: 3-point vertex (Wetterich)",
    {Psi[i1], Psibar[i2], Phi[i3]},
    yFK, yQMeS,
    yDoFun, "actionYukawaSymbolic,{Psi,Psibar,Phi}"
]];

(* Scalar 4-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Yukawa: Scalar 4-point (Wetterich)",
    {Phi[i1], Phi[i2], Phi[i3], Phi[i4]},
    yFK, yQMeS,
    yDoFun, "actionYukawaSymbolic,{Phi,Phi,Phi,Phi}"
]];
