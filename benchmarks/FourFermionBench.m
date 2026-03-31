(* FourFermionBench.m — Four-fermion theory benchmarks: FunKit vs QMeS vs DoFun *)

benchmarks = {};

ffFK    = GetFunKitSetupFourFermion[];
ffQMeS  = GetQMeSWetterichSetupFourFermion[];
ffDoFun = GetDoFunSetupFourFermion[];

(* 2-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Four-Fermion: 2-point (Wetterich)",
    {Psibar[i1], Psi[i2]},
    ffFK, ffQMeS,
    ffDoFun, "actionFourFermionSymbolic,{Psibar,Psi}"
]];

(* 4-point function *)
AppendTo[benchmarks, BenchmarkCase[
    "Four-Fermion: 4-point (Wetterich)",
    {Psibar[i1], Psibar[i2], Psi[i3], Psi[i4]},
    ffFK, ffQMeS,
    ffDoFun, "actionFourFermionSymbolic,{Psibar,Psibar,Psi,Psi}"
]];
