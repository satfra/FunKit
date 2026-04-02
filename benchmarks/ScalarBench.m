(* ScalarBench.m — Scalar O(N) theory benchmarks: FunKit vs DoFun vs QMeS *)

benchmarks = {};

sFK = GetFunKitSetupScalar[];

sQMeS = GetQMeSWetterichSetupScalar[];

sQMeSDSE = GetQMeSDSESetupScalar[];

sDoFun = GetDoFunSetupScalar[];

(* ==================== DSE benchmarks ==================== *)

(* 2-point DSE *)

AppendTo[benchmarks, BenchmarkDSECase["Scalar Theory: 2-point DSE", Phi[i1], {Phi[i2]}, sFK, sQMeSDSE, sDoFun, "doDSE[actionONSymbolic,{Phi,Phi}]"]];

(* 3-point DSE *)

AppendTo[benchmarks, BenchmarkDSECase["Scalar Theory: 3-point DSE", Phi[i1], {Phi[i2], Phi[i3]}, sFK, sQMeSDSE, sDoFun, "doDSE[actionONSymbolic,{Phi,Phi,Phi}]"]];

(* 4-point DSE *)

AppendTo[benchmarks, BenchmarkDSECase["Scalar Theory: 4-point DSE", Phi[i1], {Phi[i2], Phi[i3], Phi[i4]}, sFK, sQMeSDSE, sDoFun, "doDSE[actionONSymbolic,{Phi,Phi,Phi,Phi}]"]];

(* ==================== Flow equation (Wetterich) benchmarks ==================== *)

(* 2-point function *)

AppendTo[benchmarks, BenchmarkCase["Scalar Theory: 2-point function (Wetterich)", {Phi[i1], Phi[i2]}, sFK, sQMeS, sDoFun, "actionONSymbolic,{Phi,Phi}"]];

(* 3-point function *)

AppendTo[benchmarks, BenchmarkCase["Scalar Theory: 3-point function (Wetterich)", {Phi[i1], Phi[i2], Phi[i3]}, sFK, sQMeS, sDoFun, "actionONSymbolic,{Phi,Phi,Phi}"]];

(* 4-point function *)

AppendTo[benchmarks, BenchmarkCase["Scalar Theory: 4-point function (Wetterich)", {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}, sFK, sQMeS, sDoFun, "actionONSymbolic,{Phi,Phi,Phi,Phi}"]];

(* 6-point function *)

AppendTo[benchmarks, BenchmarkCase["Scalar Theory: 6-point function (Wetterich)", {Phi[i1], Phi[i2], Phi[i3], Phi[i4], Phi[i5], Phi[i6]}, sFK, sQMeS, sDoFun, "actionONSymbolic,{Phi,Phi,Phi,Phi,Phi,Phi}"]];
