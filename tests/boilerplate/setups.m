(* ::Package:: *)

(* Scalar Setups *)

GetQMeSWetterichSetupScalar[] :=
  Module[{i, j, p, fields, eq, trunc},
    eq = {"Prefactor" -> {1/2}, <|"type" -> "Regulatordot", "indices" -> {i, j}|>, <|"type" -> "Propagator", "indices" -> {i, j}|>};
    fields = <|"bosonic" -> {Phi[p]}, "fermionic" -> {}|>;
    trunc = {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}};
    Return[<|"MasterEquation" -> eq, "FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetFunKitSetupScalar[] :=
  Module[{p, fields, eq, trunc},
    fields = <|"Commuting" -> {Phi[p]}, "Grassmann" -> {}|>;
    trunc = <|Rdot -> {{Phi, Phi}}, Propagator -> {{Phi, Phi}}, GammaN -> {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}|>;
    Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
  ]

(* Yukawa Setups *)

GetQMeSWetterichSetupYukawa[] :=
  Module[{i, j, a, b, p, fields, eq, trunc},
    eq = {"Prefactor" -> {1/2}, <|"type" -> "Regulatordot", "indices" -> {i, j}|>, <|"type" -> "Propagator", "indices" -> {i, j}|>};
    fields = <|"bosonic" -> {Phi[p]}, "fermionic" -> {{Psi[p, {a}], Psibar[p, {a}]}}|>;
    trunc = {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}};
    Return[<|"MasterEquation" -> eq, "FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetFunKitSetupYukawa[] :=
  Module[{p, fields, eq, trunc},
    fields = <|"Commuting" -> {Phi[p]}, "Grassmann" -> {{Psi[p, {a}], Psibar[p, {a}]}}|>;
    trunc = <|Rdot -> {{Phi, Phi}, {Psi, Psibar}}, Propagator -> {{Phi, Phi}, {Psi, Psibar}}, GammaN -> {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}}|>;
    Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];
