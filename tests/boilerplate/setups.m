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
    trunc = <|Rdot -> {{Phi, Phi}}, Propagator -> {{Phi, Phi}}, GammaN -> {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}, S -> {{Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}, Field -> {{}}|>;
    Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetQMeSDSESetupScalar[] :=
  Module[{i, j, p, fields, trunc, classAct},
    classAct = {{Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}};
    fields = <|"bosonic" -> {Phi[p]}, "fermionic" -> {}|>;
    trunc = {{Phi}, {Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}};
    Return[<|"MasterEquation" -> <|"getDSE" -> "True", "classicalAction" -> classAct|>,
      "FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetDoFunSetupScalar[] :=
"
actionONSymbolic={{Phi,4}};
setFields[{Phi}];
";

(* Yukawa Setups *)

GetQMeSWetterichSetupYukawa[] :=
  Module[{i, j, a, b, p, fields, eq, trunc},
    eq = {"Prefactor" -> {1/2}, <|"type" -> "Regulatordot", "indices" -> {i, j}|>, <|"type" -> "Propagator", "indices" -> {i, j}|>};
    fields = <|"bosonic" -> {Phi[p]}, "fermionic" -> {{Psibar[p, {a}], Psi[p, {a}]}}|>;
    trunc = {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}};
    Return[<|"MasterEquation" -> eq, "FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetFunKitSetupYukawa[] :=
  Module[{p, fields, eq, trunc},
    fields = <|"Commuting" -> {Phi[p]}, "Grassmann" -> {{Psibar[p, {a}], Psi[p, {a}]}}|>;
    trunc = <|Rdot -> {{Phi, Phi}, {Psi, Psibar}}, Propagator -> {{Phi, Phi}, {Psi, Psibar}}, GammaN -> {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}}|>;
    Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetDoFunSetupYukawa[] :=
"
actionYukawaSymbolic={{Phi,2}, {Psi, Psibar}, {Psi, Psibar, Phi}};
setFields[{Phi}, {{Psi, Psibar}}];
";

(* Four-Fermion Setup *)

GetFunKitSetupFourFermion[] :=
  Module[{p, a, fields, trunc},
    fields = <|"Commuting" -> {}, "Grassmann" -> {{Psibar[p, {a}], Psi[p, {a}]}}|>;
    trunc = <|
        Rdot -> {{Psi, Psibar}},
        Propagator -> {{Psi, Psibar}},
        GammaN -> {{Psibar, Psi}, {Psibar, Psibar, Psi, Psi}}
    |>;
    Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetQMeSWetterichSetupFourFermion[] :=
  Module[{i, j, a, p, fields, eq, trunc},
    eq = {"Prefactor" -> {1/2}, <|"type" -> "Regulatordot", "indices" -> {i, j}|>, <|"type" -> "Propagator", "indices" -> {i, j}|>};
    fields = <|"bosonic" -> {}, "fermionic" -> {{Psibar[p, {a}], Psi[p, {a}]}}|>;
    trunc = {{Psibar, Psi}, {Psibar, Psibar, Psi, Psi}};
    Return[<|"MasterEquation" -> eq, "FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetDoFunSetupFourFermion[] :=
"
actionFourFermionSymbolic={{Psi,Psibar},{Psi,Psi,Psibar,Psibar}};
setFields[{},{{Psi,Psibar}}];
";

(* Extended Yukawa Setups — includes four-fermion vertex *)

GetFunKitSetupYukawaExtended[] :=
  Module[{p, fields, eq, trunc},
    fields = <|"Commuting" -> {Phi[p]}, "Grassmann" -> {{Psibar[p, {a}], Psi[p, {a}]}}|>;
    trunc = <|Rdot -> {{Phi, Phi}, {Psi, Psibar}}, Propagator -> {{Phi, Phi}, {Psi, Psibar}}, GammaN -> {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}, {Psibar, Psibar, Psi, Psi}}|>;
    Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetQMeSWetterichSetupYukawaExtended[] :=
  Module[{i, j, a, b, p, fields, eq, trunc},
    eq = {"Prefactor" -> {1/2}, <|"type" -> "Regulatordot", "indices" -> {i, j}|>, <|"type" -> "Propagator", "indices" -> {i, j}|>};
    fields = <|"bosonic" -> {Phi[p]}, "fermionic" -> {{Psibar[p, {a}], Psi[p, {a}]}}|>;
    trunc = {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}, {Psibar, Psibar, Psi, Psi}};
    Return[<|"MasterEquation" -> eq, "FieldSpace" -> fields, "Truncation" -> trunc|>];
  ];

GetDoFunSetupYukawaExtended[] :=
"
actionYukawaExtSymbolic={{Phi,2},{Psi,Psibar},{Psi,Psibar,Phi},{Psi,Psi,Psibar,Psibar}};
setFields[{Phi},{{Psi,Psibar}}];
";

(* Yang-Mills Setups *)

GetFunKitSetupYangMills[] :=
    Module[{p, v, col},
        <|
            "FieldSpace" -> <|
                "Commuting" -> {A[p, {v, col}]},
                "Grassmann" -> {{cb[p, {col}], c[p, {col}]}}
            |>,
            "Truncation" -> <|
                GammaN -> {{A, A}, {A, A, A}, {A, A, A, A}, {A, cb, c}, {cb, c}},
                Propagator -> {{A, A}, {cb, c}},
                Rdot -> {{A, A}, {cb, c}},
                S -> {{A, A}, {A, A, A}, {A, A, A, A}, {cb, c}, {cb, c, A}},
                Field -> {{}}
            |>
        |>
    ];

GetQMeSDSESetupYangMills[] :=
    Module[{p, v, col, fields, trunc, classAct},
        classAct = {{A, A}, {A, A, A}, {A, A, A, A}, {cb, c}, {cb, c, A}};
        fields = <|"bosonic" -> {A[p, {v, col}]}, "fermionic" -> {{cb[p, {col}], c[p, {col}]}}|>;
        trunc = {{A, A}, {A, A, A}, {A, A, A, A}, {A, cb, c}, {cb, c}};
        <|"MasterEquation" -> <|"getDSE" -> "True", "classicalAction" -> classAct|>,
          "FieldSpace" -> fields, "Truncation" -> trunc|>
    ];

GetQMeSWetterichSetupYangMills[] :=
    Module[{i, j, p, v, col, fields, eq, trunc},
        eq = {"Prefactor" -> {1/2}, <|"type" -> "Regulatordot", "indices" -> {i, j}|>, <|"type" -> "Propagator", "indices" -> {i, j}|>};
        fields = <|"bosonic" -> {A[p, {v, col}]}, "fermionic" -> {{cb[p, {col}], c[p, {col}]}}|>;
        trunc = {{A, A}, {A, A, A}, {A, A, A, A}, {A, cb, c}, {cb, c}};
        <|"MasterEquation" -> eq, "FieldSpace" -> fields, "Truncation" -> trunc|>
    ];

GetDoFunSetupYangMills[] :=
"
actionYMSymbolic={{A,2},{A,3},{A,4},{cb,c},{cb,c,A}};
setFields[{A},{{c,cb}}];
";

(* Yukawa setup with source fields *)

GetFunKitSetupWithSources[] :=
    Module[{p, a, fields, trunc},
        fields = <|
            "Commuting" -> {Phi[p]},
            "Grassmann" -> {{Psibar[p, {a}], Psi[p, {a}]}},
            "CommutingSource" -> {J[p]},
            "GrassmannSource" -> {eta[p, {a}]}
        |>;
        trunc = <|
            Rdot -> {{Phi, Phi}, {Psi, Psibar}},
            Propagator -> {{Phi, Phi}, {Psi, Psibar}, {J, Phi}},
            GammaN -> {{Phi}, {Psi, Psibar}, {Phi, Phi},
                       {Psi, Psibar, Phi}, {J}, {J, Phi}}
        |>;
        Return[<|"FieldSpace" -> fields, "Truncation" -> trunc|>];
    ];
