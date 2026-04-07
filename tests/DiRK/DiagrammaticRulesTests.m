(**********************************************************************************
    Tests for DiRK FMakeDiagrammaticRules and FSetSymmetricDressing
    Covers: rule RHS correctness, DerivePropagators option, symmetric dressing
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    FMakeDiagrammaticRules: rule count and RHS structure
**********************************************************************************)

(* Note: Generated rules are RuleDelayed, so Part[rule, 2] evaluates the RHS
   prematurely and triggers TensorBases errors on unmatched patterns.
   Use FreeQ on Hold @@ {rule} to inspect structure safely. *)

AppendTo[tests, VerificationTest[
    Module[{ymSetup, rules},
        ymSetup = <|
            "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
            "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
            "FeynmanRules" -> <|
                Propagator -> {{A, A} -> "AA"},
                GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
            |>
        |>;
        FunKit`FSetNotationA[];
        rules = FunKit`FMakeDiagrammaticRules[ymSetup];
        Length[rules] === 3
    ],
    True,
    TestID -> "FMakeDiagrammaticRules: YM generates exactly 3 rules"
]];

AppendTo[tests, VerificationTest[
    Module[{ymSetup, rules},
        ymSetup = <|
            "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
            "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
            "FeynmanRules" -> <|
                Propagator -> {{A, A} -> "AA"},
                GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
            |>
        |>;
        FunKit`FSetNotationA[];
        rules = FunKit`FMakeDiagrammaticRules[ymSetup];
        Not @ FreeQ[Hold @@ {rules[[1]]}, dressing]
    ],
    True,
    TestID -> "FMakeDiagrammaticRules: YM Propagator rule contains dressing"
]];

AppendTo[tests, VerificationTest[
    Module[{ymSetup, rules},
        ymSetup = <|
            "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
            "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
            "FeynmanRules" -> <|
                Propagator -> {{A, A} -> "AA"},
                GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
            |>
        |>;
        FunKit`FSetNotationA[];
        rules = FunKit`FMakeDiagrammaticRules[ymSetup];
        Not @ FreeQ[Hold @@ {rules[[1]]}, InverseProp]
    ],
    True,
    TestID -> "FMakeDiagrammaticRules: YM Propagator rule uses InverseProp (default DerivePropagators)"
]];

AppendTo[tests, VerificationTest[
    Module[{ymSetup, rules},
        ymSetup = <|
            "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
            "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
            "FeynmanRules" -> <|
                Propagator -> {{A, A} -> "AA"},
                GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
            |>
        |>;
        FunKit`FSetNotationA[];
        rules = FunKit`FMakeDiagrammaticRules[ymSetup];
        Not @ FreeQ[Hold @@ {rules[[2]]}, dressing] && Not @ FreeQ[Hold @@ {rules[[3]]}, dressing]
    ],
    True,
    TestID -> "FMakeDiagrammaticRules: YM vertex rules contain dressing"
]];

AppendTo[tests, VerificationTest[
    Module[{ymSetup, rules},
        ymSetup = <|
            "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
            "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
            "FeynmanRules" -> <|
                Propagator -> {{A, A} -> "AA"},
                GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
            |>
        |>;
        FunKit`FSetNotationA[];
        rules = FunKit`FMakeDiagrammaticRules[ymSetup];
        AllTrue[rules, Not @ FreeQ[Hold @@ {#}, TensorBases`TBGetVertex]&]
    ],
    True,
    TestID -> "FMakeDiagrammaticRules: all rules reference TBGetVertex"
]];

(**********************************************************************************
    FMakeDiagrammaticRules: DerivePropagators -> False
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Module[{ymSetup, rules},
        ymSetup = <|
            "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
            "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
            "FeynmanRules" -> <|
                Propagator -> {{A, A} -> "AA"},
                GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
            |>
        |>;
        FunKit`FSetNotationA[];
        rules = FunKit`FMakeDiagrammaticRules[ymSetup, "DerivePropagators" -> False];
        FreeQ[Hold @@ {rules[[1]]}, InverseProp]
    ],
    True,
    TestID -> "FMakeDiagrammaticRules: DerivePropagators False omits InverseProp"
]];

(**********************************************************************************
    FSetSymmetricDressing: full symmetrization
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Module[{},
        FSetSymmetricDressing[GammaN, {A, A, A}];
        dressing[GammaN, {A, A, A}, 1, {c, b, a}] === dressing[GammaN, {A, A, A}, 1, {a, b, c}]
    ],
    True,
    TestID -> "FSetSymmetricDressing: gluon full symmetrization reorders arguments"
]];

(**********************************************************************************
    FSetSymmetricDressing: partial index symmetrization
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Module[{},
        FSetSymmetricDressing[GammaN, {A, cb, c}, {1, 3}];
        dressing[GammaN, {A, cb, c}, 1, {z, b, a}] === dressing[GammaN, {A, cb, c}, 1, {a, b, z}]
    ],
    True,
    TestID -> "FSetSymmetricDressing: partial index symmetrization reorders specified positions"
]];
