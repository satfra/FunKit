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
    FMakeDiagrammaticRules: fermionic propagator inversion (qbq)

    TensorBases 1.3.0 changed TBMakePropagator to expand the inverse propagator
    with all momenta incoming, i.e. its tensors sit at +p rather than at -p.
    Every MOMENTUM-ODD dressing flipped sign as a result. FMakeDiagrammaticRules
    contracts the returned dressings with TBGetVertex at +propMom, so it has
    always assumed the new convention; against an older TensorBases the quark
    propagator comes out with the wrong sign on its pslash dressing.

    The tests above cannot see that. Every propagator they invert -- AA, cbc --
    is momentum-EVEN, hence bit-identical under either routing. "qbq" is the
    smallest basis with an odd element:

        T_1 = d_col d_flav I (gamma.p)      (odd)
        T_2 = d_col d_flav d_Dirac          (even)

    For an inverse propagator a_1 (I pslash) + a_2 the inverse is exactly

        (a_2 - I a_1 pslash) / D ,    D = a_2^2 + a_1^2 p^2 ,

    so the odd coefficient must carry the sign OPPOSITE to a_1 while the even
    one carries the sign of a_2. That relative sign is the whole content of the
    change, and it is pinned below as the bilinear

        c_odd a_2 + c_even a_1 == 0 .

    Asserting the bilinear rather than literal coefficients keeps the test
    independent of the overall prefactor FunKit puts in front of the basis
    expansion (CommuteSign[setup,q,qb] = -1 for two Grassmann legs, times the
    GetOrder signs) and of whatever form the common denominator takes. Against
    the old routing the same combination is -2 a_1 a_2 / D, i.e. manifestly
    nonzero -- verified by running the generation through a shim that reproduces
    the old behaviour.

    Requires FORM: TBRestrictBasis and TBMakePropagator both trace. So do the AA
    tests above, so this adds no dependency. It builds one restricted basis,
    TBCache/qbq_restrict_1_2; TBCache/ is gitignored.
**********************************************************************************)

(* Generated once -- every propagator rule costs a FORM round trip, and
   TBMakePropagator results are never cached. CheckAbort so that a broken quark
   path fails these three tests instead of taking down the rest of the file. *)

quarkPropData =
    CheckAbort[
        Module[{setup, rules, marked, applied, tMark, pq, dq1, cq1, fq1, dq2, cq2, fq2},
            setup =
                Append[
                    GetFunKitSetupQCD[]
                    ,
                    "FeynmanRules" -> <|Propagator -> {{qb, q} -> "qbq"}|>
                ];
            FunKit`FSetNotationA[];
            rules = FunKit`FMakeDiagrammaticRules[setup];
            (*Swap the tensor structures for inert markers BEFORE applying the rule.
              ReplaceAll works structurally, so the RuleDelayed's right-hand side is
              not evaluated here -- the same property the note at the top of this file
              relies on. Comparing against separately computed TBGetVertex output would
              not work: TBEvaluateBasisElement renames the closed Lorentz index on
              every call, so the dummies would not cancel.*)
            marked = rules /. TensorBases`TBGetVertex[_, n_Integer, _] :> tMark[n];
            (*GetOrder reverses the leg order for propagators, so the rule is stated for
              {q,qb} and it is leg 1 that carries propMom. Applying the rule is what
              binds the leg indices and makes the momenta resolve.*)
            applied = Propagator[{q, qb}, {{pq, {dq1, cq1, fq1}}, {-pq, {dq2, cq2, fq2}}}] /. marked;
            <|
                "Rules" -> rules,
                "Odd" -> Coefficient[applied, tMark[1]],
                "Even" -> Coefficient[applied, tMark[2]],
                "Dressings" ->
                    SortBy[
                        DeleteDuplicates @ Cases[applied, dressing[InverseProp, __], Infinity]
                        ,
                        #[[3]]&
                    ]
            |>
        ]
        ,
        $Failed
    ];

AppendTo[tests, VerificationTest[
    quarkPropData =!= $Failed
        && Length[quarkPropData["Rules"]] === 1
        && Not @ FreeQ[Hold @@ {quarkPropData["Rules"][[1]]}, InverseProp]
        (*The tensors come from the ORIGINAL basis (FMakeDiagrammaticRules.m:77 uses
          rule, not newBasisName) -- the restricted basis only carries the inversion,
          whose result is a vector of scalar coefficients, so its name never appears.*)
        && Not @ FreeQ[Hold @@ {quarkPropData["Rules"][[1]]}, "qbq"]
    ,
    True
    ,
    TestID -> "FMakeDiagrammaticRules: qbq propagator rule is derived from the qbq basis"
]];

(* Guards the assertion below against passing degenerately on a dropped term. *)

AppendTo[tests, VerificationTest[
    quarkPropData =!= $Failed
        && Length[quarkPropData["Dressings"]] === 2
        && Simplify[quarkPropData["Odd"]] =!= 0
        && Simplify[quarkPropData["Even"]] =!= 0
    ,
    True
    ,
    TestID -> "FMakeDiagrammaticRules: qbq propagator expands in both basis elements"
]];

(* The regression. Red against TensorBases <= 1.2.1. *)

AppendTo[tests, VerificationTest[
    Simplify[
        quarkPropData["Odd"] * quarkPropData["Dressings"][[2]]
            + quarkPropData["Even"] * quarkPropData["Dressings"][[1]]
    ] === 0
    ,
    True
    ,
    TestID -> "FMakeDiagrammaticRules: qbq pslash dressing carries the sign opposite to the mass dressing"
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
