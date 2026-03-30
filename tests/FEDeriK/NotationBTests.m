tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    NotationB: Tests verifying NotationB-specific correctness
**********************************************************************************)

(* Yang-Mills setup for NotationB tests *)

GetYangMillsSetup[] :=
    Module[{p, v, c},
        <|
            "FieldSpace" -> <|
                "Commuting" -> {A[p, {v, c}]},
                "Grassmann" -> {{cb[p, {c}], c[p, {c}]}}
            |>,
            "Truncation" -> <|
                GammaN -> {
                    {A, A},
                    {A, A, A},
                    {A, A, A, A},
                    {A, cb, c},
                    {cb, c}
                },
                Propagator -> {
                    {A, A},
                    {cb, c}
                },
                Rdot -> {
                    {A, A},
                    {cb, c}
                },
                S -> {
                    {A, A},
                    {A, A, A},
                    {A, A, A, A},
                    {cb, c},
                    {cb, c, A}
                },
                Field -> {{}}
            |>
        |>
    ];

(* ---- Test 1: FMakeClassicalAction uses makeObj for S-vertices ---- *)

FunKit`FSetNotationB[];

Module[{setup, classAct, sVertex, fields},
    setup = GetFunKitSetupScalar[];
    setup = Append[setup, "Truncation" -> Append[setup["Truncation"],
        S -> {{Phi, Phi}, {Phi, Phi, Phi}}
    ]];
    FunKit`FSetGlobalSetup[setup];
    classAct = FunKit`FMakeClassicalAction[setup];
    sVertex = Cases[classAct, _S, {2}][[1]];
    fields = FunKit`Private`getFields[sVertex];
    (* In NotationB, getFields on S-vertices should return field symbols, not {List, List} *)
    AppendTo[tests, TestCreate[
        And @@ (MatchQ[#, _Symbol]& /@ fields),
        True,
        TestID -> "NotationB: FMakeClassicalAction S-vertex fields are symbols"
    ]];
];

(* Disable auto-simplify for NotationB DSE tests — FSimplify has separate NotationB issues *)
FunKit`Private`$AutoSimplify = False;

(* ---- Test 2: Scalar DSE in NotationB ---- *)

Module[{setup, dse, derived},
    setup = GetFunKitSetupScalar[];
    setup = Append[setup, "Truncation" -> Append[setup["Truncation"],
        S -> {{Phi, Phi}, {Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}
    ]];
    FunKit`FSetGlobalSetup[setup];
    dse = FunKit`FMakeDSE[Phi[i1]];
    AppendTo[tests, TestCreate[
        Head[dse],
        FEx,
        TestID -> "NotationB: Scalar FMakeDSE produces FEx"
    ]];
    derived = FunKit`FTakeDerivatives[dse, {Phi[i2]}];
    AppendTo[tests, TestCreate[
        Head[derived],
        FEx,
        TestID -> "NotationB: Scalar FTakeDerivatives produces FEx"
    ]];
];

(* ---- Test 3: Yang-Mills gluon DSE (FMakeDSE + FTakeDerivatives) ---- *)

Module[{setup, dse, derived},
    setup = GetYangMillsSetup[];
    FunKit`FSetGlobalSetup[setup];
    dse = FunKit`FMakeDSE[A[i1]];
    AppendTo[tests, TestCreate[
        Head[dse],
        FEx,
        TestID -> "NotationB: Yang-Mills FMakeDSE produces FEx"
    ]];
    derived = FunKit`FTakeDerivatives[dse, {A[i2]}];
    AppendTo[tests, TestCreate[
        Head[derived],
        FEx,
        TestID -> "NotationB: Yang-Mills FTakeDerivatives produces FEx"
    ]];
];

(* Restore default notation *)
FunKit`FSetNotationA[];
