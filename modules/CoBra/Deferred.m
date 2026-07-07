(**********************************************************************************
    Deferred.m -- The FDeferred handle for lazy C++ pipeline evaluation and
    its dispatch into FTruncate / FSimplify / FEvaluate.

    Public API:
      FDeferred                  -- Inert handle head
      FDeferredQ                 -- Predicate
      FEvaluate                  -- Force a deferred computation

    Internal:
      CppDeferTakeDerivatives    -- Build a handle (called from FTakeDerivatives)
      CppResolveDerivatives      -- Eager C++ path for FResolveDerivatives
      CppRunPipeline             -- The fused C++ call driver

    A handle captures setup, expression, derivative list and symmetries at
    deferral time, so later FSetGlobalSetup changes cannot leak in. Forcing it
    runs ONE fused C++ call: derivatives (+ truncation via in_deriv_trunc,
    + simplification) in a single external process.
**********************************************************************************)

FunKit::cppNotBuilt = "The C++ backend has not been built in this session. Call FSetBackendCpp[] first.";

FDeferredQ[_FDeferred] :=
    True;

FDeferredQ[_] :=
    False;

(*Key access: FDeferred[data]["Setup"] etc.*)

FDeferred[data_Association][key_String] :=
    data[key];

(**********************************************************************************
    Handle construction and modification
**********************************************************************************)

CppDeferTakeDerivatives[setup_, expr_, derivList_, optSyms_] :=
    Module[{terms, annotations, syms, ex, handle},
        If[!CppBackendReadyQ[],
            Message[FunKit::cppNotBuilt];
            Abort[];
        ];
        {terms, annotations} = SeparateFExAnnotations[expr];
        syms = FMergeSymmetries[optSyms, Lookup[annotations, "Symmetries", {}]];
        ex = FixIndices[setup, FEx @@ terms];
        handle =
            FDeferred[<|
                "Setup" -> setup,
                "Expression" -> ex,
                "DerivativeList" -> derivList,
                "Symmetries" -> syms,
                "AutoSymmetries" -> TrueQ[$AutoBuildSymmetryList] && syms === {},
                "Created" -> Now
            |>];
        (*Fail fast: a dry serialization runs the full eligibility gate now,
          not when the user finally forces the handle*)
        CppSerializeInput[setup, ex, derivList, syms, <|"Truncate" -> False, "Simplify" -> False, "EmitDerivatives" -> False|>];
        handle
    ];

ReSetup[FDeferred[data_Association], setup_] :=
    (
        AssertFSetup[setup];
        FDeferred[<|data, "Setup" -> setup|>]
    );

AddDeferredSymmetries[d_FDeferred, {}] :=
    d;

AddDeferredSymmetries[FDeferred[data_Association], syms_List] :=
    FDeferred[<|data, "Symmetries" -> FMergeSymmetries[data["Symmetries"], syms], "AutoSymmetries" -> False|>];

(**********************************************************************************
    The fused-call driver
**********************************************************************************)

CppRunPipeline[d_FDeferred, truncate_, simplify_] :=
    Module[{data = First[d], cppSimplify, input, map, openSyms, openInverse, result},
        If[!CppBackendReadyQ[],
            Message[FunKit::cppNotBuilt];
            Abort[];
        ];
        (*The engine's simplify requires a fully truncated equation (derivative
          resolution generically introduces AnyField legs), so it only runs in
          the same call as truncation; untruncated results are simplified by
          the native FSimplify below instead*)
        cppSimplify = TrueQ[simplify] && TrueQ[truncate];
        {input, map} =
            CppSerializeInput[
                data["Setup"], data["Expression"], data["DerivativeList"], data["Symmetries"],
                <|"Truncate" -> truncate, "Simplify" -> cppSimplify, "EmitDerivatives" -> TrueQ[data["AutoSymmetries"]]|>
            ];
        (*Open legs keep their symbols through the round trip; everything else
          gets fresh names. Open = odd occurrence count within a term of the
          serialized equation, which covers derivative legs, embedded FDOps
          and bare fields alike*)
        openSyms =
            DeleteDuplicates @ Flatten @ Map[
                Function[term,
                    Module[{labels},
                        (*sign objects (FMinus/SymmFactor) reference indices of other
                          objects without closing them -- skip their legs*)
                        labels = Abs[#[[2]]]& /@ Flatten[Cases[term, o_Association /; KeyExistsQ[o, "legs"] && !MemberQ[{"FMinus", "SymmFactor"}, o["type"]] :> o["legs"]], 1];
                        Select[Tally[labels], OddQ[#[[2]]]&][[All, 1]]
                    ]
                ]
                ,
                input["equation"]
            ];
        openInverse = KeyTake[Association[Reverse /@ Normal[map]], openSyms];
        result = CppExecute[data["Setup"], input, openInverse];
        (*The truncation rule Field -> {{}} (drop all bare fields) has no
          engine counterpart -- emulate it here. Derivatives never create bare
          fields, so post-filtering is equivalent to dropping during the pass*)
        If[TrueQ[truncate] && Lookup[Lookup[data["Setup"], "Truncation", <||>], Field] === {{}},
            result =
                FEx @@ Select[
                    List @@ result
                    ,
                    Function[term, !AnyTrue[List @@ term, couldBeField[#] && MemberQ[GetAllFields[data["Setup"]], Head[#]]&]]
                ]
        ];
        result = CppAttachSymmetries[data, result];
        If[TrueQ[simplify] && !cppSimplify,
            result = FunKit`FSimplify[data["Setup"], result]
        ];
        result
    ];

(**********************************************************************************
    Eager C++ path for FResolveDerivatives (used e.g. inside FMakeDSE): the
    equation's embedded FDOps are serialized as-is and resolved in one run.
**********************************************************************************)

CppResolveDerivatives[setup_, eq_FEx, optSyms_] :=
    Module[{terms, annotations, syms, d},
        {terms, annotations} = SeparateFExAnnotations[eq];
        syms = FMergeSymmetries[optSyms, Lookup[annotations, "Symmetries", {}]];
        d =
            FDeferred[<|
                "Setup" -> setup,
                "Expression" -> FixIndices[setup, FEx @@ terms],
                "DerivativeList" -> {},
                "Symmetries" -> syms,
                "AutoSymmetries" -> False,
                "Created" -> Now
            |>];
        CppRunPipeline[d, False, TrueQ[$AutoSimplify]]
    ];

(**********************************************************************************
    Consumers: FTruncate / FSimplify / FEvaluate / chained FTakeDerivatives.
    The _FDeferred patterns are more specific than the existing catch-alls in
    Truncation.m / Simplify.m / Global.m, so no edits are needed there.
**********************************************************************************)

FTruncate[setup_, d_FDeferred] :=
    CppRunPipeline[ReSetup[d, setup], True, TrueQ[$AutoSimplify]];

FTruncate[d_FDeferred] :=
    CppRunPipeline[d, True, TrueQ[$AutoSimplify]];

FSimplify[setup_, d_FDeferred, OptionsPattern[FSimplify]] :=
    CppRunPipeline[AddDeferredSymmetries[ReSetup[d, setup], OptionValue[FSimplify, "Symmetries"]], False, True];

FSimplify[d_FDeferred, OptionsPattern[FSimplify]] :=
    CppRunPipeline[AddDeferredSymmetries[d, OptionValue[FSimplify, "Symmetries"]], False, True];

FResolveDerivatives[setup_, d_FDeferred, OptionsPattern[FResolveDerivatives]] :=
    CppRunPipeline[AddDeferredSymmetries[ReSetup[d, setup], OptionValue[FResolveDerivatives, "Symmetries"]], False, TrueQ[$AutoSimplify]];

Options[FEvaluate] = {"Truncate" -> False, "Simplify" -> Automatic};

FEvaluate[d_FDeferred, OptionsPattern[]] :=
    CppRunPipeline[d, TrueQ[OptionValue["Truncate"]], OptionValue["Simplify"] /. Automatic :> TrueQ[$AutoSimplify]];

FEvaluate[setup_, d_FDeferred, opts : OptionsPattern[]] :=
    FEvaluate[ReSetup[d, setup], opts];

FEvaluate[a___] :=
    (
        Message[FunKit::invalidArguments, FEvaluate];
        Abort[]
    );

(*Chaining derivatives is cheap: just extend the handle*)

FTakeDerivatives[setup_, d_FDeferred, derivList_List, OptionsPattern[FTakeDerivatives]] :=
    Module[{data = First[ReSetup[d, setup]], optSyms = OptionValue[FTakeDerivatives, "Symmetries"]},
        FDeferred[<|
            data,
            "DerivativeList" -> Join[data["DerivativeList"], derivList],
            "Symmetries" -> FMergeSymmetries[data["Symmetries"], optSyms],
            "AutoSymmetries" -> TrueQ[data["AutoSymmetries"]] && optSyms === {} && TrueQ[$AutoBuildSymmetryList]
        |>]
    ];

FTakeDerivatives[d_FDeferred, derivList_List, opts : OptionsPattern[FTakeDerivatives]] :=
    Module[{data = First[d]},
        FTakeDerivatives[data["Setup"], d, derivList, opts]
    ];

(**********************************************************************************
    Display
**********************************************************************************)

FDeferred /: MakeBoxes[fd : FDeferred[data_Association], form : StandardForm | TraditionalForm] :=
    BoxForm`ArrangeSummaryBox[
        FDeferred, fd, None,
        {
            BoxForm`SummaryItem[{"input terms: ", Length[DropFExAnnotations[data["Expression"]]]}],
            BoxForm`SummaryItem[{"derivatives: ", Row[data["DerivativeList"], ","]}]
        },
        {
            BoxForm`SummaryItem[{"backend: ", "C++ (" <> If[CppBackendReadyQ[], "ready", "NOT BUILT"] <> ")"}],
            BoxForm`SummaryItem[{"created: ", data["Created"]}]
        },
        form
    ];

Format[FDeferred[data_Association], OutputForm] :=
    "FDeferred[<" <> ToString[Length[data["DerivativeList"]]] <> " derivatives on " <>
    ToString[Length[DropFExAnnotations[data["Expression"]]]] <> " terms, C++ backend>]";

(**********************************************************************************
    Traps: incompatible consumers give an actionable error instead of a
    pattern-mismatch abort
**********************************************************************************)

FDeferred::force = "`1` cannot operate on a deferred C++ computation. Call FEvaluate[expr] (or FTruncate/FSimplify) first to obtain a concrete FEx.";

Scan[
    Function[f,
        f[___, _FDeferred, ___] :=
            (
                Message[FDeferred::force, f];
                Abort[]
            )
    ]
    ,
    {QMeSForm, DoFunForm, FunKitForm, FTruncateOpenIndices, FExpand, DExpand, FOrderFields, FResolveFDOp}
];

(*Modules loaded after CoBra's dependencies (DiANE, AnSEL routing) -- trap only
  when the symbol actually exists*)

Scan[
    Function[name,
        If[NameQ[name],
            With[{f = Symbol[name]},
                f[___, _FDeferred, ___] :=
                    (
                        Message[FDeferred::force, f];
                        Abort[]
                    )
            ]
        ]
    ]
    ,
    {"FunKit`FPrint", "FunKit`FPlot", "FunKit`FRoute", "FunKit`FUnroute"}
];

FDeferred /: NonCommutativeMultiply[___, _FDeferred, ___] :=
    (
        Message[FDeferred::force, NonCommutativeMultiply];
        Abort[]
    );

FDeferred /: Times[___, _FDeferred, ___] :=
    (
        Message[FDeferred::force, Times];
        Abort[]
    );

FDeferred /: Plus[___, _FDeferred, ___] :=
    (
        Message[FDeferred::force, Plus];
        Abort[]
    );
