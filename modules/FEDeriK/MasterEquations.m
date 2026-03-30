(**********************************************************************************
    MasterEquations.m -- Master equation definitions (Wetterich, DSE, generalized)

    Public API:
      FMakeClassicalAction        -- Constructs classical action from setup truncation
      WetterichEquation          -- Returns the Wetterich flow equation
      GeneralizedFlowEquation    -- Returns the generalized flow equation with Phidot
      RGInvGeneralizedFlowEquation -- RG-invariant version of the generalized flow eq.
      FMakeDSE                   -- Derives Dyson-Schwinger equation for a given field
**********************************************************************************)

(**********************************************************************************
    Classical Action
**********************************************************************************)

FMakeClassicalAction::noTruncation = "The given setup does not have a truncation for S!";

FMakeClassicalAction[setup_] :=
    Module[{indices, i, prefac},
        AssertFSetup[setup];
        If[FreeQ[Keys[setup["Truncation"]], S],
            Message[FMakeClassicalAction::noTruncation];
            Abort[]
        ];
        FEx @@
            Map[
                (
                    prefac = Split[#];
                    prefac = Times @@ (1 / ((Length[#]& /@ prefac)!));
                    indices = Map[Unique["i"]&, #];
                    FTerm[prefac, makeObj[S, #, -indices]] ** (FTerm @@ Table[Construct[#[[i]], indices[[i]]], {i, 1, Length[#]}])
                )&
                ,
                OrderFieldList[setup, #]& /@ setup["Truncation"][S]
            ]
    ];

(**********************************************************************************
    Wetterich Equation
**********************************************************************************)

WetterichEquation :=
    Module[{a, b},
        a = Symbol @ SymbolName @ Unique["a"];
        b = Symbol @ SymbolName @ Unique["b"];
        FEx[FTerm[1/2, makeObj[Propagator, {AnyField, AnyField}, {a, b}], makeObj[Rdot, {AnyField, AnyField}, {-a, -b}]]]
    ];

(**********************************************************************************
    Generalized Flow Equations
**********************************************************************************)

FAddCorrelationFunction[Phidot];

FSetUnorderedIndices[Phidot, 1];

FSetTexStyles[Phidot -> "\\dot{\\Phi}"];

GeneralizedFlowEquation :=
    Module[{a, b, c},
        a = Symbol @ SymbolName @ Unique["a"];
        b = Symbol @ SymbolName @ Unique["b"];
        c = Symbol @ SymbolName @ Unique["c"];
        FEx[FTerm[-1, makeObj[Phidot, {AnyField}, {a}], makeObj[GammaN, {AnyField}, {-a}]], FTerm[1/2, makeObj[Propagator, {AnyField, AnyField}, {a, b}], makeObj[Rdot, {AnyField, AnyField}, {-a, -b}]], FTerm[makeObj[Propagator, {AnyField, AnyField}, {a, c}], makeObj[Phidot, {AnyField, AnyField}, {-c, b}], makeObj[R, {AnyField, AnyField}, {-a, -b}]]]
    ];

(* ::Input::Initialization:: *)

RGInvGeneralizedFlowEquation :=
    Module[{a, b, c},
        a = Symbol @ SymbolName @ Unique["a"];
        b = Symbol @ SymbolName @ Unique["b"];
        c = Symbol @ SymbolName @ Unique["c"];
        FEx[FTerm[-1, makeObj[Phidot, {AnyField}, {a}], makeObj[GammaN, {AnyField}, {-a}]], FTerm[1/2, makeObj[Propagator, {AnyField, AnyField}, {a, b}], makeObj[Rdot, {AnyField, AnyField}, {-a, -b}]]]
    ];

(**********************************************************************************
    DSEs
**********************************************************************************)

FMakeDSE[setup_, field_] :=
    Module[{classAct, dS},
        AssertFSetup[setup];
        AssertDerivativeList[setup, {field}];
        (*Make a classical action*)
        classAct = FMakeClassicalAction[setup];
        (*Take one derivative with "field" classical action*)
        dS =
            FResolveDerivatives[setup, FTerm[FDOp[field]] ** classAct] //
            ReduceIndices[setup, #]& //
            ReduceIndices[setup, #]&;
        (*Separate powers out into factors in the FTerm. Need this to insert FDOp in the next step*)
        dS = dS //. Times[pre___, f1_[id1_], post___] :> NonCommutativeMultiply[pre, f1[id1], post];
        (*Insert \[Phi]^a->\[CapitalPhi]^a+G^ab\[Delta]/\[Delta]\[CapitalPhi]^b
          Use Replace at level {2} (FTerm arguments) to avoid replacing field[index]
          patterns inside indexed objects in NotationB.*)
        dS =
            Replace[dS,
                Map[
                    #[id_] :>
                        Module[{i},
                            i = Symbol @ SymbolName @ Unique["i"];
                            FEx[FTerm[#[id]], FTerm[makeObj[Propagator, {#, AnyField}, {id, i}], FDOp[AnyField[i]]]]
                        ]&
                    ,
                    GetAllFields[setup]
                ],
                {2}
            ];
        dS //
        FResolveDerivatives[setup, #]& //
        If[ModuleLoaded[AnSEL] && $AutoSimplify === True,
            FunKit`FSimplify[setup, #],
            #
        ]&
    ];
