(**********************************************************************************
    Classical Action
**********************************************************************************)

MakeClassicalAction::noTruncation = "The given setup does not have a truncation for S!";

MakeClassicalAction[setup_] :=
    Module[{indices, i, prefac},
        AssertFSetup[setup];
        If[FreeQ[Keys[setup["Truncation"]], S],
            Message[MakeClassicalAction::noTruncation];
            Abort[]
        ];
        FEx @@
            Map[
                (
                    prefac = Split[#];
                    prefac = Times @@ (1 / ((Length[#]& /@ prefac)!));
                    indices = Map[Unique["i"]&, #];
                    FTerm[prefac, S[#, -indices]] ** (FTerm @@ Table[Construct[#[[i]], indices[[i]]], {i, 1, Length[#]}])
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
        FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]]
    ];

(**********************************************************************************
    Generalized Flow Equations
**********************************************************************************)

AddCorrelationFunction[Phidot];

SetUnorderedIndices[Phidot, 1];

GeneralizedFlowEquation :=
    Module[{a, b, c},
        a = Symbol @ SymbolName @ Unique["a"];
        b = Symbol @ SymbolName @ Unique["b"];
        c = Symbol @ SymbolName @ Unique["c"];
        FEx[FTerm[-1, Phidot[{AnyField}, {a}], GammaN[{AnyField}, {-a}]], FTerm[1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]], FTerm[Propagator[{AnyField, AnyField}, {a, c}], Phidot[{AnyField, AnyField}, {-c, b}], R[{AnyField, AnyField}, {-a, -b}]]]
    ];

(* ::Input::Initialization:: *)

RGInvGeneralizedFlowEquation :=
    Module[{a, b, c},
        a = Symbol @ SymbolName @ Unique["a"];
        b = Symbol @ SymbolName @ Unique["b"];
        c = Symbol @ SymbolName @ Unique["c"];
        FEx[FTerm[-1, Phidot[{AnyField}, {a}], GammaN[{AnyField}, {-a}]], FTerm[1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]]
    ];

(**********************************************************************************
    DSEs
**********************************************************************************)

MakeDSE[setup_, field_] :=
    Module[{classAct, dS},
        AssertFSetup[setup];
        AssertDerivativeList[setup, {field}];
        (*Make a classical action*)
        classAct = MakeClassicalAction[setup];
        (*Take one derivative with "field" classical action*)
        dS =
            FResolveDerivatives[setup, FTerm[FDOp[field]] ** classAct] //
            ReduceIndices[setup, #]& //
            ReduceIndices[setup, #]&;
        (*Separate powers out into factors in the FTerm. Need this to insert FDOp in the next step*)
        dS = dS //. Times[pre___, f1_[id1_], post___] :> NonCommutativeMultiply[pre, f1[id1], post];
        (*Insert \[Phi]^a->\[CapitalPhi]^a+G^ab\[Delta]/\[Delta]\[CapitalPhi]^b *)
        dS =
            dS /.
                (
                    Map[
                        #[id_] :>
                            Module[{i},
                                i = Symbol @ SymbolName @ Unique["i"];
                                FEx[FTerm[#[id]], FTerm[Propagator[{#, AnyField}, {id, i}], FDOp[AnyField[i]]]]
                            ]&
                        ,
                        GetAllFields[setup]
                    ]
                );
        dS // FResolveDerivatives[setup, #]&
    ];
