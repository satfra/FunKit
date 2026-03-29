(**********************************************************************************
    FMakeDiagrammaticRules.m -- Generates diagrammatic Feynman rules from setup

    Public API:
      FMakeDiagrammaticRules     -- Converts Feynman rules in setup to diagrammatic
                                    replacement rules with tensor bases and dressings
**********************************************************************************)

Options[FMakeDiagrammaticRules] = {"DerivePropagators" -> True};

FMakeDiagrammaticRules::noFeynmanRules = "The setup does not contain a \"FeynmanRules\" key.";

FMakeDiagrammaticRules[setup_, OptionsPattern[]] :=
    Module[{ruleList, truncationList, idx, jdx, kdx, minusRule, object, fieldContent, rule, dress, minusOrig, minusBasis, subset = All, orderOrig, orderBasis, newBasisName, propMom, annotations, indSyms},
        AssertFSetup[setup];
        If[Not @ KeyExistsQ[setup, "FeynmanRules"],
            Message[FMakeDiagrammaticRules::noFeynmanRules];
            Abort[]
        ];
        ruleList = {};
        truncationList = Normal[setup["FeynmanRules"]];
        For[idx = 1, idx <= Length[truncationList], idx++,
            object = truncationList[[idx, 1]];
            FunKitDebug[1, "Creating diagrammatic rule for ", object];
            For[jdx = 1, jdx <= Length[truncationList[[idx, 2]]], jdx++,
                rule = Values[truncationList[[idx, 2, jdx]]];
                FunKitDebug[1, "  Creating diagrammatic rule for ", rule];
                (*Check what the subset of the original basis is*)
                If[Head[rule] === List,
                    annotations = Select[rule, Head[#] === Rule&];
                    rule = Select[rule, Head[#] =!= Rule&];
                    If[Length[rule] < 2,
                        subset = Range[TensorBases`TBGetBasisSize[makePosIdx[rule[[1]]]]];
                        ,
                        subset = Flatten[{rule[[2 ;; ]]}];
                    ];
                    rule = rule[[1]];
                    ,
                    annotations = {};
                    subset = Range[TensorBases`TBGetBasisSize[makePosIdx[rule]]];
                ];
                minusRule =
                    If[isNeg[rule],
                        -1
                        ,
                        1
                    ];
                rule *= minusRule;
                {minusOrig, orderOrig} =
                    GetOrder[
                        setup
                        ,
                        Keys @ truncationList[[idx, 2, jdx]]
                        ,
                        If[object === Propagator,
                            True
                            ,
                            False
                        ]
                    ];
                fieldContent = (Keys @ truncationList[[idx, 2, jdx]])[[orderOrig]];
                {minusBasis, orderBasis} = GetOrder[setup, fieldContent, TensorBases`TBGetBasisFields[rule] /. annotations];
                dress =
                    If[OptionValue["DerivePropagators"] && object === Propagator,
                        FunKitDebug[2, "    Creating propagator rule"];
                        newBasisName = rule <> "_restrict_" <> StringReplace[ToString[subset], {" " -> "", "," -> "_", "{" -> "", "}" -> ""}];
                        FunKitDebug[2, "      Creating restricted basis for propagator inversion ", newBasisName, " with rule ", rule, ", subset ", subset];
                        If[Not @ TensorBases`TBBasisExists[newBasisName],
                            TensorBases`TBRestrictBasis[rule, newBasisName, subset];
                        ];
                        orderBasis = Reverse @ orderBasis;
                        ((CommuteSign[setup, ##]& @@ fieldContent) * TensorBases`TBMakePropagator[newBasisName, Table[dressing[InverseProp, Reverse @ fieldContent, subset[[kdx]], $mom], {kdx, 1, Length[subset]}], propMom])
                        ,
                        FunKitDebug[2, "Creating nPoint rule"];
                        (Table[dressing[object, fieldContent, subset[[kdx]], $mom], {kdx, 1, Length[subset]}])
                    ];
                rule = minusOrig * minusRule * minusBasis * dress . (Table[$tens[rule, subset[[kdx]], $ind], {kdx, 1, Length[subset]}]);
                indSyms = Table[Unique["idx"], {Length[fieldContent]}];
                With[{
                    lhs = makeObj[object, fieldContent, Pattern[#, Blank[]] & /@ indSyms],
                    indList = indSyms
                },
                    AppendTo[ruleList,
                        lhs :> (Evaluate @ rule)
                            /. $tens -> TensorBases`TBGetVertex
                            /. $mom :> indList[[All, 1]]
                            /. {$ind :> Flatten /@ indList[[$order]], propMom :> indList[[$order[[1]], 1]]}
                            /. $order -> (Evaluate @ orderBasis)
                    ];
                ];
            ];
        ];
        Return[ruleList];
    ];

FMakeDiagrammaticRules[___] :=
    (
        Message[FunKit::invalidArguments, FMakeDiagrammaticRules];
        Abort[]
    );
