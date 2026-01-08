Options[FMakeDiagrammaticRules] = {"DerivePropagators" -> True};

FMakeDiagrammaticRules[setup_, OptionsPattern[]] :=
    Module[{ruleList, truncationList, idx, jdx, kdx, minusRule, object, fieldContent, rule, dress, minusOrig, minusBasis, subset = All, orderOrig, orderBasis, newBasisName, propMom},
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
                    subset =
                        If[Head[rule[[2]]] === List,
                            rule[[2]]
                            ,
                            {rule[[2]]}
                        ];
                    rule = rule[[1]]
                    ,
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
                {minusBasis, orderBasis} = GetOrder[setup, fieldContent, TensorBases`TBGetBasisFields[rule]];
                dress =
                    If[OptionValue["DerivePropagators"] && object === Propagator,
                        FunKitDebug[2, "    Creating propagator rule"];
                        newBasisName = rule <> "_restrict_" <> StringReplace[ToString[subset], {" " -> "", "," -> "_", "{" -> "", "}" -> ""}];
                        FunKitDebug[2, "      Creating restricted basis for propagator inversion ", newBasisName, " with rule ", rule, ", subset ", subset];
                        If[Not @ TensorBases`TBBasisExists[newBasisName],
                            TensorBases`TBRestrictBasis[rule, newBasisName, subset]
                        ];
                        orderBasis = Reverse @ orderBasis;
                        ((CommuteSign[setup, ##]& @@ fieldContent) * TensorBases`TBMakePropagator[newBasisName, Table[dressing[InverseProp, Reverse @ fieldContent, subset[[kdx]], $mom], {kdx, 1, Length[subset]}], propMom])
                        ,
                        FunKitDebug[2, "Creating nPoint rule"];
                        (Table[dressing[object, fieldContent, subset[[kdx]], $mom], {kdx, 1, Length[subset]}])
                    ];
                rule = minusOrig * minusRule * minusBasis * dress . (Table[$tens[rule, subset[[kdx]], $ind], {kdx, 1, Length[subset]}]);
                AppendTo[ruleList, OrderObject[setup, object[fieldContent, ind : (_List)]] :> (Evaluate @ rule) /. $tens -> TensorBases`TBGetVertex /. $mom :> ind$[[All, 1]] /. {$ind :> Flatten /@ ind$[[$order]], propMom :> ind$[[$order[[1]], 1]]} /. $order -> (Evaluate @ orderBasis)];
            ];
        ];
        Return[ruleList];
    ];
