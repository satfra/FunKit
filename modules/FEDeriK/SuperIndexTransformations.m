(* ::Input::Initialization:: *)

GetSuperIndexTermTransformations::momentumConservation = "Momentum conservation for the momentum `1` has been violated.";

GetSuperIndexTermTransformations::multiContraction = "The indices `1` have been contracted with more than one other object.";

Unprotect[$momentum, $groupIndex];

$momentum[-i_] :=
    -$momentum[i];

Protect[$momentum, $groupIndex];

GetSuperIndexTermTransformations::usage = "Returns a set {fw,bw}, where fw is a transformation from explicit to super indices, and bw the backwards transformation.
fw is a list of three lists. The fw[[1]] is a list that transforms the explicit index lists as they occur in the given expression, fw[[2]] transforms the momenta, fw[[3]] transforms the group indices. bw has the same structure.";

GetSuperIndexTermTransformationsSingleFTerm[setup_, term_FTerm] :=
    Module[
        {doFields, undoFields, kdx, allObj, idx, jdx, newObj, indexPosToChange, indicesToChange, newSuperIndices, repl, replForward, replBackward}
        ,
        (*Get all objects and bring them in standard form*)
        doFields = replFields[setup];
        undoFields = unreplFields[setup];
        allObj = ExtractObjectsWithIndex[setup, term] /. doFields /. a_[f_, i_] /; MemberQ[$allObjects, a] :> a[Transpose[{f, i}]];
        (*We find all positions where indices are given explicitly*)
        indexPosToChange = Map[Join[Position[#[[1]], {_Symbol | Times[-1, _Symbol], _List}, {1}], Position[#[[1]], {_Symbol | Times[-1, _Symbol]}, {1}]]&, allObj];
        indexPosToChange = Map[Flatten, indexPosToChange];
        If[Length[indexPosToChange] === 0,
            Return[{{{}, {}, {}}, {{}, {}, {}}}]
        ];
        (*Next, we isolate the group indices and try to group according to these. If no group indices are present, we try to group by momenta.*)
        indicesToChange = Flatten[Table[allObj[[idx, 1, indexPosToChange[[idx]]]], {idx, 1, Length[allObj]}], 1];
        indexPosToChange =
            PositionIndex[
                Join[
                        (*group indices:*)Select[indicesToChange, Length[#[[2]]] === 2&][[All, 2, 2]]
                        ,
                        (*momenta:*)
                        Abs[Select[indicesToChange, Length[#[[2]]] === 1&][[All, 2]]]
                    ] /. Abs[a_] :> a
            ];
        (*We assign each unique index group a new superindex*)
        newSuperIndices = Map[Unique["i"]&, indexPosToChange];
        repl = AssociationMap[indicesToChange[[indexPosToChange[Keys[#]]]] -> Values[#]&, newSuperIndices];
        (*This is the resulting full replacement:*)
        repl =
            Flatten @
                KeyValueMap[
                    (*unpaired case*)If[Length[#1] === 1,
                        #1[[1]] -> {#1[[1, 1]], #2}
                        ,
                        (*We have a pair*)
                        If[Length[#1] === 2,
                            If[(-#1[[1, 2, 1]]) =!= #1[[2, 2, 1]],
                                Message[GetSuperIndexTermTransformations::momentumConservation, #1[[2, 2, 1]]];
                                Abort[]
                            ];
                            {#1[[1]] -> {#1[[1, 1]], #2}, #1[[2]] -> {#1[[2, 1]], -#2}}
                            ,
                            (*We have multiple pairs*)
                            If[Mod[Length[#1], 2] === 0,
                                Table[{Sort[#1][[kdx]] -> {Sort[#1][[1, 1]], #2}, Sort[#1][[-kdx]] -> {Sort[#1][[-kdx, 1]], -#2}}, {kdx, 1, Length[#1] / 2}]
                                ,
                                (*Uneven number of objects: failure*)
                                Message[GetSuperIndexTermTransformations::multiContraction, #1];
                                Abort[]
                            ]
                        ]
                    ]&
                    ,
                    repl
                ];
        (*Furthermore, we isolate the group index replacements and the momentum replacements:*)
        replForward =
            {
                repl
                ,
                Map[Keys[#][[2, 1]] -> $momentum[Values[#]]&, repl]
                ,
                Flatten @
                    Table[
                        If[Length[Keys[repl[[idx]]][[2]]] > 1,
                            Table[(Keys[repl[[idx]]][[2, 2, jdx]] -> $groupIndex[Values[repl[[idx]]], jdx]), {jdx, 1, Length[Keys[repl[[idx]]][[2, 2]]]}]
                            ,
                            {}
                        ]
                        ,
                        {idx, 1, Length[repl]}
                    ]
            };
        (*Finally, construct the back-transformation and return:*)
        replBackward = Map[Map[Values[#] -> Keys[#]&, #]&, replForward];
        replBackward[[1]] = Join[Map[{Keys[#][[1]], -Keys[#][[2]]} -> {Values[#][[1]], Join[{-Values[#][[2, 1]]}, Values[#][[2, 2 ;; ]]]}&, replBackward[[1]]], replBackward[[1]]];
        Return[{replForward, replBackward}];
    ];

GetSuperIndexTermTransformations[setup_, eq_FEx] :=
    Module[{repl, replForward, replBackward, doFields, undoFields, forwardFunction, backwardFunction},
        doFields = replFields[setup];
        undoFields = unreplFields[setup];
        repl = Map[GetSuperIndexTermTransformationsSingleFTerm[setup, #]&, List @@ eq];
        replForward = {Join @@ repl[[All, 1, 1]], Join @@ repl[[All, 1, 2]], Join @@ repl[[All, 1, 3]]};
        replBackward = {Join @@ repl[[All, 2, 1]], Join @@ repl[[All, 2, 2]], Join @@ repl[[All, 2, 3]]};
        forwardFunction[expr_] :=
            Module[{ret},
                ret = expr /. doFields /. a_[f_, i_] /; MemberQ[$allObjects, a] :> a[Transpose[{f, i}]];
                ret = ret /. replForward[[1]] /. replForward[[3]] /. replForward[[2]];
                ret = ret /. a_[l_List] /; MemberQ[$allObjects, a] :> a @@ Transpose[l] /. undoFields;
                Return[ret];
            ];
        backwardFunction[expr_] :=
            Module[{ret},
                ret = expr /. doFields /. a_[f_, i_] /; MemberQ[$allObjects, a] :> a[Transpose[{f, i}]];
                ret = ret /. replBackward[[2]] /. replBackward[[3]] /. replBackward[[1]];
                ret = ret /. a_[l_List] /; MemberQ[$allObjects, a] :> a @@ Transpose[l] /. undoFields;
                Return[ret];
            ];
        Return[{forwardFunction, backwardFunction}];
    ];

GetSuperIndexTermTransformations[setup_, term_FTerm] :=
    GetSuperIndexTermTransformations[setup, FEx[term]];
