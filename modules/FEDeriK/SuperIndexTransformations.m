(**********************************************************************************
    SuperIndexTransformations.m -- Transforms between explicit and super-index forms

    Public API:
      GetSuperIndexTermTransformations -- Returns {forward, backward} transformation
                                          functions between explicit and super-indices
      NormalizeSuperIndices      -- Canonicalizes closed super-index names

    Internal:
      GetSuperIndexTermTransformationsSingleFTerm -- Computes transformations for one FTerm
                                    (used by GetSuperIndexTermTransformations)

    Variables:
      $momentum                  -- Tagged head for momenta in transformations
      $groupIndex                -- Tagged head for group indices in transformations
**********************************************************************************)

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
        {kdx, allObj, idx, jdx, newObj, indexPosToChange, indicesToChange, newSuperIndices, repl, replForward, replBackward, canonicalMom}
        ,
        (* Pick a sign-canonical form so {m} and {-m} share a grouping key.
           Abs[m] doesn't auto-simplify for Plus expressions like -l1+p1, which
           used to leave conjugate momenta in different groups and trip the
           pair-conservation check below. *)
        canonicalMom[m_] := If[OrderedQ[{m, -m}], m, -m];
(* Zip indexed objects notation-agnostically via getFields/getIndices.
   Filter to indexedObjectQ first to avoid double-extracting NotationB embedded
   field[index] pairs that appear at depth 2 inside indexed objects. *)
        Module[{allObjRaw = ExtractObjectsWithIndex[setup, term]},
            allObj = Select[allObjRaw, indexedObjectQ] /. (obj_?indexedObjectQ) :> (Head[obj])[Transpose[{getFields[obj], getIndices[obj]}]];
            (* Free field applications (e.g. background fields) are processed separately *)
            allObj = Join[allObj, replFields[setup, Select[allObjRaw, Not[indexedObjectQ[#]]&]]];
        ];
        (*We find all positions where indices are given explicitly*)
        indexPosToChange = Map[Join[Position[#[[1]], {_Symbol | Times[-1, _Symbol], _List}, {1}], Position[#[[1]], {_Symbol | Times[-1, _Symbol]}, {1}]]&, allObj];
        indexPosToChange = Map[Flatten, indexPosToChange];
        If[Length[indexPosToChange] === 0,
            Return[{{{}, {}, {}}, {{}, {}, {}}}]
        ];
(*Next, we isolate the group indices and try to group according to these. If no group indices are present, we try to group by momenta.
  Guard: skip entries with no positions to change. In NotationB bare fields have an atomic
  first Part, so Part[atom, {}] would produce Symbol[] instead of {} — avoid this.*)
        indicesToChange =
            Flatten[
                Table[
                    If[Length[indexPosToChange[[idx]]] > 0,
                        allObj[[idx, 1, indexPosToChange[[idx]]]]
                        ,
                        {}
                    ]
                    ,
                    {idx, 1, Length[allObj]}
                ]
                ,
                1
            ];
        indexPosToChange =
            PositionIndex[
                Join[
                        (*group indices:*)Select[indicesToChange, Length[#[[2]]] === 2&][[All, 2, 2]]
                        ,
                        (*momenta — canonicalise sign so {m} and {-m} share a key:*)
                        Map[{canonicalMom[#[[1]]]}&, Select[indicesToChange, Length[#[[2]]] === 1&][[All, 2]]]
                    ]
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

GetSuperIndexTermTransformations[setup_, eq_] :=
    Module[{repl, replForward, replBackward, forwardFunction, backwardFunction},
        repl = Map[GetSuperIndexTermTransformationsSingleFTerm[setup, #]&, List @@ eq];
        replForward = {Join @@ repl[[All, 1, 1]], Join @@ repl[[All, 1, 2]], Join @@ repl[[All, 1, 3]]};
        replBackward = {Join @@ repl[[All, 2, 1]], Join @@ repl[[All, 2, 2]], Join @@ repl[[All, 2, 3]]};
        forwardFunction[expr_] :=
            Module[{ret},
                ret = replFields[setup, expr /. (obj_?indexedObjectQ) :> (Head[obj])[Transpose[{getFields[obj], getIndices[obj]}]]];
                ret = ret /. replForward[[1]] /. replForward[[3]] /. replForward[[2]];
                ret = ret /. a_[l_List] /; MemberQ[$allObjects, a] :> makeObj[a, l[[All, 1]], l[[All, 2]]];
                ret = unreplFields[setup, ret];
                Return[ret];
            ];
        backwardFunction[expr_] :=
            Module[{ret},
                ret = replFields[setup, expr /. (obj_?indexedObjectQ) :> (Head[obj])[Transpose[{getFields[obj], getIndices[obj]}]]];
                ret = ret /. replBackward[[2]] /. replBackward[[3]] /. replBackward[[1]];
                ret = ret /. a_[l_List] /; MemberQ[$allObjects, a] :> makeObj[a, l[[All, 1]], l[[All, 2]]];
                ret = unreplFields[setup, ret];
                Return[ret];
            ];
        Return[{forwardFunction, backwardFunction}];
    ];

GetSuperIndexTermTransformations[setup_, term_FTerm] :=
    GetSuperIndexTermTransformations[setup, {term}];

NormalizeSuperIndices[setup_, expr_FTerm] :=
    Module[{cindices, orderingFunction},
        cindices = GetClosedSuperIndices[setup, expr];
        orderingFunction[e1_, e2_] :=
            Module[{p1, p2, idx},
                p1 = FirstPosition[expr, e1];
                p2 = FirstPosition[expr, e2];
                For[idx = 1, idx <= Min[Length[p1], Length[p2]], idx++,
                    If[p1[[idx]] < p2[[idx]],
                        Return[True]
                    ];
                    If[p1[[idx]] > p2[[idx]],
                        Return[False]
                    ];
                ];
                Return[False];
            ];
        cindices = Sort[cindices, orderingFunction];
        expr /. Thread[cindices -> Table[Symbol["sIdx" <> ToString[idx]], {idx, 1, Length[cindices]}]]
    ];

NormalizeSuperIndices[setup_, expr_FEx] :=
    Map[NormalizeSuperIndices[setup, #]&, expr];
