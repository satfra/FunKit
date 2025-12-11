(*AntiGrassmann-Grassmann gives 1, otherwise -1*)

GrassOrder[setup_, f1_, f2_, sign_] :=
    GrassOrder[setup, f1, f2, sign] =
        Module[{},
            (2 * Boole[IsFermion[setup, f1]] - 1) ^ Boole[!(sign === 1)]
        ];

(*Return 
Subscript[\[Gamma], ab] = \[Gamma]^ab = (0 -1
                                         1  0) and ordering (\[Psi], Overscript[\[Psi], _])*)

metric[setup_, a_, b_] :=
    metric[setup, a, b] =
        Module[{f1 = makePosIdx[a], f2 = makePosIdx[b], f2p, lower, sign},
            f2p = GetPartnerField[setup, f2];
            If[(f1 =!= f2p && f1 =!= f2),
                Return[0]
            ];
            (*Subscript[\[Gamma], a]^b = Subscript[\[Delta], a]^b*)
            lower = Map[isNeg, {a, b}];
            If[f1 === f2 && lower[[1]] && Not[lower[[2]]],
                Return[1]
            ];
            (*Subscript[\[Gamma]^a, b] = (-1)^abSubscript[\[Delta]^a, b]*)
            sign = CommuteSign[setup, f1, f2];
            If[f1 === f2 && Not[lower[[1]]] && lower[[2]],
                Return[sign]
            ];
            (*Subscript[\[Gamma], ab]=\[Gamma]^ab and fields fit with partners*)
            If[f1 === f2p && Not[Xor @@ lower],
                Return @ GrassOrder[setup, f1, f2, sign]
            ];
            (*Otherwise, 0*)
            Return[0]
        ];

(**********************************************************************************
    Reduce all metric and FMinus factors in FTerm or FEx expressions
**********************************************************************************)

ReduceIndices::FTermFEx = "The given expression is neither an FTerm nor an FEx:
`1`";

ReduceIndices[setup_, term_] :=
    (
        Message[ReduceIndices::FTermFEx, term];
        Abort[]
    );

ReduceIndices[setup_, term_FTerm] :=
    Module[{closedSIndices, cases, casesOpen, closed, i, both, result = term, casesFMinus},
        closedSIndices = GetClosedSuperIndices[setup, term];
        (*Pick out all metric factors and FMinus...*)
        cases = Cases[term, \[Gamma][__] | FMinus[__], {1, 3}];
        (*..which do not contain undetermined fields...*)
        cases = Select[cases, FreeQ[getFields[#], AnyField]&];
        casesFMinus = Select[cases, Head[#] === FMinus&];
        cases = Select[cases, Head[#] === \[Gamma]&];
        closed = Map[MemberQ[closedSIndices, makePosIdx[#]]&, getIndices /@ cases, {2}];
        casesOpen = Pick[cases, Map[Not[#[[1]] || #[[2]]]&, closed]];
        cases = Pick[cases, Map[#[[1]] || #[[2]]&, closed]];
        (*closed is a truth array indicating which indices of cases are closed *)
        closed = Pick[closed, Map[#[[1]] || #[[2]]&, closed]];
        (*replace the terms in question by the evaluated metric factor*)
        result = result /. Map[# :> metric[setup, getIdxSign[#, 1] getField[#, 1], getIdxSign[#, 2] getField[#, 2]]&, Join[cases, casesOpen]];
        (*replace the remaining indices. If both are up or both or down, the remaining indices change signs.*)
        If[Length[cases] > 0,
            result =
                result /.
                    Table[
                        both =
                            If[!Xor[isNeg[getIndex[cases[[i]], 1]], isNeg[getIndex[cases[[i]], 2]]],
                                -1
                                ,
                                1
                            ];
                        If[closed[[i, 1]],
                            makePosIdx[getIndex[cases[[i]], 1]] -> both * makePosIdx[getIndex[cases[[i]], 2]]
                            ,
                            makePosIdx[getIndex[cases[[i]], 2]] -> both * makePosIdx[getIndex[cases[[i]], 1]]
                        ]
                        ,
                        {i, 1, Length[cases]}
                    ];
        ];
        (*Resolve all FMinus factors*)
        result = result /. Map[# -> CommuteSign[setup, getField[#, 1], getField[#, 2]]&, casesFMinus];
        FunKitDebug[5, "Reduced factors in FTerm: ", cases];
        Return[result];
    ];

ReduceIndices[setup_, eq_FEx] :=
    Module[{},
        Map[ReduceIndices[setup, #]&, eq]
    ];
