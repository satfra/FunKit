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

SymmetryFactorFromList[ex_List] :=
    Module[{ret},
        ret = Gather[ex];
        ret = 1 / Factorial[Length[#]]& /@ ret;
        Times @@ ret
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

ReduceIndices[setup_, {}] :=
    {};

ReduceIndices[setup_, term_FTerm] :=
    Module[{gPairs, closedSIndices, cases, casesOpen, closed, i, both, result = term, casesFMinus, casesSymmetry, casesGamma},
        closedSIndices = GetClosedSuperIndices[setup, term];
        (*Pick out all metric factors and FMinus...*)
        cases = Cases[term, \[Gamma][__] | FMinus[__] | SymmetryFactor[__], Infinity];
        (*..which do not contain undetermined fields...*)
        cases = Select[cases, FreeQ[getFields[#], AnyField]&];
        (*..and which contain at least one closed index.*)
        casesFMinus = Select[cases, Head[#] === FMinus&];
        casesSymmetry = Select[cases, Head[#] === SymmetryFactor&];
        casesGamma = Select[cases, Head[#] === \[Gamma]&];
        (*We have to exclude a particular case here: if we have two gammas, contracted with each other, and one open index each, we cannot replace both!*)
        (*First, find all Gammas that have an overlap of one closed index:*)
        gPairs = Subsets[casesGamma, {2}];
        gPairs = Select[gPairs, Length[DeleteDuplicates[makePosIdx /@ Join[#[[2, 2]], #[[1, 2]]]]] == 3&];
        (*Now, see which of these have a closed index in common:*)
        closed = Map[getIndices, gPairs, {2}];
        closed = Map[List @@ #&, Map[MemberQ[closedSIndices, makePosIdx[#]]&, closed, {3}]];
        closed = Map[Total[Boole /@ Flatten[#]] == 2&, closed];
        gPairs = Pick[gPairs, closed];
        (*Remove the first of each of these from the list of replaceable Gammas*)
        casesGamma = Complement[casesGamma, gPairs[[All, 1]]];
        (*Now, make a truth array indicating which of the remaining gammas are closed in which index*)
        closed = Map[MemberQ[closedSIndices, makePosIdx[#]]&, getIndices /@ casesGamma, {2}];
        (*Finally, remove all elements from casesGamma with only open indices*)
        casesGamma = Pick[casesGamma, Map[#[[1]] || #[[2]]&, closed]];
        (*Next do the thing: replace the terms in question by the evaluated metric factors*)
        FunKitDebug[5, "Found Gamma factors in FTerm: ", casesGamma];
        FunKitDebug[5, "Found FMinus factors in FTerm: ", casesFMinus];
        FunKitDebug[5, "Found Symmetry factors in FTerm: ", casesSymmetry];
        FunKitDebug[5, "Closed indices: ", closed];
        result = result /. Map[# :> metric[setup, getIdxSign[#, 1] getField[#, 1], getIdxSign[#, 2] getField[#, 2]]&, casesGamma];
        (*Resolve all FMinus factors*)
        result = result /. Map[# -> CommuteSign[setup, getField[#, 1], getField[#, 2]]&, casesFMinus];
        (*Resolve all SymmetryFactor factors*)
        result = result /. Map[# -> SymmetryFactorFromList[getFields[#]]&, casesSymmetry];
        (*replace the remaining indices. If both are up or both or down, the remaining indices change signs.*)
        If[Length[casesGamma] > 0,
            result =
                result /.
                    Table[
                        both =
                            If[!Xor[isNeg[getIndex[casesGamma[[i]], 1]], isNeg[getIndex[casesGamma[[i]], 2]]],
                                -1
                                ,
                                1
                            ];
                        If[closed[[i, 1]],
                            makePosIdx[getIndex[casesGamma[[i]], 1]] -> both * makePosIdx[getIndex[casesGamma[[i]], 2]]
                            ,
                            makePosIdx[getIndex[casesGamma[[i]], 2]] -> both * makePosIdx[getIndex[casesGamma[[i]], 1]]
                        ]
                        ,
                        {i, 1, Length[casesGamma]}
                    ];
        ];
        Return[result];
    ];

ReduceIndices[setup_, eq_FEx] :=
    Module[{},
        Map[ReduceIndices[setup, #]&, eq]
    ];
