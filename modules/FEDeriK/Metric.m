(**********************************************************************************
    Metric.m -- Field-space metric, index reduction, and commutation signs

    Public API:
      ReduceIndices              -- Resolves all metric/FMinus/SymmetryFactor in FTerm/FEx
      ReduceGamma                -- Resolves only metric factors in FTerm (used by ReduceIndices)
      ReduceIndicesLight         -- Resolves only FMinus/SymmetryFactor in FTerm/FEx (used during derivative iteration)
      ReduceIndicesBatch         -- Batched version of ReduceIndices for Lists of FTerms (used in Truncation)

    Internal:
      GrassOrder                 -- Computes Grassmann ordering sign for a field pair
                                    (used by metric)
      metric                     -- Evaluates the field-space metric for two fields
                                    (used by ReduceIndices)
      SymmetryFactorFromList     -- Computes symmetry factor from field repetitions
                                    (used by ReduceIndices)
**********************************************************************************)

(*AntiGrassmannField-GrassmannField gives 1, otherwise -1*)

GrassOrder[setup_, f1_, f2_, sign_] :=
    GrassOrder[setup, f1, f2, sign] =
        Module[{},
            (2 * Boole[IsGrassmannField[setup, f1]] - 1) ^ Boole[!(sign === 1)]
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

(**********************************************************************************
    Light variant: resolves only FMinus and SymmetryFactor (cheap scalar signs).
    Skips the expensive gamma pair-matching and GetClosedSuperIndices.
    Used during derivative iteration where terms still contain AnyField.
**********************************************************************************)

ReduceIndicesLight[setup_, term_FTerm] :=
    Module[{result = term, casesFMinus, casesSymmetry},
        casesFMinus = Cases[term, FMinus[__], Infinity];
        casesFMinus = Select[casesFMinus, FreeQ[getFields[#], AnyField]&];
        casesSymmetry = Cases[term, SymmetryFactor[__], Infinity];
        casesSymmetry = Select[casesSymmetry, FreeQ[getFields[#], AnyField]&];
        If[Length[casesFMinus] > 0,
            result = result /. Map[# -> CommuteSign[setup, getField[#, 1], getField[#, 2]]&, casesFMinus];
        ];
        If[Length[casesSymmetry] > 0,
            result = result /. Map[# -> SymmetryFactorFromList[getFields[#]]&, casesSymmetry];
        ];
        result
    ];

ReduceIndicesLight[setup_, eq_FEx] :=
    Map[ReduceIndicesLight[setup, #]&, eq];

ReduceIndicesLight[setup_, 0] :=
    0;

ReduceIndicesLight[setup_, {}] :=
    {};

ReduceIndices::FTermFEx = "The given expression is neither an FTerm nor an FEx:
`1`";

ReduceIndices[setup_, term_] :=
    (
        Message[ReduceIndices::FTermFEx, term];
        Abort[]
    );

ReduceIndices[setup_, {}] :=
    {};

ReduceIndices[setup_, 0] :=
    0;

ReduceGamma[setup_, term_FTerm] :=
    Module[{gPairs, closedSIndices, closed, i, both, result = term, casesGamma, t0 = AbsoluteTime[]},
        closedSIndices = GetClosedSuperIndices[setup, term];
        (*Pick out all metric factors*)
        casesGamma = Cases[term, \[Gamma][__], Infinity];
        (*..which do not contain undetermined fields...*)
        casesGamma = Select[casesGamma, FreeQ[getFields[#], AnyField]&];
        (*We have to exclude a particular case here: if we have two gammas, contracted with each other, and one open index each, we cannot replace both!*)
        (*First, find all Gammas that have an overlap of one closed index:*)
        gPairs = Subsets[casesGamma, {2}];
        gPairs = Select[gPairs, Length[DeleteDuplicates[makePosIdx /@ Join[getIndices[#[[2]]], getIndices[#[[1]]]]]] == 3&];
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
        FunKitDebug[5, "Closed indices: ", closed];
        result = result /. Map[# :> metric[setup, getIdxSign[#, 1] getField[#, 1], getIdxSign[#, 2] getField[#, 2]]&, casesGamma];
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
        If[ValueQ[$ReduceIndicesTime],
            $ReduceIndicesTime += AbsoluteTime[] - t0;
            $ReduceIndicesCount++
        ];
        Return[result];
    ];

ReduceIndices[setup_, term_FTerm] :=
    Module[{gPairs, closedSIndices, cases, casesOpen, closed, i, both, result = term, casesFMinus, casesSymmetry, casesGamma, t0 = AbsoluteTime[]},
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
        gPairs = Select[gPairs, Length[DeleteDuplicates[makePosIdx /@ Join[getIndices[#[[2]]], getIndices[#[[1]]]]]] == 3&];
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
        If[ValueQ[$ReduceIndicesTime],
            $ReduceIndicesTime += AbsoluteTime[] - t0;
            $ReduceIndicesCount++
        ];
        Return[result];
    ];

(* Batched ReduceIndices: processes a List of FTerms with shared FMinus/SymmetryFactor resolution.
   Collects all unique FMinus/SymmetryFactor across all terms, builds rules once, applies in one /. pass.
   Terms with γ are handled individually (γ resolution involves per-term closed-index analysis). *)

ReduceIndicesBatch[setup_, terms_List] :=
    Module[
        {allFMinus, allSymF, batchRules, result = terms, t0 = AbsoluteTime[]}
        ,
        (*Batch FMinus + SymmetryFactor: collect unique instances, build rules, apply once*)
        allFMinus = DeleteDuplicates @ Cases[result, fm_FMinus /; FreeQ[getFields[fm], AnyField], Infinity];
        allSymF = DeleteDuplicates @ Cases[result, sf_SymmetryFactor /; FreeQ[getFields[sf], AnyField], Infinity];
        batchRules = Join[Map[# -> CommuteSign[setup, getField[#, 1], getField[#, 2]]&, allFMinus], Map[# -> SymmetryFactorFromList[getFields[#]]&, allSymF]];
        If[Length[batchRules] > 0,
            result = result /. Dispatch[batchRules]
        ];
        (*Per-term γ resolution — only for terms that actually contain γ*)
        result =
            Map[
                If[!FreeQ[#, \[Gamma]],
                    ReduceGamma[setup, #]
                    ,
                    #
                ]&
                ,
                result
            ];
        If[ValueQ[$ReduceIndicesTime],
            $ReduceIndicesTime += AbsoluteTime[] - t0;
            $ReduceIndicesCount += Length[terms]
        ];
        result
    ];

ReduceIndices[setup_, eq_FEx] :=
    Module[{},
        Map[ReduceIndices[setup, #]&, eq]
    ];
