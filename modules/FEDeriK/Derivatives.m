(**********************************************************************************
    ResolveFDOp : Resolve a single occurrence of FDOp in an FTerm or FEx.
**********************************************************************************)

ResolveFDOp::nested = "The given term contains nested FDOp. Before proceeding, you need to expand these with DExpand. 
Error in `1`";

ResolveFDOp[setup_, FEx_FEx] :=
    Module[{},
        Return[FEx @@ Map[ResolveFDOp[setup, #]&, List @@ FEx]];
    ];

ResolveFDOp[setup_, term_FTerm] :=
    Module[
        {rTerm = term, FDOpPos, termsNoFDOp, dF, idx, i, obj, ind, a, dTerms, nPre, nPost, ret, cTerm, doFields, fw, bw}
        ,
        (*We cannot proceed if any nested FDOp are present*)
        If[MemberQ[(List @@ rTerm), FTerm[pre___, FDOp[__], post___], {1, 5}],
            Message[ResolveFDOp::nested, term];
            Abort[]
        ];
        (*If no derivatives are present, do nothing*)
        If[FreeQ[rTerm, FDOp[__]],
            Return[rTerm]
        ];
        FDOpPos = Length[rTerm] - FirstPosition[Reverse @ (List @@ rTerm), FDOp[_]][[1]] + 1;
        termsNoFDOp = FTerm[rTerm[[1 ;; FDOpPos - 1]], rTerm[[FDOpPos + 1 ;; ]]];
        (*If the derivative operator is trailing, it acts on nothing and the term is zero.*)
        If[FDOpPos >= Length[rTerm],
            Return[FEx[0]]
        ];
        dF = rTerm[[FDOpPos, 1]];
        (*Perform the product rule*)
        nPre = FDOpPos - 1;
        nPost = Length[rTerm] - FDOpPos;
        (*commuting it past*)
        cTerm = 1;
        dTerms = Table[0, {idx, 1, nPost}];
        doFields = replFields[setup];
        Do[
            dTerms[[idx]] = FTerm[termsNoFDOp[[ ;; nPre + idx - 1]], FTerm[cTerm, FunctionalD[setup, termsNoFDOp[[nPre + idx]], dF]], termsNoFDOp[[nPre + idx + 1 ;; ]]];
            obj = ExtractObjectsWithIndex[setup, FTerm[termsNoFDOp[[nPre + idx]]]];
            obj = Select[obj, MemberQ[$nonCommutingObjects, Head[#]] || MatchQ[#, _Symbol[_]]&];
            obj = obj /. doFields;
            (*Commuting the next derivative past the objects in the current part*)
            cTerm = cTerm * Times @@ Map[FMinus[{Head[dF], #[[1]]}, {dF[[1]], #[[2]]}]&, Transpose[{Flatten[obj[[All, 1]]], Flatten[obj[[All, 2]]]}]];
            ,
            {idx, 1, nPost}
        ];
        (*Note: up till here, the performance impact is minimal.However, the following blowup of terms will multiply it*)
        dTerms = ReduceIndices[setup, FEx @@ dTerms];
        Return[ReduceFEx[setup, dTerms]];
    ];

(**********************************************************************************
    ResolveDerivatives : Iteratively resolve all derivative operators in an FTerm or FEx
**********************************************************************************)

ResolveDerivatives::argument = "The given argument is neither an FTerm nor a FEx.
The argument was `1`";

Options[ResolveDerivatives] = {"Symmetries" -> {}};

ResolveDerivatives[setup_, term_FTerm, OptionsPattern[]] :=
    ResolveDerivatives[setup, FEx[term], "Symmetries" -> OptionValue["Symmetries"]]

ResolveDerivatives[setup_, eq_FEx, OptionsPattern[]] :=
    Module[{ret = eq, mmap, fw, bw, i},
        FunKitDebug[1, "Resolving derivatives"];
        If[FreeQ[ret, FDOp[__], Infinity],
            Return[ReduceFEx[setup, FEx[ret]]]
        ];
        {fw, bw} = GetSuperIndexTermTransformations[setup, eq];
        ret = ret // fw;
        (*ParallelMap will incur some overhead, but it quickly pays off*)
        mmap =
            If[Total[Length /@ (List @@ FEx[ret])] > 10,
                ParallelMap
                ,
                Map
            ];
        i = 0;
        While[
            MemberQ[ret, FDOp[__], Infinity] && i < $MaxDerivativeIterations
            ,
            FunKitDebug[1, "Doing derivative pass ", i + 1];
            ret = FEx @@ mmap[ResolveFDOp[setup, #]&, List @@ ret];
(*If AnSEL has been loaded, use FSimplify to reduce redundant terms
If[ModuleLoaded[AnSEL],
    ret = FunKit`FSimplify[setup, ret, "Symmetries" -> OptionValue["Symmetries"]]
];*)
            i++;
        ];
        ret = ret // bw;
        FunKitDebug[1, "Finished resolving derivatives"];
        Return[ret];
    ]

ResolveDerivatives[setup_, a___] :=
    Module[{},
        Message[ResolveDerivatives::argument, {a}];
        Abort[];
    ];

(**********************************************************************************
    TakeDerivatives : Take several functional derivatives on a given expression.
**********************************************************************************)

Options[TakeDerivatives] = {"Symmetries" -> {}};

TakeDerivatives[setup_, expr_, derivativeList_, OptionsPattern[]] :=
    Module[{result, externalIndexNames, outputReplacements, derivativeListSIDX},
        AssertFSetup[setup];
        AssertDerivativeList[setup, derivativeList];
        (*We take them in reverse order.*)
        derivativeListSIDX = derivativeList;
        (*First, fix the indices in the input equation, i.e. make everything have unique names*)
        result = FixIndices[setup, FEx[expr]];
        If[Length[derivativeListSIDX] === 0,
            Return[ResolveDerivatives[setup, result, "Symmetries" -> OptionValue["Symmetries"]]]
        ];
        FunKitDebug[1, "Adding the derivative operator ", (FTerm @@ (FDOp /@ derivativeListSIDX))];
        (*Perform all the derivatives, one after the other*)
        result = ResolveDerivatives[setup, (FTerm @@ (FDOp /@ derivativeListSIDX)) ** result];
(*
If[ModuleLoaded[AnSEL],
    result = FunKit`FSimplify[setup, result, "Symmetries" -> OptionValue["Symmetries"]]
];*)
        Return[result];
    ];
