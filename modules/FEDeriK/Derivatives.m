(**********************************************************************************
    Derivatives.m -- Functional derivative resolution

    Public API:
      FResolveFDOp               -- Resolves a single FDOp in an FTerm or FEx
      FResolveDerivatives        -- Iteratively resolves all FDOp in an expression
      FTakeDerivatives           -- Takes functional derivatives w.r.t. a field list
**********************************************************************************)

FResolveFDOp::nested = "The given term contains nested FDOp. Before proceeding, you need to expand these with DExpand. 
Error in `1`";

FResolveFDOp[setup_, expr_FEx] :=
    Module[{},
        AssertFSetup[setup];
        Return[FEx @@ Catenate[BalancedMap[FResolveFDOpInternal[setup, #]&, List @@ expr]]];
    ];

(* Public wrapper: returns FEx for backward compatibility *)
FResolveFDOp[setup_, term_FTerm] :=
    FEx @@ FResolveFDOpInternal[setup, term];

(* Internal workhorse: returns a plain List of FTerms (no FEx wrapper) *)
FResolveFDOpInternal[setup_, term_FTerm] :=
    Module[
        {rTerm = term, FDOpPos, termsNoFDOp, dF, idx, i, obj, ind, a, dTerms, nPre, nPost, ret, cTerm, doFields, fw, bw, deriv}
        ,
        (*We cannot proceed if any nested FDOp are present*)
        If[MemberQ[(List @@ rTerm), FTerm[pre___, FDOp[__], post___], {1, 5}],
            Message[FResolveFDOp::nested, term];
            Abort[]
        ];
        (*Find rightmost FDOp; if none present, return immediately*)
        FDOpPos = FirstPosition[Reverse @ (List @@ rTerm), _FDOp, Missing["NotFound"], {1}];
        If[MissingQ[FDOpPos],
            Return[{rTerm}]
        ];
        FDOpPos = Length[rTerm] - FDOpPos[[1]] + 1;
        termsNoFDOp = FTerm[rTerm[[1 ;; FDOpPos - 1]], rTerm[[FDOpPos + 1 ;; ]]];
        (*If the derivative operator is trailing, it acts on nothing and the term is zero.*)
        If[FDOpPos >= Length[rTerm],
            Return[{}]
        ];
        dF = rTerm[[FDOpPos, 1]];
        FunKitDebug[2, "Found derivative operator ", FDOp[dF], " at position ", FDOpPos, " in given term."];
        (*Perform the product rule*)
        nPre = FDOpPos - 1;
        nPost = Length[rTerm] - FDOpPos;
        (*commuting it past*)
        cTerm = 1;
        dTerms = Table[0, {idx, 1, nPost}];
        doFields = replFields[setup];
        Do[
            deriv = FunctionalD[setup, termsNoFDOp[[nPre + idx]], dF] // Expand;
            If[Head[deriv] === Plus,
                deriv = FEx @@ (FTerm /@ deriv);
            ];
            dTerms[[idx]] = FTerm[termsNoFDOp[[ ;; nPre + idx - 1]], FTerm[cTerm, deriv], termsNoFDOp[[nPre + idx + 1 ;; ]]];
            FunKitDebug[5, "Performed derivative on term ", idx, ": ", dTerms[[idx]]];
            obj = ExtractObjectsWithIndex[setup, FTerm[termsNoFDOp[[nPre + idx]]]];
            obj = Select[obj, MemberQ[$nonCommutingObjects, Head[#]] || MatchQ[#, _Symbol[_]]&];
            obj = obj /. doFields;
            (*Commuting the next derivative past the objects in the current part.
              Extract {field, index} pairs from each object. Bare field applications (e.g. A[si])
              are not indexed objects, so getFields/getIndices would fail on them — handle separately.*)
            Module[{pairs},
                pairs = Flatten[
                    Map[
                        If[indexedObjectQ[#],
                            Transpose[{getFields[#], getIndices[#]}],
                            {{Head[#], #[[1]]}}
                        ]&,
                        obj
                    ], 1];
                cTerm = cTerm * Times @@ Map[makeObj[FMinus, {Head[dF], #[[1]]}, {dF[[1]], #[[2]]}]&, pairs];
            ];
            ,
            {idx, 1, nPost}
        ];
        (*Note: up till here, the performance impact is minimal.However, the following blowup of terms will multiply it*)
        (*Light reduction: only resolve FMinus/SymmetryFactor signs. Full metric resolution deferred to per-pass.*)
        dTerms = Map[ReduceIndicesLight[setup, #]&, dTerms];
        FunKitDebug[6, "Result: ", dTerms];
        (*Filter zeros and return as plain list of FTerms*)
        Select[dTerms, # =!= FTerm[0] && # =!= 0&]
    ];

FResolveFDOp[setup_, expr_] :=
    (Message[FunKit::invalidArguments, FResolveFDOp]; Abort[]);

(**********************************************************************************
    FResolveDerivatives : Iteratively resolve all derivative operators in an FTerm or FEx
**********************************************************************************)

FResolveDerivatives::argument = "The given argument is neither an FTerm nor a FEx.
The argument was `1`";

Options[FResolveDerivatives] = {"Symmetries" -> {}};

FResolveDerivatives[setup_, term_FTerm, OptionsPattern[]] :=
    FResolveDerivatives[setup, FEx[term], "Symmetries" -> OptionValue["Symmetries"]]

FResolveDerivatives[setup_, eq_FEx, OptionsPattern[]] :=
    Module[{ret = eq, annotations, fw, bw, i, symmetries},
        AssertFSetup[setup];
        FunKitDebug[1, "Resolving derivatives"];
        If[FreeQ[ret, FDOp[__], Infinity],
            Return[ReduceFEx[setup, FEx[ret]]]
        ];
        {ret, annotations} = SeparateFExAnnotations[ret];
        symmetries =
            If[KeyExistsQ[annotations, "Symmetries"],
                annotations["Symmetries"]
                ,
                {}
            ];
        symmetries = FMergeSymmetries[symmetries, OptionValue["Symmetries"]];
        {fw, bw} = GetSuperIndexTermTransformations[setup, ret];
        ret = BalancedMap[fw, ret];
        (*Convert to plain list of FTerms for the derivative loop*)
        If[Head[ret] === FEx, ret = List @@ ret];
        (*ParallelMap will incur some overhead, but it quickly pays off*)
        i = 0;
        While[
            MemberQ[ret, FDOp[__], {1, 3}] && i < $MaxDerivativeIterations
            ,
            FunKitDebug[1, "Doing derivative pass ", i + 1];
            Module[{t0 = AbsoluteTime[]},
                ret = Catenate[Map[FResolveFDOpInternal[setup, #]&, ret]];
                If[ValueQ[$ProfileFDOp], $ProfileFDOp += AbsoluteTime[] - t0];
            ];
            (*If AnSEL has been loaded, use FSimplify to reduce redundant terms.
              Skip for high-symmetry cases where the O(n^2 * |symmetries|) cost is too high. *)
            If[ModuleLoaded[AnSEL] && $AutoSimplify === True && Length[ret] < 32 && Length[symmetries] <= 6,
                Module[{t0 = AbsoluteTime[]},
                    ret = Map[ReduceIndices[setup, #]&, ret];
                    ret = List @@ FunKit`FSimplify[setup, FEx @@ ret, "Symmetries" -> symmetries];
                    If[ValueQ[$ProfileDerivSimplify], $ProfileDerivSimplify += AbsoluteTime[] - t0];
                ];
            ];
            FunKitDebug[1, "Finished pass ", i + 1, ", current length: ", Length[ret]];
            i++;
        ];
        (*Full metric resolution once after all derivative passes*)
        ret = Map[ReduceIndices[setup, #]&, ret];
        ret = Map[bw, ret];
        FunKitDebug[1, "Finished resolving derivatives"];
        Return[MergeFExAnnotations[FEx @@ ret, annotations]];
    ]

FResolveDerivatives[setup_, a___] :=
    Module[{},
        Message[FResolveDerivatives::argument, {a}];
        Abort[];
    ];

(**********************************************************************************
    FTakeDerivatives : Take several functional derivatives on a given expression.
**********************************************************************************)

Options[FTakeDerivatives] = {"Symmetries" -> {}};

FTakeDerivatives[setup_, expr_FTerm, derivativeList_, OptionsPattern[]] :=
    FTakeDerivatives[setup, FEx[expr], derivativeList, "Symmetries" -> OptionValue["Symmetries"]];

FTakeDerivatives[setup_, expr_FEx, derivativeList_, OptionsPattern[]] :=
    Module[{result, externalIndexNames, outputReplacements, derivativeListSIDX, symmetries, annotations},
        AssertFSetup[setup];
        AssertDerivativeList[setup, derivativeList];
        (*We take them in reverse order.*)
        derivativeListSIDX = derivativeList;
        (***)
        {result, annotations} = SeparateFExAnnotations[expr];
        (*First, fix the indices in the input equation, i.e. make everything have unique names*)
        result = FixIndices[setup, result];
        If[Length[derivativeListSIDX] === 0,
            Return[FResolveDerivatives[setup, result, "Symmetries" -> OptionValue["Symmetries"]]]
        ];
        If[ModuleLoaded[AnSEL] && OptionValue["Symmetries"] === {} && $AutoBuildSymmetryList === True,
            FunKitDebug[2, "Auto-building symmetry list for derivatives"];
            symmetries = FunKit`FMakeSymmetryList[setup, derivativeListSIDX];
            FunKitDebug[3, "Built symmetries: ", symmetries];
            ,
            symmetries = OptionValue["Symmetries"];
        ];
        symmetries = FMergeSymmetries[symmetries, OptionValue["Symmetries"]];
        If[KeyExistsQ[annotations, "Symmetries"],
            symmetries = FMergeSymmetries[symmetries, annotations["Symmetries"]];
        ];
        If[symmetries =!= {},
            result = FEx[FEx @@ result, "Symmetries" -> symmetries]
        ];
        FunKitDebug[1, "Adding the derivative operator ", (FTerm @@ (FDOp /@ derivativeListSIDX))];
        (*Perform all the derivatives, one after the other*)
        result = FResolveDerivatives[setup, (FTerm @@ (FDOp /@ derivativeListSIDX)) ** (FEx @@ result)];
        (*Finally, reduce indices again to clean up any duplicates introduced by the derivatives*)
        Return[result];
    ];
