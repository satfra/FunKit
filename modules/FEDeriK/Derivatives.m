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
        {rTerm = unreplFields[setup, term], FDOpPos, termsNoFDOp, dF, idx, i, obj, ind, a, dTerms, nPre, nPost, ret, cTerm, deriv}
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
        Do[
            deriv = functionalDeriv[setup, termsNoFDOp[[nPre + idx]], dF] // Expand;
            If[Head[deriv] === Plus,
                deriv = FEx @@ (FTerm /@ deriv);
            ];
            dTerms[[idx]] = FTerm[termsNoFDOp[[ ;; nPre + idx - 1]], FTerm[cTerm, deriv], termsNoFDOp[[nPre + idx + 1 ;; ]]];
            FunKitDebug[5, "Performed derivative on term ", idx, ": ", dTerms[[idx]]];
            obj = ExtractObjectsWithIndex[setup, FTerm[termsNoFDOp[[nPre + idx]]]];
            obj = Select[obj, MemberQ[$nonCommutingObjects, Head[#]] || MatchQ[#, _Symbol[_]]&];
            obj = replFields[setup, obj];
(*Commuting the next derivative past the objects in the current part.
  Extract {field, index} pairs from each object. Bare field applications (e.g. A[si])
  are not indexed objects, so getFields/getIndices would fail on them — handle separately.*)
            Module[{pairs},
                pairs =
                    Flatten[
                        Map[
                            If[indexedObjectQ[#],
                                Transpose[{getFields[#], getIndices[#]}]
                                ,
                                {{Head[#], #[[1]]}}
                            ]&
                            ,
                            obj
                        ]
                        ,
                        1
                    ];
                (*Emit one FMinus per (leg-of-dF, pair) combination so the
                  product reproduces (-1)^(parity(dF) * parity(pair-field))
                  for multi-leg correlation-function derivative variables.
                  Single-leg dF = head[i] yields exactly one FMinus per pair,
                  identical to the prior behaviour.*)
                Module[{dFFields, dFInds},
                    {dFFields, dFInds} =
                        If[Length[dF] === 2 && ListQ[dF[[1]]] && ListQ[dF[[2]]],
                            (*multi-leg correlator: head[{f1,...,fn},{i1,...,in}]*)
                            {dF[[1]], dF[[2]]}
                            ,
                            (*single-leg field application: head[i]*)
                            {{Head[dF]}, {dF[[1]]}}
                        ];
                    cTerm = cTerm * Times @@ Flatten @ Map[
                        Function[pair,
                            MapThread[
                                Function[{f, i}, makeObj[FMinus, {f, pair[[1]]}, {i, pair[[2]]}]]
                                ,
                                {dFFields, dFInds}
                            ]
                        ]
                        ,
                        pairs
                    ];
                ];
            ];
            ,
            {idx, 1, nPost}
        ];
        (*Note: up till here, the performance impact is minimal.However, the following blowup of terms will multiply it*)
        (*Light reduction: only resolve FMinus/SymmetryFactor signs. Full metric resolution deferred to per-pass.*)
        dTerms = ReduceIndicesBatch[setup, dTerms];
        FunKitDebug[6, "Result: ", dTerms];
        (*Filter zeros and return as plain list of FTerms*)
        Select[dTerms, # =!= FTerm[0] && # =!= 0&]
    ];

FResolveFDOp[setup_, expr_] :=
    (
        Message[FunKit::invalidArguments, FResolveFDOp];
        Abort[]
    );

(**********************************************************************************
    FResolveDerivatives : Iteratively resolve all derivative operators in an FTerm or FEx
**********************************************************************************)

FResolveDerivatives::argument = "The given argument is neither an FTerm nor a FEx.
The argument was `1`";

Options[FResolveDerivatives] = {"Symmetries" -> {}, "Backend" -> Automatic};

FResolveDerivatives[setup_, term_FTerm, opts : OptionsPattern[]] :=
    FResolveDerivatives[setup, FEx[term], opts]

FResolveDerivatives[setup_, eq_FEx, OptionsPattern[]] :=
    Module[{ret = eq, annotations, fw, bw, i, symmetries},
        AssertFSetup[setup];
        FunKitDebug[1, "Resolving derivatives"];
        If[FreeQ[ret, FDOp[__], Infinity],
            Return[ReduceFEx[setup, FEx[ret]]]
        ];
(*C++ backend: resolve the embedded FDOps in one fused external run (CoBra).
  Unsupported input warns and falls through to the Mathematica implementation
  below.*)
        If[CppBackendActiveQ[OptionValue["Backend"]],
            Module[{cppResult},
                cppResult = Block[{$CppSoftFail = True}, Catch[CppResolveDerivatives[setup, eq, OptionValue["Symmetries"]], $CppFallbackTag]];
                Return[
                    If[cppResult =!= $CppFallbackMarker,
                        cppResult
                        ,
                        (*rerun with the backend pinned, so internal calls cannot re-enter the C++ path*)
                        FResolveDerivatives[setup, eq, "Symmetries" -> OptionValue["Symmetries"], "Backend" -> "Mathematica"]
                    ]
                ];
            ];
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
        If[Head[ret] === FEx,
            ret = List @@ ret
        ];
        (*ParallelMap will incur some overhead, but it quickly pays off*)
        i = 0;
        While[
            MemberQ[ret, FDOp[__], {1, 3}] && i < $MaxDerivativeIterations
            ,
            FunKitDebug[1, "Doing derivative pass ", i + 1];
            Module[{t0 = AbsoluteTime[]},
                ret = Catenate[Map[FResolveFDOpInternal[setup, #]&, ret]];
                If[ValueQ[$ProfileFDOp],
                    $ProfileFDOp += AbsoluteTime[] - t0
                ];
            ];
(*If AnSEL has been loaded, use FSimplify to reduce redundant terms.
  Skip for high-symmetry cases where the O(n^2 * |symmetries|) cost is too high. *)
            If[ModuleLoaded[AnSEL] && $AutoSimplify === True && Length[ret] < 32 && Length[symmetries] <= 6,
                Module[{t0 = AbsoluteTime[]},
                    ret = ReduceIndicesBatch[setup, ret];
                    ret = List @@ FunKit`FSimplify[setup, FEx @@ ret, "Symmetries" -> symmetries];
                    If[ValueQ[$ProfileDerivSimplify],
                        $ProfileDerivSimplify += AbsoluteTime[] - t0
                    ];
                ];
            ];
            FunKitDebug[1, "Finished pass ", i + 1, ", current length: ", Length[ret]];
            i++;
        ];
        (*Full metric resolution once after all derivative passes — batched*)
        ret = ReduceIndicesBatch[setup, ret];
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

Options[FTakeDerivatives] = {"Symmetries" -> {}, "Backend" -> Automatic};

FTakeDerivatives[setup_, expr_FTerm, derivativeList_, opts : OptionsPattern[]] :=
    FTakeDerivatives[setup, FEx[expr], derivativeList, opts];

FTakeDerivatives[setup_, expr_FEx, derivativeList_, OptionsPattern[]] :=
    Module[{result, externalIndexNames, outputReplacements, derivativeListSIDX, symmetries, annotations},
        AssertFSetup[setup];
        AssertDerivativeList[setup, derivativeList];
(*C++ backend: return a lazy handle; FTruncate/FSimplify/FEvaluate on it run
  one fused external call -- derivatives, truncation and simplification in a
  single process (CoBra). Unsupported input warns and falls through to the
  Mathematica implementation below.*)
        If[CppBackendActiveQ[OptionValue["Backend"]],
            Module[{cppHandle},
                cppHandle = Block[{$CppSoftFail = True}, Catch[CppDeferTakeDerivatives[setup, expr, derivativeList, OptionValue["Symmetries"]], $CppFallbackTag]];
                Return[
                    If[cppHandle =!= $CppFallbackMarker,
                        cppHandle
                        ,
                        (*rerun with the backend pinned, so internal calls cannot re-enter the C++ path*)
                        FTakeDerivatives[setup, expr, derivativeList, "Symmetries" -> OptionValue["Symmetries"], "Backend" -> "Mathematica"]
                    ]
                ];
            ];
        ];
        (*We take them in reverse order.*)
        derivativeListSIDX = derivativeList;
        (***)
        {result, annotations} = SeparateFExAnnotations[expr];
        (*First, fix the indices in the input equation, i.e. make everything have unique names*)
        result = FixIndices[setup, result];
        If[Length[derivativeListSIDX] === 0,
            Return[FResolveDerivatives[setup, result, "Symmetries" -> OptionValue["Symmetries"], "Backend" -> OptionValue["Backend"]]]
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
        result = FResolveDerivatives[setup, (FTerm @@ (FDOp /@ derivativeListSIDX)) ** (FEx @@ result), "Backend" -> OptionValue["Backend"]];
        (*Finally, reduce indices again to clean up any duplicates introduced by the derivatives*)
        Return[result];
    ];
