(**********************************************************************************
    GPU-Optimized C++ Code Generation Pipeline

    Multi-pass optimization for expressions targeting GPU execution via Kokkos/DiFfRG.
    Passes:
      1. Interpolator call hoisting (global memory reads)
      2. Reciprocal hoisting (division -> multiplication)
      3. Power chain combination
      4. DAG-based common subexpression elimination
      5. Algebraic factoring
      6. Register-pressure splitting (accumulator pattern)
**********************************************************************************)

(**********************************************************************************
    Pass 1: Interpolator Call Hoisting
    Extract ALL unique function calls matching $codeOptimizeInterps.
    These are global memory reads (~400 cycle latency) and must be computed once.
**********************************************************************************)

hoistInterpolators[expr_] :=
    Module[{interpPatterns, interpCalls, uniqueCalls, names, rules, newExpr},
        interpPatterns = $codeOptimizeInterps;
        interpCalls = Flatten @ Map[Cases[expr, #, Infinity]&, interpPatterns];
        uniqueCalls = DeleteDuplicates[interpCalls];
        If[Length[uniqueCalls] === 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
        (* Use strings as placeholders — CppForm quotes them, stripQuotedNames fixes it *)
        names = Table["_interp" <> ToString[i], {i, 1, Length[uniqueCalls]}];
        rules = Thread[uniqueCalls -> names];
        newExpr = expr //. rules;
        <|
            "Expr" -> newExpr,
            "Definitions" -> Table[{names[[i]], uniqueCalls[[i]]}, {i, 1, Length[uniqueCalls]}],
            "Count" -> Length[uniqueCalls]
        |>
    ];

(**********************************************************************************
    Pass 2: Reciprocal Hoisting
    Find Power[expr, n] where n < 0 and expr is non-trivial.
    Emit _inv_N = powr<-1>(base) once, rewrite Power[base, -k] -> Power[_inv_N, k].
**********************************************************************************)

hoistReciprocals[expr_] :=
    Module[{negativePowers, bases, uniqueBases, names, rules, newExpr},
        If[Not @ $codeHoistReciprocals,
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
        negativePowers = Cases[expr, Power[base_, n_Integer] /; n < 0 && Not @ NumericQ[base], Infinity];
        bases = DeleteDuplicates[#[[1]]& /@ negativePowers];
        (* Only hoist non-trivial bases *)
        uniqueBases = Select[bases, Not @ AtomQ[#]&];
        If[Length[uniqueBases] === 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
        names = Table["_inv" <> ToString[i], {i, 1, Length[uniqueBases]}];
        (* Replace Power[base, n] (n<0) with Power["_invN", -n] *)
        rules = Table[
            With[{base = uniqueBases[[idx]], name = names[[idx]]},
                Power[base, n_Integer] /; n < 0 :> Power[name, -n]
            ],
            {idx, 1, Length[uniqueBases]}
        ];
        newExpr = expr //. rules;
        <|
            "Expr" -> newExpr,
            "Definitions" -> Table[{names[[i]], Power[uniqueBases[[i]], -1]}, {i, 1, Length[uniqueBases]}],
            "Count" -> Length[uniqueBases]
        |>
    ];

(**********************************************************************************
    Pass 3: Power Chain Combination
    Within each Times term, combine Power[x, a] * Power[x, b] -> Power[x, a+b].
    Also treats bare factors x as Power[x, 1].
**********************************************************************************)

toPowerPair[Power[x_, n_]] := {x, n};
toPowerPair[x_] := {x, 1};

combinePowersInProduct[t_Times] :=
    Module[{factors, asPowers, grouped},
        factors = List @@ t;
        asPowers = toPowerPair /@ factors;
        grouped = GroupBy[asPowers, First -> Last, Total];
        (* Short-circuit if no bases appear more than once *)
        If[Length[grouped] === Length[factors], Return[t]];
        Times @@ KeyValueMap[Power, grouped]
    ];

combinePowersInProduct[x_] := x;

combinePowerChains[expr_] :=
    expr /. t_Times :> combinePowersInProduct[t];

(**********************************************************************************
    Pass 4: DAG-Based Common Subexpression Elimination
    Uses Experimental`OptimizeExpression with fallback to weighted frequency CSE.
**********************************************************************************)

dagCSE[expr_, remainingRegisters_Integer] :=
    Module[{result, vars, assignments, finalExpr, cleanExpr, stringRules, reverseStringRules, stringNames},
        If[remainingRegisters <= 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}|>]
        ];
        (* Collect all string placeholders in the expression and temporarily replace
           them with clean symbols so Experimental`OptimizeExpression can work properly *)
        stringNames = DeleteDuplicates @ Cases[expr, _String, Infinity];
        If[Length[stringNames] > 0,
            stringRules = Table[stringNames[[idx]] -> Symbol["placeholder" <> ToString[idx]], {idx, 1, Length[stringNames]}];
            reverseStringRules = Map[Reverse, stringRules];
            cleanExpr = expr //. stringRules;
            ,
            cleanExpr = expr;
            reverseStringRules = {};
        ];
        (* Try Experimental`OptimizeExpression with timeout *)
        result = Quiet @ TimeConstrained[
            Experimental`OptimizeExpression[cleanExpr],
            120,
            $Failed
        ];
        If[result =!= $Failed && Head[result] === Experimental`OptimizedExpression,
            Module[{block, setExprs, body, allAssignments, kept, dropped, inlineRules,
                    cseNames, symbolToString},
                block = result[[1]];
                If[Head[block] === Block,
                    vars = block[[1]];
                    body = block[[2]];
                    If[Head[body] === CompoundExpression,
                        setExprs = Most[List @@ body];
                        finalExpr = Last[List @@ body];
                        allAssignments = Cases[setExprs, HoldPattern[Set[var_, val_]] :> {var, val}];
                        (* Limit to remaining register budget *)
                        If[Length[allAssignments] > remainingRegisters,
                            kept = Take[allAssignments, remainingRegisters];
                            dropped = Drop[allAssignments, remainingRegisters];
                            inlineRules = Map[#[[1]] -> #[[2]]&, dropped];
                            finalExpr = finalExpr //. inlineRules;
                            kept = Map[{#[[1]], #[[2]] //. inlineRules}&, kept];
                            allAssignments = kept;
                        ];
                        (* Rename optimizer symbols to string names "_cseN" *)
                        cseNames = Table["_cse" <> ToString[idx], {idx, 1, Length[allAssignments]}];
                        symbolToString = Table[allAssignments[[idx, 1]] -> cseNames[[idx]], {idx, 1, Length[allAssignments]}];
                        finalExpr = finalExpr //. symbolToString //. reverseStringRules;
                        assignments = Table[
                            {cseNames[[idx]], allAssignments[[idx, 2]] //. symbolToString //. reverseStringRules},
                            {idx, 1, Length[allAssignments]}
                        ];
                        Return[<|"Expr" -> finalExpr, "Definitions" -> assignments|>]
                        ,
                        Return[<|"Expr" -> body //. reverseStringRules, "Definitions" -> {}|>]
                    ];
                    ,
                    Return[<|"Expr" -> block //. reverseStringRules, "Definitions" -> {}|>]
                ];
            ]
            ,
            (* Fallback: weighted frequency CSE (enhanced version of current algorithm) *)
            fallbackCSE[expr, remainingRegisters]
        ]
    ];

fallbackCSE[expr_, maxVars_Integer] :=
    Module[{optList, subexprs, replacementObj, replacementKeys, names, rules, rulesFS, newExpr, definitions},
        optList = $codeOptimizeFunctions;
        subexprs = Flatten @ Map[Cases[expr, #, Infinity]&, optList];
        replacementObj = Select[Counts[subexprs], # > 1&];
        replacementObj = TakeLargest[replacementObj, Min[maxVars, Length[replacementObj]]];
        replacementKeys = Keys[replacementObj];
        If[Length[replacementKeys] === 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}|>]
        ];
        (* Use strings as placeholders, same pattern as legacy code *)
        names = Table["_cse" <> ToString[i], {i, 1, Length[replacementKeys]}];
        rules = Table[replacementKeys[[i]] -> names[[i]], {i, 1, Length[replacementKeys]}];
        (* Parallelize FullSimplify across CSE candidates *)
        rulesFS = Thread[parallelSimplify[replacementKeys] -> names];
        newExpr = expr //. Reverse[rules] //. Reverse[rulesFS];
        definitions = Table[
            {names[[i]], replacementKeys[[i]] //. Reverse[rules[[ ;; i - 1]]] //. Reverse[rulesFS[[ ;; i - 1]]]},
            {i, 1, Length[replacementKeys]}
        ];
        <|"Expr" -> newExpr, "Definitions" -> definitions|>
    ];

(**********************************************************************************
    Pass 5: Algebraic Factoring
    Apply FactorTerms to group additive terms by common multiplicative factors.
**********************************************************************************)

algebraicFactor[expr_] :=
    Module[{result, prev},
        If[Not @ $codeFactorTerms,
            Return[expr]
        ];
        result = expr;
        Do[
            prev = result;
            result = FactorTerms[result];
            If[result === prev, Break[]];
            ,
            {3}
        ];
        result
    ];

(**********************************************************************************
    Pass 6: Register-Pressure Splitting (Accumulator Pattern)
    If the final expression has too many terms, split into scoped chunks.
**********************************************************************************)

splitForRegisters[definitions_, finalExpr_] :=
    Module[{terms, numTerms, chunkSize, chunks},
        If[Not @ $codeUseAccumulator,
            Return[<|"UseAccumulator" -> False, "Definitions" -> definitions, "Expr" -> finalExpr|>]
        ];
        (* Only split if finalExpr is a Plus with many terms *)
        If[Head[finalExpr] =!= Plus,
            Return[<|"UseAccumulator" -> False, "Definitions" -> definitions, "Expr" -> finalExpr|>]
        ];
        terms = List @@ finalExpr;
        numTerms = Length[terms];
        chunkSize = $codeMaxChunkSize;
        If[numTerms <= chunkSize,
            Return[<|"UseAccumulator" -> False, "Definitions" -> definitions, "Expr" -> finalExpr|>]
        ];
        (* Partition into chunks *)
        chunks = Partition[terms, UpTo[chunkSize]];
        <|
            "UseAccumulator" -> True,
            "Definitions" -> definitions,
            "Chunks" -> Map[Plus @@ #&, chunks]
        |>
    ];

(**********************************************************************************
    Pipeline Orchestrator
    Runs all passes in sequence and returns an Association with optimization results.
**********************************************************************************)

optimizeExpression[equation_] :=
    Module[{expr, interpResult, invResult, interpCount, invCount, remainingRegs,
            cseResult, allDefs, splitResult},

        FunKitDebug[1, "Starting optimization pipeline (level ", $codeOptimizationLevel, ")"];

        (* Level 0: legacy behavior *)
        If[$codeOptimizationLevel === 0,
            Return[legacyCppCode[equation]]
        ];

        expr = equation;

        (* Pass 1: Interpolator hoisting *)
        interpResult = hoistInterpolators[expr];
        expr = interpResult["Expr"];
        interpCount = interpResult["Count"];
        FunKitDebug[2, "Pass 1: Hoisted ", interpCount, " interpolator calls"];

        (* Pass 2: Reciprocal hoisting *)
        invResult = hoistReciprocals[expr];
        expr = invResult["Expr"];
        invCount = invResult["Count"];
        FunKitDebug[2, "Pass 2: Hoisted ", invCount, " reciprocals"];

        (* Pass 3: Power chain combination *)
        expr = combinePowerChains[expr];
        FunKitDebug[2, "Pass 3: Combined power chains"];

        remainingRegs = $availableRegisters - interpCount - invCount;

        (* Pass 4: CSE *)
        cseResult = dagCSE[expr, remainingRegs];
        expr = cseResult["Expr"];
        FunKitDebug[2, "Pass 4: CSE found ", Length[cseResult["Definitions"]], " subexpressions"];

        (* Pass 5: Algebraic factoring (level 2 only) *)
        If[$codeOptimizationLevel >= 2,
            If[Length[cseResult["Definitions"]] > 0,
                (* Factor the final expression and all CSE definitions in parallel *)
                Module[{allExprsToFactor, factoredExprs, cseDefs = cseResult["Definitions"]},
                    allExprsToFactor = Prepend[cseDefs[[All, 2]], expr];
                    factoredExprs =
                        If[Length[allExprsToFactor] >= $codeParallelThreshold && Length[Kernels[]] > 0,
                            ParallelMap[algebraicFactor, allExprsToFactor, DistributedContexts -> Automatic]
                            ,
                            Map[algebraicFactor, allExprsToFactor]
                        ];
                    expr = factoredExprs[[1]];
                    cseResult["Definitions"] = Table[
                        {cseDefs[[idx, 1]], factoredExprs[[idx + 1]]},
                        {idx, 1, Length[cseDefs]}
                    ];
                ];
                ,
                expr = algebraicFactor[expr];
            ];
            FunKitDebug[2, "Pass 5: Applied algebraic factoring"];
        ];

        (* Collect all definitions *)
        allDefs = Join[interpResult["Definitions"], invResult["Definitions"], cseResult["Definitions"]];

        (* Pass 6: Register-pressure splitting (level 2 only) *)
        If[$codeOptimizationLevel >= 2,
            splitResult = splitForRegisters[allDefs, expr];
            FunKitDebug[2, "Pass 6: Accumulator = ", splitResult["UseAccumulator"]];
            Return[splitResult]
        ];

        <|"UseAccumulator" -> False, "Definitions" -> allDefs, "Expr" -> expr|>
    ];

(**********************************************************************************
    Legacy code path ($codeOptimizationLevel = 0)
    Reproduces the original CppCode behavior exactly.
**********************************************************************************)

legacyCppCode[equation_] :=
    Module[{optList, interpObj, replacementObj, replacementNames, replacements, replacementsFS},
        optList = $codeOptimizeFunctions;
        interpObj = Flatten @ Map[Cases[equation, #, Infinity]&, optList];
        replacementObj = Select[Counts[interpObj], # > 1&];
        replacementObj =
            AssociationMap[
                If[MatchQ[#[[1]], $codeOptimizeInterps[[1]]],
                    #[[1]] -> #[[2]] * 1000
                    ,
                    #[[1]] -> #[[2]]
                ]&
                ,
                replacementObj
            ];
        replacementObj = TakeLargest[replacementObj, Min[$availableRegisters, Length[replacementObj]]];
        replacementObj = Keys[replacementObj];
        replacementNames = Table["_repl" <> ToString[i], {i, 1, Length[replacementObj]}];
        replacements = Table[replacementObj[[i]] -> replacementNames[[i]], {i, 1, Length[replacementObj]}];
        (* Parallelize FullSimplify across replacement candidates *)
        replacementsFS =
            If[Length[replacementObj] > 0,
                Thread[parallelSimplify[replacementObj] -> replacementNames]
                ,
                {}
            ];
        <|
            "UseAccumulator" -> False,
            "Definitions" -> Table[{replacementNames[[i]], replacementObj[[i]] //. Reverse[replacements[[ ;; i - 1]]] //. Reverse[replacementsFS[[ ;; i - 1]]]}, {i, 1, Length[replacementObj]}],
            "Expr" -> (equation //. Reverse[replacements] //. Reverse[replacementsFS])
        |>
    ];

(**********************************************************************************
    Formatting: Convert optimized result to C++ code string
**********************************************************************************)

formatDefinitions[defs_] :=
    Module[{simplifiedExprs, result = ""},
        If[Length[defs] === 0, Return[""]];
        (* FullSimplify is the dominant cost — parallelize it across definitions *)
        simplifiedExprs = parallelSimplify[defs[[All, 2]]];
        result = StringJoin @ Table[
            "const auto " <> defs[[i, 1]] <> " = " <> CppForm[simplifiedExprs[[i]]] <> ";\n",
            {i, 1, Length[defs]}
        ];
        result <> "\n"
    ];

formatReturnStatement[expr_] :=
    " return " <> CppForm[expr] <> ";";

stripQuotedNames[code_String, names_List] :=
    StringReplace[code, Map["\"" <> # <> "\"" -> #&, names]];

getAllVarNames[optimized_] :=
    Map[#[[1]]&, optimized["Definitions"]];
