(**********************************************************************************
    CppOptimize.m -- Multi-pass optimization pipeline for numerical expressions

    Internal (all used by CppCode/JuliaCode via optimizeExpression):
      optimizeExpression         -- Pipeline orchestrator: runs all passes
      hoistInterpolators         -- Pass 1: extracts global interpolator calls
      earlySplit                 -- Pass 2: decides single vs multi-kernel split
      dagCSE                     -- Pass 3: DAG-based common subexpression elimination
      fallbackCSE                -- Pass 3 fallback: weighted-frequency CSE
      normalizePowerBases        -- Pass 4: rewrites powers via existing temporaries
      algebraicFactor            -- Pass 5: applies FactorTerms iteratively
      hoistTranscendentals       -- Pass 6: hoists expensive transcendental calls
      buildFMAChain              -- Pass 7 helper: chains FMA patterns from sum terms
      fmaRestructure             -- Pass 7: restructures a*b+c into fma() calls
      extractSummationCore       -- Extracts Plus core from Times[pref, Plus[...]]
      optimizeSubKernel          -- Runs per-kernel optimization passes
      splitIntoSubKernels        -- Post-optimization sub-kernel splitting
      formatDefinitions          -- Formats defs as C++ const auto declarations
                                    (used by Cpp, Julia)
      formatReturnStatement      -- Formats a return statement
                                    (used by Cpp)
      stripQuotedNames           -- Removes quotes around CSE variable names
                                    (used by Cpp, Julia)
      getAllVarNames             -- Extracts all variable names from optimized result
                                    (used by Cpp, Julia)

    Pipeline overview:
      GLOBAL: interpolator hoisting -> early split decision
      PER-KERNEL: CSE -> power chains -> factoring -> transcendentals -> FMA
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
        <|"Expr" -> newExpr, "Definitions" -> Table[{names[[i]], uniqueCalls[[i]]}, {i, 1, Length[uniqueCalls]}], "Count" -> Length[uniqueCalls]|>
    ];

(**********************************************************************************
    Pass 1b: Composite-Denominator Hoisting
    Extract every distinct negative-integer power of a composite (Plus/Times) base
    -- the regulated-propagator denominators 1/(M^2 + q*(...)^2) etc. These are NOT
    interpolator leaves, so without global hoisting they fall to the per-sub-kernel
    CSE (Pass 3) and are recomputed once per chunk when the term-sum is split.
    Hoisting them to shared "_den" defs (like interpolators) computes each once.

    Bases are sorted by LeafCount ascending so a denominator nested inside another
    gets a lower index; each def body then substitutes the already-assigned inner
    "_denK" (rules without its own), making the def list topologically ordered.
    Runs AFTER interpolator hoisting, so bases already carry "_interp" placeholders.
**********************************************************************************)

hoistDivisions[expr_] :=
    Module[{cands, uniqueDens, names, rules, newExpr, defs},
        If[Not @ TrueQ[$codeHoistDivisions],
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
        (* composite reciprocals: Power[base, n<0] with base a Plus or Times (a shared,
           non-trivial denominator), not a bare symbol/number/placeholder *)
        cands = Cases[expr,
            Power[b_, n_Integer] /; n < 0 && (Head[b] === Plus || Head[b] === Times),
            Infinity];
        (* inner (smaller) denominators first -> def list is dependency-ordered *)
        uniqueDens = SortBy[DeleteDuplicates[cands], LeafCount];
        If[Length[uniqueDens] === 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
        names = Table["_den" <> ToString[i], {i, 1, Length[uniqueDens]}];
        rules = Thread[uniqueDens -> names];
        newExpr = expr //. rules;
        (* def body for _denI: replace NESTED denominators (all rules except its own,
           which would otherwise collapse the whole body to its own name) *)
        defs = Table[{names[[i]], uniqueDens[[i]] //. Drop[rules, {i}]}, {i, 1, Length[uniqueDens]}];
        <|"Expr" -> newExpr, "Definitions" -> defs, "Count" -> Length[uniqueDens]|>
    ];

(**********************************************************************************
    Power Basis Normalization
    After CSE, rewrite Power[base, m] in terms of an already-hoisted Power[base, n]
    temporary when m is a nonzero integer multiple of n.  This prevents independent
    hoisting of e.g. powr<2>(l1) and powr<-4>(l1) when the latter is just
    powr<-2>(_cse1).
**********************************************************************************)

normalizePowerBases[expr_, defs_] :=
    Module[
        {powerDefs, grouped, rules, newExpr, newDefs}
        ,
        (* Collect CSE defs that are pure integer powers of a non-string base *)
        powerDefs = Cases[defs, {name_, Power[base_ /; Not[StringQ[base]], n_Integer] /; Abs[n] > 1} :> {name, base, n}];
        If[Length[powerDefs] === 0,
            Return[{expr, defs}]
        ];
        (* Group by base: base -> list of {name, exponent} *)
        grouped = GroupBy[powerDefs, #[[2]]& -> ({#[[1]], #[[3]]}&)];
(* For each base build a rule: Power[base, m] -> Power[repName, m/repN]
   Representative = entry with smallest Abs[n]; tie broken by positive exponent first *)
        rules =
            Flatten @
                KeyValueMap[
                    Function[{base, entries},
                        Module[{rep, repName, repN},
                            rep = First @ SortBy[entries, {Abs[#[[2]]]&, -Sign[#[[2]]]&}];
                            repName = rep[[1]];
                            repN = rep[[2]];
                            (* Inject concrete base, repN, repName so the rule matches literally *)
                            With[{b = base, n = repN, r = repName},
                                HoldPattern[Power[b, m_Integer]] /; m =!= n && Divisible[m, n] :> Power[r, m / n]
                            ]
                        ]
                    ]
                    ,
                    grouped
                ];
        newExpr = expr //. rules;
        newDefs = Map[{#[[1]], #[[2]] //. rules}&, defs];
        {newExpr, newDefs}
    ];

(**********************************************************************************
    Recompute-cost estimate for a CSE candidate (register-pressure-aware CSE).
    Atoms (numbers, symbols, string placeholders) are free; +,* cost 1 per extra arg;
    a small positive integer power (powr<n>, n<=8) costs 1; negative/fractional powers
    (division, pow()) and any other call (sqrt/exp/log/interpolators) cost ~8.
    A candidate is "cheap to rematerialize" when its cost <= $cseCostThreshold, in which
    case it is left inline rather than spending a long-lived register on it.
**********************************************************************************)

cseRematCost[e_] /; NumericQ[e] := 0;
cseRematCost[_Symbol] := 0;
cseRematCost[_String] := 0;
cseRematCost[Power[b_, n_Integer]] := cseRematCost[b] + If[n > 0, If[n <= 8, 1, 2], 8];
cseRematCost[Power[b_, _]] := cseRematCost[b] + 8;
cseRematCost[Times[args__]] := Total[cseRematCost /@ {args}] + (Length[{args}] - 1);
cseRematCost[Plus[args__]] := Total[cseRematCost /@ {args}] + (Length[{args}] - 1);
cseRematCost[_[args__]] := 8 + Total[cseRematCost /@ {args}];
cseRematCost[_] := 8;

cheapToRemat[e_] := TrueQ[$codeCSECostFilter] && cseRematCost[e] <= $cseCostThreshold;

(**********************************************************************************
    DAG-Based Common Subexpression Elimination
    Uses Experimental`OptimizeExpression with fallback to weighted frequency CSE.
**********************************************************************************)

dagCSE[expr_, remainingRegisters_Integer] :=
    Module[{result, assignments, finalExpr, cleanExpr, stringRules, reverseStringRules, stringNames},
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
        result = cgTimed[$ProfileCgOptExpr, Quiet @ TimeConstrained[Experimental`OptimizeExpression[cleanExpr], 120, $Failed]];
        If[result =!= $Failed && Head[result] === Experimental`OptimizedExpression,
            Module[
                {heldBlock, numParts, allAssignments, kept, dropped, inlineRules, cseNames, symbolToString}
                ,
                (* Extract with Hold to prevent Block from evaluating *)
                heldBlock = Extract[result, {1}, Hold];
                If[MatchQ[
                    heldBlock
                    ,
                    Hold[
                        Block[_,
                            _CompoundExpression
                        ]
                    ]
                ],
(* Count CompoundExpression parts without evaluating them.
   Replace wraps the matched sequence in Hold, then Length
   counts Hold's arguments — all without triggering evaluation. *)
                    numParts =
                        Replace[
                            heldBlock
                            ,
                            Hold[
                                    Block[_,
                                        CompoundExpression[args___]
                                    ]
                                ] :> Length[Hold[args]]
                        ];
(* Extract Set expressions (all but last in CompoundExpression).
   Extract with Hold wrapper prevents each Set from evaluating. *)
                    allAssignments = Table[Extract[heldBlock, {1, 2, i}, Hold] /. Hold[Set[var_, val_]] :> {var, val}, {i, 1, numParts - 1}];
(* Extract final expression via Hold then release — the symbolic
   expression doesn't have side effects, so ReleaseHold is safe *)
                    finalExpr = ReleaseHold @ Extract[heldBlock, {1, 2, numParts}, Hold];
(* Register-pressure-aware rematerialization: drop CSE temporaries that are cheap to
   recompute (small positive powers, a single add or multiply) so they are not held in long-lived
   registers across a large basic block. Inline the dropped defs back into the remaining
   defs and the final expression (//. resolves chains of dropped-on-dropped deps). *)
                    If[TrueQ[$codeCSECostFilter] && Length[allAssignments] > 0,
                        Module[{cheapMask, cheapDefs, keepDefs, rematRules},
                            cheapMask = Map[cheapToRemat[#[[2]]]&, allAssignments];
                            cheapDefs = Pick[allAssignments, cheapMask, True];
                            keepDefs = Pick[allAssignments, cheapMask, False];
                            If[Length[cheapDefs] > 0,
                                rematRules = Map[#[[1]] -> #[[2]]&, cheapDefs];
                                finalExpr = finalExpr //. rematRules;
                                allAssignments = Map[{#[[1]], #[[2]] //. rematRules}&, keepDefs];
                            ];
                        ];
                    ];
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
                    assignments = Table[{cseNames[[idx]], allAssignments[[idx, 2]] //. symbolToString //. reverseStringRules}, {idx, 1, Length[allAssignments]}];
                    Return[<|"Expr" -> finalExpr, "Definitions" -> assignments|>]
                    ,
                    (* Not the expected structure — try fallback *)
                    fallbackCSE[expr, remainingRegisters]
                ]
            ]
            ,
            (* Fallback: weighted frequency CSE (enhanced version of current algorithm) *)
            fallbackCSE[expr, remainingRegisters]
        ]
    ];

fallbackCSE[expr_, maxVars_Integer] :=
    cgTimed[$ProfileCgFallbackCSE,
    Module[{optList, subexprs, replacementObj, replacementKeys, names, rules, rulesFS, newExpr, definitions},
        optList = $codeOptimizeFunctions;
        subexprs = Flatten @ Map[Cases[expr, #, Infinity]&, optList];
        replacementObj = Select[Counts[subexprs], # > 1&];
        (* Register-pressure-aware: drop cheap-to-recompute candidates (rematerialize). *)
        If[TrueQ[$codeCSECostFilter],
            replacementObj = KeySelect[replacementObj, Not @ cheapToRemat[#]&];
        ];
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
        definitions = Table[{names[[i]], replacementKeys[[i]] //. Reverse[rules[[ ;; i - 1]]] //. Reverse[rulesFS[[ ;; i - 1]]]}, {i, 1, Length[replacementKeys]}];
        <|"Expr" -> newExpr, "Definitions" -> definitions|>
    ]
    ];

(**********************************************************************************
    Algebraic Factoring
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
            If[result === prev,
                Break[]
            ];
            ,
            {3}
        ];
        result
    ];

(**********************************************************************************
    FMA Pattern Restructuring
    Restructure a*b + c patterns into explicit fma(a, b, c) calls.
    GPU FMA units execute this as a single instruction with better precision.
**********************************************************************************)

buildFMAChain[{}] :=
    0;

buildFMAChain[{x_}] :=
    x;

buildFMAChain[terms_List] :=
    Module[{mulTerms, nonMulTerms, result},
        mulTerms = Select[terms, MatchQ[#, _Times]&];
        nonMulTerms = Select[terms, Not @ MatchQ[#, _Times]&];
        If[Length[mulTerms] === 0,
            Return[Plus @@ terms]
        ];
        (* Build FMA chain: pair each Times term with remaining sum *)
        result =
            If[Length[nonMulTerms] > 0,
                Plus @@ nonMulTerms
                ,
                0
            ];
        (* Chain from last to first for maximum throughput *)
        Do[
            Module[{factors, a, b},
                factors = List @@ mulTerms[[i]];
                a = First[factors];
                b = Times @@ Rest[factors];
                result = fmaGroup[a, b, result];
            ]
            ,
            {i, Length[mulTerms], 1, -1}
        ];
        result
    ];

fmaRestructure[expr_] :=
    If[Not @ $codeFMARestructure,
        expr
        ,
        expr /. p_Plus :> buildFMAChain[List @@ p]
    ];

(**********************************************************************************
    Transcendental Hoisting
    Hoist expensive transcendental calls to temporaries.
    On GPU, these cost 20-100 cycles vs 4 for multiply.
    Accepts maxVars parameter to limit number of hoisted variables.
**********************************************************************************)

hoistTranscendentals[expr_, maxVars_Integer] :=
    Module[{transcCalls, uniqueCalls, names, rules, newExpr, expBases, expPairs, extraDefs},
        If[maxVars <= 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
(* Find all transcendental calls with non-trivial arguments.
   Note: Exp[x] evaluates to Power[E, x] in Mathematica, so we must match that form too. *)
        transcCalls = Join[Cases[expr, (h : Exp | cppExp | Log | Sin | Cos | Tan | Sqrt | Tanh | Sinh | Cosh)[a_] /; !AtomQ[a], Infinity], Cases[expr, Power[E, a_] /; !AtomQ[a], Infinity]];
        uniqueCalls = DeleteDuplicates[transcCalls];
        If[Length[uniqueCalls] === 0,
            Return[<|"Expr" -> expr, "Definitions" -> {}, "Count" -> 0|>]
        ];
        (* Detect Exp[x] / Exp[-x] pairs for reciprocal reuse *)
        expBases = Join[Cases[uniqueCalls, (Exp | cppExp)[a_] :> a], Cases[uniqueCalls, Power[E, a_] :> a]];
        expPairs = {};
        extraDefs = {};
        Do[
            If[MemberQ[expBases, -base],
                (* Both Exp[x] and Exp[-x] exist — we'll hoist Exp[x] and derive Exp[-x] via reciprocal *)
                AppendTo[expPairs, base];
            ]
            ,
            {base, expBases}
        ];
        (* Remove Exp[-x] from uniqueCalls if Exp[x] exists, to avoid double-hoisting *)
        uniqueCalls = Select[uniqueCalls, Not[MatchQ[#, (Exp | cppExp)[a_] /; MemberQ[expPairs, -a]] || MatchQ[#, Power[E, a_] /; MemberQ[expPairs, -a]]]&];
        (* Sort by frequency (descending) and limit to maxVars *)
        If[Length[uniqueCalls] > maxVars,
            Module[{freqs},
                freqs = Map[Count[expr, #, Infinity]&, uniqueCalls];
                uniqueCalls = Take[uniqueCalls[[Ordering[freqs, All, GreaterEqual]]], maxVars];
            ];
        ];
        names = Table["_tran" <> ToString[i], {i, 1, Length[uniqueCalls]}];
        rules = Thread[uniqueCalls -> names];
        newExpr = expr //. rules;
        (* For Exp[-x] where Exp[x] was hoisted, replace with powr<-1>(hoisted) *)
        Do[
            Module[{negCall, posCall, posName, negRule},
                posCall = Select[uniqueCalls, MatchQ[#, (Exp | cppExp)[b_] /; b === base]&];
                If[Length[posCall] > 0,
                    posName = posCall[[1]] /. rules;
                    negCall = (Head[posCall[[1]]])[-base];
                    negRule = negCall -> Power[posName, -1];
                    newExpr = newExpr //. negRule;
                ];
            ]
            ,
            {base, DeleteDuplicates[expPairs]}
        ];
        <|"Expr" -> newExpr, "Definitions" -> Table[{names[[i]], uniqueCalls[[i]]}, {i, 1, Length[uniqueCalls]}], "Count" -> Length[uniqueCalls]|>
    ];

(* Backward-compatible no-limit version *)

hoistTranscendentals[expr_] :=
    hoistTranscendentals[expr, Infinity];

(**********************************************************************************
    Early Splitting
    Decides whether to split into sub-kernels BEFORE running per-kernel optimization.
    Only receives interpDefs as global defs; splits the raw expression into chunks.
    Partitions interps into shared (referenced by 2+ kernels) and local (1 kernel).
**********************************************************************************)

(* Extract the summation core from an expression.
   If expr = prefactor * Plus[terms...], returns {prefactor, {term1, term2, ...}}.
   If expr = Plus[terms...], returns {1, {term1, term2, ...}}.
   When multiple Plus factors exist in a Times, selects the largest by LeafCount.
   Otherwise returns {1, {expr}} (single-term, no splitting possible). *)

extractSummationCore[expr_] :=
    Module[
        {innerPlus, prefactors}
        ,
        (* Direct Plus at top level *)
        If[Head[expr] === Plus,
            Return[{1, List @@ expr}]
        ];
        (* Times[prefactors..., Plus[terms...]] — find the largest Plus factor *)
        If[Head[expr] === Times,
            Module[{factors = List @@ expr, plusPositions, bestPos, bestLeafCount},
                plusPositions = Flatten @ Position[factors, _Plus, {1}];
                If[Length[plusPositions] > 0,
                    (* Pick the Plus with the largest LeafCount *)
                    bestPos = First @ MaximalBy[plusPositions, LeafCount[factors[[#]]]&];
                    innerPlus = factors[[bestPos]];
                    prefactors = Delete[factors, bestPos];
                    Return[{Times @@ prefactors, List @@ innerPlus}]
                ];
            ];
        ];
        (* No Plus found — single term *)
        {1, {expr}}
    ];

(* Recursively refine a single term for KernelSplitLevel > 1: split the term's largest
   nested Plus in half, `level`-1 times. Returns a list of sub-terms whose sum equals the
   input term (algebraic identity: pf*(a+b) == pf*a + pf*b). Terms with no nested Plus are
   returned unchanged. This turns a single heavy "prefactor * big-polynomial * propagators"
   term into several smaller terms the partitioner can place in separate sub-kernels. *)

splitDeep[term_, level_Integer] :=
    If[level <= 1,
        {term}
        ,
        Module[{pf, subterms, half},
            {pf, subterms} = extractSummationCore[term];
            If[Length[subterms] <= 1,
                {term}
                ,
                half = Ceiling[Length[subterms] / 2];
                Join[
                    splitDeep[pf * (Plus @@ Take[subterms, half]), level - 1],
                    splitDeep[pf * (Plus @@ Drop[subterms, half]), level - 1]
                ]
            ]
        ]
    ];

earlySplit[interpDefs_, expr_] :=
    Module[
        {prefactor, terms, numTerms, exprComplexity, totalWeight, chunkSize, chunks, subKernelData, interpNames, interpsByName}
        ,
        (* Extract the summation core, handling Times[prefactor, Plus[...]] *)
        {prefactor, terms} = extractSummationCore[expr];
        (* KernelSplitLevel > 1: refine heavy terms by splitting their nested sums, so a
           single big nested term becomes several smaller ones for the partitioner. *)
        If[$kernelSplitLevel > 1,
            terms = Flatten[splitDeep[#, $kernelSplitLevel]& /@ terms]
        ];
        numTerms = Length[terms];
        exprComplexity = Total[LeafCount /@ terms];
        totalWeight = Length[interpDefs] + exprComplexity;
        chunkSize = $codeMaxKernelTerms;
        FunKitDebug[2, "Early split check: ", numTerms, " terms (split level ", $kernelSplitLevel, "), LeafCount ", exprComplexity, ", ", Length[interpDefs], " interp defs, total weight ", totalWeight, " vs threshold ", chunkSize];
        (* Below threshold or cannot split — single kernel *)
        If[(totalWeight <= chunkSize && numTerms <= chunkSize) || numTerms <= 1,
            Return[<|"Split" -> False, "Expr" -> expr, "SharedDefs" -> interpDefs|>]
        ];
        (* Determine per-chunk size *)
        Module[{targetChunks},
            targetChunks = Max[2, Ceiling[totalWeight / chunkSize]];
            chunkSize = Ceiling[numTerms / targetChunks];
        ];
        chunks = Partition[terms, UpTo[chunkSize]];
        (* Build chunk expressions with prefactor *)
        chunks =
            Map[
                If[prefactor =!= 1,
                    prefactor * Plus @@ #
                    ,
                    Plus @@ #
                ]&
                ,
                chunks
            ];
        (* Partition interp defs: shared (used by 2+ chunks) vs local (1 chunk) *)
        interpNames = #[[1]]& /@ interpDefs;
        interpsByName = Association @ Table[interpDefs[[i, 1]] -> interpDefs[[i]], {i, Length[interpDefs]}];
        Module[
            {chunkRefs, useCounts, sharedNames, sharedDefs, defsByName}
            ,
            (* name -> definition, for transitive reference expansion *)
            defsByName = Association @ Table[interpDefs[[i, 1]] -> interpDefs[[i]], {i, Length[interpDefs]}];
            (* Find which def names each chunk references -- TRANSITIVELY: a chunk that
               references _den5 also "uses" every _interp/_den that _den5's body reads,
               so a denominator shared across chunks (and its inputs) is correctly counted
               as shared rather than dropped or recomputed per chunk. *)
            chunkRefs =
                Map[
                    Function[{chunk},
                        Module[{refs, prevLen = -1},
                            (* level {0, Infinity}: a chunk (or def body) that IS a bare
                               placeholder string -- e.g. a term that is a single hoisted
                               reciprocal _denK with no other factors -- sits at level 0 and
                               would be missed by the default Infinity (= {1, Infinity}). *)
                            refs = Intersection[interpNames, DeleteDuplicates @ Cases[chunk, _String, {0, Infinity}]];
                            While[Length[refs] =!= prevLen,
                                prevLen = Length[refs];
                                refs = DeleteDuplicates @ Join[refs,
                                    Intersection[interpNames,
                                        Flatten @ Map[Cases[defsByName[#][[2]], _String, {0, Infinity}]&, refs]]];
                            ];
                            refs
                        ]
                    ],
                    chunks
                ];
            (* Count how many chunks reference each interp/den *)
            useCounts = Counts[Flatten[chunkRefs]];
            sharedNames = Keys @ Select[useCounts, # > 1&];
            sharedDefs = Select[interpDefs, MemberQ[sharedNames, #[[1]]]&];
            (* Build sub-kernel data with local interps *)
            subKernelData =
                Table[
                    Module[{localInterpNames, localInterps},
                        localInterpNames = Select[chunkRefs[[i]], Not @ MemberQ[sharedNames, #]&];
                        localInterps = Select[interpDefs, MemberQ[localInterpNames, #[[1]]]&];
                        <|"Expr" -> chunks[[i]], "InterpDefs" -> localInterps|>
                    ]
                    ,
                    {i, Length[chunks]}
                ];
            <|"Split" -> True, "SharedDefs" -> sharedDefs, "SubKernels" -> subKernelData|>
        ]
    ];

(**********************************************************************************
    Per-Kernel Optimization
    Runs CSE, power chains, factoring, transcendental hoisting, and FMA
    independently for a single sub-kernel expression with its own register budget.
**********************************************************************************)

optimizeSubKernel[expr_, sharedDefCount_Integer] :=
    Module[{e, perKernelBudget, cseResult, cseDefs, remainingBudget, tranResult, allDefs},
        perKernelBudget = Max[0, $availableRegisters - sharedDefCount];
        FunKitDebug[2, "  Per-kernel budget: ", perKernelBudget, " (registers=", $availableRegisters, ", shared=", sharedDefCount, ")"];
        e = expr;
        (* CSE with per-kernel budget *)
        cseResult = cgTimed[$ProfileCgCSE, dagCSE[e, perKernelBudget]];
        e = cseResult["Expr"];
        cseDefs = cseResult["Definitions"];
        FunKitDebug[2, "  CSE found ", Length[cseDefs], " subexpressions"];
(* Normalize power bases: rewrite Power[base, m] using an already-hoisted
   Power[base, n] temporary when m is a nonzero integer multiple of n *)
        {e, cseDefs} = cgTimed[$ProfileCgPowerNorm, normalizePowerBases[e, cseDefs]];
(* Topological sort: normalizePowerBases can introduce forward references
   (e.g. _cse6 = 1/_cse7 when _cse7 = l1^2) by rewriting powers in terms
   of later-defined temporaries.  Re-sort so each def comes after its deps. *)
        If[Length[cseDefs] > 1,
            cseDefs = Module[{names, nameSet, deps, visited, order, visit},
                names = cseDefs[[All, 1]];
                nameSet = Association @ Table[names[[i]] -> i, {i, Length[names]}];
                deps = Map[
                    Function[{def}, Select[Cases[def[[2]], _String, Infinity], KeyExistsQ[nameSet, #]&]],
                    cseDefs
                ];
                visited = <||>;
                order = {};
                visit[idx_] := (
                    If[Lookup[visited, idx, False], Return[]];
                    visited[idx] = True;
                    Scan[Function[{dep}, visit[nameSet[dep]]], deps[[idx]]];
                    AppendTo[order, idx];
                );
                Do[visit[i], {i, 1, Length[cseDefs]}];
                cseDefs[[order]]
            ];
        ];
        FunKitDebug[2, "  Power basis normalization done"];
        (* Algebraic factoring *)
        cgTimed[$ProfileCgFactor,
            If[Length[cseDefs] > 0,
                Module[{allExprsToFactor, factoredExprs},
                    allExprsToFactor = Prepend[cseDefs[[All, 2]], e];
                    factoredExprs =
                        If[Length[allExprsToFactor] >= $codeParallelThreshold && Length[Kernels[]] > 0,
                            ParallelMap[algebraicFactor, allExprsToFactor, DistributedContexts -> Automatic]
                            ,
                            Map[algebraicFactor, allExprsToFactor]
                        ];
                    e = factoredExprs[[1]];
                    cseDefs = Table[{cseDefs[[idx, 1]], factoredExprs[[idx + 1]]}, {idx, 1, Length[cseDefs]}];
                ];
                ,
                e = algebraicFactor[e];
            ]
        ];
        (* Transcendental hoisting with remaining budget *)
        remainingBudget = Max[0, perKernelBudget - Length[cseDefs]];
        tranResult = cgTimed[$ProfileCgTranscendental, hoistTranscendentals[e, remainingBudget]];
        e = tranResult["Expr"];
        FunKitDebug[2, "  Transcendental hoisting: ", tranResult["Count"], " (budget was ", remainingBudget, ")"];
        (* FMA restructuring *)
        e = cgTimed[$ProfileCgFMA, fmaRestructure[e]];
        allDefs = Join[cseDefs, tranResult["Definitions"]];
        If[Length[allDefs] > 0,
            allDefs = cgTimed[$ProfileCgFMA, Map[{#[[1]], fmaRestructure[#[[2]]]}&, allDefs]];
        ];
        <|"Expr" -> e, "Definitions" -> allDefs|>
    ];

(**********************************************************************************
    Legacy Sub-Kernel Splitting (post-optimization)
    Used by the single-kernel path to split into sub-kernels after optimization,
    when the combined defs + expression exceed $codeMaxKernelTerms.
**********************************************************************************)

splitIntoSubKernels[allDefs_, finalExpr_] :=
    Module[
        {terms, numTerms, chunkSize, chunks, subKernels, sharedDefs, defNames, defsByName, prefactor, exprComplexity, totalWeight}
        ,
        (* Extract the summation core, handling Times[prefactor, Plus[...]] *)
        {prefactor, terms} = extractSummationCore[finalExpr];
        numTerms = Length[terms];
        exprComplexity = Total[LeafCount /@ terms];
        totalWeight = Length[allDefs] + exprComplexity;
        chunkSize = $codeMaxKernelTerms;
        FunKitDebug[2, "Sub-kernel check: ", numTerms, " terms, LeafCount ", exprComplexity, ", ", Length[allDefs], " defs, total weight ", totalWeight, " vs threshold ", chunkSize];
        If[(totalWeight <= chunkSize && numTerms <= chunkSize) || numTerms <= 1,
            (* Below threshold or single term — no split needed *)
            Return[<|"UseSubKernels" -> False, "Definitions" -> allDefs, "Expr" -> finalExpr|>]
        ];
(* Determine per-chunk size: aim for chunks where each chunk's
   LeafCount stays under the budget *)
        Module[{targetChunks},
            targetChunks = Max[2, Ceiling[totalWeight / chunkSize]];
            chunkSize = Ceiling[numTerms / targetChunks];
        ];
        (* Partition into sub-kernel chunks *)
        chunks = Partition[terms, UpTo[chunkSize]];
        (* Build name -> definition mapping *)
        defNames = #[[1]]& /@ allDefs;
        defsByName = Association @ Table[allDefs[[i, 1]] -> allDefs[[i]], {i, Length[allDefs]}];
        (* For each chunk, find which definitions it references *)
        subKernels =
            Table[
                Module[{chunkExpr, referencedNames, relevantDefs},
                    chunkExpr = Plus @@ chunk;
                    (* Multiply by prefactor if present *)
                    If[prefactor =!= 1,
                        chunkExpr = prefactor * chunkExpr;
                    ];
                    referencedNames = Intersection[defNames, DeleteDuplicates @ Cases[chunkExpr, _String, Infinity]];
                    (* Also include definitions referenced by other definitions (transitive) *)
                    Module[{prevLen = 0},
                        While[
                            Length[referencedNames] =!= prevLen
                            ,
                            prevLen = Length[referencedNames];
                            referencedNames = DeleteDuplicates @ Join[referencedNames, Intersection[defNames, Flatten @ Map[Cases[defsByName[#][[2]], _String, Infinity]&, referencedNames]]];
                        ];
                    ];
                    relevantDefs = Select[allDefs, MemberQ[referencedNames, #[[1]]]&];
                    <|"Terms" -> chunkExpr, "Definitions" -> relevantDefs|>
                ]
                ,
                {chunk, chunks}
            ];
        (* Find definitions used by multiple sub-kernels — these are "shared" *)
        Module[{allUsed, useCounts, sharedNames},
            allUsed = Flatten @ Map[#["Definitions"][[All, 1]]&, subKernels];
            useCounts = Counts[allUsed];
            sharedNames = Keys @ Select[useCounts, # > 1&];
            sharedDefs = Select[allDefs, MemberQ[sharedNames, #[[1]]]&];
            (* Remove shared defs from per-kernel defs *)
            subKernels = Map[<|"Terms" -> #["Terms"], "Definitions" -> Select[#["Definitions"], Not @ MemberQ[sharedNames, #[[1]]]&]|>&, subKernels];
        ];
        <|"UseSubKernels" -> True, "SharedDefinitions" -> sharedDefs, "SubKernels" -> subKernels|>
    ];

(**********************************************************************************
    Pipeline Orchestrator
    Runs all passes and returns an Association with optimization results.

    GLOBAL: interpolator hoisting → early split decision
    PER-KERNEL: CSE → power chains → factoring → transcendentals → FMA
**********************************************************************************)

optimizeExpression[equation_] :=
    Module[{expr, interpResult, interpCount, divResult, divCount, globalDefs, splitResult, sharedDefs, subKernels, result, allDefs},
        FunKitDebug[1, "Starting optimization pipeline (optimize = ", $codeOptimize, ")"];
        (* If optimization is disabled, return raw expression with no passes *)
        If[!TrueQ[$codeOptimize],
            Return[<|"Definitions" -> {}, "Expr" -> equation, "UseSubKernels" -> False|>]
        ];
        expr = equation;
        (* === GLOBAL PASSES === *)
        (* Pass 1: Interpolator hoisting — extract global memory reads *)
        interpResult = cgTimed[$ProfileCgHoist, hoistInterpolators[expr]];
        expr = interpResult["Expr"];
        interpCount = interpResult["Count"];
        FunKitDebug[2, "Hoisted ", interpCount, " interpolator calls"];
        (* Pass 1b: Composite-denominator hoisting — share reciprocals across chunks *)
        divResult = cgTimed[$ProfileCgHoist, hoistDivisions[expr]];
        expr = divResult["Expr"];
        divCount = divResult["Count"];
        FunKitDebug[2, "Hoisted ", divCount, " composite denominators"];
        (* global defs: interps FIRST, then denominators (which reference interps) *)
        globalDefs = Join[interpResult["Definitions"], divResult["Definitions"]];
        (* Pass 2: Early split decision *)
        splitResult = cgTimed[$ProfileCgSplit, earlySplit[globalDefs, expr]];
        FunKitDebug[2, "Early split: ", splitResult["Split"]];
        (* === PER-KERNEL PASSES === *)
        If[TrueQ[splitResult["Split"]],
            (* Multi-kernel path: optimize each sub-kernel independently *)
            sharedDefs = splitResult["SharedDefs"];
            subKernels =
                Table[
                    Module[{kernelResult, localInterpDefs, totalShared},
                        localInterpDefs = splitResult["SubKernels"][[i]]["InterpDefs"];
                        totalShared = Length[sharedDefs] + Length[localInterpDefs];
                        FunKitDebug[2, "Optimizing sub-kernel ", i, " of ", Length[splitResult["SubKernels"]], " (shared=", Length[sharedDefs], ", localInterps=", Length[localInterpDefs], ")"];
                        kernelResult = optimizeSubKernel[splitResult["SubKernels"][[i]]["Expr"], totalShared];
                        <|"Terms" -> kernelResult["Expr"], "Definitions" -> Join[localInterpDefs, kernelResult["Definitions"]]|>
                    ]
                    ,
                    {i, Length[splitResult["SubKernels"]]}
                ];
            (* Apply FMA to shared definitions *)
            If[Length[sharedDefs] > 0,
                sharedDefs = Map[{#[[1]], fmaRestructure[#[[2]]]}&, sharedDefs];
            ];
            FunKitDebug[2, "Multi-kernel optimization complete: ", Length[subKernels], " sub-kernels"];
            <|"UseSubKernels" -> True, "SharedDefinitions" -> sharedDefs, "SubKernels" -> subKernels|>
            ,
            (* Single-kernel path: optimize the whole expression *)
            result = optimizeSubKernel[expr, interpCount + divCount];
            allDefs = Join[globalDefs, result["Definitions"]];
            FunKitDebug[2, "Single-kernel optimization complete: ", Length[allDefs], " total defs"];
            (* Try splitting for registers if expression is large *)
            splitResult = cgTimed[$ProfileCgSplit, splitIntoSubKernels[allDefs, result["Expr"]]];
            FunKitDebug[2, "Post-optimization split: SubKernels=", TrueQ[splitResult["UseSubKernels"]]];
            splitResult
        ]
    ];

(**********************************************************************************
    Formatting: Convert optimized result to C++ code string
**********************************************************************************)

formatDefinitions[defs_] :=
    Module[{simplifiedExprs, result = ""},
        If[Length[defs] === 0,
            Return[""]
        ];
        (* FullSimplify is the dominant cost — parallelize it across definitions *)
        simplifiedExprs = parallelSimplify[defs[[All, 2]]];
        result = StringJoin @ Table["const auto " <> defs[[i, 1]] <> " = " <> CppForm[simplifiedExprs[[i]]] <> ";\n", {i, 1, Length[defs]}];
        result <> "\n"
    ];

formatReturnStatement[expr_] :=
    "  return " <> CppForm[expr] <> ";";

stripQuotedNames[code_String, names_List] :=
    StringReplace[code, Map["\"" <> # <> "\"" -> #&, names]];

getAllVarNames[optimized_] :=
    If[TrueQ[optimized["UseSubKernels"]],
        Join[Map[#[[1]]&, optimized["SharedDefinitions"]], Flatten @ Map[Map[#[[1]]&, #["Definitions"]]&, optimized["SubKernels"]]]
        ,
        Map[#[[1]]&, optimized["Definitions"]]
    ];
