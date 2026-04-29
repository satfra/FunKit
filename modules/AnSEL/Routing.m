(**********************************************************************************
    Routing.m -- Momentum and index routing through diagrams

    Public API:
      FRoute                     -- Routes momenta/indices through FTerm/FEx diagrams
      FUnroute                   -- Reverses routing, restoring superindex form

    Internal:
      FieldSetupIndices          -- Gets the index structure of a field from setup
                                    (used by FRoute)
      fermionicExtMomRouting     -- Detects if external momentum routing is fermionic
                                    (used by FRoute)
      makeMomentaAlternatives    -- Creates bosonic/fermionic momentum alternatives
                                    (used by FRoute for FEx)
      isLoopAssociation          -- Tests if Association is a loop association
                                    (used by FUnroute, DiANE/FPlot, DiANE/FPrint)
      isRoutedAssociation        -- Tests if Association is a routed association
                                    (used by FUnroute, DiANE/FPlot, DiANE/FPrint)
**********************************************************************************)

(*A convenience function to quickly obtain the index structure of a given field.*)

FieldSetupIndices::notFound = "The field `1` was not found in the setup's field space.";

(*Solve a single linear momentum conservation equation sum == 0 for variable mom.
  Faster than Solve[] which dispatches through the full CAS solver.*)

solveLinearMomConservation[sum_, mom_] :=
    Module[{coeff = Coefficient[sum, mom]},
        mom -> -(sum /. mom -> 0) / coeff
    ];

FieldSetupIndices[setup_, field_] :=
    FieldSetupIndices[setup, field] =
        Module[{result},
            result = SelectFirst[Flatten @ Values[setup["FieldSpace"]], Head[#] === field&];
            If[MissingQ[result],
                Message[FieldSetupIndices::notFound, field];
                Abort[]
            ];
            List @@ result
        ];

FRoute::undeterminedFields = "Cannot route indices in expressions with undetermined fields.";

FRoute::momentaFailed = "Cannot route momenta in the given expression. Final momentum conservation read `1`";

FRoute::conservationFail = "Momentum conservation could not be fulfilled. Error in `1`.
Full Expression:
    `2`";

FRoute::noObjectForIndex = "No indexed object could be found for the index `1`.";

FRoute::noLoopMomentum = "No loop momentum could be found for momentum routing at vertex `1`.";

(*detect if we should route into a fermionic loopMomentum (True) or a bosonic one (False)*)

fermionicExtMomRouting[setup_, vertex_] :=
    Module[{momsum, factor},
        momsum = Total[vertex[[2, All, 1]]];
        (*for this, we can set all external momenta with fermions equal and all external momenta with bosons to 0*)
        momsum = momsum //. externalMomentum[p_, True] :> externalMomentum[1, True] //. externalMomentum[p_, False] :> 0;
        (*Extract the prefactor of externalMomentum[1, True] (or 0, if it does not appear)*)
        momsum = Flatten[momsum /. Plus[a_, b__] :> List[a, b]];
        factor =
            Cases[
                momsum
                ,
                Times[c___, externalMomentum[1, True]] | externalMomentum[1, True] :>
                    If[Length[{c}] > 0,
                        Times[c]
                        ,
                        1
                    ]
            ];
        factor =
            If[Length[factor] > 0,
                Total[factor]
                ,
                0
            ];
        Return[Mod[factor, 2] === 1];
    ];

(* Partition an FTerm into connected components by BFS over shared closed
   superindices. Items with no indices (numeric coefficients, scalar factors)
   are absorbed into the first component so that multiplying the components
   reproduces the original coefficient. Mirrors the logic of FDisconnectedQ
   in modules/AnSEL/Disconnected.m. *)

partitionFTermByConnectivity[setup_, ft_FTerm] :=
    Module[{items, indexedItems, scalarItems, allFields, depth1Fields, idxO, idxF, indexedPos, scalarPos, closedIdx, idxSets, visited, queue, cur, idx, pos, components, compMembers},
        items = List @@ ft;
        allFields = Join[GetAllFields[setup], {AnyField}];
        depth1Fields = Cases[ft, Alternatives @@ Map[Blank[#]&, allFields], {1}];
        idxO = Cases[ft, Alternatives @@ Map[Blank[#]&, $indexedObjects], {1, 2}];
        idxF = Select[Cases[ft, Alternatives @@ Map[Blank[#]&, allFields], {1}], MemberQ[depth1Fields, #]&];
        indexedItems = Join[idxO, idxF];
        indexedPos = Flatten @ Map[Position[items, #, {1}, 1]&, indexedItems];
        scalarPos = Complement[Range[Length[items]], indexedPos];
        scalarItems = items[[scalarPos]];
        If[Length[indexedItems] <= 1,
            Return[{ft}]
        ];
        closedIdx = GetClosedSuperIndices[setup, ft];
        idxSets = objectIndices /@ indexedItems;
        components = {};
        visited = <||>;
        While[Length[visited] < Length[indexedItems],
            cur = First @ Complement[Range[Length[indexedItems]], Keys[visited]];
            compMembers = {cur};
            AssociateTo[visited, cur -> True];
            queue = {cur};
            While[Length[queue] > 0,
                cur = First[queue];
                queue = Rest[queue];
                Do[
                    If[!KeyExistsQ[visited, pos] && Length[Intersection[idxSets[[cur]], idxSets[[pos]], closedIdx]] > 0,
                        AssociateTo[visited, pos -> True];
                        AppendTo[queue, pos];
                        AppendTo[compMembers, pos];
                    ]
                    ,
                    {pos, 1, Length[indexedItems]}
                ];
            ];
            AppendTo[components, Sort[compMembers]];
        ];
        If[Length[components] == 1,
            Return[{ft}]
        ];
        components = SortBy[components, First];
        Return[
            MapIndexed[
                If[#2[[1]] === 1,
                    FTerm @@ Join[scalarItems, indexedItems[[#1]]]
                    ,
                    FTerm @@ indexedItems[[#1]]
                ]&
                ,
                components
            ]
        ]
    ];

(* The main routing function *)

(* Disconnected fast path: split the term into connected components, route
   each independently, then merge. Each component is routed in isolation so
   per-vertex momentum conservation holds locally; externals/loops in
   subsequent components are renumbered to avoid collisions. *)

FRoute[setup_, expr_FTerm] /; FDisconnectedQ[setup, expr] :=
    Module[{components, mergedTerm = FTerm[], mergedExt = {}, mergedLoops = {}, extOffset = 0, loopOffset = 0, comp, routed, extNew, loopNew, nExt, nLoops, renameRules, k, routedTerm, newExtPairs, newLoopMoms},
        FunKitDebug[1, "FRoute: term is disconnected, splitting into connected components"];
        components = partitionFTermByConnectivity[setup, expr];
        FunKitDebug[2, "  FRoute: ", Length[components], " components"];
        Do[
            comp = components[[i]];
            routed = FRoute[setup, comp];
            extNew = routed["ExternalIndices"];
            loopNew = routed["LoopMomenta"];
            nExt = Length[extNew];
            nLoops = Length[loopNew];
            (* Rename only the independent base momenta: p1..p_(N-1) for externals
               (leg N is -(sum of others) and follows automatically through the
               substitution) and l1..l_M for loops (all loops are independent). *)
            renameRules =
                Join[
                    Table[
                        Symbol["p" <> ToString[k]] -> Symbol["p" <> ToString[extOffset + k]]
                        ,
                        {k, 1, nExt - 1}
                    ]
                    ,
                    Table[
                        Symbol[$loopMomentumName <> ToString[k]] -> Symbol[$loopMomentumName <> ToString[loopOffset + k]]
                        ,
                        {k, 1, nLoops}
                    ]
                ];
            routedTerm = First @ routed["Expression"];
            mergedTerm = mergedTerm ** (routedTerm /. renameRules);
            newExtPairs = (extNew /. renameRules);
            mergedExt = Join[mergedExt, newExtPairs];
            newLoopMoms = loopNew /. renameRules;
            mergedLoops = Join[mergedLoops, newLoopMoms];
            extOffset += Max[nExt - 1, 0];
            loopOffset += nLoops;
            ,
            {i, 1, Length[components]}
        ];
        Return[<|"Expression" -> FEx[mergedTerm], "ExternalIndices" -> Sort @ mergedExt, "LoopMomenta" -> Sort @ mergedLoops|>]
    ];

FRoute[setup_, expr_FTerm] :=
    Module[{openIndices, closedIndices, objects, ret = ReduceFTerm[setup, ReduceIndices[setup, expr]], idx, a, indPos, assocField, subObj, subMom, subExtMom, indStruct, externalIndices, externalMomenta, kind, f, momRepl, i, mom, loopMomenta, sidx, discard, rightMomenta, closedIndex, nextObj, tmp, flag, availMomenta, frozenMomenta = {}},
        AssertFSetup[setup];
        FunKitDebug[1, "FRoute: routing the sub-term ", expr];
        (*We first get all closed, open indices and all indexed objects. *)
        openIndices = Sort @ GetOpenSuperIndices[setup, ret];
        closedIndices = GetClosedSuperIndices[setup, ret];
        objects = FixedPoint[replFields[setup, #]&, ExtractObjectsWithIndex[setup, ret]];
        (*If there are any undetermined fields, we cannot route indices. *)
        If[MemberQ[objects[[All, 1]], AnyField, {1, 4}],
            Message[FRoute::undeterminedFields];
            Abort[]
        ];
        (*We need to reorder the objects list. In particular, (if possible) the next object should always share a closed index with the one before it.*)
        Do[
            subObj = objects[[idx]];
            (*find the closed index in the subObj*)
            closedIndex = Cases[getIndices[subObj], x_ /; MemberQ[closedIndices, x], Infinity];
            (*find the first object after with a shared closedIndex*)
            nextObj = Select[objects[[idx + 1 ;; ]], ContainsAny[makePosIdx /@ getIndices[#], closedIndex]&];
            If[Length[nextObj] === 0,
                Continue[]
            ];
            nextObj = FirstPosition[objects, nextObj[[1]]];
            (*Swap the object right after with nextObj*)
            If[Not @ MissingQ[nextObj] && nextObj[[1]] =!= idx + 1,
                nextObj = nextObj[[1]];
                FunKitDebug[2, "  FRoute: Swapping objects at positions ", idx + 1, " and ", nextObj];
                tmp = objects[[idx + 1]];
                objects[[idx + 1]] = objects[[nextObj]];
                objects[[nextObj]] = tmp;
            ]
            ,
            {idx, 1, Length[objects] - 1}
        ];
        (*Now, momenta. Collect all closed-index replacement rules in one pass, then apply at once. *)
        Module[{allClosedRules = {}},
            Do[
                subObj = Select[objects, MemberQ[#, closedIndices[[idx]], Infinity]&];
                If[Length[subObj] === 0,
                    Message[FRoute::noObjectForIndex, closedIndices[[idx]]];
                    Abort[]
                ];
                subObj = subObj[[1]];
                (*The indexed object we currently modify. There are always two and we simply grab the first. *)
                (*The position of the current index inside the subObj*)
                indPos = FirstPosition[getIndices[subObj], closedIndices[[idx]]][[1]];
                (*See what kind of field is associated with the index*)
                assocField = getField[subObj, indPos];
                (*Grab the index structure of this field from the setup and assign a new momentum variable*)
                indStruct =
                    Map[
                        If[MatchQ[#, _Symbol],
                            Unique[SymbolName[#]]
                            ,
                            #
                        ]&
                        ,
                        FieldSetupIndices[setup, assocField]
                        ,
                        {1, 3}
                    ];
                indStruct[[1]] = loopMomentum[indStruct[[1]], IsGrassmann[setup, assocField]];
                (* We want to keep the index sign in the momenta, but remove it from the group indices *)
                Module[{rules = {closedIndices[[idx]] -> indStruct}},
                    If[Length[indStruct] > 1,
                        rules = Join[rules, Thread[-(makePosIdx /@ indStruct[[2]]) -> (makePosIdx /@ indStruct[[2]])]];
                    ];
                    allClosedRules = Join[allClosedRules, rules];
                ];
                ,
                {idx, 1, Length[closedIndices]}
            ];
            (*Apply all closed-index rules at once — two passes for sign propagation in group indices*)
            ret = ret /. allClosedRules /. allClosedRules;
            objects = objects /. allClosedRules /. allClosedRules;
        ];
(*Next, we treat the external superindices. We assign to each an open group structure and a new momentum p1,p2,... 
Momentum conservation is already enforced here, i.e. \!\(
\*SubscriptBox[\(\[Sum]\), \(i\)]
\*SubscriptBox[\(p\), \(i\)]\)=0 and we choose Subscript[p, n]=-\!\(
\*SubscriptBox[\(\[Sum]\), \(i < n\)]\(
\*SubscriptBox[\(p\), \(i\)]\ for\ the\ last\ momentum\ \(
\*SubscriptBox[\(p\), \(n\)]\(.\)\)\)\)*)
        (*Collect ALL open-index replacement rules, then apply at once.*)
        externalIndices = Table[{}, {idx, 1, Length[openIndices]}];
        Module[{allOpenRules = {}},
            Do[
                subObj = Select[objects, MemberQ[#, openIndices[[idx]], Infinity]&];
                If[Length[subObj] === 0,
                    Message[FRoute::noObjectForIndex, openIndices[[idx]]];
                    Abort[]
                ];
                subObj = subObj[[1]];
                indPos = FirstPosition[getIndices[subObj], openIndices[[idx]]][[1]];
                assocField = getField[subObj, indPos];
                indStruct =
                    Map[
                        If[MatchQ[#, _Symbol],
                            (* Strip Module-local "$nnn" suffix so that user-named field
                               indices (e.g. p, a from Module[{p,a},...] in a setup helper)
                               yield clean externals p1, a1 rather than p$120491. *)
                            Symbol[StringSplit[SymbolName[#], "$"][[1]] <> ToString[idx]]
                            ,
                            #
                        ]&
                        ,
                        FieldSetupIndices[setup, assocField]
                        ,
                        {1, 3}
                    ];
                If[idx === Length[openIndices],
                    indStruct[[1]] = -Total[Values[externalIndices][[ ;; idx - 1, 1]]]
                    ,
                    indStruct[[1]] = externalMomentum[indStruct[[1]], IsGrassmann[setup, assocField]];
                ];
                AppendTo[allOpenRules, (-openIndices[[idx]]) -> indStruct];
                AppendTo[allOpenRules, openIndices[[idx]] -> indStruct];
                externalIndices[[idx]] = openIndices[[idx]] -> indStruct;
                ,
                {idx, 1, Length[openIndices]}
            ];
            ret = ret /. allOpenRules;
            objects = objects /. allOpenRules;
        ];
        (*extract a list of all new external momenta*)
        externalMomenta = Values[externalIndices][[All, 1]];
        FunKitDebug[2, "  FRoute: Determined external momenta as ", externalMomenta];
        (*Now, we do the momentum routing. We iterate over all objects in subObj and fully resolve them.*)

        If[$routingAlgorithm === "Regulator",
            (*In the regulator routing algorithm, we never route through regulators. We resolve momentum conservation
              at every Rdot first, reducing each Rdot's two legs to a {l, -l} pure-loop pair, then mark the surviving
              loop momentum as frozen so the main conservation loop below never picks it as a solve-for variable. This
              prevents external momenta from leaking into the regulator via chained substitutions at other vertices.*)
            Module[{rdotPositions, ridx, rdotPos, rdotMoms, rdotMomRepl, freezeMom, otherMom},
                rdotPositions = Flatten @ Position[objects, x_ /; Head[x] === Rdot, {1}, Heads -> False];
                FunKitDebug[3, "  FRoute Regulator: found Rdot objects at positions ", rdotPositions];
                Do[
                    rdotPos = rdotPositions[[ridx]];
                    rdotMoms = getIndices[objects[[rdotPos]]][[All, 1]];
                    If[Length[rdotMoms] < 2,
                        Continue[]
                    ];
                    If[Total[rdotMoms] =!= 0,
                        otherMom = FirstCase[rdotMoms, loopMomentum[__, _], Missing[], Infinity];
                        If[!MissingQ[otherMom],
                            rdotMomRepl = solveLinearMomConservation[Total[rdotMoms], otherMom];
                            FunKitDebug[3, "    FRoute Regulator: resolving Rdot conservation as ", rdotMomRepl];
                            objects = objects /. rdotMomRepl;
                            ret = ret /. rdotMomRepl;
                        ];
                    ];
                    freezeMom = FirstCase[
                        getIndices[objects[[rdotPos]]][[All, 1]],
                        loopMomentum[__, _],
                        Missing[],
                        Infinity
                    ];
                    If[!MissingQ[freezeMom],
                        AppendTo[frozenMomenta, freezeMom];
                        FunKitDebug[3, "    FRoute Regulator: frozen loop momentum on Rdot is ", freezeMom];
                    ];
                    ,
                    {ridx, 1, Length[rdotPositions]}
                ];
                frozenMomenta = DeleteDuplicates[frozenMomenta];
            ];
        ];

        Do[
            subObj = objects[[idx]];
            subMom = getIndices[subObj][[All, 1]];
            (*See if the object has any external (sub-)momenta*)
            subExtMom = Select[subMom, (ContainsAny[externalMomenta, makePosIdx /@ Flatten[{# /. Plus[a_, b__] :> List[a, b]}]])&];
            FunKitDebug[3, "  FRoute: routing the subObj ", subObj];
            FunKitDebug[3, "    FRoute: subExtMom are ", subExtMom];
            (*********************************************************************************)
            (* CASE 0: If momentum conservation is already fulfilled, do nothing *)
            (*********************************************************************************)
            If[Total @ subMom === 0,
                FunKitDebug[3, "      FRoute: Have only external momenta"];
                If[Total @ getIndices[subObj][[All, 1]] =!= 0,
                    Message[FRoute::conservationFail, subObj, ret];
                    Abort[];
                ];
                Continue[]
            ];
            (*********************************************************************************)
            (*CASE 1: we have no external momentum anywhere in the legs of the subObject *)
            (*********************************************************************************)
            If[Length[subExtMom] === 0,
                (*If we have nothing to enforce, skip this object. This is the case for 1-Point functions*)
                availMomenta = Total[getIndices[subObj][[All, 1]]];
                availMomenta = makePosIdx /@ Flatten[{availMomenta //. {Plus[a_, b__] :> List[a, b], Times[a_loopMomentum, b__] :> List[a, b]}}];
                availMomenta = Select[availMomenta, MatchQ[#, loopMomentum[__, _]]&];
                FunKitDebug[5, "        available loop momenta =  ", availMomenta];
                If[Length[availMomenta] < 2,
                    Continue[]
                ];
                FunKitDebug[3, "      FRoute: No external momenta"];
                (*Grab the first loopMomentum that is fermionic; in Regulator mode skip frozen ones so external momenta don't leak via substitution*)
                mom = Select[availMomenta, MatchQ[#, loopMomentum[_, True]] && Not @ MemberQ[frozenMomenta, #]&];
                If[Length[mom] === 0,
                    (*Otherwise, we have no (purely) fermionic loop momenta, so just grab the first bosonic one*)
                    mom = Select[availMomenta, MatchQ[#, loopMomentum[_, False]] && Not @ MemberQ[frozenMomenta, #]&];
                ];
                If[Length[mom] === 0,
                    Message[FRoute::noLoopMomentum, subObj];
                    Abort[]
                ];
                mom = mom[[1]];
                (*Now create the replacement rule*)
                momRepl = solveLinearMomConservation[Total[getIndices[subObj][[All, 1]]], mom];
                objects = objects /. momRepl;
                ret = ret /. momRepl;
                FunKitDebug[3, "      FRoute: routing a momentum as ", momRepl];
                Continue[];
            ];
            (*********************************************************************************)
            (*Case 2: We have both internal and external momenta *)
            (*********************************************************************************)
            If[Length[subExtMom] <= Length[subMom],
                FunKitDebug[3, "      FRoute: Have both internal and external momenta"];
                flag = fermionicExtMomRouting[setup, subObj];
                FunKitDebug[3, "        Are we routing a fermionic external momentum? ", flag];
                (*If we have a fermionic external momentum, we need to route it correctly. In that case, try to find a fermionic loopMomentum*)
                availMomenta = Total[getIndices[subObj][[All, 1]]];
                availMomenta = makePosIdx /@ Flatten[{availMomenta //. {Plus[a_, b__] :> List[a, b], Times[a_loopMomentum, b__] :> List[a, b]}}];
                availMomenta = Select[availMomenta, MatchQ[#, loopMomentum[__, _]]&];
                FunKitDebug[5, "        available loop momenta =  ", availMomenta];
                (*Grab one of the momenta which is a loopMomentum; in Regulator mode skip frozen ones so external momenta don't leak via substitution*)
                If[flag,
                    mom = Select[availMomenta, MatchQ[#, loopMomentum[_, True]] && Not @ MemberQ[frozenMomenta, #]&];
                ];
                If[Not @ flag || Length[mom] === 0,
                    mom = Select[availMomenta, MatchQ[#, loopMomentum[_, False]] && Not @ MemberQ[frozenMomenta, #]&];
                    If[Length[mom] === 0,
                        mom = Select[availMomenta, MatchQ[#, loopMomentum[_, True]] && Not @ MemberQ[frozenMomenta, #]&];
                    ];
                ];
                If[Length[mom] === 0,
                    Message[FRoute::noLoopMomentum, subObj];
                    Abort[]
                ];
                mom = mom[[1]];
                (*now build the replacement rule*)
                momRepl = solveLinearMomConservation[Total[getIndices[subObj][[All, 1]]], mom];
                (*if the given momentum is NOT a fermionic one, we will need to replace all the momenta on the right-hand-side with NOT fermionic ones*)
                If[Not @ mom[[2]],
                    rightMomenta = Cases[momRepl[[2]], loopMomentum[__, True], Infinity] // DeleteDuplicates;
                    rightMomenta = Map[# -> Head[#][#[[1]], False]&, rightMomenta];
                    subObj = subObj /. rightMomenta;
                    objects = objects /. rightMomenta;
                    ret = ret /. rightMomenta;
                    (*Keep frozenMomenta in sync — Regulator-mode prologue may have stored
                      a fermionic-tagged frozen momentum that just got re-tagged to bosonic here.*)
                    frozenMomenta = frozenMomenta /. rightMomenta;
                    momRepl = solveLinearMomConservation[Total[getIndices[subObj][[All, 1]]], mom];
                ];
                objects = objects /. momRepl;
                ret = ret /. momRepl;
                FunKitDebug[3, "      FRoute: routing a momentum as ", momRepl];
                Continue[];
            ];
            ,
            {idx, 1, Length[objects]}
        ];
        (*Sanity check to see that we did not make an error*)
        Do[
            subObj = objects[[idx]];
            (*Skip again Fields and such*)
            If[Length[getIndices[subObj][[All, 1]]] < 2,
                Continue[]
            ];
            (*Check the conservation of momentum at all vertices*)
            If[Total[getIndices[subObj][[All, 1]]] =!= 0,
                Message[FRoute::momentaFailed, Total[getIndices[subObj][[All, 1]]]];
                Abort[]
            ];
            ,
            {idx, 1, Length[objects]}
        ];
        (*replace the loopMomenta[...] by l1, l2, ...*)
        loopMomenta = Cases[objects[[All, 2, 1]], loopMomentum[__], Infinity] // DeleteDuplicates;
        kind =
            If[#[[2]],
                "f"
                ,
                ""
            ]&;
        (*Insert loop momenta*)
        ret = ret /. Thread[loopMomenta -> Table[Symbol[$loopMomentumName <> kind[loopMomenta[[idx]]] <> ToString[idx]], {idx, 1, Length[loopMomenta]}]];
        (*Insert external momenta*)
        ret = ret //. externalMomentum[p_, _] :> p;
        externalIndices = externalIndices //. externalMomentum[p_, _] :> p;
        loopMomenta = loopMomenta /. Thread[loopMomenta -> Table[Symbol[$loopMomentumName <> ToString[idx]], {idx, 1, Length[loopMomenta]}]];
        Return[<|"Expression" -> FEx[ret], "ExternalIndices" -> Sort @ externalIndices, "LoopMomenta" -> Sort @ loopMomenta|>];
    ];

makeMomentaAlternatives[mom_] :=
    Module[{idx},
        idx = StringSplit[SymbolName[mom], x:NumberString :> ToExpression @ x][[2]];
        Return[Alternatives[Symbol[$loopMomentumName <> ToString[idx]], Symbol[$loopMomentumName <> "f" <> ToString[idx]]]]
    ];

FRoute[setup_, expr_FEx] :=
    Module[{results, ret, idx, subidx},
        results = FEx @@ DropFExAnnotations[expr];
        results = FRoute[setup, #]& /@ (List @@ results);
        results = GatherBy[results, Length[#["LoopMomenta"]]&];
        results = Map[<|"Expression" -> FEx @@ #[[All, Key["Expression"]]], "ExternalIndices" -> #[[1, Key["ExternalIndices"]]], "LoopMomenta" -> makeMomentaAlternatives /@ #[[1, Key["LoopMomenta"]]]|>&, results];
        results = Association @@ Map[ToString[Length[#["LoopMomenta"]]] ~~ "-Loop" -> #&, results];
        Return[results];
    ];

(* Check if an expression is a loop association, i.e. a distinct routed n-loop term (used also in DiANE) *)

isLoopAssociation[expr_] :=
    Module[{},
        If[Head[expr] =!= Association,
            Return[False]
        ];
        If[FreeQ[Keys[expr], "Expression"],
            Return[False]
        ];
        If[FreeQ[Keys[expr], "ExternalIndices"],
            Return[False]
        ];
        If[FreeQ[Keys[expr], "LoopMomenta"],
            Return[False]
        ];
        Return[True];
    ];

(* Check if an expression is a association of loop associations, i.e. several distinct n-loop terms (used also in DiANE) *)

isRoutedAssociation[expr_] :=
    Module[{},
        If[Head[expr] =!= Association,
            Return[False]
        ];
        Return @ AllTrue[expr, isLoopAssociation]
    ];

(* Functions to try and undo routing. Note that this can fail, e.g. when we have multiple fields with no index structure but identical momenta. *)

FUnroute[setup_, assoc_Association] /; isLoopAssociation[assoc] :=
    Module[{},
        AssertFSetup[setup];
        Return @ FUnroute[setup, assoc["Expression"] /. Map[#[[2]] -> #[[1]]&, assoc["ExternalIndices"]]];
    ];

FUnroute[setup_, assoc_Association] /; isRoutedAssociation[assoc] :=
    FEx @@ (FUnroute[setup, #]& /@ (List @@ assoc));

FUnroute[setup_, term_FEx] :=
    Module[{},
        AssertFSetup[setup];
        FUnroute[setup, #]& /@ term
    ];

FUnroute[setup_, term_FTerm] :=
    Module[{fw, bw},
        AssertFSetup[setup];
        {fw, bw} = GetSuperIndexTermTransformations[setup, term];
        Return[term // fw];
    ];

(* Catch-all definitions *)

FRoute[___] :=
    (
        Message[FunKit::invalidArguments, FRoute];
        Abort[]
    );

FUnroute[___] :=
    (
        Message[FunKit::invalidArguments, FUnroute];
        Abort[]
    );
