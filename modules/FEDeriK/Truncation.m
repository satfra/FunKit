(**********************************************************************************
    Truncation.m -- Truncation of functional expressions to a given field content

    Public API:
      FTruncate                  -- Truncates FEx/FTerm to setup truncation table
      FTruncateOpenIndices       -- Truncates only open (external) indices

    Internal:
      truncationPass             -- Applies truncation rules to a single FTerm
                                    (used by FTruncate, LTrunc, OTrunc)
      truncationList             -- Builds memoized Dispatch rules from setup
                                    (used by truncationPass)
      insertFields               -- Replaces AnyField at index positions with a field
                                    (used by LTrunc)
      insertFieldsIfAnyField     -- Like insertFields but only replaces AnyField slots
                                    (used by LTrunc)
      LTrunc                     -- Truncates closed indices recursively
                                    (used by FTruncate)
      OTrunc                     -- Truncates open indices
                                    (used by FTruncateOpenIndices)
**********************************************************************************)

truncationPass[setup_, expr_FEx] :=
    Module[{},
        Map[truncationPass[setup, #]&, expr]
    ];

truncationList[setup_] :=
    truncationList[setup] =
        Dispatch @
            Map[
                obj : #[__] /; FreeQ[obj, AnyField, Infinity] :>
                    If[FreeQ[Sort /@ setup["Truncation"][#], Sort @ getFields[obj]],
                        0
                        ,
                        obj
                    ]&
                ,
                Intersection[Keys[setup["Truncation"]], $indexedObjects]
            ];

truncationPass[setup_, expr_FTerm] :=
    Module[{ret = expr, i},
        FunKitDebug[3, "Truncating term ", ret];
        (*Get rid of any truncated ordered functions*)
        ret = ret /. truncationList[setup];
        FunKitDebug[3, "Truncation result reads ", ret];
        (*Finally, remove the metric factors*)
        ret = ReduceIndices[setup, ret];
        Return[ret];
    ];

truncationPass[setup_, expr_] :=
    Module[{ret = expr},
        ret = ret /. truncationList[setup];
        Return[ret];
    ];

(* Light variant: applies truncation dispatch only, no ReduceIndices.
   Used inside LTrunc expansion where ReduceIndices would be wasted on partial terms. *)
truncationPassLight[setup_, expr_FTerm] :=
    expr /. truncationList[setup];

truncationPassLight[setup_, expr_FEx] :=
    Map[truncationPassLight[setup, #]&, expr];

truncationPassLight[setup_, expr_] :=
    expr /. truncationList[setup];

(* ::Input::Initialization:: *)

FTruncate::wrongExpr = "Cannot truncate an expression which is neither an FEx nor an FTerm. The expression was `1`";

FTruncate::noTruncation = "The given setup does not have a key \"Truncation\"";

FTruncate::missingCorrF = "The given truncation misses a truncation table for the correlation function `1`";

FTruncate::missing = "The given truncation misses a truncation table for `1`";

FTruncate::FDOp = "The given expression contains unresolved derivative operators! Cannot truncate before resolving all FDOp.";

FTruncate::emptyTruncation = "The truncation table in the given setup is empty. FTruncate will pass the expression through unchanged. Did you forget to add vertices to the setup?";

indices::inconsistentContractions = "The index `1` has been contracted in an inconsistent way in the expression
    `2`";

indices::objectNotFound = "Could not find the expected number of objects containing the index `1` in the expression `2`. Found `3` object(s), expected `4`.";

(*inside an object, find all occurences of idx and replace the fields at the respective positions with field.*)

insertFields[obj_, idx_, field_Symbol] :=
    Module[{positions},
        positions = Flatten[Position[makePosIdx /@ getIndices[obj], makePosIdx @ idx]];
        Fold[setField[#1, #2, field]&, obj, positions]
    ];

(*Like insertFields, but only replaces positions where the current field is AnyField.*)

insertFieldsIfAnyField[obj_, idx_, field_Symbol] :=
    Module[{positions, anyPositions},
        positions = Flatten[Position[makePosIdx /@ getIndices[obj], makePosIdx @ idx]];
        anyPositions = Select[positions, getField[obj, #] === AnyField&];
        Fold[setField[#1, #2, field]&, obj, anyPositions]
    ];

(* Generate candidate field assignments for an object's AnyField slots at closed indices.
   Returns a list of Associations mapping makePosIdx[index] -> field.
   Empty list {} means no valid assignment exists. {<||>} means nothing to enumerate. *)
getObjectCandidates[setup_, truncSorted_, obj_, closedIndexSet_Association, nonSourceFields_List] :=
    Module[{h = Head[obj], nLegs, entries, perms, closedAnySlots = {}, fixedSlots = {}, s, idx},
        nLegs = Length[getFields[obj]];
        Do[
            If[getField[obj, s] =!= AnyField,
                AppendTo[fixedSlots, s]
            ,
                idx = makePosIdx[getIndex[obj, s]];
                If[KeyExistsQ[closedIndexSet, idx],
                    AppendTo[closedAnySlots, s]
                ];
            ];
            , {s, 1, nLegs}
        ];
        If[closedAnySlots === {}, Return[{<||>}]];
        If[!KeyExistsQ[truncSorted, h], Return[{<||>}]];
        entries = Select[setup["Truncation"][h], Length[#] === nLegs&];
        If[entries === {}, Return[{}]];
        perms = DeleteDuplicates[Join @@ Map[Permutations, entries]];
        Do[perms = Select[perms, #[[s]] === getField[obj, s]&];, {s, fixedSlots}];
        Do[perms = Select[perms, MemberQ[nonSourceFields, #[[s]]]&];, {s, closedAnySlots}];
        If[perms === {}, Return[{}]];
        DeleteDuplicates @ Map[
            Function[perm,
                Association @@ Table[makePosIdx[getIndex[obj, s]] -> perm[[s]], {s, closedAnySlots}]
            ],
            perms
        ]
    ];

(* Apply index->field assignment to an object's AnyField slots. Notation-agnostic. *)
applyAssignmentToObj[obj_, assignment_Association] :=
    Module[{result = obj, s, idx},
        Do[
            If[getField[result, s] === AnyField,
                idx = makePosIdx[getIndex[result, s]];
                If[KeyExistsQ[assignment, idx],
                    result = setField[result, s, assignment[idx]]
                ];
            ];
            , {s, 1, Length[getFields[obj]]}
        ];
        result
    ];

(* Recursively validate/expand vertex-like objects.
   Returns list of Associations (complete index->field mappings for surviving terms).
   vertices: list of {retPosition, object} pairs, sorted by leg count ascending.
   assignment: Association of makePosIdx[index] -> field. *)
expandVertices[setup_, truncSorted_, truncKeys_, closedIndexSet_, nonSourceFields_,
               {}, assignment_Association] := {assignment};

expandVertices[setup_, truncSorted_, truncKeys_, closedIndexSet_, nonSourceFields_,
               vertices_List, assignment_Association] :=
    Module[{vObj, resolvedObj, h, newAssign, candidates, remainingClosedAny, s, idx},
        vObj = vertices[[1, 2]];
        resolvedObj = applyAssignmentToObj[vObj, assignment];
        remainingClosedAny = Select[Range[Length[getFields[resolvedObj]]],
            getField[resolvedObj, #] === AnyField &&
            KeyExistsQ[closedIndexSet, makePosIdx[getIndex[resolvedObj, #]]]&
        ];
        If[remainingClosedAny === {},
            (* All closed AnyField slots determined — validate if fully concrete *)
            h = Head[resolvedObj];
            If[FreeQ[resolvedObj, AnyField] && KeyExistsQ[truncSorted, h] &&
               !MemberQ[truncSorted[h], Sort @ getFields[resolvedObj]],
                {} (* Killed by truncation *)
            ,
                newAssign = assignment;
                Do[
                    idx = makePosIdx[getIndex[resolvedObj, s]];
                    If[KeyExistsQ[closedIndexSet, idx] && !KeyExistsQ[newAssign, idx],
                        AssociateTo[newAssign, idx -> getField[resolvedObj, s]]
                    ];
                    , {s, 1, Length[getFields[resolvedObj]]}
                ];
                expandVertices[setup, truncSorted, truncKeys, closedIndexSet, nonSourceFields,
                               Rest[vertices], newAssign]
            ]
        ,
            (* Some closed AnyField remain — enumerate candidates *)
            candidates = getObjectCandidates[setup, truncSorted, resolvedObj, closedIndexSet, nonSourceFields];
            If[candidates === {}, Return[{}]];
            Join @@ Map[
                Function[cand,
                    expandVertices[setup, truncSorted, truncKeys, closedIndexSet, nonSourceFields,
                                   Rest[vertices], Join[assignment, cand]]
                ],
                candidates
            ]
        ]
    ];

LTrunc[setup_, {}] :=
    {};

LTrunc[setup_, expr_] :=
    (
        Message[FTruncate::wrongExpr, expr];
        Abort[]
    );

LTrunc[setup_, expr_FEx] :=
    Join @@ Map[LTrunc[setup, #]&, List @@ expr];

(* LTrunc returns a list of bare lists (each = one surviving term's factors).
   FTruncate handles wrapping back into FTerm/FEx after BalancedMap.
   Object-level enumeration: propagators first, then vertices smallest-to-largest. *)
LTrunc[setup_, expr_FTerm] :=
    Module[{ret = List @@ expr, closedIndices, allFields = GetNonSourceFields[setup],
            ignore, doFields, undoFields, a,
            sentinelExpr, rawObjects, rawIndices, counts,
            propLikeHeads, propLike = {}, vertexLike = {},
            truncKeys, truncSorted, closedIndexSet,
            propCandidates, propCombinations, survivors, concreteFields,
            assignment, localRet, terms = {},
            tExtract, tExpand, pos, h, s, idx, killed, hasClosedAny},
        doFields = replFields[setup];
        undoFields = unreplFields[setup];
        ret = ret /. doFields;
        (*Start off with the nested FTerms — recurse into them first*)
        ret = ret /. FTerm[a__] :> LTrunc[setup, FTerm[a]];
        (*If no AnyField remains, just apply truncation and return*)
        If[FreeQ[ret, AnyField, Infinity],
            Return[{List @@ (truncationPassLight[setup, FTerm @@ ret] /. undoFields)}]
        ];
        tExtract = AbsoluteTime[];
        sentinelExpr = FTerm @@ (ret /. FTerm[__] :> ignore);
        {rawObjects, rawIndices} = ExtractObjectsAndIndices[setup, sentinelExpr];
        rawIndices = Select[rawIndices, Head[#] =!= List&];
        counts = Map[Count[rawObjects, #, {1, 5}]&, rawIndices];
        closedIndices = Pick[rawIndices, Map[Mod[#, 2] === 0&, counts]];
        If[Length[closedIndices] === 0,
            Return[{List @@ (truncationPassLight[setup, FTerm @@ ret] /. undoFields)}]
        ];
        (*Pre-compute lookup structures*)
        truncKeys = Intersection[Keys[setup["Truncation"]], $indexedObjects];
        truncSorted = Association @@ Map[(# -> (Sort /@ setup["Truncation"][#]))&, truncKeys];
        closedIndexSet = Association @@ Map[(# -> True)&, closedIndices];
        (*Classify top-level objects with AnyField at closed indices into
          propagator-like (enumerate first) and vertex-like (validate/expand after)*)
        propLikeHeads = {Propagator, Rdot, R};
        Do[
            If[objectQ[ret[[pos]]] && !FreeQ[ret[[pos]], AnyField] &&
               MemberQ[truncKeys, Head[ret[[pos]]]],
                hasClosedAny = False;
                Do[
                    If[getField[ret[[pos]], s] === AnyField &&
                       KeyExistsQ[closedIndexSet, makePosIdx[getIndex[ret[[pos]], s]]],
                        hasClosedAny = True; Break[]
                    ];
                    , {s, 1, Length[getFields[ret[[pos]]]]}
                ];
                If[hasClosedAny,
                    If[MemberQ[propLikeHeads, Head[ret[[pos]]]],
                        AppendTo[propLike, {pos, ret[[pos]]}]
                    ,
                        AppendTo[vertexLike, {pos, ret[[pos]]}]
                    ];
                ];
            ];
            , {pos, 1, Length[ret]}
        ];
        (*If no objects need enumeration, apply truncation and return*)
        If[propLike === {} && vertexLike === {},
            Return[{List @@ (truncationPassLight[setup, FTerm @@ ret] /. undoFields)}]
        ];
        (*Sort vertices by leg count ascending — smaller objects prune earlier*)
        vertexLike = SortBy[vertexLike, Length[getFields[#[[2]]]]&];
        tExtract = AbsoluteTime[] - tExtract;
        tExpand = AbsoluteTime[];
        (*Phase 1: Generate propagator/regulator candidates*)
        If[Length[propLike] > 0,
            propCandidates = Map[
                getObjectCandidates[setup, truncSorted, #[[2]], closedIndexSet, allFields]&,
                propLike
            ];
            (*If any propagator has zero valid candidates, no terms survive*)
            If[AnyTrue[propCandidates, # === {}&],
                tExpand = AbsoluteTime[] - tExpand;
                If[ValueQ[$ProfileLTruncDetail],
                    $ProfileLTruncExtract += tExtract;
                    $ProfileLTruncExpand += tExpand;
                    $ProfileLTruncCalls++;
                ];
                Return[{}]
            ];
            propCombinations = Tuples[propCandidates];
            propCombinations = Map[Join @@ # &, propCombinations];
        ,
            propCombinations = {<||>}
        ];
        (*Phase 2: Validate/expand vertices for each propagator combination*)
        survivors = Join @@ Map[
            expandVertices[setup, truncSorted, truncKeys, closedIndexSet, allFields,
                           vertexLike, #]&,
            propCombinations
        ];
        (*Collect concrete fields from all objects at closed indices.
          Fills gaps for FMinus etc. whose AnyField is at an index determined by concrete objects.*)
        concreteFields = <||>;
        Do[
            If[objectQ[ret[[pos]]],
                Do[
                    idx = makePosIdx[getIndex[ret[[pos]], s]];
                    If[KeyExistsQ[closedIndexSet, idx] && getField[ret[[pos]], s] =!= AnyField && !KeyExistsQ[concreteFields, idx],
                        AssociateTo[concreteFields, idx -> getField[ret[[pos]], s]]
                    ];
                    , {s, 1, Length[getFields[ret[[pos]]]]}
                ];
            ];
            , {pos, 1, Length[ret]}
        ];
        survivors = Map[Join[concreteFields, #]&, survivors];
        (*Phase 3: Materialize surviving assignments into concrete terms*)
        Do[
            assignment = survivors[[si]];
            localRet = ret;
            Do[
                If[objectQ[localRet[[pos]]],
                    localRet[[pos]] = applyAssignmentToObj[localRet[[pos]], assignment]
                ,
                    (*Non-object element (e.g. Times[FMinus, FMinus]): scoped ReplaceAll*)
                    If[!FreeQ[localRet[[pos]], AnyField],
                        localRet[[pos]] = localRet[[pos]] /.
                            obj_?objectQ /; !FreeQ[obj, AnyField] :> applyAssignmentToObj[obj, assignment]
                    ];
                ];
                , {pos, 1, Length[localRet]}
            ];
            (*Final truncation check on fully-concrete objects*)
            killed = False;
            Do[
                If[objectQ[localRet[[pos]]],
                    h = Head[localRet[[pos]]];
                    If[MemberQ[truncKeys, h] && FreeQ[localRet[[pos]], AnyField, Infinity],
                        If[!MemberQ[truncSorted[h], Sort @ getFields[localRet[[pos]]]],
                            killed = True
                        ];
                    ];
                ];
                If[killed, Break[]];
                , {pos, 1, Length[localRet]}
            ];
            If[!killed, AppendTo[terms, localRet]];
            , {si, 1, Length[survivors]}
        ];
        tExpand = AbsoluteTime[] - tExpand;
        If[ValueQ[$ProfileLTruncDetail],
            $ProfileLTruncExtract += tExtract;
            $ProfileLTruncExpand += tExpand;
            $ProfileLTruncCalls++;
            $ProfileLTruncPairs += Length[propLike] + Length[vertexLike];
        ];
        (*Return list of bare lists, with undoFields applied*)
        Map[(# /. undoFields)&, terms]
    ];

OTrunc[setup_, {}] :=
    {}

OTrunc[setup_, expr_FTerm] :=
    Module[{ret = List @@ expr, curi, allObj, openIndices, i, allFields = GetNonSourceFields[setup], idx, subObj, idxOccur, idxPos, ignore, doFields, a, undoFields},
        FunKitDebug[3, "Truncating the term (open indices) ", expr];
        doFields = replFields[setup];
        undoFields = unreplFields[setup];
        ret = ret /. doFields;
        (*Start off with the nested FTerms*)
        ret = ret /. FTerm[a__] :> OTrunc[setup, FTerm[a]];
        (*Abort if there is nothing to do*)
        If[FreeQ[ret, AnyField, Infinity],
            Return[truncationPass[setup, FTerm @@ ret] /. undoFields]
        ];
        (*Get all open indices*)
        openIndices = GetOpenSuperIndices[setup, FTerm @@ (ret /. FTerm[__] :> ignore)];
        If[Length[openIndices] === 0,
            FunKitDebug[3, "  No open indices!"];
            Return[FTerm @@ (ret /. undoFields)]
            ,
            FunKitDebug[3, "  Found open indices: ", openIndices];
        ];
        allObj = ExtractObjectsWithIndex[setup, FTerm @@ (ret /. FTerm[__] :> ignore)];
        ret = FEx[FTerm @@ ret];
        (*Next, find all factors that needs to be expanded*)
        For[curi = 1, curi <= Length[openIndices], curi++,
            idx = openIndices[[curi]];
            subObj = Select[allObj, MemberQ[getIndices[#], idx, {1, 3}]&];
            If[Length[subObj] < 1,
                Message[indices::objectNotFound, idx, expr, Length[subObj], 1];
                Abort[];
            ];
            idxOccur =
                If[MemberQ[getIndices[subObj[[1]]], -idx],
                    -idx
                    ,
                    idx
                ];
            idxPos = FirstPosition[getIndices[subObj[[1]]], idxOccur][[1]];
            (*If there's no AnyField, continue*)
            If[getField[subObj[[1]], idxPos] =!= AnyField,
                Continue[]
            ];
            (*Otherwise, directly expand*)
            ret =
                FEx @@
                    Map[
                        Module[{s1 = subObj[[1]], t},
                            s1 = setField[s1, idxPos, #];
                            s1 = truncationPass[setup, s1];
                            t = ret /. {subObj[[1]] :> s1, FMinus[{a_, a_}, {getIndex[s1, idxPos], getIndex[s1, idxPos]}] :> FMinus[{#, #}, {getIndex[s1, idxPos], getIndex[s1, idxPos]}], FMinus[{a_, b_}, {getIndex[s1, idxPos], ib_}] :> FMinus[{#, b}, {getIndex[s1, idxPos], ib}], FMinus[{a_, b_}, {ia_, getIndex[s1, idxPos]}] :> FMinus[{a, #}, {ia, getIndex[s1, idxPos]}]};
                            ReduceIndices[setup, t]
                        ]&
                        ,
                        allFields
                    ];
        ];
        Return[truncationPass[setup, ret] /. undoFields];
    ];

FTruncateOpenIndices[setup_, expr_FEx] :=
    Module[{ret0, ret1, ret2, ret3, annotations},
        AssertFSetup[setup];
        If[KeyFreeQ[setup, "Truncation"],
            Message[FTruncate::noTruncation];
            Abort[]
        ];
        If[Length[Keys[setup["Truncation"]]] === 0,
            Message[FTruncate::emptyTruncation];
        ];
        If[MemberQ[expr, FDOp[__], Infinity],
            Message[FTruncate::FDOp];
            Abort[]
        ];
        FunKitDebug[1, "Truncating (open indices) the given expression"];
        {ret0, annotations} = SeparateFExAnnotations[expr];
            (*Resolve open indices directly*)
            ret0 = BalancedMap[OTrunc[setup, #]&, ret0];
        (*Finally, reduce indices again to be safe*)
        ret0 = BalancedMap[ReduceIndices[setup, #]&, ret0];
        FunKitDebug[1, "Finished truncating (open indices) the given expression"];
        ret0 = OrderFields[setup, FixIndices[setup, #]& /@ ret0];
        (*Directly remove all FEx[]*)
        ret0 = ret0 /. FEx[] -> {} // Flatten;
        ret0 = FEx @@ ret0;
        ret0 = MergeFExAnnotations[ret0, annotations];
        If[ModuleLoaded[AnSEL] && $AutoSimplify === True,
            ret0 = FunKit`FSimplify[setup, ret0];
            {ret0, annotations} = SeparateFExAnnotations[ret0];
            ret0 = BalancedMap[ReduceIndices[setup, #]&, ret0];
            ret0 = MergeFExAnnotations[FEx @@ ret0, annotations];
        ];
        Return[ret0];
    ];

FTruncate[setup_, expr_FEx] :=
    Module[{ret0, ret1, ret2, ret3, annotations},
        AssertFSetup[setup];
        If[KeyFreeQ[setup, "Truncation"],
            Message[FTruncate::noTruncation];
            Abort[]
        ];
        If[Length[Keys[setup["Truncation"]]] === 0,
            Message[FTruncate::emptyTruncation];
        ];
        If[MemberQ[expr, FDOp[__], Infinity],
            Message[FTruncate::FDOp];
            Abort[]
        ];
        FunKitDebug[1, "Truncating the given expression"];
        {ret0, annotations} = SeparateFExAnnotations[expr];
        (*Take care of closed indices — LTrunc returns lists-of-lists*)
        Module[{t0 = AbsoluteTime[]},
            ret0 = BalancedMap[LTrunc[setup, #]&, ret0];
            (*Merge: ret0 is a List where each element is a list-of-bare-lists from LTrunc.
              Flatten one level, filter empties/zeros, wrap each bare list in FTerm.*)
            ret0 = If[Length[ret0] > 0, Join @@ ret0, {}];
            ret0 = Select[ret0, # =!= {} && # =!= {0}&];
            ret0 = Map[FTerm @@ # &, ret0];
            If[ValueQ[$ProfileLTrunc], $ProfileLTrunc += AbsoluteTime[] - t0];
        ];
        (*ret0 is now a flat List of FTerms — reduce indices*)
        Module[{t0 = AbsoluteTime[]},
            ret0 = Map[ReduceIndices[setup, #]&, ret0];
            If[ValueQ[$ProfilePostRI], $ProfilePostRI += AbsoluteTime[] - t0];
        ];
        FunKitDebug[1, "Finished truncating the given expression"];
        Module[{t0 = AbsoluteTime[]},
            ret0 = OrderFields[setup, FixIndices[setup, #]& /@ ret0];
            If[ValueQ[$ProfileFixOrder], $ProfileFixOrder += AbsoluteTime[] - t0];
        ];
        (*Directly remove all FEx[]*)
        ret0 = ret0 /. FEx[] -> {} // Flatten;
        ret0 = FEx @@ ret0;
        ret0 = MergeFExAnnotations[ret0, annotations];
        If[ModuleLoaded[AnSEL] && $AutoSimplify === True,
            Module[{t0 = AbsoluteTime[]},
                ret0 = FunKit`FSimplify[setup, ret0];
                If[ValueQ[$ProfileFSimplify], $ProfileFSimplify += AbsoluteTime[] - t0];
            ];
            {ret0, annotations} = SeparateFExAnnotations[ret0];
            ret0 = BalancedMap[ReduceIndices[setup, #]&, ret0];
            ret0 = MergeFExAnnotations[FEx @@ ret0, annotations];
        ];
        Return[ret0];
    ];

FTruncate[setup_, expr_FTerm] :=
    FTruncate[setup, FEx[expr]];

FTruncateOpenIndices[setup_, expr_FTerm] :=
    FTruncateOpenIndices[setup, FEx[expr]];

FTruncate[setup_, expr_] :=
    (Message[FTruncate::wrongExpr, expr]; Abort[]);

FTruncateOpenIndices[setup_, expr_] :=
    (Message[FTruncate::wrongExpr, expr]; Abort[]);
