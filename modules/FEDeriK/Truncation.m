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

LTrunc[setup_, {}] :=
    {};

LTrunc[setup_, expr_] :=
    (
        Message[FTruncate::wrongExpr, expr];
        Abort[]
    );

LTrunc[setup_, expr_FEx] :=
    Module[{},
        Map[LTrunc[setup, #]&, expr]
    ];

LTrunc[setup_, expr_FTerm] :=
    Module[{ret = List @@ expr, curi, allObj, closedIndices, openIndices, i, allFields = GetNonSourceFields[setup], idx, subObj, idxOccur, idxPos, ignore, notFoundCuri, doFields, a, undoFields},
        FunKitDebug[3, "Truncating the term (closed indices) ", expr];
        doFields = replFields[setup];
        undoFields = unreplFields[setup];
        ret = ret /. doFields;
        (*Start off with the nested FTerms*)
        ret = ret /. FTerm[a__] :> LTrunc[setup, FTerm[a]];
        (*Abort if there is nothing to do*)
        If[FreeQ[ret, AnyField, Infinity],
            Return[truncationPass[setup, FTerm@@ret] /. undoFields]
        ];
        (*Single call: extract objects and closed indices in one traversal*)
        Module[{sentinelExpr, rawObjects, rawIndices, counts},
            sentinelExpr = FTerm @@ (ret /. FTerm[__] :> ignore);
            {rawObjects, rawIndices} = ExtractObjectsAndIndices[setup, sentinelExpr];
            rawIndices = Select[rawIndices, Head[#] =!= List&];
            counts = Map[Count[rawObjects, #, {1, 5}]&, rawIndices];
            closedIndices = Pick[rawIndices, Map[Mod[#, 2] === 0&, counts]];
            allObj = rawObjects /. doFields;
        ];
        (*Abort if there is nothing to do*)
        If[Length[closedIndices] === 0,
            Return[truncationPass[setup, FTerm@@ret] /. undoFields]
        ];
        FunKitDebug[3, "  Searching for the first object that needs expansion..."];
        (*Next, try to find the first factor that needs to be expanded*)
        notFoundCuri = True;
        curi = 1;
        While[
            notFoundCuri
            ,
            If[curi > Length[closedIndices],
                FunKitDebug[2, "Leaving AnyFields in open indices unexpanded: ", FTerm @@ ret /. undoFields];
            Return[truncationPass[setup, FTerm@@ret] /. undoFields]
            ];
            idx = closedIndices[[curi]];
            subObj = Select[allObj, MemberQ[getIndices[#], idx, {1, 3}]&];
            If[Length[subObj] < 2,
                Message[indices::objectNotFound, idx, expr, Length[subObj], 2];
                Abort[];
            ];
            idxOccur =
                {
                    If[MemberQ[getIndices[subObj[[1]]], -idx],
                        -idx
                        ,
                        idx
                    ]
                    ,
                    If[MemberQ[getIndices[subObj[[2]]], -idx],
                        -idx
                        ,
                        idx
                    ]
                };
            If[Sort @ idxOccur =!= Sort @ {idx, -idx},
                Message[indices::inconsistentContractions, idx, expr];
                Abort[]
            ];
            idxPos = {FirstPosition[getIndices[subObj[[1]]], idxOccur[[1]]][[1]], FirstPosition[getIndices[subObj[[2]]], idxOccur[[2]]][[1]]};
            If[getField[subObj[[1]], idxPos[[1]]] =!= AnyField && getField[subObj[[2]], idxPos[[2]]] =!= AnyField,
                curi++;
                Continue[]
            ];
            notFoundCuri = False;
        ];
        If[getField[subObj[[1]], idxPos[[1]]] === AnyField && getField[subObj[[2]], idxPos[[2]]] === AnyField,
            (*Now replace all the fields:*)
            ret =
                FEx @@
                    Map[
                        Module[
                            {s1 = subObj[[1]], s2 = subObj[[2]], idx1, idx2, field1, field2, localRet}
                            ,
                            (*Pick the indices associated to where we want to insert a given field:*)
                            idx1 = getIndex[s1, idxPos[[1]]];
                            idx2 = getIndex[s2, idxPos[[2]]];
                            FunKitDebug[3, "Replacing the index ", makePosIdx[idx1], " by field ", #];
                            (*And find all index-looking objects and put in a replacement of the field at the right position.*)
                            localRet = ret /. {obj_?objectQ /; (MemberQ[makePosIdx /@ getIndices[obj], idx1] || MemberQ[makePosIdx /@ getIndices[obj], idx2]) :> insertFields[insertFields[obj, idx2, #], idx1, #]};
                            truncationPass[setup, FTerm @@ localRet]
                        ]&
                        ,
                        allFields
                    ];
            Return[LTrunc[setup, ret /. undoFields]];
        ];
        (* One side has AnyField, the other has a concrete field.
           Expand AnyField over all fields (like the both-AnyField case),
           but only replace positions where the field is currently AnyField. *)
        Module[{anyIdx},
            If[getField[subObj[[1]], idxPos[[1]]] === AnyField,
                anyIdx = getIndex[subObj[[1]], idxPos[[1]]];
                ,
                anyIdx = getIndex[subObj[[2]], idxPos[[2]]];
            ];
            ret =
                FEx @@
                    Map[
                        Module[{localRet},
                            FunKitDebug[3, "Replacing AnyField at index ", makePosIdx[anyIdx], " with field ", #];
                            localRet = ret /. {obj_?objectQ /; MemberQ[makePosIdx /@ getIndices[obj], makePosIdx @ anyIdx] :> insertFieldsIfAnyField[obj, anyIdx, #]};
                            truncationPass[setup, FTerm @@ localRet]
                        ]&
                        ,
                        allFields
                    ];
            Return[LTrunc[setup, ret /. undoFields]];
        ];
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
        (*Take care of closed indices recursively*)
        ret0 = BalancedMap[LTrunc[setup, #]&, ret0];
        (*Finally, reduce indices again to be safe*)
        ret0 = BalancedMap[ReduceIndices[setup, #]&, ret0];
        FunKitDebug[1, "Finished truncating the given expression"];
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

FTruncate[setup_, expr_FTerm] :=
    FTruncate[setup, FEx[expr]];

FTruncateOpenIndices[setup_, expr_FTerm] :=
    FTruncateOpenIndices[setup, FEx[expr]];

FTruncate[setup_, expr_] :=
    (Message[FTruncate::wrongExpr, expr]; Abort[]);

FTruncateOpenIndices[setup_, expr_] :=
    (Message[FTruncate::wrongExpr, expr]; Abort[]);
