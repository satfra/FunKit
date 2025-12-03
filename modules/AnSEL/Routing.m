(**********************************************************************************
    IndexRouting
**********************************************************************************)

(*A convenience function to quickly obtain the index structure of a given field.*)

FieldSetupIndices[setup_, field_] :=
    Module[{},
        List @@ SelectFirst[Flatten @ Values[setup["FieldSpace"]], Head[#] === field&]
    ];

FRoute::undeterminedFields = "Cannot route indices in expressions with undetermined fields.";

FRoute::momentaFailed = "Cannot route momenta in the given expression. Final momentum conservation read `1`";

FRoute::conservationFail = "Momentum conservation could not be fulfilled. Error in `1`.
Full Expression:
    `2`";

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

(* The main routing function *)

FRoute[setup_, expr_FTerm] :=
    Module[{openIndices, closedIndices, objects, ret = ReduceFTerm[setup, ReduceIndices[setup, expr]], doFields, idx, a, indPos, assocField, subObj, subMom, subExtMom, indStruct, externalIndices, externalMomenta, kind, f, momRepl, i, mom, loopMomenta, sidx, discard, rightMomenta, closedIndex, nextObj, tmp, flag},
        FunKitDebug[1, "FRoute: routing the sub-term ", expr];
        (*We first get all closed, open indices and all indexed objects. *)
        doFields = replFields[setup];
        openIndices = Sort @ GetOpenSuperIndices[setup, ret];
        closedIndices = GetClosedSuperIndices[setup, ret];
        objects = ExtractObjectsWithIndex[setup, ret] //. doFields;
        (*If there are any undetermined fields, we cannot route indices. *)
        If[MemberQ[objects[[All, 1]], AnyField, {1, 4}],
            Message[FRoute::undeterminedFields];
            Abort[]
        ];
        (*We need to reorder the objects list. In particular, (if possible) the next object should always share a closed index with the one before it.*)
        Do[
            subObj = objects[[idx]];
            (*find the closed index in the subObj*)
            closedIndex = Cases[subObj[[2]], x_ /; MemberQ[closedIndices, x], Infinity];
            (*find the first object after with a shared closedIndex*)
            nextObj = Select[objects[[idx + 1 ;; ]], ContainsAny[makePosIdx /@ #[[2]], closedIndex]&];
            If[Length[nextObj] === 0,
                Continue[]
            ];
            nextObj = FirstPosition[objects, nextObj[[1]]];
            (*Swap the object right after with nextObj*)
            If[nextObj =!= "NotFound" && nextObj[[1]] =!= idx + 1,
                FunKitDebug[2, "  FRoute: Swapping objects at positions ", idx + 1, " and ", nextObj];
                tmp = objects[[idx + 1]];
                objects[[idx + 1]] = objects[[nextObj]];
                objects[[nextObj]] = tmp;
            ]
            ,
            {idx, 1, Length[objects] - 1}
        ];
        (*Now, momenta. As a first step, we insert the correct index structures into all superindices and define momentum variables at every single vertex. We loop over all closed indices.*)
        Do[
            (*The indexed object we currently modify. There are always two and we simply grab the first. *)subObj = Select[objects, MemberQ[#, closedIndices[[idx]], Infinity]&][[1]];
            (*The position of the current index inside the subObj*)
            indPos = FirstPosition[subObj[[2]], closedIndices[[idx]]][[1]];
            (*See what kind of field is associated with the index*)
            assocField = subObj[[1, indPos]];
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
            (* replace all occurences of the superindex with the fitting index structure. *)
            (* We want to keep the index sign in the momenta, but remove it from the group indices *)
            ret = ret /. closedIndices[[idx]] -> indStruct;
            objects = objects /. closedIndices[[idx]] -> indStruct;
            If[Length[indStruct] > 1,
                ret = ret /. (-indStruct[[2]]) -> indStruct[[2]];
                objects = objects /. (-indStruct[[2]]) -> indStruct[[2]];
            ];
            ,
            {idx, 1, Length[closedIndices]}
        ];
(*Next, we treat the external superindices. We assign to each an open group structure and a new momentum p1,p2,... 
Momentum conservation is already enforced here, i.e. \!\(
\*SubscriptBox[\(\[Sum]\), \(i\)]
\*SubscriptBox[\(p\), \(i\)]\)=0 and we choose Subscript[p, n]=-\!\(
\*SubscriptBox[\(\[Sum]\), \(i < n\)]\(
\*SubscriptBox[\(p\), \(i\)]\ for\ the\ last\ momentum\ \(
\*SubscriptBox[\(p\), \(n\)]\(.\)\)\)\)*)
        externalIndices = Table[{}, {idx, 1, Length[openIndices]}];
        Do[
            (*see above*)subObj = Select[objects, MemberQ[#, openIndices[[idx]], Infinity]&][[1]];
            indPos = FirstPosition[subObj[[2]], openIndices[[idx]]][[1]];
            assocField = subObj[[1, indPos]];
            indStruct =
                Map[
                    If[MatchQ[#, _Symbol],
                        Symbol[SymbolName[#] <> ToString[idx]]
                        ,
                        #
                    ]&
                    ,
                    FieldSetupIndices[setup, assocField]
                    ,
                    {1, 3}
                ];
(*Subscript[p, n]=-\!\(
\*SubscriptBox[\(\[Sum]\), \(i < n\)]
\*SubscriptBox[\(p\), \(i\)]\)*)
            If[idx === Length[openIndices],
                indStruct[[1]] = -Total[Values[externalIndices][[ ;; idx - 1, 1]]]
                ,
                indStruct[[1]] = externalMomentum[indStruct[[1]], IsGrassmann[setup, assocField]];
            ];
            (*Wrap the momenta in externalMomentum[...]*)
            (*Do the replacements*)
            ret = ret /. (-openIndices[[idx]]) -> indStruct;
            ret = ret /. (openIndices[[idx]]) -> indStruct;
            objects = objects /. (-openIndices[[idx]]) -> indStruct;
            objects = objects /. openIndices[[idx]] -> indStruct;
            (*This is information for the user, which we will return.    *)
            externalIndices[[idx]] = openIndices[[idx]] -> indStruct;
            ,
            {idx, 1, Length[openIndices]}
        ];
        (*extract a list of all new external momenta*)
        externalMomenta = Values[externalIndices][[All, 1]];
        FunKitDebug[2, "  FRoute: Determined external momenta as ", externalMomenta];
        (*Now, we do the momentum routing. We iterate over all objects in subObj and fully resolve them.*)
        Do[
            subObj = objects[[idx]];
            subMom = subObj[[2, All, 1]];
            (*See if the object has any external (sub-)momenta*)
            subExtMom = Select[subMom, (ContainsAny[externalMomenta, makePosIdx /@ Flatten[{# /. Plus[a_, b__] :> List[a, b]}]])&];
            FunKitDebug[3, "  FRoute: routing the subObj ", subObj];
            FunKitDebug[3, "    FRoute: subExtMom are ", subExtMom];
            (*********************************************************************************)
            (* CASE 0: If momentum conservation is already fulfilled, do nothing *)
            (*********************************************************************************)
            If[Total @ subMom === 0,
                FunKitDebug[3, "      FRoute: Have only external momenta"];
                If[Total @ subObj[[2, All, 1]] =!= 0,
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
                If[Length[subObj[[2, All, 1]]] < 2,
                    Continue[]
                ];
                FunKitDebug[3, "      FRoute: No external momenta"];
                (*Grab the first loopMomentum that is fermionic*)
                tmp = subObj[[2, All]] //. loopMomentum[_, True] -> loopMomentum[1, True];
                f = Select[tmp, MemberQ[#, loopMomentum[_, True], Infinity]&];
                If[Length[f] =!= 0,
                    f = Position[tmp, f[[1]]][[1, 1]];
                    ,
                    (*Otherwise, we have no (purely) fermionic loop momenta, so just grab the first bosonic one*)
                    f = Select[subObj[[2, All]], MemberQ[#, loopMomentum[_, False], Infinity]&];
                    f = Position[subObj[[2]], f[[1]]][[1, 1]];
                ];
                (*Make a list of momenta out of the momentum sum in the subObj at position f *)
                tmp = makePosIdx /@ Flatten[{subObj[[2, f, 1]] //. {Plus[a_, b__] :> List[a, b], Times[a_loopMomentum, b__] :> List[a, b]}}];
                (*Grab one of the momenta which is a loopMomentum *)
                mom = Select[tmp, MatchQ[#, loopMomentum[_, True]]&];
                If[Length[mom] === 0,
                    mom = Select[tmp, MatchQ[#, loopMomentum[_, False]]&];
                ];
                mom = mom[[1]];
                (*Now create the replacement rule*)
                momRepl = Solve[Total[subObj[[2, All, 1]]] == 0, mom][[1, 1]];
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
                If[flag,
                    f = Select[subObj[[2, All]], MemberQ[#, loopMomentum[_, True], Infinity]&];
                    FunKitDebug[5, "        1. Chose f =  ", f];
                ];
                (*Otherwise, or, if we can't find a fermionic loopMomentum, pick a bosonic one*)
                If[Not @ flag || Length[f] === 0,
                    f = Select[subObj[[2, All]], MemberQ[#, loopMomentum[_, False], Infinity]&];
                    (* There's one more (nested) case: we have only fermionic loop Momenta, but no external fermionic one.*)
                    If[Length[f] === 0,
                        f = Select[subObj[[2, All]], MemberQ[#, loopMomentum[_, True], Infinity]&];
                    ];
                    FunKitDebug[5, "        2. Chose f =  ", f];
                ];
                f = Position[subObj[[2]], f[[1]]][[1, 1]];
                FunKitDebug[5, "        Final f =  ", f];
                (*Make a list of momenta out of the momentum sum in the subObj at position f *)
                tmp = makePosIdx /@ Flatten[{subObj[[2, f, 1]] //. {Plus[a_, b__] :> List[a, b], Times[a_loopMomentum, b__] :> List[a, b]}}];
                FunKitDebug[5, "        tmp =  ", tmp];
                (*Grab one of the momenta which is a loopMomentum *)
                If[flag,
                    mom = Select[tmp, MatchQ[#, loopMomentum[_, True]]&];
                ];
                If[Not @ flag || Length[mom] === 0,
                    mom = Select[tmp, MatchQ[#, loopMomentum[_, False]]&];
                    If[Length[mom] === 0,
                        mom = Select[tmp, MatchQ[#, loopMomentum[_, True]]&];
                    ];
                ];
                mom = mom[[1]];
                (*now build the replacement rule*)
                momRepl = Solve[Total[subObj[[2, All, 1]]] == 0, mom][[1, 1]];
                (*if the given momentum is NOT a fermionic one, we will need to replace all the momenta on the right-hand-side with NOT fermionic ones*)
                If[Not @ mom[[2]],
                    rightMomenta = Cases[momRepl[[2]], loopMomentum[__, True], Infinity] // DeleteDuplicates;
                    rightMomenta = Map[# -> Head[#][#[[1]], False]&, rightMomenta];
                    subObj = subObj /. rightMomenta;
                    objects = objects /. rightMomenta;
                    ret = ret /. rightMomenta;
                    momRepl = Solve[Total[subObj[[2, All, 1]]] == 0, mom][[1, 1]];
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
            If[Length[subObj[[2, All, 1]]] < 2,
                Continue[]
            ];
            (*Check the conservation of momentum at all vertices*)
            If[Total[subObj[[2, All, 1]]] =!= 0,
                Message[FRoute::momentaFailed, Total[subObj[[2, All, 1]]]];
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
        results = DropFExAnnotations[expr];
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
        Return @ FUnroute[assoc["Expression"] /. Map[#[[2]] -> #[[1]]&, assoc["ExternalIndices"]]];
    ];

FUnroute[setup_, assoc_Association] /; isRoutedAssociation[assoc] :=
    FEx @@ (FUnroute[setup, #]& /@ (List @@ assoc));

FUnroute[setup_, term_FEx] :=
    FUnroute[setup, #]& /@ term;

FUnroute[setup_, term_FTerm] :=
    Module[{fw, bw},
        {fw, bw} = GetSuperIndexTermTransformations[setup, term];
        Return[term // fw];
    ];
