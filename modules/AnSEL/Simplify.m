(**********************************************************************************
    Simplify.m -- Diagram identification and simplification

    Public API:
      FBuildSymmetryList          -- Constructs field permutations from symmetry groups
      FMergeSymmetries            -- Merges two symmetry lists
      FMakeSymmetryList          -- Builds symmetry list from fields and their types
      FSimplify                  -- Simplifies FEx by identifying equivalent diagrams
      FSimplifyNoSym             -- Simplifies FEx without symmetry information

    Internal:
      StartPoints                -- Finds viable starting points for diagram comparison
                                    (used by TermsEqualAndSum)
      IterateDiagram             -- Traverses a diagram along closed indices
                                    (used by TermsEqualAndSum)
      TermsEqualAndSum           -- Tests if two FTerms are equivalent and sums them
                                    (used by SubFSimplify)
      RearrangeFields            -- Rearranges standalone fields to match equivalence
                                    (used by TermsEqualAndSum)
      FTermContent               -- Extracts notation-agnostic field content key
                                    (used by SeparateTermGroups)
      SeparateTermGroups         -- Groups FTerms by field content for simplification
                                    (used by FSimplify)
      SubFSimplify               -- Simplifies a group of same-content FTerms
                                    (used by FSimplify)
**********************************************************************************)

(* Construct all permutations of fields in a derivativeList and their prefactors, given a list of symmetries *)

FBuildSymmetryList::invalidSymmetries = "Symmetries must be given as a list.";

FBuildSymmetryList::invalidSymmetryFormat = "Symmetries must be given as a list of lists.";

FBuildSymmetryList::invalidSymmetry = "The symmetry `1` is not a valid symmetry.";

FBuildSymmetryList::invalidCycle = "The cycle `1` is not a valid cycle.";

FBuildSymmetryList[setup_, symmetries_, derivativeList_] :=
    Module[{procDerList, buildOneSymmetry},
        If[Head[symmetries] =!= List,
            Message[FBuildSymmetryList::invalidSymmetries];
            Abort[]
        ];
        If[AnyTrue[symmetries, Not[Head[#] === List]&],
            Message[FBuildSymmetryList::invalidSymmetryFormat];
            Abort[]
        ];
        If[Length[symmetries] === 0 || symmetries === {{}},
            Return[{}]
        ];
        If[Length[derivativeList] === 0,
            Return[{}]
        ];
        procDerList = derivativeList /. unreplFields[setup];
        buildOneSymmetry[sym_] :=
            Module[{valid = True, buildCycle, pairs},
                If[AnyTrue[sym[[ ;; -2]], Not[Head[#] === List]&],
                    valid = False
                ];
                pairs = Subsets[sym[[ ;; -2]], {2}];
                valid = Not @ AnyTrue[Map[ContainsAny[#[[1]], #[[2]]]&, pairs], Identity];
                If[Not @ valid,
                    Message[FBuildSymmetryList::invalidSymmetry, sym];
                    Abort[]
                ];
                buildCycle[cyc_] :=
                    Module[{cycvalid = True, numberRules, idx, nextIdx},
                        If[AnyTrue[cyc, Not[IntegerQ[#]]&],
                            cycvalid = False
                        ];
                        If[AnyTrue[cyc, (# > Length[derivativeList]) || (# < 1)&],
                            cycvalid = False
                        ];
                        If[Not @ cycvalid,
                            Message[FBuildSymmetryList::invalidCycle, cyc];
                            Abort[]
                        ];
                        numberRules = {};
                        For[idx = 1, idx <= Length[cyc], idx++,
                            nextIdx = Mod[(idx), Length[cyc]] + 1;
                            numberRules = Join[numberRules, {{cyc[[idx]], cyc[[nextIdx]]}}];
                        ];
                        Return[Map[procDerList[[#[[1]], 1]] -> procDerList[[#[[2]], 1]]&, numberRules]];
                    ];
                <|"Rule" -> Flatten[Map[buildCycle, sym[[ ;; -2]]], 1], "Factor" -> sym[[-1]]|>
            ];
        Return @ Join[{<|"Rule" -> {}, "Factor" -> 1|>}, Map[buildOneSymmetry, symmetries /. Cycles -> Identity]];
    ];

(*Merge Symmetry lists*)

FMergeSymmetries[sym1_, sym2_] :=
    Module[{symCombine},
        Return[Join[sym1, sym2] // DeleteDuplicates];
        (* I am not sure we want to automatically blow up the number of rules*)
        symCombine[a_, b_] :=
            Module[{ret},
                ret = Join[a["Rule"] /. b["Rule"], b["Rule"]] // DeleteDuplicates;
                (* remove any trivial rules*)
                ret = Sort @ Select[ret, Not @ MatchQ[#, HoldPattern[a_ -> a_]]&];
                Return[<|"Rule" -> ret, "Factor" -> a["Factor"] * b["Factor"]|>];
            ];
        Return[
            Outer[symCombine, sym1, sym2] //
            Flatten //
            DeleteDuplicates
        ];
    ];

(*Build a symmetry list from a set of fields*)

FMakeSymmetryList[f___] :=
    (
        Message[FunKit::invalidArguments, FMakeSymmetryList];
        Abort[]
    );

FMakeSymmetryList[setup_, {fields___}] /; AllTrue[{fields}, Length[#] == 1&] :=
    FMakeSymmetryList[setup, Head[#]& /@ {fields}, #[[1]]& /@ {fields}];

FMakeSymmetryList::fieldIndexMismatch = "Number of fields and indices must be equal.";

FMakeSymmetryList[setup_, {fields___}, {indices___}] :=
    Module[{symmetries, subSymmetries, cycles, fieldsWPos, curField, idx, curFieldList, symCombine},
        AssertFSetup[setup];
        If[Length[{fields}] =!= Length[{indices}],
            Message[FMakeSymmetryList::fieldIndexMismatch];
            Abort[]
        ];
        (*First, annotate all fields with their position and index, then sort them into sets of identical fields*)
        fieldsWPos = Table[{{fields}[[idx]], {indices}[[idx]], idx}, {idx, 1, Length[{fields}]}];
        fieldsWPos = GatherBy[fieldsWPos, First];
        (*First, for all bosonic fields, we can just construct all possible cycles*)
        subSymmetries = Table[{}, {Length[fieldsWPos]}];
        Do[
            curField = fieldsWPos[[idx, 1]];
            curFieldList = Map[#[[1]][#[[2]]]&, fieldsWPos[[idx]]];
            If[IsCommuting[setup, curField[[1]]],
                (*Build all possible cycles for the positions of this field*)
                cycles = Map[Join[# /. Cycles -> Sequence, {1}]&, PermutationCycles /@ Permutations[Range[Length[fieldsWPos[[idx]]]]]];
                subSymmetries[[idx]] = FBuildSymmetryList[setup, cycles, curFieldList] // DeleteDuplicates;
            ];
            If[IsGrassmann[setup, curField[[1]]],
                (*For fermionic fields, we can only swap pairs, introducing a -1 factor*)
                cycles = Map[Join[# /. Cycles -> Sequence, {-1}]&, PermutationCycles /@ Permutations[Range[Length[fieldsWPos[[idx]]]]]];
                cycles = Select[cycles, Length[#[[1]]] == 2&];
                subSymmetries[[idx]] = FBuildSymmetryList[setup, cycles, curFieldList] // DeleteDuplicates;
                subSymmetries[[idx]] = Join[{<|"Rule" -> {}, "Factor" -> 1|>}, subSymmetries[[idx]]];
            ];
            ,
            {idx, 1, Length[fieldsWPos]}
        ];
        (*Merging two rules*)
        symCombine[a_, b_] :=
            Module[{ret},
                ret = Sort @ Join[a["Rule"], b["Rule"]];
                Return[<|"Rule" -> ret, "Factor" -> a["Factor"] * b["Factor"]|>];
            ];
        (*Trivial rule*)
        symmetries = {<|"Rule" -> {}, "Factor" -> 1|>};
        Do[symmetries = Outer[symCombine, symmetries, subSymmetries[[idx]]] // Flatten, {idx, 1, Length[fieldsWPos]}];
        symmetries = symmetries // DeleteDuplicates;
        If[Length[symmetries] === 0,
            Return[{}]
        ];
        Return[symmetries];
    ];

(*Get viable starting points for a comparison of two diagrams*)

StartPoints[setup_, t1_FTerm, t2_FTerm, cidx1_, cidx2_] :=
    Module[{obj1, obj2, count, desired, sList, match1, match2, doFields, fieldKey},
        doFields = replFields[setup];
(*Get all sub-objects inside the terms. Apply replFields only to non-indexed objects
  (standalone field applications) to avoid corrupting indexed-object structure in NotationB.*)
        obj1 =
            Reverse @
                Sort @
                    Map[
                        If[indexedObjectQ[#],
                            #
                            ,
                            # /. doFields
                        ]&
                        ,
                        ExtractObjectsWithIndex[setup, t1]
                    ];
        obj2 =
            Reverse @
                Sort @
                    Map[
                        If[indexedObjectQ[#],
                            #
                            ,
                            # /. doFields
                        ]&
                        ,
                        ExtractObjectsWithIndex[setup, t2]
                    ];
        FunKitDebug[4, "StartPoints: Comparing objects ", obj1, " and ", obj2];
        (*Build a notation-agnostic field-content key for each object*)
        fieldKey[obj_] := Head[obj] @@ Sort @ getFields[obj];
        (*If the objects (with field content) do not match, they are not identical.*)
        If[Sort @ Map[fieldKey, obj1] =!= Sort @ Map[fieldKey, obj2],
            FunKitDebug[4, "Failed at object head check"];
            Return[{False, Null, Null}]
        ];
        If[Length[cidx1] =!= Length[cidx2],
            FunKitDebug[4, "Failed at closed index count check: ", Length[cidx1], " vs ", Length[cidx2]];
            Return[{False, Null, Null}]
        ];
        (*Otherwise, we check which object is the "rarest"*)
        sList = Map[fieldKey, obj1];
        count = Counts[sList];
        desired = Keys[count][[PositionSmallest[Values[count]][[1]]]];
        match1 = Select[obj1, (fieldKey[#] === desired)&];
        match2 = Select[obj2, (fieldKey[#] === desired)&];
        (*return all possible starting points *)
        Return[{True, match1, match2}]
    ];

(*Find all objects following the closed indices attached to the object curPos*)

IterateDiagram::noFollowObject = "No follow object could be found for index `1` in the diagram.";

IterateDiagram[setup_Association, allObj_, closedIndices_, openIndices_, curPos_, entryIdx_] :=
    Module[{otherIndices, followObjects, i, candidates},
        FunKitDebug[4, "Inspecting: ", curPos];
        (*All indices except the one we entered with*)
        FunKitDebug[4, "Entry index: ", entryIdx];
        otherIndices = DeleteCases[makePosIdx /@ getIndices[curPos], entryIdx];
        otherIndices = Intersection[otherIndices, closedIndices];
        FunKitDebug[4, "Found outgoing indices: ", otherIndices];
        (*all objects containing the otherIndices*)
        followObjects =
            Table[
                candidates = Select[DeleteCases[allObj, curPos], MemberQ[getIndices[#], otherIndices[[i]], Infinity]&];
                If[Length[candidates] === 0,
                    Message[IterateDiagram::noFollowObject, otherIndices[[i]]];
                    Abort[]
                ];
                candidates[[1]]
                ,
                {i, 1, Length[otherIndices]}
            ];
        FunKitDebug[3, "Found followObjects: ", followObjects];
        Return[{otherIndices, followObjects}]
    ];

(*maximum accepted loop length.*)

$MaxIterLoop = 100;

TermsEqualAndSum::exceededLoopLimit = "Exceeded the maximum allowed length of a loop! (" <> ToString[$MaxIterLoop] <> ")";

TermsEqualAndSum::branchFailure = "Arrived at unhandled branch point";

TermsEqualAndSum[
    setup_
    ,
    it1_
    ,(* Original term 1 *)
    it2_
    ,(* Original term 2 *)
    MallObjt1_
    ,
    cidxt1_
    ,
    oidxt1_
    ,
    Mmemory1_
    ,
    entry1_
    ,(* Index at which we start in t1 *)
    MallObjt2_
    ,
    Mcidxt2_
    ,
    oidxt2_
    ,
    Mmemory2_
    ,
    entry2_
    , (* Index at which we start in t2 *)
    Msign2_
] :=
    Module[{t1 = it1, t2 = it2, nt1, nt2, allObjt1 = MallObjt1, curIdx1, curPos1, nextInd1, nextPos1, memory1 = Mmemory1, assocFields1, allObjt2 = MallObjt2, curIdx2, curPos2, nextInd2, nextPos2, memory2 = Mmemory2, assocFields2, sign2 = Msign2, iter = 1, idx, jdx, viableBranches, branchSign, branchItRepl, branchObj, temp1, temp2, cidxt2 = Mcidxt2, curIdxRepl, ncidxt2, noidxt2, nmemory2, allIdxRepl = {}, nallIdxRepl, nallIdxReplNew},
        FunKitDebug[3, "Following along a chain of indices."];
        curIdx1 = makePosIdx @ entry1;
        curIdx2 = makePosIdx @ entry2;
        curPos1 = memory1[[-1]];
        curPos2 = memory2[[-1]];
        While[
            iter < $MaxIterLoop
            ,
            (*Take a single step forward in the terms*)
            {nextInd1, nextPos1} = IterateDiagram[setup, allObjt1, cidxt1, oidxt1, curPos1, curIdx1];
            {nextInd2, nextPos2} = IterateDiagram[setup, allObjt2, cidxt2, oidxt2, curPos2, curIdx2];
            (*If the (set of) next object(s) is different for 1 and 2, we can immediately abort.*)
            If[Sort @ Map[Head[#][Sort[getFields[#]]]&, nextPos1] =!= Sort @ Map[Head[#][Sort[getFields[#]]]&, nextPos2],
                FunKitDebug[3, "FAILURE ------------ Heads do not match: ", nextPos1, ", ", nextPos2];
                Return[{False, allObjt2, t2, allIdxRepl}]
            ];
            (*Check if the external indices in the current object match *)
            If[Intersection[oidxt1, makePosIdx /@ getIndices[curPos1]] =!= Intersection[oidxt2, makePosIdx /@ getIndices[curPos2]],
                FunKitDebug[3, "FAILURE ------------ Current open indices disagree: ", Intersection[oidxt1, makePosIdx /@ getIndices[curPos1]], ", ", Intersection[oidxt2, makePosIdx /@ getIndices[curPos2]]];
                Return[{False, allObjt2, t2, allIdxRepl}]
            ];
            FunKitDebug[3, "Next objects along the chain: ", nextPos1, ", ", nextPos2];
            FunKitDebug[3, "Entering through: ", nextInd1, ", ", nextInd2];
            (*Case 1: There is only a single object following*)
            If[Length[nextInd1] === 1,
                FunKitDebug[3, "-------- CASE 1: Following the index chain."];
                (*Check if the open indices aggree*)
                If[Sort @ Intersection[oidxt1, makePosIdx /@ getIndices[nextPos1[[1]]]] =!= Sort @ Intersection[oidxt2, makePosIdx /@ getIndices[nextPos2[[1]]]],
                    FunKitDebug[3, "FAILURE ------------ Next open indices disagree.", Sort @ Intersection[oidxt1, makePosIdx /@ getIndices[nextPos1[[1]]]], ", ", Sort @ Intersection[oidxt2, makePosIdx /@ getIndices[nextPos2[[1]]]]];
                    Return[{False, allObjt2, t2, allIdxRepl}]
                ];
                (*replace the indices with the ones in curPos1*)
                curIdxRepl =
                    If[nextInd1[[1]] =!= Null && nextInd2[[1]] =!= Null,
                        nextInd2[[1]] -> nextInd1[[1]]
                        ,
                        {}
                    ];
                (*Compose new rule with existing rules to resolve chaining*)
                curIdxRepl = curIdxRepl /. allIdxRepl;
                AppendTo[allIdxRepl, curIdxRepl];
                FunKitDebug[4, "Replacing indices: ", curIdxRepl];
                allObjt2 = allObjt2 /. curIdxRepl;
                memory2 = memory2 /. curIdxRepl;
                curPos2 = curPos2 /. curIdxRepl;
                nextPos2[[1]] = nextPos2[[1]] /. curIdxRepl;
                t2 = t2 /. curIdxRepl;
                sign2 = sign2 /. curIdxRepl;
                cidxt2 = cidxt2 /. curIdxRepl;
                nextInd2[[1]] = nextInd1[[1]];
                (*fix the current object*)
                {temp1, temp2} = RearrangeFields[setup, curPos1, curPos2, {nextInd1[[1]], nextInd2[[1]]}];
                sign2 = sign2 * temp1;
                t2 = t2 /. curPos2 -> temp2;
                (*replace first the object*)
                allObjt2 = allObjt2 /. curPos2 -> temp2;
                memory2 = memory2 /. curPos2 -> temp2;
                curPos2 = temp2;
                (*fix the next object*)
                {temp1, temp2} = RearrangeFields[setup, nextPos1[[1]], nextPos2[[1]], {nextInd1[[1]], nextInd2[[1]]}];
                sign2 = sign2 * temp1;
                allObjt2 = allObjt2 /. nextPos2[[1]] -> temp2;
                memory2 = memory2 /. nextPos2[[1]] -> temp2;
                t2 = t2 /. nextPos2[[1]] -> temp2;
                nextPos2[[1]] = temp2;
                FunKitDebug[4, "New sign: ", sign2];
                (*Check if we closed a loop*)
                If[FirstPosition[memory1, nextPos1[[1]]] === FirstPosition[memory2, nextPos2[[1]]] && NumericQ[FirstPosition[memory1, nextPos1[[1]]][[1]]],
                    FunKitDebug[3, "SUCCESS ------------ Closed a loop."];
                    Return[{sign2, allObjt2, t2, allIdxRepl}]
                ];
                (*Closed one loop, but not the other*)
                If[FirstPosition[memory1, nextPos1[[1]]] =!= FirstPosition[memory2, nextPos2[[1]]],
                    FunKitDebug[3, "FAILURE ------------ Closed only one loop."];
                    Return[{False, allObjt2, t2, allIdxRepl}]
                ];
                (*step forward*)
                curIdx1 = nextInd1[[1]];
                curPos1 = nextPos1[[1]];
                curIdx2 = nextInd2[[1]];
                curPos2 = nextPos2[[1]];
                (*update the memory*)
                AppendTo[memory1, curPos1];
                AppendTo[memory2, curPos2];
                iter++;
                Continue[];
            ];
            (*Case 2: End of the line.*)
            If[Length[nextInd1] === 0,
                FunKitDebug[4, "-------- CASE 2: Finished an index chain in (", curPos1, ", ", curPos2, ")"];
                (*We need to check if both expressions are with FDOps *)
                If[Head @ curPos1 === Field,
                    temp1 = Cases[t1, FDOp[getField[curPos1, 1][getIndex[curPos1, 1]]], Infinity];
                    temp2 = Cases[t2, FDOp[getField[curPos2, 1][getIndex[curPos2, 1]]], Infinity];
                    If[Length[temp1] =!= Length[temp2],
                        FunKitDebug[3, "FAILURE ------------ Number of FDOps is different."];
                        Return[{False, allObjt2, t2, allIdxRepl}]
                    ];
                ];
                FunKitDebug[3, "SUCCESS ------------ Index chain ended with equality."];
                Return[{sign2, allObjt2, t2, allIdxRepl}]
            ];
            (*Case 3: Branching point.*)
            If[Length[nextInd1] > 1,
                FunKitDebug[3, "-------- CASE 3: Index chain is branching."];
                (*We need to build all possible combinations between the "next" indices and follow these separately, until one of them fits.*)
                assocFields1 = getField[curPos1, FirstPosition[getIndices[curPos1], #][[1]]]& /@ nextInd1;
                assocFields2 = getField[curPos2, FirstPosition[getIndices[curPos2], #][[1]]]& /@ nextInd2;
                viableBranches = Map[Transpose[{Transpose @ {nextInd1, assocFields1, nextPos1}, #}]&, Permutations[Transpose @ {nextInd2, assocFields2, nextPos2}]];
                viableBranches = Select[viableBranches, AllTrue[#, (#[[1, 2]] === #[[2, 2]])&]&];
                FunKitDebug[4, "Viable Branches: ", viableBranches];
                For[idx = 1, idx <= Length[viableBranches], idx++,
                    branchSign = sign2;
                    branchObj = allObjt2;
                    ncidxt2 = cidxt2;
                    noidxt2 = oidxt2;
                    nt2 = t2;
                    nmemory2 = memory2;
                    nallIdxRepl = allIdxRepl;
                    Do[
                        curIdxRepl = viableBranches[[idx, jdx, 2, 1]] -> viableBranches[[idx, jdx, 1, 1]];
                        AppendTo[nallIdxRepl, curIdxRepl];
                        FunKitDebug[4, "Replacing indices: ", curIdxRepl];
                        (*Fix the outgoing objects*)
                        {branchSign, branchItRepl} = RearrangeFields[setup, curPos1, curPos2, viableBranches[[idx, jdx, All, 1]]];
                        (*Fix the incoming objects*)
                        {temp1, temp2} = RearrangeFields[setup, viableBranches[[idx, jdx, 1, 3]], viableBranches[[idx, jdx, 2, 3]], viableBranches[[idx, jdx, All, 1]]];
                        branchSign = temp1 * branchSign /. nallIdxRepl;
                        branchObj = branchObj /. curPos2 -> branchItRepl;
                        branchObj = branchObj /. viableBranches[[idx, jdx, 2, 3]] -> temp2;
                        branchObj = branchObj /. nallIdxRepl;
                        nt2 = nt2 /. curPos2 -> branchItRepl /. viableBranches[[idx, jdx, 2, 3]] -> temp2;
                        nt2 = nt2 /. nallIdxRepl;
                        viableBranches[[idx, jdx, 2, 1]] = viableBranches[[idx, jdx, 2, 1]] /. nallIdxRepl;
                        ncidxt2 = ncidxt2 /. nallIdxRepl;
                        noidxt2 = noidxt2 /. nallIdxRepl;
                        nmemory2 = nmemory2 /. nallIdxRepl;
                        viableBranches[[idx, jdx, 2, 3]] = temp2 /. nallIdxRepl;
                        FunKitDebug[4, "Branching at ", branchObj];
                        {branchSign, branchObj, nt2, nallIdxReplNew} = TermsEqualAndSum[setup, t1, nt2, allObjt1, cidxt1, oidxt1, Append[memory1, viableBranches[[idx, jdx, 1, 3]]], viableBranches[[idx, jdx, 1, 1]], branchObj, ncidxt2, noidxt2, Append[memory2 /. curPos2 -> branchItRepl, viableBranches[[idx, jdx, 2, 3]]] /. nallIdxRepl, viableBranches[[idx, jdx, 2, 1]], branchSign];
                        nallIdxRepl = Join[nallIdxRepl, nallIdxReplNew] // DeleteDuplicates;
                        FunKitDebug[6, "Returned from branch call ", jdx, " of ", Length[viableBranches[[idx]]]];
                        If[branchSign === False,
                            Break[]
                        ];
                        ,
                        {jdx, 1, Length[viableBranches[[idx]]]}
                    ];
                    If[branchSign === False,
                        Continue[]
                    ];
                    FunKitDebug[3, "SUCCESS ------------ Branch ", idx, " succeeded, branchSign is ", branchSign];
                    Return[{branchSign, branchObj, nt2, nallIdxRepl}];
                ];
                FunKitDebug[3, "FAILURE ------------ Branch failed."];
                Return[{False, allObjt2, t2, allIdxRepl}];
            ];
            (*Nothing should lead here*)
            Message[TermsEqualAndSum::branchFailure];
            Abort[];
        ];
        (*Nothing should lead here*)
        Message[TermsEqualAndSum::exceededLoopLimit];
        Abort[];
    ];

RearrangeFields[setup_, t1_, t2_, equiv_] :=
    Module[
        {ipos1, ipos2, idx, sign, newt2, nf1, ni1, nf2, ni2}
        ,
(* Given two objects t1, t2, re-order the fields in the indexed object t2,
so that the exit index equivalently fits the position in t1.
Returns both the sign and the reordered t2*)
        If[equiv[[1]] === Null || equiv[[2]] === Null,
            Return[{1, t2}]
        ];
        nf1 = getFields[t1];
        ni1 = getIndices[t1];
        nf2 = getFields[t2];
        ni2 = getIndices[t2];
        ipos1 = FirstPosition[makePosIdx /@ ni1, equiv[[1]]][[1]];
        ipos2 = FirstPosition[makePosIdx /@ ni2, equiv[[2]]][[1]];
        (*nothing to do:*)
        If[ipos1 === ipos2,
            Return[{1, t2}]
        ];
        sign =
            If[ipos2 > ipos1,
                (*commute pos2 backwards*)
                Table[makeObj[FMinus, {nf2[[ipos2]], nf2[[ipos2 - idx]]}, {ni2[[ipos2]], ni2[[ipos2 - idx]]}], {idx, 1, ipos2 - ipos1}]
                ,
                (*commute pos2 forwards*)
                Table[makeObj[FMinus, {nf2[[ipos2]], nf2[[ipos2 + idx]]}, {ni2[[ipos2]], ni2[[ipos2 + idx]]}], {idx, 1, ipos1 - ipos2}]
            ];
        (*Resolve the resulting FMinus, if possible*)
        sign = Times @@ ReduceIndices[setup, FTerm @@ sign];
        (*Replace the indices & fields in t2*)
        newt2 = makeObj[Head[t2], Insert[Delete[nf2, ipos2], nf2[[ipos2]], ipos1], Insert[Delete[ni2, ipos2], ni2[[ipos2]], ipos1]];
        FunKitDebug[4, "Given ", t1, ", rearranged ", t2, " to ", newt2, " with sign ", sign];
        Return[{sign, newt2}];
    ];

TermsEqualAndSum::undeterminedFields = "Error: Cannot equate terms if they are not fully truncated, i.e. contain instances of AnyField.";

(* This is the main function for checking if two diagrams are equal to each other. Returns either False, or the sum of the two terms *)

(* Preprocessed variant: terms already have ReduceIndices applied.
   Skips only ReduceIndices; still does FixIndices+FOrderFields for correct index naming. *)

(* TermsEqualAndSumPre: terms are assumed already normalized (FixIndices + FOrderFields)
   by SubFSimplify before the pairwise loop. No redundant re-normalization. *)
TermsEqualAndSumPre[setup_, it1_FTerm, it2_FTerm] :=
    TermsEqualAndSumCore[setup, it1, it2];

TermsEqualAndSum[setup_, it1_FTerm, it2_FTerm] :=
    Module[
        {t1 = ReduceIndices[setup, it1], t2 = ReduceIndices[setup, it2]}
        ,
        (*Start off by default-ordering t1 and t2*)
        t1 = FixIndices[setup, FOrderFields[setup, t1]];
        t2 = FixIndices[setup, FOrderFields[setup, t2]];
        (*Skip comparison for intermediate terms that still contain unresolved AnyField placeholders.*)
        If[!FreeQ[it1, AnyField] || !FreeQ[it2, AnyField],
            Return[False]
        ];
        TermsEqualAndSumCore[setup, t1, t2]
    ];

TermsEqualAndSumCore[setup_, t1_FTerm, t2_FTerm] :=
    Module[
        {nt1, nt2, curIdx1, curIdx2, curIdxRepl, startPoints, doFields, allObjt1, allObjt2, cidxt1, cidxt2, oidxt1, oidxt2, startt1, startt1fields, cidxstartt1, startt2, nstartt2, startt2fields, cidxstartt2, branchAllObjt2, idx, jdx, equal = False, startsign, a, factor, removeOther, fac1, fac2, terms1, terms2, nallIdxReplNew, tmp}
        ,
        (*Briefly check the trivial case*)
        FunKitDebug[4, "    TermsEqualAndSum: Comparing \n  ", t1, "\n   &\n  ", t2];
        If[t1 === t2,
            FunKitDebug[3, "    Terms are identical, returning FTerm[2, t1]."];
            Return @ FTerm[2, t1]
        ];
        If[Length[t1] >= 2 && Length[t2] >= 2,
            If[t1[[2 ;; ]] === t2[[2 ;; ]] && FreeQ[{t1[[1]]}, Alternatives @@ $indexedObjects, Infinity],
                FunKitDebug[3, "    Terms are identical starting with t1[[2 ;; ]], returning FTerm[t1[[1]] + t1[[2]], t1[[2 ;; ]]]."];
                Return @ FTerm[t1[[1]] + t2[[1]], t1[[2 ;; ]]]
            ];
            If[t1[[2 ;; ]] === t2[[1 ;; ]] && FreeQ[{t1[[1]]}, Alternatives @@ $indexedObjects, Infinity],
                FunKitDebug[3, "    Terms are identical starting with t1[[2 ;; ]], returning FTerm[t1[[1]] + 1, t1[[2 ;; ]]]."];
                Return @ FTerm[t1[[1]] + 1, t1[[2 ;; ]]]
            ];
            If[t1[[1 ;; ]] === t2[[2 ;; ]] && FreeQ[{t1[[1]]}, Alternatives @@ $indexedObjects, Infinity],
                FunKitDebug[3, "    Terms are identical starting with t1[[1 ;; ]], returning FTerm[1 + t2[[1]], t1[[1 ;; ]]]."];
                Return @ FTerm[1 + t2[[1]], t1[[1 ;; ]]]
            ];
        ];
        (*Let's obtain closed superindices*)
        cidxt1 = GetClosedSuperIndices[setup, t1];
        cidxt2 = GetClosedSuperIndices[setup, t2];
        If[Length[cidxt1] =!= Length[cidxt2],
            FunKitDebug[3, "    Different number of closed indices: ", Length[cidxt1], " vs ", Length[cidxt2]];
            Return[False]
        ];
        If[Length[cidxt1] == 0,
            (*We had the identity already above, so nothing to do here*)
            Return[False]
        ];
        (*Get all the possible starting points for the search — pass pre-computed closed indices*)
        startPoints = StartPoints[setup, t1, t2, cidxt1, cidxt2];
        If[Not[startPoints[[1]]],
            FunKitDebug[3, "    No matching StartPoints could be identified"];
            Return[False]
        ];
        FunKitDebug[4, "Collected StartPoints"];
        doFields = replFields[setup];
        (*collect objects for both terms; apply replFields only to non-indexed objects*)
        allObjt1 =
            Select[
                Map[
                    If[indexedObjectQ[#],
                        #
                        ,
                        # /. doFields
                    ]&
                    ,
                    ExtractObjectsWithIndex[setup, t1]
                ]
                ,
                FreeQ[FMinus[__]]
            ];
        allObjt2 =
            Select[
                Map[
                    If[indexedObjectQ[#],
                        #
                        ,
                        # /. doFields
                    ]&
                    ,
                    ExtractObjectsWithIndex[setup, t2]
                ]
                ,
                FreeQ[FMinus[__]]
            ];
        oidxt1 = GetOpenSuperIndices[setup, t1];
        oidxt2 = GetOpenSuperIndices[setup, t2];
        (*We pick the first candidate for t1 and iterate over all candidates for t2.*)
        startt1 = startPoints[[2, 1]];
        (*starting indices can only be closed indices! We pick these out with the following 4 commands*)
        startt1fields = getFields[startt1];
        cidxstartt1 = Map[MemberQ[cidxt1, makePosIdx @ #]&, getIndices[startt1]];
        startt1fields = Pick[startt1fields, cidxstartt1];
        cidxstartt1 = makePosIdx /@ Pick[getIndices[startt1], cidxstartt1];
        (*Sanity check*)
        If[Length[cidxstartt1] === 0,
            Return[False]
        ];
        FunKitDebug[3, "Comparing the terms \n  ", t1, "\n  ", t2];
        If[Length[Intersection[getIndices[startt1], cidxt1]] == 1,
            (*If there are no other closed indices, the current index is outgoing and we should not mark it as entry*)
            curIdx1 = Null
            ,
            curIdx1 = cidxstartt1[[1]];
        ];
        (*If the terms are equal for any starting candidates for t2, we have succeeded*)
        For[idx = 1, idx <= Length[startPoints[[3]]], idx++,
            startt2 = startPoints[[3, idx]];
            (*We need to identify all possible insertion points in t2 that fit the insertion in t1*)
            (*starting indices can only be 1. closed indices 2. have same field content as the starting point in t1. We pick these out with the following 2 commands*)
            cidxstartt2 = Map[(MemberQ[cidxt2, #[[1]]] && #[[2]] === startt1fields[[1]])&, Transpose[{makePosIdx /@ getIndices[startt2], getFields[startt2]}]];
            cidxstartt2 = Pick[makePosIdx /@ getIndices[startt2], cidxstartt2];
            (*Loop over all possible starting indices*)
            FunKitDebug[4, "We have: ", Length[cidxstartt2], " possible starting indices in t2."];
            For[jdx = 1, jdx <= Length[cidxstartt2], jdx++,
                If[Length[Intersection[getIndices[startt2], cidxt2]] == 1,
                    (*If there are no other closed indices, the current index is outgoing and we should not mark it as entry*)
                    curIdx2 = Null
                    ,
                    curIdx2 = cidxstartt2[[jdx]];
                ];
                curIdxRepl =
                    If[curIdx1 =!= Null && curIdx2 =!= Null,
                        curIdx2 -> curIdx1
                        ,
                        {}
                    ];
                FunKitDebug[4, "Replacing indices: ", curIdxRepl];
                (*re-order the starting point so that it fits the first.*)
                {startsign, nstartt2} = RearrangeFields[setup, startt1, startt2, {curIdx1, curIdx2}];
                branchAllObjt2 = allObjt2 /. startt2 -> nstartt2;
                (*if nstartt2 (first object) has only one field, set the start index to Null*)
                (*iterate the diagram*)
                FunKitDebug[3, "Starting sign: ", startsign /. curIdxRepl];
                FunKitDebug[3, "Starting point replacement: ", curIdxRepl];
                FunKitDebug[3, "StartPoints: \n  ", startt1, "\n  ", nstartt2 /. curIdxRepl];
                FunKitDebug[3, "StartIndices: \n  ", curIdx1, "\n  ", curIdx2 /. curIdxRepl];
                {equal, branchAllObjt2, nt2, nallIdxReplNew} = TermsEqualAndSum[setup, t1, t2 /. curIdxRepl, allObjt1, cidxt1, oidxt1, {startt1}, curIdx1, branchAllObjt2 /. curIdxRepl, cidxt2 /. curIdxRepl, oidxt2 /. curIdxRepl, {nstartt2} /. curIdxRepl, curIdx2 /. curIdxRepl, startsign /. curIdxRepl];
                FunKitDebug[6, "Returned from main call"];
                FunKitDebug[3, "Finished pass ", jdx, " with equal=", equal];
                (*If we found an equality, break out*)
                If[equal =!= False,
                    Break[]
                ];
            ];
            If[equal =!= False,
                Break[]
            ];
        ];
        (*If equal===False, the terms are clearly not equal*)
        If[equal === False,
            Return[False];
        ];
        (*No need to do any ordering if there are no explicit Grassmanns in the expression*)
        If[GrassmannCount[setup, t1] === 0,
            FunKitDebug[2, "Found two equal terms"];
            {fac1, terms1} = SplitPrefactor[setup, t1];
            {fac2, terms2} = SplitPrefactor[setup, nt2];
            factor = fac1 + equal * fac2;
            factor = ReduceIndices[setup, FTerm[factor]];
            FunKitDebug[2, "With prefactor: ", factor];
            Return @ FTerm[factor, terms1];
        ];
        FunKitDebug[2, "STOPPING HERE: Need to resolve Grassmann factors!"];
        Return[False];
        Print[Style["FATAL: Could not resolve Grassmann factors", Red]];
        Abort[];
    ];

(**********************************************************************************
    Identification of sums of diagrams
**********************************************************************************)

FTermContent[setup_, term_FTerm] :=
    Module[{objs},
        objs = FunKit`Private`ExtractObjectsWithIndex[setup, term];
        Hash[Sort @ Map[Head[#] @@ FunKit`Private`getFields[#]&, objs], "SHA"]
    ];

(* Given an FEx, subdivide its FTerms into groups with identical content *)

SeparateTermGroups[setup_, expr_] :=
    Module[
        {ret = List @@ expr, identifierRep, removeFirsts, groupedDiagrams}
        ,
        (*We group all diagrams into groups that could be potentially identical. We simply make sure that in each group all diagrams have the same objects.*)
        identifierRep = Map[FTermContent[setup, #]&, ret];
        identifierRep = Thread[{identifierRep, ret}];
        removeFirsts[ex_] := Map[#[[2]]&, ex];
        groupedDiagrams = Map[removeFirsts, GatherBy[identifierRep, #[[1]]&]];
        FunKitDebug[2, "Separated into ", Length[groupedDiagrams], " groups."];
        Return[groupedDiagrams]
    ];

(* Withing a group of possibly matching FTerms, check for any possible equalities *)

SubFSimplify[setup_, expr_] /; Length[expr] > 64 :=
    Module[{chunks, ret, temp},
        temp = PrintTemporary[Style["WARNING: FSimplify called on a large expression. This may take a while.", Orange]];
        chunks = Partition[List @@ expr, UpTo[48]];
        ret = Flatten[PMap[SubFSimplify[setup, #]&, chunks]];
        NotebookDelete[temp];
        Return[SubFSimplify[setup, ret]];
    ];

SubFSimplify[setup_, expr_] /; Length[expr] <= 64 :=
    Module[
        {ret = List @@ expr, idx, jdx, red}
        ,
        (* Preprocess: ReduceIndices, then re-normalize once (γ resolution may change indices) *)
        ret = ReduceIndicesBatch[setup, ret];
        ret = OrderFields[setup, FixIndices[setup, #]& /@ ret];
        For[idx = 1, idx <= Length[ret], idx++,
            For[jdx = idx + 1, jdx <= Length[ret], jdx++,
                red = TermsEqualAndSumPre[setup, ret[[idx]], ret[[jdx]]];
                FunKitDebug[3, "Compared ", idx, " and ", jdx, ", result: ", red];
                If[red =!= False,
                    ret[[idx]] = red;
                    ret = Delete[ret, jdx];
                    jdx--;
                ];
            ];
        ];
        Return[ret];
    ];

(* Withing a group of possibly matching FTerms, check for any possible equalities, but this time with a given list of symmetries *)

SubFSimplify[setup_, expr_, symmetryList_] /; Length[expr] > 64 :=
    Module[{chunks, ret, temp},
        temp = PrintTemporary[Style["WARNING: FSimplify called on a large expression with symmetries. This may take a while.", Orange]];
        chunks = Partition[List @@ expr, UpTo[48]];
        ret = Flatten[PMap[SubFSimplify[setup, #, symmetryList]&, chunks]];
        NotebookDelete[temp];
        Return[SubFSimplify[setup, ret, symmetryList]];
    ];

SubFSimplify[setup_, expr_, symmetryList_] /; Length[expr] <= 64 :=
    Module[
        {ret = List @@ expr, idx, jdx, kdx, red, preprocess, t2sym}
        ,
        (* Preprocess for symmetry-transformed terms (which are newly created and need normalization) *)
        preprocess = FixIndices[setup, FOrderFields[setup, ReduceIndices[setup, #]]]&;
        (* Normalize the group once *)
        ret = ReduceIndicesBatch[setup, ret];
        ret = OrderFields[setup, FixIndices[setup, #]& /@ ret];
        For[idx = 1, idx <= Length[ret], idx++,
            For[jdx = idx + 1, jdx <= Length[ret], jdx++,
                For[kdx = 1, kdx <= Length[symmetryList], kdx++,
                    t2sym = preprocess[FTerm[symmetryList[[kdx, Key["Factor"]]]] ** ret[[jdx]] /. symmetryList[[kdx, Key["Rule"]]]];
                    red = TermsEqualAndSumPre[setup, ret[[idx]], t2sym];
                    FunKitDebug[3, "Compared ", idx, " and ", jdx, ", result: ", red];
                    If[red =!= False,
                        ret[[idx]] = red;
                        ret = Delete[ret, jdx];
                        jdx--;
                        kdx = Length[symmetryList] + 1;
                    ];
                ];
            ];
        ];
        Return[ret];
    ];

(**********************************************************************************
    FSimplify, as exported by FunKit.
**********************************************************************************)

FSimplifyNoSym[setup_, expr_] :=
    Module[{subGroups, res, useParallel},
        FunKitDebug[1, "Simplifying diagrammatic expression of length ", Length[expr]];
        subGroups = SeparateTermGroups[setup, expr];
        useParallel = AllTrue[subGroups, Length[#] <= 64&] && ($FunKitDebugLevel <= 2);
        If[useParallel,
            res = FEx @@ Flatten[PMap[SubFSimplify[setup, #]&, subGroups]];
            ,
            res = FEx @@ Flatten[Map[SubFSimplify[setup, #]&, subGroups]];
        ];
        FunKitDebug[1, "FTerms before: ", Length[expr], ", after: ", Length[res]];
        Return[res];
    ];

FSimplify::disconnected = "The expression contains disconnected diagrams. These will be skipped during simplification.";

Options[FSimplify] = {"Symmetries" -> {}};

FSimplify[setup_, inexpr_FEx, OptionsPattern[]] :=
    Module[{subGroups, res, expr, annotations, useParallel, symmetries, connectedTerms, disconnectedTerms, connectedExpr},
        AssertFSetup[setup];
        {expr, annotations} = SeparateFExAnnotations[inexpr];
        (* Guard: separate disconnected diagrams and skip them *)
        disconnectedTerms = Select[expr, FDisconnectedQ[setup, #]&];
        If[Length[disconnectedTerms] > 0,
            Message[FSimplify::disconnected];
            connectedTerms = Select[expr, !FDisconnectedQ[setup, #]&];
            If[Length[connectedTerms] == 0,
                Return[MergeFExAnnotations[FEx @@ expr, annotations]]
            ];
            connectedExpr = FEx @@ connectedTerms;
            res = FSimplify[setup, MergeFExAnnotations[connectedExpr, annotations], (Sequence @@ Thread[Rule @@ {#, OptionValue[FSimplify, #]}]& @ Keys[Options[FSimplify]])];
            Return[FEx @@ Join[List @@ res, disconnectedTerms]]
        ];
        expr = FixIndices[setup, expr];
        expr = FOrderFields[setup, expr];
        symmetries =
            If[KeyExistsQ[annotations, "Symmetries"],
                annotations["Symmetries"]
                ,
                {}
            ];
        symmetries = FMergeSymmetries[symmetries, OptionValue["Symmetries"]];
        FunKitDebug[3, "FSimplify: Using symmetry list ", symmetries];
        If[symmetries === {},
            Return[MergeFExAnnotations[FSimplifyNoSym[setup, expr], annotations]]
        ];
        FunKitDebug[1, "Simplifying diagrammatic expression of length ", Length[expr], "with symmetry list"];
        subGroups = SeparateTermGroups[setup, expr];
        useParallel = AllTrue[subGroups, Length[#] <= 64&] && ($FunKitDebugLevel <= 2);
        If[useParallel,
            res = FEx @@ Flatten[PMap[SubFSimplify[setup, #, Evaluate @ symmetries]&, subGroups]];
            ,
            res = FEx @@ Flatten[Map[SubFSimplify[setup, #, symmetries]&, subGroups]];
        ];
        FunKitDebug[1, "FTerms before: ", Length[expr], ", after: ", Length[res]];
        Return[MergeFExAnnotations[res, annotations]];
    ];

FSimplify[setup_, inexpr_FTerm, OptionsPattern[]] :=
    inexpr;

FSimplify[___] :=
    (
        Message[FunKit::invalidArguments, "FSimplify"];
        Abort[]
    );
