(**********************************************************************************
    Identification of specific diagrams (i.e. FTerms)
**********************************************************************************)

(* Construct all permutations of fields in a derivativeList and their prefactors, given a list of symmetries *)

BuildSymmetryList[setup_, symmetries_, derivativeList_] :=
    Module[{procDerList, buildOneSymmetry},
        If[Head[symmetries] =!= List,
            Print["Symmetries must be given as a list!"];
            Abort[]
        ];
        If[Length[symmetries] == 0,
            Return[{}]
        ];
        If[Length[derivativeList] == 0,
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
                    Print[sym, " is  not a valid symmetry!"];
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
                            Print[cyc, " is  not a valid cycle!"];
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

MergeSymmetries[sym1_, sym2_] :=
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
    Message[FunKit::invalidArguments, FMakeSymmetryList]

FMakeSymmetryList[setup_, {fields___}] /; AllTrue[{fields}, Length[#] == 1&] :=
    FMakeSymmetryList[setup, Head[#]& /@ {fields}, #[[1]]& /@ {fields}];

FMakeSymmetryList[setup_, {fields___}, {indices___}] :=
    Module[{symmetries, subSymmetries, cycles, fieldsWPos, curField, idx, curFieldList, symCombine},
        If[Length[{fields}] =!= Length[{indices}],
            Print["Error in FMakeSymmetryList: Number of fields and indices must be equal!"];
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
                subSymmetries[[idx]] = BuildSymmetryList[setup, cycles, curFieldList] // DeleteDuplicates;
            ];
            If[IsGrassmann[setup, curField[[1]]],
                (*For fermionic fields, we can only swap pairs, introducing a -1 factor*)
                cycles = Map[Join[# /. Cycles -> Sequence, {-1}]&, PermutationCycles /@ Permutations[Range[Length[fieldsWPos[[idx]]]]]];
                cycles = Select[cycles, Length[#[[1]]] == 2&];
                subSymmetries[[idx]] = BuildSymmetryList[setup, cycles, curFieldList] // DeleteDuplicates;
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
        (*Next, for all fermionic fields, we can only swap pairs, introducing a -1 factor*)
        Return[symmetries];
    ];

(*Get viable starting points for a comparison of two diagrams*)

StartPoints[setup_, t1_FTerm, t2_FTerm] :=
    Module[{obj1, obj2, count, desired, sList, match1, match2, cidx1, cidx2, doFields},
        doFields = replFields[setup];
        (*Get all sub-objects inside the terms*)
        obj1 = Reverse @ Sort @ ExtractObjectsWithIndex[setup, t1] /. doFields;
        obj2 = Reverse @ Sort @ ExtractObjectsWithIndex[setup, t2] /. doFields;
        FunKitDebug[4, "StartPoints: Comparing objects ", obj1, " and ", obj2];
        (*If the objects (with field content) do not match, they are not identical.*)
        If[Sort @ Map[Head[#][Sort @ #[[1]]]&, obj1] =!= Sort @ Map[Head[#][Sort @ #[[1]]]&, obj2],
            FunKitDebug[4, "Failed at object head check"];
            Return[{False, Null, Null}]
        ];
        cidx1 = GetClosedSuperIndices[setup, t1];
        cidx2 = GetClosedSuperIndices[setup, t2];
        If[Length[cidx1] =!= Length[cidx2],
            FunKitDebug[4, "Failed at closed index count check: ", Length[cidx1], " vs ", Length[cidx2]];
            Return[{False, Null, Null}]
        ];
        (*Otherwise, we check which object is the "rarest"*)
        sList = Map[Head[#][#[[1]]]&, obj1];
        count = Counts[sList];
        desired = Keys[count][[PositionSmallest[Values[count]][[1]]]];
        match1 = Select[obj1, (Head[#][#[[1]]] === desired)&];
        match2 = Select[obj2, (Head[#][#[[1]]] === desired)&];
        (*return all possible starting points *)
        Return[{True, match1, match2}]
    ];

(*Find all objects following the closed indices attached to the object curPos*)

IterateDiagram[setup_Association, allObj_, closedIndices_, openIndices_, curPos_, entryIdx_] :=
    Module[{otherIndices, followObjects, i},
        FunKitDebug[4, "Inspecting: ", curPos];
        (*All indices except the one we entered with*)
        otherIndices = DeleteCases[makePosIdx /@ curPos[[2]], entryIdx];
        otherIndices = Intersection[otherIndices, closedIndices];
        FunKitDebug[4, "Found outgoing indices: ", otherIndices];
        (*all objects containing the otherIndices*)
        followObjects = Table[Select[DeleteCases[allObj, curPos], MemberQ[#[[2]], otherIndices[[i]], Infinity]&][[1]], {i, 1, Length[otherIndices]}];
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
    cidxt2_
    ,
    oidxt2_
    ,
    Mmemory2_
    ,
    entry2_
    , (* Index at which we start in t2 *)
    Msign2_
] :=
    Module[{t1 = it1, t2 = it2, nt1, nt2, allObjt1 = MallObjt1, curIdx1, curPos1, nextInd1, nextPos1, memory1 = Mmemory1, assocFields1, allObjt2 = MallObjt2, curIdx2, curPos2, nextInd2, nextPos2, memory2 = Mmemory2, assocFields2, sign2 = Msign2, iter = 1, idx, jdx, viableBranches, branchSign, branchItRepl, branchObj, temp1, temp2},
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
            If[Sort @ Map[Head[#][Sort[#[[1]]]]&, nextPos1] =!= Sort @ Map[Head[#][Sort[#[[1]]]]&, nextPos2],
                FunKitDebug[3, "FAILURE ------------ Heads do not match: ", nextPos1, ", ", nextPos2];
                Return[{False, allObjt2, t2}]
            ];
            (*Check if the external indices in the current object match *)
            If[Intersection[oidxt1, makePosIdx /@ (curPos1[[2]])] =!= Intersection[oidxt2, makePosIdx /@ (curPos2[[2]])],
                FunKitDebug[3, "FAILURE ------------ Current open indices disagree: ", Intersection[oidxt1, makePosIdx /@ (curPos1[[2]])], ", ", Intersection[oidxt2, makePosIdx /@ (curPos2[[2]])]];
                Return[{False, allObjt2, t2}]
            ];
            FunKitDebug[3, "Next objects along the chain: ", nextPos1, ", ", nextPos2];
            FunKitDebug[3, "Entering through: ", nextInd1, ", ", nextInd2];
            (*Case 1: There is only a single object following*)
            If[Length[nextInd1] === 1,
                FunKitDebug[3, "-------- CASE 1: Following the index chain."];
                (*Check if the open indices aggree*)
                If[Sort @ Intersection[oidxt1, makePosIdx /@ nextPos1[[1, 2]]] =!= Sort @ Intersection[oidxt2, makePosIdx /@ nextPos2[[1, 2]]],
                    FunKitDebug[3, "FAILURE ------------ Next open indices disagree.", Sort @ Intersection[oidxt1, makePosIdx /@ nextPos1[[1, 2]]], ", ", Sort @ Intersection[oidxt2, makePosIdx /@ nextPos2[[1, 2]]]];
                    Return[{False, allObjt2, t2}]
                ];
                (*replace the indices with the ones in curPos1*)
                FunKitDebug[4, "Replacing index ", nextInd2[[1]], " with ", nextInd1[[1]]];
                allObjt2 = allObjt2 /. nextInd2[[1]] -> nextInd1[[1]];
                memory2 = memory2 /. nextInd2[[1]] -> nextInd1[[1]];
                curPos2 = curPos2 /. nextInd2[[1]] -> nextInd1[[1]];
                nextPos2[[1]] = nextPos2[[1]] /. nextInd2[[1]] -> nextInd1[[1]];
                t2 = t2 /. nextInd2[[1]] -> nextInd1[[1]];
                sign2 = sign2 /. nextInd2[[1]] -> nextInd1[[1]];
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
                    Return[{sign2, allObjt2, t2}]
                ];
                (*Closed one loop, but not the other*)
                If[FirstPosition[memory1, nextPos1[[1]]] =!= FirstPosition[memory2, nextPos2[[1]]],
                    FunKitDebug[3, "FAILURE ------------ Closed only one loop."];
                    Return[{False, allObjt2, t2}]
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
                    temp1 = Cases[t1, FDOp[curPos1[[1, 1]][curPos1[[2, 1]]]], Infinity];
                    temp2 = Cases[t2, FDOp[curPos2[[1, 1]][curPos2[[2, 1]]]], Infinity];
                    If[Length[temp1] =!= Length[temp2],
                        FunKitDebug[3, "FAILURE ------------ Number of FDOps is different."];
                        Return[{False, allObjt2, t2}]
                    ];
                ];
                FunKitDebug[3, "SUCCESS ------------ Index chain ended with equality."];
                Return[{sign2, allObjt2, t2}]
            ];
            (*Case 3: Branching point.*)
            If[Length[nextInd1] > 1,
                FunKitDebug[3, "-------- CASE 3: Index chain is branching."];
                (*We need to build all possible combinations between the "next" indices and follow these separately, until one of them fits.*)
                assocFields1 = curPos1[[1, FirstPosition[curPos1[[2]], #][[1]]]]& /@ nextInd1;
                assocFields2 = curPos2[[1, FirstPosition[curPos2[[2]], #][[1]]]]& /@ nextInd2;
                viableBranches = Map[Transpose[{Transpose @ {nextInd1, assocFields1, nextPos1}, #}]&, Permutations[Transpose @ {nextInd2, assocFields2, nextPos2}]];
                viableBranches = Select[viableBranches, AllTrue[#, (#[[1, 2]] === #[[2, 2]])&]&];
                FunKitDebug[4, "Viable Branches: ", viableBranches];
                For[idx = 1, idx <= Length[viableBranches], idx++,
                    branchSign = sign2;
                    branchObj = allObjt2;
                    Do[
                        (*Fix the outgoing objects*){branchSign, branchItRepl} = RearrangeFields[setup, curPos1, curPos2, viableBranches[[idx, jdx, All, 1]]];
                        (*Fix the incoming objects*)
                        {temp1, temp2} = RearrangeFields[setup, viableBranches[[idx, jdx, 1, 3]], viableBranches[[idx, jdx, 2, 3]], viableBranches[[idx, jdx, All, 1]]];
                        branchSign = temp1 * branchSign;
                        branchObj = branchObj /. curPos2 -> branchItRepl;
                        branchObj = branchObj /. viableBranches[[idx, jdx, 2, 3]] -> temp2;
                        nt2 = t2 /. curPos2 -> branchItRepl /. viableBranches[[idx, jdx, 2, 3]] -> temp2;
                        viableBranches[[idx, jdx, 2, 3]] = temp2;
                        FunKitDebug[4, "Branching at ", branchObj];
                        {branchSign, branchObj, nt2} = TermsEqualAndSum[setup, t1, nt2, allObjt1, cidxt1, oidxt1, Append[memory1, viableBranches[[idx, jdx, 1, 3]]], viableBranches[[idx, jdx, 1, 1]], branchObj, cidxt2, oidxt2, Append[memory2 /. curPos2 -> branchItRepl, viableBranches[[idx, jdx, 2, 3]]], viableBranches[[idx, jdx, 2, 1]], branchSign];
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
                    Return[{branchSign, branchObj, nt2}];
                ];
                FunKitDebug[3, "FAILURE ------------ Branch failed."];
                Return[{False, allObjt2, t2}];
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
        {ipos1, ipos2, idx, sign, newt2}
        ,
(* Given two objects t1, t2, re-order the fields in the indexed object t2,
so that the exit index equivalently fits the position in t1.
Returns both the sign and the reordered t2*)
        ipos1 = FirstPosition[makePosIdx /@ t1[[2]], equiv[[1]]][[1]];
        ipos2 = FirstPosition[makePosIdx /@ t2[[2]], equiv[[2]]][[1]];
        (*nothing to do:*)
        If[ipos1 === ipos2,
            Return[{1, t2}]
        ];
        sign =
            If[ipos2 > ipos1,
                (*commute pos2 backwards*)
                Table[FMinus[{t2[[1, ipos2]], t2[[1, ipos2 - idx]]}, {t2[[2, ipos2]], t2[[2, ipos2 - idx]]}], {idx, 1, ipos2 - ipos1}]
                ,
                (*commute pos2 forwards*)
                Table[FMinus[{t2[[1, ipos2]], t2[[1, ipos2 + idx]]}, {t2[[2, ipos2]], t2[[2, ipos2 + idx]]}], {idx, 1, ipos1 - ipos2}]
            ];
        (*Resolve the resulting FMinus, if possible*)
        sign = Times @@ ReduceIndices[setup, FTerm @@ sign];
        (*Replace the indices & fields in t2*)
        newt2 = Head[t2][Insert[Delete[t2[[1]], ipos2], t2[[1, ipos2]], ipos1], Insert[Delete[t2[[2]], ipos2], t2[[2, ipos2]], ipos1]];
        FunKitDebug[4, "Given ", t1, ", rearranged ", t2, " to ", newt2, " with sign ", sign];
        Return[{sign, newt2}];
    ];

TermsEqualAndSum::undeterminedFields = "Error: Cannot equate terms if they are not fully truncated, i.e. contain instances of AnyField.";

(* This is the main function for checking if two diagrams are equal to each other. Returns either False, or the sum of the two terms *)

TermsEqualAndSum[setup_, it1_FTerm, it2_FTerm] :=
    Module[
        {t1 = ReduceIndices[setup, it1], t2 = ReduceIndices[setup, it2], nt1, nt2, curIdx2, curIdxRepl, startPoints, doFields, allObjt1, allObjt2, cidxt1, cidxt2, oidxt1, oidxt2, startt1, startt1fields, cidxstartt1, startt2, nstartt2, startt2fields, cidxstartt2, branchAllObjt2, idx, jdx, equal = False, startsign, a, factor, removeOther, fac1, fac2, terms1, terms2}
        ,
        (*If[MemberQ[t1,AnyField,Infinity],Message[TermsEqualAndSum::undeterminedFields];Abort[]];*)
        (*Briefly check the trivial case*)
        FunKitDebug[4, "    TermsEqualAndSum: Comparing \n  ", t1, "\n   &\n  ", t2];
        If[it1 === it2,
            Return @ FTerm[2, t1]
        ];
        If[t1[[2 ;; ]] === t2[[2 ;; ]],
            Return @ FTerm[t1[[1]] + t2[[1]], t1[[2 ;; ]]]
        ];
        If[t1[[2 ;; ]] === t2[[1 ;; ]],
            Return @ FTerm[t1[[1]] + 1, t1[[2 ;; ]]]
        ];
        If[t1[[1 ;; ]] === t2[[2 ;; ]],
            Return @ FTerm[1 + t2[[1]], t2[[2 ;; ]]]
        ];
        (*Get all the possible starting points for the search*)
        startPoints = StartPoints[setup, t1, t2];
        If[Not[startPoints[[1]]],
            FunKitDebug[3, "    No matching StartPoints could be identified"];
            Return[False]
        ];
        FunKitDebug[4, "Collected StartPoints"];
        doFields = replFields[setup];
        (*collect objects for both terms*)
        allObjt1 = Select[ExtractObjectsWithIndex[setup, t1] /. doFields, FreeQ[FMinus[__]]];
        allObjt2 = Select[ExtractObjectsWithIndex[setup, t2] /. doFields, FreeQ[FMinus[__]]];
        cidxt1 = GetClosedSuperIndices[setup, t1];
        cidxt2 = GetClosedSuperIndices[setup, t2];
        oidxt1 = GetOpenSuperIndices[setup, t1];
        oidxt2 = GetOpenSuperIndices[setup, t2];
        (*We pick the first candidate for t1 and iterate over all candidates for t2.*)
        startt1 = startPoints[[2, 1]];
        (*starting indices can only be closed indices! We pick these out with the following 4 commands*)
        startt1fields = startt1[[1]];
        cidxstartt1 = Map[MemberQ[cidxt1, makePosIdx @ #]&, startt1[[2]]];
        startt1fields = Pick[startt1fields, cidxstartt1];
        cidxstartt1 = makePosIdx /@ Pick[startt1[[2]], cidxstartt1];
        (*Sanity check*)
        If[Length[cidxstartt1] === 0,
            Return[False]
        ];
        FunKitDebug[3, "Comparing the terms \n  ", t1, "\n  ", t2];
        (*If the terms are equal for any starting candidates for t2, we have succeeded*)
        For[idx = 1, idx <= Length[startPoints[[3]]], idx++,
            startt2 = startPoints[[3, idx]];
            (*We need to identify all possible insertion points in t2 that fit the insertion in t1*)
            (*starting indices can only be 1. closed indices 2. have same field content as the starting point in t1. We pick these out with the following 2 commands*)
            cidxstartt2 = Map[(MemberQ[cidxt2, #[[1]]] && #[[2]] === startt1fields[[1]])&, Transpose[{makePosIdx /@ startt2[[2]], startt2[[1]]}]];
            cidxstartt2 = Pick[makePosIdx /@ startt2[[2]], cidxstartt2];
            (*Loop over all possible starting indices*)
            For[jdx = 1, jdx <= Length[cidxstartt2], jdx++,
                curIdx2 = cidxstartt2[[jdx]];
                curIdxRepl = curIdx2 -> cidxstartt1[[1]];
                (*re-order the starting point so that it fits the first.*)
                {startsign, nstartt2} = RearrangeFields[setup, startt1, startt2, {cidxstartt1[[1]], curIdx2}];
                branchAllObjt2 = allObjt2 /. startt2 -> nstartt2;
                (*iterate the diagram*)
                FunKitDebug[3, "Starting sign: ", startsign /. curIdxRepl];
                FunKitDebug[3, "Starting point replacement: ", curIdxRepl];
                FunKitDebug[3, "StartPoints: \n  ", startt1, "\n  ", nstartt2 /. curIdxRepl];
                FunKitDebug[3, "StartIndices: \n  ", cidxstartt1[[1]], "\n  ", curIdx2 /. curIdxRepl];
                {equal, branchAllObjt2, nt2} = TermsEqualAndSum[setup, t1, t2 /. curIdxRepl, allObjt1, cidxt1, oidxt1, {startt1}, cidxstartt1[[1]], branchAllObjt2 /. curIdxRepl, cidxt2 /. curIdxRepl, oidxt2 /. curIdxRepl, {nstartt2} /. curIdxRepl, curIdx2 /. curIdxRepl, startsign /. curIdxRepl];
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
            FunKitDebug[2, "With prefactor: ", factor];
            Return @ FTerm[factor, terms1];
        ];
        FunKitDebug[2, "STOPPING HERE: Need to resolve Grassmann factors!"];
        Return[False];
        Print[Style["FATAL: Could not resolve Grassmann factors", Red]];
        Abort[];
    ];

(**********************************************************************************
    Identification of sums of diagrams (i.e. FEx)
**********************************************************************************)

FTermContent[setup_, term_FTerm] :=
    Module[{},
        Hash[Sort @ Map[Head[#][#[[1]]]&, FunKit`Private`ExtractObjectsWithIndex[setup, term] /. FunKit`Private`replFields[setup]], "SHA"]
    ];

(* Given an FEx, subdivide its FTerms into groups with identical content *)

SeparateTermGroups[setup_, expr_FEx] :=
    Module[
        {ret = List @@ expr, identifierRep, removeFirsts, groupedDiagrams}
        ,
        (*We group all diagrams into groups that could be potentially identical. We simply make sure that in each group all diagrams have the same objects.*)
        identifierRep = Map[FTermContent[setup, #]&, ret];
        identifierRep = Thread[{identifierRep, ret}];
        removeFirsts[ex_] := Map[#[[2]]&, ex];
        groupedDiagrams = (FEx @@ #)& /@ Map[removeFirsts, GatherBy[identifierRep, #[[1]]&]];
        FunKitDebug[2, "Separated into ", Length[groupedDiagrams], " groups."];
        Return[groupedDiagrams]
    ];

(* Withing a group of possibly matching FTerms, check for any possible equalities *)

SubFSimplify[setup_, expr_FEx] :=
    Module[{ret = List @@ expr, idx, jdx, red},
        For[idx = 1, idx <= Length[ret], idx++,
            For[jdx = idx + 1, jdx <= Length[ret], jdx++,
                red = TermsEqualAndSum[setup, ret[[idx]], ret[[jdx]]];
                FunKitDebug[3, "Comparing ", idx, " and ", jdx, ", result: ", red];
                If[red =!= False,
                    ret[[idx]] = red;
                    ret = Delete[ret, jdx];
                    jdx--;
                ];
            ];
        ];
        Return[FEx @@ ret];
    ];

(* Withing a group of possibly matching FTerms, check for any possible equalities, but this time with a given list of symmetries *)

SubFSimplify[setup_, expr_FEx, symmetryList_] :=
    Module[{ret = List @@ expr, idx, jdx, kdx, red},
        For[idx = 1, idx <= Length[ret], idx++,
            For[jdx = idx + 1, jdx <= Length[ret], jdx++,
                For[kdx = 1, kdx <= Length[symmetryList], kdx++,
                    red = TermsEqualAndSum[setup, ret[[idx]], ret[[jdx]] /. symmetryList[[kdx, Key["Rule"]]]];
                    If[red =!= False,
                        ret[[idx]] = FTerm[symmetryList[[kdx, Key["Factor"]]]] ** red;
                        ret = Delete[ret, jdx];
                        jdx--;
                        kdx = Length[symmetryList] + 1;
                    ];
                ];
            ];
        ];
        Return[FEx @@ ret];
    ];

(**********************************************************************************
    FSimplify, as exported by FunKit.
**********************************************************************************)

FSimplifyNoSym[setup_, expr_FEx] :=
    Module[{
        subGroups
        ,
        res
        ,
        map =
            If[$FunKitDebugLevel >= 2,
                Map
                ,
                ParallelMap
            ]
    },
        FunKitDebug[1, "Simplifying diagrammatic expression of length ", Length[expr]];
        subGroups = SeparateTermGroups[setup, expr];
        res = FEx @@ map[SubFSimplify[setup, #]&, subGroups];
        FunKitDebug[1, "FTerms before: ", Length[expr], ", after: ", Length[res]];
        Return[res];
    ];

Options[FSimplify] = {"Symmetries" -> {}};

FSimplify[setup_, inexpr_FEx, OptionsPattern[]] :=
    Module[{
        subGroups
        ,
        res
        ,
        map =
            If[$FunKitDebugLevel >= 2,
                Map
                ,
                ParallelMap
            ]
        ,
        expr
        ,
        annotations
    },
        {expr, annotations} = SeparateFExAnnotations[inexpr];
        expr = FOrderFields[setup, expr];
        symmetries =
            If[KeyExistsQ[annotations, "Symmetries"],
                annotations["Symmetries"]
                ,
                {}
            ];
        symmetries = MergeSymmetries[symmetries, OptionValue["Symmetries"]];
        FunKitDebug[3, "FSimplify: Using symmetry list ", symmetries];
        If[symmetries === {},
            Return[MergeFExAnnotations[FSimplifyNoSym[setup, expr], annotations]]
        ];
        FunKitDebug[1, "Simplifying diagrammatic expression of length ", Length[expr], "with symmetry list"];
        subGroups = SeparateTermGroups[setup, expr];
        res = FEx @@ map[SubFSimplify[setup, #, symmetries]&, subGroups];
        FunKitDebug[1, "FTerms before: ", Length[expr], ", after: ", Length[res]];
        Return[MergeFExAnnotations[res, annotations]];
    ];
