(**********************************************************************************
    Simplify.m -- Diagram identification and simplification

    Public API (exposed via ::usage in modules/AnSEL.m):
      FSimplify                  -- Simplifies FEx by identifying equivalent diagrams
      FMakeSymmetryList          -- Builds symmetry list from fields and their types

    Cross-module helpers (FunKit`Private` context, called from FEDeriK):
      FBuildSymmetryList         -- Constructs field permutations from symmetry groups
      FMergeSymmetries           -- Merges two symmetry lists
      FSimplifyNoSym             -- Simplifies FEx without symmetry information

    File-local helpers:
      StartPoints                -- Find viable starting points for diagram comparison
      IterateDiagram             -- Traverse a diagram along closed indices
      RearrangeFields            -- Reorder fields in an indexed object to align with
                                    a target index position; emits the Grassmann sign
      TermsEqualAndSum           -- Internal BFS used by TermsEqualCore (different
                                    arity from the public-named variants below)
      TermsEqualCore             -- Equality check on two preprocessed FTerms;
                                    returns False or {sign, nt2}
      TermsEqualAndSumPre        -- TermsEqualCore + coefficient summation
      TermsEqualPre              -- TermsEqualCore projected to just the sign
                                    (used by matchDisconnectedTerms)
      FTermContent               -- Notation-agnostic field-content fingerprint
                                    (used by SeparateTermGroups)
      SeparateTermGroups         -- Group FTerms by field content
      PrecomputeTermData         -- Cache cidx/oidx/objs/fp/disconnected per FTerm
      TransformTermData          -- Apply a symmetry rule to cached data
      SubFSimplify               -- Simplify a single fingerprint group
      grassmannPermutationSign   -- Sign of a permutation on per-component
                                    Grassmann parities (counts odd-odd inversions)
      candidateBijections        -- Enumerate fingerprint-respecting bijections
                                    between two component lists
      matchDisconnectedTerms     -- Per-component matcher that dispatches to
                                    TermsEqualPre, used by SubFSimplify when
                                    either FTerm in a pair is disconnected
**********************************************************************************)

(* Profiling accumulators — set to non-zero initial values to enable *)

$ProfileSubFSimplify = 0.;

$ProfileTermsEqual = 0.;

$ProfileTermsEqualCount = 0;

$ProfileTermsEqualSuccess = 0;

$ProfileStartPoints = 0.;

$ProfileGraphTraversal = 0.;

$ProfileRearrangeFields = 0.;

$ProfileSymPreprocess = 0.;

$ProfileFSimplifyEnabled = False;

ResetFSimplifyProfile[] :=
    (
        $ProfileFSimplifyEnabled = True;
        $ProfileSubFSimplify = 0.;
        $ProfileTermsEqual = 0.;
        $ProfileTermsEqualCount = 0;
        $ProfileTermsEqualSuccess = 0;
        $ProfileStartPoints = 0.;
        $ProfileGraphTraversal = 0.;
        $ProfileRearrangeFields = 0.;
        $ProfileSymPreprocess = 0.;
    );

PrintFSimplifyProfile[] :=
    (
        Print["  SubFSimplify total:    ", NumberForm[$ProfileSubFSimplify, {5, 4}], " s"];
        Print["  TermsEqualAndSum:      ", NumberForm[$ProfileTermsEqual, {5, 4}], " s  (", $ProfileTermsEqualCount, " calls, ", $ProfileTermsEqualSuccess, " matches)"];
        Print["    StartPoints:         ", NumberForm[$ProfileStartPoints, {5, 4}], " s"];
        Print["    Graph traversal:     ", NumberForm[$ProfileGraphTraversal, {5, 4}], " s"];
        Print["    RearrangeFields:     ", NumberForm[$ProfileRearrangeFields, {5, 4}], " s"];
        Print["  Sym preprocess:        ", NumberForm[$ProfileSymPreprocess, {5, 4}], " s"];
    );

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
        procDerList = unreplFields[setup, derivativeList];
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

(* Merge two symmetry lists by concatenation; an Outer-product merge that
   composes individual rules was tried but rejected because it blows up the
   number of equivalent rules without finding new equalities. *)

FMergeSymmetries[sym1_, sym2_] :=
    Join[sym1, sym2] // DeleteDuplicates;

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

(**********************************************************************************
    FSymmetry -- user-facing constructor for a single symmetry

    FMakeSymmetryList[setup, fields, indices] builds the FULL permutation group of the
    correlator. That group is a property of the correlation function, not of any one
    diagram, so reducing with it produces an expression that is only equal to the
    original after symmetrisation -- see SYMMETRY-REDUCTION-DESIGN.md. Which subgroup
    may actually be used depends on the contraction the user will apply, which FunKit
    cannot know. Hence symmetries are supplied by hand, and this is the ergonomic way
    to write them down:

        FSymmetry[Symmetric,     {i1,i2}, {i3,i4}]   i1<->i2 together with i3<->i4, +1
        FSymmetry[Antisymmetric, {i1,i2}]            i1<->i2, -1
        FSymmetry[Symmetric,     {i1,i2,i3}]         the 3-cycle i1->i2->i3->i1, +1
        FSymmetry[-1,            {i1,i2}, {i3,i4}]   explicit factor

    Each argument after the head is a cycle of superindices; all cycles are applied
    simultaneously and must be disjoint. Unlike FBuildSymmetryList, which is keyed on
    POSITIONS in the derivative list, these are the indices themselves.
**********************************************************************************)

FSymmetry::badFactor = "The first argument of FSymmetry must be Symmetric, Antisymmetric or a number, not `1`.";

FSymmetry::badCycle = "FSymmetry cycles must be lists of at least two distinct superindices; received `1`.";

FSymmetry::notDisjoint = "The cycles of `1` are not disjoint: the index `2` appears more than once.";

FSymmetry::noCycles = "FSymmetry requires at least one cycle.";

symmetryFactor[Symmetric] :=
    1;

symmetryFactor[Antisymmetric] :=
    -1;

symmetryFactor[f_ /; NumericQ[f]] :=
    f;

symmetryFactor[f_] :=
    (
        Message[FSymmetry::badFactor, f];
        Abort[]
    );

(*Lower one FSymmetry to the internal <|"Rule" -> ..., "Factor" -> ...|> form.*)

lowerFSymmetry[s : FSymmetry[head_, cycles___List]] :=
    Module[{cyc = {cycles}, all},
        If[Length[cyc] === 0,
            Message[FSymmetry::noCycles];
            Abort[]
        ];
        If[AnyTrue[cyc, Length[#] < 2 || Length[DeleteDuplicates[#]] =!= Length[#]&],
            Message[FSymmetry::badCycle, SelectFirst[cyc, Length[#] < 2 || Length[DeleteDuplicates[#]] =!= Length[#]&]];
            Abort[]
        ];
        all = Flatten[cyc];
        If[Length[DeleteDuplicates[all]] =!= Length[all],
            Message[FSymmetry::notDisjoint, s, First @ Cases[Tally[all], {x_, n_} /; n > 1 :> x]];
            Abort[]
        ];
        (*Sort to match the canonical form produced by symCombine in FMakeSymmetryList, so that
          the same symmetry written two ways compares equal under DeleteDuplicates.*)
        <|"Rule" -> Sort @ Flatten[Map[Thread[# -> RotateLeft[#]]&, cyc]], "Factor" -> symmetryFactor[head]|>
    ];

lowerFSymmetry[x_] :=
    (
        Message[FunKit::invalidArguments, FSymmetry];
        Abort[]
    );

(*Assemble FSymmetry objects into a symmetry list, adding the identity if missing.
  Note that the list need NOT be closed under composition: each merge under a given
  sigma is exact provided the user's contraction is covariant under that sigma,
  independently of the other elements. Closure only matters if the symmetriser is to
  be a projector (FSymmetrise).*)

$identitySymmetry = <|"Rule" -> {}, "Factor" -> 1|>;

FMakeSymmetryList[syms__FSymmetry] :=
    Module[{lowered},
        lowered = DeleteDuplicates[lowerFSymmetry /@ {syms}];
        If[Not @ MemberQ[lowered, $identitySymmetry],
            lowered = Prepend[lowered, $identitySymmetry]
        ];
        lowered
    ];

(*The list form FMakeSymmetryList[{s1, s2, ...}] is declared in AnSEL/Global.m, ahead of the
  fields_List overload there, which would otherwise capture it.*)

(**********************************************************************************
    FSymmetrise / FCheckSymmetry

    FSymmetrise[expr, syms] applies (1/|G|) sum_sigma f_sigma sigma(.) to an FEx.
    FCheckSymmetry[expr, syms] tests whether expr already has that symmetry, i.e.
    whether FSymmetrise is the identity on it. Reducing with a symmetry the expression
    does not actually possess silently returns a different object, so this is the
    precondition to check before passing a hand-made list to FSimplify.
**********************************************************************************)

(*Accept either an FEx or a bare list of FTerms: SeparateFExAnnotations returns the terms in
  whichever of the two the caller handed in.*)

negateFEx[expr_] :=
    FEx @@ ((FTerm[-1] ** #)& /@ (List @@ expr));

FSymmetrise[setup_, expr_FEx, syms_List] :=
    Module[{terms, annotations},
        AssertFSetup[setup];
        If[syms === {},
            Return[expr]
        ];
        {terms, annotations} = SeparateFExAnnotations[expr];
        MergeFExAnnotations[
            FEx @@ Flatten @ Table[
                List @@ ((FEx @@ ((FTerm[s["Factor"] / Length[syms]] ** #)& /@ (List @@ terms))) /. s["Rule"])
                ,
                {s, syms}
            ]
            ,
            annotations
        ]
    ];

FSymmetrise[setup_, expr_FEx] :=
    Module[{annotations},
        annotations = SeparateFExAnnotations[expr][[2]];
        FSymmetrise[setup, expr, If[KeyExistsQ[annotations, "Symmetries"], annotations["Symmetries"], {}]]
    ];

FSymmetrise[___] :=
    (
        Message[FunKit::invalidArguments, FSymmetrise];
        Abort[]
    );

FCheckSymmetry[setup_, expr_FEx, syms_List] :=
    Module[{terms, symmetrised},
        AssertFSetup[setup];
        terms = SeparateFExAnnotations[expr][[1]];
        symmetrised = SeparateFExAnnotations[FSymmetrise[setup, FEx @@ terms, syms]][[1]];
        (*Go through FSimplify, not FSimplifyNoSym: the former first applies FOrderFields and
          FixIndices, without which the matcher does not see equal terms as equal. Both inputs
          are annotation-free, so no symmetries are picked up and the comparison stays exact.*)
        Length @ FSimplify[setup, FEx @@ Join[List @@ terms, List @@ negateFEx[symmetrised]]] === 0
    ];

FCheckSymmetry[___] :=
    (
        Message[FunKit::invalidArguments, FCheckSymmetry];
        Abort[]
    );

(*Get viable starting points for a comparison of two diagrams.
  Requires pre-computed object lists (from PrecomputeTermData). *)

StartPoints[setup_, t1_FTerm, t2_FTerm, cidx1_, cidx2_, obj1_, obj2_] :=
    Module[{count, desired, sList, match1, match2, fieldKey, $profTmp, $profResult},
        {$profTmp, $profResult} =
            AbsoluteTiming[
                Module[{},
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
                    desired = Keys[count][[First @ Ordering[Values[count], 1]]];
                    match1 = Select[obj1, (fieldKey[#] === desired)&];
                    match2 = Select[obj2, (fieldKey[#] === desired)&];
                    (*return all possible starting points *)
                    {True, match1, match2}
                ]
            ];
        $ProfileStartPoints += $profTmp;
        Return[$profResult]
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
    Module[{t1 = it1, t2 = it2, nt2, allObjt1 = MallObjt1, curIdx1, curPos1, nextInd1, nextPos1, memory1 = Mmemory1, assocFields1, allObjt2 = MallObjt2, curIdx2, curPos2, nextInd2, nextPos2, memory2 = Mmemory2, assocFields2, sign2 = Msign2, iter = 1, idx, jdx, viableBranches, branchSign, branchItRepl, branchObj, temp1, temp2, cidxt2 = Mcidxt2, curIdxRepl, ncidxt2, noidxt2, nmemory2, allIdxRepl = {}, nallIdxRepl, nallIdxReplNew},
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
                (*Check if the open indices agree*)
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
                {allObjt2, memory2, curPos2, nextPos2, t2, sign2, cidxt2} = {allObjt2, memory2, curPos2, nextPos2, t2, sign2, cidxt2} /. curIdxRepl;
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
    Module[{ipos1, ipos2, idx, sign, newt2, nf1, ni1, nf2, ni2, $profTmp, $profResult},
        {$profTmp, $profResult} =
            AbsoluteTiming[
                Module[
                    {}
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
                    If[FreeQ[nf2, AnyField],
                        (* Fast path: resolve commutation signs directly *)
                        sign =
                            If[ipos2 > ipos1,
                                Product[CommuteSign[setup, nf2[[ipos2]], nf2[[ipos2 - idx]]], {idx, 1, ipos2 - ipos1}]
                                ,
                                Product[CommuteSign[setup, nf2[[ipos2]], nf2[[ipos2 + idx]]], {idx, 1, ipos1 - ipos2}]
                            ];
                        ,
                        (* Slow path: fields contain AnyField, must use FMinus+ReduceIndices *)
                        sign =
                            If[ipos2 > ipos1,
                                Table[makeObj[FMinus, {nf2[[ipos2]], nf2[[ipos2 - idx]]}, {ni2[[ipos2]], ni2[[ipos2 - idx]]}], {idx, 1, ipos2 - ipos1}]
                                ,
                                Table[makeObj[FMinus, {nf2[[ipos2]], nf2[[ipos2 + idx]]}, {ni2[[ipos2]], ni2[[ipos2 + idx]]}], {idx, 1, ipos1 - ipos2}]
                            ];
                        sign = Times @@ ReduceIndices[setup, FTerm @@ sign];
                    ];
                    (*Replace the indices & fields in t2*)
                    newt2 = makeObj[Head[t2], Insert[Delete[nf2, ipos2], nf2[[ipos2]], ipos1], Insert[Delete[ni2, ipos2], ni2[[ipos2]], ipos1]];
                    FunKitDebug[4, "Given ", t1, ", rearranged ", t2, " to ", newt2, " with sign ", sign];
                    {sign, newt2}
                ]
            ];
        $ProfileRearrangeFields += $profTmp;
        Return[$profResult];
    ];

TermsEqualAndSum::undeterminedFields = "Error: Cannot equate terms if they are not fully truncated, i.e. contain instances of AnyField.";

(* TermsEqualPre: equality check between two preprocessed (FixIndices +
   FOrderFields applied) FTerms.  Returns either False or {sign, nt2}.

   `sign` is typically ±1 but can be a symbolic scalar in the AnyField slow
   path of RearrangeFields — callers must accept that.

   `nt2` is the index-renamed t2.  Its scalar prefactor carries any
   FMinus[...] introduced during alignment, so a caller that sums
   coefficients must read fac2 from nt2 (not from the original t2).

   `data1`/`data2` come from PrecomputeTermData. *)

TermsEqualPre[setup_, t1_FTerm, t2_FTerm, data1_Association, data2_Association] :=
    Module[{nt2 = t2, curIdx1, curIdx2, curIdxRepl, startPoints, allObjt1, allObjt2, cidxt1, cidxt2, oidxt1, oidxt2, startt1, startt1fields, cidxstartt1, startt2, nstartt2, cidxstartt2, branchAllObjt2, idx, jdx, equal = False, startsign, fac1, fac2, terms1, terms2, nallIdxReplNew, tmp, $profT0 = AbsoluteTime[]},
        $ProfileTermsEqualCount++;
        FunKitDebug[4, "    TermsEqual: Comparing \n  ", t1, "\n   &\n  ", t2];
        (* No AnyField guard here: the RearrangeFields slow path produces
           symbolic FMinus[...] factors when AnyField is present in fields
           lists, and the BFS accumulates them into `equal`.  Callers must
           be prepared to receive a symbolic sign. *)
        If[t1 === t2,
            FunKitDebug[3, "    Terms are identical, sign = 1."];
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            $ProfileTermsEqualSuccess++;
            Return[{1, t2}]
        ];
        {fac1, terms1} = SplitPrefactor[setup, t1];
        {fac2, terms2} = SplitPrefactor[setup, t2];
        If[terms1 === terms2,
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            $ProfileTermsEqualSuccess++;
            Return[{1, t2}]
        ];
        (* Use pre-computed data *)
        cidxt1 = data1["cidx"];
        cidxt2 = data2["cidx"];
        If[Length[cidxt1] =!= Length[cidxt2],
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            Return[False]
        ];
        If[Length[cidxt1] == 0,
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            Return[False]
        ];
        allObjt1 = data1["objs"];
        allObjt2 = data2["objs"];
        oidxt1 = data1["oidx"];
        oidxt2 = data2["oidx"];
        startPoints = StartPoints[setup, t1, t2, cidxt1, cidxt2, allObjt1, allObjt2];
        If[Not[startPoints[[1]]],
            FunKitDebug[3, "    No matching StartPoints could be identified"];
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            Return[False]
        ];
        FunKitDebug[4, "Collected StartPoints"];
        (*We pick the first candidate for t1 and iterate over all candidates for t2.*)
        startt1 = startPoints[[2, 1]];
        (*starting indices can only be closed indices! We pick these out with the following 4 commands*)
        startt1fields = getFields[startt1];
        cidxstartt1 = Map[MemberQ[cidxt1, makePosIdx @ #]&, getIndices[startt1]];
        startt1fields = Pick[startt1fields, cidxstartt1];
        cidxstartt1 = makePosIdx /@ Pick[getIndices[startt1], cidxstartt1];
        (*Sanity check*)
        If[Length[cidxstartt1] === 0,
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            Return[False]
        ];
        FunKitDebug[3, "Comparing the terms \n  ", t1, "\n  ", t2];
        If[Length[Intersection[getIndices[startt1], cidxt1]] == 1,
            curIdx1 = Null
            ,
            curIdx1 = cidxstartt1[[1]];
        ];
        For[idx = 1, idx <= Length[startPoints[[3]]], idx++,
            startt2 = startPoints[[3, idx]];
            cidxstartt2 = Map[(MemberQ[cidxt2, #[[1]]] && #[[2]] === startt1fields[[1]])&, Transpose[{makePosIdx /@ getIndices[startt2], getFields[startt2]}]];
            cidxstartt2 = Pick[makePosIdx /@ getIndices[startt2], cidxstartt2];
            FunKitDebug[4, "We have: ", Length[cidxstartt2], " possible starting indices in t2."];
            For[jdx = 1, jdx <= Length[cidxstartt2], jdx++,
                If[Length[Intersection[getIndices[startt2], cidxt2]] == 1,
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
                {startsign, nstartt2} = RearrangeFields[setup, startt1, startt2, {curIdx1, curIdx2}];
                branchAllObjt2 = allObjt2 /. startt2 -> nstartt2;
                {tmp, {equal, branchAllObjt2, nt2, nallIdxReplNew}} = AbsoluteTiming[TermsEqualAndSum[setup, t1, t2 /. curIdxRepl, allObjt1, cidxt1, oidxt1, {startt1}, curIdx1, branchAllObjt2 /. curIdxRepl, cidxt2 /. curIdxRepl, oidxt2 /. curIdxRepl, {nstartt2} /. curIdxRepl, curIdx2 /. curIdxRepl, startsign /. curIdxRepl]];
                $ProfileGraphTraversal += tmp;
                If[equal =!= False,
                    Break[]
                ];
            ];
            If[equal =!= False,
                Break[]
            ];
        ];
        If[equal === False,
            $ProfileTermsEqual += AbsoluteTime[] - $profT0;
            Return[False];
        ];
        $ProfileTermsEqualSuccess++;
        $ProfileTermsEqual += AbsoluteTime[] - $profT0;
        Return[{equal, nt2}]
    ];

(* mergeCoefficientIntoTerm: build a flat FTerm from a summed coefficient and an
   objects-only FTerm.  The coefficient is itself an FTerm here (ReduceIndices
   wraps its argument, e.g. ReduceIndices[setup, FTerm[2]] -> FTerm[2]), so the
   naive FTerm[factor, terms] is doubly nested and relies on the
   FTerm[___, FTerm[__], ___] flattening downvalues (Notation.m) to collapse.
   If that flattening does not fire, an FTerm leaks into the prefactor and a
   later merge produces a malformed coefficient like 1 + FTerm[2].  Splicing the
   parts explicitly makes the result flat by construction, independent of the
   downvalue firing. *)

mergeCoefficientIntoTerm[factor_, terms_FTerm] :=
    FTerm @@ Join[If[Head[factor] === FTerm, List @@ factor, {factor}], List @@ terms];

(* TermsEqualAndSumPre: equality check + coefficient summation.  Returns
   either False or FTerm[fac1 + sign * fac2, ...t1's objects...].  Connected-
   case only: bare-Grassmann FTerms are intentionally not merged via this
   path because the BFS does not track Grassmann sign on bare fields;
   matchDisconnectedTerms handles those via the per-component permutation. *)

TermsEqualAndSumPre[setup_, t1_FTerm, t2_FTerm, data1_Association, data2_Association] :=
    Module[{result, sign, nt2, fac1, fac2, terms1, terms2, factor},
        result = TermsEqualPre[setup, t1, t2, data1, data2];
        If[result === False, Return[False]];
        {sign, nt2} = result;
        If[GrassmannCount[setup, t1] =!= 0, Return[False]];
        FunKitDebug[2, "Found two equal terms"];
        {fac1, terms1} = SplitPrefactor[setup, t1];
        {fac2, terms2} = SplitPrefactor[setup, nt2];
        factor = ReduceIndices[setup, FTerm[fac1 + sign * fac2]];
        If[factor === 0 || factor === FTerm[0], Return[FTerm[0]]];
        mergeCoefficientIntoTerm[factor, terms1]
    ];

(* 3-arg variants normalize their inputs first; for callers that haven't
   pre-applied FixIndices/FOrderFields/PrecomputeTermData. *)

TermsEqual[setup_, it1_FTerm, it2_FTerm] :=
    Module[{t1, t2},
        If[!FreeQ[it1, AnyField] || !FreeQ[it2, AnyField], Return[False]];
        t1 = FixIndices[setup, FOrderFields[setup, ReduceIndices[setup, it1]]];
        t2 = FixIndices[setup, FOrderFields[setup, ReduceIndices[setup, it2]]];
        TermsEqualPre[setup, t1, t2, PrecomputeTermData[setup, t1], PrecomputeTermData[setup, t2]]
    ];

TermsEqualAndSum[setup_, it1_FTerm, it2_FTerm] :=
    Module[{t1, t2},
        If[!FreeQ[it1, AnyField] || !FreeQ[it2, AnyField], Return[False]];
        t1 = FixIndices[setup, FOrderFields[setup, ReduceIndices[setup, it1]]];
        t2 = FixIndices[setup, FOrderFields[setup, ReduceIndices[setup, it2]]];
        TermsEqualAndSumPre[setup, t1, t2, PrecomputeTermData[setup, t1], PrecomputeTermData[setup, t2]]
    ];

(**********************************************************************************
    Identification of sums of diagrams
**********************************************************************************)

FTermContent[setup_, term_FTerm] :=
    Module[{objs},
        (* replFields normalizes depth-1 field applications like Π[ci61] into
           Field[{Π},{ci61}], so getFields returns the field name {Π} rather
           than the index ci61.  Without this the fingerprint depends on
           closed-index names and structurally-equal FTerms get partitioned
           into different groups by SeparateTermGroups. *)
        objs =
            Map[
                If[FunKit`Private`indexedObjectQ[#],
                    #
                    ,
                    FunKit`Private`replFields[setup, #]
                ]&
                ,
                FunKit`Private`ExtractObjectsWithIndex[setup, term]
            ];
        If[$VersionNumber >= 13.0,
            Hash[Sort @ Map[Head[#] @@ FunKit`Private`getFields[#]&, objs], "SHA256"]
            ,
            Hash[ToString[Sort @ Map[Head[#] @@ FunKit`Private`getFields[#]&, objs], InputForm], "SHA256"]
        ]
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

(* Pre-compute per-term data for the pairwise comparison loop *)

PrecomputeTermData[setup_, term_FTerm] :=
    Module[{objs},
        objs =
            Select[
                Map[
                    If[indexedObjectQ[#],
                        #
                        ,
                        replFields[setup, #]
                    ]&
                    ,
                    ExtractObjectsWithIndex[setup, term]
                ]
                ,
                FreeQ[#, FMinus[__]]&
            ];
        Module[{cidx = GetClosedSuperIndices[setup, term], oidx = GetOpenSuperIndices[setup, term], fieldKey},
            fieldKey[obj_] := Head[obj] @@ Sort @ getFields[obj];
            <|"cidx" -> cidx, "oidx" -> oidx, "objs" -> objs, "fp" -> {Length[cidx], Sort @ Map[fieldKey, objs]}, "disconnected" -> FDisconnectedQ[setup, term]|>
        ]
    ];

(* Transform pre-computed data under a symmetry rule (index permutation only).
   Valid because symmetry rules only permute open indices, not field content. *)

TransformTermData[data_Association, rule_List] :=
    <|"cidx" -> (data["cidx"] /. rule), "oidx" -> (data["oidx"] /. rule), "objs" -> (data["objs"] /. rule), "disconnected" -> data["disconnected"]|>;

(* Disconnected-matching helpers.

   matchDisconnectedTerms: per-component matcher for two disconnected FTerms.
   Returns either False or the merged FTerm.  Called from SubFSimplify
   when at least one term in a pair is flagged disconnected — direct
   TermsEqualAndSumPre would walk only one connected sub-graph from a single
   start point, accepting equality after verifying just one component.

   candidateBijections: enumerate fingerprint-respecting bijections between
   two component lists (typically a single bijection when fingerprints are
   unique; k! permutations within each fingerprint-equal group of k).

   grassmannPermutationSign: sign of a permutation on per-component Grassmann
   parities.  Counts inversions where both swapped components are
   Grassmann-odd. *)

grassmannPermutationSign[parities_List, perm_List] :=
    Module[{n = Length[perm], sgn = 1, i, j},
        Do[
            Do[
                If[perm[[i]] > perm[[j]] && parities[[ perm[[i]] ]] === 1 && parities[[ perm[[j]] ]] === 1,
                    sgn = -sgn
                ]
                ,
                {j, i + 1, n}
            ]
            ,
            {i, 1, n - 1}
        ];
        sgn
    ];

(* Enumerate all bijections β : {1..n} -> {1..n} with fp1[[i]] === fp2[[β[[i]]]].
   When fingerprints are unique, exactly one bijection is produced. *)

candidateBijections[fp1_List, fp2_List] :=
    Module[{n = Length[fp1], slots, build},
        slots = Table[Flatten @ Position[fp2, fp1[[i]]], {i, n}];
        build[partial_, used_] :=
            Module[{i = Length[partial] + 1, choices, results = {}},
                If[i > n, Return[{partial}]];
                choices = Complement[slots[[i]], used];
                Do[AppendTo[results, build[Append[partial, j], Append[used, j]]], {j, choices}];
                Flatten[results, 1]
            ];
        build[{}, {}]
    ];

matchDisconnectedTerms[setup_, t1_FTerm, t2_FTerm, data1_Association, data2_Association] :=
    Module[{fac1, fac2, terms1, terms2, comps1, comps2, fp1, fp2, parities,
            dataPerComp1, dataPerComp2},
        (* Strip scalar prefactors so per-component comparisons see coefficient-1 sub-FTerms. *)
        {fac1, terms1} = SplitPrefactor[setup, t1];
        {fac2, terms2} = SplitPrefactor[setup, t2];
        comps1 = partitionFTermByConnectivity[setup, terms1];
        comps2 = partitionFTermByConnectivity[setup, terms2];
        If[Length[comps1] =!= Length[comps2], Return[False]];
        fp1 = Map[FTermContent[setup, #]&, comps1];
        fp2 = Map[FTermContent[setup, #]&, comps2];
        If[Sort[fp1] =!= Sort[fp2], Return[False]];
        dataPerComp1 = Map[PrecomputeTermData[setup, #]&, comps1];
        dataPerComp2 = Map[PrecomputeTermData[setup, #]&, comps2];
        parities = Map[Mod[GrassmannCount[setup, #], 2]&, comps1];
        (* For each candidate bijection, run the connected-case TermsEqualPre on
           every component pair.  The per-pair result is False or {sign, _} —
           we only need the sign here; the per-component nt2 is irrelevant
           because we stripped scalars from t1/t2 first, so per-component
           prefactors are all 1. *)
        Catch[
            Do[
                Module[{results, signs, sign, factor},
                    results = Table[
                        TermsEqualPre[
                            setup,
                            comps1[[i]], comps2[[ β[[i]] ]],
                            dataPerComp1[[i]], dataPerComp2[[ β[[i]] ]]
                        ]
                        ,
                        {i, Length[comps1]}
                    ];
                    If[FreeQ[results, False],
                        signs = First /@ results;
                        sign = (Times @@ signs) * grassmannPermutationSign[parities, β];
                        factor = ReduceIndices[setup, FTerm[fac1 + sign * fac2]];
                        If[factor === 0 || factor === FTerm[0], Throw[FTerm[0]]];
                        Throw[mergeCoefficientIntoTerm[factor, terms1]]
                    ]
                ]
                ,
                {β, candidateBijections[fp1, fp2]}
            ];
            False
        ]
    ];

(* Within a group of possibly matching FTerms, check for any possible equalities *)

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
        {ret = List @@ expr, idx, jdx, red, termData, $profT0 = AbsoluteTime[]}
        ,
        (* Preprocess: ReduceIndices, then re-normalize once (γ resolution may change indices) *)
        ret = ReduceIndicesBatch[setup, ret];
        ret = FixIndices[setup, OrderFields[setup, #]]& /@ ret;
        (* Pre-compute per-term data once *)
        termData = Map[PrecomputeTermData[setup, #]&, ret];
        For[idx = 1, idx <= Length[ret], idx++,
            For[jdx = idx + 1, jdx <= Length[ret], jdx++,
                If[termData[[idx]]["fp"] =!= termData[[jdx]]["fp"],
                    Continue[]
                ];
                red =
                    If[ TrueQ[termData[[idx]]["disconnected"]] || TrueQ[termData[[jdx]]["disconnected"]],
                        matchDisconnectedTerms[setup, ret[[idx]], ret[[jdx]], termData[[idx]], termData[[jdx]]]
                        ,
                        TermsEqualAndSumPre[setup, ret[[idx]], ret[[jdx]], termData[[idx]], termData[[jdx]]]
                    ];
                FunKitDebug[3, "Compared ", idx, " and ", jdx, ", result: ", red];
                If[red =!= False,
                    ret[[idx]] = red;
                    ret = Delete[ret, jdx];
                    termData[[idx]] = PrecomputeTermData[setup, ret[[idx]]];
                    termData = Delete[termData, jdx];
                    jdx--;
                ];
            ];
        ];
        $ProfileSubFSimplify += AbsoluteTime[] - $profT0;
        Return[ret];
    ];

(* Within a group of possibly matching FTerms, check for any possible equalities, but this time with a given list of symmetries *)

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
        {ret = List @@ expr, idx, jdx, kdx, red, matched, t2sym, data2sym, termData, nonTrivialSym, $profT0 = AbsoluteTime[], $profTmpSP}
        ,
        (* Filter out identity symmetry — handled separately with cached data *)
        nonTrivialSym = Select[symmetryList, #["Rule"] =!= {} || #["Factor"] =!= 1&];
        (* Normalize the group once *)
        ret = ReduceIndicesBatch[setup, ret];
        ret = FixIndices[setup, OrderFields[setup, #]]& /@ ret;
        termData = Map[PrecomputeTermData[setup, #]&, ret];
        For[idx = 1, idx <= Length[ret], idx++,
            For[jdx = idx + 1, jdx <= Length[ret], jdx++,
                (* Fingerprint is symmetry-invariant — skip pair entirely if mismatch *)If[termData[[idx]]["fp"] =!= termData[[jdx]]["fp"],
                    Continue[]
                ];
                matched = False;
                (* Identity symmetry: use cached data directly *)
                red =
                    If[ TrueQ[termData[[idx]]["disconnected"]] || TrueQ[termData[[jdx]]["disconnected"]],
                        matchDisconnectedTerms[setup, ret[[idx]], ret[[jdx]], termData[[idx]], termData[[jdx]]]
                        ,
                        TermsEqualAndSumPre[setup, ret[[idx]], ret[[jdx]], termData[[idx]], termData[[jdx]]]
                    ];
                If[red =!= False,
                    matched = True
                ];
                (* Non-trivial symmetries *)
                If[!matched,
                    For[kdx = 1, kdx <= Length[nonTrivialSym], kdx++,
                        $profTmpSP = AbsoluteTime[];
                        (* Transform pre-computed data directly under symmetry rule *)
                        data2sym = TransformTermData[termData[[jdx]], nonTrivialSym[[kdx, Key["Rule"]]]];
                        (* Build symmetry-transformed term without full preprocess *)
                        t2sym = FTerm[nonTrivialSym[[kdx, Key["Factor"]]]] ** ret[[jdx]] /. nonTrivialSym[[kdx, Key["Rule"]]];
                        $ProfileSymPreprocess += AbsoluteTime[] - $profTmpSP;
                        red =
                            If[ TrueQ[termData[[idx]]["disconnected"]] || TrueQ[data2sym["disconnected"]],
                                matchDisconnectedTerms[setup, ret[[idx]], t2sym, termData[[idx]], data2sym]
                                ,
                                TermsEqualAndSumPre[setup, ret[[idx]], t2sym, termData[[idx]], data2sym]
                            ];
                        If[red =!= False,
                            matched = True;
                            Break[];
                        ];
                    ];
                ];
                If[matched,
                    ret[[idx]] = red;
                    ret = Delete[ret, jdx];
                    termData[[idx]] = PrecomputeTermData[setup, ret[[idx]]];
                    termData = Delete[termData, jdx];
                    jdx--;
                ];
            ];
        ];
        $ProfileSubFSimplify += AbsoluteTime[] - $profT0;
        Return[ret];
    ];

(**********************************************************************************
    FSimplify, as exported by FunKit.
**********************************************************************************)

FSimplifyNoSym[setup_, expr_] :=
    Module[{subGroups, res, useParallel},
        FunKitDebug[1, "Simplifying diagrammatic expression of length ", Length[expr]];
        subGroups = SeparateTermGroups[setup, expr];
        useParallel = AllTrue[subGroups, Length[#] <= 64&] && ($FunKitDebugLevel <= 2) && !$ProfileFSimplifyEnabled;
        If[useParallel,
            res = FEx @@ Flatten[PMap[SubFSimplify[setup, #]&, subGroups]];
            ,
            res = FEx @@ Flatten[Map[SubFSimplify[setup, #]&, subGroups]];
        ];
        FunKitDebug[1, "FTerms before: ", Length[expr], ", after: ", Length[res]];
        Return[res];
    ];

Options[FSimplify] = {"Symmetries" -> {}};

FSimplify[setup_, inexpr_FEx, OptionsPattern[]] :=
    Module[{subGroups, res, expr, annotations, useParallel, symmetries},
        AssertFSetup[setup];
        {expr, annotations} = SeparateFExAnnotations[inexpr];
        expr = FOrderFields[setup, expr];
        expr = FixIndices[setup, expr];
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
        useParallel = AllTrue[subGroups, Length[#] <= 64&] && ($FunKitDebugLevel <= 2) && !$ProfileFSimplifyEnabled;
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
