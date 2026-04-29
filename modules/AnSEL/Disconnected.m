(**********************************************************************************
    Disconnected.m -- Disconnected diagram detection and component partitioning

    Public API:
      FDisconnectedQ             -- Checks if an FTerm/FEx is a disconnected diagram

    Internal:
      objectIndices              -- Extracts positive superindices from an object
                                    (used by FDisconnectedQ, partitionFTermByConnectivity)
      partitionFTermByConnectivity
                                 -- Splits an FTerm into a list of connected
                                    component FTerms via BFS over closed-index
                                    connectivity.  Used by FRoute (Routing.m) for
                                    its disconnected-FTerm fast path and by
                                    matchDisconnectedTerms (Simplify.m) for the
                                    per-component matching of disconnected pairs.
**********************************************************************************)

FDisconnectedQ::invalidArguments = "FDisconnectedQ expects an FTerm or FEx expression.";

(* Helper: get all positive superindices carried by a single object.
   Works for both indexed objects (Propagator, GammaN, ...) and standalone
   field applications (Phi[i1], Psi[i2], ...). *)
objectIndices[obj_] :=
    If[MemberQ[$indexedObjects, Head[obj]],
        makePosIdx /@ getIndices[obj]
        ,
        {makePosIdx[obj[[1]]]}
    ];

FDisconnectedQ[setup_Association, expr_FTerm] :=
    Module[{objects, allFields, depth1Fields, idxO, idxF, closedIdx, visited, queue, cur, curIndices},
        (* Extract indexed objects and standalone field applications — mirrors ExtractObjectsAndIndices. *)
        allFields = Join[GetAllFields[setup], {AnyField}];
        depth1Fields = Cases[expr, Alternatives @@ Map[Blank[#]&, allFields], {1}];
        idxO = Cases[expr, Alternatives @@ Map[Blank[#]&, $indexedObjects], {1, 2}];
        idxF = Select[Cases[expr, Alternatives @@ Map[Blank[#]&, allFields], {1}], MemberQ[depth1Fields, #]&];
        objects = Join[idxO, idxF];
        If[Length[objects] <= 1, Return[False]];
        (* Closed indices are the edges of the connectivity graph.  Multiple
           objects with no closed indices means trivially disconnected. *)
        closedIdx = GetClosedSuperIndices[setup, expr];
        If[Length[closedIdx] == 0, Return[True]];
        (* BFS from the first object; if any object is unreachable, the FTerm
           is disconnected. *)
        visited = <|1 -> True|>;
        queue = {1};
        While[Length[queue] > 0,
            cur = First[queue];
            queue = Rest[queue];
            curIndices = Intersection[objectIndices[objects[[cur]]], closedIdx];
            Do[
                Do[
                    If[!KeyExistsQ[visited, pos] && MemberQ[objectIndices[objects[[pos]]], idx],
                        AssociateTo[visited, pos -> True];
                        AppendTo[queue, pos];
                    ]
                    ,
                    {pos, 1, Length[objects]}
                ]
                ,
                {idx, curIndices}
            ];
        ];
        Length[visited] < Length[objects]
    ];

FDisconnectedQ[setup_Association, expr_FEx] :=
    Or @@ (FDisconnectedQ[setup, #]& /@ (List @@ Select[expr, Head[#] === FTerm&]));

FDisconnectedQ[setup_Association, ___] :=
    (
        Message[FDisconnectedQ::invalidArguments];
        Abort[]
    );

(* Partition an FTerm into connected components by BFS over shared closed
   superindices.  Items with no indices (numeric coefficients, scalar factors)
   are absorbed into the first component so that multiplying the components
   reproduces the original coefficient.  Mirrors the BFS structure of
   FDisconnectedQ above. *)

partitionFTermByConnectivity[setup_, ft_FTerm] :=
    Module[{items, indexedItems, scalarItems, allFields, depth1Fields, idxO, idxF, indexedPos, scalarPos, closedIdx, idxSets, visited, queue, cur, components, compMembers},
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
