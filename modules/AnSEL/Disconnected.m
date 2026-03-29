(**********************************************************************************
    FDisconnectedQ — Check whether an FTerm represents a disconnected diagram.

    A diagram is disconnected if its indexed objects partition into two or more
    groups with no shared closed superindex between groups. Uses BFS on the
    index-connectivity graph starting from the first object.
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
    Module[{objects, indices, iObjs, allFields, depth1Fields, idxO, idxF, closedIdx, visited, queue, cur, curIndices, idx, pos},
        (* Extract indexed objects and standalone field applications — mirrors ExtractObjectsAndIndices logic *)
        iObjs = $indexedObjects;
        allFields = Join[GetAllFields[setup], {AnyField}];
        depth1Fields = Cases[expr, Alternatives @@ Map[Blank[#]&, allFields], {1}];
        idxO = Cases[expr, Alternatives @@ Map[Blank[#]&, iObjs], {1, 2}];
        idxF = Select[Cases[expr, Alternatives @@ Map[Blank[#]&, allFields], {1}], MemberQ[depth1Fields, #]&];
        objects = Join[idxO, idxF];
        (* Trivially connected *)
        If[Length[objects] <= 1, Return[False]];
        (* Get closed indices — these are the edges *)
        closedIdx = GetClosedSuperIndices[setup, expr];
        If[Length[closedIdx] == 0,
            (* No closed indices, but multiple objects => disconnected *)
            Return[True]
        ];
        (* BFS from the first object *)
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
        Return[Length[visited] < Length[objects]]
    ];

FDisconnectedQ[setup_Association, expr_FEx] :=
    Or @@ (FDisconnectedQ[setup, #]& /@ (List @@ Select[expr, Head[#] === FTerm&]));

FDisconnectedQ[setup_Association, ___] :=
    (
        Message[FDisconnectedQ::invalidArguments];
        Abort[]
    );
