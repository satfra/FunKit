(* ::Section:: *)

(*Diagram drawing*)

(* ::Input::Initialization:: *)

MakeEdgeRule[setup_, obj_] :=
    Module[{},
        If[IsAntiFermion[setup, obj[[1, 1]]] && IsFermion[setup, obj[[1, 2]]],
            Return[makePosIdx @ obj[[2, 1]] -> makePosIdx @ obj[[2, 2]]]
        ];
        If[IsFermion[setup, obj[[1, 1]]] && IsAntiFermion[setup, obj[[1, 2]]],
            Return[makePosIdx @ obj[[2, 2]] -> makePosIdx @ obj[[2, 1]]]
        ];
        Return[makePosIdx @ obj[[2, 1]] <-> makePosIdx @ obj[[2, 2]]];
    ];

(* ::Input::Initialization:: *)

crosscircle[r_] :=
    Graphics[{Thick, Line[{{r / Sqrt[2], r / Sqrt[2]}, {-r / Sqrt[2], -r / Sqrt[2]}}], Line[{{r / Sqrt[2], -r / Sqrt[2]}, {-r / Sqrt[2], r / Sqrt[2]}}], Circle[{0, 0}, r]}];

cross[r_] :=
    Graphics[{Line[{{r / Sqrt[2], r / Sqrt[2]}, {-r / Sqrt[2], -r / Sqrt[2]}}], Line[{{r / Sqrt[2], -r / Sqrt[2]}, {-r / Sqrt[2], r / Sqrt[2]}}]}];

$standardVertexStyles = {GammaN -> Graphics @ Style[Disk[{0, 0}, 2], Gray], S -> Graphics @ Style[Disk[{0, 0}, 1.5], Black], Rdot -> crosscircle[1], Field -> cross[1], R -> Graphics @ Style[Disk[{0, 0}, 2], Blue], Phidot -> Graphics @ Style[Polygon[{{2, 0}, {0, 2 * Sqrt[3]}, {-2, 0}}], Black]};

$standardVertexSize = {GammaN -> 0.15, S -> 0.05, Rdot -> 0.25, Field -> 0.1, R -> 0.2};

(* ::Input::Initialization:: *)

arcFunc[g_, r_:1.5][list_, DirectedEdge[x_, x_]] :=
    With[{v = DynamicLocation["VertexID$" <> ToString[VertexIndex[g, x]], Automatic, Center]},
        Arrow[BezierCurve[Join[{v}, ScalingTransform[r {1, 1}, list[[1]]][list[[{5, 8, 10, 16, 18, 21}]]], {v}], SplineDegree -> 7]]
    ]

arcFuncUn[g_, r_:1.5][list_, UndirectedEdge[x_, x_]] :=
    With[{v = DynamicLocation["VertexID$" <> ToString[VertexIndex[g, x]], Automatic, Center]},
        Arrow[BezierCurve[Join[{v}, ScalingTransform[r {1, 1}, list[[1]]][list[[{5, 8, 10, 16, 18, 21}]]], {v}], SplineDegree -> 7]]
    ]

(* ::Input::Initialization:: *)

FPlot::FDOp = "Cannot plot diagrams with unresolved derivative operators!";

braceTexTerm[expr_] :=
    Module[
        {ret}
        ,
        (*TODO*)
        ret = ToString[expr];
        If[StringTake[ret, 1] != "(",
            ret = "\\left(" <> ret <> "\\right)"
        ];
        Return[ret];
    ];

GetDiagram[setup_, expr_FTerm] :=
    Module[{PossibleVertices, PossibleEdges, Styles, diag, allObj, fieldObj, vertices, edges, vertexReplacements, graph, phantomVertices, edgeFields, fieldVertices, fieldEdges, fieldEdgeFields, oidx, externalVertices, vertexNames, doubledVertices, externalEdges, externalFields, idx, prefactor, doubledEdges, doFields, eWeights, addVertexSizes = {}},
        If[MemberQ[expr, FDOp[__], Infinity],
            Message[FPlot::FDOp];
            Abort[]
        ];
        diag = FUnroute[setup, expr];
        doFields = replFields[setup];
        PossibleVertices =
            Join[
                {GammaN, S, Rdot, Field, R, Phidot}
                ,
                If[KeyExistsQ[setup, "DiagramStyling"] && KeyExistsQ[setup["DiagramStyling"], "Vertices"],
                    setup["DiagramStyling"]["Vertices"]
                    ,
                    {}
                ]
            ];
        PossibleEdges =
            Join[
                {Propagator}
                ,
                If[KeyExistsQ[setup, "DiagramStyling"] && KeyExistsQ[setup["DiagramStyling"], "Edges"],
                    setup["DiagramStyling"]["Edges"]
                    ,
                    {}
                ]
            ];
        Styles =
            If[KeyExistsQ[setup, "DiagramStyling"] && KeyExistsQ[setup["DiagramStyling"], "Styles"],
                setup["DiagramStyling"]["Styles"]
                ,
                Thread[(# -> ColorData[97, "ColorList"][[1 ;; Length[#]]])& @ DeleteDuplicates[GetAllFields[setup] /. Map[#[[1]] -> #[[2]]&, GetFieldPairs[setup]]]]
            ];
        Styles = Join[Styles, Map[GetPartnerField[setup, #[[1]]] -> #[[2]]&, Select[Styles, HasPartnerField[setup, #[[1]]]&]]];
        If[FreeQ[Keys[Styles], AnyField],
            Styles = Join[Styles, {AnyField -> {Blue, Dotted}}]
        ];
        allObj = ExtractObjectsWithIndex[setup, diag] //. doFields;
        fieldObj =
            Flatten[
                Select[allObj, Head[#] === Field&] /.
                    Field[{f_}, {i_}] :>
                        Module[{oi},
                            {Propagator[{f, GetPartnerField[setup, f]}, {oi, i}], Field[{f}, {oi}]}
                        ]
            ];
        allObj = Select[allObj, Head[#] =!= Field&];
        (*prepare vertices*)
        vertices = Select[allObj, MemberQ[PossibleVertices, Head[#]] && (FreeQ[PossibleEdges, Head[#]] || Length[#[[2]]] =!= 2)&];
        vertexReplacements =
            Flatten @
                Module[{v},
                    Map[
                        (
                            v = Unique["v"];
                            Map[(makePosIdx[#] -> v)&, #[[2]]]
                        )&
                        ,
                        vertices
                    ]
                ];
        vertices = Map[Head[#] @@ ((makePosIdx /@ #[[2]] /. vertexReplacements) // DeleteDuplicates)&, vertices];
        (*Edge case: we have a vertex twice!*)
        (*first, extract all vertex names*)
        doubledVertices = Select[vertices, Length[#] > 1&];
        (*then, filter the duplicates*)
        doubledEdges = {};
        Do[
            If[Length[doubledVertices[[idx]]] === 2,
                AppendTo[doubledEdges, doubledVertices[[idx, 1]] \[UndirectedEdge] doubledVertices[[idx, 2]]];
            ];
            ,
            {idx, 1, Length[doubledVertices]}
        ];
        (*Make 'em bold*)
        doubledEdges = Map[Style[#, Thick, Black]&, doubledEdges];
        (*Props and vertices for attached fields*)
        fieldVertices = Select[fieldObj, (Head[#] === Field)&];
        fieldVertices = Map[Head[#] @@ ((makePosIdx /@ #[[2]] /. vertexReplacements) // DeleteDuplicates)&, fieldVertices];
        fieldEdges = Select[fieldObj, (Head[#] =!= Field)&];
        fieldEdgeFields = Table[SelectFirst[fieldEdges[[idx, 1]], MemberQ[Styles, #, Infinity]&], {idx, 1, Length[fieldEdges]}];
        fieldEdges = Map[MakeEdgeRule[setup, #]&, fieldEdges /. vertexReplacements];
        fieldEdges = Table[Style[fieldEdges[[idx]], ##]& @@ Flatten @ {fieldEdgeFields[[idx]] /. Styles}, {idx, 1, Length[fieldEdges]}];
        (*prepare edges*)
        edges = Select[allObj, MemberQ[PossibleEdges, Head[#]] && Length[#[[2]]] === 2&];
        edgeFields = Table[SelectFirst[edges[[idx, 1]], MemberQ[Styles, #, Infinity]&], {idx, 1, Length[edges]}];
        edges = Map[MakeEdgeRule[setup, #]&, edges /. vertexReplacements];
        edges = Table[Style[edges[[idx]], ##]& @@ Flatten @ {edgeFields[[idx]] /. Styles}, {idx, 1, Length[edges]}];
        (*Add additional vertices for external indices*)
        externalVertices = GetOpenSuperIndices[setup, diag];
        externalFields = Table[SelectFirst[allObj, MemberQ[makePosIdx /@ #[[2]], externalVertices[[idx]]]&], {idx, 1, Length[externalVertices]}];
        externalFields = Table[externalFields[[idx, 1, FirstPosition[makePosIdx /@ externalFields[[idx, 2]], externalVertices[[idx]]][[1]]]], {idx, 1, Length[externalVertices]}];
        externalVertices = Unique /@ externalVertices;
        externalEdges = Table[MakeEdgeRule[setup, Propagator[{GetPartnerField[setup, externalFields[[idx]]], externalFields[[idx]]}, {externalVertices[[idx]], GetOpenSuperIndices[setup, diag][[idx]] /. vertexReplacements}]], {idx, 1, Length[externalVertices]}];
        externalEdges = Table[Style[externalEdges[[idx]], ##]& @@ Flatten @ {externalFields[[idx]] /. Styles}, {idx, 1, Length[externalEdges]}];
        (*get the prefactor*)
        prefactor =
            FTex[FTerm[Times @@ (diag /. doFields /. Map[Blank[#] -> 1&, Join[{Field}, $indexedObjects]])]] //
            braceTexTerm //
            MaTeX`MaTeX;
        oidx = GetOpenSuperIndices[setup, diag];
        Do[
            If[MemberQ[externalEdges, oidx[[idx]], Infinity],
                AppendTo[addVertexSizes, oidx[[idx]] -> 0.00001]
            ];
            ,
            {idx, 1, Length[GetOpenSuperIndices[setup, diag]]}
        ];
        vertexNames = DeleteDuplicates @ Flatten[List @@ #& /@ vertices];
        eWeights = Join[Map[1&, edges], Map[1&, externalEdges], Map[1&, fieldEdges], Map[0.5&, doubledEdges]];
        graph = Graph[Join[vertexNames, externalVertices, fieldVertices[[All, 1]]], Join[edges, externalEdges, fieldEdges, doubledEdges], EdgeWeight -> eWeights, VertexShape -> Join[Thread[vertices[[All, 1]] -> (vertices[[All, 0]] /. $standardVertexStyles)], Thread[externalVertices -> Map[Graphics @ Style[Disk[{0, 0}, 0.0], Gray]&, externalVertices]], Thread[fieldVertices[[All, 1]] -> (fieldVertices[[All, 0]] /. $standardVertexStyles)]], VertexSize -> Join[Thread[vertices[[All, 1]] -> (vertices[[All, 0]] /. $standardVertexSize)], addVertexSizes], GraphLayout -> {"SpringElectricalEmbedding", "EdgeWeighted" -> False}, PerformanceGoal -> "Quality", ImageSize -> Small, EdgeStyle -> Arrowheads[{{.07, .6}}]];
        Row[{prefactor, Graph[graph, EdgeShapeFunction -> {x_ \[DirectedEdge] x_ :> arcFunc[graph, 20.0], x_ \[UndirectedEdge] x_ :> arcFuncUn[graph, 20.0]}]}]
    ];

FPlot[setup_, expr_FTerm] :=
    Module[{},
        Print[GetDiagram[setup, expr]];
        Return @ expr
    ];

FPlot[setup_, expr_FEx] :=
    Module[{},
        Print[Plus @@ (GetDiagram[setup, #]& /@ (DropFExAnnotations @ expr))];
        Return @ expr
    ];

FPlot[setup_, expr_Association] /; isLoopAssociation[expr] :=
    Module[{},
        FPlot[setup, expr["Expression"]];
        Return @ expr
    ];

FPlot[setup_, expr_Association] /; isRoutedAssociation @ expr :=
    Module[{},
        FPlot[setup, (List @@ routedAssociation)[[All, Key["Expression"]]]];
        Return @ expr
    ];

FPlot::type = "Unknown type `1`";

FPlot[setup_, a_] :=
    (
        Message[FPlot::type, Head[a]];
        Abort[]
    )

(* ::Section:: *) 