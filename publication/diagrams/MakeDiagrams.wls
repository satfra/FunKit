(* ::Package:: *)

(* Regenerates the diagram figures of FunKit.tex as cropped vector PDFs.

     wolfram -script publication/diagrams/MakeDiagrams.wls              all figures
     wolfram -script publication/diagrams/MakeDiagrams.wls diagram_3 ... selected figures

   Output goes next to this script as <name>.pdf. Each entry of $figures repeats the code of
   the paper cell that shows the figure; keep the two in sync when editing either.

   The diagrams are FPlot's: same layout, vertex shapes, colours and prefactors. They are drawn
   here with plain graphics primitives at one common scale instead of through Graph, whose
   rendering varies with each diagram's extent and merges the styles of parallel lines.
   Needs pdflatex (for MaTeX) and pdfcrop on the PATH. *)

(* In command-line sessions FunKit.m redefines Style[x, ___] := x, which would strip all colours
   and vertex shapes. Loading under a front end skips that; no internet, so the update check
   cannot open a dialog. Print is muted because the front-end banner is unreadable here. *)
Block[{$AllowInternet = False, Print = Null&}, UsingFrontEnd[Get["FunKit`"]]];
If[DownValues[Style] =!= {}, Print["Style is overridden; diagrams would lose their styling."]; Exit[1]];

$outDir = DirectoryName[ExpandFileName[$InputFileName]];

(* ---------------------------------------------------------------------------------------------
   Layout parameters. Lengths without a unit are in units of the median line of a diagram.
   FunKit.tex shows every figure at scale 0.5 (\diagram), so a 16 pt label prints at 8 pt.
--------------------------------------------------------------------------------------------- *)

$propagatorPoints = 70;   (* pt per unit, roughly FPlot's own scale; FunKit's vertex shapes need it *)
$lineWidth = 1.6;         (* pt *)
$arrowSize = 0.22;
$parallelSpacing = 0.3;   (* bulge between neighbouring parallel lines *)
$loopSize = 0.8;          (* reach of a self-loop *)
$vertexScale = 0.7;       (* multiplies FunKit's VertexSize values *)
$labelFontSize = 16;      (* pt *)
$labelGap = 0.06;         (* between a leg's end and its label *)
$cropMargin = 3;          (* pt of white space kept around the figure *)

(* prefactors and signs; FPlot's MaTeX calls pick this up too *)
SetOptions[MaTeX`MaTeX, FontSize -> 18];

(* ---------------------------------------------------------------------------------------------
   Setups, as in the paper
--------------------------------------------------------------------------------------------- *)

scalar = <|
  "FieldSpace" -> <|"Commuting" -> {Phi[p]}|>,
  "Truncation" -> <|GammaN -> {{Phi, Phi, Phi}, {Phi, Phi, Phi, Phi}}, Propagator -> {{Phi, Phi}}, Rdot -> {{Phi, Phi}}|>
|>;

wEq = FTerm[1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]];

ymFields = <|
  "Commuting" -> {A[p, {v, c}]},
  "Grassmann" -> {{cb[p, {c}], c[p, {c}]}},
  "BoseStatistics" -> {c}
|>;
ymTruncation = <|
  GammaN -> {{A, A}, {A, A, A}, {A, A, A, A}, {A, cb, c}, {cb, c}},
  Propagator -> {{A, A}, {cb, c}},
  Rdot -> {{A, A}, {cb, c}},
  S -> {{A, A}, {A, A, A}, {A, A, A, A}, {cb, c}, {cb, c, A}},
  Field -> {{}}
|>;
ymUntruncated = <|"FieldSpace" -> ymFields|>;
ym = <|"FieldSpace" -> ymFields, "Truncation" -> ymTruncation|>;
ymStyled = Append[ym, "DiagramStyling" -> <|"EdgeStyles" -> {A -> {Orange}, c -> {Black, Dashed}}|>];

njl = <|
  "FieldSpace" -> <|"Commuting" -> {}, "Grassmann" -> {{psibar[p, {d}], psi[p, {d}]}}|>,
  "Truncation" -> <|GammaN -> {{psibar, psi, psibar, psi}, {psibar, psi}}, Propagator -> {{psibar, psi}}, Rdot -> {{psibar, psi}}|>
|>;

(* ---------------------------------------------------------------------------------------------
   Figures: name :> {setup, expression, diagrams per row}
--------------------------------------------------------------------------------------------- *)

$figures = <|
  (* Introduction: scalar two-point flow *)
  "diagram_0" :> {scalar, FTruncate[scalar, FTakeDerivatives[scalar, WetterichEquation, {Phi[i1], Phi[i2]}]], All},
  (* Deriving diagrams: untruncated gluon two-point flow *)
  "diagram_1" :> {ymUntruncated, FTakeDerivatives[ymUntruncated, wEq, {A[i1], A[i2]}], All},
  (* Truncating the output *)
  "diagram_3" :> {ym, FTruncate[ym, FTakeDerivatives[ym, wEq, {A[i1], A[i2]}]], All},
  (* Gluon DSE, two-loop part *)
  "diagram_4" :> {ym, FRoute[ym, FEvaluate @ FTruncate[ym, FTakeDerivatives[ym, FMakeDSE[ym, A[i1]], {A[i2]}]]]["2-Loop"], All},
  (* Equation and diagram output: styled gluon two-point flow *)
  "diagram_9" :> {ymStyled, FTruncate[ymStyled, FTakeDerivatives[ymStyled, wEq, {A[i1], A[i2]}]], All},
  (* Manually creating diagrammatic rules: ghost two-point flow *)
  "diagram_5" :> {ymStyled, FTruncate[ymStyled, FTakeDerivatives[ymStyled, wEq, {cb[i1], c[i2]}]], All},
  (* Momentum projections: ghost-gluon vertex DSE *)
  "diagram_8" :> {ym, FTruncate[ym, FTakeDerivatives[ym, FMakeDSE[ym, A[i1]], {cb[i2], c[i3]}]], 3},
  (* NJL: fermion two-point flow *)
  "diagram_6" :> {njl, FTruncate[njl, FTakeDerivatives[njl, WetterichEquation, {psibar[i1], psi[i2]}]], All},
  (* NJL: four-fermion flow *)
  "diagram_7" :> {njl, FTruncate[njl, FTakeDerivatives[njl, WetterichEquation, {psibar[i1], psi[i2], psibar[i3], psi[i4]}]], 3}
|>;

(* ---------------------------------------------------------------------------------------------
   Diagram data from FunKit
--------------------------------------------------------------------------------------------- *)

(* {prefactor, diagramGraph[...]} per term, in term order (FPlot shows them as a sorted Plus) *)
diagrams[setup_, expr_FDeferred] := diagrams[setup, FEvaluate[expr]];
diagrams[setup_, expr_FEx] := Join @@ (diagrams[setup, #]& /@ Select[List @@ expr, Head[#] === FTerm&]);
diagrams[setup_, term_FTerm] :=
  (* Take the arguments FGetDiagram passes to Graph: a Graph keeps one style per vertex pair *)
  Block[{Graph},
    Graph[vertices_List, edges_List, opts___] := diagramGraph[vertices, edges, Association[opts]];
    Graph[g_diagramGraph, ___] := g;
    {FunKit`Private`FGetDiagram[setup, term]}
  ];

(* One loop order of an FRoute result: label the external legs by their momenta, as FPlot does *)
diagrams[setup_, expr_Association] /; KeyExistsQ[expr, "ExternalIndices"] :=
  Module[{legs = Values[expr["ExternalIndices"]], placeholders},
    placeholders = Table[Unique["leg"], Length[legs]];
    Block[{FunKit`Private`$externalLabelOverride = AssociationThread[placeholders, legs[[All, 1]]]},
      diagrams[setup, expr["Expression"] /. Thread[legs -> placeholders]]
    ]
  ];

(* ---------------------------------------------------------------------------------------------
   Drawing one diagram
--------------------------------------------------------------------------------------------- *)

edgeData[Style[e_, style___]] := {toEdge[e], {style}};
edgeData[e_] := {toEdge[e], {}};
toEdge[(Rule | DirectedEdge)[a_, b_]] := DirectedEdge[a, b];
toEdge[(TwoWayRule | UndirectedEdge)[a_, b_]] := UndirectedEdge[a, b];

(* FPlot's vertex positions, scaled so that the median line has length 1 *)
vertexPositions[vertices_, edges_, opts_] :=
  Module[{g, pos, lengths},
    g = Graph[vertices, edges, GraphLayout -> opts[GraphLayout], EdgeWeight -> opts[EdgeWeight]];
    pos = AssociationThread[VertexList[g], GraphEmbedding[g]];
    lengths = Norm[pos[#[[1]]] - pos[#[[2]]]]& /@ Select[edges, #[[1]] =!= #[[2]]&];
    If[lengths === {}, pos, pos / Median[lengths]]
  ];

bezierPoint[pts_, t_] := Sum[BernsteinBasis[Length[pts] - 1, k - 1, t] pts[[k]], {k, Length[pts]}];
normal[{x_, y_}] := {-y, x};

neighbours[v_, edges_, pos_] := Cases[edges, _[v, w_] | _[w_, v] /; w =!= v :> pos[w]];

(* Control points of every edge. Parallel lines bend apart symmetrically; self-loops go into the
   widest free angle around their vertex. *)
edgeCurves[edges_, pos_] :=
  Module[{curves = ConstantArray[None, Length[edges]]},
    KeyValueMap[
      Function[{pair, idx},
        curves[[idx]] = If[pair[[1]] === pair[[2]],
          loopCurve[pos[pair[[1]]], #]& /@ loopAngles[pair[[1]], Length[idx], edges, pos],
          MapIndexed[parallelCurve[pos, pair, #1, (#2[[1]] - (Length[idx] + 1) / 2) $parallelSpacing]&, edges[[idx]]]
        ]
      ],
      GroupBy[Range[Length[edges]], Sort[List @@ edges[[#]]]&]
    ];
    curves
  ];

parallelCurve[pos_, pair_, edge_, offset_] :=
  With[{n = normal @ Normalize[pos[pair[[2]]] - pos[pair[[1]]]]},
    {pos[edge[[1]]], Mean[pos /@ pair] + 2 offset n, pos[edge[[2]]]}
  ];

loopCurve[p_, angle_] := {p, p + $loopSize AngleVector[angle + 0.6], p + $loopSize AngleVector[angle - 0.6], p};

(* Directions of n self-loops at v, each into the currently widest gap between lines *)
loopAngles[v_, n_, edges_, pos_] :=
  Module[{taken, gaps, k, angles = {}},
    taken = Sort[ArcTan @@ (# - pos[v])& /@ neighbours[v, edges, pos]];
    Do[
      If[taken === {},
        AppendTo[angles, Pi / 2],
        gaps = Differences[Append[taken, First[taken] + 2 Pi]];
        k = Last @ Ordering[gaps];
        AppendTo[angles, Mod[taken[[k]] + gaps[[k]] / 2, 2 Pi, -Pi]]
      ];
      taken = Sort[Append[taken, Last[angles]]],
      {n}
    ];
    angles
  ];

arrowhead[pts_] :=
  Module[{m = bezierPoint[pts, 1/2], t, n, a = $arrowSize},
    t = Normalize[bezierPoint[pts, 0.51] - bezierPoint[pts, 0.49]];
    n = normal[t];
    Polygon[{m + 0.5 a t, m - 0.5 a t + 0.3 a n, m - 0.25 a t, m - 0.5 a t - 0.3 a n}]
  ];

(* distance from the centre of a w x h box to its boundary along the unit vector {dx, dy} *)
halfExtent[{w_, h_}, {dx_, dy_}] := Min[If[dx == 0, Infinity, w / 2 / Abs[dx]], If[dy == 0, Infinity, h / 2 / Abs[dy]]];

texLabel[s_String] := StringReplace[s, RegularExpression["([A-Za-z]+)(\\d+)"] -> "$1_{$2}"];

(* External-leg labels sit beyond the end of the leg, along its direction *)
labelInsets[labels_, edges_, pos_] :=
  Module[{vertices = Keys[labels], texts, sizes, direction, centre},
    If[vertices === {}, Return[{{}, {}}]];
    texts = MaTeX`MaTeX[texLabel /@ Values[labels], FontSize -> $labelFontSize];
    sizes = (ImageSize /. Options[#, ImageSize]) / $propagatorPoints& /@ texts;
    centre = MapThread[
      Function[{v, size},
        direction = Normalize[pos[v] - First[neighbours[v, edges, pos], pos[v] - {0, 1}]];
        pos[v] + direction ($labelGap + halfExtent[size, direction])
      ],
      {vertices, sizes}
    ];
    {
      MapThread[Inset[#1, #2, Center, #3[[1]]]&, {texts, centre, sizes}],
      Flatten[MapThread[{#1 - #2 / 2, #1 + #2 / 2}&, {centre, sizes}], 1]
    }
  ];

renderDiagram[diagramGraph[vertices_, rawEdges_, opts_]] :=
  Module[{edges, styles, pos, curves, shapes, sizes, labels, labelPrims, labelPoints, colours, points, range},
    {edges, styles} = Transpose[edgeData /@ rawEdges];
    pos = vertexPositions[vertices, edges, opts];
    curves = edgeCurves[edges, pos];
    shapes = Association[opts[VertexShape]];
    sizes = KeySelect[Association[opts[VertexSize]], KeyExistsQ[pos, #] && KeyExistsQ[shapes, #]&];
    labels = Association[opts[VertexLabels] /. Placed[lbl_, ___] :> First[Cases[lbl, _String, {0, Infinity}]]];
    {labelPrims, labelPoints} = labelInsets[labels, edges, pos];
    colours = Cases[#, _?ColorQ]& /@ styles;
    points = Join[
      Flatten[Table[bezierPoint[c, t], {c, curves}, {t, Subdivide[8]}], 1],
      Flatten[KeyValueMap[{pos[#1] - $vertexScale #2 / 2, pos[#1] + $vertexScale #2 / 2}&, sizes], 1],
      labelPoints
    ];
    range = MinMax /@ Transpose[points] + {{-1, 1}, {-1, 1}} $lineWidth / $propagatorPoints;
    Graphics[
      {
        CapForm["Round"],
        MapThread[{AbsoluteThickness[$lineWidth], Sequence @@ #2, BezierCurve[#1, SplineDegree -> Length[#1] - 1]}&, {curves, styles}],
        MapThread[If[Head[#1] === DirectedEdge, {Sequence @@ #3, arrowhead[#2]}, Nothing]&, {edges, curves, colours}],
        (* a white backdrop keeps lines out of open vertex shapes such as the regulator insertion *)
        KeyValueMap[{White, Disk[pos[#1], $vertexScale #2 / 2], Inset[shapes[#1], pos[#1], Center, $vertexScale #2]}&, sizes],
        labelPrims
      },
      PlotRange -> range, PlotRangePadding -> None, ImagePadding -> None,
      ImageSize -> $propagatorPoints (#[[2]] - #[[1]]& /@ range)
    ]
  ];

(* ---------------------------------------------------------------------------------------------
   Assembling and writing a figure
--------------------------------------------------------------------------------------------- *)

tex[s_String] := MaTeX`MaTeX[s];

(* "prefactor diagram + prefactor diagram + ...". Terms whose prefactor contains Grassmann signs
   are joined with \oplus and shown without prefactor, as FPlot does. *)
layout[parts_List, perRow_] :=
  Module[{signed, cells},
    signed = FreeQ[parts[[All, 1]], FunKit`Private`shortenTexTag];
    cells = MapIndexed[
      Function[{part, k},
        Grid[
          {{
            If[k[[1]] > 1, tex[If[signed, "+", "\\oplus"]], Nothing],
            If[signed && part[[1]] =!= "", part[[1]], Nothing],
            renderDiagram[part[[2]]]
          }},
          Alignment -> {Automatic, Center}, Spacings -> {0.6, 0}
        ]
      ],
      parts
    ];
    Column[
      Grid[{#}, Alignment -> {Left, Center}, Spacings -> 1.5]& /@ Partition[cells, UpTo[Replace[perRow, All -> Length[cells]]]],
      Spacings -> 1.5
    ]
  ];

makeFigure[name_String] :=
  Module[{setup, expr, perRow, raw, out, crop},
    {setup, expr, perRow} = $figures[name];
    raw = FileNameJoin[{$TemporaryDirectory, name <> "-uncropped.pdf"}];
    out = FileNameJoin[{$outDir, name <> ".pdf"}];
    Export[raw, layout[diagrams[setup, expr], perRow], "PDF"];
    crop = RunProcess[{"pdfcrop", "--margins", ToString[$cropMargin], raw, out}];
    DeleteFile[raw];
    If[crop["ExitCode"] =!= 0, Print[crop["StandardError"]]; Exit[1]];
    Print["Wrote ", out];
  ];

(* ---------------------------------------------------------------------------------------------
   Main
--------------------------------------------------------------------------------------------- *)

(* wolframscript fills $ScriptCommandLine; wolfram -script only $CommandLine *)
$arguments = Rest @ Replace[$ScriptCommandLine, {} :> Drop[$CommandLine, First @ FirstPosition[$CommandLine, "-script"]]];
$selected = Replace[$arguments, {} -> Keys[$figures]];
If[!SubsetQ[Keys[$figures], $selected],
  Print["Unknown figure(s): ", Complement[$selected, Keys[$figures]], ". Known: ", Keys[$figures]];
  Exit[1]
];
Scan[makeFigure, $selected];
