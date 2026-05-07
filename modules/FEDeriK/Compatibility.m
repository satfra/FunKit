(**********************************************************************************
    Compatibility.m -- Interoperability with QMeS and DoFun packages

    Public API:
      FunKitForm                 -- Converts QMeS/DoFun expressions to FunKit form
      QMeSForm                   -- Converts FunKit expressions to QMeS notation
      DoFunForm                  -- Converts FunKit to DoFun notation

    Internal:
      QMeSNaming                 -- Renames a single object to QMeS convention
                                    (used by QMeSForm for routed output)
      QMeSSuperindexDiagramQ     -- Tests if a list is a QMeS superindex diagram
                                    (used by FunKitForm)
      DoFunSuperindexDiagramQ    -- Tests if an expression is a DoFun superindex diagram
                                    (used by FunKitForm)
      DoFunAlgebraicDiagramQ     -- Tests if a list is a DoFun algebraic diagram
                                    (used by FunKitForm)
      QMeSNamedDiagramQ          -- Tests if a list is a QMeS named-symbol diagram
                                    (used by FunKitForm)
**********************************************************************************)

FunKitForm::parseError = "Cannot parse field names from QMeS symbol name `1`.";

FunKitForm::indexMismatch = "Index count mismatch for QMeS symbol `1`: got `2` indices, expected `3` (superindex) or `4` (routed).";

(**********************************************************************************
    Detection helpers: superindex vs routed
**********************************************************************************)

routedObjectQ[obj_ /; orderedObjectQ[obj]] :=
    AnyTrue[getIndices[obj], Head[#] === List&];

routedObjectQ[___] :=
    False;

routedFTermQ[fterm_FTerm] :=
    AnyTrue[Select[List @@ fterm, orderedObjectQ], routedObjectQ];

routedFTermQ[___] :=
    False;

routedFExQ[fex_FEx] :=
    Module[{terms},
        terms = Select[List @@ fex, Head[#] === FTerm&];
        If[Length[terms] === 0,
            Return[False]
        ];
        routedFTermQ[terms[[1]]]
    ];

routedFExQ[___] :=
    False;

(**********************************************************************************
    QMeS Compatibility: FunKit -> QMeS
**********************************************************************************)

(* Named-symbol conversion for routed output *)

QMeSNaming[setup_, expr_] :=
    expr;

QMeSNaming[setup_, obj_ /; orderedObjectQ[obj]] :=
    Module[
        {oldCanonicalOrdering, transf, prefactor, mobj, mfields, mindices, prefix, fieldPart, indexPart}
        ,
        (*QMeS follows c>ag>g, so we switch temporarily!*)
        Block[{$CanonicalOrdering},
            $CanonicalOrdering = "c>ag>g";
            transf = OrderObject[setup, obj];
        ];
        prefactor = 1;
        If[MatchQ[transf, Times[-1, a_]],
            prefactor = -1;
            transf = -transf;
        ];
        mobj = Head[transf];
        mfields = getFields[transf];
        mindices = getIndices[transf];
        prefix =
            Switch[Head[obj],
                Propagator,
                    "G"
                ,
                GammaN,
                    "\[CapitalGamma]"
                ,
                Rdot,
                    "Rdot"
                ,
                _,
                    ToString[Head[obj]]
            ];
        fieldPart = StringJoin[Map[ToString, mfields]];
        indexPart = Flatten[mindices];
        Return[prefactor * Symbol[prefix <> fieldPart][indexPart]];
    ];

(* Native Association format for superindex output *)

qmeSNativeObject[setup_, obj_ /; orderedObjectQ[obj]] :=
    Module[{transf, prefactor, head, mfields, mindices, typeStr, specStr, nPointVal, indices},
        Block[{$CanonicalOrdering},
            $CanonicalOrdering = "c>ag>g";
            transf = OrderObject[setup, obj];
        ];
        prefactor = 1;
        If[MatchQ[transf, Times[-1, a_]],
            prefactor = -1;
            transf = -transf;
        ];
        head = Head[transf];
        mfields = getFields[transf];
        mindices = getIndices[transf];
        Switch[head,
            Propagator,
                indices = MapThread[{#1, {#2}}&, {mfields, mindices}];
                {prefactor, <|"type" -> "Propagator", "indices" -> indices|>}
            ,
            Rdot,
                indices = MapThread[{#1, {-#2}}&, {mfields, mindices}];
                {prefactor, <|"type" -> "Regulatordot", "indices" -> indices|>}
            ,
            GammaN,
                indices = MapThread[{#1, {-#2}}&, {mfields, mindices}];
                {prefactor, <|"type" -> "nPoint", "indices" -> indices, "nPoint" -> Length[mfields], "spec" -> "none"|>}
            ,
            S,
                indices = MapThread[{#1, {-#2}}&, {mfields, mindices}];
                {prefactor, <|"type" -> "nPoint", "indices" -> indices, "nPoint" -> Length[mfields], "spec" -> "classical"|>}
        ]
    ];

qmeSNativeTerm[setup_, fterm_FTerm] :=
    Module[{elems, objects, coeffs, converted, totalPrefactor},
        elems = List @@ fterm;
        objects = Select[elems, orderedObjectQ];
        coeffs = Select[elems, (!orderedObjectQ[#])&];
        converted = Map[qmeSNativeObject[setup, #]&, objects];
        totalPrefactor = Times @@ coeffs * Times @@ converted[[All, 1]];
        Prepend[converted[[All, 2]], "Prefactor" -> {totalPrefactor}]
    ];

(* QMeSForm: superindex input -> QMeS native Association format *)

QMeSForm[setup_, obj_ /; orderedObjectQ[obj] && !routedObjectQ[obj]] :=
    (
        AssertFSetup[setup];
        qmeSNativeObject[setup, obj]
    );

QMeSForm[setup_, fterm_FTerm] /; !routedFTermQ[fterm] :=
    (
        AssertFSetup[setup];
        qmeSNativeTerm[setup, fterm]
    );

QMeSForm[setup_, fex_FEx] /; !routedFExQ[fex] :=
    (
        AssertFSetup[setup];
        Map[qmeSNativeTerm[setup, #]&, SeparateFExAnnotations[fex][[1]]]
    );

(* QMeSForm: routed input -> QMeS named-symbol format *)

QMeSForm[setup_, obj_ /; orderedObjectQ[obj] && routedObjectQ[obj]] :=
    (
        AssertFSetup[setup];
        QMeSNaming[setup, obj]
    );

QMeSForm[setup_, fterm_FTerm] /; routedFTermQ[fterm] :=
    (
        AssertFSetup[setup];
        Map[QMeSNaming[setup, #]&, fterm, {1, 3}] //. {FTerm :> Times}
    );

QMeSForm[setup_, fex_FEx] /; routedFExQ[fex] :=
    (
        AssertFSetup[setup];
        Map[QMeSNaming[setup, #]&, fex, {1, 3}] //. {FEx :> List, FTerm :> Times}
    );

(* QMeSForm: routed Association input *)

QMeSForm[setup_, assoc_Association] /; isLoopAssociation[assoc] :=
    (
        AssertFSetup[setup];
        QMeSForm[setup, assoc["Expression"]]
    );

QMeSForm[setup_, assoc_Association] /; isRoutedAssociation[assoc] :=
    (
        AssertFSetup[setup];
        Map[QMeSForm[setup, #]&, assoc]
    );

(* QMeSForm: generic Association fallback *)

QMeSForm[setup_, expr_Association] :=
    (
        AssertFSetup[setup];
        AssociationMap[QMeSForm[setup, #]&, expr]
    );

(**********************************************************************************
    QMeS -> FunKit: superindex (native Association format)
**********************************************************************************)

QMeSSuperindexDiagramQ[__] :=
    False;

QMeSSuperindexDiagramQ[l_List] :=
    Module[{yes},
        If[l[[1, 1]] =!= "Prefactor",
            Return[False]
        ];
        If[Not @ AllTrue[l[[2 ;; ]], AssociationQ],
            Return[False]
        ];
        If[Not @ AllTrue[l[[2 ;; ]], KeyMemberQ["type"]],
            Return[False]
        ];
        If[Not @ AllTrue[l[[2 ;; ]], KeyMemberQ["indices"]],
            Return[False]
        ];
        Return[True];
    ];

FunKitForm[setup_, diag_List] /; QMeSSuperindexDiagramQ[diag] :=
    Module[{pref, newa},
        pref = diag[[1, 2, 1]];
        newa = diag[[2 ;; ]];
        newa =
            newa //.
                {
                    <|"type" -> "Regulatordot", "indices" -> {a__}|> :> makeObj[Rdot, {a}[[All, 1]], -{a}[[All, 2, 1]]]
                    ,(**)
                    <|"type" -> "Propagator", "indices" -> {a__}|> :> makeObj[Propagator, {a}[[All, 1]], {a}[[All, 2, 1]]]
                    ,(**)
                    <|"type" -> "nPoint", "indices" -> {a__}, "nPoint" -> _, "spec" -> "classical"|> :> makeObj[S, {a}[[All, 1]], -{a}[[All, 2, 1]]]
                    ,(**)
                    <|"type" -> "nPoint", "indices" -> {a__}, "nPoint" -> _, "spec" -> "none"|> :> makeObj[GammaN, {a}[[All, 1]], -{a}[[All, 2, 1]]]
                };
        Return[FTerm[pref, ##]& @@ newa]
    ];

FunKitForm[setup_, expr_List] /; AllTrue[expr, QMeSSuperindexDiagramQ] :=
    FEx @@ Map[FunKitForm[setup, #]&, expr];

(**********************************************************************************
    QMeS -> FunKit: routed (named-symbol format)
**********************************************************************************)

QMeSNamedDiagramQ[expr_List] :=
    Module[{},
        If[Length[expr] === 0,
            Return[False]
        ];
        AnyTrue[
            expr
            ,
            Module[{factors},
                factors =
                    If[Head[#] === Times,
                        List @@ #
                        ,
                        If[Length[#] === 1 && Head[#[[1]]] === Times,
                            List @@ #[[1]]
                            ,
                            {#}
                        ]
                    ];
                AnyTrue[factors, MatchQ[#, _Symbol[_List]] && StringMatchQ[SymbolName[Head[#]], ("G" | "\[CapitalGamma]" | "Rdot" | "S") ~~ __]&]
            ]&
        ]
    ];

QMeSNamedDiagramQ[___] :=
    False;

parseFieldNames[setup_, str_String] :=
    Module[{allFields, fieldNames, sorted, result = {}, remaining = str},
        allFields = GetAllFields[setup];
        fieldNames = Map[ToString, allFields];
        sorted = SortBy[fieldNames, -StringLength[#]&];
        While[
            StringLength[remaining] > 0
            ,
            Module[{matched = False},
                Do[
                    If[StringStartsQ[remaining, fn],
                        AppendTo[result, Symbol[fn]];
                        remaining = StringDrop[remaining, StringLength[fn]];
                        matched = True;
                        Break[];
                    ];
                    ,
                    {fn, sorted}
                ];
                If[!matched,
                    Message[FunKitForm::parseError, str];
                    Abort[]
                ];
            ];
        ];
        result
    ];

reverseQMeSNaming[setup_, sym_Symbol[indices_List]] :=
    Module[{name, prefix, head, fieldStr, fields, nFields, slotsPerField, result, pos},
        name = SymbolName[sym];
        (* Identify prefix and head *)
        {prefix, head} =
            Which[
                StringStartsQ[name, "\[CapitalGamma]"],
                    {"\[CapitalGamma]", GammaN}
                ,
                StringStartsQ[name, "Rdot"],
                    {"Rdot", Rdot}
                ,
                StringStartsQ[name, "G"],
                    {"G", Propagator}
                ,
                True,
                    {StringTake[name, 1], S}
            ];
        fieldStr = StringDrop[name, StringLength[prefix]];
        fields = parseFieldNames[setup, fieldStr];
        nFields = Length[fields];
        slotsPerField = Map[Length[FieldSetupIndices[setup, #]]&, fields];
        pos = 1;
        result =
            Table[
                Module[{nSlots = slotsPerField[[i]], chunk},
                    chunk = indices[[pos ;; pos + nSlots - 1]];
                    pos += nSlots;
                    If[nSlots === 1,
                        {chunk[[1]]}
                        ,
                        {chunk[[1]], chunk[[2 ;; ]]}
                    ]
                ]
                ,
                {i, 1, nFields}
            ];
        makeObj[head, fields, result]
    ];

FunKitForm[setup_, expr_List] /; QMeSNamedDiagramQ[expr] :=
    Module[{},
        AssertFSetup[setup];
        FEx @@
            Map[
                Module[{factors, numericPart, symbolicParts, objects},
                    factors =
                        If[Head[#] === Times,
                            List @@ #
                            ,
                            If[Length[#] === 1 && Head[#[[1]]] === Times,
                                List @@ #[[1]]
                                ,
                                {#}
                            ]
                        ];
                    numericPart = Select[factors, NumericQ];
                    symbolicParts = Select[factors, MatchQ[#, _Symbol[_List]]&];
                    objects = Map[reverseQMeSNaming[setup, #]&, symbolicParts];
                    FTerm @@ Join[numericPart, objects]
                ]&
                ,
                expr
            ]
    ];

(**********************************************************************************
    DoFun Compatibility: FunKit -> DoFun
**********************************************************************************)

(* Superindex detection *)

DoFunSuperindexDiagramQ[expr_DoFun`DoDSERGE`op] :=
    True;

DoFunSuperindexDiagramQ[expr_] :=
    Module[{l},
        If[FreeQ[expr, DoFun`DoDSERGE`op[__], Infinity],
            Return[False]
        ];
        l = expr //. Times[a__, DoFun`DoDSERGE`op[b__]] :> DoFun`DoDSERGE`op[b];
        If[Head[l] === Plus,
            l = List @@ l
            ,
            l = {l};
        ];
        Return[AllTrue[l, DoFunSuperindexDiagramQ]];
    ];

(* Algebraic detection *)

DoFunAlgebraicDiagramQ[expr_List] :=
    Module[{},
        If[Length[expr] === 0,
            Return[False]
        ];
        (* Algebraic format has P/V/S/dR with field-application args and a trailing Rule (explicit->...) *)
        AnyTrue[expr, Not @ FreeQ[#, (DoFun`DoDSERGE`P | DoFun`DoDSERGE`V | DoFun`DoDSERGE`S | DoFun`DoDSERGE`dR)[___, _Rule]]&]
    ];

DoFunAlgebraicDiagramQ[___] :=
    False;

(**********************************************************************************
    DoFun -> FunKit: superindex (symbolic format)
**********************************************************************************)

FunKitForm[setup_, diag_] /; DoFunSuperindexDiagramQ[diag] :=
    Module[{repl, hasGrassmann},
        (* DoFun and FunKit disagree by a sign for every interaction vertex (n>=3)
           that carries a Grassmann field. The mismatch arises from DoFun's
           left-only Grassmann-derivative convention (DoFun 3, footnote 3 of
           arXiv:1908.02760) combined with the bare-vertex sign S^{i1...in} =
           -delta^n S/delta phi^n (Eq. 7 of arXiv:0808.2939) — both bare S and
           dressed GammaN with Grassmann legs pick up a -1 per occurrence
           relative to FunKit's symbolic representation. The 2-pt kinetic S is
           exempt because n=2 is sign-free in DoFun's expansion convention. *)
        hasGrassmann[fields_] := AnyTrue[fields, IsGrassmann[setup, #]&];
        repl =
            {
                Times[a___, DoFun`DoDSERGE`op[f__]] :> FTerm[a, f]
                , (**)
                DoFun`DoDSERGE`op[f__] :> FTerm[f]
                ,
                DoFun`DoDSERGE`P[f__] :> makeObj[Propagator, {f}[[All, 1]], {f}[[All, 2]]]
                ,
                DoFun`DoDSERGE`V[f__] :>
                    With[{fields = {f}[[All, 1]], indices = {f}[[All, 2]]},
                        If[hasGrassmann[fields],
                            -makeObj[GammaN, fields, indices],
                            makeObj[GammaN, fields, indices]
                        ]
                    ]
                ,
                DoFun`DoDSERGE`dR[f__] :> makeObj[Rdot, {f}[[All, 1]], {f}[[All, 2]]]
                ,
                DoFun`DoDSERGE`S[f__] :>
                    With[{fields = {f}[[All, 1]], indices = {f}[[All, 2]]},
                        If[Length[fields] >= 3 && hasGrassmann[fields],
                            -makeObj[S, fields, -indices],
                            makeObj[S, fields, -indices]
                        ]
                    ]
            };
        FunKit`FEx[diag //. repl]
    ];

(**********************************************************************************
    DoFun -> FunKit: algebraic (routed format)
**********************************************************************************)

doFunArgToRoutedIdx[arg_] :=
    Module[{elems},
        elems = List @@ arg;
        If[Length[elems] === 1,
            {elems[[1]]}
            ,
            {elems[[1]], elems[[2 ;; ]]}
        ]
    ];

FunKitForm[setup_, expr_List] /; DoFunAlgebraicDiagramQ[expr] :=
    Module[{repl},
        repl =
            {
                DoFun`DoDSERGE`P[f__, _Rule] :> makeObj[Propagator, Head /@ {f}, doFunArgToRoutedIdx /@ {f}]
                ,
                DoFun`DoDSERGE`V[f__, _Rule] :> makeObj[GammaN, Head /@ {f}, doFunArgToRoutedIdx /@ {f}]
                ,
                DoFun`DoDSERGE`dR[f__, _Rule] :> makeObj[Rdot, Head /@ {f}, doFunArgToRoutedIdx /@ {f}]
                ,
                DoFun`DoDSERGE`S[f__, _Rule] :>
                    Module[{fields = Head /@ {f}, indices = doFunArgToRoutedIdx /@ {f}},
                        makeObj[S, fields, Map[ReplacePart[#, 1 -> -#[[1]]]&, indices]]
                    ]
            };
        FEx @@
            Map[
                Module[{factors, converted},
                    converted = # //. repl;
                    If[Head[converted] === Times,
                        FTerm @@ (List @@ converted)
                        ,
                        FTerm[converted]
                    ]
                ]&
                ,
                expr
            ]
    ];

(**********************************************************************************
    DoFunForm: superindex -> DoFun symbolic format
**********************************************************************************)

doFunObject[obj_Propagator] :=
    DoFun`DoDSERGE`P @@ Transpose[{getFields[obj], getIndices[obj]}];

doFunObject[obj_GammaN] :=
    DoFun`DoDSERGE`V @@ Transpose[{getFields[obj], getIndices[obj]}];

doFunObject[obj_Rdot] :=
    DoFun`DoDSERGE`dR @@ Transpose[{getFields[obj], getIndices[obj]}];

doFunObject[obj_S] :=
    DoFun`DoDSERGE`S @@ Transpose[{getFields[obj], -getIndices[obj]}];

(* Per-Grassmann-vertex sign factor that mirrors the FunKitForm[] flip rule, so
   that DoFun -> FunKit -> DoFun roundtrips preserve the original expression. *)
doFunObjectSign[setup_, obj_] :=
    Module[{fields = getFields[obj], head = Head[obj]},
        Which[
            head === S && Length[fields] >= 3 && AnyTrue[fields, IsGrassmann[setup, #]&], -1,
            head === GammaN && AnyTrue[fields, IsGrassmann[setup, #]&], -1,
            True, 1
        ]
    ];

doFunTerm[setup_, fterm_FTerm] :=
    Module[{elems, objects, coeffs, signFactor},
        elems = List @@ fterm;
        objects = Select[elems, orderedObjectQ];
        coeffs = Select[elems, (!orderedObjectQ[#])&];
        signFactor = Times @@ Map[doFunObjectSign[setup, #]&, objects];
        If[objects === {},
            Times @@ coeffs
            ,
            signFactor * Times @@ coeffs * DoFun`DoDSERGE`op @@ Map[doFunObject, objects]
        ]
    ];

DoFunForm[setup_, obj_ /; orderedObjectQ[obj] && !routedObjectQ[obj]] :=
    (
        AssertFSetup[setup];
        doFunObjectSign[setup, obj] * doFunObject[obj]
    );

DoFunForm[setup_, fterm_FTerm] /; !routedFTermQ[fterm] :=
    (
        AssertFSetup[setup];
        doFunTerm[setup, fterm]
    );

DoFunForm[setup_, fex_FEx] /; !routedFExQ[fex] :=
    (
        AssertFSetup[setup];
        Plus @@ Map[doFunTerm[setup, #]&, SeparateFExAnnotations[fex][[1]]]
    );

(**********************************************************************************
    DoFunForm: routed -> DoFun algebraic format
**********************************************************************************)

routedIdxToDoFunArg[field_, idx_List] :=
    field[Sequence @@ Flatten[idx]];

doFunAlgebraicObject[obj_Propagator] :=
    Module[{fields = getFields[obj], indices = getIndices[obj]},
        DoFun`DoDSERGE`P[Sequence @@ MapThread[routedIdxToDoFunArg, {fields, indices}], Global`explicit -> False]
    ];

doFunAlgebraicObject[obj_GammaN] :=
    Module[{fields = getFields[obj], indices = getIndices[obj]},
        DoFun`DoDSERGE`V[Sequence @@ MapThread[routedIdxToDoFunArg, {fields, indices}], Global`explicit -> False]
    ];

doFunAlgebraicObject[obj_Rdot] :=
    Module[{fields = getFields[obj], indices = getIndices[obj]},
        DoFun`DoDSERGE`dR[Sequence @@ MapThread[routedIdxToDoFunArg, {fields, indices}], Global`explicit -> False]
    ];

doFunAlgebraicObject[obj_S] :=
    Module[{fields = getFields[obj], indices = getIndices[obj], negIndices},
        negIndices = Map[ReplacePart[#, 1 -> -#[[1]]]&, indices];
        DoFun`DoDSERGE`S[Sequence @@ MapThread[routedIdxToDoFunArg, {fields, negIndices}], Global`explicit -> False]
    ];

doFunAlgebraicTerm[fterm_FTerm] :=
    Module[{elems, objects, coeffs},
        elems = List @@ fterm;
        objects = Select[elems, orderedObjectQ];
        coeffs = Select[elems, (!orderedObjectQ[#])&];
        If[objects === {},
            Times @@ coeffs
            ,
            Times @@ coeffs * Times @@ Map[doFunAlgebraicObject, objects]
        ]
    ];

DoFunForm[setup_, obj_ /; orderedObjectQ[obj] && routedObjectQ[obj]] :=
    (
        AssertFSetup[setup];
        doFunAlgebraicObject[obj]
    );

DoFunForm[setup_, fterm_FTerm] /; routedFTermQ[fterm] :=
    (
        AssertFSetup[setup];
        doFunAlgebraicTerm[fterm]
    );

DoFunForm[setup_, fex_FEx] /; routedFExQ[fex] :=
    (
        AssertFSetup[setup];
        Map[doFunAlgebraicTerm, SeparateFExAnnotations[fex][[1]]]
    );

(* DoFunForm: routed Association input *)

DoFunForm[setup_, assoc_Association] /; isLoopAssociation[assoc] :=
    (
        AssertFSetup[setup];
        DoFunForm[setup, assoc["Expression"]]
    );

DoFunForm[setup_, assoc_Association] /; isRoutedAssociation[assoc] :=
    (
        AssertFSetup[setup];
        Map[DoFunForm[setup, #]&, assoc]
    );

(* DoFunForm: generic Association fallback *)

DoFunForm[setup_, expr_Association] :=
    (
        AssertFSetup[setup];
        AssociationMap[DoFunForm[setup, #]&, expr]
    );
