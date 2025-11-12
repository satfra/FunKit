(**********************************************************************************
    General Compatibility definitions
**********************************************************************************)

FunKitForm[expr_List] :=
    FEx @@ Map[FunKitForm, expr];

(**********************************************************************************
    QMeS Compatibility
**********************************************************************************)

QMeSNaming[setup_, expr_] :=
    expr;

QMeSNaming[setup_, obj_[fields_List, indices_List] /; MemberQ[$OrderedObjects, obj]] :=
    Module[
        {oldCanonicalOrdering, transf, prefactor, mobj, mfields, mindices, prefix, fieldPart, indexPart}
        ,
        (*QMeS follows b>af>f, so we switch temporarily!*)
        Block[{$CanonicalOrdering},
            $CanonicalOrdering = "b>af>f";
            transf = OrderObject[setup, obj[fields, indices]];
        ];
        prefactor = 1;
        If[MatchQ[transf, Times[-1, a_]],
            prefactor = -1;
            transf = -transf;
        ];
        mobj = Head[transf];
        mfields = (List @@ transf)[[1]];
        mindices = (List @@ transf)[[2]];
        prefix =
            Switch[obj,
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
                    ToString[obj]
            ];
        fieldPart = StringJoin[Map[ToString, mfields]];
        indexPart = Flatten[mindices];
        Return[prefactor * Symbol[prefix <> fieldPart][indexPart]];
    ];

QMeSForm[setup_, expr_] :=
    Map[QMeSNaming[setup, #]&, expr, {1, 3}] //. {FEx :> List, FTerm :> Times};

QMeSForm[setup_, expr_Association] :=
    Map[QMeSForm[setup, #]&, expr];

(* Transforming QMeS to FunKit *)

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

FunKitForm[diag_List] /; QMeSSuperindexDiagramQ[diag] :=
    Module[{pref, newa},
        pref = diag[[1, 2, 1]];
        newa = diag[[2 ;; ]];
        newa = newa //. {<|"type" -> "Regulatordot", "indices" -> {a__}|> :> Rdot[{a}[[All, 1]], {a}[[All, 2, 1]]], <|"type" -> "Propagator", "indices" -> {a__}|> :> Propagator[{a}[[All, 1]], {a}[[All, 2, 1]]], <|"type" -> "nPoint", "indices" -> {a__}, __|> :> GammaN[{a}[[All, 1]], {a}[[All, 2, 1]]]};
        Return[FTerm[pref, ##]& @@ newa]
    ];

(**********************************************************************************
    DoFun Compatibility
**********************************************************************************)

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
        ];
        Return[AllTrue[l, DoFunSuperindexDiagramQ]];
    ];

FunKitForm[diag_] /; DoFunSuperindexDiagramQ[diag] :=
    Module[{repl},
        repl =
            {
                DoFun`DoDSERGE`op[f__] :> FunKit`FTerm[f]
                , (**)
                DoFun`DoDSERGE`P[f__] :> FunKit`Propagator[{f}[[All, 1]], {f}[[All, 2 ;; ]]]
                ,
                DoFun`DoDSERGE`V[f__] :> FunKit`GammaN[{f}[[All, 1]], {f}[[All, 2 ;; ]]]
                ,
                DoFun`DoDSERGE`dR[f__] :> FunKit`Regulatordot[{f}[[All, 1]], {f}[[All, 2 ;; ]]]
            };
        FEx[diag //. repl]
    ];
