GetcFields[setup_] :=
    GetcFields[setup] =
        Map[
            If[Head[#] === List,
                Head[#[[2]]]
                ,
                Head[#]
            ]&
            ,
            setup["FieldSpace"]["Commuting"]
        ];

GetAnticFields[setup_] :=
    GetAnticFields[setup] =
        Select[
            Map[
                If[Head[#] === List,
                    Head[#[[1]]]
                    ,
                    {}
                ]&
                ,
                setup["FieldSpace"]["Commuting"]
            ]
            ,
            # =!= {}&
        ];

GetFermions[setup_] :=
    GetFermions[setup] =
        Map[
            If[Head[#] === List,
                Head[#[[2]]]
                ,
                Head[#]
            ]&
            ,
            setup["FieldSpace"]["Grassmann"]
        ];

GetAntiFermions[setup_] :=
    GetAntiFermions[setup] =
        Select[
            Map[
                If[Head[#] === List,
                    Head[#[[1]]]
                    ,
                    {}
                ]&
                ,
                setup["FieldSpace"]["Grassmann"]
            ]
            ,
            # =!= {}&
        ];

GetCommuting[setup_] :=
    GetCommuting[setup] =
        Flatten @
            Select[
                Map[
                    If[Head[#] === List,
                        {Head[#[[1]]], Head[#[[2]]]}
                        ,
                        Head[#]
                    ]&
                    ,
                    setup["FieldSpace"]["Commuting"]
                ]
                ,
                # =!= {}&
            ];

GetAntiCommuting[setup_] :=
    GetAntiCommuting[setup] =
        Flatten @
            Select[
                Map[
                    If[Head[#] === List,
                        {Head[#[[1]]], Head[#[[2]]]}
                        ,
                        Head[#]
                    ]&
                    ,
                    setup["FieldSpace"]["Grassmann"]
                ]
                ,
                # =!= {}&
            ];

FieldNameQ[setup_, name_Symbol] :=
    FieldNameQ[setup, name] = MemberQ[Join[GetCommuting[setup], GetAntiCommuting[setup]], name];

(* ::Input::Initialization:: *)

GetFieldPairs[setup_] :=
    GetFieldPairs[setup] = Map[{Head[#[[1]]], Head[#[[2]]]}&, Select[Join[setup["FieldSpace"]["Grassmann"], setup["FieldSpace"]["Commuting"]], Head[#] === List&]];

GetSingleFields[setup_] :=
    GetSingleFields[setup] = Map[Head[#]&, Select[Join[setup["FieldSpace"]["Grassmann"], setup["FieldSpace"]["Commuting"]], Head[#] =!= List&]];

GetAllFields[setup_] :=
    GetAllFields[setup] = Join[Flatten @ GetFieldPairs[setup], GetSingleFields[setup]];

HasPartnerField[setup_, field_] :=
    HasPartnerField[setup, field] = MemberQ[Flatten @ GetFieldPairs[setup], field];

HasPartnerField[setup_, field_[__]] :=
    HasPartnerField[setup, field];

IsFermion[setup_, field_] :=
    IsFermion[setup, field] = MemberQ[GetFermions[setup], field];

IsFermion[setup_, field_[__]] :=
    IsFermion[setup, field];

IsAntiFermion[setup_, field_] :=
    IsAntiFermion[setup, field] = MemberQ[GetAntiFermions[setup], field];

IsAntiFermion[setup_, field_[__]] :=
    IsAntiFermion[setup, field];

IscField[setup_, field_] :=
    IscField[setup, field] = MemberQ[GetcFields[setup], field];

IscField[setup_, field_[__]] :=
    IscField[setup, field];

IsAnticField[setup_, field_] :=
    IsAnticField[setup, field] = MemberQ[GetAnticFields[setup], field];

IsAnticField[setup_, field_[__]] :=
    IsAnticField[setup, field];

IsGrassmann[setup_, field_] :=
    IsGrassmann[setup, field] = IsFermion[setup, field] || IsAntiFermion[setup, field];

(* ::Input::Initialization:: *)

GetPartnerField[setup_, field_Symbol] :=
    GetPartnerField[setup, field] =
        Module[{pairs, sel},
            If[Not @ HasPartnerField[setup, field],
                Return[field]
            ];
            pairs = GetFieldPairs[setup];
            sel = Select[pairs, MemberQ[#, field, Infinity]&][[1]];
            sel = DeleteCases[sel, field];
            If[Length[sel] > 0,
                Return[sel[[1]]]
            ];
            Print["field ", field, " not found!"];
            Abort[];
        ];

GetPartnerField[setup_, field_Symbol[i__]] :=
    GetPartnerField[setup, field][i]

(* ::Subsubsection::Closed:: *)

(*Checking field content of expressions*)

(* ::Input::Initialization:: *)

ExtractFields[setup_Association, expr_] :=
    Module[{},
        Return @ (DeleteDuplicates[Head /@ Cases[{expr}, Alternatives @@ Map[Blank, GetAllFields[setup]], Infinity]]);
    ];

ExtractFieldsWithIndex[setup_Association, expr_] :=
    Module[{},
        Return @ Cases[{expr}, Alternatives @@ Map[Blank, GetAllFields[setup]], Infinity];
    ];

(* ::Input::Initialization:: *)

ContainsGrassmann[setup_Association, expr_] :=
    Module[{},
        Return @ AnyTrue[ExtractFields[setup, expr], IsFermion[setup, #] || IsAntiFermion[setup, #]&];
    ]

GrassmannCount[setup_Association, expr_] :=
    Module[{},
        Return[Length @ Select[ExtractFieldsWithIndex[setup, expr], IsFermion[setup, Head[#]] || IsAntiFermion[setup, Head[#]]&]];
    ]

(* ::Subsubsection::Closed:: *)

(*Indices*)

(* ::Input::Initialization:: *)

(*Get a list of all unique super-indices within the expression expr*)

GetAllSuperIndices[setup_, expr_FTerm] :=
    Module[{idxO, idxF},
        idxO = Cases[expr, Alternatives @@ (Map[Blank[#]&, $indexedObjects]), {1, 2}];
        idxF = Cases[expr, Alternatives @@ (Map[Blank[#]&, GetAllFields[setup]]), {1, 2}];
        Return[makePosIdx /@ (idxF[[All, 1]] \[Union] Join @@ idxO[[All, 2]]) // DeleteDuplicates]
    ];

GetAllSuperIndices[setup_Association, expr_FEx] :=
    Module[{},
        Return @ (GetAllSuperIndices[setup, #]& /@ (List @@ expr))
    ];

(* ::Input::Initialization:: *)

ExtractObjectsWithIndex[setup_Association, expr_FTerm] :=
    Module[{},
        Return @ Cases[expr, Alternatives @@ (Map[Blank[#]&, {AnyField} \[Union] $indexedObjects \[Union] GetAllFields[setup]]), {1, 2}];
    ];

ExtractObjectsWithIndex[setup_Association, expr_FEx] :=
    Module[{},
        Return @ ((ExtractObjectsWithIndex[setup, #]& /@ (List @@ expr)))
    ];

(* ::Input::Initialization:: *)

ExtractObjectsAndIndices[setup_, expr_FTerm] :=
    Module[{idxO, idxF},
        idxO = Cases[expr, Alternatives @@ (Map[Blank[#]&, $indexedObjects]), {1, 2}];
        idxF = Cases[expr, Alternatives @@ (Map[Blank[#]&, Join[GetAllFields[setup], {AnyField}]]), {1, 2}];
        Return[{Join[idxO, idxF], makePosIdx /@ Join[idxF[[All, 1]], Join @@ idxO[[All, 2]]] // DeleteDuplicates}]
    ];

ExtractObjectsAndIndices[setup_Association, expr_FEx] :=
    Module[{},
        Return @ DeleteDuplicates @ ({Flatten[#[[All, 1]]], Join @@ #[[All, 2]]}& @ (ExtractObjectsAndIndices[setup, #]& /@ (List @@ expr)))
    ];

(* ::Input::Initialization:: *)

SuperIndices::undeterminedSums = "There are indices with count > 2 in the expression
    `1`
This is not allowed for valid terms/equation. Problematic indices:
    `2`";

(* ::Input::Initialization:: *)

(*Get a list of all closed super-indices within the expression expr*)

GetClosedSuperIndices[setup_, expr_] :=
    Module[{objects, indices, count},
        {objects, indices} = ExtractObjectsAndIndices[setup, expr];
        indices = Select[indices, Head[#] =!= List&];
        count = Map[Count[objects, #, {1, 5}]&, indices];
        Return[Pick[indices, Map[Mod[#, 2] === 0&, count]]];
    ];

(* ::Input::Initialization:: *)

(*Get a list of all open super-indices within the expression expr*)

GetOpenSuperIndices[setup_, expr_] :=
    Module[{objects, indices, count},
        {objects, indices} = ExtractObjectsAndIndices[setup, expr];
        indices = Select[indices, Head[#] =!= List&];
        count = Map[Count[objects, #, Infinity]&, indices];
        Return[Pick[indices, Map[Mod[#, 2] =!= 0&, count]]];
    ];

(* ::Input::Initialization:: *)

(*Check whether all indices are closed within expr. 
This disallows also multiple use of a single index name, !anywhere!*)

AllSuperIndicesClosed[setup_, expr_FTerm] :=
    Module[{objects, indices, count},
        {objects, indices} = ExtractObjectsAndIndices[setup, expr];
        count = Map[Count[objects, #, Infinity]&, indices];
        Return[AllTrue[count, # == 2&]];
    ];

AllSuperIndicesClosed[setup_, expr_FEx] :=
    And @@ (AllSuperIndicesClosed[setup, #]& /@ expr)

AllSuperIndicesClosed[setup_, expr_] :=
    (
        Message[type::error];
        Abort[]
    )

SuperIndicesValid[setup_, expr_FTerm] :=
    Module[{objects, indices, count},
        {objects, indices} = ExtractObjectsAndIndices[setup, expr];
        indices = Select[indices, Head[#] =!= List&];
        count = Map[Count[objects, #, Infinity]&, indices];
        If[AnyTrue[count, # > 2&],
            Message[SuperIndices::undeterminedSums, expr, Pick[indices, # > 2& /@ count]];
            Return[False]
        ];
        Return[True];
    ];

SuperIndicesValid[setup_, expr_FEx] :=
    SuperIndicesValid[setup, #]& /@ expr

SuperIndicesValid[setup_, expr_] :=
    (
        Message[type::error];
        Abort[]
    )

SetSymmetricObject[obj_, {f__}] :=
    Module[{},
        Unprotect[obj];
        obj[{f}, {any__}] /; Not @ OrderedQ[{any}] := obj[{f}, Sort @ {any}];
        Protect[obj];
    ];

SetSymmetricObject[obj_, {f__}, {i__Integer}] :=
    Module[{},
        Unprotect[obj];
        obj[{f}, {any__}] /; Not @ OrderedQ[{any}[[{i}]]] :=
            Module[{new = {any}},
                new[[{i}]] = Sort @ new[[{i}]];
                obj[{f}, new]
            ];
        Protect[obj];
    ];

(* Expanding / Shortening between Field[{f}, {i...}] and f[i...] *)

replFields[setup_] :=
    replFields[setup] =
        Dispatch @
            Module[{allFields},
                allFields = Join[GetAllFields[setup], {AnyField}];
                Join[Thread[(#[a_]& /@ allFields) :> Evaluate[(Field[{#}, {a}]& /@ allFields)]], Thread[(#[a_, b_List]& /@ allFields) :> Evaluate[(Field[{#}, {{a, b}}]& /@ allFields)]]]
            ];

unreplFields[setup_] :=
    unreplFields[setup] = Dispatch @ Thread[(Field[{#}, {a_}]& /@ GetAllFields[setup]) :> Evaluate[(#[a]& /@ GetAllFields[setup])]]
