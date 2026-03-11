(**********************************************************************************
    Getting lists of fields from setup
**********************************************************************************)

GetCommutingFields[setup_] :=
    GetCommutingFields[setup] =
        Map[
            If[Head[#] === List,
                Head[#[[2]]]
                ,
                Head[#]
            ]&
            ,
            Lookup[setup["FieldSpace"], "Commuting", {}]
        ];

GetAntiCommutingFields[setup_] :=
    GetAntiCommutingFields[setup] =
        Select[
            Map[
                If[Head[#] === List,
                    Head[#[[1]]]
                    ,
                    {}
                ]&
                ,
                Lookup[setup["FieldSpace"], "Commuting", {}]
            ]
            ,
            # =!= {}&
        ];

GetGrassmannFields[setup_] :=
    GetGrassmannFields[setup] =
        Map[
            If[Head[#] === List,
                Head[#[[2]]]
                ,
                Head[#]
            ]&
            ,
            Lookup[setup["FieldSpace"], "Grassmann", {}]
        ];

GetAntiGrassmannFields[setup_] :=
    GetAntiGrassmannFields[setup] =
        Select[
            Map[
                If[Head[#] === List,
                    Head[#[[1]]]
                    ,
                    {}
                ]&
                ,
                Lookup[setup["FieldSpace"], "Grassmann", {}]
            ]
            ,
            # =!= {}&
        ];

GetCommuting[setup_] :=
    GetCommuting[setup] =
        Join[
            Flatten @
                Select[
                    Map[
                        If[Head[#] === List,
                            {Head[#[[1]]], Head[#[[2]]]}
                            ,
                            Head[#]
                        ]&
                        ,
                        Lookup[setup["FieldSpace"], "Commuting", {}]
                    ]
                    ,
                    # =!= {}&
                ]
            ,
            GetCSourceFields[setup]
        ];

GetGrassmann[setup_] :=
    GetGrassmann[setup] =
        Join[
            Flatten @
                Select[
                    Map[
                        If[Head[#] === List,
                            {Head[#[[1]]], Head[#[[2]]]}
                            ,
                            Head[#]
                        ]&
                        ,
                        Lookup[setup["FieldSpace"], "Grassmann", {}]
                    ]
                    ,
                    # =!= {}&
                ]
            ,
            GetGrassmannSourceFields[setup]
        ];

GetFieldPairs[setup_] :=
    GetFieldPairs[setup] = Map[{Head[#[[1]]], Head[#[[2]]]}&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] === List&]];

GetSingleFields[setup_] :=
    GetSingleFields[setup] = Join[
        Map[Head[#]&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] =!= List&]],
        GetAllSourceFields[setup]
    ];

GetAllFields[setup_] :=
    GetAllFields[setup] = Join[Flatten @ GetFieldPairs[setup], Map[Head[#]&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] =!= List&]], GetAllSourceFields[setup]];

(**********************************************************************************
    Source field accessors
**********************************************************************************)

GetCSourceFields[setup_] :=
    GetCSourceFields[setup] =
        Map[Head, Lookup[setup["FieldSpace"], "CommutingSource", {}]];

GetGrassmannSourceFields[setup_] :=
    GetGrassmannSourceFields[setup] =
        Map[Head, Lookup[setup["FieldSpace"], "GrassmannSource", {}]];

GetAllSourceFields[setup_] :=
    GetAllSourceFields[setup] =
        Join[GetCSourceFields[setup], GetGrassmannSourceFields[setup]];

GetNonSourceFields[setup_] :=
    GetNonSourceFields[setup] =
        Join[Flatten @ GetFieldPairs[setup], Map[Head[#]&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] =!= List&]]];

(**********************************************************************************
    Getting single field properties
**********************************************************************************)

FieldNameQ[setup_, name_Symbol] :=
    FieldNameQ[setup, name] = MemberQ[Join[GetCommuting[setup], GetGrassmann[setup]], name];

HasPartnerField[setup_, field_] :=
    HasPartnerField[setup, field] = MemberQ[Flatten @ GetFieldPairs[setup], field];

HasPartnerField[setup_, field_[__]] :=
    HasPartnerField[setup, field];

IsGrassmannField[setup_, field_] :=
    IsGrassmannField[setup, field] = MemberQ[GetGrassmannFields[setup], field];

IsGrassmannField[setup_, field_[__]] :=
    IsGrassmannField[setup, field];

IsAntiGrassmannField[setup_, field_] :=
    IsAntiGrassmannField[setup, field] = MemberQ[GetAntiGrassmannFields[setup], field];

IsAntiGrassmannField[setup_, field_[__]] :=
    IsAntiGrassmannField[setup, field];

IsCommutingField[setup_, field_] :=
    IsCommutingField[setup, field] = MemberQ[GetCommutingFields[setup], field];

IsCommutingField[setup_, field_[__]] :=
    IsCommutingField[setup, field];

IsAntiCommutingField[setup_, field_] :=
    IsAntiCommutingField[setup, field] = MemberQ[GetAntiCommutingFields[setup], field];

IsAntiCommutingField[setup_, field_[__]] :=
    IsAntiCommutingField[setup, field];

IsGrassmann[setup_, field_] :=
    IsGrassmann[setup, field] = IsGrassmannField[setup, field] || IsAntiGrassmannField[setup, field] || IsGrassmannSource[setup, field];

IsCommuting[setup_, field_] :=
    IsCommuting[setup, field] = IsCommutingField[setup, field] || IsAntiCommutingField[setup, field] || IsCSource[setup, field];

(**********************************************************************************
    Source field predicates
**********************************************************************************)

IsCSource[setup_, field_] :=
    IsCSource[setup, field] = MemberQ[GetCSourceFields[setup], field];

IsCSource[setup_, field_[__]] :=
    IsCSource[setup, field];

IsGrassmannSource[setup_, field_] :=
    IsGrassmannSource[setup, field] = MemberQ[GetGrassmannSourceFields[setup], field];

IsGrassmannSource[setup_, field_[__]] :=
    IsGrassmannSource[setup, field];

IsSource[setup_, field_] :=
    IsSource[setup, field] = IsCSource[setup, field] || IsGrassmannSource[setup, field];

IsSource[setup_, field_[__]] :=
    IsSource[setup, field];

(**********************************************************************************
    Getting partner fields
**********************************************************************************)

GetPartnerField::notFound = "The field `1` was not found in the field pairs of the given setup.";

GetPartnerField[setup_, field_Symbol] :=
    GetPartnerField[setup, field] =
        Module[{pairs, sel},
            If[Not @ HasPartnerField[setup, field],
                Return[field]
            ];
            pairs = GetFieldPairs[setup];
            sel = Select[pairs, MemberQ[#, field, Infinity]&];
            If[Length[sel] === 0,
                Message[GetPartnerField::notFound, field];
                Abort[];
            ];
            sel = DeleteCases[sel[[1]], field];
            If[Length[sel] > 0,
                Return[sel[[1]]]
            ];
            Message[GetPartnerField::notFound, field];
            Abort[];
        ];

GetPartnerField[setup_, field_Symbol[i__]] :=
    GetPartnerField[setup, field][i]

(**********************************************************************************
    Field extraction from expressions
**********************************************************************************)

ExtractFields[setup_Association, expr_] :=
    Module[{},
        Return @ (DeleteDuplicates[Head /@ Cases[{expr}, Alternatives @@ Map[Blank, GetAllFields[setup]], Infinity]]);
    ];

ExtractFieldsWithIndex[setup_Association, expr_] :=
    Module[{},
        Return @ Cases[{expr}, Alternatives @@ Map[Blank, GetAllFields[setup]], Infinity];
    ];

ContainsGrassmann[setup_Association, expr_] :=
    Module[{},
        Return @ AnyTrue[ExtractFields[setup, expr], IsGrassmann[setup, #]&];
    ]

GrassmannCount[setup_Association, expr_] :=
    Module[{},
        Return[Length @ Select[ExtractFieldsWithIndex[setup, expr], IsGrassmann[setup, Head[#]]&]];
    ]

(**********************************************************************************
    Index extraction from FTerms / FExs
**********************************************************************************)

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

(**********************************************************************************
    Getting indexed objects from FTerms / FExs
**********************************************************************************)

ExtractObjectsWithIndex[setup_Association, expr_FTerm] :=
    Module[{},
        Return @ Cases[expr, Alternatives @@ (Map[Blank[#]&, {AnyField} \[Union] $indexedObjects \[Union] GetAllFields[setup]]), {1, 2}];
    ];

ExtractObjectsWithIndex[setup_Association, expr_FEx] :=
    Module[{},
        Return @ ((ExtractObjectsWithIndex[setup, #]& /@ (List @@ expr)))
    ];

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

(**********************************************************************************
    Examining superindices
**********************************************************************************)

SuperIndices::undeterminedSums = "There are indices with count > 2 in the expression
    `1`
This is not allowed for valid terms/equation. Problematic indices:
    `2`";

(*Get a list of all closed super-indices within the expression expr*)

GetClosedSuperIndices[setup_, expr_] :=
    Module[{objects, indices, count},
        {objects, indices} = ExtractObjectsAndIndices[setup, expr];
        indices = Select[indices, Head[#] =!= List&];
        count = Map[Count[objects, #, {1, 5}]&, indices];
        Return[Pick[indices, Map[Mod[#, 2] === 0&, count]]];
    ];

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
    And @@ (AllSuperIndicesClosed[setup, #]& /@ (List @@ expr))

AllSuperIndicesClosed[setup_, expr_] :=
    (
        Message[type::error, expr];
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
        Message[type::error, expr];
        Abort[]
    )

FSetSymmetricObject::emptyFields = "The field list must not be empty. Use FSetSymmetricObject[obj, {field1, field2, ...}].";

FSetSymmetricObject[obj_, {f__}] :=
    Module[{},
        Unprotect[obj];
        obj[{f}, {any__}] /; Not @ OrderedQ[{any}] := obj[{f}, Sort @ {any}];
        Protect[obj];
    ];

FSetSymmetricObject[obj_, {f__}, {i__Integer}] :=
    Module[{},
        Unprotect[obj];
        obj[{f}, {any__}] /; Not @ OrderedQ[{any}[[{i}]]] :=
            Module[{new = {any}},
                new[[{i}]] = Sort @ new[[{i}]];
                obj[{f}, new]
            ];
        Protect[obj];
    ];

FSetSymmetricObject[_, {}] :=
    (Message[FSetSymmetricObject::emptyFields]; Abort[]);

(* Expanding / Shortening between Field[{f}, {i...}] and f[i...] *)

replFields[setup_] :=
    replFields[setup] =
        Dispatch @
            Module[{allFields},
                allFields = Join[GetAllFields[setup], {AnyField}];
                Join[Thread[(#[a_]& /@ allFields) :> Evaluate[(Field[{#}, {a}]& /@ allFields)]], Thread[(#[a_, b_List]& /@ allFields) :> Evaluate[(Field[{#}, {{a, b}}]& /@ allFields)]]]
            ];

unreplFields[setup_] :=
    Module[{allFields},
        allFields = Join[GetAllFields[setup], {AnyField}];
        unreplFields[setup] = Dispatch @ Thread[(Field[{#}, {a_}]& /@ allFields) :> Evaluate[(#[a]& /@ allFields)]]
    ];
