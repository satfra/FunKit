(**********************************************************************************
    IndexedObjects.m -- Field accessors, predicates, and indexed object utilities

    Public API (field accessors, all memoized):
      GetCommutingFields         -- Returns commuting field heads
      GetAntiCommutingFields     -- Returns anti-commuting (partner) field heads
      GetGrassmannFields         -- Returns Grassmann field heads
      GetAntiGrassmannFields     -- Returns anti-Grassmann field heads
      GetCommuting               -- Returns all commuting + commuting-source fields
      GetGrassmann               -- Returns all Grassmann + Grassmann-source fields
      GetFieldPairs              -- Returns {anti, field} pairs
      GetSingleFields            -- Returns unpaired fields + source fields
      GetAllFields               -- Returns all field heads in the setup
      GetCSourceFields           -- Returns commuting source field heads
      GetGrassmannSourceFields   -- Returns Grassmann source field heads
      GetAllSourceFields         -- Returns all source field heads
      GetNonSourceFields         -- Returns all non-source field heads

    Public API (field predicates, all memoized):
      FieldNameQ                 -- True if symbol is a known field name
      HasPartnerField            -- True if field has a conjugate partner
      IsGrassmannField           -- True if field is Grassmann (not anti)
      IsAntiGrassmannField       -- True if field is anti-Grassmann
      IsCommutingField           -- True if field is commuting (not anti)
      IsAntiCommutingField       -- True if field is anti-commuting
      IsGrassmann                -- True if field has Grassmann statistics
      IsCommuting                -- True if field has commuting statistics
      IsCSource                  -- True if field is a commuting source
      IsGrassmannSource          -- True if field is a Grassmann source
      IsSource                   -- True if field is any source field
      GetPartnerField            -- Returns conjugate partner (or self if unpaired)

    Public API (indexed object utilities):
      ExtractFields              -- Returns deduplicated field heads in expression
      ExtractFieldsWithIndex     -- Returns field-with-index instances
      ContainsGrassmann          -- True if expression contains Grassmann fields
      GrassmannCount             -- Counts Grassmann field applications
      GetAllSuperIndices         -- Extracts all unique super-indices
      ExtractObjectsWithIndex    -- Returns indexed objects and fields from expression
      ExtractObjectsAndIndices   -- Returns {objects, indices} pair
      GetClosedSuperIndices      -- Returns contracted (even-count) indices
      GetOpenSuperIndices        -- Returns free (odd-count) indices
      AllSuperIndicesClosed      -- True if all indices are contracted
      SuperIndicesValid          -- True if no index appears more than twice
      FSetSymmetricObject        -- Installs symmetry rules on an object head

    Internal:
      replFields                 -- Builds field[idx] -> Field[{field},{idx}] dispatch
                                    (used broadly: Truncation, Routing, Derivatives, etc.)
      unreplFields               -- Builds reverse dispatch Field -> field[idx]
                                    (used by Truncation, Routing, etc.)
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
    GetSingleFields[setup] = Join[Map[Head[#]&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] =!= List&]], GetAllSourceFields[setup]];

GetAllFields[setup_] :=
    GetAllFields[setup] = Join[Flatten @ GetFieldPairs[setup], Map[Head[#]&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] =!= List&]], GetAllSourceFields[setup]];

(**********************************************************************************
    Source field accessors
**********************************************************************************)

GetCSourceFields[setup_] :=
    GetCSourceFields[setup] = Map[Head, Lookup[setup["FieldSpace"], "CommutingSource", {}]];

GetGrassmannSourceFields[setup_] :=
    GetGrassmannSourceFields[setup] = Map[Head, Lookup[setup["FieldSpace"], "GrassmannSource", {}]];

GetAllSourceFields[setup_] :=
    GetAllSourceFields[setup] = Join[GetCSourceFields[setup], GetGrassmannSourceFields[setup]];

GetNonSourceFields[setup_] :=
    GetNonSourceFields[setup] = Join[Flatten @ GetFieldPairs[setup], Map[Head[#]&, Select[Join[Lookup[setup["FieldSpace"], "Grassmann", {}], Lookup[setup["FieldSpace"], "Commuting", {}]], Head[#] =!= List&]]];

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
    Module[
        {masked}
        ,
        (*Mask indexed objects — same rationale as ExtractFieldsWithIndex*)
        masked = expr /. (obj_?indexedObjectQ :> Null);
        Return @ (DeleteDuplicates[Head /@ Cases[{masked}, Alternatives @@ Map[Blank, GetAllFields[setup]], Infinity]]);
    ];

ExtractFieldsWithIndex[setup_Association, expr_] :=
    Module[
        {masked}
        ,
(*Mask indexed objects to avoid counting field[index] arguments embedded inside
  them. In NotationB, e.g. S[A[-i1], cb[-i2], c[-i3]], the ghost fields would
  otherwise be found by Cases at Infinity depth.*)
        masked = expr /. (obj_?indexedObjectQ :> Null);
        Return @ Cases[{masked}, Alternatives @@ Map[Blank, GetAllFields[setup]], Infinity];
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
        idxF = Cases[expr, Alternatives @@ (Map[Blank[#]&, GetAllFields[setup]]), {1}];
        Return[makePosIdx /@ (idxF[[All, 1]] \[Union] Join @@ (getIndices /@ idxO)) // DeleteDuplicates]
    ];

GetAllSuperIndices[setup_Association, expr_FEx] :=
    Module[{},
        Return @ (GetAllSuperIndices[setup, #]& /@ (List @@ expr))
    ];

(**********************************************************************************
    Getting indexed objects from FTerms / FExs
**********************************************************************************)

(* Cached Alternatives patterns for object/field extraction — rebuilt when $indexedObjects changes *)

$objFieldAlt[setup_] :=
    $objFieldAlt[setup] = Alternatives @@ Map[Blank[#]&, Join[$indexedObjects, GetAllFields[setup], {AnyField}]];

$objAlt :=
    $objAlt = Alternatives @@ Map[Blank[#]&, $indexedObjects];

$fieldAlt[setup_] :=
    $fieldAlt[setup] = Alternatives @@ Map[Blank[#]&, Join[GetAllFields[setup], {AnyField}]];

ExtractObjectsWithIndex[setup_Association, expr_FTerm] :=
    Module[{iObjs, all, depth1Fields, masked},
        iObjs = $indexedObjects;
(*Mask FMinus and SymmetryFactor before searching: in NotationB their field[index]
  arguments at depth 2 would otherwise be found and mistakenly kept.*)
        masked = expr /. {(h : FMinus | SymmetryFactor)[__] :> h};
        all = Cases[masked, $objFieldAlt[setup], {1, 2}];
        depth1Fields = Cases[masked, $fieldAlt[setup], {1}];
        Return @ Select[all, MemberQ[iObjs, Head[#]] || MemberQ[depth1Fields, #]&];
    ];

ExtractObjectsWithIndex[setup_Association, expr_FEx] :=
    Module[{},
        Return @ ((ExtractObjectsWithIndex[setup, #]& /@ (List @@ expr)))
    ];

ExtractObjectsAndIndices[setup_, expr_FTerm] :=
    Module[
        {idxO, idxF, masked}
        ,
        (*Mask FMinus/SymmetryFactor — same rationale as ExtractObjectsWithIndex.*)
        masked = expr /. {(h : FMinus | SymmetryFactor)[__] :> h};
        idxO = Cases[masked, $objAlt, {1, 2}];
        idxF = Cases[masked, $fieldAlt[setup], {1}];
        Return[{Join[idxO, idxF], makePosIdx /@ Join[idxF[[All, 1]], Join @@ (getIndices /@ idxO)] // DeleteDuplicates}]
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
    (
        Message[FSetSymmetricObject::emptyFields];
        Abort[]
    );

(* Expanding / Shortening between Field[{f}, {i...}] and f[i...] *)

replFieldsDispatch[setup_] :=
    replFieldsDispatch[setup] =
        Dispatch @
            Module[{allFields, repl},
                allFields = Join[GetAllFields[setup], {AnyField}];
                repl = Join[Thread[(#[a__]& /@ allFields) :> Evaluate[(makeObj[Field, {#}, {a}]& /@ allFields)]]];
                Select[repl, Not @ (((Head @ #[[1]])[None] /. #) === (Head @ #[[1]])[None])&]
            ];

replFields[setup_, expr_] :=
    With[{disp = replFieldsDispatch[setup]},
        If[Head[expr] === List,
            Map[
                If[indexedObjectQ[#],
                    #
                    ,
                    # /. disp
                ]&
                ,
                expr
            ]
            ,
            If[indexedObjectQ[expr],
                expr
                ,
                expr /. disp
            ]
        ]
    ];

unreplFieldsDispatch[setup_] :=
    unreplFieldsDispatch[setup] =
        Dispatch @
            Module[{allFields, repl},
                allFields = Join[GetAllFields[setup], {AnyField}];
                repl = Thread[(makeObj[Field, {#}, {a__}]& /@ allFields) :> Evaluate[(#[a]& /@ allFields)]];
                Select[repl, Head @ #[[1]] =!= Head @ #[[2]]&]
            ];

unreplFields[setup_, expr_] :=
    With[{disp = unreplFieldsDispatch[setup]},
        expr /. disp
    ];
