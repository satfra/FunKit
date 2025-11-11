(* Check if a given field definition is valid. Can be either its own anti-field or a pair {af,f} *)

FieldDefQ[expr_] :=
    Module[{},
        If[Head[expr] === List,
            If[Length[expr] =!= 2,
                Print["A field definition must be either of form f[x...] or {af[x...],f[x...]}. \"", expr, "\" does not fit."];
                Return[False]
            ];
            If[Head[expr[[1]]] === Head[expr[[2]]],
                Print["A field definition {af[x...],f[x...]} must have different field names af and f. \"", expr, "\" does not fit."];
                Return[False]
            ];
            If[Not @ (List @@ (expr[[1]]) === List @@ (expr[[2]])),
                Print["A field definition {af[x...],f[x...]} must have identical indices. \"", expr, "\" does not fit."];
                Return[False]
            ];
            Do[
                If[Not @ (MatchQ[expr[[i]], _Symbol[_, {__Symbol}]] || MatchQ[expr[[i]], _Symbol[_]]),
                    Print["A field definition f[x...] must have indices f[p] or f[p,{a,b,...}]. \"", expr[[i]], "\" does not fit."];
                    Return[False]
                ]
                ,
                {i, 1, 2}
            ];
            Return[True];
        ];
        If[Not @ (MatchQ[expr, _Symbol[_, {__Symbol}]] || MatchQ[expr, _Symbol[_]]),
            Print["A field definition f[x...] must have indices f[p] or f[p,{a,b,...}]. \"", expr, "\" does not fit."];
            Return[False]
        ];
        Return[True];
    ];

FieldDef::invalidFieldDefinition = "The given field definition `1` is not valid.";

AssertFieldDef[expr_] :=
    If[Not @ FieldDefQ[expr],
        Message[FieldDefinition::invalidFieldDefinition];
        Abort[]
    ];

(* ::Input::Initialization:: *)

(* Check if a given field space definition is valid *)

FieldSpaceDefQ[fieldSpace_] :=
    Module[{},
        If[Head[fieldSpace] =!= Association,
            Print["An FSetup must be an association"];
            Return[False]
        ];
        If[Not @ (Keys[fieldSpace] === {"Commuting", "Grassmann"}),
            Print["fields must contain the two keys {\"Commuting\",\"Grassmann\"}!"];
            Return[False]
        ];
        If[Not @ ListQ[fieldSpace["Commuting"]],
            Print["fields[\"Commuting\"] must be a list!"];
            Return[False]
        ];
        If[Not @ (And @@ Map[FieldDefQ, fieldSpace["Commuting"]]),
            Print["fields[\"Commuting\"] must contain valid fields!"];
            Return[False]
        ];
        If[Not @ ListQ[fieldSpace["Grassmann"]],
            Print["fields[\"Grassmann\"] must be a list!"];
            Return[False]
        ];
        If[Not @ (And @@ Map[FieldDefQ, fieldSpace["Grassmann"]]),
            Print["fields[\"Grassmann\"] must contain valid fields!"];
            Return[False]
        ];
        Return[True];
    ];

FieldSpaceDefinition::invalidFieldDefinition = "The given field space definition is invalid.";

AssertFieldSpaceDef[fields_] :=
    If[Not @ FieldSpaceDefQ[fields],
        Message[FieldSpaceDefinition::invalidFieldDefinition];
        Abort[]
    ];

(* ::Subsubsection::Closed:: *)

(*FSetup*)

(* ::Input::Initialization:: *)

FSetup::notFSetup = "The given setup is not valid!";

FSetupQ[setup_] :=
    Module[{},
        If[Not @ (Head[setup] === Association),
            Print["A valid setup must be an Association!"];
            Return[False]
        ];
        If[Not @ MemberQ[Keys[setup], "FieldSpace"],
            Print["A valid setup must have the key \"FieldSpace\"!"];
            Return[False]
        ];
        If[Not @ FieldSpaceDefQ[setup["FieldSpace"]],
            Return[False]
        ];
        Return[True];
    ];

AssertFSetup[setup_] :=
    Module[{},
        If[Not @ (Head[setup] === Association),
            Print["A valid setup must be an Association!"];
            Message[FSetup::notFSetup];
            Abort[]
        ];
        If[Not @ MemberQ[Keys[setup], "FieldSpace"],
            Print["A valid setup must have the key \"FieldSpace\"!"];
            Message[FSetup::notFSetup];
            Abort[]
        ];
        AssertFieldSpaceDef[setup["FieldSpace"]];
    ];

(* ::Subsubsection::Closed:: *)

(*Existence of a field*)

(* ::Input::Initialization:: *)

(* Check if a given field definition is valid. Can be either its own anti-field or a pair {af,f} *)

FieldQ[setup_, expr_] :=
    FieldQ[setup, expr] =
        Module[{},
            If[Not @ MatchQ[expr, _Symbol[_Symbol]] && Not @ MatchQ[expr, _Symbol[-_Symbol]] && Not @ MatchQ[expr, _Symbol[_List]],
                Print["A field f must have a single super-index f[i]. \"", expr, "\" does not fit."];
                Return[False]
            ];
            If[Not @
                (
                    MemberQ[
                        Map[
                            Head
                            ,
                            setup["FieldSpace"] //
                            Values //
                            Flatten
                        ]
                        ,
                        Head[expr]
                    ]
                ),
                Print["The field \"", expr, "\" is not present in the given field space."];
                Return[False]
            ];
            Return[True];
        ];

Field::invalidField = "The given field `1` does not exist.";

AssertField[setup_, expr_] :=
    If[Not @ FieldQ[setup, expr],
        Message[FieldDefinition::invalidField];
        Abort[]
    ];

(*Derivative List*)

(*Check a derivative list for correct formatting.*)

DerivativeListQ[setup_, derivativeList_] :=
    Module[{},
        If[Not @ (Head[derivativeList] === List),
            Print["A valid derivativeList must be an List!"];
            Return[False]
        ];
        If[Not @ AllTrue[derivativeList, FieldQ[setup, #]&],
            Print["A valid derivativeList must be an List of fields f_[p_,{___}] of f_[p_] which have been defined in the setup!"];
            Return[False]
        ];
        Return[True];
    ];

DeriveEquation::invalidDerivativeList = "The given derivativeList `1` is not valid.";

AssertDerivativeList[setup_, expr_] :=
    If[Not @ DerivativeListQ[setup, expr],
        Message[DeriveEquation::invalidDerivativeList];
        Abort[]
    ];

(* ::Subsubsection::Closed:: *) 