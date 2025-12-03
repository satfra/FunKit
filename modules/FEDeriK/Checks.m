(* Check if a given field definition is valid. Can be either its own anti-field or a pair {af,f} *)

FieldDefQ::lengthMismatch = "A field definition must be either of form f[x...] or {af[x...],f[x...]}. \"`1`\" does not fit.";

FieldDefQ::sameHead = "A field definition {af[x...],f[x...]} must have different field names af and f. \"`1`\" does not fit.";

FieldDefQ::indexMismatch = "A field definition {af[x...],f[x...]} must have identical indices. \"`1`\" does not fit.";

FieldDefQ::invalidFieldForm = "A field definition f[x...] must have indices f[p] or f[p,{a,b,...}]. \"`1`\" does not fit.";

FieldDefQ[expr_] :=
    Module[{},
        If[Head[expr] === List,
            If[Length[expr] =!= 2,
                Message[FieldDefQ::lengthMismatch, expr];
                Return[False];
            ];
            If[Head[expr[[1]]] === Head[expr[[2]]],
                Message[FieldDefQ::sameHead, expr];
                Return[False]
            ];
            If[Not @ (List @@ (expr[[1]]) === List @@ (expr[[2]])),
                Message[FieldDefQ::indexMismatch, expr];
                Return[False]
            ];
            Do[
                If[Not @ (MatchQ[expr[[i]], _Symbol[_, {__Symbol}]] || MatchQ[expr[[i]], _Symbol[_]]),
                    Message[FieldDefQ::invalidFieldForm, expr[[i]]];
                    Return[False]
                ]
                ,
                {i, 1, 2}
            ];
            Return[True];
        ];
        If[Not @ (MatchQ[expr, _Symbol[_, {__Symbol}]] || MatchQ[expr, _Symbol[_]]),
            Message[FieldDefQ::invalidFieldForm, expr];
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

FieldSpaceDefQ::association = "The field space definition must be an Association.";

FieldSpaceDefQ::keys = "The field space definition must contain the keys {\"Commuting\",\"Grassmann\"}.";

FieldSpaceDefQ::list = "The entries of the field space definition must be lists.";

FieldSpaceDefQ::fields = "The entries of the field space definition must contain valid field definitions.";

FieldSpaceDefQ[fieldSpace_] :=
    Module[{},
        If[Head[fieldSpace] =!= Association,
            Message[FieldSpaceDefQ::association];
            Return[False]
        ];
        If[Not @ (Keys[fieldSpace] === {"Commuting", "Grassmann"}),
            Message[FieldSpaceDefQ::keys];
            Return[False]
        ];
        If[Not @ ListQ[fieldSpace["Commuting"]],
            Message[FieldSpaceDefQ::list];
            Return[False]
        ];
        If[Not @ (And @@ Map[FieldDefQ, fieldSpace["Commuting"]]),
            Message[FieldSpaceDefQ::fields];
            Return[False]
        ];
        If[Not @ ListQ[fieldSpace["Grassmann"]],
            Message[FieldSpaceDefQ::list];
            Return[False]
        ];
        If[Not @ (And @@ Map[FieldDefQ, fieldSpace["Grassmann"]]),
            Message[FieldSpaceDefQ::fields];
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

(*FSetup*)

FSetupQ::association = "A valid setup must be an Association.";

FSetupQ::fieldSpaceKey = "A valid setup must have the key \"FieldSpace\".";

FSetupQ[setup_] :=
    Module[{},
        If[Not @ (Head[setup] === Association),
            Message[FSetupQ::association];
            Return[False]
        ];
        If[Not @ MemberQ[Keys[setup], "FieldSpace"],
            Message[FSetupQ::fieldSpaceKey];
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
            Message[FSetupQ::association];
            Abort[]
        ];
        If[Not @ MemberQ[Keys[setup], "FieldSpace"],
            Message[FSetupQ::fieldSpaceKey];
            Abort[]
        ];
        AssertFieldSpaceDef[setup["FieldSpace"]];
    ];

(*Existence of a field*)

(* Check if a given field definition is valid. Can be either its own anti-field or a pair {af,f} *)

FieldQ::invalidFieldForm = "A field must have the form f[i] or f[-i]. \"`1`\" does not fit.";

FieldQ::unknownField = "The field \"`1`\" is not defined in the given field space.";

FieldQ[setup_, expr_] :=
    FieldQ[setup, expr] =
        Module[{},
            If[Not @ MatchQ[expr, _Symbol[_Symbol]] && Not @ MatchQ[expr, _Symbol[-_Symbol]] && Not @ MatchQ[expr, _Symbol[_List]],
                Message[FieldQ::invalidFieldForm, expr];
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
                Message[FieldQ::unknownField, expr];
                Return[False]
            ];
            Return[True];
        ];

FieldQ::invalidField = "The given field `1` does not exist.";

AssertField[setup_, expr_] :=
    If[Not @ FieldQ[setup, expr],
        Message[FieldQ::invalidField, expr];
        Abort[]
    ];

(*Derivative List*)

(*Check a derivative list for correct formatting.*)

DerivativeListQ::notList = "A derivative list must be a List.";

DerivativeListQ::invalidField = "All entries of a derivative list must be valid fields defined in the setup.";

DerivativeListQ[setup_, derivativeList_] :=
    Module[{},
        If[Not @ (Head[derivativeList] === List),
            Message[DerivativeListQ::notList];
            Return[False]
        ];
        If[Not @ AllTrue[derivativeList, Quiet[FieldQ[setup, #]]&],
            If[AllTrue[derivativeList, Head[#] === AnyField || FieldQ[setup, #]&],
                Message[FunKit::warning, "The derivative list contains AnyField entries."];
                ,
                Message[DerivativeListQ::invalidField];
                Return[False]
            ]
        ];
        Return[True];
    ];

DeriveEquation::invalidDerivativeList = "The given derivativeList `1` is not valid.";

AssertDerivativeList[setup_, expr_] :=
    If[Not @ DerivativeListQ[setup, expr],
        Message[DeriveEquation::invalidDerivativeList, expr];
        Abort[]
    ];
