(**********************************************************************************
    Setting the canonical ordering, used throughout FunKit as a standard
**********************************************************************************)

$AvailableCanonicalOrderings = {"g>ag>c", "ag>g>c", "c>g>ag", "c>ag>g"};

CanonicalOrdering::unknownInteger = "The integer `1` should be between 1 and 4.";

CanonicalOrdering::unknownString = "The expression `1` should be one of " <> ToString[$AvailableCanonicalOrderings];

FSetCanonicalOrdering[a_Integer] :=
    Module[{},
        Switch[a,
            1,
                $CanonicalOrdering = "g>ag>c"
            ,
            2,
                $CanonicalOrdering = "ag>g>c"
            ,
            3,
                $CanonicalOrdering = "c>g>ag"
            ,
            4,
                $CanonicalOrdering = "c>ag>g"
            ,
            _,
                Message[CanonicalOrdering::unknownInteger, a];
                Abort[]
        ];
        Print["Canonical ordering set to ", $CanonicalOrdering];
    ];

FSetCanonicalOrdering[a_] :=
    Module[{},
        Switch[a,
            "g>ag>c",
                $CanonicalOrdering = "g>ag>c"
            ,
            "ag>g>c",
                $CanonicalOrdering = "ag>g>c"
            ,
            "c>g>ag",
                $CanonicalOrdering = "c>g>ag"
            ,
            "c>ag>g",
                $CanonicalOrdering = "c>ag>g"
            ,
            _,
                Message[CanonicalOrdering::unknownString, a];
                Abort[]
        ];
        Print["Canonical ordering set to ", $CanonicalOrdering];
    ];

(**********************************************************************************
    Ordering expressions
**********************************************************************************)

(*Returns true if f1 < f2, and false if f1 > f2*)

FieldOrderLess::unknownField = "The field `1` does not match any known field category (Grassmann, anti-Grassmann, commuting, anti-commuting, source, or AnyField) in the given setup.";

FieldOrderLess[setup_, f1_Symbol, f2_Symbol] :=
    FieldOrderLess[setup, f1, f2] =
        Module[{kind1, kind2, idxOrder, n1, n2, picked1, picked2},
            kind1 = {IsGrassmannField[setup, #], IsAntiGrassmannField[setup, #], IsCommutingField[setup, #], IsAntiCommutingField[setup, #], # === AnyField, IsGrassmannSource[setup, #], IsCSource[setup, #]}&[f1];
            kind2 = {IsGrassmannField[setup, #], IsAntiGrassmannField[setup, #], IsCommutingField[setup, #], IsAntiCommutingField[setup, #], # === AnyField, IsGrassmannSource[setup, #], IsCSource[setup, #]}&[f2];
            Switch[$CanonicalOrdering,
                "g>ag>c",
                    idxOrder = {4, 3, 2, 1, 0, -1, -2}
                ,
                "ag>g>c",
                    idxOrder = {3, 4, 1, 2, 0, -1, -2}
                ,
                "c>g>ag",
                    idxOrder = {2, 1, 4, 3, 0, -2, -1}
                ,
                "c>ag>g",
                    idxOrder = {1, 2, 3, 4, 0, -2, -1}
                ,
                _,
                    Message[CanonicalOrdering::unknownString, $CanonicalOrdering];
                    Abort[];
            ];
            picked1 = Pick[idxOrder, kind1];
            picked2 = Pick[idxOrder, kind2];
            If[Length[picked1] === 0,
                Message[FieldOrderLess::unknownField, f1];
                Abort[];
            ];
            If[Length[picked2] === 0,
                Message[FieldOrderLess::unknownField, f2];
                Abort[];
            ];
            n1 = picked1[[1]];
            n2 = picked2[[1]];
            If[n1 === n2,
                Return[OrderedQ[{f1, f2}]]
            ];
            Return[n1 < n2]
        ];

(*Returns the sign that results from exchanging the two fields f1 and f2*)

CommuteSign[setup_, f1_, f2_] :=
    CommuteSign[setup, f1, f2] =
        Module[{},
            Return[-2 * Boole[MemberQ[GetGrassmann[setup], f1] && MemberQ[GetGrassmann[setup], f2]] + 1];
        ];

(*Excluding indices in certain objects from being reordered*)

$unorderedIndices[_] = 0;

FSetUnorderedIndices::invalidArgs = "Object `1` must be a registered object (MemberQ[$allObjects]) and n (`2`) must be a non-negative integer.";

FSetUnorderedIndices[obj_, n_Integer] /; n >= 0 && MemberQ[$allObjects, obj] :=
    Set[$unorderedIndices[obj], n];

FSetUnorderedIndices[obj_, n_] :=
    (
        Message[FSetUnorderedIndices::invalidArgs, obj, n];
        Abort[]
    );

(* In case of a tie, we use lexical ordering: *)

indicesLess[i1_, i2_] :=
    Module[{},
        Return[Sort @ {i1, i2} === {i1, i2}];
    ]

(*Find all instances of $OrderedObjects and order their field value according to the canonical scheme*)

OrderObject[setup_, expr_] :=
    expr;

OrderObject[setup_, obj_[fields_List, indices_List] /; MemberQ[$OrderedObjects, obj]] :=
    Module[
        {i, curi, prefactor, pref, reverse, nfields = fields, nindices = indices}
        ,
        (*Do not order if there is an undetermined field!*)
        If[MemberQ[nfields, AnyField] || FreeQ[$indexedObjects, obj],
            Return[obj[nfields, nindices]]
        ];
        (*The propagator gets a reverse ordering*)
        reverse =
            If[obj === Propagator,
                True
                ,
                False
            ];
        pref =
            If[reverse,
                Identity
                ,
                Not
            ];
        prefactor = 1;
        (*Always compare the ith field with all previous fields and put it in the right place. Iterate until one reaches the end of the array, then it is sorted.*)
        For[i = 1, i <= Length[nfields] - $unorderedIndices[obj], i++,
            curi = i;
            (*Check if we should switch curi and curi-1*)
            While[
                curi >= 2 && (pref @ FieldOrderLess[setup, nfields[[curi]], nfields[[curi - 1]]] || (nfields[[curi]] === nfields[[curi - 1]] && pref @ indicesLess[nindices[[curi]], nindices[[curi - 1]]]))
                ,
                nfields[[{curi, curi - 1}]] = nfields[[{curi - 1, curi}]];
                nindices[[{curi, curi - 1}]] = nindices[[{curi - 1, curi}]];
                prefactor *= CommuteSign[setup, nfields[[curi]], nfields[[curi - 1]]];
                curi--;
            ];
        ];
        Return[prefactor * obj[nfields, nindices]];
    ];

GetOrder[setup_, fields_List, reverse_:False] /; BooleanQ[reverse] :=
    Module[{i, curi, prefactor, pref, nfields = fields, norder = Range[Length[fields]]},
        pref =
            If[reverse,
                Identity
                ,
                Not
            ];
        prefactor = 1;
        (*Always compare the ith field with all previous fields and put it in the right place. Iterate until one reaches the end of the array, then it is sorted.*)
        For[i = 2, i <= Length[nfields], i++,
            curi = i;
            (*Check if we should switch curi and curi-1*)
            While[
                curi >= 2 && pref @ FieldOrderLess[setup, nfields[[curi]], nfields[[curi - 1]]]
                ,
                nfields[[{curi, curi - 1}]] = nfields[[{curi - 1, curi}]];
                norder[[{curi, curi - 1}]] = norder[[{curi - 1, curi}]];
                prefactor *= CommuteSign[setup, nfields[[curi]], nfields[[curi - 1]]];
                curi--;
            ];
        ];
        Return[{prefactor, norder}];
    ];

OrderObject::cantOrder = "Cannot reorder the fields `1` in the order `2`";

GetOrder[setup_, fields_List, fieldOrder_List] :=
    Module[{nfields = fields, norder, prefactor = 1, i, pos, j, len},
        len = Length[fields];
        norder = Range[len];
        (*sanity check*)
        If[Sort[fields] =!= Sort[fieldOrder],
            Message[OrderObject::cantOrder, fields, fieldOrder];
            Abort[]
        ];
        For[i = 1, i <= len, i++,
            If[nfields[[i]] === fieldOrder[[i]],
                Continue[]
            ];
            pos = FirstPosition[nfields[[i ;; len]], fieldOrder[[i]]];
            If[pos === Missing["NotFound"],
                Message[OrderObject::cantOrder, fields, fieldOrder];
                Abort[]
            ];
            pos = pos[[1]] + i - 1;
            For[j = pos, j > i, j--,
                prefactor *= CommuteSign[setup, nfields[[j - 1]], nfields[[j]]];
                nfields[[{j - 1, j}]] = nfields[[{j, j - 1}]];
                norder[[{j - 1, j}]] = norder[[{j, j - 1}]];
            ];
        ];
        {prefactor, norder}
    ]

(* Order an object, e.g. GammaN, Propagator, ... *)

OrderObject[setup_, obj_[fields_List, indices_List] /; MemberQ[$OrderedObjects, obj], fieldOrder_List] :=
    Module[
        {i, curi, prefactor, pref, reverse, nfields = fields, nindices = indices}
        ,
        (*Do not order if there is an undetermined field!*)
        If[MemberQ[nfields, AnyField] || FreeQ[$indexedObjects, obj],
            Return[obj[nfields, nindices]]
        ];
        (*The propagator gets a reverse ordering*)
        reverse =
            If[obj === Propagator,
                True
                ,
                False
            ];
        pref =
            If[reverse,
                Identity
                ,
                Not
            ];
        prefactor = 1;
        (*Always compare the ith field with all previous fields and put it in the right place. Iterate until one reaches the end of the array, then it is sorted.*)
        i = 1;
        While[
            i <= Length[nfields] - $unorderedIndices[obj]
            ,
            curi = i;
            If[nfields[[curi]] === fieldOrder[[curi]],
                i++;
                Continue[]
            ];
            (*Check if we should switch curi and curi-1*)
            While[
                nfields[[curi]] =!= fieldOrder[[curi]]
                ,
                If[curi + 1 > Length[nfields] - $unorderedIndices[obj],
                    Message[OrderObject::cantOrder, fields, fieldOrder];
                    Abort[]
                ];
                nfields[[{curi, curi + 1}]] = nfields[[{curi + 1, curi}]];
                nindices[[{curi, curi + 1}]] = nindices[[{curi + 1, curi}]];
                prefactor *= CommuteSign[setup, nfields[[curi]], nfields[[curi + 1]]];
                curi++;
            ];
        ];
        Return[prefactor * obj[nfields, nindices]];
    ];

OrderFieldList[setup_, fields_List] :=
    Module[
        {i, curi, nfields = fields}
        ,
        (*Always compare the ith field with all previous fields and put it in the right place. Iterate until one reaches the end of the array, then it is sorted.*)
        For[i = 1, i <= Length[nfields], i++,
            curi = i;
            (*Check if we should switch curi and curi-1*)
            While[
                curi >= 2 && Not @ FieldOrderLess[setup, nfields[[curi]], nfields[[curi - 1]]]
                ,
                nfields[[{curi, curi - 1}]] = nfields[[{curi - 1, curi}]];
                curi--;
            ];
        ];
        Return[nfields];
    ];

(* Order everything *)

OrderFields[setup_, expr_] :=
    Map[OrderObject[setup, #]&, OrderObject[setup, expr], Infinity];

FOrderFields[setup_, expr_] :=
    (
        AssertFSetup[setup];
        OrderFields[setup, expr]
    );
