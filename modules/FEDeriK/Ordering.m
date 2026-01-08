(**********************************************************************************
    Setting the canonical ordering, used throughout FunKit as a standard
**********************************************************************************)

$AvailableCanonicalOrderings = {"f>af>b", "af>f>b", "b>f>af", "b>af>f"};

CanonicalOrdering::unknownInteger = "The integer `1` should be between 1 and 4.";

CanonicalOrdering::unknownString = "The expression `1` should be one of " <> ToString[$AvailableCanonicalOrderings];

SetCanonicalOrdering[a_Integer] :=
    Module[{},
        Switch[a,
            1,
                $CanonicalOrdering = "f>af>b"
            ,
            2,
                $CanonicalOrdering = "af>f>b"
            ,
            3,
                $CanonicalOrdering = "b>f>af"
            ,
            4,
                $CanonicalOrdering = "b>af>f"
            ,
            _,
                Message[CanonicalOrdering::unknownInteger, a]
        ];
        Print["Canonical ordering set to ", $CanonicalOrdering];
    ];

SetCanonicalOrdering[a_] :=
    Module[{},
        Switch[a,
            "f>af>b",
                $CanonicalOrdering = "f>af>b"
            ,
            "af>f>b",
                $CanonicalOrdering = "af>f>b"
            ,
            "b>f>af",
                $CanonicalOrdering = "b>f>af"
            ,
            "b>af>f",
                $CanonicalOrdering = "b>af>f"
            ,
            _,
                Message[CanonicalOrdering::unknownString, a]
        ];
        Print["Canonical ordering set to ", $CanonicalOrdering];
    ];

(**********************************************************************************
    Ordering expressions
**********************************************************************************)

(*Returns true if f1 < f2, and false if f1 > f2*)

FieldOrderLess[setup_, f1_Symbol, f2_Symbol] :=
    FieldOrderLess[setup, f1, f2] =
        Module[{kind1, kind2, idxOrder, n1, n2},
            kind1 = {IsFermion[setup, #], IsAntiFermion[setup, #], IscField[setup, #], IsAnticField[setup, #], # === AnyField}&[f1];
            kind2 = {IsFermion[setup, #], IsAntiFermion[setup, #], IscField[setup, #], IsAnticField[setup, #], # === AnyField}&[f2];

            Switch[$CanonicalOrdering,
                "f>af>b",
                    idxOrder = {4, 3, 2, 1, 0}
                ,
                "af>f>b",
                    idxOrder = {3, 4, 1, 2, 0}
                ,
                "b>f>af",
                    idxOrder = {2, 1, 4, 3, 0}
                ,
                "b>af>f",
                    idxOrder = {1, 2, 3, 4, 0}
                ,
                _,
                    Print["Order failure: order \"" <> $CanonicalOrdering <> "\" unknown."];
                    Abort[];
            ];
            n1 = Pick[idxOrder, kind1][[1]];
            n2 = Pick[idxOrder, kind2][[1]];

            If[n1 === n2,
                Return[OrderedQ[{f1, f2}]]
            ];
            Return[n1 < n2]
        ];

(*Returns the sign that results from exchanging the two fields f1 and f2*)

CommuteSign[setup_, f1_, f2_] :=
    CommuteSign[setup, f1, f2] =
        Module[{},
            Return[-2 * Boole[MemberQ[GetAntiCommuting[setup], f1] && MemberQ[GetAntiCommuting[setup], f2]] + 1];
        ];

(*Excluding indices in certain objects from being reordered*)

$unorderedIndices[_] = 0;

SetUnorderedIndices[obj_, n_Integer] /; n >= 0 && MemberQ[$allObjects, obj] :=
    Set[$unorderedIndices[obj], n];

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
    Module[{i, curi, prefactor, nfields = fields, norder = Range[Length[fields]]},
        prefactor = 1;
        (*Always compare the ith field with all previous fields and put it in the right place. Iterate until one reaches the end of the array, then it is sorted.*)
        i = 1;
        While[
            i <= Length[nfields]
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
                If[curi + 1 > Length[nfields],
                    Message[OrderObject::cantOrder, fields, fieldOrder];
                    Abort[]
                ];
                nfields[[{curi, curi + 1}]] = nfields[[{curi + 1, curi}]];
                norder[[{curi, curi + 1}]] = norder[[{curi + 1, curi}]];
                prefactor *= CommuteSign[setup, nfields[[curi]], nfields[[curi + 1]]];
                curi++;
            ];
        ];
        Return[{prefactor, norder}];
    ];

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
    OrderFields[setup, expr];
