(**********************************************************************************
    Overall Defs
**********************************************************************************)

type::error = "The expression given is neither an FEx nor an FTerm.";

(**********************************************************************************
    RULES: FTerm
**********************************************************************************)

Unprotect[FTerm, NonCommutativeMultiply];

FTerm::TimesError = "An FTerm cannot be multiplied using Times[__]. To multiply FTerms, use term1**term2, also with scalars, a**term. Error in expression
`1`";

FTerm::FTermPowerError = "An FTerm cannot be taken to a power of an FTerm.";

(*Multiplication of FTerms. *)

FTerm /: NonCommutativeMultiply[FTerm[], FTerm[]] :=
    FTerm[]

FTerm /: NonCommutativeMultiply[FTerm[a__], FTerm[b__]] :=
    FTerm[a, b]

FTerm /: NonCommutativeMultiply[FTerm[], FTerm[b__]] :=
    FTerm[b]

FTerm /: NonCommutativeMultiply[FTerm[b__], FTerm[]] :=
    FTerm[b]

FTerm /: NonCommutativeMultiply[a_, FTerm[b__]] /; NumericQ[a] :=
    FTerm[a, b]

NonCommutativeMultiply /: FTerm[pre___, NonCommutativeMultiply[in___], post___] :=
    FTerm[pre, in, post]

(*FTerm[pre___,Times[inpre__,NonCommutativeMultiply[in___],inpost__],post___]:=FTerm[pre,inpre*inpost,in,post]*)

FTerm[1, post___] :=
    FTerm[post]

couldBeField = MatchQ[#, _Symbol[_Symbol]] || MatchQ[#, _Symbol[-_Symbol]] || MatchQ[#, _Symbol[{_, _List}]] || MatchQ[#, _Symbol[{_, _List}]]&;

isFreeTerm = Not @ (ContainsAny[GetAllSymbols[#], $nonCommutingObjects] || Or @@ Map[couldBeField, #, Infinity])&;

FTerm[first_, pre___, Times[a_, other2_], post___] /; NumericQ[first] && NumericQ[a] :=
    FTerm[first * a, pre, other2, post]

FTerm[first_, pre___, Times[a_, other2_], post___] /; Not @ NumericQ[first] && NumericQ[a] :=
    FTerm[a, first, pre, other2, post]

FTerm[first_, pre___, a_, post___] /; NumericQ[first] && NumericQ[a] :=
    FTerm[first * a, pre, post]

FTerm[first_, pre___, a_, post___] /; Not[NumericQ[first]] && NumericQ[a] :=
    FTerm[a, first, pre, post]

(*Some reduction for FTerm[]*)

FTerm /: FTerm[] * FTerm[a_] :=
    FTerm[a]

FTerm /: Plus[FTerm[], FTerm[b_]] /; NumericQ[b] :=
    FTerm[1 + b];

FTerm /: Power[FTerm[], n_] :=
    FTerm[];

FTerm /: Log[FTerm[]] :=
    FTerm[0];

FTerm[{}] :=
    FTerm[0];

FTerm /: Times[a_, FTerm[]] :=
    FTerm[a];

FTerm /: Power[FTerm[a_], b_] /; NumericQ[a] && NumericQ[b] :=
    FTerm[Power[a, b]];

FTerm /: Plus[FTerm[a_], FTerm[b_]] /; NumericQ[a] && NumericQ[b] :=
    FTerm[a + b];

FTerm /: Times[FTerm[a_], FTerm[b_]] /; NumericQ[a] && NumericQ[b] :=
    FTerm[a * b];

FTerm /: Times[a_, FTerm[b__]] /; NumericQ[a] :=
    FTerm[a, b];

(*Pre-reduction of zero FTerms*)

FTerm[___, 0, ___] :=
    FTerm[0]

FTerm[pre___, FTerm[], post___] :=
    FTerm[pre, post]

(*Reduction of immediately nested FTerms*)

FTerm[pre___, FTerm[in__], post___] :=
    FTerm[pre, in, post]

Protect[FTerm, NonCommutativeMultiply];

(**********************************************************************************
    RULES: FEx
**********************************************************************************)

Unprotect[FEx];

FEx::TimesError = "A FEx cannot be multiplied using Times[__]. To multiply FExs, use eq1**eq2, also with scalars, a**eq. Error in expression
`1`";

(*Removal of zero FTerms*)

FExNumericMerge[expr_] :=
    Module[{ret = expr, numeric = 0},
        ret =
            Map[
                    If[# === FTerm[],
                        numeric = numeric + 1;
                        {}
                        ,
                        If[Length[#] === 1 && NumericQ[#[[1]]],
                            numeric = numeric + #[[1]];
                            {}
                            ,
                            #
                        ]
                    ]&
                    ,
                    ret
                ] // Flatten;
        If[ret === {},
            Return[{FTerm[numeric] /. FTerm[0] -> {} // Flatten}]
            ,
            Return[{FTerm[numeric], ret} /. FTerm[0] -> {} // Flatten]
        ];
    ];

FEx[pre__, FTerm[], post___] :=
    FEx @@ FExNumericMerge[{pre, FTerm[], post}];

FEx[pre__, FTerm[a_], post___] /; NumericQ[a] :=
    FEx @@ FExNumericMerge[{pre, FTerm[a], post}];

FEx[FTerm[0], post___] :=
    FEx[post];

(*Sum splitting of FTerms*)

(*FEx[preEq___,FTerm[preTerm___,Plus[a_,b__],postTerm___],postEq___]:=(FEx[preEq,##,postEq]&@@(FTerm[preTerm,#,postTerm]&/@{a,b}))*)

(*Sums of FTerms*)

FEx[preEq___, Plus[FTerm[a__], FTerm[b__], c___], postEq___] :=
    FEx[preEq, FTerm[a], Plus[FTerm[b], c], postEq]

FEx /: Plus[FEx[a___], FTerm[b__]] :=
    FEx[a, FTerm[b]]

(*Sums of FExs*)

FEx /: Plus[preEq___, FEx[terms1___], midEq___, FEx[terms2___], postEq___] :=
    Plus[preEq, midEq, postEq, FEx[terms1, terms2]]

(*Multiplication of FExs*)

FEx /: Times[pre___, FEx[a__], post___] :=
    (
        Message[FEx::TimesError, {pre, FEx[a], post}];
        Abort[]
    );

FEx[preFEx__, Times[pre___, FTerm[a__], post___], postFEx__] :=
    (
        Message[FTerm::TimesError, {pre, FTerm[a], post}];
        Abort[]
    );

FEx /: NonCommutativeMultiply[FTerm[b__], FEx[c__FTerm, d___Rule]] :=
    FEx[Map[FTerm[b] ** #&, FEx[c]], d];

FEx /: NonCommutativeMultiply[FEx[c__, d___Rule], FTerm[b__]] :=
    FEx[Map[# ** FTerm[b]&, FEx[c]], d];

FEx /: NonCommutativeMultiply[FEx[a__, r1___Rule], FEx[b__, r2___Rule]] :=
    FEx[##, r1, r2]& @@ (Flatten @ Table[FEx[{a}[[i]] ** {b}[[j]]], {i, 1, Length[{a}]}, {j, 1, Length[{b}]}]);

(*Removing zeros and ones*)

FEx /: NonCommutativeMultiply[FTerm[b___], FEx[]] :=
    FEx[]

FEx /: NonCommutativeMultiply[FTerm[], FEx[b___]] :=
    FEx[b]

FEx /: NonCommutativeMultiply[FEx[b___], FTerm[]] :=
    FEx[b]

FEx /: NonCommutativeMultiply[FTerm[0], FEx[b___]] :=
    FEx[]

FEx /: NonCommutativeMultiply[FEx[b___], FTerm[0]] :=
    FEx[]

FEx[pre___, 0, post___] :=
    FEx[pre, post]

FEx /: FTerm[FEx[a__]] :=
    FEx[a];

FEx /: FTerm[FEx[a__], FEx[b__]] :=
    FEx @@ (Flatten @ Table[FEx[{a}[[i]] ** {b}[[j]]], {i, 1, Length[{a}]}, {j, 1, Length[{b}]}])

(*Fixing missing FTerms*)

FEx[pre___, a_, post___] /; (Head[a] =!= FTerm && Head[a] =!= FEx && Head[a] =!= Rule && FreeQ[{a}, Pattern, Infinity]) :=
    (FEx[pre, FTerm[a], post]);

(*Reduction of immediately nested FExs*)

FExMerge[a_List] :=
    Module[{prev, ret = a},
        prev = {};
        While[
            ret =!= prev
            ,
            prev = ret;
            ret =
                Map[
                        If[Head[#] === FEx,
                            List @@ #
                            ,
                            #
                        ]&
                        ,
                        ret
                    ] // Flatten;
        ];
        Return[ret];
    ];

FEx[pre___, FEx[in__], post___] :=
    FEx @@ (FExMerge[{pre, in, post}]);

FEx[pre___, FEx[], post___] :=
    FEx @@ (FExMerge[{pre, post}]);

FEx /: FTerm[FEx[a__]] :=
    FEx[a]

FEx /: FTerm[pre___, FEx[a__], post___] :=
    NonCommutativeMultiply[FTerm[pre], FEx[a], FTerm[post]]

(*Expand nested FEx in sub-terms*)

FEx[pre___, FTerm[prein___, FEx[in___], postin___], post___] :=
    FEx[pre, NonCommutativeMultiply[FTerm[prein], FEx[in], FTerm[postin]], post]

(*Annotations are always moved to the back (e.g. "Symmetries"->{...})*)

FEx[pre___, annotation_Rule, post_, end___] /; Head[post] =!= Rule :=
    FEx[pre, post, annotation, end];

FEx[annotations__Rule] :=
    FEx[];

FEx[FTerm[f___], annotations__Rule] /; AllTrue[$allObjects, FreeQ[{f}, #, Infinity]&] :=
    FEx[FTerm[f]];

Protect[FEx, FTerm];

(**********************************************************************************
    Expanding Plus[ ... ] inside FEx and FTerm
**********************************************************************************)

FExpand[expr_FTerm] :=
    Module[{ret = expr},
        Return[ret];
    ];

FExpand[expr_FEx] :=
    Module[{ret = expr},
        Return[ret];
    ];

(**********************************************************************************
    Splitting into prefactor and indexed objects
**********************************************************************************)

SplitPrefactor[setup_, expr_FTerm] :=
    Module[{prefactor, ret, objPattern, idx, removeOther},
        objPattern = Alternatives @@ Join[Map[Blank, $indexedObjects \[Union] {FDOp}], Map[Blank, GetAllFields[setup] \[Union] {AnyField}], Map[Superscript[#, _]&, GetAllFields[setup] \[Union] {AnyField}]];
        removeOther = Dispatch[{objPattern -> 1}];
        prefactor = expr /. removeOther;
        ret = List @@ expr;
        Do[ret[[idx]] = ret[[idx]] / (ret[[idx]] /. removeOther);, {idx, 1, Length[ret]}];
        Return[{Times @@ prefactor, FTerm @@ ret}];
    ];

SplitPrefactor[expr_FTerm] :=
    Module[{prefactor, ret, objPattern, idx, removeOther},
        objPattern = Alternatives @@ Join[Map[Blank, $indexedObjects \[Union] {FDOp}], Map[Blank, {AnyField}]];
        removeOther = Dispatch[{objPattern -> 1}];
        prefactor = expr /. removeOther;
        ret = List @@ expr;
        Do[ret[[idx]] = ret[[idx]] / (ret[[idx]] /. removeOther);, {idx, 1, Length[ret]}];
        Return[{Times @@ prefactor, FTerm @@ ret}];
    ];

(**********************************************************************************
    Annotations
**********************************************************************************)

SeparateFExAnnotations[infex_FEx] :=
    Module[
        {annotations = {}, mainTerms = {}, lastIdx, fex = List @@ infex}
        ,
        (*Just go from the back:*)
        lastIdx = Length[fex];
        While[lastIdx >= 1 && Head[fex[[lastIdx]]] === Rule, lastIdx--;];
        mainTerms = fex[[1 ;; lastIdx]];
        annotations = fex[[lastIdx + 1 ;; ]];
        Return[{mainTerms, Association @@ annotations}];
    ];

DropFExAnnotations[fex_FEx] :=
    SeparateFExAnnotations[fex][[1]];

MergeFExAnnotations[mainTerms_FEx, annotations_Association] :=
    FEx[##]& @@ Join[List @@ mainTerms, Normal @ annotations];

(**********************************************************************************
    Checks and assertions
**********************************************************************************)

FTermQ[expr_] :=
    Head[expr] === FTerm;

FTerm::notFTerm = "The term `1` is not an FTerm.";

AssertFTerm[expr_] :=
    If[Not @ FTermQ[expr],
        Message[FTerm::notFTerm, expr];
        Abort[]
    ];

FExQ[expr_] :=
    Head[expr] === FEx;

FEx::notFEx = "The term `1` is not an FEx.";

AssertFEx[expr_] :=
    If[Not @ FExQ[expr],
        Message[FEx::notFEx, expr];
        Abort[]
    ];

(**********************************************************************************
    FMinus
**********************************************************************************)

Unprotect[FMinus];

(*Grassmann minus signs do not care about index positioning. We force them always up*)

FMinus[{a_, b_}, {Times[-1, ia_], ib_}] :=
    FMinus[{a, b}, {ia, ib}]

FMinus[{a_, b_}, {ia_, Times[-1, ib_]}] :=
    FMinus[{a, b}, {ia, ib}]

(* Powers are simple *)

FMinus /: Power[FMinus[{a_, b_}, {ia_, ib_}], n_Integer] /; EvenQ[n] :=
    1

FMinus /: Power[FMinus[{a_, b_}, {ia_, ib_}], n_Integer] /; OddQ[n] :=
    FMinus[{a, b}, {ia, ib}]

Protect[FMinus];

(**********************************************************************************
    FDOp
**********************************************************************************)

Unprotect[FDOp];

FDOp::invalid = "`1` is not a valid FDOp.";

FDOp::invalidArguments = "`1` is not a valid FDOp. FDOp takes a single field with an index as argument.";

FDOp::arithmetic = "An FDOp cannot be included as anything but a factor in an FTerm. Error in 
  `1`";

FDOp /: Times[a___, FDOp[b___], c___] :=
    (
        Message[FDOp::arithmetic, StringTake[ToString @ (Hold @ Times[a, FDOp[b], c]), {6, -2}]];
        Abort[]
    )

FDOp /: Plus[a___, FDOp[b___], c___] :=
    (
        Message[FDOp::arithmetic, StringTake[ToString @ (Hold @ Plus[a, FDOp[b], c]), {6, -2}]];
        Abort[]
    )

FDOp[a_, b__] :=
    (
        Message[FDOp::invalidArguments, StringTake[ToString @ (Hold @ FDOp[a, b]), {6, -2}]];
        Abort[]
    )

FDOpQ[setup_, expr_] :=
    (Head[expr] === FDOp) && MatchQ[expr, (_[_, {__}] | _[_])] && FieldQ[setup, #]& @@ expr;

AssertFDOp[setup_, expr_] :=
    If[Not @ FDOpQ[setup, expr],
        Message[FDOp::invalid, expr];
        Abort[]
    ];

Protect[FDOp];

(**********************************************************************************
    Utility definitions
**********************************************************************************)

getFields[obj_] :=
    obj[[1]];

getField[obj_, pos_] :=
    obj[[1, pos]];

getIndices[obj_] :=
    obj[[2]];

getIndex[obj_, pos_] :=
    obj[[2, pos]];

getIdxSign[obj_, pos_] :=
    -2 * Boole[isNeg[getIndex[obj, pos]]] + 1;

makeObj[kind_Symbol, fieldList_List, indexList_List] :=
    kind[fieldList, indexList];

ObjectQ[expr_] :=
    MatchQ[expr, _Symbol[_List, _List]] && MemberQ[$OrderedObjects, Head[expr]];
