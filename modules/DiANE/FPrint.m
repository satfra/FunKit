(**********************************************************************************
    Index list formatting
**********************************************************************************)

MakeTexIndexList[{i__}] :=
    Module[{ni = Length[{i}], isLower, indices = {i}, lowerList, upperList, removeTrailingPhantoms},
        isLower = Map[MatchQ[#, Times[-1, _]]&, {i}];
        indices =
            Table[
                If[isLower[[idx]],
                    -indices[[idx]]
                    ,
                    indices[[idx]]
                ]
                ,
                {idx, 1, ni}
            ];
        lowerList =
            Table[
                If[isLower[[idx]],
                    ToString @ TeXForm[indices[[idx]]]
                    ,
                    "\\phantom{" <> ToString @ TeXForm[indices[[idx]]] <> "}"
                ]
                ,
                {idx, 1, ni}
            ];
        upperList =
            Table[
                If[isLower[[idx]],
                    "\\phantom{" <> ToString @ TeXForm[indices[[idx]]] <> "}"
                    ,
                    ToString @ TeXForm[indices[[idx]]]
                ]
                ,
                {idx, 1, ni}
            ];
        removeTrailingPhantoms[l_] :=
            Module[{ret = l},
                While[
                    StringContainsQ[ret[[-1]], "\\phantom"]
                    ,
                    ret = Delete[ret, -1];
                    If[Length[ret] === 0,
                        Return[{""}]
                    ];
                ];
                Return[ret];
            ];
        upperList = removeTrailingPhantoms @ upperList;
        lowerList = removeTrailingPhantoms @ lowerList;
        Return[{StringJoin @@ lowerList, StringJoin @@ upperList}]
    ]

(* ::Input::Initialization:: *)

MakeIdxField[f_, i_, up] :=
    MakeIdxField[
        f
        ,
        If[MatchQ[i, Times[-1, _]],
            -i
            ,
            i
        ]
    ]

MakeIdxField[f_, i_, down] :=
    MakeIdxField[
        f
        ,
        If[MatchQ[i, Times[-1, _]],
            i
            ,
            -i
        ]
    ]

MakeIdxField[f_, i_] :=
    Module[{isLower, idx},
        isLower = MatchQ[i, Times[-1, _]];
        idx =
            If[isLower,
                -i
                ,
                i
            ];
        If[f === AnyField,
            Return[ToString[TeXForm[idx]]]
        ];
        If[isLower,
            Return[ToString @ TeXForm[Subscript[f, idx]]]
        ];
        Return[ToString @ TeXForm[Superscript[f, idx]]]
    ]

MakeTexIndexList[{f__}, {i__}] :=
    Module[{ni = Length[{f}], isLower, fields = {f}, indices = {i}, lowerList, upperList, removeTrailingPhantoms},
        isLower = Map[MatchQ[#, Times[-1, _]]&, {i}];
        lowerList =
            Table[
                If[isLower[[idx]],
                    MakeIdxField[fields[[idx]], indices[[idx]], up]
                    ,
                    "\\phantom{" <> MakeIdxField[fields[[idx]], indices[[idx]], up] <> "}"
                ]
                ,
                {idx, 1, ni}
            ];
        upperList =
            Table[
                If[isLower[[idx]],
                    "\\phantom{" <> MakeIdxField[fields[[idx]], indices[[idx]], up] <> "}"
                    ,
                    MakeIdxField[fields[[idx]], indices[[idx]], up]
                ]
                ,
                {idx, 1, ni}
            ];
        removeTrailingPhantoms[l_] :=
            Module[{ret = l},
                While[
                    StringContainsQ[ret[[-1]], "\\phantom"]
                    ,
                    ret = Delete[ret, -1];
                    If[Length[ret] === 0,
                        Return[{""}]
                    ];
                ];
                Return[ret];
            ];
        upperList = removeTrailingPhantoms @ upperList;
        lowerList = removeTrailingPhantoms @ lowerList;
        Return[{StringJoin @@ lowerList, StringJoin @@ upperList}]
    ]

(**********************************************************************************
    Prettify closed indices
**********************************************************************************)

$availableIndices = Join[Alphabet["English"], Alphabet["English", "CommonAlphabetUpper"], Alphabet["Greek"]];

prettySuperIndices::type = "Unknown type `1`";

prettySuperIndices[setup_, expr_FEx] :=
    Map[prettySuperIndices[setup, #]&, expr];

prettySuperIndices[setup_, expr_FTerm] :=
    Module[{closedIndices, openIndices, repl, indices},
        closedIndices = GetClosedSuperIndices[setup, expr];
        openIndices = GetOpenSuperIndices[setup, expr];
        indices = $availableIndices;
        Do[indices = Select[indices, # =!= ToString[openIndices[[i]]]&], {i, 1, Length[openIndices]}];
        repl = Thread[closedIndices -> indices[[1 ;; Length[closedIndices]]]];
        Return[expr //. repl]
    ];

prettySuperIndices[setup_, expr_Association] /; isLoopAssociation[expr] :=
    Association[Normal[expr] /. FEx[a___] :> prettySuperIndices[setup, FEx[a]]]

prettySuperIndices[setup_, expr_List] :=
    Map[prettySuperIndices[setup, #]&, expr];

prettySuperIndices[setup_, expr_Association] /; isRoutedAssociation[expr] :=
    Association @ Map[prettySuperIndices[setup, #]&, Normal @ expr];

prettySuperIndices[setup_, a_] :=
    (
        Message[prettySuperIndices::type, Head @ a];
        Abort[]
    )

prettyExplicitIndices::type = "Unknown type `1`";

prettyExplicitIndices[setup_, expr_FEx] :=
    Map[prettyExplicitIndices[setup, #]&, expr];

prettyExplicitIndices[setup_, expr_FTerm] :=
    Module[{allIndices, closedIndices, openIndices, repl, indices, ret = expr},
        allIndices = Select[ExtractObjectsAndIndices[setup, expr][[2]], Head[#] === List&];
        allIndices =
            Map[
                If[Length[#] === 2,
                    #
                    ,
                    ret = ret /. # -> Join[#, {Hash[Sort @ {#, -#}]}];
                    Join[#, {Hash[Sort @ {#, -#}]}]
                ]&
                ,
                allIndices
            ];
        allIndices = allIndices[[All, 2]];
        closedIndices = Pick[allIndices, Count[allIndices, #] === 2& /@ allIndices];
        openIndices = Pick[allIndices, Count[allIndices, #] =!= 2& /@ allIndices];
        indices = $availableIndices;
        repl = Thread[closedIndices -> indices[[1 ;; Length[closedIndices]]]] \[Union] Thread[openIndices -> indices[[Length[closedIndices] + 1 ;; Length[closedIndices] + Length[openIndices]]]];
        Return[ret //. repl]
    ];

prettyExplicitIndices[setup_, expr_Association] /; isLoopAssociation[expr] :=
    Association[Normal[expr] /. FEx[a___] :> prettyExplicitIndices[setup, FEx[a]]]

prettyExplicitIndices[setup_, expr_List] :=
    Map[prettyExplicitIndices[setup, #]&, expr];

prettyExplicitIndices[setup_, expr_Association] /; isRoutedAssociation[expr] :=
    Association @ Map[prettyExplicitIndices[setup, #]&, Normal @ expr];

prettyExplicitIndices[setup_, a_] :=
    (
        Message[prettyExplicitIndices::type, Head @ a];
        Abort[]
    )

(**********************************************************************************
    User-defined formatting definitions
**********************************************************************************)

$TexStyles = {};

$Fields = {};

AddTexStyles::invalidRule = "The given set of style rules does not follow the pattern Symbol->String.";

AddTexStyles[a__Rule] :=
    Module[{},
        If[Or @@ Map[Head[#] =!= String&, Values[{a}]],
            Message[AddTexStyles::invalidRule];
            Abort[]
        ];
        $TexStyles = DeleteDuplicates[Join[$TexStyles, {a}]];
    ]

FSetTexStyles[a__Rule] :=
    Module[{},
        If[Or @@ Map[Head[#] =!= String&, Values[{a}]],
            Message[AddTexStyles::invalidRule];
            Abort[]
        ];
        $TexStyles = DeleteDuplicates[{a}];
    ]

FSetTexStyles[] :=
    Module[{},
        $TexStyles = {};
    ]

(**********************************************************************************
    Utils
**********************************************************************************)

isLoopAssociation[expr_] :=
    Module[{},
        If[Head[expr] =!= Association,
            Return[False]
        ];
        If[FreeQ[Keys[expr], "Expression"],
            Return[False]
        ];
        If[FreeQ[Keys[expr], "ExternalIndices"],
            Return[False]
        ];
        If[FreeQ[Keys[expr], "LoopMomenta"],
            Return[False]
        ];
        Return[True];
    ];

isRoutedAssociation[expr_] :=
    Module[{},
        If[Head[expr] =!= Association,
            Return[False]
        ];
        Return @ AllTrue[expr, isLoopAssociation]
    ];

(**********************************************************************************
    Main definitions for TeXForm
**********************************************************************************)

RenewFormatDefinitions[] :=
    Module[{},
        Unprotect[FDOp, GammaN, Propagator, Rdot, FTerm, FEx, \[Gamma], \[Delta], FMinus, S];
        Unprotect @@ $allObjects;
        (*Field formatting with superindices*)
        Map[
            (
                Format[Keys[#][any_], TeXForm] :=
                    Module[{head, arg},
                        head = Keys[#] //. $TexStyles //. $Fields;
                        arg = Format[any, TeXForm] // ToString;
                        TeXUtilities`TeXVerbatim[head <> arg]
                    ];
                Format[Superscript[Keys[#], any_], TeXForm] :=
                    Module[{head, arg},
                        head = Keys[#] //. $TexStyles //. $Fields;
                        arg = Format[any, TeXForm] // ToString;
                        TeXUtilities`TeXVerbatim[head <> "^{" <> arg <> "}"]
                    ];
                Format[Subscript[Keys[#], any_], TeXForm] :=
                    Module[{head, arg},
                        head = Keys[#] //. $TexStyles //. $Fields;
                        arg = Format[any, TeXForm] // ToString;
                        TeXUtilities`TeXVerbatim[head <> "_{" <> arg <> "}"]
                    ];
            )&
            ,
            $TexStyles \[Union] Select[$Fields, FreeQ[Keys[$TexStyles], Keys[#]]&]
        ];
        (*Field formatting with explicit indices*)
        Map[
            (
                Format[Keys[#][{p_, indices_}], TeXForm] :=
                    Module[{head, arg},
                        head = Keys[#] //. $TexStyles //. $Fields;
                        arg = Format[indices, TeXForm] // ToString;
                        TeXUtilities`TeXVerbatim[head <> arg <> "(" <> ToString[TeXForm[p]] <> ")"]
                    ];
                Format[Superscript[Keys[#], {p_, indices_}], TeXForm] :=
                    Module[{head, arg},
                        head = Keys[#] //. $TexStyles //. $Fields;
                        arg = Format[indices, TeXForm] // ToString;
                        TeXUtilities`TeXVerbatim[head <> "^{" <> arg <> "}(" <> ToString[TeXForm[p]] <> ")"]
                    ];
                Format[Subscript[Keys[#], {p_, indices_}], TeXForm] :=
                    Module[{head, arg},
                        head = Keys[#] //. $TexStyles //. $Fields;
                        arg = Format[indices, TeXForm] // ToString;
                        TeXUtilities`TeXVerbatim[head <> "_{" <> arg <> "}(" <> ToString[TeXForm[p]] <> ")"]
                    ];
            )&
            ,
            $TexStyles \[Union] Select[$Fields, FreeQ[Keys[$TexStyles], Keys[#]]&]
        ];
        (*Other formatting*)
        Format[FDOp[f_], TeXForm] :=
            Module[{},
                TeXUtilities`TeXDelimited["\\frac{\\delta}{\\delta", f, "}"]
            ];
        Map[
            (
                Format[#[{f__}, {i__}], TeXForm] :=
                    Module[{sub, sup, ret},
                        {sub, sup} = MakeTexIndexList[{f}, {i}];
                        ret =
                            Switch[#,
                                Propagator,
                                    "G"
                                ,
                                FMinus,
                                    "(-1)"
                                ,
                                Rdot,
                                    "\\partial_t R"
                                ,
                                Phidot,
                                    "\\dot{\\Phi}"
                                ,
                                GammaN,
                                    "\\Gamma"
                                ,
                                S,
                                    "S"
                                ,
                                _,
                                    TeXForm[#] // ToString
                            ];
                        If[(# //. $TexStyles) =!= #,
                            ret = (# //. $TexStyles)
                        ];
                        If[StringLength[sub] =!= 0,
                            ret = ret <> "_{" <> sub <> "}"
                        ];
                        If[StringLength[sup] =!= 0,
                            ret = ret <> "^{" <> sup <> "}"
                        ];
                        TeXUtilities`TeXVerbatim[ret]
                    ]
            )&
            ,
            $allObjects
        ];
        Map[
            (
                Format[#[{f__}, {i__}], TeXForm] /; AllTrue[{i}, (Head[#] === List)&] :=
                    Module[{sub, sup, ret},
                        {sub, sup} = MakeTexIndexList[{f}, -{i}[[All, 2]]];
                        ret =
                            Switch[#,
                                Propagator,
                                    "G"
                                ,
                                FMinus,
                                    "(-1)"
                                ,
                                Rdot,
                                    "\\partial_t R"
                                ,
                                Phidot,
                                    "\\dot{\\Phi}"
                                ,
                                GammaN,
                                    "\\Gamma"
                                ,
                                S,
                                    "S"
                                ,
                                _,
                                    TeXForm[#] // ToString
                            ];
                        If[(# //. $TexStyles) =!= #,
                            ret = (# //. $TexStyles)
                        ];
                        If[StringLength[sub] =!= 0,
                            ret = ret <> "_{" <> sub <> "}"
                        ];
                        If[StringLength[sup] =!= 0,
                            ret = ret <> "^{" <> sup <> "}"
                        ];
                        TeXUtilities`TeXVerbatim[ret <> "(" <> StringRiffle[Map[ToString @ TeXForm[#]&, {i}[[All, 1]]], ","] <> ")"]
                    ]
            )&
            ,
            $allObjects
        ];
        Format[FTerm[a__], TeXForm] :=
            Module[{obj, integrals, replNames, idx, prefix, postfix, body, fac, terms},
                integrals = Pick[$availableLoopMomenta, Map[MemberQ[{a}, #, Infinity]&, $availableLoopMomenta]];
                replNames = Join[Thread[$availableLoopMomenta -> Table[Subscript[Symbol[$loopMomentumName], idx], {idx, 1, Length[$availableLoopMomenta]}]], Thread[$availableLoopMomentaf -> Table[Subscript[Symbol[$loopMomentumName], "f," <> ToString @ idx], {idx, 1, Length[$availableLoopMomentaf]}]]];
                prefix = StringJoin[Map["\\int_{" <> ToString[TeXForm[#]] <> "}"&, integrals //. replNames]];
                postfix = "";
                {fac, body} = SplitPrefactor[FTerm[a]];
                body = List @@ body;
                If[MemberQ[fac, _?Negative, {0, 1}],
                    If[fac === -1,
                        prefix = prefix <> "\\left(-";
                        postfix = "\\right)" <> postfix;
                        ,
                        prefix = prefix <> "\\left(";
                        postfix = "\\right)" <> postfix;
                        body = Join[{fac}, body];
                    ]
                    ,
                    If[fac =!= 1,
                        body = Join[{fac}, body]
                    ]
                ];
                body = body //. replNames;
                (* Apply subscript formatting *)
                body = body //. Map[# -> Subscript[Symbol[StringTake[SymbolName[#], {1}]], ToExpression @ StringTake[SymbolName[#], {2, -1}]]&, Select[GetAllSymbols[body], StringMatchQ[SymbolName[#], LetterCharacter ~~ DigitCharacter..]&]];
                (* If there are any factors with head Plus, we need to enclose them with braces *)
                body =
                    Flatten @
                        Map[
                            If[Head[#] === Plus,
                                {"\\left(", #, "\\right)"}
                                ,
                                #
                            ]&
                            ,
                            body
                        ];
                TeXUtilities`TeXDelimited[
                        prefix
                        ,
                        ##
                        ,
                        postfix
                        ,
                        "DelimSeparator" -> ""
                        ,
                        "BodySeparator" -> "\\,"
                        ,
                        (*It is not clear why the call to RenewFormatDefinitions[] is necessary here. However, removing it leads to TeXForm ignoring all custom TeXStyles.*)
                        "BodyConverter" ->
                            (
                                If[Head[#] === String,
                                    #
                                    ,
                                    ToString[
                                        RenewFormatDefinitions[];
                                        Format[#, TeXForm]
                                    ]
                                ]&
                            )
                    ]& @@ body
            ];
        Format[FTerm[], TeXForm] :=
            Module[{},
                TeXUtilities`TeXVerbatim["1"]
            ];
        Format[FEx[a___], TeXForm] :=
            If[Length[Flatten[(List @@ #&) /@ {a}]] <= 9,
                TeXUtilities`TeXDelimited["", a, "", "DelimSeparator" -> "", "BodySeparator" -> "\n\\,+\\,", "BodyConverter" -> (ToString[Format[#, TeXForm]]&)]
                ,
                TeXUtilities`TeXDelimited["\\begin{aligned}\\  &", a, "\n\\end{aligned}", "DelimSeparator" -> "", "BodySeparator" -> "\n\\\\ &\\,+\\,", "BodyConverter" -> (ToString[Format[#, TeXForm]]&)]
            ];
        Unprotect[Association];
        Format[Association[a__], TeXForm] /; isRoutedAssociation[Association[a]] :=
            Module[{parts},
                parts = (List @@ Association[a])[[All, Key["Expression"]]];
                parts = ToString[TeXForm[FEx[#]]]& /@ parts;
                parts =
                    Join[
                        {parts[[1]]}
                        ,
                        Map[
                            If[StringTake[#, {1, 17}] === "\\begin{aligned}&\n",
                                StringJoin[{"\\begin{aligned}&\n\\,+\\,", StringTake[#, {18, -1}]}]
                                ,
                                StringJoin[{"\\,+\\,", #}]
                            ]&
                            ,
                            parts[[2 ;; ]]
                        ]
                    ];
                TeXUtilities`TeXVerbatim["\\begin{aligned}&\n" <> StringRiffle[parts, "\n \\\&\n"] <> "\n\\end{aligned}"]
            ];
        Protect[Association];
        Protect[FDOp, GammaN, Propagator, Rdot, FTerm, FEx, \[Gamma], \[Delta], FMinus, S];
        Protect @@ $allObjects;
    ];

(**********************************************************************************
    FPrint and FTex commands
**********************************************************************************)

FTex[setup_, expr_] :=
    Module[
        {prExp = expr, fields, ret}
        ,
        (*Turn a given expression into LaTeX code*)
        If[Head[prExp] === FEx,
            prExp = DropFExAnnotations[prExp];
        ];
        AssertFSetup[setup];
        fields = GetAllFields[setup];
        FunKitDebug[1, "Creating LaTeX expression"];
        (*update the formatting definitions for TeXForm*)
        $Fields = Thread[fields -> Map[ToString[TeXForm[#]]&, fields]];
        RenewFormatDefinitions[];
        (*Assign human-readable superindices*)
        prExp = prettySuperIndices[setup, prExp];
        prExp = prettyExplicitIndices[setup, prExp];
        (*make fields with indices to sub-/super-script*)
        prExp = prExp //. Map[#[Times[-1, a_]] :> Subscript[#, a]&, fields] //. Map[#[a_] :> Superscript[#, a]&, fields];
        (*For correct rendering, fully expand any FTerms*)
        prExp = prExp //. FTerm[pre___, Times[a_, b_], post___] :> FTerm[pre, a, b, post];
        ret = ToString[TeXForm[prExp]];
        ret = StringReplace[ret, "\\text{" ~~ s:LetterCharacter ~~ "}" -> s];
        Return[ret];
    ];

FTex[setup_, expr_List] /; AllTrue[expr, (Head[#] === FEx || Head[#] === FTerm)&] :=
    FTex[setup, FEx @@ expr];

(*For the output of a full routing*)

FTex[setup_, expr_Association] /; isRoutedAssociation @ expr :=
    FTex[setup, (List @@ expr)[[All, Key["Expression"]]]];

FTex[setup_, expr_Association] /; isLoopAssociation @ expr :=
    FTex[setup, expr["Expression"]];

(*For direct printing*)

FPrint[setup_, expr_] :=
    (
        Print[FTex[setup, expr] // MaTeX`MaTeX];
        Return[expr]
    );
