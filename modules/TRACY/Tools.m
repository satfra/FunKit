(**********************************************************************************
     Miscellaneous tools for TRACY
**********************************************************************************)

SafeReplaceTrace[expr_] :=
    Module[{allDressings, rule, reverse},
        allDressings = Cases[expr, dressing[__] | _Real, Infinity] // DeleteDuplicates;
        rule = Map[# -> Unique["dressing"]&, allDressings];
        reverse = Map[Values[#] -> Keys[#]&, rule];
        FormTracer`DefineFormAutoDeclareFunctions @@ (FormTracer`Private`formCFunctionAutoDeclareList \[Union] {SymbolName @ dressing});
        Return[{rule, reverse}];
    ];

Protect @ $dummy;

customExclusions[a_] :=
    And @@ {a =!= List, a =!= Complex, a =!= Plus, a =!= Power, a =!= Times, a =!= Rational, a =!= Pattern, a =!= $dummy}

removeFORMTracerRule :=
    Map[Head[#][__] :> $dummy[RandomInteger[10^12]]&, Values[FormTracer`Private`lorentzTensorReplacementRulesOutput // Normal]] \[Union] Map[Head[#][__] :> $dummy[RandomInteger[10^12]]&, Values[FormTracer`Private`groupTensorReplacementRulesOutput // Normal]] \[Union] Map[#[__] :> $dummy[RandomInteger[10^12]]&, FormTracer`Private`combinedTensorNames] \[Union] Map[# :> $dummy[RandomInteger[10^12]]&, Global`GetFormTracerGroupConstants[]];

GetAllCustomSymbols[expr_] :=
    Module[{obj},
        obj = DeleteDuplicates @ Cases[expr /. removeFORMTracerRule, (a_Symbol /; customExclusions[a]) | (a_Symbol[__] /; customExclusions[a]), Infinity];
        obj = DeleteDuplicates @ ((# /. a_[__] :> a)& /@ obj);
        Return[obj];
    ];

GetAllMomenta[expr_] :=
    Module[{obj},
        obj = DeleteDuplicates @ Cases[expr, sp[__] | sps[__] | vec[__] | vecs[__], Infinity];
        obj = obj /. {sp[a_, b_] :> {a, b}, sps[a_, b_] :> {a, b}, vecs[a_, _] :> {a}, vec[a_, _] :> {a}} // Flatten;
        obj // DeleteDuplicates
    ];

(* ::Input::Initialization:: *)

ClearAll[balancedBracesQ]

balancedBracesQ[str_String] :=
    Module[{cases, idx},
        If[Not @ (StringCount[str, "("] === StringCount[str, ")"]),
            Return[False]
        ];
        cases = StringCases[str, "(" | ")"];
        For[idx = 1, idx <= Length[cases], idx++,
            If[(Count[cases[[ ;; idx]], "("] < Count[cases[[ ;; idx]], ")"]),
                Return[False]
            ];
        ];
        Return[True];
    ];

balancedBracketsQ[str_String] :=
    Module[{cases, idx},
        If[Not @ (StringCount[str, "["] === StringCount[str, "]"]),
            Return[False]
        ];
        cases = StringCases[str, "[" | "]"];
        For[idx = 1, idx <= Length[cases], idx++,
            If[(Count[cases[[ ;; idx]], "["] < Count[cases[[ ;; idx]], "]"]),
                Return[False]
            ];
        ];
        Return[True];
    ];

hasFortranOperator[a_] :=
    StringContainsQ[a, "*"] || StringContainsQ[a, "/"] || StringContainsQ[a, "+"] || StringContainsQ[a, "-"] || StringContainsQ[a, "**"] || StringContainsQ[a, "^"] || StringContainsQ[a, "="] || StringContainsQ[a, "("] || StringContainsQ[a, ")"] || StringContainsQ[a, " "];

fortranToMathematica[expr_String] :=
    Module[{start, res, pres},
        start = StringPosition[expr, "\n"];
        start =
            If[Length[start] <= 1,
                1
                ,
                start[[2, 1]]
            ];
        res = StringTake[expr, {start, -1}];
        While[
            pres =!= res
            ,
            pres = res;
            res = StringReplace[res, {Shortest[(a_ /; Not @ hasFortranOperator[a]) ~~ "(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> a ~~ "[" ~~ arg1 ~~ "]", Shortest["pow(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "Power[" ~~ arg1 ~~ "]", Shortest["sqrt(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "Sqrt[" ~~ arg1 ~~ "]", Shortest["FTxsp(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "sp[" ~~ arg1 ~~ "]", Shortest["FTxsps(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "sps[" ~~ arg1 ~~ "]", Shortest["FTxvec(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "vec[" ~~ arg1 ~~ "]", Shortest["FTxvecs(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "vecs[" ~~ arg1 ~~ "]", Shortest["FTxvecs(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "vecs[" ~~ arg1 ~~ "]", Shortest["w(" ~~ (arg1__ /; balancedBracesQ[arg1]) ~~ ")"] :> "$w$[" ~~ arg1 ~~ "]", "**" -> "^", " " -> "", "&\n&" -> " ", "i_" -> "\!\(\*TagBox[
StyleBox[
RowBox[{\"Complex\", \"[\", 
RowBox[{\"0\", \",\", \"1\"}], \"]\"}],\nShowSpecialCharacters->False,\nShowStringCharacters->True,\nNumberMarks->True],
FullForm]\)", "expr=" ~~ a__ :> a}];
            res = StringReplace[res, Map[ToString[#[[2]]] -> ToString[#[[1]]]&, FormTracer`GetExtraVarsSynonyms[]]];
        ];
        StringReplace[res, ";" -> "\n"]
    ];

ImportAndSimplifyFORM[file_, transf_ : (#&), mSimplify_ : (Simplify[#, Trig -> False, TimeConstraint -> 0.05]&)] :=
    Module[{fortran, math, splitmath, getAffectedVar, tempExpr, evalExpr, strExpr, expr, i, monitor},
        fortran = Import[file, "Text"];
        math = fortranToMathematica[fortran];
        splitmath = StringSplit[math, "\n"];
        getAffectedVar[line_String] :=
            Module[{pos, str},
                pos = StringPosition[line, Shortest["w[" ~~ (arg1__ /; balancedBracketsQ[arg1]) ~~ "]"]];
                str = StringTake[line, pos][[1]];
                Return[str];
            ];
        tempExpr = Select[splitmath, (StringLength[#] > 0 && StringTake[#, {1}] === "w")&];
        evalExpr = splitmath[[-1]];
        If[$FrontEnd === Null,
            Do[
                strExpr = getAffectedVar[tempExpr[[i]]];
                expr = strExpr ~~ "=" ~~ ToString[mSimplify] ~~ "[" ~~ ToString[transf] ~~ "[" ~~ strExpr ~~ "]]";
                ToExpression[tempExpr[[i]]];
                ToExpression[expr];
                ,
                {i, 1, Length[tempExpr]}
            ];
            ,
            ResourceFunction["MonitorProgress"][
                Do[
                    strExpr = getAffectedVar[tempExpr[[i]]];
                    expr = strExpr ~~ "=" ~~ ToString[mSimplify] ~~ "[" ~~ ToString[transf] ~~ "[" ~~ strExpr ~~ "]]";
                    ToExpression[tempExpr[[i]]];
                    ToExpression[expr];
                    ,
                    {i, 1, Length[tempExpr]}
                ]
            ];
        ];
        Return[ToExpression[evalExpr]];
    ];

(**********************************************************************************
     Trying to normalize indices in expressions
**********************************************************************************)

NormalizeIndices[expr_FTerm] :=
    Module[{cindices, orderingFunction},
        cindices = FormTracer`GetClosedIndices[expr];
        orderingFunction[e1_, e2_] :=
            Module[{p1, p2, idx},
                p1 = FirstPosition[expr, e1];
                p2 = FirstPosition[expr, e2];
                For[idx = 1, idx <= Min[Length[p1], Length[p2]], idx++,
                    If[p1[[idx]] < p2[[idx]],
                        Return[True]
                    ];
                    If[p1[[idx]] > p2[[idx]],
                        Return[False]
                    ];
                ];
                Return[False];
            ];
        cindices = Sort[cindices, orderingFunction];
        expr /. Thread[cindices -> Table[Symbol["nIdx" <> ToString[idx]], {idx, 1, Length[cindices]}]]
    ];

NormalizeIndices[ex_List] :=
    Map[NormalizeIndices, ex];

NormalizeIndices[ex_FEx] :=
    Map[NormalizeIndices, ex];

NormalizeIndices[ex_] :=
    ex;

makeHashFile[expr_, subdir_:""] :=
    $TraceCacheDir <> subdir <> ToString @ Hash[NormalizeIndices @ expr, "SHA256"] <> ".m"
