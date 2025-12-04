(*Julia*)

(* ::Subsection::Closed:: *)

(*Julia formatting*)

(* ::Input::Initialization:: *)

JuliaForm[expr_] :=
    Module[{Fstr},
        Fstr = ToString[FortranForm[expr //. {E^x_ :> exp[x]}]];
        StringReplace[Fstr, {a_ ~~ "(i)" -> a ~~ "[i]", a_ ~~ "(-1 + i)" -> a ~~ "[i-1]", a_ ~~ "(1 + i)" -> a ~~ "[i+1]", "**" -> "^", ".*" -> "*", ".+" -> "+", "Pi" -> "\[Pi]", "Sqrt" -> "sqrt", "Coth" -> "coth", "Tanh" -> "tanh"}]
    ];

IndentCode[code_String, level_Integer] :=
    Module[{ret = code},
        For[i = 1, i <= level, i++,
            ret = StringReplace[ret, "\n" -> "\n  "];
        ];
        (*Remove trailing whitespaces*)
        While[StringTake[ret, {-1}] === " ", ret = StringTake[ret, StringLength[ret] - 1];];
        Return[ret];
    ]

(* ::Subsection:: *)

(*Julia code creation*)

(* ::Input::Initialization:: *)

JuliaCode[equation_] :=
    Module[{optList, interpObj, replacementObj, replacementNames, replacements, definitions, returnStatement},
        optList = $codeOptimizeFunctions;
        interpObj = Flatten @ Map[Cases[equation, #, Infinity]&, optList];
        replacementObj = Keys @ Select[Counts[interpObj], # > 1&];
        replacementNames = Table["_repl" <> ToString[i], {i, 1, Length[replacementObj]}];
        replacements = Table[replacementObj[[i]] -> replacementNames[[i]], {i, 1, Length[replacementObj]}];
        definitions =
            If[Length[replacementObj] > 0,
                StringJoin[Table[ToString[replacementNames[[i]]] <> " = " <> JuliaForm[FullSimplify @ replacementObj[[i]]] <> "\n", {i, 1, Length[replacementObj]}]] <> "\n"
                ,
                ""
            ];
        returnStatement = "return " <> JuliaForm[equation //. replacements];
        returnStatement = StringReplace[returnStatement, Map["\"" <> # <> "\"" -> #&, replacementNames]];
        definitions <> returnStatement
    ];

(* ::Subsection:: *)

(*Julia function creation*)

(* ::Input::Initialization:: *)

ClearAll[MakeJuliaFunction];

Options[MakeJuliaFunction] = {"Parameters" -> {}, "Name" -> "kernel", "Prefix" -> "", "Body" -> ""};

MakeJuliaFunction[OptionsPattern[]] :=
    Module[
        {functionPrefix, functionName, functionParameters, idx, functionBody}
        ,
        (*Create prefixe for the function, e.g. static or such + the return value*)
        functionPrefix =
            If[OptionValue["Prefix"] === "",
                ""
                ,
                OptionValue["Prefix"] <> " "
            ];
        functionName = "function " <> OptionValue["Name"];
        functionParameters =
            "(" <>
                StringRiffle[
                    Map[
                        If[AssociationQ[#],
                            #["Name"]
                            ,
                            ToString[#]
                        ]&
                        ,
                        OptionValue["Parameters"]
                    ]
                    ,
                    ", "
                ] <> ")";
        (*create the body*)
        functionBody = StringReplace["\n" <> OptionValue["Body"] <> "\n", "\n\n" -> "\n"];
        functionBody = IndentCode[functionBody, 1];
        Return[functionPrefix <> functionName <> functionParameters <> functionBody <> "end"]
    ];

MakeJuliaFunction[expr_, OptionsPattern[]] :=
    Module[{newBody},
        newBody = OptionValue["Body"] <> "\n" <> JuliaCode[expr];
        MakeJuliaFunction @@ (Evaluate @ Join[{"Body" -> newBody}, Thread[Rule @@ {#, OptionValue[MakeJuliaFunction, #]}]& @ Keys[Options[MakeJuliaFunction]]])
    ];

(* ::Section:: *) 