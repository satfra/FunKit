(*Fortran*)

(* ::Subsection:: *)

(*Fortran code creation*)

(* ::Input::Initialization:: *)

FortranCode[equation_, name_:"result"] :=
    Module[{optList, interpObj, replacementObj, replacementNames, replacements, definitions, returnStatement},
        optList = $codeOptimizeFunctions;
        interpObj = Flatten @ Map[Cases[equation, #, Infinity]&, optList];
        replacementObj = Keys @ Select[Counts[interpObj], # > 1&];
        replacementNames = Table["repl" <> ToString[i], {i, 1, Length[replacementObj]}];
        replacements = Table[replacementObj[[i]] -> Symbol @ replacementNames[[i]], {i, 1, Length[replacementObj]}];
        definitions =
            If[Length[replacementObj] > 0,
                StringJoin[Table["real, parameter :: " <> ToString[replacementNames[[i]]] <> " = " <> ToString @ FortranForm[FullSimplify @ replacementObj[[i]]] <> "\n", {i, 1, Length[replacementObj]}]] <> "\n"
                ,
                ""
            ];
        returnStatement = name <> " = " <> ToString @ FortranForm[equation //. replacements];
        returnStatement = StringReplace[returnStatement, Map["\"" <> # <> "\"" -> #&, replacementNames]];
        definitions <> "\n" <> returnStatement
    ];

(* ::Subsection:: *)

(*Fortran function creation*)

(* ::Input::Initialization:: *)

ClearAll[MakeFortranFunction];

Options[MakeFortranFunction] = {"Parameters" -> {}, "Name" -> "kernel", "Prefix" -> "", "Body" -> ""};

MakeFortranFunction[OptionsPattern[]] :=
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
                        "real " <>
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

MakeFortranFunction[expr_, OptionsPattern[]] :=
    Module[{newBody},
        newBody = OptionValue["Body"] <> "\n" <> FortranCode[expr, OptionValue["Name"]];
        MakeFortranFunction @@ (Evaluate @ Join[{"Body" -> newBody}, Thread[Rule @@ {#, OptionValue[MakeFortranFunction, #]}]& @ Keys[Options[MakeFortranFunction]]])
    ];
