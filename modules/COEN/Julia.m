(**********************************************************************************
    Julia.m -- Julia code generation

    Public API:
      JuliaForm                  -- Converts a Mathematica expression to Julia syntax
      JuliaCode                  -- Generates optimized Julia code from an expression
      MakeJuliaFunction          -- Wraps expression/body in a Julia function

    Internal:
      IndentCode                 -- Indents a code string by a given level
                                    (used by MakeJuliaFunction, MakeFortranFunction)
**********************************************************************************)

JuliaForm[expr_] :=
    Module[{Fstr},
        Fstr = ToString[FortranForm[expr //. {
            E^x_ :> Global`tmp$$exp[x],
            Complex[re_, im_] :> Global`tmp$$complex[re, im],
            fmaGroup[a_, b_, c_] :> Global`tmp$$fma[a, b, c],
            (*ArcTan[x, y] is x-part first, Julia's atan(y, x) is y-part first: swap before the
              name map below, which would otherwise emit the arguments positionally.*)
            ArcTan[a_, b_] :> Global`tmp$$atan2[b, a]
        }]];
        StringReplace[Fstr,
        {a_ ~~ "(i)" -> a ~~ "[i]", a_ ~~ "(-1 + i)" -> a ~~ "[i-1]", a_ ~~ "(1 + i)" -> a ~~ "[i+1]", "**" -> "^", ".*" -> "*", ".+" -> "+", "Pi" -> "\[Pi]",
        "Sqrt" -> "sqrt",
        "Log" -> "log", "Exp" -> "exp", "tmp$$exp" -> "exp", "tmp$$complex" -> "complex",
        "tmp$$fma" -> "fma", "tmp$$atan2" -> "atan",
        "Sin" -> "sin", "Cos" -> "cos", "Tan" -> "tan", "Cot" -> "cot",
        "ArcSin" -> "asin", "ArcCos" -> "acos", "ArcTan" -> "atan", "ArcCot" -> "acot",
        "Sinh" -> "sinh", "Cosh" -> "cosh",  "Tanh" -> "tanh", "Coth" -> "coth",
        "ArcSinh" -> "asinh", "ArcCosh" -> "acosh", "ArcTanh" -> "atanh", "ArcCoth" -> "acoth"
        }]
    ];

IndentCode[code_String, level_Integer] :=
    Module[{ret = code, idx},
        For[idx = 1, idx <= level, idx++,
            ret = StringReplace[ret, "\n" -> "\n  "];
        ];
        (*Remove trailing whitespaces*)
        While[StringLength[ret] > 0 && StringTake[ret, {-1}] === " ", ret = StringTake[ret, StringLength[ret] - 1];];
        Return[ret];
    ]

(* ::Subsection:: *)

(*Julia code creation*)

(* ::Input::Initialization:: *)

Options[JuliaCode] = {"ReturnTransform" -> Identity};

JuliaCode[equation_, OptionsPattern[]] :=
    Module[{transform, optimized, varNames, juliaFormatDefs, definitions, returnStatement},
        transform = OptionValue["ReturnTransform"];
        optimized = optimizeExpression[equation];
        varNames = getAllVarNames[optimized];
        juliaFormatDefs = Function[{defs},
            If[Length[defs] === 0, "",
                Module[{simplifiedExprs},
                    simplifiedExprs = parallelSimplify[defs[[All, 2]]];
                    StringJoin @ Table[
                        defs[[i, 1]] <> " = " <> JuliaForm[simplifiedExprs[[i]]] <> "\n",
                        {i, 1, Length[defs]}
                    ] <> "\n"
                ]
            ]
        ];
        (* Sub-kernel pattern *)
        If[TrueQ[optimized["UseSubKernels"]],
            Module[{subKernels, sharedDefs, sharedCode, allNames, subCode, accSym, accStr, accLine, wrappedReturn},
                sharedDefs = optimized["SharedDefinitions"];
                subKernels = optimized["SubKernels"];
                allNames = Join[
                    sharedDefs[[All, 1]],
                    Flatten @ Map[#["Definitions"][[All, 1]]&, subKernels]
                ];
                sharedCode = juliaFormatDefs[sharedDefs];
                sharedCode = stripQuotedNames[sharedCode, allNames];
                subCode = Table[
                    Module[{localDefs, termsCode},
                        localDefs = juliaFormatDefs[subKernels[[i]]["Definitions"]];
                        localDefs = stripQuotedNames[localDefs, allNames];
                        termsCode = JuliaForm[subKernels[[i]]["Terms"]];
                        termsCode = stripQuotedNames[termsCode, allNames];
                        "# subkernel " <> ToString[i] <> "\n" <>
                        localDefs <>
                        "_result" <> ToString[i] <> " = " <> termsCode <> "\n"
                    ],
                    {i, Length[subKernels]}
                ];
                accLine = "_acc = " <> StringRiffle[Table["_result" <> ToString[i], {i, Length[subKernels]}], " + "] <> "\n";
                accSym = Unique["postAcc"];
                accStr = ToString[accSym];
                wrappedReturn = StringReplace[JuliaForm[transform[accSym]], accStr -> "_acc"];
                Return[
                    sharedCode <> StringJoin[subCode] <> accLine <>
                    "return " <> wrappedReturn
                ]
            ]
        ];
        (* Standard path: definitions + return *)
        definitions = juliaFormatDefs[optimized["Definitions"]];
        definitions = stripQuotedNames[definitions, varNames];
        returnStatement = "return " <> JuliaForm[transform[optimized["Expr"]]];
        returnStatement = stripQuotedNames[returnStatement, varNames];
        FunKitDebug[2, "Definitions: ", definitions];
        FunKitDebug[2, "returnStatement: ", returnStatement];
        definitions <> returnStatement
    ];

(* ::Subsection:: *)

(*Julia function creation*)

(* ::Input::Initialization:: *)

ClearAll[MakeJuliaFunction];

Options[MakeJuliaFunction] = {"Parameters" -> {}, "Name" -> "kernel", "Prefix" -> "", "Body" -> "", "ReturnTransform" -> Identity};

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
        newBody = OptionValue["Body"] <> "\n" <> JuliaCode[expr, "ReturnTransform" -> OptionValue["ReturnTransform"]];
        MakeJuliaFunction @@ (Evaluate @ Join[{"Body" -> newBody}, Thread[Rule @@ {#, OptionValue[MakeJuliaFunction, #]}]& @ Keys[Options[MakeJuliaFunction]]])
    ];

(* ::Section:: *) 