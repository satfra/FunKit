(**********************************************************************************
    Fortran.m -- Fortran code generation

    Public API:
      FortranCodeForm            -- Converts a Mathematica expression to Fortran syntax
      FortranCode                -- Generates optimized Fortran code from an expression
      MakeFortranFunction        -- Wraps expression/body in a Fortran function

    Internal:
      fortranFixNames            -- Renames optimizer variables to valid Fortran identifiers
      Uses IndentCode from Julia.m (shared helper)
**********************************************************************************)

FortranCodeForm[expr_] :=
    Module[{processedExpr, Fstr},
        (* Expand fmaGroup to arithmetic since Fortran has no fma intrinsic *)
        processedExpr = expr //. fmaGroup[a_, b_, c_] :> a * b + c;
        Fstr = ToString[System`FortranForm[processedExpr //. {
            E^x_ :> Global`tmp$$exp[x],
            Complex[re_, im_] :> Global`tmp$$cmplx[re, im]
        }]];
        StringReplace[Fstr,
        {"tmp$$exp" -> "exp", "tmp$$cmplx" -> "cmplx",
        "Sqrt" -> "sqrt", "Log" -> "log",
        "Sin" -> "sin", "Cos" -> "cos", "Tan" -> "tan", "Cot" -> "cot",
        "ArcSin" -> "asin", "ArcCos" -> "acos", "ArcTan" -> "atan", "ArcCot" -> "acot",
        "Sinh" -> "sinh", "Cosh" -> "cosh", "Tanh" -> "tanh", "Coth" -> "coth",
        "ArcSinh" -> "asinh", "ArcCosh" -> "acosh", "ArcTanh" -> "atanh", "ArcCoth" -> "acoth",
        "Pi" -> "acos(-1.0d0)"
        }]
    ];

(* Fortran identifiers must start with a letter. The optimizer produces names
   like _interp1, _cse1, _tran1, _result1, _den1 which are invalid. The sub-kernel
   accumulator _acc and the ReturnTransform placeholder postAcc<n> are also
   coerced. This function renames them by stripping any leading underscore and
   adding an "fk" prefix. *)
fortranFixNames[code_String] :=
    StringReplace[code, {
        "_" ~~ prefix:("interp"|"cse"|"tran"|"result"|"den") ~~ num:DigitCharacter.. :> "fk" <> prefix <> num,
        WordBoundary ~~ "_acc" ~~ WordBoundary :> "fkacc"
    }];

(* Generate double precision declarations for all optimizer variables in the code *)
fortranVarDeclarations[code_String] :=
    Module[{numbered, accVar, vars},
        numbered = Union @ StringCases[code, "fk" ~~ ("interp"|"cse"|"tran"|"result"|"den") ~~ DigitCharacter..];
        accVar = If[StringContainsQ[code, WordBoundary ~~ "fkacc" ~~ WordBoundary], {"fkacc"}, {}];
        vars = Join[numbered, accVar];
        If[Length[vars] === 0, "",
            "double precision :: " <> StringRiffle[vars, ", "] <> "\n"
        ]
    ];

(* ::Subsection:: *)

(*Fortran code creation*)

(* ::Input::Initialization:: *)

Options[FortranCode] = {"ReturnTransform" -> Identity};

FortranCode[equation_, name_:"kernel", OptionsPattern[]] :=
    Module[{transform, optimized, varNames, fortranFormatDefs, definitions, returnStatement, result},
        transform = OptionValue["ReturnTransform"];
        optimized = optimizeExpression[equation];
        varNames = getAllVarNames[optimized];
        fortranFormatDefs = Function[{defs},
            If[Length[defs] === 0, "",
                Module[{simplifiedExprs},
                    simplifiedExprs = parallelSimplify[defs[[All, 2]]];
                    StringJoin @ Table[
                        defs[[i, 1]] <> " = " <> FortranCodeForm[simplifiedExprs[[i]]] <> "\n",
                        {i, 1, Length[defs]}
                    ] <> "\n"
                ]
            ]
        ];
        (* Sub-kernel path *)
        If[TrueQ[optimized["UseSubKernels"]],
            Module[{subKernels, sharedDefs, sharedCode, allNames, subCode, accSym, accStr, accLine, wrappedReturn},
                sharedDefs = optimized["SharedDefinitions"];
                subKernels = optimized["SubKernels"];
                allNames = Join[
                    sharedDefs[[All, 1]],
                    Flatten @ Map[#["Definitions"][[All, 1]]&, subKernels]
                ];
                sharedCode = fortranFormatDefs[sharedDefs];
                sharedCode = stripQuotedNames[sharedCode, allNames];
                subCode = Table[
                    Module[{localDefs, termsCode},
                        localDefs = fortranFormatDefs[subKernels[[i]]["Definitions"]];
                        localDefs = stripQuotedNames[localDefs, allNames];
                        termsCode = FortranCodeForm[subKernels[[i]]["Terms"]];
                        termsCode = stripQuotedNames[termsCode, allNames];
                        "! subkernel " <> ToString[i] <> "\n" <>
                        localDefs <>
                        "_result" <> ToString[i] <> " = " <> termsCode <> "\n"
                    ],
                    {i, Length[subKernels]}
                ];
                accLine = "_acc = " <> StringRiffle[Table["_result" <> ToString[i], {i, Length[subKernels]}], " + "] <> "\n";
                accSym = Unique["postAcc"];
                accStr = ToString[accSym];
                wrappedReturn = StringReplace[FortranCodeForm[transform[accSym]], accStr -> "_acc"];
                result = sharedCode <> StringJoin[subCode] <> accLine <>
                    name <> " = " <> wrappedReturn;
                Return[fortranFixNames[result]]
            ]
        ];
        (* Standard path: definitions + return *)
        definitions = fortranFormatDefs[optimized["Definitions"]];
        definitions = stripQuotedNames[definitions, varNames];
        returnStatement = name <> " = " <> FortranCodeForm[transform[optimized["Expr"]]];
        returnStatement = stripQuotedNames[returnStatement, varNames];
        FunKitDebug[2, "Fortran definitions: ", definitions];
        FunKitDebug[2, "Fortran returnStatement: ", returnStatement];
        fortranFixNames[definitions <> returnStatement]
    ];

(* ::Subsection:: *)

(*Fortran function creation*)

(* ::Input::Initialization:: *)

ClearAll[MakeFortranFunction];

Options[MakeFortranFunction] = {"Parameters" -> {}, "Name" -> "kernel", "Prefix" -> "", "Body" -> "", "ReturnTransform" -> Identity};

MakeFortranFunction[OptionsPattern[]] :=
    Module[
        {functionName, functionParameters, paramNames, paramDeclarations, body, varDecls, functionBody}
        ,
        functionName = OptionValue["Name"];
        paramNames =
            Map[
                If[AssociationQ[#],
                    #["Name"]
                    ,
                    ToString[#]
                ]&
                ,
                OptionValue["Parameters"]
            ];
        functionParameters = "(" <> StringRiffle[paramNames, ", "] <> ")";
        paramDeclarations =
            If[Length[paramNames] > 0,
                StringJoin @ Map["double precision, intent(in) :: " <> # <> "\n"&, paramNames]
                ,
                ""
            ];
        body = OptionValue["Body"];
        (* Extract optimizer variable declarations from the body *)
        varDecls = fortranVarDeclarations[body];
        (*create the body: declarations first, then executable code*)
        functionBody = StringReplace[
            "\nimplicit none\n" <> paramDeclarations <> varDecls <> body <> "\n",
            "\n\n" -> "\n"
        ];
        functionBody = IndentCode[functionBody, 1];
        "double precision function " <> functionName <> functionParameters <> functionBody <> "end function " <> functionName
    ];

MakeFortranFunction[expr_, OptionsPattern[]] :=
    Module[{newBody},
        newBody = OptionValue["Body"] <> "\n" <> FortranCode[expr, OptionValue["Name"], "ReturnTransform" -> OptionValue["ReturnTransform"]];
        MakeFortranFunction @@ (Evaluate @ Join[{"Body" -> newBody}, Thread[Rule @@ {#, OptionValue[MakeFortranFunction, #]}]& @ Keys[Options[MakeFortranFunction]]])
    ];
