(*C++ formatting*)

Unprotect @ CExpression;

ClearAll[CExpression]

Get["SymbolicC`"]

UseCppPowr[True] :=
    Set[$CppPowr, True];

UseCppPowr[False] :=
    Set[$CppPowr, False];

$CppPowr = True;

$CppPrecision = 20;

CppForm[expr_] :=
    Internal`InheritedBlock[
        {processedExpr, nest, $MinPrecision = $CppPrecision, $MaxPrecision = $CppPrecision, CExpression}
        ,
        processedExpr = N[expr];
        nest := GenerateCode[CExpression[#]]&;
        (*associativity*)
        CExpression /: GenerateCode[CExpression[Times[a__, Plus[b_, c__], d__]]] := "(" <> nest[Times[a]] <> ") * (" <> nest[Plus[b, c]] <> ") * (" <> nest[Times[d]] <> ")";
        CExpression /: GenerateCode[CExpression[Times[Plus[b_, c__], d__]]] := "(" <> nest[Plus[b, c]] <> ") * (" <> nest[Times[d]] <> ")";
        (*recursion for + and * *)
        CExpression /: GenerateCode[CExpression[Plus[a_, b__]]] := nest[a] <> " + " <> nest[Plus[b]];
        CExpression /: GenerateCode[CExpression[Times[a_, b__]]] := "(" <> nest[a] <> ") * (" <> nest[Times[b]] <> ")";
        CExpression /: GenerateCode[CExpression[Times[-1, b_, a__]]] /; Head[b] =!= Plus := "(-(" <> nest[b] <> "))";
        (*functions*)
        CExpression /: GenerateCode[CExpression[a_[args___]]] := nest[a] <> "(" <> StringJoin @ StringRiffle[nest /@ {args}, ", "] <> ")";
        (*number conversion*)
        CExpression /: GenerateCode[CExpression[I]] := "complex<double>(0,1)";
        CExpression /: GenerateCode[CExpression[a_Real]] :=
            ToString[
                NumberForm[
                    N[a, $CppPrecision]
                    ,
                    $CppPrecision
                    ,
                    NumberFormat ->
                        (
                            If[#3 === "",
                                #1
                                ,
                                Row[{#1, "e", #3}]
                            ]&
                        )
                ]
            ];
        CExpression /: GenerateCode[CExpression[Rational[a_, b_]]] := nest[N[a / b, $CppPrecision]];
        CExpression /: GenerateCode[CExpression[Complex[r_, i_]]] := "complex<double>(" <> nest[r] <> "," <> nest[i] <> ")";
        CExpression /: GenerateCode[CExpression[a_]] /; NumericQ[a] && Not @ IntegerQ[a] := nest[N[a, $CppPrecision]];
        CExpression /: GenerateCode[CExpression[Re[v_]]] := "real(" <> nest[v] <> ")";
        CExpression /: GenerateCode[CExpression[Im[v_]]] := "imag(" <> nest[v] <> ")";
        CExpression /: GenerateCode[CExpression[Conjugate[v_]]] := "conj(" <> nest[v] <> ")";
        CExpression /: GenerateCode[CExpression[Sign[v_]]] := "sign(" <> nest[v] <> ")";
        (*Powers and such*)
        CExpression /: GenerateCode[CExpression[Sqrt[arg_]]] := "sqrt(" <> nest[arg] <> ")";
        If[$CppPowr,
            CExpression /: GenerateCode[CExpression[Power[a_, b_Integer]]] := "powr<" <> ToString[b] <> ">(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Power[a_, b_ /; Element[b + 1/2, Integers]]]] :=
                If[b === 1/2,
                    "sqrt(" <> nest[a] <> ")"
                    ,
                    "sqrt(powr<" <> nest[2 b] <> ">(" <> nest[a] <> "))"
                ];
            ,
            CExpression /: GenerateCode[CExpression[Power[a_, b_Integer]]] := "pow(" <> nest[a] <> ", " <> ToString[b] <> ")";
        ];
        CExpression /: GenerateCode[CExpression[Power[a_, b_]]] := "pow(" <> nest[a] <> "," <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Power[E, b_]]] := "exp(" <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Power[E, b_Integer]]] := "exp(" <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Exp[a_]]] := "exp(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Exp[a_] - 1]] := "expm1(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[1 - Exp[a_]]] := "-expm1(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Plus[Exp[a_], -1, c__]]] := "expm1(" <> nest[a] <> ") + " <> nest[Plus[c]];
        CExpression /: GenerateCode[CExpression[Plus[-Exp[a_], 1, c__]]] := "(-expm1(" <> nest[a] <> ")) + " <> nest[Plus[c]];
        CExpression /: GenerateCode[CExpression[Log[a_]]] := "log(" <> nest[a] <> ")";
        (*trigonometric*)
        CExpression /: GenerateCode[CExpression[Sin[a_]]] := "sin(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Cos[a_]]] := "cos(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Tan[a_]]] := "tan(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Cot[a_]]] := "powr<-1>(tan(" <> nest[a] <> "))";
        CExpression /: GenerateCode[CExpression[ArcSin[a_]]] := "asin(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCos[a_]]] := "acos(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcTan[a_]]] := "atan(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcTan[a_, b_]]] := "atan2(" <> nest[a] <> ", " <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCot[a_]]] := "atan(powr<-1>(" <> nest[a] <> "))";
        CExpression /: GenerateCode[CExpression[Sinh[a_]]] := "sinh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Cosh[a_]]] := "cosh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Tanh[a_]]] := "tanh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Coth[a_]]] := "powr<-1>(tanh(" <> nest[a] <> "))";
        CExpression /: GenerateCode[CExpression[ArcSinh[a_]]] := "asinh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCosh[a_]]] := "acosh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcTanh[a_]]] := "atanh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCoth[a_]]] := "atanh(powr<-1>(" <> nest[a] <> "))";
        (*min, max, abs*)
        CExpression /: GenerateCode[CExpression[Abs[a_]]] := "abs(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Min[a_, b_]]] := "min(" <> nest[a] <> "," <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Min[a_, b_, c__]]] := "min({" <> nest[a] <> "," <> nest[b] <> StringRiffle[Map[nest, {c}], ","] "})";
        CExpression /: GenerateCode[CExpression[Max[a_, b_]]] := "max(" <> nest[a] <> "," <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Max[a_, b_, c__]]] := "max({" <> nest[a] <> "," <> nest[b] <> StringRiffle[Map[nest, {c}], ","] "})";
        Return[ToCCodeString[CExpression[processedExpr]]];
    ];

clangFormatExists = Quiet[RunProcess[{"clang-format", "--help"}]] =!= $Failed;

CreateClangFormat[path_:"./"] :=
    If[Not @ FileExistsQ[path <> ".clang-format"],
        Export[path <> "/.clang-format",                                                                                                                                                                                                             "BasedOnStyle: LLVM
UseTab: Never
IndentWidth: 2
TabWidth: 2
BreakBeforeBraces: Linux
AllowShortIfStatementsOnASingleLine: true
IndentCaseLabels: false
ColumnLimit: 120
AccessModifierOffset: -2
NamespaceIndentation: All
AllowShortEnumsOnASingleLine: true
", "Text"]
    ];

WriteCodeToFile[fileName_String, expression_String] :=
    Module[{tmpfileName},
        tmpfileName = fileName <> ".tmpcode";
        Export[tmpfileName, expression, "Text"];
        If[clangFormatExists,
            CreateClangFormat[];
            RunProcess[$SystemShell, All, "clang-format " <> tmpfileName <> " > " <> tmpfileName <> "_formatted && mv " <> tmpfileName <> "_formatted " <> tmpfileName];
        ];
        If[FileExistsQ[fileName],
            If[Import[fileName, "Text"] == Import[tmpfileName, "Text"],
                Print[fileName <> " unchanged"];
                RunProcess[$SystemShell, All, "rm " <> tmpfileName]
                ,
                Print["Exported to " <> fileName];
                RunProcess[$SystemShell, All, "mv " <> tmpfileName <> " " <> fileName]
            ]
            ,
            Print["Exported to " <> fileName];
            RunProcess[$SystemShell, All, "mv " <> tmpfileName <> " " <> fileName]
        ]
    ];

FormatCode[expression_String] :=
    Module[{tmpfileName1, tmpfileName2, output},
        tmpfileName1 = "/tmp/in_" <> makeTemporaryFileName[];
        tmpfileName2 = "/tmp/out_" <> makeTemporaryFileName[];
        Export[tmpfileName1, expression, "Text"];
        If[clangFormatExists,
            (*RunProcess[$SystemShell, All, "rm /tmp/.clang-format"];*)
            CreateClangFormat["/tmp/"];
            RunProcess[$SystemShell, All, "clang-format " <> tmpfileName1 <> " > " <> tmpfileName2];
        ];
        If[FileExistsQ[tmpfileName2],
            output = Import[tmpfileName2, "Text"];
            RunProcess[$SystemShell, All, "rm " <> tmpfileName1 <> " " <> tmpfileName2];
            Return[output];
        ];
        Return[expression]
    ];

formatFORMCode[expr_String] :=
    Module[{start, res, pres, idx, maxW, repl},
        start = StringPosition[expr, "\n"];
        start =
            If[Length[start] <= 1,
                1
                ,
                start[[2, 1]]
            ];
        res = StringTake[expr, {start, -1}];
        (*operation replacements*)
        While[
            pres =!= res
            ,
            pres = res;
            res = StringReplace[res, {Shortest["pow(" ~~ (arg1__) ~~ "," ~~ (arg2 : (DigitCharacter... | "-" ~~ (DigitCharacter...))) ~~ ")"] /; balancedBracesQ[arg1] && StringFreeQ[arg1, ";"] :> "powr<" ~~ arg2 ~~ ">(" ~~ arg1 ~~ ")", Shortest["pow(" ~~ (arg1__) ~~ "," ~~ "1./2." ~~ ")"] /; balancedBracesQ[arg1] && StringFreeQ[arg1, ";"] :> "sqrt(" ~~ arg1 ~~ ")", " " -> ""}];
        ];
        (*turn the buffer into a list of definitions of variables*)
        maxW = Max[Map[ToExpression @ StringTake[#, {3, -2}]&, StringCases[res, Shortest["w[" ~~ (arg1__ /; balancedRBracesQ[arg1]) ~~ "]"]]]];
        For[idx = 1, idx <= maxW, idx++,
            res = StringReplacePart[res, "auto _tmp" <> ToString[idx] <> "", StringPosition[res, "w[" <> ToString[idx] <> "]", 1]];
        ];
        res = StringReplace[res, {Shortest["w[" ~~ (arg1__ /; balancedRBracesQ[arg1]) ~~ "]"] :> "_tmp" ~~ arg1 ~~ "", "expr=" -> "return ", "\n" -> ""}];
        res = FormatCode[res];
        (*Get rid of unecessary copies*)
        repl = Map[StringReplace[#, "auto _tmp" ~~ a__ ~~ " = " ~~ b__ ~~ ";" /; hasNoOperators[b] :> "_tmp" ~~ a ~~ "->" ~~ b]&, Select[StringSplit[res, "\n"], StringMatchQ[#, "auto _tmp" ~~ a__ ~~ "=" ~~ b__ ~~ ";" /; hasNoOperators[b]]&]];
        repl = Map[((a_ /; MatchQ[a, "(" | " " | "-"]) ~~ #[[1]] ~~ (b_ /; MatchQ[b, ")" | " " | ";"]) :> a ~~ #[[2]] ~~ b&) @ StringSplit[#, "->"]&, repl];
        res = StringJoin[Select[StringSplit[res, "\n"], Not @ StringMatchQ[#, "auto _tmp" ~~ a__ ~~ "=" ~~ b__ /; hasNoOperators[b]]&]];
        res = StringReplace[res, repl];
        Return[res];
    ];

FORMCode[expr_] :=
    Module[{origVars, tmpfileName, import},
        origVars = FormTracer`GetExtraVars[];
        tmpfileName = "/tmp/FO_" <> makeTemporaryFileName[];
        FormTracer`AddExtraVars @@ GetAllCustomSymbols[expr];
        FormTracer`FormTrace[expr // Rationalize, {}, {}, {tmpfileName, "O4"}];
        FormTracer`DefineExtraVars[origVars];
        import = Import[tmpfileName, "Text"];
        RunProcess[$SystemShell, All, "rm " <> tmpfileName];
        import // formatFORMCode
    ];

(*C++ code creation*)

(* ::Input::Initialization:: *)

(* ::Input::Initialization:: *)

CppCode[equation_] :=
    Module[{optList, interpObj, replacementObj, replacementNames, replacements, replacementsFS, definitions, returnStatement},
        optList = $codeOptimizeFunctions;
        interpObj = Flatten @ Map[Cases[equation, #, Infinity]&, optList];
        replacementObj = Select[Counts[interpObj], # > 1&];
        (*We weigh function calls (potentially fetching global memory)*)
        (*much stronger to make sure these are preferentially cached.*)
        replacementObj =
            AssociationMap[
                If[MatchQ[#[[1]], $codeOptimizeInterps[[1]]],
                    #[[1]] -> #[[2]] * 1000
                    ,
                    #[[1]] -> #[[2]]
                ]&
                ,
                replacementObj
            ];
        replacementObj = TakeLargest[replacementObj, Min[$availableRegisters, Length[replacementObj]]];
        replacementObj = Keys[replacementObj];
        replacementNames = Table["_repl" <> ToString[i], {i, 1, Length[replacementObj]}];
        replacements = Table[replacementObj[[i]] -> replacementNames[[i]], {i, 1, Length[replacementObj]}];
        replacementsFS = Table[FullSimplify[replacementObj[[i]]] -> replacementNames[[i]], {i, 1, Length[replacementObj]}];
        FunKitDebug[2, "Replacements: ", replacements];
        FunKitDebug[2, "ReplacementsFS: ", replacementsFS];
        definitions =
            If[Length[replacementObj] > 0,
                StringJoin[Table["const auto " <> ToString[replacementNames[[i]]] <> " = " <> CppForm[FullSimplify @ replacementObj[[i]] //. Reverse[replacements[[ ;; i - 1]]] //. Reverse[replacementsFS[[ ;; i - 1]]]] <> ";\n", {i, 1, Length[replacementObj]}]] <> "\n"
                ,
                ""
            ];
        definitions = StringReplace[definitions, Map["\"" <> # <> "\"" -> #&, replacementNames]];
        FunKitDebug[2, "Definitions: ", definitions];
        replacements = Reverse[replacements];
        replacementsFS = Reverse[replacementsFS];
        returnStatement = " return " <> CppForm[equation //. replacements //. replacementsFS] <> ";";
        FunKitDebug[2, "returnStatement: ", returnStatement];
        returnStatement = StringReplace[returnStatement, Map["\"" <> # <> "\"" -> #&, replacementNames]];
        definitions <> returnStatement
    ];

(* ::Subsection:: *)

(*C++ function creation*)

(* ::Subsubsection:: *)

(*Helper functions*)

(* ::Input::Initialization:: *)

makeCppTemplateParameter[n_] :=
    "typename T" <> ToString[n];

makeCppParameter[t_Association, n_] :=
    Module[{ret},
        ret =
            If[KeyFreeQ[t, "Const"] || Not @ t["Const"],
                ""
                ,
                "const "
            ];
        ret =
            ret <>
                If[KeyFreeQ[t, "Type"] || t["Type"] === "template",
                    "T" <> ToString[n]
                    ,
                    t["Type"]
                ];
        ret =
            ret <>
                If[KeyExistsQ[t, "Reference"] && t["Reference"],
                    "& "
                    ,
                    " "
                ];
        ret = ret <> t["Name"];
        Return[ret];
    ];

prepParam[it_String] :=
    <|"Type" -> "auto", "Reference" -> True, "Name" -> it, "Const" -> True|>;

prepParam[it_Association] :=
    Module[{res = it},
        If[KeyFreeQ[res, "Const"],
            AssociateTo[res, "Const" -> True]
        ];
        If[KeyFreeQ[res, "Reference"],
            AssociateTo[res, "Reference" -> True]
        ];
        Return[res];
    ];

prepParam[it_] :=
    (
        Print[it, " is not a valid C++ parameter!"];
        Abort[]
    );

(* ::Input::Initialization:: *)

MakeParameterString[it_] :=
    Module[{ret, t = prepParam @ it},
        ret =
            If[KeyFreeQ[t, "Const"] || Not @ t["Const"],
                ""
                ,
                "const "
            ];
        ret = ret <> t["Type"];
        ret =
            ret <>
                If[KeyExistsQ[t, "Reference"] && t["Reference"],
                    "& "
                    ,
                    " "
                ];
        ret = ret <> t["Name"];
        Return[ret];
    ]

(* ::Subsubsection:: *)

(*Creating functions*)

(* ::Input::Initialization:: *)

ClearAll[MakeCppFunction];

Options[MakeCppFunction] = {"Return" -> "auto", "Parameters" -> {}, "Name" -> "function", "Prefix" -> "", "Suffix" -> "", "CodeParser" -> "Cpp", "Body" -> "", "Class" -> "", "Templates" -> {}};

MakeCppFunction[OptionsPattern[]] :=
    Module[{functionPrefix, functionSuffix, functionName, functionParameters, functionTemplates, idx, functionBody, parameters},
        FunKitDebug[1, "Preparing Cpp function..."];
        (*Create prefixes for the function, e.g. static or such + the return value*)
        functionPrefix = OptionValue["Prefix"];
        functionPrefix = functionPrefix <> " " <> OptionValue["Return"] <> " ";
        functionSuffix =
            If[OptionValue["Suffix"] =!= "",
                " " <> OptionValue["Suffix"] <> " "
                ,
                ""
            ];
        functionName =
            If[OptionValue["Class"] === "",
                    ""
                    ,
                    OptionValue["Class"] <> "::"
                ] <> OptionValue["Name"];
        parameters = prepParam /@ OptionValue["Parameters"];
        (*Create both a template list and a parameter list*)
        functionTemplates =
            If[Length[Select[parameters, KeyFreeQ[#, "Type"] || #["Type"] === "template"&]] === 0,
                ""
                ,
                StringRiffle[Pick[Table[makeCppTemplateParameter[idx], {idx, 1, Length[parameters]}], Table[KeyFreeQ[parameters[[idx]], "Type"] || parameters[[idx]]["Type"] === "template", {idx, 1, Length[parameters]}]], ", "]
            ];
        functionTemplates =
            If[OptionValue["Templates"] === {},
                    ""
                    ,
                    "typename " <> StringRiffle[OptionValue["Templates"], ", typename "]
                ] <>
                If[functionTemplates =!= "",
                    ", " <> functionTemplates
                    ,
                    ""
                ];
        functionTemplates =
            If[functionTemplates === "",
                ""
                ,
                "template<" <> functionTemplates <> ">\n"
            ];
        functionParameters = "(" <> StringRiffle[Table[makeCppParameter[parameters[[idx]], idx], {idx, 1, Length[parameters]}], ", "] <> ")";
        (*create the body*)
        functionBody =
            If[OptionValue["Body"] === None,
                ";"
                ,
                StringReplace["{\n" <> OptionValue["Body"] <> "\n}", "\n\n" -> ""]
            ];
        FunKitDebug[2, "  Prepared Cpp function; now parsing code."];
        Return[FormatCode[functionTemplates <> functionPrefix <> functionName <> functionParameters <> functionSuffix <> "\n" <> functionBody]]
    ];

MakeCppFunction[expr_, OptionsPattern[]] :=
    Module[{codeParser, newBody},
        codeParser =
            If[OptionValue["CodeParser"] === "FORM",
                FORMCode
                ,
                CppCode
            ];
        newBody = OptionValue["Body"] <> "\n" <> codeParser[expr];
        MakeCppFunction @@ (Evaluate @ Join[{"Body" -> newBody}, Thread[Rule @@ {#, OptionValue[MakeCppFunction, #]}]& @ Keys[Options[MakeCppFunction]]])
    ];

(* ::Subsection:: *)

(*Creating Headers*)

(* ::Input::Initialization:: *)

Options[MakeCppClass] = {"TemplateTypes" -> {}, "MembersPublic" -> {}, "MembersPrivate" -> {}, "MembersProtected" -> {}, "Name" -> "Class", "Bases" -> {}};

MakeCppClass[OptionsPattern[]] :=
    Module[
        {classPrefix, classSuffix, className, classParameters, classTemplates, codeParser, classBody}
        ,
        (*Create prefixe for the class, e.g. static or such + the return value*)
        classPrefix =
            If[Length[OptionValue["TemplateTypes"]] > 0,
                "template<" ~~ StringRiffle[Map["typename " ~~ #&, OptionValue["TemplateTypes"]], ", "] ~~ ">\n"
                ,
                ""
            ];
        classPrefix = classPrefix <> "class ";
        classSuffix =
            If[Length[OptionValue["Bases"]] > 0,
                " : public " ~~ StringRiffle[OptionValue["Bases"], ", public "]
                ,
                ""
            ];
        className = OptionValue["Name"];
        (*create the body*)
        classBody =
            "{\n" <>
                If[Length[OptionValue["MembersPublic"]] > 0,
                    "public: " <> StringRiffle[OptionValue["MembersPublic"], "\n\n"]
                    ,
                    ""
                ] <>
                If[Length[OptionValue["MembersProtected"]] > 0,
                    "protected: " <> StringRiffle[OptionValue["MembersProtected"], "\n\n"]
                    ,
                    ""
                ] <>
                If[Length[OptionValue["MembersPrivate"]] > 0,
                    "private: " <> StringRiffle[OptionValue["MembersPrivate"], "\n\n"]
                    ,
                    ""
                ] <> "\n};";
        StringReplace[classBody, {";;" -> ";"}];
        Return[FormatCode[classPrefix <> className <> classSuffix <> "\n" <> classBody]]
    ];

(* ::Input::Initialization:: *)

Options[MakeCppHeader] = {"Includes" -> {}, "Body" -> {}};

MakeCppHeader[OptionsPattern[]] :=
    Module[
        {headerPrefix, headerIncludes, headerBody}
        ,
        (*Create prefixe for the header, e.g. static or such + the return value*)
        headerPrefix = "#pragma once\n";
        headerIncludes = StringRiffle[Map["#include \"" ~~ # ~~ "\""&, OptionValue["Includes"]], "\n"] <> "\n";
        (*create the body*)
        headerBody = StringRiffle[OptionValue["Body"], "\n"];
        Return[FormatCode[headerPrefix <> "\n" <> headerIncludes <> "\n" <> headerBody]];
    ];

(* ::Input::Initialization:: *)

Options[MakeCppBlock] = {"Includes" -> {}, "Body" -> {}, "Namespace" -> ""};

MakeCppBlock[OptionsPattern[]] :=
    Module[
        {sourcePrefix, sourcePostfix, sourceIncludes, sourceBody}
        ,
        (*Create prefixe for the source, e.g. static or such + the return value*)
        sourcePrefix =
            If[OptionValue["Namespace"] =!= "",
                "namespace " <> OptionValue["Namespace"] <> "\n{\n"
                ,
                ""
            ];
        sourceIncludes =
            If[OptionValue["Includes"] =!= {},
                StringRiffle[Map["#include \"" ~~ # ~~ "\""&, OptionValue["Includes"]], "\n"] <> "\n\n"
                ,
                ""
            ];
        (*create the body*)
        sourceBody = StringRiffle[OptionValue["Body"], "\n"] <> "\n";
        sourcePostfix =
            If[OptionValue["Namespace"] =!= "",
                "}"
                ,
                ""
            ];
        Return[FormatCode[sourcePrefix <> sourceIncludes <> sourceBody <> sourcePostfix]];
    ];

(* ::Input::Initialization:: *)

$DefaultRegulatorDefinitions = "
static __forceinline__ __device__ __host__ auto RB(const auto k2, const auto p2) { return REG::RB(k2, p2); }
static __forceinline__ __device__ __host__ auto RF(const auto k2, const auto p2) { return REG::RF(k2, p2); }

static __forceinline__ __device__ __host__ auto RBdot(const auto k2, const auto p2) { return REG::RBdot(k2, p2); }
static __forceinline__ __device__ __host__ auto RFdot(const auto k2, const auto p2) { return REG::RFdot(k2, p2); }

static __forceinline__ __device__ __host__ auto dq2RB(const auto k2, const auto p2) { return REG::dq2RB(k2, p2); }
static __forceinline__ __device__ __host__ auto dq2RF(const auto k2, const auto p2) { return REG::dq2RF(k2, p2); }
";

Options[CreateKernelClass] = {"integrationVariables" -> {}, "parameters" -> {}, "CodeParser" -> "FORM", "PrivateDefinitions" -> $DefaultRegulatorDefinitions, "integrandBody" -> "", "constantBody" -> ""};

CreateKernelClass[name_String, integrand_, constant_:0, OptionsPattern[]] :=
    Module[{ret, parameters, parametersIntegrand},
        parameters = OptionValue["parameters"];
        parameters = Map[KeyDrop[#, {"Type"}]&, parameters];
        parametersIntegrand = Join[Map[<|"Name" -> #|>&, OptionValue["integrationVariables"]], parameters];
        ret = MakeCppHeader["Body" -> {MakeCppClass["TemplateTypes" -> {"REG"}, "Name" -> name, "MembersPublic" -> {MakeCppFunction[integrand, "parameters" -> parametersIntegrand, "CodeParser" -> OptionValue["CodeParser"], "Name" -> "kernel", "static" -> True, "CUDA" -> True, "const" -> False, "return" -> "auto", "body" -> OptionValue["integrandBody"]], MakeCppFunction[constant, "parameters" -> parameters, "CodeParser" -> "Cpp", "Name" -> "constant", "static" -> True, "CUDA" -> True, "const" -> False, "return" -> "auto", "body" -> OptionValue["constantBody"]]}, "MembersPrivate" -> {OptionValue["PrivateDefinitions"]}]}];
        Return[ret]
    ]
