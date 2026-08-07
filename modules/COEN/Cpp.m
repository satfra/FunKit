(**********************************************************************************
    Cpp.m -- C++ code generation and formatting

    Public API:
      CppForm                    -- Converts a Mathematica expression to C++ syntax
      CppCode                    -- Generates optimized C++ code from an expression
      CppCodeFORM                -- Generates C++ code via FORM simplification
      MakeCppFunction            -- Creates a C++ function definition string
      MakeCppClass               -- Creates a C++ class definition string
      MakeCppHeader              -- Creates a C++ header file string
      MakeCppBlock               -- Creates a C++ source block with namespace
      CreateKernelClass          -- Creates a full GPU kernel class (header + integrand)
      WriteCodeToFile            -- Exports code to file with clang-format formatting
      FormatCppCode              -- Formats C++ code via clang-format
      FUseCppPowr                -- Toggles powr<N> template vs pow() usage
      MakeParameterString        -- Generates full parameter declaration string

    Internal:
      clangFormatExists          -- Whether clang-format is available on PATH
                                    (used by WriteCodeToFile, FormatCppCode)
      CreateClangFormat          -- Creates .clang-format config file
                                    (used by WriteCodeToFile, FormatCppCode)
      wrapLargeStatementsForClangFormat -- Wraps large statements with clang-format off
                                    (used by WriteCodeToFile, FormatCppCode)
      fixClangFormatOffIndentation -- Re-indents clang-format off sections
                                    (used by WriteCodeToFile, FormatCppCode)
      formatFORMCode             -- Converts FORM Fortran output to C++ code
                                    (used by CppCodeFORM)
      makeCppTemplateParameter   -- Generates "typename TN" string
                                    (used by MakeCppFunction)
      makeCppParameter           -- Generates parameter string from Association
                                    (used by MakeCppFunction)
      prepParam                  -- Normalizes parameter spec to Association
                                    (used by MakeCppFunction, MakeParameterString)

    Variables:
      $CppPowr                   -- Whether to use powr<N> templates (default True)
      $CppPrecision              -- Numeric precision for C++ output (default 20)
      $DefaultRegulatorDefinitions -- Default regulator GPU function definitions
**********************************************************************************)

Unprotect @ CExpression;

ClearAll[CExpression]

Get["SymbolicC`"]

FUseCppPowr[True] :=
    Set[$CppPowr, True];

FUseCppPowr[False] :=
    Set[$CppPowr, False];

$CppPowr = True;

(* Formatter selection: default to the fast in-process pretty-printer; opt into
   clang-format for exact LLVM-style output (see formatCppInProcess / formatViaClangFormat). *)

$FUseClangFormat = False;

FSetUseClangFormat[b_?BooleanQ] :=
    Set[$FUseClangFormat, b];

FSetUseClangFormat[___] :=
    (
        Message[FunKit::invalidArguments, FSetUseClangFormat];
        Abort[]
    );

$CppPrecision = 20;

Options[CppForm] = {"Format" -> False};

Options[cppFormEmit] = {"Format" -> False};

(* The printer is split into an INSTALL and an EMIT.

   withCppFormRules installs the CExpression rules below -- about fifty of them, plus nest and
   factorCode -- into an Internal`InheritedBlock. cppFormEmit assumes they are already installed
   and just renders one expression. CppForm is the two composed, so its public contract is
   unchanged; callers that render many expressions in a row (formatDefinitions) wrap the whole
   batch in withCppFormRules once and call cppFormBatched per expression instead.

   The split exists because the install is a fixed per-call cost -- measured at 0.67 s of
   CppForm's 1.17 s over the 2447 calls a large split kernel makes, i.e. more than half of the
   pass was reinstalling the same rules.

   $CppPowr, $codeFastMath and $codePrecision are read at INSTALL time (they select which rules
   get defined), so a batch sees the values they had when the batch opened. Nothing mutates them
   mid-emission. withCppFormRules nests safely: an inner InheritedBlock inherits the outer's
   already-installed rules, so a stray CppForm inside a batch costs time, never correctness. *)

SetAttributes[withCppFormRules, HoldFirst];

CppForm[expr_, opts : OptionsPattern[]] :=
    withCppFormRules[cppFormBatched[expr, opts]];

(* Profiled emit for callers already inside withCppFormRules. *)

cppFormBatched[expr_, opts : OptionsPattern[cppFormEmit]] :=
    (
        If[TrueQ[$ProfileCodegenOn], $ProfileCgCppFormCount++];
        cgTimed[$ProfileCgCppForm, cppFormEmit[expr, opts]]
    );

withCppFormRules[body_] :=
    Internal`InheritedBlock[
        {nest, factorCode, $MinPrecision = $CppPrecision, $MaxPrecision = $CppPrecision, CExpression}
        ,
        nest := GenerateCode[CExpression[#]]&;
        (*a factor of a product: a sum must be wrapped in parentheses, since * binds tighter than + in C++*)
        factorCode[p_Plus] := "(" <> nest[p] <> ")";
        factorCode[x_] := nest[x];
        (*powr*)
        If[$CppPowr,
            CExpression /: GenerateCode[CExpression[Power[a_, b_Integer]]] := "powr<" <> ToString[b] <> ">(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Power[a_, b_ /; Element[b + 1/2, Integers]]]] :=
                If[b === 1/2,
                    "sqrt(" <> nest[a] <> ")"
                    ,
                    "sqrt(powr<" <> ToString[2 b] <> ">(" <> nest[a] <> "))"
                ];
            ,
            CExpression /: GenerateCode[CExpression[Power[a_, b_Integer]]] := "pow(" <> nest[a] <> ", " <> ToString[b] <> ")";
        ];
        (*FMA*)
        CExpression /: GenerateCode[CExpression[fmaGroup[a_, b_, c_]]] := "fma(" <> nest[a] <> ", " <> nest[b] <> ", " <> nest[c] <> ")";
        (*products: map factorCode over every factor at once and join with " * ", so each sum factor is
          parenthesized. We must NOT peel one factor and recurse via nest[Times[rest]]: a single-argument
          Times collapses (Times[Plus[..]] -> Plus[..]), routing a trailing sum to the Plus rule and
          dropping its parentheses — the FUNKIT_KERNEL_PRINTER_BUG. The patterns require >= 2 factors
          (f1_, frest__): Times has the OneIdentity attribute, so Times[factors__] would also match a lone
          atom as Times[atom] and recurse forever via factorCode -> nest -> GenerateCode.*)
        CExpression /: GenerateCode[CExpression[Times[r_Real /; r == -1, a__]]] := "-" <> StringRiffle[factorCode /@ {a}, " * "];
        CExpression /: GenerateCode[CExpression[Times[f1_, frest__]]] := StringRiffle[factorCode /@ {f1, frest}, " * "];
        (*sums: render all terms at once and join with sign merging ("+ -x" -> "- x").
          Must NOT peel one term and recurse via nest[Plus[rest]]: that costs one evaluator
          recursion level per term, and the decltype leaf sums of large split kernels have
          thousands of terms — past $RecursionLimit, which silently corrupts the output.*)
        CExpression /: GenerateCode[CExpression[Plus[a_, b__]]] :=
            Module[{parts = Map[nest, {a, b}]},
                First[parts] <> StringJoin @ Map[
                    If[StringStartsQ[#, "-"],
                        " - " <> StringDrop[#, 1]
                        ,
                        " + " <> #
                    ]&
                    ,
                    Rest[parts]
                ]
            ];
        (*functions*)
        CExpression /: GenerateCode[CExpression[a_[args___]]] := nest[a] <> "(" <> StringJoin @ StringRiffle[nest /@ {args}, ", "] <> ")";
        (*string placeholders — CSE variable names are Mathematica strings; output as bare identifiers*)
        CExpression /: GenerateCode[CExpression[a_String]] := a;
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
        CExpression /: GenerateCode[CExpression[a_Integer]] := ToString[a] <> ".";
        CExpression /: GenerateCode[CExpression[a_]] /; NumericQ[a] && Not @ IntegerQ[a] := nest[N[a, $CppPrecision]];
        CExpression /: GenerateCode[CExpression[Re[v_]]] := "real(" <> nest[v] <> ")";
        CExpression /: GenerateCode[CExpression[Im[v_]]] := "imag(" <> nest[v] <> ")";
        CExpression /: GenerateCode[CExpression[Conjugate[v_]]] := "conj(" <> nest[v] <> ")";
        CExpression /: GenerateCode[CExpression[Sign[v_]]] := "sign(" <> nest[v] <> ")";
        (*Powers and such*)
        CExpression /: GenerateCode[CExpression[Sqrt[arg_]]] := "sqrt(" <> nest[arg] <> ")";
        CExpression /: GenerateCode[CExpression[cppExp[a_]]] := "exp(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Exp[a_]]] := "exp(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Power[a_, b_]]] := "pow(" <> nest[a] <> "," <> nest[b] <> ")";
        (* expm1(x) = exp(x)-1, more accurate near x=0. Match the literal -1./1. that the
           upstream N[] produces (the original literal integer -1/1 never fired post-N).
           IMPORTANT: use literal reals, NOT a condition like m_/;m==-1 — a condition forces
           an O(#terms) check on every Plus node (orderless), which is catastrophically slow on
           large sums; literal parts are matched by fast indexed lookup. *)
        CExpression /: GenerateCode[CExpression[Plus[cppExp[a_], -1.]]] := "expm1(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Plus[1., Times[-1., cppExp[a_]]]]] := "-expm1(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Plus[cppExp[a_], -1., c__]]] := "expm1(" <> nest[a] <> ") + " <> nest[Plus[c]];
        CExpression /: GenerateCode[CExpression[Plus[Times[-1., cppExp[a_]], 1., c__]]] := "(-expm1(" <> nest[a] <> ")) + " <> nest[Plus[c]];
        CExpression /: GenerateCode[CExpression[Log[a_]]] := "log(" <> nest[a] <> ")";
        (*trigonometric*)
        CExpression /: GenerateCode[CExpression[Sin[a_]]] := "sin(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Cos[a_]]] := "cos(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Tan[a_]]] := "tan(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Cot[a_]]] := If[$CppPowr, "powr<-1>(tan(" <> nest[a] <> "))", "pow(tan(" <> nest[a] <> "), -1)"];
        CExpression /: GenerateCode[CExpression[ArcSin[a_]]] := "asin(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCos[a_]]] := "acos(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcTan[a_]]] := "atan(" <> nest[a] <> ")";
        (*Mathematica's two-argument ArcTan[x, y] is x-part first, C's atan2(y, x) is y-part first.
          Emitting them positionally reflects the angle about the pi/4 axis, which for the
          S0/S1/SPhi vertex grids is not a leg permutation and so cannot be absorbed elsewhere.*)
        CExpression /: GenerateCode[CExpression[ArcTan[a_, b_]]] := "atan2(" <> nest[b] <> ", " <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCot[a_]]] := If[$CppPowr, "atan(powr<-1>(" <> nest[a] <> "))", "atan(pow(" <> nest[a] <> ", -1))"];
        CExpression /: GenerateCode[CExpression[Sinh[a_]]] := "sinh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Cosh[a_]]] := "cosh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Tanh[a_]]] := "tanh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Coth[a_]]] := If[$CppPowr, "powr<-1>(tanh(" <> nest[a] <> "))", "pow(tanh(" <> nest[a] <> "), -1)"];
        CExpression /: GenerateCode[CExpression[ArcSinh[a_]]] := "asinh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCosh[a_]]] := "acosh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcTanh[a_]]] := "atanh(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[ArcCoth[a_]]] := If[$CppPowr, "atanh(powr<-1>(" <> nest[a] <> "))", "atanh(pow(" <> nest[a] <> ", -1))"];
        (*min, max, abs*)
        CExpression /: GenerateCode[CExpression[Abs[a_]]] := "abs(" <> nest[a] <> ")";
        CExpression /: GenerateCode[CExpression[Min[a_, b_]]] := "min(" <> nest[a] <> "," <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Min[a_, b_, c__]]] := "min({" <> nest[a] <> "," <> nest[b] <> "," <> StringRiffle[Map[nest, {c}], ","] <> "})";
        CExpression /: GenerateCode[CExpression[Max[a_, b_]]] := "max(" <> nest[a] <> "," <> nest[b] <> ")";
        CExpression /: GenerateCode[CExpression[Max[a_, b_, c__]]] := "max({" <> nest[a] <> "," <> nest[b] <> "," <> StringRiffle[Map[nest, {c}], ","] <> "})";
        (*fast-math intrinsics — single precision only, must come AFTER standard rules to override*)
        If[$codeFastMath && $codePrecision === "single",
            CExpression /: GenerateCode[CExpression[cppExp[a_]]] := "__expf(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Exp[a_]]] := "__expf(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Log[a_]]] := "__logf(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Sqrt[a_]]] := "__fsqrt_rn(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Sin[a_]]] := "__sinf(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Cos[a_]]] := "__cosf(" <> nest[a] <> ")";
            CExpression /: GenerateCode[CExpression[Tan[a_]]] := "__tanf(" <> nest[a] <> ")";
        ];
        body
    ];

(* GenerateCode applies the CExpression rules installed above directly. ToCCodeString does
   the same but carries ~26 ms/call of fixed SymbolicC overhead (line-wrapping
   bookkeeping) — catastrophic when CppForm is called 65-110x per kernel. Our
   rules already emit fully-parenthesized code, and clang-format normalizes
   whitespace downstream, so the output is equivalent (whitespace-only delta). *)

cppFormEmit[expr_, OptionsPattern[]] :=
    Module[{processedExpr},
        processedExpr = N[expr /. Power[E, x_] :> cppExp[x]];
        If[TrueQ[OptionValue["Format"]],
            FormatCppCode[GenerateCode[CExpression[processedExpr]]]
            ,
            GenerateCode[CExpression[processedExpr]]
        ]
    ];

clangFormatExists = Quiet[RunProcess[{"clang-format", "--help"}]] =!= $Failed;

CreateClangFormat[path_:"./"] :=
    If[Not @ FileExistsQ[FileNameJoin[{path, ".clang-format"}]],
        Export[FileNameJoin[{path, ".clang-format"}], "BasedOnStyle: LLVM
UseTab: Never
IndentWidth: 2
TabWidth: 2
BreakBeforeBraces: Linux
AllowShortIfStatementsOnASingleLine: true
IndentCaseLabels: false
ColumnLimit: 140
AccessModifierOffset: -2
NamespaceIndentation: All
AllowShortEnumsOnASingleLine: true
ContinuationIndentWidth: 2
AlignOperands: DontAlign
", "Text"]
    ];

WriteCodeToFile::unchanged = "File `1` unchanged.";

WriteCodeToFile::exported = "Exported to `1`.";

WriteCodeToFile[fileName_String, expression_String] :=
    Module[{tmpfileName},
        tmpfileName = fileName <> ".tmpcode";
        (* FormatCppCode picks the in-process formatter or clang-format per FSetUseClangFormat *)
        Export[tmpfileName, FormatCppCode[expression], "Text"];
        If[FileExistsQ[fileName],
            If[Import[fileName, "Text"] == Import[tmpfileName, "Text"],
                Message[WriteCodeToFile::unchanged, fileName];
                Quiet[DeleteFile[tmpfileName]]
                ,
                Message[WriteCodeToFile::exported, fileName];
                RenameFile[tmpfileName, fileName]
            ]
            ,
            Message[WriteCodeToFile::exported, fileName];
            RenameFile[tmpfileName, fileName]
        ]
    ];

(* The codegen pipeline emits one statement per line (see the formatCppInProcess note
   below), so over-long statements are fenced per LINE. Splitting on ";" (the old
   approach) glued the "off" marker onto the previous statement's line — where the
   formatters, which match markers at line start, never saw it — and turned each
   chunk's leading newline into a spurious blank line.
   - StringSplit[..., All] keeps zero-length pieces, so the newline structure
     round-trips exactly through StringRiffle.
   - inOff skips lines already inside an off-region: the sub-kernel path emits its
     own fence around the giant "using _T = decltype(...)" line, whose line text
     contains no marker and must not be double-wrapped. The StringFreeQ guard skips
     marker-bearing lines themselves. *)
wrapLargeStatementsForClangFormat[code_String] :=
    Module[{inOff = False},
        StringRiffle[
            Flatten @ Map[
                Function[ln,
                    Module[{t = StringTrim[ln]},
                        Which[
                            StringStartsQ[t, "// clang-format off"],
                                inOff = True;
                                {ln}
                            ,
                            StringStartsQ[t, "// clang-format on"],
                                inOff = False;
                                {ln}
                            ,
                            !inOff && StringLength[ln] > $codeFormatStatementLimit && StringFreeQ[ln, "// clang-format"],
                                {"// clang-format off", ln, "// clang-format on"}
                            ,
                            True,
                                {ln}
                        ]
                    ]
                ]
                ,
                StringSplit[code, "\n", All]
            ]
            ,
            "\n"
        ]
    ];

fixClangFormatOffIndentation[code_String] :=
    Module[{lines, i, indent, inOff, result},
        lines = StringSplit[code, "\n"];
        result = {};
        inOff = False;
        indent = "";
        Do[
            Which[
                StringStartsQ[StringTrim[lines[[i]]], "// clang-format off"],
                    (* Determine indent from the previous non-empty formatted line *)indent = First[StringCases[SelectFirst[Reverse @ result, StringLength[#] > 0&, ""], RegularExpression["^(\\s*)"] :> "$1"], ""];
                    inOff = True;
                    AppendTo[result, indent <> "// clang-format off"]
                ,
                StringStartsQ[StringTrim[lines[[i]]], "// clang-format on"],
                    inOff = False;
                    AppendTo[result, indent <> "// clang-format on"]
                ,
                inOff,
                    (* Re-indent: strip existing leading whitespace, apply detected indent *)AppendTo[result, indent <> StringReplace[lines[[i]], RegularExpression["^\\s+"] -> ""]]
                ,
                True,
                    AppendTo[result, lines[[i]]]
            ]
            ,
            {i, 1, Length[lines]}
        ];
        StringRiffle[result, "\n"]
    ];

(* In-process C++ pretty-printer (default formatter).

   The codegen pipeline already emits one statement per line (formatDefinitions in
   CppOptimize.m), so formatting reduces to consistent indentation by brace depth. This
   avoids the ~1 s/call clang-format spawn on large kernels entirely.

   - wrapLargeStatementsForClangFormat is reused to fence over-long statements (and the
     giant "using _T = decltype(...)" lines) in // clang-format off/on regions.
   - Lines inside those regions are passed through verbatim; fixClangFormatOffIndentation
     re-indents the markers afterwards (shared with the clang-format path).
   - Brace depth is counted only outside off-regions; balanced inline braces (e.g.
     "_T _acc{};", "min({a, b})") net to zero, so they do not perturb indentation. *)

cppIndent[n_Integer] :=
    StringJoin @ ConstantArray["  ", Max[n, 0]];

formatCppInProcess[code_String] :=
    Module[{pre, lines, depth, out, inOff, t},
        pre = wrapLargeStatementsForClangFormat[code];
        (* Break a leading access specifier ("public:", etc.) onto its own line for
           readability; safe because these only occur at class scope, never in off-regions. *)
        lines =
            Flatten @ Map[
                Function[ln,
                    Module[{tt = StringTrim[ln], spec},
                        If[StringMatchQ[tt, ("public:" | "private:" | "protected:") ~~ Whitespace ~~ __],
                            spec = First @ StringCases[tt, StartOfString ~~ ("public:" | "private:" | "protected:")];
                            {spec, StringTrim @ StringDrop[tt, StringLength[spec]]}
                            ,
                            {ln}
                        ]
                    ]
                ],
                StringSplit[pre, "\n"]
            ];
        depth = 0;
        out = {};
        inOff = False;
        Do[
            Module[{ln = lines[[i]]},
                Which[
                    StringStartsQ[StringTrim[ln], "// clang-format off"],
                        AppendTo[out, cppIndent[depth] <> "// clang-format off"];
                        inOff = True;
                    ,
                    StringStartsQ[StringTrim[ln], "// clang-format on"],
                        inOff = False;
                        AppendTo[out, cppIndent[depth] <> "// clang-format on"];
                    ,
                    inOff,
                        AppendTo[out, ln];
                    ,
                    True,
                        t = StringTrim[ln];
                        If[t === "",
                            AppendTo[out, ""];
                            ,
                            AppendTo[out, cppIndent[depth - If[StringStartsQ[t, "}"], 1, 0]] <> t];
                            depth = Max[depth + StringCount[t, "{"] - StringCount[t, "}"], 0];
                        ];
                ]
            ]
            ,
            {i, 1, Length[lines]}
        ];
        fixClangFormatOffIndentation @ StringRiffle[out, "\n"]
    ];

(* clang-format path, kept as an opt-in fallback (FSetUseClangFormat[True]). *)

formatViaClangFormat[expression_String] :=
    Module[{tmpfileName1, tmpfileName2, output},
        tmpfileName1 = FileNameJoin[{$TemporaryDirectory, "in_" <> makeTemporaryFileName[]}];
        tmpfileName2 = FileNameJoin[{$TemporaryDirectory, "out_" <> makeTemporaryFileName[]}];
        Export[tmpfileName1, wrapLargeStatementsForClangFormat[expression], "Text"];
        CreateClangFormat[$TemporaryDirectory];
        Export[tmpfileName2, RunProcess[{"clang-format", "-style=file:" <> FileNameJoin[{$TemporaryDirectory, ".clang-format"}], tmpfileName1}, "StandardOutput"], "Text"];
        output = fixClangFormatOffIndentation @ Import[tmpfileName2, "Text"];
        Quiet[DeleteFile[{tmpfileName1, tmpfileName2}]];
        output
    ];

Options[FormatCppCode] = {"Format" -> True};

FormatCppCode[expression_String, OptionsPattern[]] :=
    If[Not @ TrueQ[OptionValue["Format"]],
        expression
        ,
        If[TrueQ[$ProfileCodegenOn], $ProfileCgClangFormatCount++];
        cgTimed[$ProfileCgClangFormat,
            If[TrueQ[$FUseClangFormat] && clangFormatExists,
                formatViaClangFormat[expression]
                ,
                formatCppInProcess[expression]
            ]
        ]
    ];

FormatCppCode[___] :=
    (
        Message[FunKit::invalidArguments, FormatCppCode];
        Abort[]
    );

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
        Module[{wCases},
            wCases = Map[ToExpression @ StringTake[#, {3, -2}]&, StringCases[res, Shortest["w[" ~~ (arg1__ /; balancedRBracesQ[arg1]) ~~ "]"]]];
            maxW =
                If[Length[wCases] === 0,
                    0
                    ,
                    Max[wCases]
                ];
        ];
        For[idx = 1, idx <= maxW, idx++,
            res = StringReplacePart[res, "auto _tmp" <> ToString[idx] <> "", StringPosition[res, "w[" <> ToString[idx] <> "]", 1]];
        ];
        res = StringReplace[res, {Shortest["w[" ~~ (arg1__ /; balancedRBracesQ[arg1]) ~~ "]"] :> "_tmp" ~~ arg1 ~~ "", "expr=" -> "return ", "\n" -> ""}];
        res = FormatCppCode[res];
        (*Get rid of unecessary copies*)
        repl = Map[StringReplace[#, "auto _tmp" ~~ a__ ~~ " = " ~~ b__ ~~ ";" /; hasNoOperators[b] :> "_tmp" ~~ a ~~ "->" ~~ b]&, Select[StringSplit[res, "\n"], StringMatchQ[#, "auto _tmp" ~~ a__ ~~ "=" ~~ b__ ~~ ";" /; hasNoOperators[b]]&]];
        repl = Map[((a_ /; MatchQ[a, "(" | " " | "-"]) ~~ #[[1]] ~~ (b_ /; MatchQ[b, ")" | " " | ";"]) :> a ~~ #[[2]] ~~ b&) @ StringSplit[#, "->"]&, repl];
        res = StringJoin[Select[StringSplit[res, "\n"], Not @ StringMatchQ[#, "auto _tmp" ~~ a__ ~~ "=" ~~ b__ /; hasNoOperators[b]]&]];
        res = StringReplace[res, repl];
        Return[res];
    ];

CppCodeFORM[___] /; ($OperatingSystem === "Windows" && !TrueQ[Global`$UseFORMOnWindows]) :=
    (Message[FunKit::warning, "CppCodeFORM requires FORM/FormTracer, which is not available on Windows. Set Global`$UseFORMOnWindows = True to override."]; Abort[]);

CppCodeFORM[expr_] :=
    Module[{origVars, tmpfileName, import},
        origVars = FormTracer`GetExtraVars[];
        tmpfileName = FileNameJoin[{$TemporaryDirectory, "FO_" <> makeTemporaryFileName[]}];
        FormTracer`AddExtraVars @@ GetAllCustomSymbols[expr];
        FormTracer`FormTrace[expr // Rationalize, {}, {}, {tmpfileName, "O4"}];
        FormTracer`DefineExtraVars[origVars];
        import = Import[tmpfileName, "Text"];
        Quiet[DeleteFile[tmpfileName]];
        import // formatFORMCode
    ];

(*C++ code creation*)

(* ::Input::Initialization:: *)

(* ::Input::Initialization:: *)

(* ---- cheap accumulator-type deduction -------------------------------------------------------

   `_T` must be the type of the FULL SUM: terms mix plain double, autodiff and complex leaves, and
   only summing all of them lets C++ promote to the common type (complex + AD is AD<complex>).
   Deducing from one term would silently pick the wrong type when an AD or complex dressing appears
   in only some of the traces. But the POLYNOMIAL STRUCTURE contributes nothing to the type — only
   the leaves do — so one occurrence of each distinct leaf deduces the same type in a fraction of
   the text. The raw expression written out for this is otherwise the single largest thing in a
   generated kernel and the only part with no CSE at all: measured 73-85% of the file on the dense
   QCD kernels, 3.35 MB of 4.42 MB on hPhiL.

   A definition only matters here if it INTRODUCES a type, i.e. its right-hand side is not pure
   arithmetic over things that already exist. `_cse`-style definitions minted by the per-sub-kernel
   passes are arithmetic by construction, so they can be ignored — which matters, because those are
   scoped inside their sub-kernel block AND their names are reused across blocks (each block renumbers
   from 1). Interpolator/regulator calls do introduce a type; those come from the GLOBAL hoisting pass,
   so their names are unique and they can safely be lifted out of the block to be nameable here. *)

(* The `1` caps the search at the first hit: the result is only ever compared against {}, and on a
   large kernel this runs once per definition over the whole right-hand side. *)

introducesTypeQ[rhs_] :=
  Cases[rhs, h_[___] /; !MemberQ[{Plus, Times, Power}, h], {0, Infinity}, 1] =!= {};

(* Leaves of `expr` whose types matter: descend arithmetic, drop numeric literals, keep the rest.

   `skipDefs` maps each skipped (arithmetic-only) definition name to its right-hand side. Such a name
   is NOT simply dropped: its own type is implied by its inputs, but those inputs may include a type
   the rest of the expression never mentions — an AD or complex parameter used only inside one
   `_cse`, say `_cse7 = d1V*2.` with `d1V` appearing nowhere else. Dropping the name would lose that
   parameter's contribution to the promotion and silently deduce a narrower `_T`, which truncates
   (an AD derivative or an imaginary part) instead of failing to compile. So descend into the
   definition. `seen` guards against revisiting a shared sub-definition. *)

accTypeLeaves[expr_, skipDefs_] :=
(* skipAlt is built ONCE. It used to be `Alternatives @@ skipNames` inside the Which below, i.e.
   rebuilt from a list as long as the kernel's definition table at every non-arithmetic node --
   and the membership test above it was a linear MemberQ over the same list. Both are per-node
   costs in a walk over the whole sub-kernel expression, which is what made this quadratic. *)
  Module[{bag = Internal`Bag[], walk, seen = <||>, skipNames = Keys[skipDefs], skipAlt},
    skipAlt = Alternatives @@ skipNames;
    walk[e_] :=
      Which[
        NumericQ[e], Null,
        KeyExistsQ[skipDefs, e],
          If[!KeyExistsQ[seen, e], seen[e] = True; walk[skipDefs[e]]],
(* type-transparent arithmetic: descend. fmaGroup is a*b+c, so all three arguments contribute —
   treating it as opaque both loses the shrink AND drags block-scoped names into the decltype. *)
        Head[e] === Plus || Head[e] === Times || Head[e] === fmaGroup, walk /@ (List @@ e),
        Head[e] === Power, walk[e[[1]]],
(* Any other call is a leaf — UNLESS it still mentions a name we are skipping, which would be
   emitted verbatim into a scope where that name does not exist. Descending is then both necessary
   and safe: the globally-hoisted calls (the ones whose type is NOT their argument's) never appear
   inline in the terms, so anything left here is an arithmetic wrapper. *)
        skipNames =!= {} && !FreeQ[e, skipAlt], walk /@ (List @@ e),
        True, Internal`StuffBag[bag, e]
      ];
    walk[expr];
    DeleteDuplicates @ Internal`BagPart[bag, All]
  ];

Options[CppCode] = {"ReturnTransform" -> Identity};

CppCode[equation_, OptionsPattern[]] :=
    Module[{transform, optimized, varNames, definitions, returnStatement},
        transform = OptionValue["ReturnTransform"];
        optimized = optimizeExpression[equation];
        varNames = getAllVarNames[optimized];
        (* Sub-kernel pattern (level 3) *)
        If[TrueQ[optimized["UseSubKernels"]],
            Module[{subKernels, sharedDefs, sharedCode, liftCode, allNames, subCode, declLine, accSym, accStr, wrappedReturn, lifted, liftedNames, arithNames, leaves},
                sharedDefs = optimized["SharedDefinitions"];
                subKernels = optimized["SubKernels"];
                allNames = Join[sharedDefs[[All, 1]], Flatten @ Map[#["Definitions"][[All, 1]]&, subKernels]];
(* Type deduction (see accTypeLeaves above). _acc accumulates un-transformed terms; the
   ReturnTransform is applied only at the final return.
   `lifted` = the sub-kernel-local definitions that INTRODUCE a type (a call, not arithmetic). Only
   those have to be nameable at the decltype, so only those leave their block.

   They are NOT all products of the global hoisting pass, so their names are NOT automatically
   unique: a `_cse` name is renumbered from 1 by every block, and introducesTypeQ classifies plenty
   of those as type-introducing (`_cse1 = powr<-1>(l1)` is a call as far as it is concerned). When
   two blocks both CSE the same subexpression they both mint `_cse1`, both get lifted, and the
   emitted function has two `const auto _cse1` in one scope -- a hard C++ error
   ("conflicting declaration"), first hit by a three-coordinate vertex kernel large enough to be
   split into several sub-kernels.

   Same name + identical rhs is the same value, so collapsing them is exactly equivalent; keep the
   first occurrence to preserve definition order, since lifted defs may reference earlier ones. Same
   name + DIFFERENT rhs would be a miscompile, so refuse loudly rather than silently pick one. *)
                lifted = Select[Flatten[Map[#["Definitions"]&, subKernels], 1], introducesTypeQ[#[[2]]]&];
                Module[{conflicts},
                    conflicts = Select[GroupBy[lifted, First], Length[DeleteDuplicates[#[[All, 2]]]] > 1&];
                    If[Length[conflicts] > 0,
                        Print["MakeCppFunction: cannot lift sub-kernel definitions -- these names carry "
                            <> "different right-hand sides in different sub-kernels: ", Keys[conflicts]];
                        Abort[]]];
                lifted = DeleteDuplicatesBy[lifted, First];
                liftedNames = lifted[[All, 1]];
(* the arithmetic-only sub-kernel defs, as name -> rhs: accTypeLeaves descends INTO these rather than
   dropping them, so a type reachable only through one of them (an AD or complex parameter used in a
   single `_cse`) still reaches the promotion. *)
                arithNames = With[{liftedSet = makeNameSet[liftedNames]},
                    Association[Rule @@@ Select[Flatten[Map[#["Definitions"]&, subKernels], 1],
                        !inNameSet[liftedSet, #[[1]]]&]]];
                leaves = DeleteDuplicates @ Join[sharedDefs[[All, 1]], liftedNames,
                    accTypeLeaves[Total[Map[#["Terms"]&, subKernels]], arithNames]];
                declLine =
                    "// clang-format off\nusing _T = decltype(" <>
                      If[leaves === {}, "0.", stripQuotedNames[CppForm[Total[leaves], "Format" -> False], allNames]] <>
                      ");\n// clang-format on\n";
                sharedCode = stripQuotedNames[formatDefinitions[sharedDefs], allNames];
                liftCode = If[lifted === {}, "", stripQuotedNames[formatDefinitions[lifted], allNames]];
                (* Scoped accumulation; only the arithmetic-only defs remain inside the block *)
(* Each block is stripped against the SHARED names plus its own, not against allNames.
   allNames carries one entry per definition of every sub-kernel -- and dagCSE renumbers _cseN
   from 1 inside each -- so it is roughly `#subkernels` times longer than the set any single
   block can mention. Passing the long list made stripQuotedNames rebuild its lookup (and
   re-check its fast-path guard) over thousands of names once per block. Equivalent by
   construction: a block's code only ever names a shared def or one of its own, and a name that
   does not occur in a string cannot be stripped from it. *)
                subCode =
                    cgTimed[$ProfileCgEmitScaffold,
                    Table[
                        Module[{localDefs, termsCode, blockNames},
                            blockNames = Join[sharedDefs[[All, 1]], subKernels[[i]]["Definitions"][[All, 1]]];
                            localDefs = formatDefinitions[Select[subKernels[[i]]["Definitions"], !introducesTypeQ[#[[2]]]&]];
                            localDefs = stripQuotedNames[localDefs, blockNames];
                            termsCode = CppForm[subKernels[[i]]["Terms"]];
                            termsCode = stripQuotedNames[termsCode, blockNames];
                            "{ // subkernel " <> ToString[i] <> "\n" <> localDefs <> "_acc += " <> termsCode <> ";\n}\n"
                        ]
                        ,
                        {i, Length[subKernels]}
                    ]];
                accSym = Unique["postAcc"];
                accStr = ToString[accSym];
                wrappedReturn = StringReplace[CppForm[transform[accSym]], accStr -> "_acc"];
                Return[sharedCode <> liftCode <> declLine <> "_T _acc{};\n" <> StringJoin[subCode] <> "return " <> wrappedReturn <> ";"]
            ]
        ];
        (* Standard path: definitions + return *)
        definitions = formatDefinitions[optimized["Definitions"]];
        definitions = stripQuotedNames[definitions, varNames];
        returnStatement = formatReturnStatement[transform[optimized["Expr"]]];
        returnStatement = stripQuotedNames[returnStatement, varNames];
        FunKitDebug[2, "Definitions: ", definitions];
        FunKitDebug[2, "returnStatement: ", returnStatement];
        definitions <> "\n" <> returnStatement
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

prepParam::invalid = "The value `1` is not a valid C++ parameter. Expected a String or Association.";

prepParam[it_] :=
    (
        Message[prepParam::invalid, it];
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

Options[MakeCppFunction] = {"Return" -> "auto", "Parameters" -> {}, "Name" -> "function", "Prefix" -> "", "Suffix" -> "", "CodeParser" -> "Cpp", "Body" -> "", "Class" -> "", "Templates" -> {}, "ReturnTransform" -> Identity};

MakeCppFunction[OptionsPattern[]] :=
    Module[{functionPrefix, functionSuffix, functionName, functionParameters, functionTemplates, idx, functionBody, parameters},
        FunKitDebug[1, "Preparing Cpp function..."];
        (*Create prefixes for the function, e.g. static or such + the return value*)
        functionPrefix =
            If[OptionValue["Prefix"] === "",
                ""
                ,
                OptionValue["Prefix"] <> " "
            ] <> OptionValue["Return"] <> " ";
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
            Module[{explicitTemplates, paramTemplates = functionTemplates},
                explicitTemplates =
                    If[OptionValue["Templates"] === {},
                        ""
                        ,
                        "typename " <> StringRiffle[OptionValue["Templates"], ", typename "]
                    ];
                Which[
                    explicitTemplates =!= "" && paramTemplates =!= "",
                        explicitTemplates <> ", " <> paramTemplates
                    ,
                    explicitTemplates =!= "",
                        explicitTemplates
                    ,
                    paramTemplates =!= "",
                        paramTemplates
                    ,
                    True,
                        ""
                ]
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
                (* Collapse every run of newlines to a single one: replacing "\n\n" with the
                   empty string glued adjacent statements together, and the
                   formatDefinitions/CppCode joins produce runs of up to three newlines. *)
                StringReplace["{\n" <> OptionValue["Body"] <> "\n}", "\n" ~~ ("\n" ..) -> "\n"]
            ];
        FunKitDebug[2, "  Prepared Cpp function; now parsing code."];
        Return[FormatCppCode[functionTemplates <> functionPrefix <> functionName <> functionParameters <> functionSuffix <> "\n" <> functionBody]]
    ];

MakeCppFunction[expr_, OptionsPattern[]] :=
    Module[{useFORM, transform, generated, newBody},
        useFORM = OptionValue["CodeParser"] === "FORM";
        transform = OptionValue["ReturnTransform"];
        generated =
            If[useFORM,
                If[transform =!= Identity,
                    Message[FunKit::warning, "ReturnTransform is not supported with CodeParser \"FORM\"; ignoring."]
                ];
                CppCodeFORM[expr]
                ,
                CppCode[expr, "ReturnTransform" -> transform]
            ];
        newBody = OptionValue["Body"] <> "\n" <> generated;
        MakeCppFunction @@ (Evaluate @ Join[{"Body" -> newBody}, Thread[Rule @@ {#, OptionValue[MakeCppFunction, #]}]& @ Keys[Options[MakeCppFunction]]])
    ];

(* ::Subsubsection:: *)

(*Split (outlined sub-kernel) function creation*)

(* ::Input::Initialization:: *)

(* Transitive closure of definition names referenced by an expression, returned in the
   dependency order of `orderedNames` (so a def appears after the defs it depends on). *)
transitiveDefRefs[expr_, orderedNames_, defsByName_] :=
    Module[{refs, prev = -1},
        refs = Intersection[orderedNames, DeleteDuplicates @ Cases[expr, _String, {0, Infinity}]];
        While[Length[refs] =!= prev,
            prev = Length[refs];
            refs = DeleteDuplicates @ Join[refs,
                Intersection[orderedNames, Flatten @ Map[Cases[defsByName[#][[2]], _String, {0, Infinity}]&, refs]]];
        ];
        With[{refSet = makeNameSet[refs]}, Select[orderedNames, inNameSet[refSet, #]&]]
    ];

ClearAll[MakeCppFunctionSplit];

MakeCppFunctionSplit::bothinterp = "Options \"Interpolators\" and \"NotInterpolators\" are mutually exclusive; provide at most one.";

(* ShareInterpolators: True = scoped per-contribution lookup-passing (OpenACC-style); False
   (default) = each sub recomputes the shared defs it uses (DCE prunes). Measured on ZA4:
   recompute spills LESS (254/1728) than scoped (255/1984) or up-front sharing — the parent's
   persistent _acc + per-block lookups outweigh the per-sub recompute, so recompute wins. *)
Options[MakeCppFunctionSplit] = Join[Options[MakeCppFunction], {"Decorator" -> "static KOKKOS_FUNCTION", "Interpolators" -> Automatic, "NotInterpolators" -> Automatic, "ShareInterpolators" -> False, "SeparateLookups" -> False}];

MakeCppFunctionSplit[expr_, opts : OptionsPattern[]] :=
    Module[{transform, optimized, prologue, name, decorator, params, argNames, argStr,
            sharedDefs, subKernels, allNames, sharedCode, subFns, kernelBody, kernelFn,
            accSym, wrappedReturn},
        (* Mutually-exclusive interpolator declaration (used by the shared-hoisting pass) *)
        If[OptionValue["Interpolators"] =!= Automatic && OptionValue["NotInterpolators"] =!= Automatic,
            Message[MakeCppFunctionSplit::bothinterp];
            Abort[]
        ];
        (* SeparateLookups: the OpenACC dispatcher->integrand structure. Emit ONE arithmetic
           `<name>_eval` that takes the hoisted interpolator results as scalar parameters (no
           spline-lookup code, no arithmetic splitting), and a `<name>` that performs the spline
           lookups and calls `<name>_eval` once. Separating the (register-heavy) lookup code from
           the polynomial into two frames is what lets the OpenACC integrand fit without spilling. *)
        If[TrueQ[OptionValue["SeparateLookups"]],
            Module[{savedMax, savedLvl, opt, defs, finalExpr, varNamesL, lookupDefs, restDefs, lookupNames,
                    prologueL, nameL, paramsL, argNamesL, decoratorL, transformL,
                    evalParams, restCode, retStmt, evalBody, evalFn, lookupCode, kernelBodyL, kernelFnL},
                nameL = OptionValue["Name"]; prologueL = OptionValue["Body"]; paramsL = OptionValue["Parameters"];
                argNamesL = Map[If[StringQ[#], #, #["Name"]]&, paramsL];
                decoratorL = OptionValue["Decorator"]; transformL = OptionValue["ReturnTransform"];
                (* Optimize WITHOUT sub-kernel splitting — we want one flat arithmetic body. *)
                savedMax = $codeMaxKernelTerms; savedLvl = $kernelSplitLevel;
                $codeMaxKernelTerms = 10^9; $kernelSplitLevel = 1;
                opt = optimizeExpression[expr];
                $codeMaxKernelTerms = savedMax; $kernelSplitLevel = savedLvl;
                defs = opt["Definitions"]; finalExpr = opt["Expr"]; varNamesL = getAllVarNames[opt];
                (* Lookups = the hoisted interpolator calls named _interp...; the _den... and
                   _cse... defs are cheap arithmetic and stay inside eval. *)
                lookupDefs = Select[defs, StringStartsQ[#[[1]], "_interp"]&];
                restDefs = Select[defs, Not @ StringStartsQ[#[[1]], "_interp"]&];
                lookupNames = lookupDefs[[All, 1]];
                evalParams = Join[paramsL, Map[<|"Name" -> #, "Type" -> "auto", "Const" -> True, "Reference" -> True|>&, lookupNames]];
                restCode = stripQuotedNames[formatDefinitions[restDefs], varNamesL];
                retStmt = stripQuotedNames[formatReturnStatement[transformL[finalExpr]], varNamesL];
                evalBody = prologueL <> "\n" <> restCode <> "\n" <> retStmt;
                evalFn = MakeCppFunction["Name" -> nameL <> "_eval", "Parameters" -> evalParams,
                    "Prefix" -> decoratorL, "Return" -> "auto", "Body" -> evalBody];
                lookupCode = stripQuotedNames[formatDefinitions[lookupDefs], varNamesL];
                kernelBodyL = prologueL <> "\n" <> lookupCode <> "return " <> nameL <> "_eval(" <> StringRiffle[Join[argNamesL, lookupNames], ", "] <> ");";
                kernelFnL = MakeCppFunction["Name" -> nameL, "Parameters" -> paramsL, "Prefix" -> OptionValue["Prefix"],
                    "Return" -> OptionValue["Return"], "Suffix" -> OptionValue["Suffix"], "Templates" -> OptionValue["Templates"],
                    "Class" -> OptionValue["Class"], "Body" -> kernelBodyL];
                Return[StringRiffle[{evalFn, kernelFnL}, "\n\n"]]
            ]
        ];
        transform = OptionValue["ReturnTransform"];
        optimized = optimizeExpression[expr];
        (* If the integrand did not split into sub-kernels, emit the plain single function. *)
        If[!TrueQ[optimized["UseSubKernels"]],
            Return[MakeCppFunction @@ (Evaluate @ Join[{expr}, Thread[Rule @@ {#, OptionValue[#]}]& @ Keys[Options[MakeCppFunction]]])]
        ];
        prologue = OptionValue["Body"];
        name = OptionValue["Name"];
        decorator = OptionValue["Decorator"];
        params = OptionValue["Parameters"];
        argNames = Map[If[StringQ[#], #, #["Name"]]&, params];
        argStr = StringRiffle[argNames, ", "];
        sharedDefs = optimized["SharedDefinitions"];
        subKernels = optimized["SubKernels"];
        allNames = Join[sharedDefs[[All, 1]], Flatten @ Map[#["Definitions"][[All, 1]]&, subKernels]];
        sharedCode = stripQuotedNames[formatDefinitions[sharedDefs], allNames];
        accSym = Unique["postAcc"];
        wrappedReturn = StringReplace[CppForm[transform[accSym]], ToString[accSym] -> "_acc"];
        If[TrueQ[OptionValue["ShareInterpolators"]] && Length[sharedDefs] > 0,
            (* SCOPED-SHARE path (mirrors the OpenACC dispatcher->integrand structure): the
               parent computes each sub-kernel's interpolators/denominators in its OWN scoped
               block and passes them as scalars to a poly-only sub-function. Block scope lets
               each contribution's lookups die before the next, so the parent never holds them
               all live; and the sub-functions carry no lookup code (just the polynomial). This
               separates the lookup working-set from the polynomial working-set into distinct
               register frames. *)
            Module[{sharedNames, declLine, leafCode, perSubShared, blocks},
                sharedNames = sharedDefs[[All, 1]];
(* Accumulator type from the type-determining LEAVES of the expression rather than the expression
   itself (see accTypeLeaves). Same deduced type — the promotion of a sum-of-products is fixed by its
   leaves — but the text is bounded by the number of DISTINCT leaves instead of by the polynomial,
   and this decltype is otherwise the only part of the emission with no CSE at all (73-85% of the
   file on the dense kernels).
   Unlike the CppCode sub-kernel path, nothing is hoisted here and no scope changes: `expr` is the
   pre-CSE expression, so its leaves are raw calls and parameters, all of which are already in scope
   at function level. The per-sub blocks keep their own shared-def declarations exactly as before, so
   the register-frame separation this path exists for is untouched. *)
                leafCode =
                  With[{lv = accTypeLeaves[expr, <||>]},
                    If[lv === {}, "0.", CppForm[Total[lv], "Format" -> False]]];
                declLine = "// clang-format off\nusing _T = decltype(" <> leafCode <> ");\n// clang-format on\n";
                (* Shared (globally-unique) defs each sub transitively needs — traversing both
                   the sub's terms and its own (locally-named) defs. Only these are passed as
                   scalars; per-sub local CSE keep their (reused) names and stay inside the sub. *)
                perSubShared =
                    Table[
                        Module[{localDefs, defsByName, allOrdered, refs},
                            localDefs = subKernels[[i]]["Definitions"];
                            defsByName = Association @ Join[
                                Table[sharedDefs[[j, 1]] -> sharedDefs[[j]], {j, Length[sharedDefs]}],
                                Table[localDefs[[j, 1]] -> localDefs[[j]], {j, Length[localDefs]}]];
                            allOrdered = Join[sharedNames, localDefs[[All, 1]]];
                            refs = transitiveDefRefs[subKernels[[i]]["Terms"], allOrdered, defsByName];
                            With[{refSet = makeNameSet[refs]}, Select[sharedNames, inNameSet[refSet, #]&]]
                        ]
                        ,
                        {i, Length[subKernels]}
                    ];
                subFns =
                    Table[
                        Module[{usedShared, localCode, termsCode, subParams, subBody},
                            usedShared = perSubShared[[i]];
                            localCode = stripQuotedNames[formatDefinitions[subKernels[[i]]["Definitions"]], allNames];
                            termsCode = stripQuotedNames[CppForm[subKernels[[i]]["Terms"]], allNames];
                            subParams = Join[params, Map[<|"Name" -> #, "Type" -> "auto", "Const" -> True, "Reference" -> True|>&, usedShared]];
                            subBody = prologue <> "\n" <> localCode <> "return " <> termsCode <> ";";
                            MakeCppFunction["Name" -> name <> "_sub" <> ToString[i], "Parameters" -> subParams,
                                "Prefix" -> decorator, "Return" -> "auto", "Body" -> subBody]
                        ]
                        ,
                        {i, Length[subKernels]}
                    ];
                blocks =
                    Table[
                        Module[{usedShared, usedDefs, defCode},
                            usedShared = perSubShared[[i]];
                            usedDefs = With[{usedSet = makeNameSet[usedShared]},
                                Select[sharedDefs, inNameSet[usedSet, #[[1]]]&]];
                            defCode = stripQuotedNames[formatDefinitions[usedDefs], allNames];
                            "{\n" <> defCode <> "_acc += " <> name <> "_sub" <> ToString[i] <> "(" <> StringRiffle[Join[argNames, usedShared], ", "] <> ");\n}\n"
                        ]
                        ,
                        {i, Length[subKernels]}
                    ];
                kernelBody = prologue <> "\n" <> declLine <> "_T _acc{};\n" <> StringJoin[blocks] <> "return " <> wrappedReturn <> ";";
            ]
            ,
            (* RECOMPUTE path: each sub-function recomputes the shared defs (DCE prunes unused). *)
            subFns =
                Table[
                    Module[{localDefs, termsCode, subBody},
                        localDefs = stripQuotedNames[formatDefinitions[subKernels[[i]]["Definitions"]], allNames];
                        termsCode = stripQuotedNames[CppForm[subKernels[[i]]["Terms"]], allNames];
                        subBody = prologue <> "\n" <> sharedCode <> localDefs <> "return " <> termsCode <> ";";
                        MakeCppFunction["Name" -> name <> "_sub" <> ToString[i], "Parameters" -> params,
                            "Prefix" -> decorator, "Return" -> "auto", "Body" -> subBody]
                    ]
                    ,
                    {i, Length[subKernels]}
                ];
            kernelBody = prologue <> "\nauto _acc = " <>
                StringRiffle[Table[name <> "_sub" <> ToString[i] <> "(" <> argStr <> ")", {i, Length[subKernels]}], " + "] <>
                ";\nreturn " <> wrappedReturn <> ";";
        ];
        kernelFn = MakeCppFunction["Name" -> name, "Parameters" -> params, "Prefix" -> OptionValue["Prefix"],
            "Return" -> OptionValue["Return"], "Suffix" -> OptionValue["Suffix"], "Templates" -> OptionValue["Templates"],
            "Class" -> OptionValue["Class"], "Body" -> kernelBody];
        StringRiffle[Append[subFns, kernelFn], "\n\n"]
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
                (* Each access-specifier section must start on its OWN line: the
                   previous section's last member may end with a "// clang-format on"
                   directive, and fixClangFormatOffIndentation discards anything
                   appended after "// clang-format on" on the same line -- which would
                   silently swallow this section header and its members (observed for
                   AD flows whose last public member is a long, wrapped declaration). *)
                If[Length[OptionValue["MembersProtected"]] > 0,
                    "\nprotected: " <> StringRiffle[OptionValue["MembersProtected"], "\n\n"]
                    ,
                    ""
                ] <>
                If[Length[OptionValue["MembersPrivate"]] > 0,
                    "\nprivate: " <> StringRiffle[OptionValue["MembersPrivate"], "\n\n"]
                    ,
                    ""
                ] <> "\n};";
        StringReplace[classBody, {";;" -> ";"}];
        Return[FormatCppCode[classPrefix <> className <> classSuffix <> "\n" <> classBody]]
    ];

MakeCppClass[___] :=
    (
        Message[FunKit::invalidArguments, MakeCppClass];
        Abort[]
    );

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
        Return[FormatCppCode[headerPrefix <> "\n" <> headerIncludes <> "\n" <> headerBody]];
    ];

MakeCppHeader[___] :=
    (
        Message[FunKit::invalidArguments, MakeCppHeader];
        Abort[]
    );

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
        Return[FormatCppCode[sourcePrefix <> sourceIncludes <> sourceBody <> sourcePostfix]];
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
        ret = MakeCppHeader["Body" -> {MakeCppClass["TemplateTypes" -> {"REG"}, "Name" -> name, "MembersPublic" -> {MakeCppFunction[integrand, "Parameters" -> parametersIntegrand, "CodeParser" -> OptionValue["CodeParser"], "Name" -> "kernel", "Prefix" -> "static", "Return" -> "auto", "Body" -> OptionValue["integrandBody"]], MakeCppFunction[constant, "Parameters" -> parameters, "CodeParser" -> "Cpp", "Name" -> "constant", "Prefix" -> "static", "Return" -> "auto", "Body" -> OptionValue["constantBody"]]}, "MembersPrivate" -> {OptionValue["PrivateDefinitions"]}]}];
        Return[ret]
    ]
