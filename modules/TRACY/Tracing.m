(**********************************************************************************
    Tracing.m -- FormTrace definitions and diagram simplification

    Public API:
      FormTracer`FormTrace        -- UpValue overloads for FEx and FTerm tracing
      FSetAlwaysExpandLorentzTensors -- Toggle automatic Lorentz tensor expansion
      FIterativelySum            -- Iteratively sums and simplifies traced diagrams
      FDiagramSimplify           -- Collects and simplifies traced diagram expressions
      FMakeFormMomentumExpansion  -- Initializes FORM momentum expansion rules
      FFormSimplify              -- Simplifies expressions via FORM with caching

    Internal:
      findCouplings              -- Extracts coupling functions from an expression
                                    (used by FDiagramSimplify)

    Variables:
      $AlwaysExpandLorentzTensors -- Whether to auto-expand Lorentz tensors (default True)
      $StandardQuickSimplify     -- Quick simplification function (TimeConstraint -> 0.1)
      $StandardSimplify          -- Full simplification function
      $standardFORMmomentumRules -- Current FORM momentum expansion rules
**********************************************************************************)

Unprotect[FEx, FTerm, FormTracer`FormTrace];

FEx /: FormTracer`FormTrace[name_String, FEx[a__], preReplRules_ : {}, postReplRules_ : {}, bracket_ : {}] :=
    Module[{idx},
        CreateFormSet[];
        (*Check if the folder of name exists, if not, create it*)
        If[Not[DirectoryQ[$TraceCacheDir <> name]],
            CreateDirectory[$TraceCacheDir <> name];
        ];
        ParallelTable[FormTracer`FormTrace[$TraceCacheDir <> name <> "/" <> ToString[idx] <> ".m", {a}[[idx]], preReplRules, postReplRules, bracket], {idx, 1, Length[{a}]}]
    ];

FEx /: FormTracer`FormTrace[FEx[a__], preReplRules_ : {}, postReplRules_ : {}, bracket_ : {}] :=
    Module[{},
        CreateFormSet[];
        ParallelMap[FormTracer`FormTrace[makeHashFile[{#, preReplRules, postReplRules, bracket}], #, preReplRules, postReplRules, bracket]&, {a}]
    ];

FTerm /: FormTracer`FormTrace[file_String, FTerm[a__], preReplRules_ : {}, postReplRules_ : {}, bracket_ : {}] :=
    Module[{expr, origVars, tmpfileName, import, repl, formReps, result},
        If[FileExistsQ[file],
            FunKitDebug[1, "Result already exists, importing from file\n  " <> file];
            result = Import[file];
            Return[result]
        ];
        result = FormTracer`FormTrace[FTerm[a], preReplRules, postReplRules, bracket];
        Export[file, result];
        Return[result];
    ];

$AlwaysExpandLorentzTensors = True;

FSetAlwaysExpandLorentzTensors[set_] /; BooleanQ[set] :=
    Module[{},
        $AlwaysExpandLorentzTensors = set;
    ];

FSetAlwaysExpandLorentzTensors[___] :=
    (
        Message[FunKit::invalidArguments, FSetAlwaysExpandLorentzTensors];
        Abort[]
    );

FTerm /: FormTracer`FormTrace[FTerm[a__], preReplRules_ : {}, postReplRules_ : {}, bracket_ : {}] :=
    Module[{expr, origVars, tmpfileName, import, repl, formReps, result, pref},
        origVars = FormTracer`GetExtraVars[];
        If[NumericQ[{a}[[1]]],
            expr = Times @@ ({a}[[2 ;; ]]) // Rationalize;
            pref = {a}[[1]]
            ,
            expr = Times[a] // Rationalize;
            pref = 1
        ];
        If[$AlwaysExpandLorentzTensors,
            expr =
                expr //
                Global`TBInsertCombinedLorentzTensors //
                Global`UseLorentzLinearity;
            Block[{Print},
                FormTracer`DisentangleLorentzStructures[True]
            ]
            ,
            Block[{Print},
                FormTracer`DisentangleLorentzStructures[False]
            ];
        ];
        repl = SafeReplaceTrace[expr];
        tmpfileName = FileNameJoin[{$TemporaryDirectory, "FS_" <> makeTemporaryFileName[]}];
        FormTracer`AddExtraVars @@ GetAllCustomSymbols[expr /. repl[[1]]];
        formReps = Map[#[[2]] -> #[[1]]&, FormTracer`GetExtraVarsSynonyms[]];
        FunKitDebug[2, "Custom symbols in expression: ", GetAllCustomSymbols[expr /. repl[[1]]]];
        FormTracer`FormTrace[expr /. repl[[1]], Join[{scallDef}, preReplRules], postReplRules, {tmpfileName, "O1", "fortran90"}, bracket];
        FormTracer`DefineExtraVars[origVars];
        FunKitDebug[2, "FORM finished, reimporting to Mathematica."];
        result = ImportAndSimplifyFORM[tmpfileName];
        FunKitDebug[2, "Import finished."];
        Quiet[DeleteFile[tmpfileName]];
        Return[pref * result /. repl[[2]] /. formReps // Rationalize];
    ];

Protect[FEx, FTerm, FormTrace];

(**********************************************************************************
     Summing diagram results
**********************************************************************************)

$StandardQuickSimplify = Quiet @ Simplify[#, TimeConstraint -> 0.1]&;

$StandardSimplify = Simplify[#]&;

FIterativelySum[expr_List] :=
    Module[{returnValue},
        returnValue = expr;
        If[Length[returnValue] == 1,
            Return[returnValue]
        ];
        While[Length[returnValue] > 1, returnValue = ParallelMap[$StandardQuickSimplify @ FFormSimplify[Total[#]]&, Partition[returnValue, UpTo[4]]]];
        Return[$StandardQuickSimplify[returnValue[[1]]]];
    ]

FIterativelySum[expr_List, finalSize_Integer /; finalSize >= 0] :=
    Module[{processLists, returnValue, i},
        If[finalSize == 0,
            Return[FIterativelySum[expr][[1]]];
        ];
        processLists = expr;
        If[Length[processLists] == finalSize,
            Return[processLists]
        ];
        processLists = Sort[processLists, (ByteCount[#1] > ByteCount[#2])&];
        processLists = Table[Downsample[processLists, finalSize, i], {i, 1, finalSize}];
        returnValue = Map[FIterativelySum[#]&, processLists];
        Return[Flatten[returnValue]]
    ];

FIterativelySum[___] :=
    (
        Message[FunKit::invalidArguments, FIterativelySum];
        Abort[]
    );

(**********************************************************************************
     Simplification routines for diagram results
**********************************************************************************)

findCouplings[expr_] :=
    Module[{symbols},
        FunKitDebug[2, "findCouplings: looking for custom symbols."];
        symbols = GetAllCustomSymbols[expr];
        symbols = Pick[symbols, Map[MemberQ[{expr}, #[__], Infinity]&, symbols]];
        symbols = DeleteDuplicates @ Cases[expr, Alternatives @@ Map[#[__]&, symbols], Infinity];
        FunKitDebug[2, "findCouplings: found: ", symbols];
        symbols = Pick[symbols, Not /@ Map[MemberQ[{expr}, Power[a_, n_] /; (MemberQ[{a}, #, Infinity] && n < 0), Infinity]&, symbols]];
        FunKitDebug[2, "findCouplings: picked: ", symbols];
        Return @ symbols
    ];

FDiagramSimplify[expr_, mSimplify_ : (Quiet @ Simplify[Simplify[#, Trig -> False, TimeConstraint -> 0.01], Trig -> False, TimeConstraint -> 0.1]&)] :=
    Module[{collected, couplings},
        couplings = findCouplings[expr];
        FunKitDebug[2, "FDiagramSimplify: Found the following couplings in the given expression: ", couplings];
        collected = Collect[expr, Map[#[__]&, couplings]];
        If[Head[collected] === Plus,
            collected = List @@ collected
            ,
            collected = {collected}
        ];
        If[Length[collected] > 1,
            collected = ParallelMap[mSimplify, collected]
            ,
            collected = {mSimplify[collected[[1]]]}
        ];
        FunKitDebug[2, "FDiagramSimplify: Finished"];
        Return[Plus @@ collected]
    ];

FMakeFormMomentumExpansion::notImplemented = "FMakeFormMomentumExpansion is not yet implemented. Use FMakeFiniteTFormMomentumExpansion for finite-temperature calculations.";

FMakeFormMomentumExpansion[] :=
    Module[{},
        $standardFORMmomentumRules = {};
    ];

FMakeFormMomentumExpansion[___] :=
    (
        Message[FMakeFormMomentumExpansion::notImplemented];
        Abort[]
    );

$standardFORMmomentumRules = {}; FMakeFormMomentumExpansion[];

ClearAll[FFormSimplify]

FFormSimplify[obj_, preReplRules_ : {}, postReplRules_ : {}, bracket_ : {}] :=
    Module[{file, origVars, tmpfileName, import, repl, expr, newSymbols, momenta, momRule, ret},
        file = makeHashFile[{obj, preReplRules, postReplRules, bracket}];
        If[FileExistsQ[file],
            ret = Import[file];
            Return[ret]
        ];
        expr = obj // Rationalize;
        origVars = FormTracer`GetExtraVars[];
        repl = SafeReplaceTrace[expr];
        momenta = expr // GetAllMomenta;
        newSymbols = GetAllCustomSymbols[expr /. repl[[1]]];
        newSymbols = Select[newSymbols, FreeQ[momenta, #]&];
        momRule =
            If[Length @ momenta > 0,
                {{FormTracer`PreambleFormRule, "Vector " <> StringRiffle[momenta, ","] <> ";"}}
                ,
                {}
            ];
        FunKitDebug[3, "FFormSimplify: Adding Extra Vars ", newSymbols];
        FormTracer`AddExtraVars @@ newSymbols;
        tmpfileName = FileNameJoin[{$TemporaryDirectory, "FS_" <> makeTemporaryFileName[]}];
        FormTracer`FormTrace[Rationalize[expr /. repl[[1]]], Join[{scallDef}, momRule, preReplRules], postReplRules, {tmpfileName, "O4,saIter=10000,saMinT=10,saMaxT=10000", "fortran90"}, bracket];
        ret = ImportAndSimplifyFORM[tmpfileName];
        FunKitDebug[2, "FFormSimplify: FORM finished"];
        FormTracer`DefineExtraVars[origVars];
        Quiet[DeleteFile[tmpfileName]];
        ret = (ret) /. repl[[2]] // Rationalize;
        Export[file, ret];
        Return[ret];
    ];
