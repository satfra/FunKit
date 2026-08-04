(**********************************************************************************
    Serialize.m -- Serialization of setups and FExs into the C++ engine's
    input format (JSON for the machine path, TOML for human-readable exports),
    plus the eligibility checks guarding the C++ pipeline.

    Public API:
      FExportCppInput            -- Export setup + FEx (+ derivatives) as JSON input
      FExportToml                -- Export setup + FEx (+ derivatives) as TOML input

    Internal:
      CppSerializeInput          -- {expr, derivatives, symmetries, setup} ->
                                    {input Association (cpplib schema), index map}
      CppObjectData              -- Classify one FTerm factor (the per-factor gate)
      CppSymmetryToCycles        -- Internal symmetry form -> cpplib cycle form
      CppTomlString              -- input Association -> TOML text
      CppAbort                   -- Hard error, never a silent fallback

    Superindex mapping: WL index symbols become integer labels. Internal
    (closed) indices are numbered 1..n in order of first occurrence; external
    (derivative) indices get labels above Max[100, n]. The sign convention
    matches cpplib: positive = upper, negative = lower.
**********************************************************************************)

FunKit::cppUnsupported = "The C++ backend cannot handle this input: `1`.
Either switch back globally with FSetBackendMathematica[], or pass the option \"Backend\" -> \"Mathematica\" to this call.";

FunKit::cppFallback = "The C++ backend cannot handle this input: `1`. Falling back to the Mathematica implementation.";

(*Unsupported input: at the pipeline call sites (which can transparently fall
  back to the native implementation) $CppSoftFail is Block'ed to True and this
  warns and throws to the enclosing Catch; everywhere else (the exporters,
  direct serializer use) it is a hard error.*)

$CppSoftFail = False;

CppAbort[reason_String] :=
    If[TrueQ[$CppSoftFail],
        Message[FunKit::cppFallback, reason];
        Throw[$CppFallbackMarker, $CppFallbackTag]
        ,
        Message[FunKit::cppUnsupported, reason];
        Abort[]
    ];

(**********************************************************************************
    Type and field names
**********************************************************************************)

CppTypeName[\[Gamma]] = "gamma";

CppTypeName[SymmetryFactor] = "SymmFactor";

CppTypeName[head_Symbol] :=
    ToString[head];

CppLeg[field_, idx_, map_] :=
    {
        If[field === AnyField,
            "AnyField"
            ,
            ToString[field]
        ]
        ,
        If[isNeg[idx],
            -map[makePosIdx[idx]]
            ,
            map[idx]
        ]
    };

(**********************************************************************************
    Per-factor classification. Returns {typeName, fields, indices, head} or
    hard-errors on anything the C++ engine cannot represent.
**********************************************************************************)

CppObjectData[setup_, obj_] :=
    Which[
        Head[obj] === FDOp,
            If[couldBeField[First[obj]],
                {"FDOp", {Head[First[obj]]}, {First[First[obj]]}, FDOp}
                ,
                CppAbort["the nested derivative operator " <> ToString[obj, InputForm] <> " (expand it with DExpand first)"]
            ]
        ,
        MemberQ[$symmetricObjects, Head[obj]],
            CppAbort["the object " <> ToString[Head[obj]] <> ", which has symmetry rules installed via FSetSymmetricObject"]
        ,
        (*objectQ covers \[Gamma], FMinus and SymmetryFactor too -- legitimate
          input: untruncated results (e.g. DSEs) carry them on AnyField legs,
          and the engine uses the same object types internally*)
        objectQ[obj],
            Module[{fields = getFields[obj]},
                Scan[
                    If[# =!= AnyField && !MemberQ[GetAllFields[setup], #],
                        CppAbort["the unknown field " <> ToString[#] <> " in " <> ToString[obj, InputForm]]
                    ]&
                    ,
                    fields
                ];
                {CppTypeName[Head[obj]], fields, getIndices[obj], Head[obj]}
            ]
        ,
        couldBeField[obj] && MemberQ[Join[GetAllFields[setup], {AnyField}], Head[obj]],
            {"Field", {Head[obj]}, {First[obj]}, Field}
        ,
        True,
            CppAbort["the factor " <> ToString[obj, InputForm] <> " (only numeric coefficients and known objects are supported)"]
    ];

(**********************************************************************************
    Symmetries: internal Association form <|"Rule" -> {i1 -> i2, ...},
    "Factor" -> +-1|> (index -> index rules, as built by FBuildSymmetryList)
    to cpplib disjoint-cycle form. The identity symmetry is dropped (implicit
    in the engine). idxField maps each derivative index to its field, for the
    cross-field check.
**********************************************************************************)

CppSymmetryToCycles[sym_Association, map_, idxField_Association] :=
    Module[{rules, factor, perm, remaining, cycles, start, cur, cyc},
        rules = Lookup[sym, "Rule", $Failed];
        factor = Lookup[sym, "Factor", $Failed];
        If[rules === $Failed || !MatchQ[factor, 1 | -1],
            CppAbort["the symmetry " <> ToString[sym, InputForm]]
        ];
        If[rules === {},
            Return[Nothing]
        ];
        Scan[
            Module[{f1 = Lookup[idxField, makePosIdx[#[[1]]]], f2 = Lookup[idxField, makePosIdx[#[[2]]]]},
                If[MissingQ[f1] || MissingQ[f2],
                    CppAbort["the symmetry rule " <> ToString[#, InputForm] <> ", which references an index that is not a derivative leg"]
                ];
                If[f1 =!= f2,
                    CppAbort["the symmetry rule " <> ToString[#, InputForm] <> ", which permutes different fields (" <> ToString[f1] <> " and " <> ToString[f2] <> ")"]
                ];
            ]&
            ,
            rules
        ];
        perm = Association @ Map[map[makePosIdx[#[[1]]]] -> map[makePosIdx[#[[2]]]]&, rules];
        If[AnyTrue[Join[Keys[perm], Values[perm]], MissingQ] || Sort[Keys[perm]] =!= Sort[Values[perm]],
            CppAbort["the symmetry " <> ToString[sym, InputForm] <> ", whose rules do not form a permutation of known indices"]
        ];
        remaining = Keys[perm];
        cycles = {};
        While[remaining =!= {},
            start = First[remaining];
            cyc = {start};
            cur = perm[start];
            While[cur =!= start,
                AppendTo[cyc, cur];
                cur = perm[cur];
            ];
            remaining = Complement[remaining, cyc];
            If[Length[cyc] > 1,
                AppendTo[cycles, cyc]
            ];
        ];
        If[cycles === {},
            Nothing
            ,
            <|"cycles" -> cycles, "factor" -> factor|>
        ]
    ];

CppSymmetryToCycles[sym_, map_, idxField_] :=
    CppAbort["the symmetry " <> ToString[sym, InputForm]];

(**********************************************************************************
    Field-space and truncation serialization
**********************************************************************************)

CppFieldEntry[pair_List] :=
    Join[CppFieldEntry[pair[[1]]], CppFieldEntry[pair[[2]]]];

CppFieldEntry[f_Symbol[p_]] :=
    <|ToString[f] -> {}|>;

CppFieldEntry[f_Symbol[p_, inds_List]] :=
    <|ToString[f] -> (ToString /@ inds)|>;

CppFieldEntry[e_] :=
    CppAbort["the field-space entry " <> ToString[e, InputForm]];

CppSerializeTruncation[setup_, hasBareFields_, doTruncate_] :=
    Module[{trunc, keys, assoc = <||>},
        trunc = Lookup[setup, "Truncation", <||>];
        If[doTruncate && (!AssociationQ[trunc] || Length[trunc] === 0),
            Message[FTruncate::noTruncation];
            Abort[];
        ];
        If[!AssociationQ[trunc],
            Return[<||>]
        ];
        keys = Intersection[Keys[trunc], $indexedObjects];
        Scan[
            Function[key,
                Which[
                    key === Field,
                        If[trunc[Field] === {{}},
                            (*"Drop all bare fields" -- inexpressible in the engine
                              (an empty Field row means "all allowed"); emulated by
                              a post-filter on the ingested result in CppRunPipeline*)
                            Null
                            ,
                            assoc["Field"] = Map[ToString, trunc[Field], {2}];
                        ]
                    ,
                    MemberQ[{\[Gamma], SymmetryFactor}, key],
                        CppAbort["the truncation key " <> ToString[key]]
                    ,
                    True,
                        assoc[ToString[key]] = Map[ToString, trunc[key], {2}];
                ]
            ]
            ,
            keys
        ];
        assoc
    ];

(**********************************************************************************
    The main serializer:
    {input Association in the cpplib schema, WL index -> integer label map}

    stages: <|"Truncate" -> True|False, "Simplify" -> True|False,
              "EmitDerivatives" -> True|False|>
    The derivative list is prepended to every term as FDOp entries, exactly as
    the native pipeline does (Derivatives.m); "EmitDerivatives" additionally
    declares the derivative legs as an analytic graded-symmetry statement.
**********************************************************************************)

(*Distribute factor-level sums that contain sign objects over the term:
  FTerm[a, p1 + p2, b] -> {FTerm[a, p1, b], FTerm[a, p2, b]}, recursively.
  Purely scalar sums (e.g. g1 + g2) stay intact -- they are handled as
  symbolic prefactors instead.*)

CppExpandSignSums[t_FTerm] :=
    Module[{lst = List @@ t, pos},
        pos = Position[lst, p_Plus /; !AllTrue[$allObjects, FreeQ[p, #]&], {1}, 1, Heads -> False];
        If[pos === {},
            {t}
            ,
            With[{i = pos[[1, 1]]},
                Flatten[CppExpandSignSums[FTerm @@ ReplacePart[lst, i -> #]]& /@ (List @@ lst[[i]])]
            ]
        ]
    ];

(**********************************************************************************
    Symbolic prefactors. The pipeline stages are linear and the engine only
    merges terms within one run, so terms may be partitioned by their
    index-free symbolic prefactor (couplings, Z-factors, I, ...), run through
    the engine per group with the prefactor stripped, and recombined -- the
    symbolic factor never enters C++ and stays exact.
**********************************************************************************)

CppSymbolicPrefactorQ[setup_, f_] :=
    !(NumericQ[f] && Im[N[f]] == 0) &&
    AllTrue[$allObjects, FreeQ[f, #]&] &&
    FreeQ[f, FDOp] &&
    FreeQ[f, (Alternatives @@ Join[GetAllFields[setup], {AnyField}])[___]];

(*One term -> {symbolic prefactor (1 if none), term with the prefactor stripped}*)

CppExtractSymbolicPrefactor[setup_, term_FTerm] :=
    Module[{factors, sym = 1, kept = {}},
        factors = Replace[List @@ term, f_Times :> Sequence @@ (List @@ f), {1}];
        Scan[
            If[CppSymbolicPrefactorQ[setup, #],
                sym *= #
                ,
                AppendTo[kept, #]
            ]&
            ,
            factors
        ];
        {sym, FTerm @@ kept}
    ];

(*Partition a term list by symbolic prefactor: {{tag, {cleanTerms...}}, ...},
  groups ordered by first occurrence*)

CppPartitionTerms[setup_, terms_List] :=
    List @@@ Normal @ GroupBy[CppExtractSymbolicPrefactor[setup, #]& /@ terms, First -> Last];

CppValidateDerivativeList[setup_, derivList_] :=
    (
        If[!ListQ[derivList],
            CppAbort["the derivative list " <> ToString[derivList, InputForm]]
        ];
        Scan[
            Which[
                !couldBeField[#] || Head[makePosIdx[First[#]]] =!= Symbol,
                    CppAbort["the derivative " <> ToString[#, InputForm]]
                ,
                Head[#] === AnyField,
                    CppAbort["a derivative with respect to AnyField"]
                ,
                !MemberQ[GetAllFields[setup], Head[#]],
                    CppAbort["the derivative " <> ToString[#, InputForm] <> " with respect to an unknown field"]
            ]&
            ,
            derivList
        ];
        If[!DuplicateFreeQ[makePosIdx[First[#]]& /@ derivList],
            CppAbort["duplicate indices in the derivative list"]
        ];
    );

CppSerializeInput[setup_, expr_FEx, derivList_List, symmetries_List, stages_Association] :=
    Module[
        {doTrunc, doSimp, emitDerivs, terms, annotations, derivData, termData, hasBareFields, extIdx, idxList, internal, extBase, map, eqJson, symJson, truncAssoc, declHeads, corr, ordr, setupAssoc, input}
        ,
        AssertFSetup[setup];
        doTrunc = TrueQ[stages["Truncate"]];
        doSimp = TrueQ[stages["Simplify"]];
        emitDerivs = TrueQ[stages["EmitDerivatives"]];
        (*Global eligibility: concepts with no C++ counterpart*)
        If[$userRules =!= {},
            CppAbort["user-defined derivative rules (added with FAddFDRule)"]
        ];
        CppValidateDerivativeList[setup, derivList];
        {terms, annotations} = SeparateFExAnnotations[expr];
        derivData = {"FDOp", {Head[#]}, {First[#]}, FDOp}& /@ derivList;
        (*FSimplify merges terms that differ only in their symbolic sign
          factors into coefficients like 1/3 + FMinus[...]/6 -- distribute
          such factor-level sums back into separate terms (exact inverse of
          the merge)*)
        terms = Flatten[CppExpandSignSums /@ terms];
        (*Walk all terms: flatten factor-level products (so sign objects like
          FMinus multiplied together become individual factors), then classify
          every factor as either a numeric coefficient or a serializable object*)
        termData =
            Map[
                Function[term,
                    Module[{factors, coeff = 1, objs = {}},
                        factors = Replace[List @@ term, f_Times :> Sequence @@ (List @@ f), {1}];
                        Scan[
                            If[NumericQ[#],
                                If[Im[N[#]] != 0,
                                    CppAbort["the complex coefficient " <> ToString[#, InputForm] <> " in the term " <> ToString[term, InputForm]]
                                ];
                                coeff *= #;
                                ,
                                AppendTo[objs, CppObjectData[setup, #]];
                            ]&
                            ,
                            factors
                        ];
                        If[coeff == 0,
                            Nothing
                            ,
                            {N[coeff], Join[derivData, objs]}
                        ]
                    ]
                ]
                ,
                terms
            ];
        hasBareFields = AnyTrue[termData, AnyTrue[#[[2]], First[#] === "Field"&]&];
        (*Index map: internal labels 1..n by first occurrence, externals above*)
        extIdx = makePosIdx[First[#]]& /@ derivList;
        idxList = DeleteDuplicates[makePosIdx /@ Flatten[Map[#[[3]]&, Flatten[termData[[All, 2]], 1]]]];
        Scan[
            If[Head[#] =!= Symbol,
                CppAbort["the index " <> ToString[#, InputForm] <> " (only plain superindex symbols are supported)"]
            ]&
            ,
            idxList
        ];
        (*The derivative indices are not the only externally visible ones: any index the input
          expression leaves open is an external leg of the result too (the field index of an
          FMakeDSE equation, say). Those must be declared to the engine as well, or a rewriting
          step is free to rename them away -- which is what used to cost the Yang-Mills gluon DSE
          its i1 leg. An index is open in a term when it occurs exactly once there.*)
        openIdx =
            DeleteDuplicates @ Flatten @ Map[
                Function[td,
                    Module[{occ = makePosIdx /@ Flatten[Map[#[[3]]&, td[[2]]]]},
                        Cases[Tally[occ], {ix_, 1} :> ix]
                    ]
                ]
                ,
                termData
            ];
        extIdx = DeleteDuplicates @ Join[extIdx, Select[openIdx, MemberQ[idxList, #]&]];
        internal = Select[idxList, !MemberQ[extIdx, #]&];
        extBase = Max[100, Length[internal]];
        map =
            Association @ Join[
                MapIndexed[#1 -> First[#2]&, internal],
                MapIndexed[#1 -> extBase + First[#2]&, extIdx]
            ];
        (*Equation*)
        eqJson =
            Map[
                Function[td,
                    Join[
                        {<|"prefactor" -> td[[1]]|>}
                        ,
                        Map[<|"type" -> #[[1]], "legs" -> MapThread[CppLeg[#1, #2, map]&, {#[[2]], #[[3]]}]|>&, td[[2]]]
                    ]
                ]
                ,
                termData
            ];
        (*Symmetries: the index -> field lookup for the cross-field check covers
          every concrete-field leg of the equation plus the derivative list, so
          annotation symmetries on embedded FDOp legs also resolve*)
        symJson =
            Module[{idxField},
                idxField =
                    Association @ Join[
                        Reverse @ Flatten @ Map[
                            Function[triple, MapThread[If[#1 =!= AnyField, makePosIdx[#2] -> #1, Nothing]&, {triple[[2]], triple[[3]]}]]
                            ,
                            Flatten[termData[[All, 2]], 1]
                        ]
                        ,
                        makePosIdx[First[#]] -> Head[#]& /@ derivList
                    ];
                CppSymmetryToCycles[#, map, idxField]& /@ symmetries
            ];
        (*Declared object types: everything occurring in the equation or the
          truncation that is not a cpplib built-in*)
        truncAssoc = CppSerializeTruncation[setup, hasBareFields, doTrunc];
        declHeads =
            DeleteDuplicates @ Join[
                Cases[Flatten[termData[[All, 2]], 1], {_, _, _, h_Symbol} /; !MemberQ[{Propagator, GammaN, FDOp, FMinus, Field, \[Gamma], SymmetryFactor}, h] :> h]
                ,
                Select[Intersection[Keys[Lookup[setup, "Truncation", <||>]], $indexedObjects], !MemberQ[{Propagator, GammaN, Field, \[Gamma], SymmetryFactor}, #]&]
            ];
        Scan[
            If[!MemberQ[$OrderedObjects, #],
                CppAbort["the object " <> ToString[#] <> " (only ordered objects and correlation functions can be declared to the engine)"]
            ]&
            ,
            declHeads
        ];
        corr = Sort[ToString /@ Select[declHeads, MemberQ[$CorrelationFunctions, #]&]];
        ordr = Sort[ToString /@ Select[declHeads, !MemberQ[$CorrelationFunctions, #]&]];
        (*Setup block*)
        setupAssoc =
            <|
                "debug" -> Max[0, $FunKitDebugLevel - 1],
                "in_deriv_trunc" -> doTrunc,
                "do_truncate" -> doTrunc,
                "do_simplify" -> doSimp
            |>;
        If[corr =!= {},
            setupAssoc["correlators"] = corr
        ];
        If[ordr =!= {},
            setupAssoc["ordered"] = ordr
        ];
        (*The externally visible index labels: the engine must never rename these away, since they
          name the external legs of the result*)
        If[extIdx =!= {},
            setupAssoc["externals"] = Lookup[map, extIdx]
        ];
        (*Unordered trailing-leg counts (FSetUnorderedIndices, e.g. Phidot's
          pinned "field" slot) -- the engine keeps these legs in place*)
        Module[{unord},
            unord = Association @ Map[ToString[#] -> $unorderedIndices[#]&, Select[declHeads, $unorderedIndices[#] =!= 0&]];
            If[unord =!= <||>,
                setupAssoc["unordered"] = unord
            ];
        ];
        Module[{cf, gf, cs, gs},
            cf = CppFieldEntry /@ Lookup[setup["FieldSpace"], "Commuting", {}];
            gf = CppFieldEntry /@ Lookup[setup["FieldSpace"], "Grassmann", {}];
            cs = CppFieldEntry /@ Lookup[setup["FieldSpace"], "CommutingSource", {}];
            gs = CppFieldEntry /@ Lookup[setup["FieldSpace"], "GrassmannSource", {}];
            If[cf =!= {},
                setupAssoc["cFields"] = cf
            ];
            If[gf =!= {},
                setupAssoc["gFields"] = gf
            ];
            If[cs =!= {},
                setupAssoc["cSources"] = cs
            ];
            If[gs =!= {},
                setupAssoc["gSources"] = gs
            ];
        ];
        If[truncAssoc =!= <||>,
            setupAssoc["truncation"] = truncAssoc
        ];
        (*Assemble*)
        input = <|"equation" -> eqJson|>;
        If[emitDerivs && derivList =!= {},
            input["derivatives"] = {ToString[Head[#]], map[makePosIdx[First[#]]]}& /@ derivList
        ];
        If[symJson =!= {},
            input["symmetries"] = symJson
        ];
        input["setup"] = setupAssoc;
        {input, map}
    ];

(**********************************************************************************
    Public exporters
**********************************************************************************)

Options[FExportCppInput] = {"Symmetries" -> {}, "Truncate" -> Automatic, "Simplify" -> True};

FExportCppInput[setup_, expr_FTerm, rest___] :=
    FExportCppInput[setup, FEx[expr], rest];

FExportCppInput[setup_, expr_FEx, file_String, opts : OptionsPattern[]] :=
    FExportCppInput[setup, expr, {}, file, opts];

FExportCppInput[setup_, expr_FEx, derivList_List, file_String, opts : OptionsPattern[]] :=
    (
        Export[file, First @ CppExportInput[setup, expr, derivList, {opts}, FExportCppInput], "RawJSON"];
        file
    );

FExportCppInput[a___] :=
    (
        Message[FunKit::invalidArguments, FExportCppInput];
        Abort[]
    );

Options[FExportToml] = Options[FExportCppInput];

FExportToml[setup_, expr_FTerm, rest___] :=
    FExportToml[setup, FEx[expr], rest];

FExportToml[setup_, expr_FEx, file_String, opts : OptionsPattern[]] :=
    FExportToml[setup, expr, {}, file, opts];

FExportToml[setup_, expr_FEx, derivList_List, file_String, opts : OptionsPattern[]] :=
    (
        Export[file, CppTomlString @ First @ CppExportInput[setup, expr, derivList, {opts}, FExportToml], "Text"];
        file
    );

FExportToml[a___] :=
    (
        Message[FunKit::invalidArguments, FExportToml];
        Abort[]
    );

(*Shared option handling: merge explicit and annotated symmetries, resolve the
  stage flags, and serialize*)

CppExportInput[setup_, expr_FEx, derivList_, opts_List, caller_] :=
    Module[{terms, annotations, syms, stages},
        {terms, annotations} = SeparateFExAnnotations[expr];
        syms = FMergeSymmetries[OptionValue[caller, opts, "Symmetries"], Lookup[annotations, "Symmetries", {}]];
        stages =
            <|
                "Truncate" -> (OptionValue[caller, opts, "Truncate"] /. Automatic -> KeyExistsQ[setup, "Truncation"]),
                "Simplify" -> TrueQ[OptionValue[caller, opts, "Simplify"]],
                "EmitDerivatives" -> (TrueQ[$AutoBuildSymmetryList] && syms === {})
            |>;
        CppSerializeInput[setup, FEx @@ terms, derivList, syms, stages]
    ];

(**********************************************************************************
    TOML emitter over the same intermediate Association
**********************************************************************************)

CppTomlReal[x_Real] :=
    Module[{s = ToString[x, InputForm]},
        s = StringReplace[s, {".*^" -> ".0e", "*^" -> "e"}];
        Which[
            StringEndsQ[s, "."],
                s <> "0"
            ,
            !StringContainsQ[s, "." | "e"],
                s <> ".0"
            ,
            True,
                s
        ]
    ];

CppTomlValue[x_Real] :=
    CppTomlReal[x];

CppTomlValue[x_Integer] :=
    ToString[x];

CppTomlValue[True] =
    "true";

CppTomlValue[False] =
    "false";

CppTomlValue[s_String] :=
    "\"" <> s <> "\"";

CppTomlValue[l_List] :=
    If[l === {},
        "[ ]"
        ,
        "[ " <> StringRiffle[CppTomlValue /@ l, ", "] <> " ]"
    ];

(*Inline table for equation entries: {prefactor = 0.5} / {type = ..., legs = ...}*)

CppTomlValue[a_Association] :=
    "{ " <> StringRiffle[KeyValueMap[#1 <> " = " <> CppTomlValue[#2]&, a], ", "] <> " }";

CppTomlString[input_Association] :=
    Module[{lines = {}, setup},
        (*Top-level arrays must precede any [table] section*)
        AppendTo[lines, "equation = ["];
        Scan[AppendTo[lines, "  " <> CppTomlValue[#] <> ","]&, input["equation"]];
        AppendTo[lines, "]"];
        If[KeyExistsQ[input, "derivatives"],
            AppendTo[lines, ""];
            AppendTo[lines, "derivatives = " <> CppTomlValue[input["derivatives"]]];
        ];
        Scan[
            (
                AppendTo[lines, ""];
                AppendTo[lines, "[[symmetries]]"];
                AppendTo[lines, "cycles = " <> CppTomlValue[#["cycles"]]];
                AppendTo[lines, "factor = " <> CppTomlValue[#["factor"]]];
            )&
            ,
            Lookup[input, "symmetries", {}]
        ];
        setup = input["setup"];
        AppendTo[lines, ""];
        AppendTo[lines, "[setup]"];
        KeyValueMap[
            If[!MemberQ[{"cFields", "gFields", "truncation"}, #1],
                AppendTo[lines, #1 <> " = " <> CppTomlValue[#2]]
            ]&
            ,
            setup
        ];
        Scan[
            Function[entry,
                AppendTo[lines, ""];
                AppendTo[lines, "[[setup.cFields]]"];
                KeyValueMap[AppendTo[lines, #1 <> " = " <> CppTomlValue[#2]]&, entry];
            ]
            ,
            Lookup[setup, "cFields", {}]
        ];
        Scan[
            Function[entry,
                AppendTo[lines, ""];
                AppendTo[lines, "[[setup.gFields]]"];
                KeyValueMap[AppendTo[lines, #1 <> " = " <> CppTomlValue[#2]]&, entry];
            ]
            ,
            Lookup[setup, "gFields", {}]
        ];
        If[KeyExistsQ[setup, "truncation"],
            AppendTo[lines, ""];
            AppendTo[lines, "[setup.truncation]"];
            KeyValueMap[AppendTo[lines, #1 <> " = " <> CppTomlValue[#2]]&, setup["truncation"]];
        ];
        StringRiffle[lines, "\n"] <> "\n"
    ];
