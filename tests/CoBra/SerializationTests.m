tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Helpers
**********************************************************************************)

scalarSetup = GetFunKitSetupScalar[];

yukawaSetup = GetFunKitSetupYukawa[];

srcSetup = GetFunKitSetupWithSources[];

(*Direct access to the internal serializer: {inputAssociation, indexMap}*)

serialize[setup_, expr_, derivList_:{}, syms_:{}, stages_:<|"Truncate" -> True, "Simplify" -> True, "EmitDerivatives" -> True|>] :=
    FunKit`Private`CppSerializeInput[setup, expr, derivList, syms, stages];

stagesNoTrunc = <|"Truncate" -> False, "Simplify" -> False, "EmitDerivatives" -> False|>;

wetterich = FEx[FTerm[1/2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]];

(**********************************************************************************
    Scalar Wetterich: full fused input structure
**********************************************************************************)

Module[{input, map},
    {input, map} = serialize[scalarSetup, wetterich, {Phi[i1], Phi[i2]}];

    AppendTo[tests,
        VerificationTest[
            Keys[input]
            ,
            {"equation", "derivatives", "setup"}
            ,
            TestID -> "CoBra-Serialize-Scalar-TopLevelKeys"
        ]
    ];

    AppendTo[tests,
        VerificationTest[
            Module[{term = input["equation"][[1]]},
                {
                    Length[input["equation"]],
                    term[[1]],
                    term[[2 ;; 3, Key["type"]]],
                    term[[4]]["type"],
                    term[[5]]["type"]
                }
            ]
            ,
            {1, <|"prefactor" -> 0.5|>, {"FDOp", "FDOp"}, "Propagator", "Rdot"}
            ,
            TestID -> "CoBra-Serialize-Scalar-TermStructure"
        ]
    ];

    (*The derivative FDOp legs carry the external labels, matching "derivatives"*)
    AppendTo[tests,
        VerificationTest[
            {
                input["derivatives"],
                input["equation"][[1, 2, "legs"]],
                input["equation"][[1, 3, "legs"]]
            }
            ,
            {{{"Phi", 101}, {"Phi", 102}}, {{"Phi", 101}}, {{"Phi", 102}}}
            ,
            TestID -> "CoBra-Serialize-Scalar-ExternalLabels"
        ]
    ];

    (*Closed index pair: same label, positive on Propagator, negative on Rdot*)
    AppendTo[tests,
        VerificationTest[
            Module[{prop = input["equation"][[1, 4, "legs"]], rdot = input["equation"][[1, 5, "legs"]]},
                {prop[[All, 1]], rdot[[All, 1]], prop[[All, 2]], rdot[[All, 2]]}
            ]
            ,
            {{"AnyField", "AnyField"}, {"AnyField", "AnyField"}, {1, 2}, {-1, -2}}
            ,
            TestID -> "CoBra-Serialize-Scalar-ClosedIndexSigns"
        ]
    ];

    AppendTo[tests,
        VerificationTest[
            Module[{s = input["setup"]},
                {
                    s["cFields"],
                    KeyExistsQ[s, "gFields"],
                    s["ordered"],
                    Sort @ Keys[s["truncation"]],
                    s["truncation"]["GammaN"],
                    {s["do_truncate"], s["do_simplify"], s["in_deriv_trunc"]}
                }
            ]
            ,
            {
                {<|"Phi" -> {}|>},
                False,
                {"Rdot", "S"},
                {"GammaN", "Propagator", "Rdot", "S"},
                {{"Phi"}, {"Phi", "Phi"}, {"Phi", "Phi", "Phi"}, {"Phi", "Phi", "Phi", "Phi"}},
                {True, True, True}
            }
            ,
            TestID -> "CoBra-Serialize-Scalar-Setup"
        ]
    ];
];

(**********************************************************************************
    Yukawa: Grassmann pair order and internal index names
**********************************************************************************)

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[yukawaSetup, wetterich, {Phi[i1], Phi[i2]}];
            {input["setup"]["cFields"], input["setup"]["gFields"], Keys /@ input["setup"]["gFields"]}
        ]
        ,
        {{<|"Phi" -> {}|>}, {<|"Psibar" -> {"a"}, "Psi" -> {"a"}|>}, {{"Psibar", "Psi"}}}
        ,
        TestID -> "CoBra-Serialize-Yukawa-GrassmannPairOrder"
    ]
];

(**********************************************************************************
    Bare fields serialize as Field objects (both notations of a bare field)
**********************************************************************************)

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[scalarSetup, FEx[FTerm[2, Phi[i1], GammaN[{Phi, Phi}, {-i1, -i2}]]], {}, {}, stagesNoTrunc];
            {
                input["equation"][[1, 1]],
                input["equation"][[1, 2, "type"]],
                input["equation"][[1, 2, "legs", 1, 1]],
                input["equation"][[1, 3, "type"]]
            }
        ]
        ,
        {<|"prefactor" -> 2.|>, "Field", "Phi", "GammaN"}
        ,
        TestID -> "CoBra-Serialize-BareField"
    ]
];

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[scalarSetup, FEx[FTerm[Field[{Phi}, {i1}], GammaN[{Phi, Phi}, {-i1, -i2}]]], {}, {}, stagesNoTrunc];
            input["equation"][[1, 2, "type"]]
        ]
        ,
        "Field"
        ,
        TestID -> "CoBra-Serialize-FieldObjectNotation"
    ]
];

(**********************************************************************************
    Symmetry conversion: internal Association form -> cpplib cycles
**********************************************************************************)

AppendTo[tests,
    VerificationTest[
        Module[{syms, input},
            syms = FunKit`FMakeSymmetryList[scalarSetup, {Phi[i1], Phi[i2]}];
            input = First @ serialize[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, syms, <|"Truncate" -> True, "Simplify" -> True, "EmitDerivatives" -> False|>];
            {KeyExistsQ[input, "derivatives"], input["symmetries"]}
        ]
        ,
        (*identity dropped; the swap becomes one 2-cycle on the external labels*)
        {False, {<|"cycles" -> {{101, 102}}, "factor" -> 1|>}}
        ,
        TestID -> "CoBra-Serialize-SymmetryCycles"
    ]
];

(*A symmetry permuting different fields is not representable*)

AppendTo[tests,
    VerificationTest[
        CheckAbort[
            Quiet @ serialize[yukawaSetup, wetterich, {Psibar[i1], Psi[i2]}, {<|"Rule" -> {i1 -> i2, i2 -> i1}, "Factor" -> -1|>}, <|"Truncate" -> True, "Simplify" -> True, "EmitDerivatives" -> False|>]
            ,
            $Aborted
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Serialize-CrossFieldSymmetryAborts"
    ]
];

(**********************************************************************************
    Eligibility gate: every rejection is a hard abort
**********************************************************************************)

(*Symbolic coefficient*)

AppendTo[tests,
    VerificationTest[
        CheckAbort[
            Quiet @ serialize[scalarSetup, FEx[FTerm[Global`g, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]], {Phi[i1]}]
            ,
            $Aborted
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Gate-SymbolicCoefficient"
    ]
];

(*Complex coefficient*)

AppendTo[tests,
    VerificationTest[
        CheckAbort[
            Quiet @ serialize[scalarSetup, FEx[FTerm[I / 2, Propagator[{AnyField, AnyField}, {a, b}], Rdot[{AnyField, AnyField}, {-a, -b}]]], {Phi[i1]}]
            ,
            $Aborted
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Gate-ComplexCoefficient"
    ]
];

(*Source fields in the setup*)

AppendTo[tests,
    VerificationTest[
        CheckAbort[
            Quiet @ serialize[srcSetup, wetterich, {Phi[i1], Phi[i2]}]
            ,
            $Aborted
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Gate-SourceFields"
    ]
];

(*User-defined functional derivative rules*)

AppendTo[tests,
    VerificationTest[
        Module[{res},
            FAddFDRule[Global`X[{f1_}, {j1_}], Phi[Global`jj_], 0];
            res = CheckAbort[Quiet @ serialize[scalarSetup, wetterich, {Phi[i1], Phi[i2]}], $Aborted];
            FClearFDRules[];
            res
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Gate-UserFDRules"
    ]
];

(*Unordered-index objects (Phidot): declared as correlators with their pinned
  trailing-leg count in setup.unordered*)

AppendTo[tests,
    VerificationTest[
        Module[{setup = scalarSetup, input},
            setup["Truncation"] = Join[setup["Truncation"], <|Phidot -> {{Phi}, {Phi, Phi}}|>];
            input = First @ serialize[setup, FEx[FTerm[-1, Phidot[{AnyField}, {a}], GammaN[{AnyField}, {-a}]]], {Phi[i1]}];
            {
                MemberQ[input["setup"]["correlators"], "Phidot"],
                input["setup"]["unordered"],
                input["setup"]["truncation"]["Phidot"]
            }
        ]
        ,
        {True, <|"Phidot" -> 1|>, {{"Phi"}, {"Phi", "Phi"}}}
        ,
        TestID -> "CoBra-Serialize-UnorderedPhidot"
    ]
];

(*Metric factors (\[Gamma]) and sign objects (FMinus) are valid input -- they
  occur in untruncated results such as DSEs; products of sign objects flatten
  into individual factors*)

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[scalarSetup, FEx[FTerm[1/2, FMinus[{AnyField, Phi}, {i2, i3}] * FMinus[{AnyField, AnyField}, {i2, i2}], \[Gamma][{Phi, Phi}, {-i1, i2}], GammaN[{Phi, Phi}, {-i2, -i3}]]], {}, {}, stagesNoTrunc];
            {input["equation"][[1, 1]], input["equation"][[1, All, Key["type"]]][[2 ;;]]}
        ]
        ,
        {<|"prefactor" -> 0.5|>, {"FMinus", "FMinus", "gamma", "GammaN"}}
        ,
        TestID -> "CoBra-Serialize-GammaAndSignObjects"
    ]
];

(*A merged sum of sign factors (as FSimplify produces on untruncated results)
  is distributed back into separate terms*)

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[scalarSetup, FEx[FTerm[1/3 + FMinus[{AnyField, Phi}, {i2, i3}] / 6, GammaN[{Phi, Phi}, {-i2, -i3}]]], {}, {}, stagesNoTrunc];
            {
                Length[input["equation"]],
                input["equation"][[All, 1, "prefactor"]],
                input["equation"][[2, 2, "type"]]
            }
        ]
        ,
        {2, {1./3, 1./6}, "FMinus"}
        ,
        TestID -> "CoBra-Serialize-SignFactorSumExpands"
    ]
];

(*Non-symbol (routed/explicit) indices*)

AppendTo[tests,
    VerificationTest[
        CheckAbort[
            Quiet @ serialize[scalarSetup, FEx[FTerm[Propagator[{Phi, Phi}, {1, 2}]]], {}, {}, stagesNoTrunc]
            ,
            $Aborted
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Gate-ExplicitIndices"
    ]
];

(*Nested FDOp*)

AppendTo[tests,
    VerificationTest[
        CheckAbort[
            Quiet @ serialize[scalarSetup, FEx[FTerm[FDOp[FTerm[Phi[i1]]], GammaN[{Phi, Phi}, {-i1, -i2}]]], {}, {}, stagesNoTrunc]
            ,
            $Aborted
        ]
        ,
        $Aborted
        ,
        TestID -> "CoBra-Gate-NestedFDOp"
    ]
];

(*Field -> {{}} (drop all bare fields) has no engine counterpart: the key is
  omitted from the serialized truncation (the WL side post-filters instead)*)

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[scalarSetup, FEx[FTerm[Phi[i1], GammaN[{Phi, Phi}, {-i1, -i2}]]], {}, {}, <|"Truncate" -> True, "Simplify" -> True, "EmitDerivatives" -> False|>];
            KeyExistsQ[input["setup"]["truncation"], "Field"]
        ]
        ,
        False
        ,
        TestID -> "CoBra-Serialize-FieldDropAllBareFieldsOmitted"
    ]
];

(*...and likewise when no bare fields occur*)

AppendTo[tests,
    VerificationTest[
        Module[{input},
            input = First @ serialize[scalarSetup, wetterich, {Phi[i1], Phi[i2]}];
            KeyExistsQ[input["setup"]["truncation"], "Field"]
        ]
        ,
        False
        ,
        TestID -> "CoBra-Serialize-FieldDropAllOmitted"
    ]
];

(*An explicit Field whitelist maps through*)

AppendTo[tests,
    VerificationTest[
        Module[{setup = scalarSetup, input},
            setup["Truncation"] = Append[setup["Truncation"], Field -> {{Phi}}];
            input = First @ serialize[setup, FEx[FTerm[Phi[i1], GammaN[{Phi, Phi}, {-i1, -i2}]]], {}, {}, <|"Truncate" -> True, "Simplify" -> True, "EmitDerivatives" -> False|>];
            input["setup"]["truncation"]["Field"]
        ]
        ,
        {{"Phi"}}
        ,
        TestID -> "CoBra-Serialize-FieldWhitelist"
    ]
];

(**********************************************************************************
    Public exporters
**********************************************************************************)

AppendTo[tests,
    VerificationTest[
        Module[{file, reimported},
            file = FileNameJoin[{$TemporaryDirectory, "cobra-test-scalar.json"}];
            FExportCppInput[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, file];
            reimported = Import[file, "RawJSON"];
            DeleteFile[file];
            {
                reimported["equation"][[1, 1, "prefactor"]],
                reimported["derivatives"],
                reimported["setup", "truncation", "Rdot"]
            }
        ]
        ,
        {0.5, {{"Phi", 101}, {"Phi", 102}}, {{"Phi", "Phi"}}}
        ,
        TestID -> "CoBra-Export-JsonRoundTrip"
    ]
];

AppendTo[tests,
    VerificationTest[
        Module[{file, text},
            file = FileNameJoin[{$TemporaryDirectory, "cobra-test-scalar.toml"}];
            FExportToml[scalarSetup, wetterich, {Phi[i1], Phi[i2]}, file];
            text = Import[file, "Text"];
            DeleteFile[file];
            {
                StringContainsQ[text, "prefactor = 0.5"],
                StringContainsQ[text, "derivatives = [ [ \"Phi\", 101 ], [ \"Phi\", 102 ] ]"],
                StringContainsQ[text, "[[setup.cFields]]"],
                StringContainsQ[text, "[setup.truncation]"],
                StringContainsQ[text, "do_truncate = true"]
            }
        ]
        ,
        {True, True, True, True, True}
        ,
        TestID -> "CoBra-Export-Toml"
    ]
];

(*Grassmann pair order survives the TOML round trip: Psibar before Psi*)

AppendTo[tests,
    VerificationTest[
        Module[{file, text, pos1, pos2},
            file = FileNameJoin[{$TemporaryDirectory, "cobra-test-yukawa.toml"}];
            FExportToml[yukawaSetup, wetterich, {Phi[i1], Phi[i2]}, file];
            text = Import[file, "Text"];
            DeleteFile[file];
            pos1 = StringPosition[text, "Psibar = [ \"a\" ]"][[1, 1]];
            pos2 = StringPosition[text, "\nPsi = [ \"a\" ]"][[1, 1]];
            pos1 < pos2
        ]
        ,
        True
        ,
        TestID -> "CoBra-Export-TomlGrassmannOrder"
    ]
];
