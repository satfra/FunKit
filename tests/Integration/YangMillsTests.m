tests = {};

Module[{fields, truncation, bases, setup, dseA, dseAA},
    fields = <|"Commuting" -> {A[p, {v, c}]}, "Grassmann" -> {{cb[p, {c}], c[p, {c}]}}|>;
    truncation = <|GammaN -> {{A, A}, {A, A, A}, {A, A, A, A}, {A, cb, c}, {cb, c}}, Propagator -> {{A, A}, {cb, c}}, Rdot -> {{A, A}, {cb, c}}, S -> {{A, A}, {A, A, A}, {A, A, A, A}, {cb, c}, {cb, c, A}}, Field -> {{}}|>;
    bases = <|GammaN -> {{A, A} -> {"AA", 1}, {A, A, A} -> "AAAClass", {A, A, A, A} -> "AAAAClass", {A, cb, c} -> {"Acbc", 1}, {cb, c} -> "cbc"}, S -> {{A, A} -> {"AA", 1}, {A, A, A} -> "AAAClass", {A, A, A, A} -> "AAAAClass", {A, cb, c} -> {"Acbc", 1}, {cb, c} -> "cbc"}, Propagator -> {{A, A} -> "AA", {cb, c} -> "cbc"}, Rdot -> {{A, A} -> {"AA", 1}, {cb, c} -> "cbc"}|>;
    setup = <|"FieldSpace" -> fields, "Truncation" -> truncation, "FeynmanRules" -> bases|>;
    dseA = FMakeDSE[setup, A[i1]];
    dseAA = FTruncate[setup, FTakeDerivatives[setup, dseA, {A[i2]}]];
    AppendTo[tests, VerificationTest[Length[dseA] > 4 && Head[dseA] === FEx, True, TestID -> "YangMills integration test: dseA has been generated"]];
    AppendTo[tests, VerificationTest[Length[dseAA] > 4 && Head[dseAA] === FEx, True, TestID -> "YangMills integration test: dseAA has been generated"]];
];
