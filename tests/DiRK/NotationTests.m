tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(* YM-like setup with FeynmanRules specified directly *)
ymSetup = <|
    "FieldSpace" -> <|"Commuting" -> {A[p, {mu, a}]}, "Grassmann" -> {{cb[p, {a}], c[p, {a}]}}|>,
    "Truncation" -> <|Propagator -> {{A, A}}, GammaN -> {{A, A, A}, {A, cb, c}}|>,
    "FeynmanRules" -> <|
        Propagator -> {{A, A} -> "AA"},
        GammaN -> {{A, A, A} -> "AAAClass", {A, cb, c} -> {"Acbc", 1}}
    |>
|>;

(* ---- NotationA tests ---- *)

FunKit`FSetNotationA[];

rulesA = FunKit`FMakeDiagrammaticRules[ymSetup];

AppendTo[tests, TestCreate[
    Length[rulesA] > 0,
    True,
    TestID -> "NotationA: FMakeDiagrammaticRules generates non-empty rules"
]];

AppendTo[tests, TestCreate[
    MatchQ[Propagator[{A, A}, {i1, i2}], rulesA[[1, 1]]],
    True,
    TestID -> "NotationA: Propagator rule LHS matches NotationA expression"
]];

AppendTo[tests, TestCreate[
    MatchQ[GammaN[{A, A, A}, {i1, i2, i3}], rulesA[[2, 1]]],
    True,
    TestID -> "NotationA: GammaN rule LHS matches NotationA expression"
]];

(* ---- NotationB tests ---- *)

FunKit`FSetNotationB[];

rulesB = FunKit`FMakeDiagrammaticRules[ymSetup];


AppendTo[tests, TestCreate[
    Length[rulesB] > 0,
    True,
    TestID -> "NotationB: FMakeDiagrammaticRules generates non-empty rules"
]];

AppendTo[tests, TestCreate[
    MatchQ[Propagator[A[i1], A[i2]], rulesB[[1, 1]]],
    True,
    TestID -> "NotationB: Propagator rule LHS matches NotationB expression"
]];

AppendTo[tests, TestCreate[
    MatchQ[GammaN[A[i1], A[i2], A[i3]], rulesB[[2, 1]]],
    True,
    TestID -> "NotationB: GammaN rule LHS matches NotationB expression"
]];

(* Restore NotationA *)
FunKit`FSetNotationA[];
