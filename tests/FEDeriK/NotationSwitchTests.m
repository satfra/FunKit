tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

testSetup = GetFunKitSetupScalar[];

(* Shorthand for private symbols used throughout this file *)

makeObj = FunKit`Private`makeObj;

getField = FunKit`Private`getField;

getFields = FunKit`Private`getFields;

getIndex = FunKit`Private`getIndex;

getIndices = FunKit`Private`getIndices;

setField = FunKit`Private`setField;

(**********************************************************************************
    NotationB: constructor and accessor tests
    Expressions are wrapped in With[{res = ...}, ...] to force eager evaluation
    before VerificationTest evaluates them.
***********************************************************************************)

FSetNotationB[];

With[{res = makeObj[Propagator, {Phi, Chi}, {i1, i2}]},
    AppendTo[tests, VerificationTest[res, Propagator[Phi[i1], Chi[i2]], TestID -> "NotationB: makeObj Propagator"]]
];

Module[{prop = Propagator[Phi[i1], Chi[i2]]},
    With[{r = getField[prop, 1]},
        AppendTo[tests, VerificationTest[r, Phi, TestID -> "NotationB: getField 1"]]
    ];
    With[{r = getField[prop, 2]},
        AppendTo[tests, VerificationTest[r, Chi, TestID -> "NotationB: getField 2"]]
    ];
    With[{r = getFields[prop]},
        AppendTo[tests, VerificationTest[r, {Phi, Chi}, TestID -> "NotationB: getFields"]]
    ];
    With[{r = getIndex[prop, 1]},
        AppendTo[tests, VerificationTest[r, i1, TestID -> "NotationB: getIndex 1"]]
    ];
    With[{r = getIndex[prop, 2]},
        AppendTo[tests, VerificationTest[r, i2, TestID -> "NotationB: getIndex 2"]]
    ];
    With[{r = getIndices[prop]},
        AppendTo[tests, VerificationTest[r, {i1, i2}, TestID -> "NotationB: getIndices"]]
    ];
    With[{r = setField[prop, 1, Psi]},
        AppendTo[tests, VerificationTest[r, Propagator[Psi[i1], Chi[i2]], TestID -> "NotationB: setField"]]
    ];
];

With[{res = makeObj[FMinus, {Phi, Chi}, {i, j}]},
    AppendTo[tests, VerificationTest[res, FMinus[Phi[i], Chi[j]], TestID -> "NotationB: FMinus uses field[index] notation"]]
];

(* FunctionalD produces \[Gamma] in NotationB format *)

With[{actual = FunKit`Private`FunctionalD[testSetup, Phi[x], Phi[y]], expected = makeObj[\[Gamma], {Phi, Phi}, {-y, x}]},
    AppendTo[tests, VerificationTest[actual, expected, TestID -> "NotationB: FunctionalD gamma in NotationB format"]]
];

(* makeObj for GammaN with three fields *)

With[{res = makeObj[GammaN, {Phi, Phi, Phi}, {i1, i2, i3}]},
    AppendTo[tests, VerificationTest[res, GammaN[Phi[i1], Phi[i2], Phi[i3]], TestID -> "NotationB: makeObj GammaN"]]
];

(* FMinus: negative index normalization *)

With[{res = FMinus[Phi[-i], Chi[j]]},
    AppendTo[tests, VerificationTest[res, FMinus[Phi[i], Chi[j]], TestID -> "NotationB: FMinus negative index normalization"]]
];

(* FMinus power rules *)

With[{res = FMinus[Phi[i], Chi[j]] ^ 2},
    AppendTo[tests, VerificationTest[res, 1, TestID -> "NotationB: FMinus squared is 1"]]
];

With[{res = FMinus[Phi[i], Chi[j]] ^ 3},
    AppendTo[tests, VerificationTest[res, FMinus[Phi[i], Chi[j]], TestID -> "NotationB: FMinus cubed is FMinus"]]
];

(* orderedObjectQ: recognises valid NotationB objects and rejects NotationA format *)

With[{res = FunKit`Private`orderedObjectQ[Propagator[Phi[i1], Chi[i2]]]},
    AppendTo[tests, VerificationTest[res, True, TestID -> "NotationB: orderedObjectQ valid object"]]
];

With[{res = FunKit`Private`orderedObjectQ[Propagator[{Phi, Chi}, {i1, i2}]]},
    AppendTo[tests, VerificationTest[res, False, TestID -> "NotationB: orderedObjectQ rejects NotationA format"]]
];

(* PrototypeObjectPattern returns a Blank pattern in NotationB *)

With[{res = FunKit`Private`PrototypeObjectPattern[Propagator]},
    AppendTo[tests, VerificationTest[res, Propagator[__], TestID -> "NotationB: PrototypeObjectPattern is Propagator[__]"]]
];

(* FTakeDerivatives: result is an FEx in NotationB format with all FDOp resolved.
   Use makeObj to build a NotationB-format input. *)

With[{res = FEvaluate[FTakeDerivatives[testSetup, FEx[FTerm[makeObj[GammaN, {Phi, Phi}, {i1, i2}]]], {Phi[k]}]]},
    AppendTo[tests, VerificationTest[Head[res] === FEx && FreeQ[res, FDOp, Infinity], True, TestID -> "NotationB: FTakeDerivatives returns FEx with no unresolved FDOp"]]
];

(* FTruncate: AnyField expansion and truncation must work in NotationB *)

With[{res = FTruncate[testSetup, FEx[FTerm[1/2,
    Propagator[AnyField[i1], AnyField[i2]],
    Rdot[AnyField[-i1], AnyField[-i2]]]]]},
    AppendTo[tests, VerificationTest[
        Head[res] === FEx && res =!= FEx[] && FreeQ[res, AnyField, Infinity],
        True,
        TestID -> "NotationB: FTruncate AnyField Propagator*Rdot survives truncation"
    ]]
];

(* Field truncation: Field -> {{}} kills bare fields in NotationB *)

With[{res = Module[{fieldSetup = testSetup},
    fieldSetup["Truncation", Field] = {{}};
    FTruncate[fieldSetup, FEx[FTerm[Phi[i1], makeObj[GammaN, {Phi, Phi}, {-i1, i2}]]]]]},
    AppendTo[tests, VerificationTest[
        res === FEx[],
        True,
        TestID -> "NotationB: FTruncate Field empty key kills bare fields"
    ]]
];

(* Field truncation: Field -> {{Phi}} keeps bare Phi fields in NotationB *)

With[{res = Module[{fieldSetup = testSetup},
    fieldSetup["Truncation", Field] = {{Phi}};
    FTruncate[fieldSetup, FEx[FTerm[Phi[i1], makeObj[GammaN, {Phi, Phi}, {-i1, i2}]]]]]},
    AppendTo[tests, VerificationTest[
        res =!= FEx[],
        True,
        TestID -> "NotationB: FTruncate Field Phi key keeps bare Phi"
    ]]
];

(**********************************************************************************
    Restore NotationA and verify
***********************************************************************************)

FSetNotationA[];

AppendTo[tests, VerificationTest[makeObj[Propagator, {Phi, Chi}, {i1, i2}], Propagator[{Phi, Chi}, {i1, i2}], TestID -> "NotationA restored: makeObj uses two-list"]];

AppendTo[tests, VerificationTest[getFields[Propagator[{Phi, Chi}, {i1, i2}]], {Phi, Chi}, TestID -> "NotationA restored: getFields extracts first list"]];

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[testSetup, Phi[x], Phi[y]], \[Gamma][{Phi, Phi}, {-y, x}], TestID -> "NotationA restored: FunctionalD gamma in NotationA format"]];
