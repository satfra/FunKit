(* ::Package:: *)

(**********************************************************************************
    Tests for AnSEL Routing module
    Covers: FRoute, FUnroute, FSetLoopMomentumName — correctness tests
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Setup: derive scalar and Yukawa flows for routing tests
**********************************************************************************)

scalarSetup = GetFunKitSetupScalar[];
yukawaSetup = GetFunKitSetupYukawa[];

(* Scalar flows — use global setup pattern like CrossTests *)
FSetGlobalSetup[scalarSetup];

scalar2ptFlow =
    FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2]}] //
    FTruncate //
    FSimplify;

scalar4ptFlow =
    FTakeDerivatives[scalarSetup, WetterichEquation, {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}] //
    FTruncate //
    FSimplify;

(* Yukawa flows *)
FSetGlobalSetup[yukawaSetup];

yukawa2ptFlow =
    FTakeDerivatives[yukawaSetup, WetterichEquation, {Psi[i1], Psibar[i2]}] //
    FTruncate //
    FSimplify;

yukawaVertexFlow =
    FTakeDerivatives[yukawaSetup, WetterichEquation, {Psi[i1], Psibar[i2], Phi[i3]}] //
    FTruncate //
    FSimplify;

(**********************************************************************************
    FRoute: scalar 2-point flow
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result},
            result = FRoute[scalarSetup, scalar2ptFlow];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point flow gives Association with 1-Loop key"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, oneLoop},
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            KeyExistsQ[oneLoop, "Expression"] && KeyExistsQ[oneLoop, "ExternalIndices"] && KeyExistsQ[oneLoop, "LoopMomenta"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point 1-Loop has Expression, ExternalIndices, LoopMomenta"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, oneLoop},
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            Head[oneLoop["Expression"]] === FEx && Length[oneLoop["LoopMomenta"]] === 1
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point has 1 loop momentum"
    ]
];

(**********************************************************************************
    FRoute: scalar 4-point flow
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result},
            result = FRoute[scalarSetup, scalar4ptFlow];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 4-point flow is 1-loop"
    ]
];

(**********************************************************************************
    FRoute: single FTerm routing
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{term, result},
            term = (List @@ scalar2ptFlow)[[1]];
            result = FRoute[scalarSetup, term];
            Head[result] === Association && KeyExistsQ[result, "Expression"] && KeyExistsQ[result, "ExternalIndices"] && KeyExistsQ[result, "LoopMomenta"]
        ]
        ,
        True
        ,
        TestID -> "FRoute FTerm: single term returns Association with correct keys"
    ]
];

(**********************************************************************************
    FRoute: external indices structure
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, oneLoop, extIdx},
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            extIdx = oneLoop["ExternalIndices"];
            (* Scalar 2-point should have exactly 2 external indices *)
            Length[extIdx] === 2
        ]
        ,
        True
        ,
        TestID -> "FRoute: scalar 2-point has 2 external indices"
    ]
];

(**********************************************************************************
    FRoute: Yukawa flows
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result},
            result = FRoute[yukawaSetup, yukawaVertexFlow];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"]
        ]
        ,
        True
        ,
        TestID -> "FRoute: Yukawa vertex flow is 1-loop"
    ]
];

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, oneLoop},
            result = FRoute[yukawaSetup, yukawa2ptFlow];
            oneLoop = result["1-Loop"];
            Length[oneLoop["LoopMomenta"]] === 1
        ]
        ,
        True
        ,
        TestID -> "FRoute: Yukawa fermion 2-point has 1 loop momentum"
    ]
];

(**********************************************************************************
    FSetLoopMomentumName: custom name propagates to routing
**********************************************************************************)

AppendTo[
    tests
    ,
    TestCreate[
        Module[{result, oneLoop, expr},
            FSetLoopMomentumName["q"];
            result = FRoute[scalarSetup, scalar2ptFlow];
            oneLoop = result["1-Loop"];
            expr = oneLoop["Expression"];
            FSetLoopMomentumName["l"]; (* restore default *)
            Not @ FreeQ[expr, q1, Infinity]
        ]
        ,
        True
        ,
        TestID -> "FSetLoopMomentumName: custom name q used in routing result"
    ]
];
