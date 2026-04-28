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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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
    VerificationTest[
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

(**********************************************************************************
    FSetRoutingAlgorithm["Regulator"]: regulator carries pure loop momentum {l, -l}
    (no external momenta leak into Rdot via chained vertex conservation)
**********************************************************************************)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, rdotInstances},
            FSetRoutingAlgorithm["Regulator"];
            result = FRoute[scalarSetup, scalar4ptFlow];
            FSetRoutingAlgorithm["Default"];
            expr = result["1-Loop"]["Expression"];
            rdotInstances = Cases[expr, _Rdot, Infinity];
            Length[rdotInstances] > 0 && FreeQ[rdotInstances, p1] && FreeQ[rdotInstances, p2] && FreeQ[rdotInstances, p3]
        ]
        ,
        True
        ,
        TestID -> "FRoute Regulator: scalar 4-point Rdot is free of external momenta"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, rdotInstances},
            FSetRoutingAlgorithm["Regulator"];
            result = FRoute[yukawaSetup, yukawaVertexFlow];
            FSetRoutingAlgorithm["Default"];
            expr = result["1-Loop"]["Expression"];
            rdotInstances = Cases[expr, _Rdot, Infinity];
            Length[rdotInstances] > 0 && FreeQ[rdotInstances, p1] && FreeQ[rdotInstances, p2] && FreeQ[rdotInstances, p3]
        ]
        ,
        True
        ,
        TestID -> "FRoute Regulator: Yukawa vertex Rdot is free of external momenta (fermionic)"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result},
            FSetRoutingAlgorithm["Regulator"];
            result = FRoute[scalarSetup, scalar4ptFlow];
            FSetRoutingAlgorithm["Default"];
            Head[result] === Association && KeyExistsQ[result, "1-Loop"] && Length[result["1-Loop"]["LoopMomenta"]] === 1
        ]
        ,
        True
        ,
        TestID -> "FRoute Regulator: scalar 4-point still yields valid 1-loop association"
    ]
];

(* Behavioural assertion: in Regulator mode, every Rdot's two leg-momenta sum to zero
   AND involve no external momentum — i.e. the regulator carries a pure loop pair {x, -x}. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, rdotInstances, momPairs, externalSyms},
            FSetRoutingAlgorithm["Regulator"];
            result = FRoute[yukawaSetup, yukawaVertexFlow];
            FSetRoutingAlgorithm["Default"];
            expr = result["1-Loop"]["Expression"];
            rdotInstances = Cases[expr, _Rdot, Infinity];
            (* NotationA: Rdot[{fields}, {leg1Idx, leg2Idx}], each legIdx is {momentum, grpIdx, ...}. *)
            momPairs = (#[[2, All, 1]])& /@ rdotInstances;
            externalSyms = {p1, p2, p3};
            Length[momPairs] > 0 &&
                AllTrue[momPairs, Simplify[Total[#]] === 0&] &&
                AllTrue[momPairs, FreeQ[#, Alternatives @@ externalSyms]&]
        ]
        ,
        True
        ,
        TestID -> "FRoute Regulator: Yukawa vertex Rdot legs sum to zero and have no external momenta"
    ]
];

(* Discriminating sanity: Default mode for the Yukawa vertex flow DOES leak p1 (or p2)
   into at least one Rdot — confirms Regulator mode is doing meaningful work, not
   passing vacuously. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{result, expr, rdotInstances},
            (* Default mode — explicitly assert no algorithm change is in effect *)
            FSetRoutingAlgorithm["Default"];
            result = FRoute[yukawaSetup, yukawaVertexFlow];
            expr = result["1-Loop"]["Expression"];
            rdotInstances = Cases[expr, _Rdot, Infinity];
            Or @@ (Not @ FreeQ[rdotInstances, #]& /@ {p1, p2, p3})
        ]
        ,
        True
        ,
        TestID -> "FRoute Default sanity: Yukawa vertex Rdot does carry external momenta"
    ]
];
