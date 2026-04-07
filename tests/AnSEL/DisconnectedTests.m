tests = {};
Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Section 1: FDisconnectedQ — scalar field theory — 8 tests
**********************************************************************************)

(* Single propagator — trivially connected *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[1, Propagator[{Phi, Phi}, {i1, i2}]]]
        ]
        ,
        False
        ,
        TestID -> "FDisconnectedQ: single propagator is connected"
    ]
];

(* Two propagators sharing an index — connected chain *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i2, i3}]]]
        ]
        ,
        False
        ,
        TestID -> "FDisconnectedQ: two propagators sharing index is connected"
    ]
];

(* Two propagators with no shared indices — disconnected *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}]]]
        ]
        ,
        True
        ,
        TestID -> "FDisconnectedQ: two propagators no shared index is disconnected"
    ]
];

(* Propagator + GammaN loop — connected *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}]]]
        ]
        ,
        False
        ,
        TestID -> "FDisconnectedQ: propagator-vertex loop is connected"
    ]
];

(* Two separate tadpole loops — disconnected *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}],
                                          Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]]]
        ]
        ,
        True
        ,
        TestID -> "FDisconnectedQ: two separate loops is disconnected"
    ]
];

(* Single object — connected *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[1, GammaN[{Phi, Phi, Phi, Phi}, {i1, i2, i3, i4}]]]
        ]
        ,
        False
        ,
        TestID -> "FDisconnectedQ: single object is connected"
    ]
];

(* Term with open indices but connected — chain with external legs *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]]
        ]
        ,
        False
        ,
        TestID -> "FDisconnectedQ: open-index chain is connected"
    ]
];

(* Term with open indices and disconnected — two separate external legs *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupScalar[];
            FDisconnectedQ[setup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2],
                                        Phi[i3], Propagator[{Phi, Phi}, {i3, i4}], Phi[i4]]]
        ]
        ,
        True
        ,
        TestID -> "FDisconnectedQ: two separate open-index chains is disconnected"
    ]
];

(**********************************************************************************
    Section 2: FDisconnectedQ — FEx overload — 2 tests
**********************************************************************************)

(* FEx with one disconnected term *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, connected, disconnected},
            setup = GetFunKitSetupScalar[];
            connected = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}]];
            disconnected = FTerm[1, Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}],
                                    Propagator[{Phi, Phi}, {i5, i6}], GammaN[{Phi, Phi}, {i5, i6}]];
            FDisconnectedQ[setup, FEx[connected, disconnected]]
        ]
        ,
        True
        ,
        TestID -> "FDisconnectedQ FEx: one disconnected term flags True"
    ]
];

(* FEx with all connected terms *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, t1, t2},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi, Phi, Phi}, {i3, i4, i5, i6}], Propagator[{Phi, Phi}, {i5, i6}]];
            FDisconnectedQ[setup, FEx[t1, t2]]
        ]
        ,
        False
        ,
        TestID -> "FDisconnectedQ FEx: all connected terms flags False"
    ]
];

(**********************************************************************************
    Section 3: FDisconnectedQ — Yukawa theory — 1 test
**********************************************************************************)

(* Disconnected diagram with mixed field types *)
AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup},
            setup = GetFunKitSetupYukawa[];
            FDisconnectedQ[setup, FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}],
                                          Propagator[{Psi, Psibar}, {i3, i4}], GammaN[{Psi, Psibar}, {i3, i4}]]]
        ]
        ,
        True
        ,
        TestID -> "FDisconnectedQ: mixed field disconnected in Yukawa"
    ]
];
