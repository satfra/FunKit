(* ::Package:: *)

(**********************************************************************************
    Tests for FSimplify on disconnected FTerms
    Covers: matchDisconnectedTerms, candidateBijections, grassmannPermutationSign

    The FSimplify pairwise loop dispatches to matchDisconnectedTerms when either
    term in a comparison is flagged disconnected (PrecomputeTermData["disconnected"]).
    The matcher partitions both into connected components, enumerates fingerprint-
    respecting bijections, and runs the connected-case TermsEqualPre per component
    pair, accumulating the Grassmann permutation sign.
**********************************************************************************)

tests = {};
Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Section 1: hand-crafted unit tests on FEx[FTerm, FTerm]
**********************************************************************************)

(* Two reordered scalar tadpoles, both bosonic — merge to one term, coeff 2. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, t1, t2, result, factor},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {a3, a4}], GammaN[{Phi, Phi}, {a3, a4}], Propagator[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi}, {a1, a2}]];
            result = FSimplify[setup, FEx[t1, t2]];
            factor = First @ FunKit`Private`SplitPrefactor[setup, result[[1]]];
            {Length[result], factor}
        ]
        ,
        {1, 2}
        ,
        TestID -> "FSimplify disconnected: two reordered tadpoles merge with coeff 2"
    ]
];

(* Three identical bosonic tadpoles, three reordered FTerms — merge to one,
   coeff 3.  Tests the bijection enumeration. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, mkTerm, t1, t2, t3, result, factor},
            setup = GetFunKitSetupScalar[];
            mkTerm[a_, b_, c_, d_, e_, f_] := FTerm[1,
                Propagator[{Phi, Phi}, {a, b}], GammaN[{Phi, Phi}, {a, b}],
                Propagator[{Phi, Phi}, {c, d}], GammaN[{Phi, Phi}, {c, d}],
                Propagator[{Phi, Phi}, {e, f}], GammaN[{Phi, Phi}, {e, f}]
            ];
            t1 = mkTerm[i1, i2, i3, i4, i5, i6];
            t2 = mkTerm[a3, a4, a5, a6, a1, a2];      (* shifted *)
            t3 = mkTerm[b5, b6, b1, b2, b3, b4];      (* another shift *)
            result = FSimplify[setup, FEx[t1, t2, t3]];
            factor = First @ FunKit`Private`SplitPrefactor[setup, result[[1]]];
            {Length[result], factor}
        ]
        ,
        {1, 3}
        ,
        TestID -> "FSimplify disconnected: three reordered identical tadpoles merge with coeff 3"
    ]
];

(* Different number of components must not merge (already covered in
   SimplifyTests, but repeated here as a sanity check for this file). *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, twoComp, threeComp, result},
            setup = GetFunKitSetupScalar[];
            twoComp = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]];
            threeComp = FTerm[1,
                Propagator[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi}, {a1, a2}],
                Propagator[{Phi, Phi}, {a3, a4}], GammaN[{Phi, Phi}, {a3, a4}],
                Propagator[{Phi, Phi}, {a5, a6}], GammaN[{Phi, Phi}, {a5, a6}]
            ];
            result = FSimplify[setup, FEx[twoComp, threeComp]];
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FSimplify disconnected: different component count must not merge"
    ]
];

(* Mixed connected + disconnected — connected term untouched, disconnected
   pair collapses. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, conn, disc1, disc2, result},
            setup = GetFunKitSetupScalar[];
            (* Connected 4-cycle *)
            conn = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i2, i3}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i4, i1}]];
            (* Two index-renamed reorderings of the same disconnected diagram *)
            disc1 = FTerm[1, Propagator[{Phi, Phi}, {j1, j2}], GammaN[{Phi, Phi}, {j1, j2}], Propagator[{Phi, Phi}, {j3, j4}], GammaN[{Phi, Phi}, {j3, j4}]];
            disc2 = FTerm[1, Propagator[{Phi, Phi}, {k3, k4}], GammaN[{Phi, Phi}, {k3, k4}], Propagator[{Phi, Phi}, {k1, k2}], GammaN[{Phi, Phi}, {k1, k2}]];
            result = FSimplify[setup, FEx[conn, disc1, disc2]];
            Length[result]
        ]
        ,
        2
        ,
        TestID -> "FSimplify disconnected: mixed connected + disconnected, only disconnected merges"
    ]
];

(* Idempotence on a mixed expression — running FSimplify again does nothing. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, t1, t2, once, twice},
            setup = GetFunKitSetupScalar[];
            t1 = FTerm[1, Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Propagator[{Phi, Phi}, {i3, i4}], GammaN[{Phi, Phi}, {i3, i4}]];
            t2 = FTerm[1, Propagator[{Phi, Phi}, {a3, a4}], GammaN[{Phi, Phi}, {a3, a4}], Propagator[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi}, {a1, a2}]];
            once = FSimplify[setup, FEx[t1, t2]];
            twice = FSimplify[setup, once];
            Length[once] === Length[twice] && First @ FunKit`Private`SplitPrefactor[setup, once[[1]]] === First @ FunKit`Private`SplitPrefactor[setup, twice[[1]]]
        ]
        ,
        True
        ,
        TestID -> "FSimplify disconnected: idempotent on reordered tadpoles"
    ]
];

(**********************************************************************************
    Section 2: Grassmann signs

    Yukawa setup: a single Psi-Psibar pair (one fermion-loop factor) is
    Grassmann-even; bare Psi[i] standalone is Grassmann-odd.  We test the
    permutation-sign machinery via two reordered components that each have
    even Grassmann count (loops).  Genuinely-odd-pair tests are harder to
    construct without violating FTerm::GrassmannOpen.
**********************************************************************************)

(* A bosonic loop and a fermion loop disconnected — reordered.  Each
   component is Grassmann-even per GrassmannCount (Propagator and GammaN are
   indexed objects, masked by ExtractFieldsWithIndex).  So the bijection
   permutation sign is +1.

   The expected coefficient is -2 (not +2): OrderFields canonicalises the
   GammaN field-list {Psi, Psibar} → {Psibar, Psi} for each FTerm before
   matching, picking up an inherent -1 fermion-loop factor.  Both reordered
   FTerms get the same -1; matchDisconnectedTerms then sums them to -2. *)

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, t1, t2, result, factor},
            setup = GetFunKitSetupYukawaExtended[];
            t1 = FTerm[1,
                Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}],
                Propagator[{Psi, Psibar}, {i3, i4}], GammaN[{Psi, Psibar}, {i4, i3}]
            ];
            t2 = FTerm[1,
                Propagator[{Psi, Psibar}, {a3, a4}], GammaN[{Psi, Psibar}, {a4, a3}],
                Propagator[{Phi, Phi}, {a1, a2}], GammaN[{Phi, Phi}, {a1, a2}]
            ];
            result = FSimplify[setup, FEx[t1, t2]];
            factor = First @ FunKit`Private`SplitPrefactor[setup, result[[1]]];
            {Length[result], factor}
        ]
        ,
        {1, -2}
        ,
        TestID -> "FSimplify disconnected: bosonic + fermionic loops, reordered, merge (fermion-loop sign -2)"
    ]
];

(**********************************************************************************
    Section 3: examples/CompositeOperators.nb pipeline regression

    Mirrors the notebook's exact flow: extended Yukawa setup, composite
    operator O(idx) = Psibar[idx<>"1"] Psi[idx<>"2"] (two distinct
    super-indices per operator), the phi -> Phi + Propagator·δ/δΦ
    substitution, and FResolveDerivatives -> FTruncate.

    Asserts:
      Gop1Pt = -Propagator[{Psi, Psibar}, ...]  — the chiral condensate.
      Gop2Pt has 5 terms total: 2 disconnected (the two distinct Wick
      contractions of the four fermion legs into open-leg propagators) and
      3 connected diagrams.  No leftover FMinus.  FRoute on the connected
      piece succeeds.

    The two disconnected terms share an FTermContent fingerprint but
    represent inequivalent Wick contractions (different open-index pairings
    on the external legs), so they correctly do NOT merge — verifying
    matchDisconnectedTerms doesn't over-merge inequivalent disconnected
    diagrams that happen to share head/field content.

    This is the regression test that the previous attempt at #2 failed.
**********************************************************************************)

(* Helper: build the notebook's exact setup (with `Field -> {{}}`, which sets
   sources to zero and is omitted from GetFunKitSetupYukawaExtended). *)

GetCompositeOperatorsSetup[] := Module[{p, fields, trunc},
    fields = <|
        "Commuting" -> {Phi[p]},
        "Grassmann" -> {{Psibar[p, {a}], Psi[p, {a}]}}
    |>;
    trunc = <|
        Rdot       -> {{Phi, Phi}, {Psi, Psibar}},
        Propagator -> {{Phi, Phi}, {Psi, Psibar}},
        GammaN     -> {{Phi}, {Psi, Psibar}, {Phi, Phi}, {Psi, Psibar, Phi}, {Psibar, Psibar, Psi, Psi}},
        Field      -> {{}}
    |>;
    <|"FieldSpace" -> fields, "Truncation" -> trunc|>
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, allFields, COp, phiToPhiPlusGD, oneOpRaw, oneOpExpanded, Gop1Pt},
            setup = GetCompositeOperatorsSetup[];
            FSetGlobalSetup[setup];
            allFields = {Phi, Psibar, Psi};
            COp[idx_] := FEx[FTerm[
                Psibar[Symbol[SymbolName[idx] <> "1"]],
                Psi[Symbol[SymbolName[idx] <> "2"]]
            ]];
            phiToPhiPlusGD[expr_] := Replace[expr,
                Map[(#[id_] :> Module[{i},
                    FEx[FTerm[#[id]],
                        FTerm[Propagator[{#, AnyField}, {id, i}], FDOp[AnyField[i]]]]
                ])&, allFields],
                {2}
            ];
            oneOpRaw = COp[i1];
            oneOpExpanded = oneOpRaw // phiToPhiPlusGD;
            Gop1Pt = oneOpExpanded // FResolveDerivatives[setup, #]& // FTruncate;
            (* Single FTerm: the chiral condensate -Propagator[{Psi, Psibar}, ...].
               Match by structure, not by index labels (Unique-generated). *)
            {
                Length[Gop1Pt],
                Count[Gop1Pt, _FMinus, Infinity],
                MatchQ[Gop1Pt, FEx[FTerm[-Propagator[{Psi, Psibar}, {_, _}]]]]
            }
        ]
        ,
        {1, 0, True}
        ,
        TestID -> "CompositeOperators 1pt: single chiral-condensate FTerm, no leftover FMinus"
    ]
];

AppendTo[
    tests
    ,
    VerificationTest[
        Module[{setup, allFields, COp, phiToPhiPlusGD, twoOpRaw, twoOpExpanded, Gop2Pt,
                discPart, connPart, fmCount, routedConn},
            setup = GetCompositeOperatorsSetup[];
            FSetGlobalSetup[setup];
            allFields = {Phi, Psibar, Psi};
            COp[idx_] := FEx[FTerm[
                Psibar[Symbol[SymbolName[idx] <> "1"]],
                Psi[Symbol[SymbolName[idx] <> "2"]]
            ]];
            phiToPhiPlusGD[expr_] := Replace[expr,
                Map[(#[id_] :> Module[{i},
                    FEx[FTerm[#[id]],
                        FTerm[Propagator[{#, AnyField}, {id, i}], FDOp[AnyField[i]]]]
                ])&, allFields],
                {2}
            ];
            twoOpRaw      = COp[i1] ** COp[i2];
            twoOpExpanded = twoOpRaw // phiToPhiPlusGD;
            Gop2Pt = twoOpExpanded // FResolveDerivatives[setup, #]& // FTruncate;
            discPart = Select[Gop2Pt, FunKit`FDisconnectedQ[setup, #]&];
            connPart = Select[Gop2Pt, !FunKit`FDisconnectedQ[setup, #]&];
            fmCount  = Count[Gop2Pt, _FMinus, Infinity];
            routedConn = FRoute[setup, connPart];
            {
                Length[Gop2Pt],
                Length[discPart],
                Length[connPart],
                fmCount,
                Length[routedConn["Expression"]]
            }
        ]
        ,
        {5, 2, 3, 0, 2}
        ,
        TestID -> "CompositeOperators 2pt: 5 total (2 disc + 3 conn), no FMinus, FRoute connected → 2 terms"
    ]
];
