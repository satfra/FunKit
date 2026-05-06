(* Regression tests for the mSTI-Yang-Mills derivative+truncation pipeline.

   The bug fixed in modules/FEDeriK/Truncation.m:651–702 (CTrunc partials loop):
   `partials` was matched on head only, ignoring index slots, so the substitution
   `partials[[qi]] -> alt` would overwrite a foreign partial-shape propagator's
   indices with the indices of `origProp` for the current pi iteration. The
   symptom was three identical Propagator[{A,A},{ci40,ci41}] in a single FTerm,
   with four orphan indices on adjacent vertices. *)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

mSTISetup = GetFunKitSetupYangMillsMSTI[];

FSetGlobalSetup[mSTISetup];

(* Smallest reproducer: the QA channel of mSTIRHS, differentiated by {A, c}.
   Pre-fix this aborted with SuperIndices::undeterminedSums + FixIndices::invalidIndices. *)

mSTIRHSQA := Module[{a, b, c1},
    a  = Symbol @ SymbolName @ Unique["a"];
    b  = Symbol @ SymbolName @ Unique["a"];
    c1 = Symbol @ SymbolName @ Unique["a"];
    FEx[FTerm[1,
        R[{AnyField, AnyField}, {-a, -b}],
        Propagator[{AnyField, AnyField}, {b, c1}],
        GammaN[{AnyField, QA}, {-c1, a}]
    ]]
];

AppendTo[
    tests,
    VerificationTest[
        Module[{res = Quiet @ Check[
            FTakeDerivatives[mSTISetup, mSTIRHSQA, {A[i1], c[i2]}] // FTruncate
            ,
            $Failed
            ,
            {FunKit`Private`SuperIndices::undeterminedSums,
             FunKit`Private`FixIndices::invalidIndices}
        ]},
            Head[res] === FEx
                && AllTrue[
                    Cases[List @@ res, _FTerm],
                    FunKit`Private`SuperIndicesValid[mSTISetup, #]&
                ]
                && FreeQ[res, FDOp, Infinity]
        ]
        ,
        True
        ,
        TestID -> "mSTI-YM regression: QA-channel Ac-derivative truncates to valid FEx"
    ]
];

(* Full mSTIRHS: all three BRST source channels (QA, Qc, Qcb), as in the
   examples/mSTI-Yang-Mills.nb cell that originally failed. *)

mSTIRHSFull := Module[{a, b, c1},
    a  = Symbol @ SymbolName @ Unique["a"];
    b  = Symbol @ SymbolName @ Unique["a"];
    c1 = Symbol @ SymbolName @ Unique["a"];
    FEx[
        FTerm[1,
            R[{AnyField, AnyField}, {-a, -b}],
            Propagator[{AnyField, AnyField}, {b, c1}],
            GammaN[{AnyField, QA}, {-c1, a}]
        ],
        FTerm[1,
            R[{AnyField, AnyField}, {-a, -b}],
            Propagator[{AnyField, AnyField}, {b, c1}],
            GammaN[{AnyField, Qc}, {-c1, a}]
        ],
        FTerm[1,
            R[{AnyField, AnyField}, {-a, -b}],
            Propagator[{AnyField, AnyField}, {b, c1}],
            GammaN[{AnyField, Qcb}, {-c1, a}]
        ]
    ]
];

AppendTo[
    tests,
    VerificationTest[
        Module[{res = Quiet @ Check[
            FTakeDerivatives[mSTISetup, mSTIRHSFull, {A[i1], c[i2]}] // FTruncate
            ,
            $Failed
            ,
            {FunKit`Private`SuperIndices::undeterminedSums,
             FunKit`Private`FixIndices::invalidIndices}
        ]},
            Head[res] === FEx
                && AllTrue[
                    Cases[List @@ res, _FTerm],
                    FunKit`Private`SuperIndicesValid[mSTISetup, #]&
                ]
                && FreeQ[res, FDOp, Infinity]
        ]
        ,
        True
        ,
        TestID -> "mSTI-YM regression: full mSTIRHS Ac-derivative truncates to valid FEx"
    ]
];
