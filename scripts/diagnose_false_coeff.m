(* ============================================================================
   diagnose_false_coeff.m

   Pinpoints where a bare `False` enters an FTerm coefficient during FSimplify
   on the ARM64/macOS build (the "FTerm[3 False]" / "FTerm[False]" artifact seen
   in the scalar / Yang-Mills 4-point flows and the AnSEL SimplifyTests).

   The bug does NOT reproduce on x86-64 Linux, so this script instruments every
   plausible injector in the sign / coefficient / metric machinery, reports the
   first anomalies (function + inputs + full call stack), and unconditionally
   dumps the environment and the full corrupt FSimplify output — so a single run
   should be enough to locate the root cause even if a hook is missed.

   Run it on the machine that shows the failures, from the repo root, and keep
   the whole log:

       wolfram -script scripts/diagnose_false_coeff.m > false_coeff.log 2>&1

   Then send back false_coeff.log (at minimum the first ANOMALY block and the
   ENVIRONMENT / corrupt-output sections).
   ============================================================================ *)

Needs["FunKit`"];
Get[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

$diagCount = 0;
$diagMax   = 14;   (* cap reports to keep the log readable *)

(* A Boolean sitting in a MULTIPLICATIVE / embedded position (e.g. `3 False`,
   `False + x`, `FTerm[9 False]`, `{2 False, ...}`) is the bug.  A bare `False`
   or `True` returned whole is a legitimate mismatch/flag and must NOT trip. *)
embeddedBool[x_] := !FreeQ[x, True | False] && x =!= True && x =!= False;

diagReport[where_, pairs_List] :=
    If[$diagCount < $diagMax,
        $diagCount++;
        Print["\n==================== ANOMALY #", $diagCount, " @ ", where, " ===================="];
        Do[Print["  ", pairs[[k, 1]], " =\n    ", InputForm[pairs[[k, 2]]]], {k, 1, Length[pairs]}];
        Print["  ---- Stack (heads) ----"];
        Print["    ", Stack[]];
        Print["  ---- Stack (exprs, truncated) ----"];
        Print["    ", InputForm[Short[Stack[_], 20]]];
        Print["=============================================================="];
    ];

(* Re-entrant wrapper: temporarily restore the symbol's real definitions inside
   a Block, call it, inspect the result, report if `pick[result]` is a Boolean in
   an embedded position.  (Wrapping a memoized f[x]=f[x]=... symbol merely defeats
   its cache for the duration — correct, just slower; fine for these tiny flows.) *)
wrapCheck[sym_Symbol, label_, pick_] :=
    Module[{saved},
        saved = DownValues[sym];
        Unprotect[sym];
        DownValues[sym] =
            Prepend[
                saved,
                HoldPattern[sym[args___]] :>
                    Block[{sym},
                        DownValues[sym] = saved;
                        With[{res = sym[args]},
                            If[embeddedBool[pick[res]],
                                diagReport[label, {{"args", {args}}, {"result", res}}]
                            ];
                            res
                        ]
                    ]
            ];
    ];

firstOr0[r_] := If[ListQ[r], First[r], 0];

(* ---- Hooks: ordered source-first.  Because reports are numbered in execution
        order, ANOMALY #1 is the earliest = closest to the true injector. ---- *)

(* metric / commutation / symmetry-factor producers (bosonic scalar: all trivial,
   so a Boolean here would already be the smoking gun) *)
wrapCheck[FunKit`Private`CommuteSign,           "CommuteSign",           Identity];
wrapCheck[FunKit`Private`GrassOrder,            "GrassOrder",            Identity];
wrapCheck[FunKit`Private`metric,                "metric",                Identity];
wrapCheck[FunKit`Private`SymmetryFactorFromList,"SymmetryFactorFromList",Identity];

(* index / metric reduction of terms and merged coefficients *)
wrapCheck[FunKit`Private`ReduceGamma,           "ReduceGamma",           Identity];
wrapCheck[FunKit`Private`ReduceIndices,         "ReduceIndices",         Identity];
wrapCheck[FunKit`Private`ReduceIndicesBatch,    "ReduceIndicesBatch",    Identity];

(* per-term preprocessing right before the pairwise merge *)
wrapCheck[FunKit`Private`OrderFields,           "OrderFields",           Identity];
wrapCheck[FunKit`Private`FixIndices,            "FixIndices",            Identity];

(* the graph-traversal sign + the merge entry points *)
wrapCheck[FunKit`Private`RearrangeFields,       "RearrangeFields",       First];
wrapCheck[FunKit`Private`TermsEqualAndSum,      "TermsEqualAndSum",      First];
wrapCheck[FunKit`Private`TermsEqualPre,         "TermsEqualPre",         firstOr0];
wrapCheck[FunKit`Private`TermsEqualAndSumPre,   "TermsEqualAndSumPre",   Identity];
wrapCheck[FunKit`Private`matchDisconnectedTerms,"matchDisconnectedTerms",Identity];
wrapCheck[FunKit`Private`mergeCoefficientIntoTerm,"mergeCoefficientIntoTerm",Identity];

(* the coefficient choke point: Times @@ prefactor sweeps any stray False in *)
wrapCheck[FunKit`Private`SplitPrefactor,        "SplitPrefactor",        First];

(* ---- Environment (parallel + context fragility is a known suspect) ---- *)
Print["==================== ENVIRONMENT ===================="];
Print["  $Version            = ", $Version];
Print["  $SystemID           = ", $SystemID];
Print["  $ProcessorCount     = ", $ProcessorCount];
Print["  $VersionNumber      = ", $VersionNumber];
Print["  $DistributedContexts= ", $DistributedContexts];
Print["  active kernels      = ", Length[Kernels[]]];
Print["=============================================================="];

(* ---- Drivers: the confirmed-failing reproducers ---- *)
report[label_, res_] :=
    Module[{bad},
        bad = Cases[res, t_FTerm /; embeddedBool[t], Infinity];
        Print[label, ": nTerms=", Length[res], "  #tainted=", Length[bad]];
        If[Length[bad] > 0, Print["   tainted terms -> ", InputForm[Take[bad, UpTo[8]]]]];
    ];

drive[label_, setup_, derivs_] :=
    Module[{d, tr, s},
        Print["\n########## DRIVER: ", label, " ##########"];
        FSetGlobalSetup[setup];
        d  = FTakeDerivatives[setup, WetterichEquation, derivs];
        report["  after FTakeDerivatives", d];
        tr = FTruncate[d];
        report["  after FTruncate", tr];
        s  = CheckAbort[FSimplify[tr], Print["  FSimplify ABORTED"]; $Aborted];
        report["  after FSimplify", s];
        Print["  FULL FSimplify output:\n    ", InputForm[s]];
    ];

drive["scalar 4-point", GetFunKitSetupScalar[], {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}];
drive["fermion 4-point", GetFunKitSetupFourFermion[], {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}];

Print["\nDIAGNOSTIC DONE. Reported ", $diagCount, " anomalies."];
