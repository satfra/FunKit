tests = {};

(**********************************************************************************
    Global hoisting and placeholder stripping.

    These two passes dominate C++ generation on a large kernel, and both were rewritten
    for asymptotics: hoistInterpolators/hoistDivisions apply their rewrite through a
    Dispatch (a bare rule list is rescanned at every subexpression, so the rewrite cost
    was O(|expr| * |rules|) with both factors growing with the flow), and
    stripQuotedNames makes one pass with an O(1) membership test instead of one
    StringReplace rule per name.

    Both rewrites are supposed to be exactly behaviour-preserving, so the tests here
    check semantics against an independent formulation rather than against stored
    output: reconstructability for the hoists, and the naive rule-list form for the
    stripper. The last test is the asymptotic guard -- the one thing a correctness test
    cannot see, and the whole reason the rewrites exist.
**********************************************************************************)

hoistInterp = FunKit`Private`hoistInterpolators;
hoistDiv = FunKit`Private`hoistDivisions;
stripNames = FunKit`Private`stripQuotedNames;

(* the naive form stripQuotedNames used to have; the rewrite must agree with it exactly *)
stripNamesNaive[code_String, names_List] :=
    StringReplace[code, Map["\"" <> # <> "\"" -> #&, names]];

(**********************************************************************************
    stripQuotedNames
**********************************************************************************)

AppendTo[tests, VerificationTest[
    stripNames["a * \"_cse1\" + \"_interp2\"", {"_cse1", "_interp2"}],
    "a * _cse1 + _interp2",
    TestID -> "COEN-Strip-Basic"]
];

(* A quoted token that is NOT in the name list must keep its quotes -- NumTracer kernels
   carry genuine string literals (the trace-function names) right next to placeholders. *)
AppendTo[tests, VerificationTest[
    stripNames["ntRe(\"ns::tr7(fenv)\") * \"_cse1\"", {"_cse1"}],
    "ntRe(\"ns::tr7(fenv)\") * _cse1",
    TestID -> "COEN-Strip-LeavesForeignStringsQuoted"]
];

(* _cse1 is a prefix of _cse12: neither may be stripped via the other. *)
AppendTo[tests, VerificationTest[
    stripNames["\"_cse1\" + \"_cse12\" + \"_cse123\"", {"_cse12"}],
    "\"_cse1\" + _cse12 + \"_cse123\"",
    TestID -> "COEN-Strip-PrefixNamesDoNotAlias"]
];

AppendTo[tests, VerificationTest[
    stripNames["x", {}],
    "x",
    TestID -> "COEN-Strip-EmptyNameList"]
];

(* The fast path must actually be taken for the names COEN mints. WordCharacter excludes "_",
   so a guard that forgets it is byte-correct (it falls back to the rule-list form) but silently
   quadratic -- invisible to every other test here. *)
AppendTo[tests, VerificationTest[
    AllTrue[{"_cse1", "_interp23", "_den4", "_tran5"},
        StringMatchQ[#, FunKit`Private`$placeholderNameChar ..]&],
    True,
    TestID -> "COEN-Strip-MintedNamesTakeFastPath"]
];

(* Agreement with the naive form over an adversarial corpus. *)
AppendTo[tests, VerificationTest[
    Module[{names, corpus},
        names = {"_cse1", "_cse12", "_interp3", "_den0"};
        corpus = {
            "\"_cse1\"*\"_cse12\"+\"_interp3\"/\"_den0\"",
            "f(\"_cse1\", \"not_a_name\", \"_den0\")",
            "\"\" + \"_cse1\"",
            "no quotes at all",
            "\"_cse1\"\"_cse12\"",
            "powr<-2>(\"_interp3\") - \"_cse99\"",
            "ntRe(\"ns::tr1(fenv)\") + \"_den0\""};
        DeleteDuplicates @ Map[stripNames[#, names] === stripNamesNaive[#, names]&, corpus]
    ],
    {True},
    TestID -> "COEN-Strip-AgreesWithNaiveForm"]
];

(**********************************************************************************
    hoistInterpolators / hoistDivisions -- reconstructability

    Substituting each definition back for its placeholder must return the original
    expression. That is the property the whole pass rests on, and it is what a change
    of rewrite strategy could silently break.
**********************************************************************************)

AppendTo[tests, VerificationTest[
    Module[{expr, r},
        expr = ZA[p^2] * Zq[q^2] + ZA[p^2] / Zq[q^2] + dtZA[p^2];
        r = hoistInterp[expr];
        {r["Count"], (r["Expr"] /. Map[Rule @@ #&, r["Definitions"]]) === expr}
    ],
    {3, True},
    TestID -> "COEN-Hoist-InterpReconstructs"]
];

(* Nested calls: the outer call is hoisted whole, so exactly the distinct calls found by
   Cases become definitions and the reconstruction still has to be exact. *)
AppendTo[tests, VerificationTest[
    Module[{expr, r},
        expr = ZA[Zq[p^2]] + Zq[p^2];
        r = hoistInterp[expr];
        (r["Expr"] /. Map[Rule @@ #&, r["Definitions"]]) === expr
    ],
    True,
    TestID -> "COEN-Hoist-InterpNestedReconstructs"]
];

AppendTo[tests, VerificationTest[
    hoistInterp[a * b + c]["Count"],
    0,
    TestID -> "COEN-Hoist-InterpNoCalls"]
];

(* Denominators: definition bodies reference each other (an outer denominator's body carries
   the inner one's placeholder), so reconstruction needs ReplaceRepeated, not ReplaceAll. *)
AppendTo[tests, VerificationTest[
    Module[{expr, r, back},
        expr = 1/(a + b) + 1/(a + b)^2 + 1/(c + 1/(a + b));
        r = hoistDiv[expr];
        back = r["Expr"] //. Map[Rule @@ #&, r["Definitions"]];
        {r["Count"] > 0, Simplify[back - expr] === 0}
    ],
    {True, True},
    TestID -> "COEN-Hoist-DivReconstructs"]
];

(**********************************************************************************
    Asymptotic guard.

    Both rewrites were O(|expr| * |names|). The frozen NumTracer-shaped fixture is only
    a small copy of a production flow, so it is scaled here the way a real one grows:
    more trace references, the SAME dressing functions. Before the rewrites this took
    ~50 s; after, well under 10. The bound is set far above the fast path and far below
    the slow one, so it fails only on a genuine reintroduction of the quadratic, not on
    a loaded machine.
**********************************************************************************)

traceSumFlow = Get[$FunKitDirectory <> "/tests/COEN/TraceSumFlow.m"];

scaleTraceSum[n_Integer] :=
    Sum[
        traceSumFlow /. (s_String /; StringContainsQ[s, "(fenv)"]) :>
            StringReplace[s, "tr" ~~ d : DigitCharacter .. ~~ "(" :>
                "tr" <> ToString[ToExpression[d] + 1000 j] <> "("],
        {j, 1, n}
    ];

AppendTo[tests, VerificationTest[
    Module[{code},
        code = TimeConstrained[FunKit`CppCode[scaleTraceSum[150]], 30, $TimedOut];
        {code === $TimedOut, StringQ[code] && StringLength[code] > 100000}
    ],
    {False, True},
    TestID -> "COEN-Hoist-LargeKernelStaysSubQuadratic"]
];
