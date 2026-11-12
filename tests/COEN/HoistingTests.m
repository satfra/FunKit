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

(* Nested calls: exactly the distinct calls found by Cases become definitions, and an outer
   call's body names the inner one (see the next test), so the reconstruction needs
   ReplaceRepeated. *)
AppendTo[tests, VerificationTest[
    Module[{expr, r},
        expr = ZA[Zq[p^2]] + Zq[p^2];
        r = hoistInterp[expr];
        {r["Count"], (r["Expr"] //. Map[Rule @@ #&, r["Definitions"]]) === expr}
    ],
    {2, True},
    TestID -> "COEN-Hoist-InterpNestedReconstructs"]
];

(* A call nested in the arguments of other hoisted calls is evaluated once: every body names
   it instead of repeating it. Device compilers do not merge the repeats of e.g. sin() (nvcc
   inlines a range reduction with local-memory stores per textual call), and a GPU kernel with
   ~1000 copies of one sin() in its lookup arguments cost ptxas several times its arithmetic.
   The definitions must also stay ordered: each body may only name earlier definitions. *)
hoistOrderedQ[defs_List] :=
    And @@ MapIndexed[
        SubsetQ[defs[[;; First[#2] - 1, 1]], Intersection[defs[[All, 1]], Cases[#1[[2]], _String, {0, Infinity}]]]&, defs];

AppendTo[tests, VerificationTest[
    Module[{expr, r, bodies},
        expr = RF[k^2, l^2 - S1 Sin[phi]] + RB[k^2, l^2 + S1 Sin[phi]] + ZA[Sqrt[1 - S1 Sin[phi]]] + Sin[phi] Cos[phi];
        r = hoistInterp[expr];
        bodies = r["Definitions"][[All, 2]];
        {Count[bodies, Sin[phi], {0, Infinity}], Count[bodies, Cos[phi], {0, Infinity}],
         hoistOrderedQ[r["Definitions"]], (r["Expr"] //. Map[Rule @@ #&, r["Definitions"]]) === expr}
    ],
    {1, 1, True, True},
    TestID -> "COEN-Hoist-InterpNestedArgumentsNamed"]
];

(* The same with a shared coordinate transform: a _gix index whose argument names a hoisted
   call is defined after that call, not in front of all of them. *)
AppendTo[tests, VerificationTest[
    Block[{FunKit`Private`$codeSharedInterpolatorGrids = {{ZA, ZB}}},
        Module[{expr, r, defs},
            expr = ZA[Sqrt[1 - Sin[phi]]] + ZB[Sqrt[1 - Sin[phi]]] + ZA[p] + ZB[p];
            r = hoistInterp[expr];
            defs = r["Definitions"];
            {Count[defs[[All, 2]], Sin[phi], {0, Infinity}], Length[Select[defs[[All, 1]], StringStartsQ[#, "_gix"]&]],
             hoistOrderedQ[defs]}
        ]
    ],
    {1, 2, True},
    TestID -> "COEN-Hoist-InterpNestedGridIndexOrdered"]
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
