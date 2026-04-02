(**********************************************************************************
    util.m -- Shared internal utilities

    Internal:
      isNeg                      -- Tests if an index is negated (down)
                                    (used by FMakeDiagrammaticRules, Notation, Metric)
      makePosIdx                 -- Strips negation from an index
                                    (used broadly: AnSEL, DiRK, DiANE, FEDeriK)
      exclusions                 -- Default symbol exclusion filter
                                    (used by customExclusions)
      customExclusions           -- Extended symbol exclusion filter
                                    (used by GetAllSymbols, TRACY/Tools)
      GetAllSymbols              -- Extracts all unique symbols from an expression
                                    (used by Notation, FPrint)
      makeTemporaryFileName      -- Generates a unique temporary filename
                                    (used by Cpp, Tracing)
      ParallelMapSerialized      -- ParallelMap with BinarySerialize round-trip
                                    (used by BalancedMap, PMap)
      BalancedMap                -- Memory-efficient parallel Map over large lists
                                    (used by Truncation, Derivatives, Tracing)
      PMap                       -- Parallel Map with serialization
                                    (used by Simplify)
      balancedBracesQ            -- Checks balanced parentheses in a string
                                    (used by Cpp, CppOptimize)
      balancedRBracesQ           -- Checks balanced square brackets in a string
                                    (used by Cpp)
      hasNoOperators             -- Tests if a string contains no operators
                                    (used by Cpp)
**********************************************************************************)

(*Is an index down?*)

isNeg[-i_] :=
    True;

isNeg[i_] :=
    False;

makePosIdx[-i_] :=
    i;

makePosIdx[i_] :=
    i;

(*Getting symbols*)

exclusions[a_] :=
    And @@ {a =!= List, a =!= Complex, a =!= Plus, a =!= Power, a =!= Times}

customExclusions[a_] :=
    And @@ {a =!= List, a =!= Complex, a =!= Plus, a =!= Power, a =!= Times, a =!= Rational, a =!= Pattern, a =!= $dummy}

GetAllSymbols[expr_] :=
    Module[{obj},
        obj = DeleteDuplicates @ Cases[expr, (a_Symbol /; customExclusions[a]) | (a_Symbol[__] /; customExclusions[a]), Infinity];
        obj = DeleteDuplicates @ ((# /. a_[__] :> a)& /@ obj);
        Return[obj];
    ];

(*File handling*)

makeTemporaryFileName[] :=
    ToString[AbsoluteTime[] * 10^6 // Round] <> "_" <> ToString[RandomInteger[{10^6, 10^7}]]

(*Balanced mapping to avoid large memory consumption in parallel processing*)

ParallelMapSerialized[f_, data_, opts___] :=
    ParallelMap[f[BinaryDeserialize @ #]&, BinarySerialize /@ data, opts];

BalancedMap[f_, list_FEx] :=
    FEx @@ BalancedMap[f, List @@ list];

$ParallelSwitchByteThreshold = 10 * 10^6; (*10MB*)

BalancedMap[f_, list_List] :=
    Module[{len = Length[list], chunks, ret, mChunk},
        If[ByteCount[list] < $ParallelSwitchByteThreshold,
            Return[Map[f, list]]
        ];
        DistributeDefinitions[f];
        (*Subdivide into chunks of length 128*)
        chunks = Partition[list, UpTo[8192]];
        ret = Table[{}, {Length[chunks]}];
        For[i = 1, i <= Length[chunks], i++,
            mChunk = chunks[[i]];
            ret[[i]] = ParallelMapSerialized[f, mChunk] // Timing;
            ret[[i]] = ret[[i, 2]];
        ];
        Return[Flatten[ret, 1]]
    ];

PMap[f_, list_List] :=
    Module[{ret},
        DistributeDefinitions[f];
        ret = ParallelMapSerialized[f, list];
        Return[ret]
    ];

(*String brace balancing*)

balancedBracesQ[str_String] :=
    Module[{cases, idx},
        If[Not @ (StringCount[str, "("] === StringCount[str, ")"]),
            Return[False]
        ];
        cases = StringCases[str, "(" | ")"];
        For[idx = 1, idx <= Length[cases], idx++,
            If[(Count[cases[[ ;; idx]], "("] < Count[cases[[ ;; idx]], ")"]),
                Return[False]
            ];
        ];
        Return[True];
    ];

balancedRBracesQ[str_String] :=
    StringCount[str, "["] === StringCount[str, "]"]

hasNoOperators[str_String] :=
    StringFreeQ[str, ")"] && StringFreeQ[str, "("] && StringFreeQ[str, "["] && StringFreeQ[str, "]"] && StringFreeQ[str, "*"] && StringFreeQ[str, "/"] && StringFreeQ[str, "+"] && StringFreeQ[str, "-"] && StringFreeQ[str, "%"] && StringFreeQ[str, "&"]
