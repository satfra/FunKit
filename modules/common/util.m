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
    Module[{ret, forceParallel},
        ret = List @@ list;
        forceParallel = Total[Length /@ ret] > 10;
        ret = BalancedMap[f, ret, forceParallel];
        Return[FEx @@ ret];
    ];

BalancedMap[f_, list_List, forceParallel_:False] :=
    Module[{len = Length[list], chunks, ret, mChunk},
        DistributeDefinitions[f];
        (*Subdivide into chunks of length 128*)
        chunks = Partition[list, UpTo[8192]];
        ret = Table[{}, {Length[chunks]}];
        For[i = 1, i <= Length[chunks], i++,
            mChunk = chunks[[i]];
            ret[[i]] = ParallelMapSerialized[f, mChunk] // Timing;
            ret[[i]] = ret[[i, 2]];
        ];
        Return[Flatten[ret]]
    ];
