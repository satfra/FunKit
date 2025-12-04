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
