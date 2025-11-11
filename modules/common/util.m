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

GetAllSymbols[expr_] :=
    DeleteDuplicates @ Cases[Flatten[{expr} //. Times[a_, b__] :> {a, b} //. a_Symbol[b__] /; exclusions[a] :> {a, b}], _Symbol, Infinity]
