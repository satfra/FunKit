tests = {};

(**********************************************************************************
    Pure FTerm tests  
**********************************************************************************)

AppendTo[tests, TestCreate[FTerm[-1, 2, 2], FTerm[-1 * 2 * 2], TestID -> "FTerm multiplication 1"]];
AppendTo[tests, TestCreate[FTerm[3, 4, 5], FTerm[3 * 4 * 5], TestID -> "FTerm multiplication 2"]];

AppendTo[tests, TestCreate[FTerm[4, Propagator[{a, b}, {c, d}], 5], FTerm[20, Propagator[{a, b}, {c, d}]], TestID -> "FTerm simplification 1"]];
AppendTo[tests, TestCreate[FTerm[2, FTerm[3, FTerm[4]]], FTerm[24], TestID -> "FTerm nested simplification 1"]];
AppendTo[tests, TestCreate[FTerm[2, Propagator[{a, b}, {c, d}], FTerm[3, Propagator[{e, f}, {g, h}]], 4], FTerm[24, Propagator[{a, b}, {c, d}], Propagator[{e, f}, {g, h}]], TestID -> "FTerm nested simplification 2"]];

AppendTo[tests, TestCreate[FTerm[], FTerm[1], TestID -> "FTerm empty"]];
AppendTo[tests, TestCreate[FTerm[5,Propagator[{a,b},{c,d}]] ** FTerm[3,Propagator[{e,f},{g,h}]], FTerm[15,Propagator[{a,b},{c,d}],Propagator[{e,f},{g,h}]], TestID -> "FTerm ** operator"]];


(**********************************************************************************
    Tests involving FEx
**********************************************************************************) 

AppendTo[tests, TestCreate[FEx[FTerm[-1, 2, 2], FTerm[3]], FEx[FTerm[-1]], TestID -> "FEx with FTerm"]];
