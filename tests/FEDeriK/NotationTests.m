tests = {};

(**********************************************************************************
    Pure FTerm tests  
**********************************************************************************)

AppendTo[tests, VerificationTest[FTerm[-1, 2, 2], FTerm[-1 * 2 * 2], TestID -> "FTerm multiplication 1"]];
AppendTo[tests, VerificationTest[FTerm[3, 4, 5], FTerm[3 * 4 * 5], TestID -> "FTerm multiplication 2"]];

AppendTo[tests, VerificationTest[FTerm[4, Propagator[{a, b}, {c, d}], 5], FTerm[20, Propagator[{a, b}, {c, d}]], TestID -> "FTerm simplification 1"]];
AppendTo[tests, VerificationTest[FTerm[2, FTerm[3, FTerm[4]]], FTerm[24], TestID -> "FTerm nested simplification 1"]];
AppendTo[tests, VerificationTest[FTerm[2, Propagator[{a, b}, {c, d}], FTerm[3, Propagator[{e, f}, {g, h}]], 4], FTerm[24, Propagator[{a, b}, {c, d}], Propagator[{e, f}, {g, h}]], TestID -> "FTerm nested simplification 2"]];

AppendTo[tests, VerificationTest[FTerm[], FTerm[1], TestID -> "FTerm empty"]];
AppendTo[tests, VerificationTest[FTerm[5,Propagator[{a,b},{c,d}]] ** FTerm[3,Propagator[{e,f},{g,h}]], FTerm[15,Propagator[{a,b},{c,d}],Propagator[{e,f},{g,h}]], TestID -> "FTerm ** operator"]];

(* A numeric factor inside an inner Times must fold into the coefficient. *)
AppendTo[tests, VerificationTest[FTerm[-1, 2 Propagator[{a, b}, {c, d}] Propagator[{e, f}, {g, h}]], FTerm[-2, Propagator[{a, b}, {c, d}] Propagator[{e, f}, {g, h}]], TestID -> "FTerm Times numeric factor extraction"]];

(* Regression: a large product with a leading numeric factor must not trigger the
   exponential Orderless+Flat partition enumeration that the old Times[a_,other2_]
   rules suffered. With the bug this returns $Aborted (was multi-second / 2^N). *)
AppendTo[tests, VerificationTest[
    TimeConstrained[FTerm[-1, 2 Times @@ Table[obj[Symbol["x" <> ToString[i]]], {i, 24}]], 5] =!= $Aborted,
    True,
    TestID -> "FTerm Times extraction is not exponential"]];


(**********************************************************************************
    Tests involving FEx
**********************************************************************************) 

AppendTo[tests, VerificationTest[FEx[FTerm[-1, 2, 2], FTerm[3]], FEx[FTerm[-1]], TestID -> "FEx with FTerm"]];
