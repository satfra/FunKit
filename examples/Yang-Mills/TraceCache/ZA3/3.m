(* Created with the Wolfram Language : www.wolfram.com *)
(-3*FunKit`dressing[FunKit`GammaN, {A, A, A}, 1, 
   {-p1 - p2, l1, -l1 + p1 + p2}]*FunKit`dressing[FunKit`GammaN, 
   {A, A, A, A}, 1, {l1 - p1 - p2, -l1, p2, p1}]*
  FunKit`dressing[FunKit`Rdot, {A, A}, 1, {-l1, l1}]*
  ((-54 + 53*cos[p1, l1]^2 + 110*cos[p1, l1]*cos[p2, l1] + 53*cos[p2, l1]^2)*
    sp[l1, l1] - (53*cos[p1, l1]^3 + 163*cos[p1, l1]^2*cos[p2, l1] + 
     cos[p2, l1]*(-54 + 53*cos[p2, l1]^2) + 
     cos[p1, l1]*(-54 + 163*cos[p2, l1]^2))*Sqrt[sp[l1, l1]]*Sqrt[sp[p, p]] + 
   33*(-1 + cos[p1, l1]^2 + 2*cos[p1, l1]*cos[p2, l1] + cos[p2, l1]^2)*
    sp[p, p]))/(11*FunKit`dressing[FunKit`InverseProp, {A, A}, 1, {l1, -l1}]^
   2*FunKit`dressing[FunKit`InverseProp, {A, A}, 1, 
   {l1 - p1 - p2, -l1 + p1 + p2}]*(sp[l1, l1] - 2*(cos[p1, l1] + cos[p2, l1])*
    Sqrt[sp[l1, l1]]*Sqrt[sp[p, p]] + sp[p, p]))
