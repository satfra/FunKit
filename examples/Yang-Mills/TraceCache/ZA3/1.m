(* Created with the Wolfram Language : www.wolfram.com *)
-1/11*(FunKit`dressing[FunKit`GammaN, {A, cb, c}, 1, {p1, lf1 - p1, -lf1}]*
   FunKit`dressing[FunKit`GammaN, {A, cb, c}, 1, 
    {-p1 - p2, lf1, -lf1 + p1 + p2}]*FunKit`dressing[FunKit`GammaN, 
    {A, cb, c}, 1, {p2, lf1 - p1 - p2, -lf1 + p1}]*
   FunKit`dressing[FunKit`Rdot, {cb, c}, 1, {lf1, -lf1}]*
   (2*(-2*cos[p1, lf1]^3 - 3*cos[p1, lf1]^2*cos[p2, lf1] + 
      3*cos[p1, lf1]*cos[p2, lf1]^2 + 2*cos[p2, lf1]^3)*sp[lf1, lf1]^(3/2)*
     Sqrt[sp[p, p]] - 6*sp[lf1, lf1]*sp[p, p] + 
    (2*cos[p2, lf1]^2 + 11*cos[p1, lf1]*(cos[p1, lf1] + cos[p2, lf1]))*
     sp[lf1, lf1]*sp[p, p]))/
  (FunKit`dressing[FunKit`InverseProp, {cb, c}, 1, {lf1, -lf1}]^2*
   FunKit`dressing[FunKit`InverseProp, {cb, c}, 1, {lf1 - p1, -lf1 + p1}]*
   FunKit`dressing[FunKit`InverseProp, {cb, c}, 1, 
    {lf1 - p1 - p2, -lf1 + p1 + p2}]*sp[p, p])
