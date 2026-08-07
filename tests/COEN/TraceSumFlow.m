(* TraceSumFlow.m -- a NumTracer-shaped integrand, frozen.

   Captured from NumTracer's lambda3d(small) GENERAL-FRAME flow (the four-quark vertex at
   two-momentum (S0,S1,SPhi) kinematics), at the point where NumTracer hands the integrand to
   FunKit`MakeCppFunction. Regenerate with benchmarks/util/BuildTraceSumCache.m.

   Why this fixture exists, and why ZA4Flow.mx cannot stand in for it: a NumTracer kernel is a
   SUM OVER TRACE REFERENCES -- opaque ntRe["...tr<N>(fenv)"] calls into a separately generated
   traces header, multiplied by dressing functions of nested radical momenta. That shape is what
   makes COEN's earlySplit chop the integrand into independent sub-kernels, and it is the shape
   whose emission dominates Wolfram-side generation time on production flows (~98% of it). The
   ZA4 flow is dense polynomial algebra with few top-level terms and does not exercise it.

   The expression is inert: it needs no NumTracer, DiFfRG or FORM to load. The only heads in it
   are ntRe, RB, RBdot, RF, the dressings (ZA, dtZA, Zq, Mq, ZAqbq1) and built-ins. *)

(* Created with the Wolfram Language : www.wolfram.com *)
-1/31104*(ntRe["DiFfRG::capture_num::tr2(fenv)"]*
    (RBdot[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2]*
      (dtZA[(1 + k^6)^(1/6)] + (50.*k^6*(-ZA[(1 + k^6)^(1/6)] + 
          ZA[1.02*(1 + k^6)^(1/6)]))/(1 + k^6)))*
    ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
         (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
         (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2]]^2*
    ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
            S1*Sin[SPhi]))/2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + 
            S1*Sin[SPhi]))/2 - (S0^2*(1 + 2*S1*Sin[SPhi]))/2 - 
         (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
            Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/
          2 - 2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
           (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/
            2)]]^2)/((RB[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + l1^2*ZA[l1])^2*
    (RB[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        S0^2*(1 + 2*S1*Sin[SPhi]) - 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) - 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*ZA[(1 + k^6)^(1/6)] + 
     (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
       S0^2*(1 + 2*S1*Sin[SPhi]) - 
       l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
         Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) - 
       2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
         (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
      ZA[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
         (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
         S0^2*(1 + 2*S1*Sin[SPhi]) - 
         l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
           Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) - 
         2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
           (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/
            2)]])*
    (Mq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
          2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
            (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]]^2 + 
      (l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
       (-((RF[k^2, l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
               2 - 2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
                (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                    Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                        Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                    Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*Zq[k])/
           Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
             2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - (Sqrt[3]*l1*
                 Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                    ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[
                        2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/Sqrt[
                    S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]) - 
         Zq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
            2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
              (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                  Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                      Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                  Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])^2)^2) + 
 (ntRe["DiFfRG::capture_num::tr4(fenv)"]*
   (RBdot[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2]*
     (dtZA[(1 + k^6)^(1/6)] + (50.*k^6*(-ZA[(1 + k^6)^(1/6)] + 
         ZA[1.02*(1 + k^6)^(1/6)]))/(1 + k^6)))*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
        (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2]]^2*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
           S1*Sin[SPhi]))/2 - (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
           Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2]]^2)/
  (62208*(RB[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + l1^2*ZA[l1])^3*
   (Mq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]^2 + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))*
     (-((RF[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
            l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + Sqrt[3]*Sqrt[
                S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*
                    (-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
                (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]*
          Zq[k])/Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
            2 - l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
             Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
          l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
            Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]])^
      2)*(Mq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]]^2 + 
    (l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     (-((RF[k^2, l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
            2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
              (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                  Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                      Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                  Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*Zq[k])/
         Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
           2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
             (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                 Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                     Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                 Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
          2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
            (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])^2)) - 
 (ntRe["DiFfRG::capture_num::tr3(fenv)"]*
   (RBdot[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2]*
     (dtZA[(1 + k^6)^(1/6)] + (50.*k^6*(-ZA[(1 + k^6)^(1/6)] + 
         ZA[1.02*(1 + k^6)^(1/6)]))/(1 + k^6)))*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
       (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2]]*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
       (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
       (S0^2*(1 + 2*S1*Sin[SPhi]))/2 + 
       (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2 - 
       l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
         Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
          S1*Sin[SPhi]))/2 - (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2]]*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
          S1*Sin[SPhi]))/2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
        2 - (S0^2*(1 + 2*S1*Sin[SPhi]))/2 - 
       (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2 - 
       2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
         (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])/
  (62208*(RB[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + l1^2*ZA[l1])^2*
   (RB[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
       S0^2*(1 + 2*S1*Sin[SPhi]) - 
       l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
         Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) - 
       2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
         (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*ZA[(1 + k^6)^(1/6)] + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      S0^2*(1 + 2*S1*Sin[SPhi]) - l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) - 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     ZA[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        S0^2*(1 + 2*S1*Sin[SPhi]) - 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) - 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])*
   (Mq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]^2 + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))*
     (-((RF[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
            l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + Sqrt[3]*Sqrt[
                S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*
                    (-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
                (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]*
          Zq[k])/Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
            2 - l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
             Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
          l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
            Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]])^
      2)*(Mq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]]^2 + 
    (l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     (-((RF[k^2, l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
            2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
              (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                  Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                      Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                  Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*Zq[k])/
         Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
           2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
             (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                 Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                     Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                 Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
          2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
            (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])^2)) + 
 (ntRe["DiFfRG::capture_num::tr1(fenv)"]*
   (RBdot[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2]*
     (dtZA[(1 + k^6)^(1/6)] + (50.*k^6*(-ZA[(1 + k^6)^(1/6)] + 
         ZA[1.02*(1 + k^6)^(1/6)]))/(1 + k^6)))*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
        (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2]]^2*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
           S1*Sin[SPhi]))/2 + (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
           Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2]]^2)/
  (31104*(RB[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + l1^2*ZA[l1])^3*
   (Mq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]^2 + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))*
     (-((RF[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
            l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + Sqrt[3]*Sqrt[
                S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*
                    (-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
                (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]*
          Zq[k])/Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
            2 + l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
             Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
          l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
            Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]])^
      2)*(Mq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]]^2 + 
    (l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     (-((RF[k^2, l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
            2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
              (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                  Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                      Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                  Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*Zq[k])/
         Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
           2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
             (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                 Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                     Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                 Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
          2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
            (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])^2)) + 
 (ntRe["DiFfRG::capture_num::tr8(fenv)"]*
   (RBdot[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2]*
     (dtZA[(1 + k^6)^(1/6)] + (50.*k^6*(-ZA[(1 + k^6)^(1/6)] + 
         ZA[1.02*(1 + k^6)^(1/6)]))/(1 + k^6)))*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 - (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
        (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2]]^2*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
           S1*Sin[SPhi]))/2 + (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
           Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2]]^2)/
  (62208*(RB[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + l1^2*ZA[l1])^3*
   (Mq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]^2 + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))*
     (-((RF[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
            l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + Sqrt[3]*Sqrt[
                S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*
                    (-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
                (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]*
          Zq[k])/Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
            2 + l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
             Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
          l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
            Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]])^
      2)*(Mq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]]^2 + 
    (l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     (-((RF[k^2, l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
            2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
              (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                  Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                      Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                  Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*Zq[k])/
         Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
           2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
             (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                 Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                     Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                 Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
          2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
            (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])^2)) - 
 (ntRe["DiFfRG::capture_num::tr9(fenv)"]*
   (RBdot[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + RB[k^2, l1^2]*
     (dtZA[(1 + k^6)^(1/6)] + (50.*k^6*(-ZA[(1 + k^6)^(1/6)] + 
         ZA[1.02*(1 + k^6)^(1/6)]))/(1 + k^6)))*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 - (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
       (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2]]*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
          S1*Sin[SPhi]))/2 + (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2]]*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 - (cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])])/2 + 
       (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
       (S0^2*(1 + 2*S1*Sin[SPhi]))/2 - 
       (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2 + 
       l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
         Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]*
   ZAqbq1[Sqrt[2/3]*Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + 
          S1*Sin[SPhi]))/2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
        2 - (S0^2*(1 + 2*S1*Sin[SPhi]))/2 + 
       (l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))/2 + 
       2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
         (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])/
  (62208*(RB[k^2, l1^2]*ZA[(1 + k^6)^(1/6)] + l1^2*ZA[l1])^2*
   (RB[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
       (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
       S0^2*(1 + 2*S1*Sin[SPhi]) + 
       l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
         Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) + 
       2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
         (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*ZA[(1 + k^6)^(1/6)] + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
      S0^2*(1 + 2*S1*Sin[SPhi]) + l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) + 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     ZA[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 - 
        S0^2*(1 + 2*S1*Sin[SPhi]) + 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])) + 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])*
   (Mq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
          Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
           (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
             Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]]^2 + 
    (l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
        Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
         (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
              S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
           Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)])))*
     (-((RF[k^2, l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
            l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + Sqrt[3]*Sqrt[
                S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*
                    (-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
                (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]*
          Zq[k])/Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/
            2 + l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
             Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 - Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
          l1*(-(cos1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) + 
            Sqrt[3]*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*Sqrt[
                ((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*Cos[2*SPhi])] + 
              (cos1*S0^2*S1*Cos[SPhi])/Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))]])^
      2)*(Mq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
        2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
          (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
            (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + 
                 S1^2*Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
              Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]]^2 + 
    (l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
      2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
        (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*
          (Sqrt[2]*cos2*Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
            Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2))*
     (-((RF[k^2, l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
            2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
              (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                  Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                      Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                  Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]*Zq[k])/
         Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
           2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
             (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                 Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                     Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                 Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]) - 
       Zq[Sqrt[l1^2 + (S0^2*(2 + Sqrt[3]*S1*Cos[SPhi] + S1*Sin[SPhi]))/2 + 
          2*(-1/2*(cos1*l1*Sqrt[S0^2*(1 - S1*Sin[SPhi])]) - 
            (Sqrt[3]*l1*Sqrt[S0^2*(1 + S1*Sin[SPhi])]*(Sqrt[2]*cos2*
                Sqrt[((-1 + cos1^2)*(-1 + S1^2))/(2 - S1^2 + S1^2*
                    Cos[2*SPhi])] + (cos1*S0^2*S1*Cos[SPhi])/
                Sqrt[S0^4*(1 - S1^2*Sin[SPhi]^2)]))/2)]])^2))
