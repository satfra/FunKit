tests = {};

fields = <|"Commuting" -> {Pion[p, {f}], Sigma[p]}|>;

truncation = <|GammaN -> {{Sigma}, {Pion, Pion}, {Sigma, Sigma}, {Pion, Pion, Sigma}, {Sigma, Sigma, Sigma}, {Pion, Pion, Pion, Pion}, {Pion, Pion, Sigma, Sigma}, {Sigma, Sigma, Sigma, Sigma}}, Propagator -> {{Pion, Pion}, {Sigma, Sigma}}, Rdot -> {{Pion, Pion}, {Sigma, Sigma}}, Field -> {{Sigma}}, Phidot -> {{Sigma}, {Sigma, Sigma}, {Sigma, Sigma, Sigma}, {Pion, Pion, Sigma}, {Pion, Pion}, {Sigma, Pion, Pion}}, R -> {{Pion, Pion}, {Sigma, Sigma}}|>;

setupON = <|"FieldSpace" -> fields, "Truncation" -> truncation|>;

GeneralizedFlowEquationRHS = GeneralizedFlowEquation[[2 ;; ]];

GeneralizedFlowEquationLHS = GeneralizedFlowEquation[[1]];

fRGSigmaSigmaRHS = FTruncate[setupON, FTakeDerivatives[setupON, GeneralizedFlowEquationRHS, {Sigma[i1], Sigma[i2]}]];

AppendTo[tests, VerificationTest[MemberQ[fRGSigmaSigmaRHS, FTerm[___, Phidot[{Sigma, Sigma}, _], ___, R[{Sigma, Sigma}, _], ___]], True, TestID -> "Verify that the flowing field derivative Phidot[{Sigma, Sigma}] appears in the flow of the Sigma two-point function"]];

AppendTo[tests, VerificationTest[MemberQ[fRGSigmaSigmaRHS, FTerm[___, Phidot[{Pion, Pion}, _], ___, R[{Pion, Pion}, _], ___]], True, TestID -> "Verify that the flowing field derivative Phidot[{Pion, Pion}] appears in the flow of the Sigma two-point function"]];
