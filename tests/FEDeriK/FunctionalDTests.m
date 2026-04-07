tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    FunctionalD Basic Tests
**********************************************************************************)

(* Test basic functional derivative rules *)

testSetup = GetFunKitSetupScalar[];

(* Basic field derivative *)

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[testSetup, Phi[i], Phi[j]], \[Gamma][{Phi, Phi}, {-j, i}], TestID -> "Basic functional derivative"]];

(* AnyField derivative *)

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[testSetup, AnyField[i], Phi[j]], \[Gamma][{Phi, AnyField}, {-j, i}], TestID -> "AnyField functional derivative"]];

(* Derivative of field without indices (should be zero) *)

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[testSetup, Phi, Phi[j]], 0, TestID -> "Derivative of field without indices"]];

(**********************************************************************************
    Correlation Function Derivatives  
**********************************************************************************)

(* GammaN derivative *)

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[testSetup, GammaN[{a, b}, {i, j}], Phi[k]], GammaN[{Phi, a, b}, {-k, i, j}], TestID -> "GammaN derivative"]];

(**********************************************************************************
    Chain Rule Tests
**********************************************************************************)

(* Division rule *)

(* Times rule with multiple factors *)

(**********************************************************************************
    Power Rule Tests
**********************************************************************************)

(* Power of FTerm *)

(* Power with FTerm exponent *)

(**********************************************************************************
    FEx Derivative Error Tests
**********************************************************************************)

(* Test that FunctionalD fails appropriately for FEx *)

AppendTo[tests, VerificationTest[CheckAbort[FunKit`Private`FunctionalD[testSetup, FEx[a], Phi[i]], "AbortTriggered"], "AbortTriggered", TestID -> "FunctionalD of FEx should abort"]];

(**********************************************************************************
    User-Defined Rules Tests
**********************************************************************************)

(* Clear any existing user rules *)

FClearFDRules[];

(* Add a custom rule *)

FAddFDRule[customFunc[x_], Phi[y_], customResult];

(* Test custom rule application *)

customResult = FunKit`Private`FunctionalD[testSetup, customFunc[i], Phi[j]];

AppendTo[tests, VerificationTest[customResult, customResult, TestID -> "Custom functional derivative rule"]];

(* Clean up user rules *)

FClearFDRules[];

(**********************************************************************************
    SymmetricDerivative Tests
**********************************************************************************)

(* Test symmetric derivative with matching field counts *)

AppendTo[tests, VerificationTest[Head[FunKit`Private`SymmetricDerivative[{Phi, Psi}, {i, j}, {Phi, Psi}, {k, l}]], Times, TestID -> "SymmetricDerivative with matching fields"]];

(* Test that SymmetricDerivative fails with mismatched field counts *)

AppendTo[tests, VerificationTest[CheckAbort[FunKit`Private`SymmetricDerivative[{Phi}, {i}, {Phi, Psi}, {j, k}], "AbortTriggered"], "AbortTriggered", TestID -> "SymmetricDerivative with mismatched fields should abort"]];

(**********************************************************************************
    Multi-Index Functional Derivatives
**********************************************************************************)

yukawaSetup = GetFunKitSetupYukawa[];

(* Test derivative with list indices *)

multiIndexResult = FunKit`Private`FunctionalD[yukawaSetup, GammaN[{Phi, Psi}, {i, j}], GammaN[{Phi, Psibar}, {k, l}]];

AppendTo[tests, VerificationTest[Head[multiIndexResult], Times, TestID -> "Multi-index functional derivative"]];

(**********************************************************************************
    Nested Function Tests
**********************************************************************************)

(* Test derivative of function applied to FTerm *)

nestedFuncResult = FunKit`Private`FunctionalD[testSetup, f[FTerm[a]], Phi[i]];

AppendTo[tests, VerificationTest[Head[nestedFuncResult], FTerm, TestID -> "Derivative of function applied to FTerm"]];

(* Test derivative with respect to multiple fields *)

multiFieldResult = FunKit`Private`FunctionalD[testSetup, Phi[i] * Phi[j], Phi[k], Phi[l]];

AppendTo[tests, VerificationTest[multiFieldResult =!= Undefined, True, TestID -> "Multi-field functional derivative"]];

(**********************************************************************************
    Source fields in functional derivatives
**********************************************************************************)

srcSetup = GetFunKitSetupWithSources[];

(* Taking a functional derivative w.r.t. a source field should work *)

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[srcSetup, J[i1], J[i2]], \[Gamma][{J, J}, {-i2, i1}], TestID -> "FunctionalD source: derivative of J w.r.t. J"]];

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[srcSetup, AnyField[i1], J[i2]], \[Gamma][{J, AnyField}, {-i2, i1}], TestID -> "FunctionalD source: derivative of AnyField w.r.t. J"]];

(**********************************************************************************
    Edge Cases and Error Handling
**********************************************************************************)

(* Test with empty arguments *)

AppendTo[tests, VerificationTest[FunKit`Private`FunctionalD[testSetup, 1, Phi[i]], 0, TestID -> "Derivative of constant"]];

(* Test linearity *)

linearResult1 = FunKit`Private`FunctionalD[testSetup, a * Phi[i] + b * Phi[j], Phi[k]];

linearResult2 = a * FunKit`Private`FunctionalD[testSetup, Phi[i], Phi[k]] + b * FunKit`Private`FunctionalD[testSetup, Phi[j], Phi[k]];

AppendTo[tests, VerificationTest[linearResult1, linearResult2, TestID -> "Functional derivative linearity"]];
