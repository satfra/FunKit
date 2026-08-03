(* ::Package:: *)

(**********************************************************************************
    The Mathematica and C++ backends must agree on what they derive.

    Two distinct invariants, because they are not the same statement:

      1. With no symmetries (the default, $AutoBuildSymmetryList -> False) the two
         backends must produce the SAME EXPRESSION, term for term. There is no
         representative freedom here: the exact object is unique.

      2. With symmetries declared, the result is only an orbit generator, so the two
         backends may legitimately pick different representatives -- this is observed
         for the Grassmann cases. What must hold is that each symmetrises back to the
         exact object, and that they symmetrise to the same thing.

    Consequence worth remembering: under (2) a contraction that is NOT covariant
    under the declared symmetries can give different answers from the two backends,
    both of them "correct generators". That is why symmetries are opt-in.

    See SYMMETRY-REDUCTION-DESIGN.md.
**********************************************************************************)

tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

$toolchainPresent = Quiet[RunProcess[{"cmake", "--version"}]] =!= $Failed;

$cppAvailable = $toolchainPresent && Quiet[CheckAbort[Check[FSetBackendCpp[], $Failed], $Failed]] =!= $Failed;

If[$cppAvailable,
    FSetBackendMathematica[]
    ,
    Print["  [BackendAlignmentTests] C++ backend unavailable — alignment cases skipped."]
];

cppTest[body_, expected_, id_] :=
    If[$cppAvailable,
        VerificationTest[body, expected, TestID -> id]
        ,
        VerificationTest[True, True, TestID -> id <> "-SkippedNoToolchain"]
    ];

SetAttributes[cppTest, HoldAll];

(**********************************************************************************
    Helpers
**********************************************************************************)

alignTerms[ex_] := FEx @@ Cases[List @@ ex, _FTerm];

alignNeg[ex_] := FEx @@ ((FTerm[-1] ** #)& /@ (List @@ alignTerms[ex]));

(*Exact equality of two FEx, decided with FSimplify WITHOUT symmetries -- that path
  is value-preserving, so it is a sound oracle.*)

alignSameQ[setup_, a_, b_] :=
    Length @ FSimplify[setup, FEx @@ Join[List @@ alignTerms[a], List @@ alignNeg[b]]] === 0;

alignDerive[setup_, dl_, syms_] :=
    If[syms === None,
        FTruncate[FTakeDerivatives[setup, WetterichEquation, dl]]
        ,
        FTruncate[FTakeDerivatives[setup, WetterichEquation, dl, "Symmetries" -> syms]]
    ];

(*Derive the same thing on both backends; always restore the Mathematica backend.*)

alignBoth[setup_, dl_, syms_] :=
    Module[{mma, cpp},
        FSetGlobalSetup[setup];
        FSetBackendMathematica[];
        mma = alignDerive[setup, dl, syms];
        FSetBackendCpp[];
        cpp = alignDerive[setup, dl, syms];
        FSetBackendMathematica[];
        {mma, cpp}
    ];

(**********************************************************************************
    1. Default: the backends must produce identical expressions
**********************************************************************************)

alignIdenticalQ[setup_, dl_] :=
    Module[{mma, cpp},
        {mma, cpp} = alignBoth[setup, dl, None];
        alignSameQ[setup, mma, cpp]
    ];

AppendTo[tests, cppTest[
    alignIdenticalQ[GetFunKitSetupScalar[], {Phi[i1], Phi[i2]}], True,
    "Backends agree (no symmetries): scalar 2-point"]];

AppendTo[tests, cppTest[
    alignIdenticalQ[GetFunKitSetupScalar[], {Phi[i1], Phi[i2], Phi[i3]}], True,
    "Backends agree (no symmetries): scalar 3-point"]];

AppendTo[tests, cppTest[
    alignIdenticalQ[GetFunKitSetupScalar[], {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}], True,
    "Backends agree (no symmetries): scalar 4-point"]];

AppendTo[tests, cppTest[
    alignIdenticalQ[GetFunKitSetupYukawa[], {Psi[i1], Psibar[i2]}], True,
    "Backends agree (no symmetries): Yukawa fermion 2-point"]];

AppendTo[tests, cppTest[
    alignIdenticalQ[GetFunKitSetupYukawa[], {Psi[i1], Psibar[i2], Phi[i3]}], True,
    "Backends agree (no symmetries): Yukawa vertex"]];

AppendTo[tests, cppTest[
    alignIdenticalQ[GetFunKitSetupFourFermion[], {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}], True,
    "Backends agree (no symmetries): four-fermion 4-point"]];

(**********************************************************************************
    2. With symmetries: both must symmetrise back to the exact object
**********************************************************************************)

alignSymRoundTrip[setup_, dl_, syms_] :=
    Module[{mma, cpp, exact},
        FSetGlobalSetup[setup];
        FSetBackendMathematica[];
        exact = alignDerive[setup, dl, None];
        {mma, cpp} = alignBoth[setup, dl, syms];
        {
            alignSameQ[setup, alignTerms[FSymmetrise[setup, alignTerms[mma], syms]], exact],
            alignSameQ[setup, alignTerms[FSymmetrise[setup, alignTerms[cpp], syms]], exact],
            alignSameQ[setup,
                alignTerms[FSymmetrise[setup, alignTerms[mma], syms]],
                alignTerms[FSymmetrise[setup, alignTerms[cpp], syms]]]
        }
    ];

AppendTo[tests, cppTest[
    alignSymRoundTrip[GetFunKitSetupScalar[], {Phi[i1], Phi[i2], Phi[i3], Phi[i4]},
        FMakeSymmetryList[GetFunKitSetupScalar[], {Phi[i1], Phi[i2], Phi[i3], Phi[i4]}]],
    {True, True, True},
    "Backends agree after symmetrisation: scalar 4-point, full permutation group"]];

AppendTo[tests, cppTest[
    alignSymRoundTrip[GetFunKitSetupFourFermion[], {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]},
        FMakeSymmetryList[GetFunKitSetupFourFermion[], {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]}]],
    {True, True, True},
    "Backends agree after symmetrisation: four-fermion, full group"]];

AppendTo[tests, cppTest[
    alignSymRoundTrip[GetFunKitSetupFourFermion[], {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]},
        FMakeSymmetryList[FSymmetry[Symmetric, {i1, i3}, {i2, i4}]]],
    {True, True, True},
    "Backends agree after symmetrisation: four-fermion, combined exchange only"]];

AppendTo[tests, cppTest[
    alignSymRoundTrip[GetFunKitSetupFourFermion[], {Psi[i1], Psibar[i2], Psi[i3], Psibar[i4]},
        FMakeSymmetryList[FSymmetry[Antisymmetric, {i1, i3}]]],
    {True, True, True},
    "Backends agree after symmetrisation: four-fermion, single antisymmetric exchange"]];

tests
