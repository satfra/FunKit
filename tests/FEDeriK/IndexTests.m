tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Setup helpers
**********************************************************************************)

sSetup = GetFunKitSetupScalar[];

ySetup = GetFunKitSetupYukawa[];

(**********************************************************************************
    GetCommutingFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetCommutingFields[sSetup], {Phi}, TestID -> "GetCommutingFields scalar: returns {Phi}"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetCommutingFields[ySetup], {Phi}, TestID -> "GetCommutingFields Yukawa: returns {Phi}"]];

(**********************************************************************************
    GetAntiCommutingFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetAntiCommutingFields[sSetup], {}, TestID -> "GetAntiCommutingFields scalar: empty (no paired commuting fields)"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetAntiCommutingFields[ySetup], {}, TestID -> "GetAntiCommutingFields Yukawa: empty (no paired commuting fields)"]];

(**********************************************************************************
    GetGrassmannFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetGrassmannFields[sSetup], {}, TestID -> "GetGrassmannFields scalar: empty"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetGrassmannFields[ySetup], {Psi}, TestID -> "GetGrassmannFields Yukawa: returns {Psi}"]];

(**********************************************************************************
    GetAntiGrassmannFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetAntiGrassmannFields[sSetup], {}, TestID -> "GetAntiGrassmannFields scalar: empty"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetAntiGrassmannFields[ySetup], {Psibar}, TestID -> "GetAntiGrassmannFields Yukawa: returns {Psibar}"]];

(**********************************************************************************
    GetCommuting Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetCommuting[sSetup], {Phi}, TestID -> "GetCommuting scalar: returns {Phi}"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetCommuting[ySetup], {Phi}, TestID -> "GetCommuting Yukawa: returns {Phi}"]];

(**********************************************************************************
    GetGrassmann Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetGrassmann[sSetup], {}, TestID -> "GetGrassmann scalar: empty"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetGrassmann[ySetup], {Psibar, Psi}, TestID -> "GetGrassmann Yukawa: returns {Psibar, Psi}"]];

(**********************************************************************************
    GetFieldPairs Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetFieldPairs[sSetup], {}, TestID -> "GetFieldPairs scalar: empty"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetFieldPairs[ySetup], {{Psibar, Psi}}, TestID -> "GetFieldPairs Yukawa: returns {{Psibar, Psi}}"]];

(**********************************************************************************
    GetSingleFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetSingleFields[sSetup], {Phi}, TestID -> "GetSingleFields scalar: returns {Phi}"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetSingleFields[ySetup], {Phi}, TestID -> "GetSingleFields Yukawa: returns {Phi} (Grassmann fields are paired)"]];

(**********************************************************************************
    GetAllFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetAllFields[sSetup], Sort @ {Phi}, TestID -> "GetAllFields scalar: returns {Phi}"]];

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetAllFields[ySetup], Sort @ {Psibar, Psi, Phi}, TestID -> "GetAllFields Yukawa: returns {Psibar, Psi, Phi}"]];

(**********************************************************************************
    FieldNameQ Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`FieldNameQ[sSetup, Phi], True, TestID -> "FieldNameQ scalar: Phi is a field"]];

AppendTo[tests, VerificationTest[FunKit`Private`FieldNameQ[sSetup, Psi], False, TestID -> "FieldNameQ scalar: Psi is not a field"]];

AppendTo[tests, VerificationTest[FunKit`Private`FieldNameQ[ySetup, Psi], True, TestID -> "FieldNameQ Yukawa: Psi is a field"]];

AppendTo[tests, VerificationTest[FunKit`Private`FieldNameQ[ySetup, Psibar], True, TestID -> "FieldNameQ Yukawa: Psibar is a field"]];

(**********************************************************************************
    HasPartnerField Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`HasPartnerField[sSetup, Phi], False, TestID -> "HasPartnerField scalar: Phi has no partner"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasPartnerField[ySetup, Psi], True, TestID -> "HasPartnerField Yukawa: Psi has partner"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasPartnerField[ySetup, Psibar], True, TestID -> "HasPartnerField Yukawa: Psibar has partner"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasPartnerField[ySetup, Phi], False, TestID -> "HasPartnerField Yukawa: Phi has no partner"]];

(* Test with indexed field *)

AppendTo[tests, VerificationTest[FunKit`Private`HasPartnerField[ySetup, Psi[i1]], True, TestID -> "HasPartnerField Yukawa: Psi[i1] (indexed) has partner"]];

(**********************************************************************************
    IsGrassmannField Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannField[ySetup, Psi], True, TestID -> "IsGrassmannField Yukawa: Psi is a Grassmann field"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannField[ySetup, Psibar], False, TestID -> "IsGrassmannField Yukawa: Psibar is not a Grassmann field (it is anti-Grassmann)"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannField[ySetup, Phi], False, TestID -> "IsGrassmannField Yukawa: Phi is not a Grassmann field"]];

(* Test with indexed field *)

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannField[ySetup, Psi[i1]], True, TestID -> "IsGrassmannField Yukawa: Psi[i1] (indexed) is a Grassmann field"]];

(**********************************************************************************
    IsAntiGrassmannField Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`IsAntiGrassmannField[ySetup, Psibar], True, TestID -> "IsAntiGrassmannField Yukawa: Psibar is an anti-Grassmann field"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsAntiGrassmannField[ySetup, Psi], False, TestID -> "IsAntiGrassmannField Yukawa: Psi is not an anti-Grassmann field"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsAntiGrassmannField[ySetup, Phi], False, TestID -> "IsAntiGrassmannField Yukawa: Phi is not an anti-Grassmann field"]];

(* Test with indexed field *)

AppendTo[tests, VerificationTest[FunKit`Private`IsAntiGrassmannField[ySetup, Psibar[i1]], True, TestID -> "IsAntiGrassmannField Yukawa: Psibar[i1] (indexed) is an anti-Grassmann field"]];

(**********************************************************************************
    IsCommutingField Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`IsCommutingField[sSetup, Phi], True, TestID -> "IsCommutingField scalar: Phi is a commuting field"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCommutingField[ySetup, Phi], True, TestID -> "IsCommutingField Yukawa: Phi is a commuting field"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCommutingField[ySetup, Psi], False, TestID -> "IsCommutingField Yukawa: Psi is not a commuting field"]];

(* Test with indexed field *)

AppendTo[tests, VerificationTest[FunKit`Private`IsCommutingField[sSetup, Phi[i1]], True, TestID -> "IsCommutingField scalar: Phi[i1] (indexed) is a commuting field"]];

(**********************************************************************************
    IsAntiCommutingField Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`IsAntiCommutingField[sSetup, Phi], False, TestID -> "IsAntiCommutingField scalar: Phi is not an anti-commuting-field"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsAntiCommutingField[ySetup, Psi], False, TestID -> "IsAntiCommutingField Yukawa: Psi is not an anti-commuting-field"]];

(**********************************************************************************
    IsGrassmann Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmann[ySetup, Psi], True, TestID -> "IsGrassmann Yukawa: Psi is Grassmann"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmann[ySetup, Psibar], True, TestID -> "IsGrassmann Yukawa: Psibar is Grassmann"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmann[ySetup, Phi], False, TestID -> "IsGrassmann Yukawa: Phi is not Grassmann"]];

(**********************************************************************************
    IsCommuting Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`IsCommuting[ySetup, Phi], True, TestID -> "IsCommuting Yukawa: Phi is commuting"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCommuting[ySetup, Psi], False, TestID -> "IsCommuting Yukawa: Psi is not commuting"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCommuting[ySetup, Psibar], False, TestID -> "IsCommuting Yukawa: Psibar is not commuting"]];

(**********************************************************************************
    GetPartnerField Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetPartnerField[ySetup, Psi], Psibar, TestID -> "GetPartnerField Yukawa: partner of Psi is Psibar"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetPartnerField[ySetup, Psibar], Psi, TestID -> "GetPartnerField Yukawa: partner of Psibar is Psi"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetPartnerField[sSetup, Phi], Phi, TestID -> "GetPartnerField scalar: partner of Phi is Phi (no partner)"]];

(* Test indexed partner field *)

AppendTo[tests, VerificationTest[FunKit`Private`GetPartnerField[ySetup, Psi[i1]], Psibar[i1], TestID -> "GetPartnerField Yukawa: partner of Psi[i1] is Psibar[i1]"]];

(**********************************************************************************
    ExtractFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`ExtractFields[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], {Phi}, TestID -> "ExtractFields scalar: extract Phi from FTerm"]];

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`ExtractFields[ySetup, FTerm[Psi[i1], Propagator[{Psi, Psibar}, {i1, i2}], Psibar[i2], Phi[i3]]], Sort @ {Psi, Psibar, Phi}, TestID -> "ExtractFields Yukawa: extract all fields from FTerm"]];

AppendTo[tests, VerificationTest[FunKit`Private`ExtractFields[sSetup, FTerm[Propagator[{Phi, Phi}, {i1, i2}]]], {}, TestID -> "ExtractFields scalar: no standalone fields in expression"]];

(**********************************************************************************
    ExtractFieldsWithIndex Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`ExtractFieldsWithIndex[sSetup, FTerm[Phi[i1], Phi[i2]]], {Phi[i1], Phi[i2]}, TestID -> "ExtractFieldsWithIndex scalar: returns indexed fields"]];

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`ExtractFieldsWithIndex[ySetup, FTerm[Psi[i1], Psibar[i2]]], Sort @ {Psi[i1], Psibar[i2]}, TestID -> "ExtractFieldsWithIndex Yukawa: returns indexed Grassmann fields"]];

(**********************************************************************************
    ContainsGrassmann Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`ContainsGrassmann[ySetup, FTerm[Phi[i1], Phi[i2]]], False, TestID -> "ContainsGrassmann Yukawa: FTerm with only Phi"]];

AppendTo[tests, VerificationTest[FunKit`Private`ContainsGrassmann[ySetup, FTerm[Psi[i1], Psibar[i2]]], True, TestID -> "ContainsGrassmann Yukawa: FTerm with Psi and Psibar"]];

AppendTo[tests, VerificationTest[FunKit`Private`ContainsGrassmann[sSetup, FTerm[Phi[i1]]], False, TestID -> "ContainsGrassmann scalar: no Grassmann fields"]];

(**********************************************************************************
    GrassmannCount Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GrassmannCount[ySetup, FTerm[Psi[i1], Psibar[i2], Phi[i3]]], 2, TestID -> "GrassmannCount Yukawa: 2 Grassmann fields in FTerm"]];

AppendTo[tests, VerificationTest[FunKit`Private`GrassmannCount[ySetup, FTerm[Phi[i1], Phi[i2]]], 0, TestID -> "GrassmannCount Yukawa: 0 Grassmann fields in boson-only FTerm"]];

AppendTo[tests, VerificationTest[FunKit`Private`GrassmannCount[ySetup, FTerm[Psi[i1], Psi[i2], Psibar[i3], Psibar[i4]]], 4, TestID -> "GrassmannCount Yukawa: 4 Grassmann fields"]];

(**********************************************************************************
    GetAllSuperIndices Tests (FTerm)
**********************************************************************************)

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetAllSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], Sort @ {i1, i2}, TestID -> "GetAllSuperIndices scalar FTerm: returns {i1, i2}"]];

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetAllSuperIndices[ySetup, FTerm[Psi[i1], Psibar[i2], GammaN[{Psi, Psibar, Phi}, {i1, i2, i3}]]], Sort @ {i1, i2, i3}, TestID -> "GetAllSuperIndices Yukawa FTerm: returns {i1, i2, i3}"]];

(**********************************************************************************
    ExtractObjectsWithIndex Tests (FTerm)
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`ExtractObjectsWithIndex[sSetup, FTerm[3, Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], {Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]}, TestID -> "ExtractObjectsWithIndex scalar FTerm: excludes prefactors"]];

(**********************************************************************************
    GetClosedSuperIndices Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetClosedSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], Sort @ {i1, i2}, TestID -> "GetClosedSuperIndices scalar: all indices closed"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetClosedSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], {i1}, TestID -> "GetClosedSuperIndices scalar: i1 closed, i2 open"]];

(**********************************************************************************
    GetOpenSuperIndices Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`GetOpenSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], {}, TestID -> "GetOpenSuperIndices scalar: no open indices when all closed"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetOpenSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], {i2}, TestID -> "GetOpenSuperIndices scalar: i2 is open"]];

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetOpenSuperIndices[ySetup, FTerm[Psi[i1], Psibar[i2]]], Sort @ {i1, i2}, TestID -> "GetOpenSuperIndices Yukawa: both indices open for standalone fields"]];

(**********************************************************************************
    AllSuperIndicesClosed Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`AllSuperIndicesClosed[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], True, TestID -> "AllSuperIndicesClosed scalar: all closed returns True"]];

AppendTo[tests, VerificationTest[FunKit`Private`AllSuperIndicesClosed[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], False, TestID -> "AllSuperIndicesClosed scalar: open index returns False"]];

(* FEx version *)

AppendTo[tests, VerificationTest[FunKit`Private`AllSuperIndicesClosed[sSetup, FEx[FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]]], True, TestID -> "AllSuperIndicesClosed scalar FEx: all closed returns True"]];

AppendTo[tests, VerificationTest[FunKit`Private`AllSuperIndicesClosed[sSetup, FEx[FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]], FTerm[Phi[i3], Propagator[{Phi, Phi}, {i3, i4}]]]], False, TestID -> "AllSuperIndicesClosed scalar FEx: one term with open index returns False"]];

(**********************************************************************************
    SuperIndicesValid Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`SuperIndicesValid[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], True, TestID -> "SuperIndicesValid scalar: valid expression returns True"]];

AppendTo[tests, VerificationTest[FunKit`Private`SuperIndicesValid[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], True, TestID -> "SuperIndicesValid scalar: open but valid expression returns True"]];

AppendTo[tests, VerificationTest[FunKit`Private`SuperIndicesValid[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Phi[i2]]], False, TestID -> "SuperIndicesValid scalar: index appearing >2 times returns False"]];

(**********************************************************************************
    replFields / unreplFields Tests
**********************************************************************************)

AppendTo[tests, VerificationTest[FunKit`Private`replFields[sSetup, Phi[i1]], Field[{Phi}, {i1}], TestID -> "replFields scalar: Phi[i1] -> Field[{Phi}, {i1}]"]];

AppendTo[tests, VerificationTest[FunKit`Private`unreplFields[sSetup, Field[{Phi}, {i1}]], Phi[i1], TestID -> "unreplFields scalar: Field[{Phi}, {i1}] -> Phi[i1]"]];

AppendTo[tests, VerificationTest[FunKit`Private`unreplFields[sSetup, FunKit`Private`replFields[sSetup, Phi[i1]]], Phi[i1], TestID -> "replFields then unreplFields scalar: roundtrip"]];

AppendTo[tests, VerificationTest[FunKit`Private`replFields[ySetup, Psi[i1]], Field[{Psi}, {i1}], TestID -> "replFields Yukawa: Psi[i1] -> Field[{Psi}, {i1}]"]];

AppendTo[tests, VerificationTest[FunKit`Private`replFields[sSetup, AnyField[i1]], Field[{AnyField}, {i1}], TestID -> "replFields scalar: AnyField[i1] -> Field[{AnyField}, {i1}]"]];

(**********************************************************************************
    Source Field Tests
**********************************************************************************)

srcSetup = GetFunKitSetupWithSources[];

(* IsCSource Tests *)

AppendTo[tests, VerificationTest[FunKit`Private`IsCSource[srcSetup, J], True, TestID -> "IsCSource: J is a commuting source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCSource[srcSetup, Phi], False, TestID -> "IsCSource: Phi is not a commuting source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCSource[srcSetup, eta], False, TestID -> "IsCSource: eta is not a commuting source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCSource[srcSetup, J[i1]], True, TestID -> "IsCSource: J[i1] (indexed) is a commuting source"]];

(* IsGrassmannSource Tests *)

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannSource[srcSetup, eta], True, TestID -> "IsGrassmannSource: eta is a Grassmann source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannSource[srcSetup, Phi], False, TestID -> "IsGrassmannSource: Phi is not a Grassmann source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannSource[srcSetup, J], False, TestID -> "IsGrassmannSource: J is not a Grassmann source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmannSource[srcSetup, eta[i1]], True, TestID -> "IsGrassmannSource: eta[i1] (indexed) is a Grassmann source"]];

(* IsSource Tests *)

AppendTo[tests, VerificationTest[FunKit`Private`IsSource[srcSetup, J], True, TestID -> "IsSource: J is a source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsSource[srcSetup, eta], True, TestID -> "IsSource: eta is a source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsSource[srcSetup, Phi], False, TestID -> "IsSource: Phi is not a source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsSource[srcSetup, Psi], False, TestID -> "IsSource: Psi is not a source"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsSource[srcSetup, J[i1]], True, TestID -> "IsSource: J[i1] (indexed) is a source"]];

(* GetCSourceFields Tests *)

AppendTo[tests, VerificationTest[FunKit`Private`GetCSourceFields[srcSetup], {J}, TestID -> "GetCSourceFields: returns {J}"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetCSourceFields[ySetup], {}, TestID -> "GetCSourceFields: empty for Yukawa (no source keys)"]];

(* GetGrassmannSourceFields Tests *)

AppendTo[tests, VerificationTest[FunKit`Private`GetGrassmannSourceFields[srcSetup], {eta}, TestID -> "GetGrassmannSourceFields: returns {eta}"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetGrassmannSourceFields[ySetup], {}, TestID -> "GetGrassmannSourceFields: empty for Yukawa (no source keys)"]];

(* GetAllSourceFields Tests *)

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetAllSourceFields[srcSetup], Sort @ {J, eta}, TestID -> "GetAllSourceFields: returns {J, eta}"]];

AppendTo[tests, VerificationTest[FunKit`Private`GetAllSourceFields[ySetup], {}, TestID -> "GetAllSourceFields: empty for Yukawa"]];

(* GetNonSourceFields Tests *)

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetNonSourceFields[srcSetup], Sort @ {Psibar, Psi, Phi}, TestID -> "GetNonSourceFields: excludes sources"]];

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetNonSourceFields[ySetup], Sort @ {Psibar, Psi, Phi}, TestID -> "GetNonSourceFields: same as GetAllFields for Yukawa"]];

(* GetAllFields includes sources *)

AppendTo[tests, VerificationTest[Sort @ FunKit`Private`GetAllFields[srcSetup], Sort @ {Psibar, Psi, Phi, J, eta}, TestID -> "GetAllFields with sources: includes source fields"]];

(* FieldNameQ recognizes sources *)

AppendTo[tests, VerificationTest[FunKit`Private`FieldNameQ[srcSetup, J], True, TestID -> "FieldNameQ: J is a recognized field name"]];

AppendTo[tests, VerificationTest[FunKit`Private`FieldNameQ[srcSetup, eta], True, TestID -> "FieldNameQ: eta is a recognized field name"]];

(* IsCommuting includes commuting sources *)

AppendTo[tests, VerificationTest[FunKit`Private`IsCommuting[srcSetup, J], True, TestID -> "IsCommuting: J (commuting source) is commuting"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsCommuting[srcSetup, eta], False, TestID -> "IsCommuting: eta (Grassmann source) is not commuting"]];

(* IsGrassmann includes Grassmann sources *)

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmann[srcSetup, eta], True, TestID -> "IsGrassmann: eta (Grassmann source) is Grassmann"]];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmann[srcSetup, J], False, TestID -> "IsGrassmann: J (commuting source) is not Grassmann"]];

(* GetSingleFields includes sources *)

AppendTo[tests, VerificationTest[MemberQ[FunKit`Private`GetSingleFields[srcSetup], J], True, TestID -> "GetSingleFields: includes J"]];

AppendTo[tests, VerificationTest[MemberQ[FunKit`Private`GetSingleFields[srcSetup], eta], True, TestID -> "GetSingleFields: includes eta"]];

(**********************************************************************************
    HasFermiStatistics Tests

    A field's Fermi/Bose STATISTICS fixes the Matsubara character of the momenta flowing
    through it, and it is not the same thing as its Grassmann parity. Ghosts anticommute
    (Grassmann) but are periodic in imaginary time, hence Bose. FunKit cannot infer this —
    it is declared via the field space's "BoseStatistics" / "FermiStatistics".
**********************************************************************************)

(* Default: no declaration => Grassmann is Fermi, commuting is Bose. Existing setups are unaffected. *)

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ySetup, Psi], True, TestID -> "HasFermiStatistics default: Grassmann Psi is Fermi"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ySetup, Psibar], True, TestID -> "HasFermiStatistics default: anti-Grassmann Psibar is Fermi"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ySetup, Phi], False, TestID -> "HasFermiStatistics default: commuting Phi is Bose"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ySetup, Psi[i1]], True, TestID -> "HasFermiStatistics default: Psi[i1] (indexed) is Fermi"]];

(* Declared: the Yang-Mills fixture declares "BoseStatistics" -> {c}. The ghost is still Grassmann
   (so it still anticommutes, and signs are unaffected) but its statistics is Bose. *)

ymStatSetup = GetFunKitSetupYangMills[];

AppendTo[tests, VerificationTest[FunKit`Private`IsGrassmann[ymStatSetup, c], True, TestID -> "HasFermiStatistics: ghost c is still Grassmann (signs unaffected)"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ymStatSetup, c], False, TestID -> "HasFermiStatistics: declared ghost c is Bose"]];

(* Partner propagation: only c is declared; cb must follow. *)

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ymStatSetup, cb], False, TestID -> "HasFermiStatistics: partner cb inherits Bose from c"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[ymStatSetup, A], False, TestID -> "HasFermiStatistics: gluon A is Bose"]];

(* A Grassmann field that is NOT declared keeps the default. QCD declares only the ghost. *)

qcdStatSetup = GetFunKitSetupQCD[];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[qcdStatSetup, q], True, TestID -> "HasFermiStatistics: undeclared quark q keeps the Fermi default"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[qcdStatSetup, qb], True, TestID -> "HasFermiStatistics: undeclared quark qb keeps the Fermi default"]];

AppendTo[tests, VerificationTest[FunKit`Private`HasFermiStatistics[qcdStatSetup, c], False, TestID -> "HasFermiStatistics: QCD ghost c is Bose"]];

(* A field declared in both lists is a contradiction and must be rejected by the field-space check. *)

AppendTo[tests, VerificationTest[
    Quiet @ FunKit`Private`FieldSpaceDefQ[<|
        "Commuting" -> {A[p, {v, col}]},
        "Grassmann" -> {{cb[p, {col}], c[p, {col}]}},
        "BoseStatistics" -> {c},
        "FermiStatistics" -> {c}
    |>],
    False,
    TestID -> "FieldSpaceDefQ: a field declared both Fermi and Bose is rejected"
]];

(* A statistics entry that is not a declared field head is a typo and must be rejected. *)

AppendTo[tests, VerificationTest[
    Quiet @ FunKit`Private`FieldSpaceDefQ[<|
        "Commuting" -> {A[p, {v, col}]},
        "Grassmann" -> {{cb[p, {col}], c[p, {col}]}},
        "BoseStatistics" -> {ghost}
    |>],
    False,
    TestID -> "FieldSpaceDefQ: an unknown field head in BoseStatistics is rejected"
]];

(* Statistics entries are HEADS, not field definitions. *)

AppendTo[tests, VerificationTest[
    Quiet @ FunKit`Private`FieldSpaceDefQ[<|
        "Commuting" -> {A[p, {v, col}]},
        "Grassmann" -> {{cb[p, {col}], c[p, {col}]}},
        "BoseStatistics" -> {c[p, {col}]}
    |>],
    False,
    TestID -> "FieldSpaceDefQ: a field definition (not a head) in BoseStatistics is rejected"
]];

(* The well-formed case is accepted. *)

AppendTo[tests, VerificationTest[
    FunKit`Private`FieldSpaceDefQ[<|
        "Commuting" -> {A[p, {v, col}]},
        "Grassmann" -> {{cb[p, {col}], c[p, {col}]}},
        "BoseStatistics" -> {c}
    |>],
    True,
    TestID -> "FieldSpaceDefQ: a well-formed BoseStatistics declaration is accepted"
]];
