tests = {};

Import[$FunKitDirectory <> "/tests/boilerplate/setups.m"];

(**********************************************************************************
    Setup helpers
**********************************************************************************)

sSetup = GetFunKitSetupScalar[];

ySetup = GetFunKitSetupYukawa[];

(**********************************************************************************
    GetcFields Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetcFields[sSetup], {Phi}, TestID -> "GetcFields scalar: returns {Phi}"]];

AppendTo[tests, TestCreate[FunKit`Private`GetcFields[ySetup], {Phi}, TestID -> "GetcFields Yukawa: returns {Phi}"]];

(**********************************************************************************
    GetAnticFields Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetAnticFields[sSetup], {}, TestID -> "GetAnticFields scalar: empty (no paired commuting fields)"]];

AppendTo[tests, TestCreate[FunKit`Private`GetAnticFields[ySetup], {}, TestID -> "GetAnticFields Yukawa: empty (no paired commuting fields)"]];

(**********************************************************************************
    GetFermions Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetFermions[sSetup], {}, TestID -> "GetFermions scalar: empty"]];

AppendTo[tests, TestCreate[FunKit`Private`GetFermions[ySetup], {Psi}, TestID -> "GetFermions Yukawa: returns {Psi}"]];

(**********************************************************************************
    GetAntiFermions Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetAntiFermions[sSetup], {}, TestID -> "GetAntiFermions scalar: empty"]];

AppendTo[tests, TestCreate[FunKit`Private`GetAntiFermions[ySetup], {Psibar}, TestID -> "GetAntiFermions Yukawa: returns {Psibar}"]];

(**********************************************************************************
    GetCommuting Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetCommuting[sSetup], {Phi}, TestID -> "GetCommuting scalar: returns {Phi}"]];

AppendTo[tests, TestCreate[FunKit`Private`GetCommuting[ySetup], {Phi}, TestID -> "GetCommuting Yukawa: returns {Phi}"]];

(**********************************************************************************
    GetAntiCommuting Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetAntiCommuting[sSetup], {}, TestID -> "GetAntiCommuting scalar: empty"]];

AppendTo[tests, TestCreate[FunKit`Private`GetAntiCommuting[ySetup], {Psibar, Psi}, TestID -> "GetAntiCommuting Yukawa: returns {Psibar, Psi}"]];

(**********************************************************************************
    GetFieldPairs Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetFieldPairs[sSetup], {}, TestID -> "GetFieldPairs scalar: empty"]];

AppendTo[tests, TestCreate[FunKit`Private`GetFieldPairs[ySetup], {{Psibar, Psi}}, TestID -> "GetFieldPairs Yukawa: returns {{Psibar, Psi}}"]];

(**********************************************************************************
    GetSingleFields Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetSingleFields[sSetup], {Phi}, TestID -> "GetSingleFields scalar: returns {Phi}"]];

AppendTo[tests, TestCreate[FunKit`Private`GetSingleFields[ySetup], {Phi}, TestID -> "GetSingleFields Yukawa: returns {Phi} (fermions are paired)"]];

(**********************************************************************************
    GetAllFields Tests
**********************************************************************************)

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetAllFields[sSetup], Sort @ {Phi}, TestID -> "GetAllFields scalar: returns {Phi}"]];

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetAllFields[ySetup], Sort @ {Psibar, Psi, Phi}, TestID -> "GetAllFields Yukawa: returns {Psibar, Psi, Phi}"]];

(**********************************************************************************
    FieldNameQ Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`FieldNameQ[sSetup, Phi], True, TestID -> "FieldNameQ scalar: Phi is a field"]];

AppendTo[tests, TestCreate[FunKit`Private`FieldNameQ[sSetup, Psi], False, TestID -> "FieldNameQ scalar: Psi is not a field"]];

AppendTo[tests, TestCreate[FunKit`Private`FieldNameQ[ySetup, Psi], True, TestID -> "FieldNameQ Yukawa: Psi is a field"]];

AppendTo[tests, TestCreate[FunKit`Private`FieldNameQ[ySetup, Psibar], True, TestID -> "FieldNameQ Yukawa: Psibar is a field"]];

(**********************************************************************************
    HasPartnerField Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`HasPartnerField[sSetup, Phi], False, TestID -> "HasPartnerField scalar: Phi has no partner"]];

AppendTo[tests, TestCreate[FunKit`Private`HasPartnerField[ySetup, Psi], True, TestID -> "HasPartnerField Yukawa: Psi has partner"]];

AppendTo[tests, TestCreate[FunKit`Private`HasPartnerField[ySetup, Psibar], True, TestID -> "HasPartnerField Yukawa: Psibar has partner"]];

AppendTo[tests, TestCreate[FunKit`Private`HasPartnerField[ySetup, Phi], False, TestID -> "HasPartnerField Yukawa: Phi has no partner"]];

(* Test with indexed field *)

AppendTo[tests, TestCreate[FunKit`Private`HasPartnerField[ySetup, Psi[i1]], True, TestID -> "HasPartnerField Yukawa: Psi[i1] (indexed) has partner"]];

(**********************************************************************************
    IsFermion Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`IsFermion[ySetup, Psi], True, TestID -> "IsFermion Yukawa: Psi is a fermion"]];

AppendTo[tests, TestCreate[FunKit`Private`IsFermion[ySetup, Psibar], False, TestID -> "IsFermion Yukawa: Psibar is not a fermion (it is anti-fermion)"]];

AppendTo[tests, TestCreate[FunKit`Private`IsFermion[ySetup, Phi], False, TestID -> "IsFermion Yukawa: Phi is not a fermion"]];

(* Test with indexed field *)

AppendTo[tests, TestCreate[FunKit`Private`IsFermion[ySetup, Psi[i1]], True, TestID -> "IsFermion Yukawa: Psi[i1] (indexed) is a fermion"]];

(**********************************************************************************
    IsAntiFermion Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`IsAntiFermion[ySetup, Psibar], True, TestID -> "IsAntiFermion Yukawa: Psibar is an anti-fermion"]];

AppendTo[tests, TestCreate[FunKit`Private`IsAntiFermion[ySetup, Psi], False, TestID -> "IsAntiFermion Yukawa: Psi is not an anti-fermion"]];

AppendTo[tests, TestCreate[FunKit`Private`IsAntiFermion[ySetup, Phi], False, TestID -> "IsAntiFermion Yukawa: Phi is not an anti-fermion"]];

(* Test with indexed field *)

AppendTo[tests, TestCreate[FunKit`Private`IsAntiFermion[ySetup, Psibar[i1]], True, TestID -> "IsAntiFermion Yukawa: Psibar[i1] (indexed) is an anti-fermion"]];

(**********************************************************************************
    IscField Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`IscField[sSetup, Phi], True, TestID -> "IscField scalar: Phi is a commuting field"]];

AppendTo[tests, TestCreate[FunKit`Private`IscField[ySetup, Phi], True, TestID -> "IscField Yukawa: Phi is a commuting field"]];

AppendTo[tests, TestCreate[FunKit`Private`IscField[ySetup, Psi], False, TestID -> "IscField Yukawa: Psi is not a commuting field"]];

(* Test with indexed field *)

AppendTo[tests, TestCreate[FunKit`Private`IscField[sSetup, Phi[i1]], True, TestID -> "IscField scalar: Phi[i1] (indexed) is a commuting field"]];

(**********************************************************************************
    IsAnticField Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`IsAnticField[sSetup, Phi], False, TestID -> "IsAnticField scalar: Phi is not an anti-commuting-field"]];

AppendTo[tests, TestCreate[FunKit`Private`IsAnticField[ySetup, Psi], False, TestID -> "IsAnticField Yukawa: Psi is not an anti-commuting-field"]];

(**********************************************************************************
    IsGrassmann Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmann[ySetup, Psi], True, TestID -> "IsGrassmann Yukawa: Psi is Grassmann"]];

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmann[ySetup, Psibar], True, TestID -> "IsGrassmann Yukawa: Psibar is Grassmann"]];

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmann[ySetup, Phi], False, TestID -> "IsGrassmann Yukawa: Phi is not Grassmann"]];

(**********************************************************************************
    IsCommuting Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`IsCommuting[ySetup, Phi], True, TestID -> "IsCommuting Yukawa: Phi is commuting"]];

AppendTo[tests, TestCreate[FunKit`Private`IsCommuting[ySetup, Psi], False, TestID -> "IsCommuting Yukawa: Psi is not commuting"]];

AppendTo[tests, TestCreate[FunKit`Private`IsCommuting[ySetup, Psibar], False, TestID -> "IsCommuting Yukawa: Psibar is not commuting"]];

(**********************************************************************************
    GetPartnerField Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetPartnerField[ySetup, Psi], Psibar, TestID -> "GetPartnerField Yukawa: partner of Psi is Psibar"]];

AppendTo[tests, TestCreate[FunKit`Private`GetPartnerField[ySetup, Psibar], Psi, TestID -> "GetPartnerField Yukawa: partner of Psibar is Psi"]];

AppendTo[tests, TestCreate[FunKit`Private`GetPartnerField[sSetup, Phi], Phi, TestID -> "GetPartnerField scalar: partner of Phi is Phi (no partner)"]];

(* Test indexed partner field *)

AppendTo[tests, TestCreate[FunKit`Private`GetPartnerField[ySetup, Psi[i1]], Psibar[i1], TestID -> "GetPartnerField Yukawa: partner of Psi[i1] is Psibar[i1]"]];

(**********************************************************************************
    ExtractFields Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`ExtractFields[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], {Phi}, TestID -> "ExtractFields scalar: extract Phi from FTerm"]];

AppendTo[tests, TestCreate[Sort @ FunKit`Private`ExtractFields[ySetup, FTerm[Psi[i1], Propagator[{Psi, Psibar}, {i1, i2}], Psibar[i2], Phi[i3]]], Sort @ {Psi, Psibar, Phi}, TestID -> "ExtractFields Yukawa: extract all fields from FTerm"]];

AppendTo[tests, TestCreate[FunKit`Private`ExtractFields[sSetup, FTerm[Propagator[{Phi, Phi}, {i1, i2}]]], {}, TestID -> "ExtractFields scalar: no standalone fields in expression"]];

(**********************************************************************************
    ExtractFieldsWithIndex Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`ExtractFieldsWithIndex[sSetup, FTerm[Phi[i1], Phi[i2]]], {Phi[i1], Phi[i2]}, TestID -> "ExtractFieldsWithIndex scalar: returns indexed fields"]];

AppendTo[tests, TestCreate[Sort @ FunKit`Private`ExtractFieldsWithIndex[ySetup, FTerm[Psi[i1], Psibar[i2]]], Sort @ {Psi[i1], Psibar[i2]}, TestID -> "ExtractFieldsWithIndex Yukawa: returns indexed fermion fields"]];

(**********************************************************************************
    ContainsGrassmann Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`ContainsGrassmann[ySetup, FTerm[Phi[i1], Phi[i2]]], False, TestID -> "ContainsGrassmann Yukawa: FTerm with only Phi"]];

AppendTo[tests, TestCreate[FunKit`Private`ContainsGrassmann[ySetup, FTerm[Psi[i1], Psibar[i2]]], True, TestID -> "ContainsGrassmann Yukawa: FTerm with Psi and Psibar"]];

AppendTo[tests, TestCreate[FunKit`Private`ContainsGrassmann[sSetup, FTerm[Phi[i1]]], False, TestID -> "ContainsGrassmann scalar: no Grassmann fields"]];

(**********************************************************************************
    GrassmannCount Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GrassmannCount[ySetup, FTerm[Psi[i1], Psibar[i2], Phi[i3]]], 2, TestID -> "GrassmannCount Yukawa: 2 Grassmann fields in FTerm"]];

AppendTo[tests, TestCreate[FunKit`Private`GrassmannCount[ySetup, FTerm[Phi[i1], Phi[i2]]], 0, TestID -> "GrassmannCount Yukawa: 0 Grassmann fields in boson-only FTerm"]];

AppendTo[tests, TestCreate[FunKit`Private`GrassmannCount[ySetup, FTerm[Psi[i1], Psi[i2], Psibar[i3], Psibar[i4]]], 4, TestID -> "GrassmannCount Yukawa: 4 Grassmann fields"]];

(**********************************************************************************
    GetAllSuperIndices Tests (FTerm)
**********************************************************************************)

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetAllSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], Sort @ {i1, i2}, TestID -> "GetAllSuperIndices scalar FTerm: returns {i1, i2}"]];

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetAllSuperIndices[ySetup, FTerm[Psi[i1], Psibar[i2], GammaN[{Psi, Psibar, Phi}, {i1, i2, i3}]]], Sort @ {i1, i2, i3}, TestID -> "GetAllSuperIndices Yukawa FTerm: returns {i1, i2, i3}"]];

(**********************************************************************************
    ExtractObjectsWithIndex Tests (FTerm)
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`ExtractObjectsWithIndex[sSetup, FTerm[3, Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], {Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]}, TestID -> "ExtractObjectsWithIndex scalar FTerm: excludes prefactors"]];

(**********************************************************************************
    GetClosedSuperIndices Tests
**********************************************************************************)

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetClosedSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], Sort @ {i1, i2}, TestID -> "GetClosedSuperIndices scalar: all indices closed"]];

AppendTo[tests, TestCreate[FunKit`Private`GetClosedSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], {i1}, TestID -> "GetClosedSuperIndices scalar: i1 closed, i2 open"]];

(**********************************************************************************
    GetOpenSuperIndices Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`GetOpenSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], {}, TestID -> "GetOpenSuperIndices scalar: no open indices when all closed"]];

AppendTo[tests, TestCreate[FunKit`Private`GetOpenSuperIndices[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], {i2}, TestID -> "GetOpenSuperIndices scalar: i2 is open"]];

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetOpenSuperIndices[ySetup, FTerm[Psi[i1], Psibar[i2]]], Sort @ {i1, i2}, TestID -> "GetOpenSuperIndices Yukawa: both indices open for standalone fields"]];

(**********************************************************************************
    AllSuperIndicesClosed Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`AllSuperIndicesClosed[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], True, TestID -> "AllSuperIndicesClosed scalar: all closed returns True"]];

AppendTo[tests, TestCreate[FunKit`Private`AllSuperIndicesClosed[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], False, TestID -> "AllSuperIndicesClosed scalar: open index returns False"]];

(* FEx version *)

AppendTo[tests, TestCreate[FunKit`Private`AllSuperIndicesClosed[sSetup, FEx[FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]]], True, TestID -> "AllSuperIndicesClosed scalar FEx: all closed returns True"]];

AppendTo[tests, TestCreate[FunKit`Private`AllSuperIndicesClosed[sSetup, FEx[FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]], FTerm[Phi[i3], Propagator[{Phi, Phi}, {i3, i4}]]]], False, TestID -> "AllSuperIndicesClosed scalar FEx: one term with open index returns False"]];

(**********************************************************************************
    SuperIndicesValid Tests
**********************************************************************************)

AppendTo[tests, TestCreate[FunKit`Private`SuperIndicesValid[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], Phi[i2]]], True, TestID -> "SuperIndicesValid scalar: valid expression returns True"]];

AppendTo[tests, TestCreate[FunKit`Private`SuperIndicesValid[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}]]], True, TestID -> "SuperIndicesValid scalar: open but valid expression returns True"]];

AppendTo[tests, TestCreate[FunKit`Private`SuperIndicesValid[sSetup, FTerm[Phi[i1], Propagator[{Phi, Phi}, {i1, i2}], GammaN[{Phi, Phi}, {i1, i2}], Phi[i2]]], False, TestID -> "SuperIndicesValid scalar: index appearing >2 times returns False"]];

(**********************************************************************************
    replFields / unreplFields Tests
**********************************************************************************)

AppendTo[tests, TestCreate[Phi[i1] /. FunKit`Private`replFields[sSetup], Field[{Phi}, {i1}], TestID -> "replFields scalar: Phi[i1] -> Field[{Phi}, {i1}]"]];

AppendTo[tests, TestCreate[Field[{Phi}, {i1}] /. FunKit`Private`unreplFields[sSetup], Phi[i1], TestID -> "unreplFields scalar: Field[{Phi}, {i1}] -> Phi[i1]"]];

AppendTo[tests, TestCreate[(Phi[i1] /. FunKit`Private`replFields[sSetup]) /. FunKit`Private`unreplFields[sSetup], Phi[i1], TestID -> "replFields then unreplFields scalar: roundtrip"]];

AppendTo[tests, TestCreate[Psi[i1] /. FunKit`Private`replFields[ySetup], Field[{Psi}, {i1}], TestID -> "replFields Yukawa: Psi[i1] -> Field[{Psi}, {i1}]"]];

AppendTo[tests, TestCreate[AnyField[i1] /. FunKit`Private`replFields[sSetup], Field[{AnyField}, {i1}], TestID -> "replFields scalar: AnyField[i1] -> Field[{AnyField}, {i1}]"]];

(**********************************************************************************
    Source Field Tests
**********************************************************************************)

srcSetup = GetFunKitSetupWithSources[];

(* IsCSource Tests *)

AppendTo[tests, TestCreate[FunKit`Private`IsCSource[srcSetup, J], True, TestID -> "IsCSource: J is a commuting source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsCSource[srcSetup, Phi], False, TestID -> "IsCSource: Phi is not a commuting source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsCSource[srcSetup, eta], False, TestID -> "IsCSource: eta is not a commuting source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsCSource[srcSetup, J[i1]], True, TestID -> "IsCSource: J[i1] (indexed) is a commuting source"]];

(* IsGrassmannSource Tests *)

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmannSource[srcSetup, eta], True, TestID -> "IsGrassmannSource: eta is a Grassmann source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmannSource[srcSetup, Phi], False, TestID -> "IsGrassmannSource: Phi is not a Grassmann source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmannSource[srcSetup, J], False, TestID -> "IsGrassmannSource: J is not a Grassmann source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmannSource[srcSetup, eta[i1]], True, TestID -> "IsGrassmannSource: eta[i1] (indexed) is a Grassmann source"]];

(* IsSource Tests *)

AppendTo[tests, TestCreate[FunKit`Private`IsSource[srcSetup, J], True, TestID -> "IsSource: J is a source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsSource[srcSetup, eta], True, TestID -> "IsSource: eta is a source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsSource[srcSetup, Phi], False, TestID -> "IsSource: Phi is not a source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsSource[srcSetup, Psi], False, TestID -> "IsSource: Psi is not a source"]];

AppendTo[tests, TestCreate[FunKit`Private`IsSource[srcSetup, J[i1]], True, TestID -> "IsSource: J[i1] (indexed) is a source"]];

(* GetCSourceFields Tests *)

AppendTo[tests, TestCreate[FunKit`Private`GetCSourceFields[srcSetup], {J}, TestID -> "GetCSourceFields: returns {J}"]];

AppendTo[tests, TestCreate[FunKit`Private`GetCSourceFields[ySetup], {}, TestID -> "GetCSourceFields: empty for Yukawa (no source keys)"]];

(* GetGrassmannSourceFields Tests *)

AppendTo[tests, TestCreate[FunKit`Private`GetGrassmannSourceFields[srcSetup], {eta}, TestID -> "GetGrassmannSourceFields: returns {eta}"]];

AppendTo[tests, TestCreate[FunKit`Private`GetGrassmannSourceFields[ySetup], {}, TestID -> "GetGrassmannSourceFields: empty for Yukawa (no source keys)"]];

(* GetAllSourceFields Tests *)

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetAllSourceFields[srcSetup], Sort @ {J, eta}, TestID -> "GetAllSourceFields: returns {J, eta}"]];

AppendTo[tests, TestCreate[FunKit`Private`GetAllSourceFields[ySetup], {}, TestID -> "GetAllSourceFields: empty for Yukawa"]];

(* GetNonSourceFields Tests *)

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetNonSourceFields[srcSetup], Sort @ {Psibar, Psi, Phi}, TestID -> "GetNonSourceFields: excludes sources"]];

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetNonSourceFields[ySetup], Sort @ {Psibar, Psi, Phi}, TestID -> "GetNonSourceFields: same as GetAllFields for Yukawa"]];

(* GetAllFields includes sources *)

AppendTo[tests, TestCreate[Sort @ FunKit`Private`GetAllFields[srcSetup], Sort @ {Psibar, Psi, Phi, J, eta}, TestID -> "GetAllFields with sources: includes source fields"]];

(* FieldNameQ recognizes sources *)

AppendTo[tests, TestCreate[FunKit`Private`FieldNameQ[srcSetup, J], True, TestID -> "FieldNameQ: J is a recognized field name"]];

AppendTo[tests, TestCreate[FunKit`Private`FieldNameQ[srcSetup, eta], True, TestID -> "FieldNameQ: eta is a recognized field name"]];

(* IsCommuting includes commuting sources *)

AppendTo[tests, TestCreate[FunKit`Private`IsCommuting[srcSetup, J], True, TestID -> "IsCommuting: J (commuting source) is commuting"]];

AppendTo[tests, TestCreate[FunKit`Private`IsCommuting[srcSetup, eta], False, TestID -> "IsCommuting: eta (Grassmann source) is not commuting"]];

(* IsGrassmann includes Grassmann sources *)

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmann[srcSetup, eta], True, TestID -> "IsGrassmann: eta (Grassmann source) is Grassmann"]];

AppendTo[tests, TestCreate[FunKit`Private`IsGrassmann[srcSetup, J], False, TestID -> "IsGrassmann: J (commuting source) is not Grassmann"]];

(* GetSingleFields includes sources *)

AppendTo[tests, TestCreate[MemberQ[FunKit`Private`GetSingleFields[srcSetup], J], True, TestID -> "GetSingleFields: includes J"]];

AppendTo[tests, TestCreate[MemberQ[FunKit`Private`GetSingleFields[srcSetup], eta], True, TestID -> "GetSingleFields: includes eta"]];
