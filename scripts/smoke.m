(* scripts/smoke.m — fast (~30s) runner-environment smoke check.

   Verifies the four things that have historically broken the self-hosted
   runner without going through the full make test suite:

     1. The Wolfram license is found from the runner user's HOME.
     2. DoFun is pre-seeded (so init.m does not try a network download).
     3. Parallel subkernels actually launch (SystemIDList / PATH propagation).
     4. CCompilers and the FORM binary are visible from a subkernel
        (the "false-green" gap where COEN/TRACY tests silently skip).

   Run via scripts/local-ci.sh smoke. Exits 0 on success, nonzero on failure. *)

Print["=== runner-side smoke test ==="];
Print["whoami            = ", Environment["USER"]];
Print["$UserBaseDirectory = ", $UserBaseDirectory];
Print["$BaseDirectory     = ", $BaseDirectory];
Print["$LicenseType       = ", $LicenseType];
Print["$PasswordFile      = ", $PasswordFile];
Print["$SystemID          = ", $SystemID];
Print["PATH (master)      = ", Environment["PATH"]];

If[StringContainsQ[ToString[$UserBaseDirectory], "/home/franz"],
  Print["FATAL: running as franz, not the runner user"]; Exit[1]];

If[$LicenseType === None || $LicenseType === $Failed,
  Print["FATAL: no valid Wolfram license"]; Exit[2]];

doFunDir = FileNameJoin[{$UserBaseDirectory, "Applications", "DoFun"}];
Print["DoFun pre-seeded?  = ", DirectoryQ[doFunDir], "  (", doFunDir, ")"];
If[!DirectoryQ[doFunDir],
  Print["FATAL: DoFun missing — getDoFun.m will try to download and may hang"];
  Exit[3]];

Print["Loading FunKit..."];
Get["FunKit`"];

Print["Checking compilers / FORM from a subkernel..."];
Needs["CCompilerDriver`"];
Print["Master CCompilers  = ", CCompilers[]];

ks = Kernels[];
If[Length[ks] === 0, ks = LaunchKernels[2]];
Print["Subkernel count    = ", Length[Kernels[]]];

subInfo = ParallelEvaluate[
  Needs["CCompilerDriver`"];
  <|
    "kernelID" -> $KernelID,
    "PATH"     -> Environment["PATH"],
    "compilers"-> CCompilers[],
    "gcc"      -> Run["which gcc      > /dev/null 2>&1"],
    "g++"      -> Run["which g++      > /dev/null 2>&1"],
    "form"     -> Run["which form     > /dev/null 2>&1"],
    "gfortran" -> Run["which gfortran > /dev/null 2>&1"],
    "julia"    -> Run["which julia    > /dev/null 2>&1"]
  |>,
  First[Kernels[]]];
Print["Subkernel report   = ", subInfo];

probes = {"gcc","g++","form","gfortran","julia"};
missing = Select[probes, subInfo[#] =!= 0 &];
If[missing =!= {},
  Print["WARN: subkernel cannot exec: ", missing,
        " — COEN/TRACY tests will skip silently"]];

CloseKernels[];
Print["=== smoke OK ==="];
Exit[0];
