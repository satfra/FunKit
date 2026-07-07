# FunKit

[![Tests](https://img.shields.io/endpoint?style=for-the-badge&url=https://gist.githubusercontent.com/satfra/5274b1492e4a72ebb1aa57a7d60b0267/raw/funkit-tests.json)](https://github.com/satfra/FunKit/actions/workflows/run-tests.yml)
[![arXiv](https://img.shields.io/badge/arXiv-2605.28935-b31b1b.svg?style=for-the-badge)](https://arxiv.org/abs/2605.28935)

`FunKit` is a Mathematica package for all tasks related to the derivation of functional equations in Quantum Field Theory (QFT). 

The Idea of `FunKit` is to be a one-stop solution to this end, starting with the definition of the QFT and truncation prescriptions, and ending with either the analytical equations, or automatically generated code to solve them numerically.

## Citing

If you use the package for your research, please cite the corresponding publication:

```
@article{Sattler:2026FunKit,
    author        = "Sattler, Franz R.",
    title         = "{FunKit: A computer algebra toolkit for functional approaches}",
    year          = "2026",
    month         = "5",
    eprint        = "2605.28935",
    archivePrefix = "arXiv",
    primaryClass  = "hep-ph"
}
```

## Basic usage

After you have [installed](#installation) the package, you can open a new notebook and call
```Mathematica
Needs["FunKit`"]
```
to load the package.
To get started, the first command you may want to use is
```Mathematica
FInfo[]
```
or more specifically,
```Mathematica
FInfo["FEDeriK"]
```
which will give you an overview of how to use the package.

For example, you may want to have the general expression for the most general flow of a two-point function from the Wetterich equation:
```Mathematica
FSetAutoSimplify[False];
FTakeDerivatives[FEmptySetup, WetterichEquation, {AnyField[i1],AnyField[i2]}];
FPrint[FEmptySetup, %];
```
We have turned off automatic simplification here to show the raw output, which reads

```math
\begin{aligned}\  &\frac{1}{2} (-1)^{i_1a} (-1)^{i_2a} (-1)^{cc} (-1)^{ee}\,G^{ab}\,\Gamma_{bi_1c}\,G^{cd}\,\Gamma_{di_2e}\,G^{ef}\,\partial_t R_{af}
    \\ &\,+\,\left(-\frac{1}{2} (-1)^{i_1a} (-1)^{i_1b} (-1)^{i_2a} (-1)^{cc}\,G^{ab}\,\Gamma_{i_1bi_2c}\,G^{cd}\,\partial_t R_{ad}\right)
    \\ &\,+\,\frac{1}{2} (-1)^{i_1i_2} (-1)^{i_1a} (-1)^{i_2a} (-1)^{cc} (-1)^{ee}\,G^{ab}\,\Gamma_{bi_2c}\,G^{cd}\,\Gamma_{di_1e}\,G^{ef}\,\partial_t R_{af}
\end{aligned}
```

For a scalar field theory, one can get a more explicit expression as follows:
```Mathematica
fields = <|"Commuting"->{Phi[p]}|>;
truncation = <|GammaN->Table[Table[Phi, {i}], {i, 1, 4}],
               S->{{Phi, Phi}, {Phi, Phi, Phi, Phi}},
               Propagator->{{Phi, Phi}},
               Rdot->{{Phi, Phi}}|>;
FSetGlobalSetup[<|"FieldSpace"->fields, "Truncation"->truncation|>];
FSetTexStyles[Phi->"\\phi"];
FTakeDerivatives[WetterichEquation, {Phi[i1], Phi[i2]}]//FTruncate//FPrint;
```

which yields the output
```math
\begin{aligned}\  &G^{\phi^{a}\phi^{b}}\,\Gamma_{\phi^{c}\phi^{b}\phi^{i_1}}\,G^{\phi^{c}\phi^{d}}\,\Gamma_{\phi^{e}\phi^{d}\phi^{i_2}}\,G^{\phi^{e}\phi^{f}}\,\partial_t R_{\phi^{f}\phi^{a}}
    \\ &\,+\,\left(-\frac{1}{2}\,G^{\phi^{a}\phi^{b}}\,\Gamma_{\phi^{c}\phi^{b}\phi^{i_2}\phi^{i_1}}\,G^{\phi^{c}\phi^{d}}\,\partial_t R_{\phi^{d}\phi^{a}}\right)
\end{aligned}
```

For the same theory, we can also derive the Dyson-Schwinger equation for the same two-point function:
```Mathematica
FTakeDerivatives[FMakeDSE[Phi[i1]], {Phi[i2]}]//FTruncate//FPrint;
```
which gives
```math
\begin{aligned}\  &S_{\phi^{i_2}\phi^{i_1}}
    \\ &\,+\,\frac{\phi^{a} \phi^{b}}{2}\,S_{\phi^{b}\phi^{a}\phi^{i_2}\phi^{i_1}}
    \\ &\,+\,\frac{1}{2}\,S_{\phi^{a}\phi^{b}\phi^{i_2}\phi^{i_1}}\,G^{\phi^{a}\phi^{b}}
    \\ &\,+\,\left(-\frac{\phi^{a}}{2}\,S_{\phi^{b}\phi^{c}\phi^{a}\phi^{i_1}}\,G^{\phi^{d}\phi^{c}}\,\Gamma_{\phi^{e}\phi^{d}\phi^{i_2}}\,G^{\phi^{b}\phi^{e}}\right)
    \\ &\,+\,\frac{1}{2}\,S_{\phi^{a}\phi^{b}\phi^{c}\phi^{i_1}}\,G^{\phi^{d}\phi^{b}}\,\Gamma_{\phi^{e}\phi^{d}\phi^{i_2}}\,G^{\phi^{f}\phi^{e}}\,G^{\phi^{g}\phi^{a}}\,\Gamma_{\phi^{h}\phi^{g}\phi^{f}}\,G^{\phi^{c}\phi^{h}}
    \\ &\,+\,\left(-\frac{1}{6}\,S_{\phi^{a}\phi^{b}\phi^{c}\phi^{i_1}}\,G^{\phi^{d}\phi^{b}}\,G^{\phi^{e}\phi^{a}}\,\Gamma_{\phi^{f}\phi^{e}\phi^{d}\phi^{i_2}}\,G^{\phi^{c}\phi^{f}}\right)
\end{aligned}
```

If you wish to wish to remove the remaining fields (i.e. go to the symmetric regime), you can add the line
```Mathematica
    ...
    Field->{},
    ...
```
to the truncation definition above.

You can of course define arbitrary master equations besides the pre-defined `WetterichEquation` and `FMakeDSE` ones (among others), see the documentation for details.

## The C++ backend

For large derivations, `FunKit` ships a C++ engine (`cpplib/`, module **CoBra**) that runs the whole `FTakeDerivatives // FTruncate // FSimplify` pipeline in a single, highly optimized external process. Activate it with

```Mathematica
FSetBackendCpp[]     (* first call: compiles the engine with CMake and runs its test suite *)
```

Requirements: CMake ≥ 3.20, a C++20 compiler with OpenMP (plus network access on the first build to fetch the test framework — or pass `"RunTests" -> False`). The backend refuses to activate if the build or its tests fail.

With the backend active, `FTakeDerivatives` returns a lightweight *deferred handle* instead of the (potentially huge) intermediate expression; passing it to `FTruncate`, `FSimplify` or `FEvaluate` runs **one fused C++ call** — derivatives, truncation and simplification together, so the untruncated intermediate never materializes:

```Mathematica
FSetGlobalSetup[setup];
FSetBackendCpp[];
flow = WetterichEquation // FTakeDerivatives[#, {Phi[i1], Phi[i2]}]& // FTruncate
(* identical in shape and (exact, rational) coefficients to the pure-Mathematica result *)
```

`FMakeDSE` and further derivatives of DSEs route through the engine automatically. Results are cached on disk keyed by their full input (see `FClearCppCache`, `FSetCppCacheDirectory`).

The backend covers the standard object types (including `Phidot`-style objects with pinned legs), source fields, and index-free symbolic prefactors — couplings, `Z`-factors and `I` are stripped per term, run through the engine per group, and re-attached exactly. Genuinely unsupported input (custom `FAddFDRule` rules, routed/explicit indices, index-dependent coefficients) gives a hard error — never a silently different result. Opt out globally with `FSetBackendMathematica[]` or per call via the `"Backend" -> "Mathematica"` option. `FExportCppInput` and `FExportToml` write stand-alone input files for the `funkit` executable (see `cpplib/README.md`).

## Examples

To learn how to compute more complicated systems, you may want to see some typical examples on how to use `FunKit` to deal with common QFTs of interest. 

You will find some showcases inside the `examples/` folder:
-  `examples/ScalarTheory.nb` shows the derivation of DSEs and fRG flows in a scalar theory.
-  `examples/Yang-Mills.nb` and `examples/Yang-Mills/` derive the functional equations for an $SU(N)$ gauge theory and provide a numerical implementation using [`DiFfRG`](https://github.com/satfra/DiFfRG).
-  `examples/Yukawa.nb` does the same for a mixed fermion-boson theory.
-  `examples/mSTI-Yang-Mills.nb` calculates the modified Slavnov–Taylor identity for the gluon two-point function.
-  `examples/FlowingReparametrisation.nb` derives fRG equations for an $O(N)$ theory with flowing field reparametrisation.
-  `examples/CompositeOperators.nb` derives correlation functions of a two-fermion composite operator.
-  `examples/FunKitPaper.nb` is a guided, runnable companion to the [companion paper](publication/FunKit.pdf).

## Installation

### From a Mathematica notebook or a CLI Wolfram session

To install the `FunKit` package in Mathematica, simply open a new notebook or kernel and download the installation file:
```Mathematica
  Import["https://raw.githubusercontent.com/satfra/FunKit/main/FunKitInstaller.m"]
```
The installer will automatically download the package and all basis definition files and their pre-built cache.

### CMake

You can also use the CMake integration if your project uses `FunKit` as a dependency. In that case, you can install the package directly from your `CMakeLists.txt`:

```cmake
include(FetchContent)
FetchContent_Declare(
        FunKit
        GIT_REPOSITORY "https://github.com/satfra/FunKit"
        GIT_TAG "main"
)
FetchContent_MakeAvailable(FunKit)
```
Alternatively, grab `FunKit` directly from the console:
```bash
$ git clone https://github.com/satfra/FunKit.git
$ mkdir FunKit/build
$ cd FunKit/build
$ cmake ..
$ make install
```

## Testing and validation

To run the test suite, you can either run 
```bash
$ make test
```
from the build directory (if you installed via CMake), or run the tests directly from a Mathematica notebook or kernel:
```Mathematica
Get["FunKit`"]
FTest[]
```
Running single tests is also possible:
```bash
$ make test-single FILE=FEDeriK/FunctionalDTests.m
```

## Related software

To the end of deriving flow equations, other useful software already exists, in particular
- [![QMeS](https://img.shields.io/badge/QMeS-3e4c2d?style=for-the-badge&logo=github)](https://github.com/QMeS-toolbox/QMeS-Derivation), which can derive fRG equations, DSEs, as well as (m)STIs.
- [![DoFun](https://img.shields.io/badge/DoFun-7a5fb0?style=for-the-badge&logo=github)](https://github.com/markusqh/DoFun) for the same tasks.

`FunKit` depends on, and builds upon
-  [![FormTracer](https://img.shields.io/badge/FormTracer-4e94cb?style=for-the-badge&logo=github)](https://github.com/FormTracer/FormTracer) to perform traces over group indices in the derived functional equations,
-  [![TensorBases](https://img.shields.io/badge/TensorBases-bb1a1a?style=for-the-badge&logo=github)](https://github.com/satfra/TensorBases) to handle tensor bases and projections.

These two packages are automatically installed when you import `FunKit` for the first time in a Mathematica notebook or session.

-  `FunKit` provides the algebraic infrastructure for [![DiFfRG](https://img.shields.io/badge/DiFfRG-1f1f1f?style=for-the-badge&logo=github)](https://github.com/satfra/DiFfRG), which is a C++ framework to solve the derived equations numerically.
