# FunKit

`FunKit` is a Mathematica package for all tasks related to the derivation of functional equations in Quantum Field Theory (QFT). 

The Idea of `FunKit` is to be a one-stop solution to this end, starting with the definition of the QFT and truncation prescriptions, and ending with either the analytical equations, or automatically generated code to solve them numerically.

## Citing

If you use the package for your research, please cite the corresponding publication:

```
@article{Sattler:2025FunKit,
    author = "Sattler, Franz R.",
    title = "{FunKit}",
    year = "2026",
    howpublished = "{\url{https://github.com/satfra/FunKit}}",
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

For example, you may want to have the general expression for the flow of a two-point function from the Wetterich equation:
```Mathematica
fields = <|"Commuting"->{Phi[p]}, "Grassmann"->{}|>;
SetGlobalSetup[<|"FieldSpace"->fields|>];
TakeDerivatives[WetterichEquation, {Phi[i1], Phi[i2]}]//FPrint;
```
Which will produce the output

$$
\frac{1}{2}\ (-1)^{\text{c}\text{c}}\ (-1)^{\text{e}\text{e}}\ (-1)^{\phi^{i_1}\text{a}}\ (-1)^{\phi^{i_2}\text{a}}\ G^{\text{a}\text{b}}\ \Gamma_{\text{b}\phi^{i_1}\text{c}}\ G^{\text{c}\text{d}}\ \Gamma_{\text{d}\phi^{i_2}\text{e}}\ G^{\text{e}\text{f}}\ \partial_t R_{\text{a}\text{f}}
$$
$$
\ +\ (-\frac{1}{2}\ (-1)^{\text{c}\text{c}}\ (-1)^{\phi^{i_2}\text{a}}\ G^{\text{a}\text{b}}\ (-1)^{\phi^{i_1}\text{a}}\ (-1)^{\phi^{i_1}\text{b}}\ \Gamma_{\phi^{i_1}\text{b}\phi^{i_2}\text{c}}\ G^{\text{c}\text{d}}\ \partial_t R_{\text{a}\text{d}})
$$
$$
\ \ \ +\ \frac{1}{2}\ (-1)^{\text{c}\text{c}}\ (-1)^{\phi^{i_2}\text{a}}\ G^{\text{a}\text{b}}\ \Gamma_{\text{b}\phi^{i_2}\text{c}}\ (-1)^{\text{e}\text{e}}\ (-1)^{\phi^{i_1}\text{a}}\ G^{\text{c}\text{d}}\ \Gamma_{\text{d}\phi^{i_1}\text{e}}\ G^{\text{e}\text{f}}\ \partial_t R_{\text{a}\text{f}}
$$

## Examples

To learn how to compute more complicated systems, you may want to see some typical examples on how to use `FunKit` to deal with common QFTs of interest. 

You will find some showcases inside the `examples/` folder:
-  `examples/ScalarTheory` shows the derivation of DSEs and fRG flows in an $O(N)$ theory.
-  `examples/Yang-Mills` derives the functional equations for an $SU(N)$ gauge theory.
-  `examples/Yukawa` does the same for a mixed fermion-boson theory.

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

## Related software

To the end of deriving flow equations, other useful software already exists, in particular
- [![QMeS](https://img.shields.io/badge/QMeS-3e4c2d?style=for-the-badge&logo=github)](https://github.com/QMeS-toolbox/QMeS-Derivation), which can derive fRG equations, DSEs, as well as (m)STIs.
- [![DoFun](https://img.shields.io/badge/DoFun-7a5fb0?style=for-the-badge&logo=github)](https://github.com/markusqh/DoFun) for the same tasks.

`FunKit` depends on, and builds upon
-  [![FormTracer](https://img.shields.io/badge/FormTracer-4e94cb?style=for-the-badge&logo=github)](https://github.com/FormTracer/FormTracer) to perform traces over group indices in the derived functional equations,
-  [![TensorBases](https://img.shields.io/badge/TensorBases-bb1a1a?style=for-the-badge&logo=github)](https://github.com/satfra/TensorBases) to handle tensor bases and projections.

These two packages are automatically installed when you import `FunKit` for the first time in a Mathematica notebook or session.

-  `FunKit` provides the algebraic infrastructure for [![DiFfRG](https://img.shields.io/badge/DiFfRG-1f1f1f?style=for-the-badge&logo=github)](https://github.com/satfra/DiFfRG), which is a C++ framework to solve the derived equations numerically.