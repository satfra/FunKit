# FunKit

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

