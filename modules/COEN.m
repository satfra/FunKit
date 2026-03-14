(* ::Package:: *)

(* ::Input:: *)

(* ::Title:: *)

(*COEN - COde ENgine*)

(* ::Section:: *)

(*Exports*)

(* ::Input::Initialization:: *)

JuliaCode::usage = "JuliaCode[expr]
Generates Julia code for the given mathematical expression.
Returns optimized Julia code that evaluates the expression efficiently.
Handles complex numbers, mathematical functions, and variable substitutions.
Useful for numerical evaluation of symbolic expressions in Julia.";

CppCode::usage = "CppCode[expr]
Generates C++ code for the given mathematical expression.
Returns optimized C++ code using complex<double> for complex numbers.
Handles mathematical functions, operators, and variable substitutions.
Output is suitable for compilation in C++ numerical libraries.";

UseCppPowr::usage = "UseCppPowr[True/False]
Controls whether to use the 'powr' function for power operations in C++ code generation.
UseCppPowr[True] enables the use of powr for better performance.
UseCppPowr[False] uses standard power operations.
Default behavior depends on the target C++ compiler and optimization needs.";

CppCodeFORM::usage = "CppCodeFORM[expr]
Generates FORM computer algebra system code for the given expression.
Returns code suitable for input to FORM for symbolic manipulation.
Handles polynomials, rational functions, and symbolic expressions.
Useful for interfacing with FORM for large-scale symbolic calculations.";

FortranCode::usage = "FortranCode[expr]
Generates Fortran code for the given mathematical expression.
Returns optimized Fortran code suitable for numerical computation.
Handles complex numbers, mathematical functions, and array operations.
Output follows modern Fortran standards for numerical libraries.";

MakeCppClass::usage = "MakeCppClass[\"Name\"->className, \"MembersPublic\"->{...}, \"MembersPrivate\"->{...}, ...]
Generates a complete C++ class definition for evaluating mathematical expressions.
Creates class structure with optional template types, base classes, and access specifiers.
See Options[MakeCppClass] for available settings.";

MakeCppHeader::usage = "MakeCppHeader[\"Includes\"->{...}, \"Body\"->{...}]
Generates a C++ header file with pragma-once guard, include directives, and body content.
See Options[MakeCppHeader] for available settings.";

MakeCppBlock::usage = "MakeCppBlock[\"Includes\"->{...}, \"Body\"->{...}, \"Namespace\"->name]
Generates a C++ source code block with optional namespace wrapping, includes, and body content.
Produces formatted C++ code suitable for compilation.";

MakeCppFunction::usage = "MakeCppFunction[\"Name\"->name, \"Return\"->returnType, \"Parameters\"->paramList, \"Body\"->body, ...]
Generates a C++ function definition based on specified options. See Options[MakeCppFunction] for available settings.";

MakeJuliaFunction::usage = "MakeJuliaFunction[\"Name\"->name, \"Return\"->returnType, \"Parameters\"->paramList, \"Body\"->body, ...]
Generates a Julia function definition based on specified options. See Options[MakeJuliaFunction] for available settings.";

MakeFortranFunction::usage = "MakeFortranFunction[\"Name\"->name, \"Parameters\"->paramList, \"Body\"->body, ...]
MakeFortranFunction[expr, \"Name\"->name, \"Parameters\"->paramList, \"Body\"->body, ...]
Generates a Fortran function definition based on specified options.
The second form additionally generates Fortran code for the given expression.
See Options[MakeFortranFunction] for available settings.";

CppForm::usage = "CppForm[expr]
Converts a Mathematica expression into its C++ code representation.";

JuliaForm::usage = "JuliaForm[expr]
Converts a Mathematica expression into its Julia code representation.";

FormatCppCode::usage = "FormatCppCode[codeString]
Uses clangd, if available on the system, to automatically format a given string of C++ code.";

MakeParameterString::usage = "MakeParameterString[param]
Generates the parameter string for a single parameter, either specified by an Association with keys 'Name', 'Type', 'Const', 'Reference' or by a string giving its name.";

FSetRegisterSize::usage = "FSetRegisterSize[n]
Sets the number of available registers for optimization in C++ code generation.
This is in particular important for calculations on the GPU, where the number of registers is very limited.
The default value is 32, but varying $availableRegisters can help optimize performance."

FSetCodeOptimizationLevel::usage = "FSetCodeOptimizationLevel[n]
Sets the optimization level for C++ code generation.
Level 0: legacy behavior (simple weighted frequency CSE).
Level 1: interpolator hoisting + reciprocal hoisting + CSE.
Level 2: full pipeline including algebraic factoring and register-pressure splitting.
Level 3: GPU-specific passes (FMA restructuring, transcendental hoisting, sub-kernel splitting).
Default is 2."

FSetCodeChunkSize::usage = "FSetCodeChunkSize[n]
Sets the maximum number of terms per accumulator chunk in the register-pressure splitting pass.
Only relevant when $codeOptimizationLevel >= 2 and $codeUseAccumulator is True.
Default is 50."

FSetGPUTarget::usage = "FSetGPUTarget[True/False]
Enables GPU-specific code emission."

FSetFastMath::usage = "FSetFastMath[True/False]
Enables CUDA fast-math intrinsics (__expf, __logf, etc.). Single precision only."

FSetMaxKernelTerms::usage = "FSetMaxKernelTerms[n]
Sets max terms per sub-kernel before splitting. Default 500."

FSetCodePrecision::usage = "FSetCodePrecision[p]
Sets code precision. Accepts \"single\" or \"double\". Default \"double\"."

(* ::Section:: *)

(*Begin Private*)

Begin["`Private`"]

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FunKit] =!= True,
	Message[ModuleLoaded::dependency, "COEN", "FunKit"];
	Abort[];
];

If[ModuleLoaded[FEDeriK] =!= True,
	Message[ModuleLoaded::dependency, "COEN", "FEDeriK"];
	Abort[];
];

ModuleLoaded[COEN] = True;

(* ::Section:: *)

(*Loading Components*)

(* ::Input::Initialization:: *)

(* Tools *)

Get[$FunKitDirectory <> "modules/COEN/Tools.m"];

(* C++ Optimization Pipeline *)

Get[$FunKitDirectory <> "modules/COEN/CppOptimize.m"];

(* C++ *)

Get[$FunKitDirectory <> "modules/COEN/Cpp.m"];

(* Julia *)

Get[$FunKitDirectory <> "modules/COEN/Julia.m"];

(* Fortran *)

Get[$FunKitDirectory <> "modules/COEN/Fortran.m"];

(* ::Section:: *)

(*End Private*)

End[]
