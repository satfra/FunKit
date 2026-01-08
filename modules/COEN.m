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

FORMCode::usage = "FORMCode[expr]
Generates FORM computer algebra system code for the given expression.
Returns code suitable for input to FORM for symbolic manipulation.
Handles polynomials, rational functions, and symbolic expressions.
Useful for interfacing with FORM for large-scale symbolic calculations.";

FortranCode::usage = "FortranCode[expr]
Generates Fortran code for the given mathematical expression.
Returns optimized Fortran code suitable for numerical computation.
Handles complex numbers, mathematical functions, and array operations.
Output follows modern Fortran standards for numerical libraries.";

MakeCppClass::usage = "MakeCppClass[className, expressions]
Generates a complete C++ class definition for evaluating mathematical expressions.
Creates header and implementation files with optimized evaluation methods.
Includes proper C++ class structure with constructors and member functions.
Useful for creating reusable C++ numerical evaluation libraries.";

MakeCppHeader::usage = "MakeCppHeader[className, expressions]
Generates C++ header file declarations for mathematical expression evaluation.
Creates class interface definitions and function prototypes.
Designed to work with MakeCppClass for complete C++ code generation.
Produces standard C++ header format with include guards.";

MakeCppBlock::usage = ""

MakeCppFunction::usage = "MakeCppFunction[\"Name\"->name, \"Return\"->returnType, \"Parameters\"->paramList, \"Body\"->body, ...]
Generates a C++ function definition based on specified options. See Options[MakeCppFunction] for available settings.";

MakeJuliaFunction::usage = "MakeJuliaFunction[\"Name\"->name, \"Return\"->returnType, \"Parameters\"->paramList, \"Body\"->body, ...]
Generates a Julia function definition based on specified options. See Options[MakeJuliaFunction] for available settings.";

MakeFortranFunction::usage = ""

DefineCppNames::usage = ""

CppForm::usage = ""

FormatCode::usage = ""

MakeParameterString::usage = ""

SetRegisterSize::usage = "SetRegisterSize[n]
Sets the number of available registers for optimization in C++ code generation.
This is in particular important for calculations on the GPU, where the number of registers is very limited.
The default value is 32, but varying $availableRegisters can help optimize performance."

(* ::Section:: *)

(*Begin Private*)

Begin["`Private`"]

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FunKit] =!= True,
	Message[ModuleLoaded::dependency, "COEN", "FunKit"];
	Abort[];
];

If[ModuleLoaded[FEDeriK] =!= True,
	Message[ModuleLoaded::dependency, "COEN", "FunKit"];
	Abort[];
];

ModuleLoaded[COEN] = True;

(* ::Section:: *)

(*Loading Components*)

(* ::Input::Initialization:: *)

(* Tools *)

Get[$FunKitDirectory <> "modules/COEN/Tools.m"];

(* C++ *)

Get[$FunKitDirectory <> "modules/COEN/Cpp.m"];

(* Julia *)

Get[$FunKitDirectory <> "modules/COEN/Julia.m"];

(* Fortran *)

Get[$FunKitDirectory <> "modules/COEN/Fortran.m"];

(* ::Section:: *)

(*End Private*)

End[]
