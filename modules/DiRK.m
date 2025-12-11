(* ::Package:: *)

(* ::Title:: *)

(*DiRK -  Diagrammatic Rule Kit*)

(* ::Section:: *)

(*Exports*)

(* ::Input::Initialization:: *)

FMakeDiagrammaticRules::usage = "FMakeDiagrammaticRules[setup]
Generates replacement rules that convert functional expressions into diagrammatic form.
Creates rules for propagators, vertices, and other diagrammatic elements based on the setup.
Returns a list of transformation rules suitable for use with ReplaceAll (/.).
Essential for converting abstract functional expressions into concrete diagrammatic representations.
Used in conjunction with TensorBases for generating basis-specific rules.";

FSetSymmetricDressing::usage = "FSetSymmetricDressing[object, fields]
FSetSymmetricDressing[object, fields, indices] 
Defines symmetry properties for diagrammatic dressing of objects.
Automatically orders field arguments according to specified symmetries.
The first form symmetrizes over all field arguments.
The second form allows specifying which indices should be symmetrized.
Essential for handling symmetric tensors and avoiding redundant diagrammatic terms.";

dressing::usage = "dressing[object, fields, momentum, arguments]
Represents the diagrammatic dressing of objects with specific field content.
Used internally by the diagrammatic rule generation system.
The object specifies the type (e.g., InverseProp), fields list the field types.
Momentum and arguments specify the kinematic and tensor structure.
Automatically handles symmetrization when FSetSymmetricDressing rules are active.";

InverseProp::usage = "InverseProp[fields, momentum, arguments]
Represents inverse propagator elements in diagrammatic expressions.
Used as a basis element for constructing propagator-based diagrammatic rules.
The fields specify the field types connected by the inverse propagator.
Momentum and arguments determine the kinematic and tensor structure.
Typically appears in dressing expressions for propagator construction.";

(* ::Section::Closed:: *)

(*Begin Private*)

Begin["`Private`"]

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FunKit] =!= True,
    Message[ModuleLoaded::dependency, "DiRK", "FunKit"];
    Abort[];
];

If[ModuleLoaded[FEDeriK] =!= True,
    Message[ModuleLoaded::dependency, "DiRK", "FEDeriK"];
    Abort[];
];

ModuleLoaded[DiRK] = True;

(* ::Section:: *)

(*Loading Components*)

(* Global setup *)

Get[$FunKitDirectory <> "modules/DiRK/Global.m"];

(* Tools *)

Get[$FunKitDirectory <> "modules/DiRK/Tools.m"];

(* Diagrammatic Rules *)

Get[$FunKitDirectory <> "modules/DiRK/FMakeDiagrammaticRules.m"];

(* ::Section:: *)

(*End Private*)

(* ::Input::Initialization:: *)

End[]
