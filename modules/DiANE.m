(* ::Package:: *)

(* ::Title:: *)

(*DiANE - Diagram Arts and Notation of Equations*)

(* ::Section:: *)

(*Exports*)

(* ::Input::Initialization:: *)

FPrint::usage = "FPrint[setup, expr]
Prints functional expressions in formatted mathematical notation using LaTeX rendering.
Converts functional expressions to LaTeX code and displays them using MaTeX.
Automatically handles field notation, indices, and mathematical symbols.
Useful for visualizing complex functional expressions in readable form.
Returns the original expression after printing.";

FTex::usage = "FTex[setup, expr]
Converts functional expressions to LaTeX code for mathematical typesetting.
Generates properly formatted LaTeX strings with correct field notation and indices.
Handles various expression types including FEx, FTerm, and routed expressions.
Supports customizable styling through TeX style settings.
Essential for creating publication-quality mathematical documents.";

FPlot::usage = "FPlot[setup, expr]
Creates graphical plots and visualizations of functional expressions.
Generates diagrams representing the structure and relationships in expressions.
Supports various plotting styles and customization options.
Useful for understanding the diagrammatic structure of functional calculations.
Integrates with Mathematica's plotting capabilities for high-quality output.";

AddTexStyles::usage = "AddTexStyles[styleRules]
Adds custom LaTeX styling rules for specific mathematical objects or fields.
Allows customization of how fields, indices, and operators appear in LaTeX output.
Style rules should be given as replacement rules (e.g., field -> \"\\mathbf{field}\").
Extends the existing TeX style dictionary without overwriting previous settings.";

FSetTexStyles::usage = "FSetTexStyles[styleRules]
Sets the complete LaTeX styling dictionary for mathematical notation.
Replaces all existing TeX styles with the provided style rules.
Style rules define how fields, operators, and indices are formatted in LaTeX.
Use AddTexStyles to extend existing styles rather than replace them completely.
Essential for consistent mathematical notation across documents.";

(* ::Section:: *)

(*Begin Private*)

(* ::Input::Initialization:: *)

Begin["`Private`"]

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FunKit] =!= True,
    Message[ModuleLoaded::dependency, "DiANE", "FunKit"];
    Abort[];
];

If[ModuleLoaded[FEDeriK] =!= True,
    Message[ModuleLoaded::dependency, "DiANE", "FEDeriK"];
    Abort[];
];

If[ModuleLoaded[AnSEL] =!= True,
    Message[ModuleLoaded::dependency, "DiANE", "AnSEL"];
    Abort[];
];

ModuleLoaded[DiANE] = True;

(* ::Section:: *)

(*Loading Components*)

(* ::Input::Initialization:: *)

(* Global setup *)

Get[$FunKitDirectory <> "modules/DiANE/Global.m"];

(* Pretty printing and LaTeX *)

Get[$FunKitDirectory <> "modules/DiANE/FPrint.m"];

(* Diagram drawing *)

Get[$FunKitDirectory <> "modules/DiANE/FPlot.m"];

(* ::Section:: *)

(*End Private*)

(* ::Input::Initialization:: *)

End[];
