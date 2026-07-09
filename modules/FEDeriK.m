(* ::Package:: *)

(* ::Title:: *)

(*FEDeriK - Functional Equation Derivation Kit*)

(* ::Section:: *)

(*Exports*)

(* ::Input::Initialization:: *)

FSetGlobalSetup::usage = "FSetGlobalSetup[setup]
Sets a global setup that is used by all FEDeriK functions when no setup is explicitly provided.
This allows calling functions like FTakeDerivatives[expr, derivativeList] without passing the setup each time.
FSetGlobalSetup[] clears the global setup.";

FTruncate::usage = "FTruncate[setup, expr]
Truncates the given expression according to the truncation tables specified in the setup.
Replaces undetermined fields (AnyField) with explicit fields from the truncation and removes terms not included in the truncation.
The expression must not contain unresolved derivative operators (FDOp).";

FTakeDerivatives::usage = "FTakeDerivatives[setup, expr, derivativeList]
Takes multiple functional derivatives of expr with respect to the fields specified in derivativeList.
Returns the result with all derivative operators resolved.
FTakeDerivatives[setup, expr, derivativeList, \"Symmetries\" -> symmetries] allows specifying symmetries for simplification.
The derivativeList should be a list of field expressions like {A[i1], A[i2]}.";

FAddFDRule::usage = "FAddFDRule[object, wrt, result]
Adds a custom functional derivative rule to be used when taking derivatives with FunctionalD.
TODO";

FClearFDRules::usage = "FClearFDRules[]
Clears all custom functional derivative rules added with FAddFDRule.";

QMeSForm::usage = "QMeSForm[setup, expr]
Converts FunKit expressions to QMeS-style notation.
For superindex input: produces QMeS native Association format (inverse of FunKitForm).
For routed input (FEx, FTerm, or Association): produces QMeS named-symbol format.
Uses the canonical ordering 'c>ag>g' for field arrangement.";

DoFunForm::usage = "DoFunForm[setup, expr]
Transforms expressions from FunKit-style notation to DoFun-style notation.
For superindex input: produces DoFun symbolic format (op[P[...], V[...], ...]).
For routed input (FEx, FTerm, or Association): produces DoFun algebraic format.";

FunKitForm::usage = "FunKitForm[expr]
Transforms expressions from QMeS-style or DoFun-style notation to FunKit-style notation.
Handles QMeS native Association format and DoFun symbolic (op[...]) format.
FunKitForm[setup, expr] also handles QMeS named-symbol format (requires setup for field name parsing).
DoFun algebraic format (with explicit->False) is detected automatically without setup.";

FExpand::usage = "FExpand[setup, expr, order]
Expands powers of FTerm and FEx expressions up to the specified order.
This is useful for expanding expressions like (FTerm[...])^n into explicit sums of terms.
Automatically fixes indices to ensure uniqueness after expansion.";

DExpand::usage = "DExpand[setup, expr, order]
Expands expressions containing derivative operators up to the specified order.
Similar to FExpand but specifically handles expansions involving functional derivatives.
Used for perturbative expansions in functional methods.";

FMakeClassicalAction::usage = "FMakeClassicalAction[setup]
Constructs the classical action from the setup's truncation table for the object S.
Returns an FEx containing all terms in the classical action with appropriate prefactors.
The setup must contain a truncation table with key S specifying the field combinations.";

WetterichEquation::usage = "WetterichEquation
The Wetterich equation for the functional renormalization group: \[PartialD]_t \[CapitalGamma] = (1/2) G^{ab} (\[PartialD]_t R)_{ab}.
Returns an FEx representing the right-hand side of the Wetterich equation with AnyField placeholders.
This is a predefined master equation that can be used with FTakeDerivatives to derive flow equations.";

FMakeDSE::usage = "FMakeDSE[setup, field]
Constructs the Dyson-Schwinger equation for the specified field.
Takes the functional derivative of the classical action with respect to the field and applies the substitution \[CurlyPhi] \[RightArrow] \[CapitalPhi] + G^{ab} \[Delta]/\[Delta]\[CapitalPhi]^b.
Returns the DSE with all derivative operators resolved.";

FResolveDerivatives::usage = "FResolveDerivatives[setup, expr]
Iteratively resolves all functional derivative operators (FDOp) in the expression.
Applies the product rule and functional derivative rules until no FDOp remain.
FResolveDerivatives[setup, expr, \"Symmetries\" -> symmetries] allows specifying symmetries for simplification during resolution.";

GeneralizedFlowEquation::usage = "GeneralizedFlowEquation
The generalized flow equation: \[PartialD]_t \[CapitalGamma] = -\[CapitalPhi]\:0307^a \[CapitalGamma]_a + (1/2) G^{ab} (\[PartialD]_t R)_{ab} + G^{ac} \[CapitalPhi]\:0307^b_c R_{ab}.
Returns an FEx representing the generalized flow equation with AnyField placeholders.
This extends the Wetterich equation to include contributions from time-dependent field expectation values.";

RGInvGeneralizedFlowEquation::usage = "RGInvGeneralizedFlowEquation
The RG-invariant generalized flow equation with additional terms for maintaining RG invariance.
Similar to GeneralizedFlowEquation but includes extra contributions to ensure renormalization group invariance.
Used in advanced functional RG calculations where RG invariance must be preserved exactly.";

FAddIndexedObject::usage = "FAddIndexedObject[name]
Adds a new indexed object to the list of user-defined indexed objects.
Indexed objects are symbols that can carry field and index information in functional expressions.
The new object will be automatically protected and included in derivative calculations.";

FShowIndexedObjects::usage = "FShowIndexedObjects[]
Displays a table of all currently defined indexed objects.
This includes both built-in objects (like Propagator, GammaN, S, R, Rdot) and user-defined objects.
Useful for checking which indexed objects are available for use in expressions.";

FAddOrderedObject::usage = "FAddOrderedObject[name]
Adds a new ordered object to the list of user-defined ordered objects.
Ordered objects are indexed objects that participate in field ordering operations, but not in functional derivative calculations.
The new object will be automatically protected and included in ordering operations.";

FShowOrderedObjects::usage = "FShowOrderedObjects[]
Displays a table of all currently defined ordered objects.
This includes both built-in ordered objects (like R, Rdot, S) and user-defined ones.
Useful for checking which ordered objects are available for use in expressions.";

FAddCorrelationFunction::usage = "FAddCorrelationFunction[name]
Adds a new correlation function to the list of user-defined correlation functions.
Correlation functions are special indexed objects that represent n-point functions in functional methods.
They are treated as non-commuting objects and participate in functional derivative calculations.";

FShowCorrelationFunctions::usage = "FShowCorrelationFunctions[]
Displays a table of all currently defined correlation functions.
This includes built-in correlation functions (Propagator, GammaN) and user-defined ones.
Correlation functions are the primary objects that functional derivatives act upon.";

FAddObject::usage = "FAddObject[name]
Adds a new object to the list of user-defined objects.
These objects can be used in functional expressions but are not treated as indexed objects.
The new object will be automatically protected and included in derivative calculations.";

FShowObjects::usage = "FShowObjects[]
Displays a table of all currently defined objects.
This includes both built-in objects (like FMinus, SymmetryFactor) and user-defined objects.
Useful for checking which objects are available for use in expressions.";

FSetUnorderedIndices::usage = "FSetUnorderedIndices[obj, indices]
Specifies which indices of an indexed object should not be reordered during field ordering operations.
This is useful for objects like Phidot where the last index (representing the field itself) should remain fixed.
The indices parameter can be a single integer or list of integers specifying which index positions to keep unordered.";

FSetSymmetricObject::usage = "FSetSymmetricObject[obj, {fields}]
Defines an indexed object as symmetric in all its indices.
FSetSymmetricObject[obj, {fields}, {positions}] makes the object symmetric only in the specified index positions.
This automatically sorts indices to canonical order and can significantly reduce the number of terms in calculations.";

FOrderFields::usage = "FOrderFields[setup, expr]
Orders fields within the expression according to the canonical ordering currently set for FunKit.
Normally, FunKit takes care of field ordering automatically, but this function allows manual reordering when needed."

FEx::usage = "FEx[term1, term2, ...]
Represents a functional expression as a sum of FTerm objects.
This is the main container for functional equations in FEDeriK.
FEx objects support non-commutative multiplication (**) and automatically handle simplification of zero terms.";

FTerm::usage = "FTerm[factor1, factor2, ...]
Represents a single term in a functional equation as a product of factors.
Factors can be numbers, indexed objects, fields, derivative operators, or appropriate combinations thereof.
Certain restrictions apply: If you put two Grassmann fields into the same factor, using this FTerm will lead to errors being thrown.";

Propagator::usage = "Propagator[{field1, field2}, {index1, index2}]
Represents the two-point correlation function (propagator) G_{field1,field2} with specified indices.
This is a built-in correlation function that appears in functional derivative calculations.";

GammaN::usage = "GammaN[{field1, field2, ...}, {index1, index2, ...}]
Represents the 1PI n-point vertex function \[CapitalGamma]_{field1,field2,...} with specified indices.";

R::usage = "R[{field1, field2}, {index1, index2}]
Represents the regulator function R_{field1,field2} used in functional renormalization group calculations.
Typically appears in expressions involving the Wetterich equation.";

Rdot::usage = "Rdot[{field1, field2}, {index1, index2}]
Represents the time derivative of the regulator function \[PartialD]_t R_{field1,field2}.
Appears in the Wetterich equation and its derivatives.";

S::usage = "S[{field1, field2, ...}, {index1, index2, ...}]
Represents terms in the classical action S with the specified field content and indices.
Used in constructing classical actions and Dyson-Schwinger equations.";

\[Gamma]::usage = "\[Gamma][{field1, field2}, {index1, index2}]
Represents the field metric used for raising and lowering indices in functional expressions.
This symbol appears automatically when functional derivatives are resolved.";

Field::usage = "Field[{field}, {index}]
Represents a field with the specified index. Alternative notation for field[index].";

FDOp::usage = "FDOp[field[index]]
Represents a functional derivative operator \[Delta]/\[Delta]field acting on everything to its right.
FDOp operators are resolved using FResolveDerivatives or FResolveFDOp functions.";

FMinus::usage = "FMinus[{field1, field2}, {index1, index2}]
Represents Grassmann minus signs (-1)^{field1\[CenterDot]field2} arising from commuting Grassmann fields.
This is automatically generated when reordering expressions containing Grassmann fields.";

SymmetryFactor::usage = "SymmetryFactor[{field1, field2, ...}, {index1, index2, ...}]
Represents the symmetry factor associated with identical fields in an expression.
When all fields are specified, it evaluates to 1 / (n1! n2! ...), where ni is the number of occurrences of the unique field i.
";

AnyField::usage = "AnyField
Placeholder symbol representing an undetermined field type.
Gets expanded to explicit fields during the truncation process using FTruncate.";

FResolveFDOp::usage = "FResolveFDOp[setup, expr]
Resolves the rightmost functional derivative operator (FDOp) in the expression.
Applies the product rule and functional derivative rules for one FDOp.
For complete resolution of all derivatives, use FResolveDerivatives instead.";

Phidot::usage = "Phidot[{field}, {index}]
Represents the time derivative of field expectation values \[PartialD]_t\:27e8field\:27e9.
Used in generalized flow equations where field expectation values are time-dependent.
This is a predefined correlation function with special index ordering rules.";

FSetAutoBuildSymmetryList::usage = "FSetAutoBuildSymmetryList[flag]
Sets whether a symmetry list should be automatically built when taking derivatives. Default is True."

FSetAutoSimplify::usage = "FSetAutoSimplify[flag]
Sets whether automatic simplification should be applied when taking derivatives and truncating. Default is True."

FEmptySetup::usage = "FEmptySetup
Returns an empty FEDeriK setup with no fields or truncation tables defined.";

FTruncateOpenIndices::usage = "FTruncateOpenIndices[setup, expr]
Truncates open indices in the given expression according to the truncation tables specified in the setup.
This means explicitly that a sum is taken over all possible fields for every single open index in the expression.";

FSetNotationA::usage = "FSetNotationA[]
Activates the default two-list object notation: obj[{field1, field2, ...}, {index1, index2, ...}].
This is the standard FunKit notation and is active by default at load time.
Call FSetNotationB[] to switch to the field[index] notation, and FSetNotationA[] to switch back.";

FSetNotationB::usage = "FSetNotationB[]
Activates the field[index] object notation: obj[field1[index1], field2[index2], ...].
In this notation each argument to a correlation function directly pairs its field with its index.
Call FSetNotationA[] to restore the default two-list notation.";

$FunKitBackend::usage = "$FunKitBackend
The active computation backend for FTakeDerivatives, FResolveDerivatives, FTruncate and FSimplify: \"Automatic\" (default), \"Cpp\" or \"Mathematica\".
Under \"Automatic\", the C++ backend is built and activated on first pipeline use; if that fails, the Mathematica implementation is used with a warning.
Switch explicitly with FSetBackendCpp[] and FSetBackendMathematica[] (provided by the CoBra module).";

(* ::Section:: *)

(* Begin Private *)

(* ::Input::Initialization:: *)

Begin["`Private`"];

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FunKit] =!= True,
  Message[ModuleLoaded::dependency, "FEDeriK", "FunKit"];
  Abort[];
];

ModuleLoaded[FEDeriK] = True;

(* ::Section:: *)

(* Loading components*)

(* ::Input::Initialization:: *)

(* Global setup *)

Get[$FunKitDirectory <> "modules/FEDeriK/Global.m"];

(* Notation: FTerm and FEx definitions *)

Get[$FunKitDirectory <> "modules/FEDeriK/Notation.m"];

(* Indexed objects *)

Get[$FunKitDirectory <> "modules/FEDeriK/IndexedObjects.m"];

(* Checks and Assertions *)

Get[$FunKitDirectory <> "modules/FEDeriK/Checks.m"];

(* FunctionalD *)

Get[$FunKitDirectory <> "modules/FEDeriK/FunctionalD.m"];

(* SuperIndex Transformations *)

Get[$FunKitDirectory <> "modules/FEDeriK/SuperIndexTransformations.m"];

(* Ordering *)

Get[$FunKitDirectory <> "modules/FEDeriK/Ordering.m"];

(* Compatibility with QMeS and DoFun *)

Get[$FunKitDirectory <> "modules/FEDeriK/Compatibility.m"];

(* Metric factors and FMinus *)

Get[$FunKitDirectory <> "modules/FEDeriK/Metric.m"];

(* Truncation *)

Get[$FunKitDirectory <> "modules/FEDeriK/Truncation.m"];

(*Reduce FTerms and FExs, fix Indices*)

Get[$FunKitDirectory <> "modules/FEDeriK/Cleaning.m"];

(* Power expansions *)

Get[$FunKitDirectory <> "modules/FEDeriK/Expansions.m"];

(* Derivative Algorithm *)

Get[$FunKitDirectory <> "modules/FEDeriK/Derivatives.m"];

(* Common Equations *)

Get[$FunKitDirectory <> "modules/FEDeriK/MasterEquations.m"];

(* ::Section:: *)

(* End Private *)

End[];
