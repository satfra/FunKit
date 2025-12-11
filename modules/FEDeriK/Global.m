(**********************************************************************************
    Global Setup: If $GlobalSetup is set, all functions that take a setup as first
    argument will use this setup automatically if called without setup.
**********************************************************************************)

Protect[$GlobalSetup];

FSetGlobalSetup[setup_] :=
    Module[{},
        AssertFSetup[setup];
        Unprotect[$GlobalSetup];
        $GlobalSetup = setup;
        Protect[$GlobalSetup];
    ];

FSetGlobalSetup[] :=
    Module[{},
        Unprotect[$GlobalSetup];
        ClearAll[$GlobalSetup];
        Protect[$GlobalSetup];
    ];

FTruncate[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTruncate[$GlobalSetup, expr];

FTakeDerivatives[expr_, derivativeList_] /; Head[$GlobalSetup] =!= Symbol :=
    FTakeDerivatives[$GlobalSetup, expr, derivativeList, "Symmetries" -> {}];

FTakeDerivatives[expr_, derivativeList_, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    FTakeDerivatives[$GlobalSetup, expr, derivativeList, "Symmetries" -> OptionValue["Symmetries"]];

QMeSForm[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    QMeSForm[$GlobalSetup, expr];

FExpand[expr_, order_Integer] /; Head[$GlobalSetup] =!= Symbol :=
    FExpand[$GlobalSetup, expr, order];

DExpand[expr_, order_Integer] /; Head[$GlobalSetup] =!= Symbol :=
    DExpand[$GlobalSetup, expr, order];

MakeClassicalAction[] /; Head[$GlobalSetup] =!= Symbol :=
    MakeClassicalAction[$GlobalSetup];

MakeDSE[field_] /; Head[$GlobalSetup] =!= Symbol :=
    MakeDSE[$GlobalSetup, field];

FResolveDerivatives[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveDerivatives[$GlobalSetup, expr, "Symmetries" -> {}];

FResolveDerivatives[expr_, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveDerivatives[$GlobalSetup, expr, "Symmetries" -> OptionValue["Symmetries"]];

FResolveFDOp[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveFDOp[$GlobalSetup, expr];

FOrderFields[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FOrderFields[$GlobalSetup, expr];

(**********************************************************************************
    Global Variables:
    - What objects are recognized by default
    - Which ones have been registered by the user
    - What is explicitly non-commuting
    - Maximum number of derivative iterations
    - Default canonical ordering (order of bosons, fermions, antifermions)
**********************************************************************************)

$userCorrelationFunctions = {};

$userIndexedObjects = {};

$userOrderedObjects = {};

$CorrelationFunctions :=
    {Propagator, GammaN} \[Union] $userCorrelationFunctions;

$OrderedObjects :=
    $CorrelationFunctions \[Union] {R, Rdot, S} \[Union] $userOrderedObjects;

$indexedObjects :=
    $OrderedObjects \[Union] {ABasis, VBasis, \[Gamma], Field} \[Union] $userIndexedObjects;

$allObjects :=
    {FMinus} \[Union] $indexedObjects

$nonCommutingObjects :=
    $CorrelationFunctions \[Union] {FDOp, Field};

$MaxDerivativeIterations = 500;

$CanonicalOrdering = "b>af>f";

Protect @@ $allObjects;

(**********************************************************************************
    Functions to allow the user to add their own objects
**********************************************************************************)

AddIndexedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userIndexedObjects, name];
        $userIndexedObjects = DeleteDuplicates[$userIndexedObjects];
        Protect @@ $allObjects;
    ];

ShowIndexedObjects[] :=
    Print[TableForm[Sort @ $indexedObjects]];

AddOrderedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userOrderedObjects, name];
        $userOrderedObjects = DeleteDuplicates[$userOrderedObjects];
        Protect @@ $allObjects;
    ];

ShowOrderedObjects[] :=
    Print[TableForm[Sort @ $userOrderedObjects]];

AddCorrelationFunction[name_Symbol] :=
    Module[{},
        AppendTo[$userCorrelationFunctions, name];
        $userCorrelationFunctions = DeleteDuplicates[$userCorrelationFunctions];
        Protect @@ $allObjects;
    ];

ShowCorrelationFunctions[] :=
    Print[TableForm[Sort @ $CorrelationFunctions]];

(**********************************************************************************
    Set/unset automatic simplification and 
    construction of symmetry lists when taking derivatives
**********************************************************************************)

$AutoBuildSymmetryList = True;

FSetAutoBuildSymmetryList[flag_:True] :=
    $AutoBuildSymmetryList = flag;

$AutoSimplify = True;

FSetAutoSimplify[flag_:True] :=
    $AutoSimplify = flag;

(**********************************************************************************
    An empty setup for default use, testing, etc.
**********************************************************************************)

FEmptySetup :=
    <|"FieldSpace" -> <|"Commuting" -> {}, "Grassmann" -> {}|>|>;
