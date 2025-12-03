(**********************************************************************************
    Global Setup
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

TakeDerivatives[expr_, derivativeList_] /; Head[$GlobalSetup] =!= Symbol :=
    TakeDerivatives[$GlobalSetup, expr, derivativeList, "Symmetries" -> {}];

TakeDerivatives[expr_, derivativeList_, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    TakeDerivatives[$GlobalSetup, expr, derivativeList, "Symmetries" -> OptionValue["Symmetries"]];

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

ResolveDerivatives[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    ResolveDerivatives[$GlobalSetup, expr, "Symmetries" -> {}];

ResolveDerivatives[expr_, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    ResolveDerivatives[$GlobalSetup, expr, "Symmetries" -> OptionValue["Symmetries"]];

ResolveFDOp[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    ResolveFDOp[$GlobalSetup, expr];

FOrderFields[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FOrderFields[$GlobalSetup, expr];

(**********************************************************************************
    Global Variables
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
    Modifying/Showing the global lists
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
    Set/unset automatic simplification
**********************************************************************************)

$AutoBuildSymmetryList = True;

FSetAutoBuildSymmetryList[flag_:True] :=
    $AutoBuildSymmetryList = flag;

$AutoSimplify = True;

FSetAutoSimplify[flag_:True] :=
    $AutoSimplify = flag;

FEmptySetup :=
    <|"FieldSpace" -> <|"Commuting" -> {}, "Grassmann" -> {}|>|>;
