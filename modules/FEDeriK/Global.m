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

FTruncateOpenIndices[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTruncateOpenIndices[$GlobalSetup, expr];

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
    - Default canonical ordering (order of Grassmann fields, anti-Grassmann fields, commuting fields)
**********************************************************************************)

$userCorrelationFunctions = {};

$userIndexedObjects = {};

$userOrderedObjects = {};

$userObjects = {};

$CorrelationFunctions :=
    Join[{Propagator, GammaN}, $userCorrelationFunctions];

$OrderedObjects :=
    Join[$CorrelationFunctions, {R, Rdot, S}, $userOrderedObjects];

$indexedObjects :=
    Join[$OrderedObjects, {\[Gamma], Field}, $userIndexedObjects];

$allObjects :=
    Join[$indexedObjects, {FMinus, SymmetryFactor}, $userObjects];

$nonCommutingObjects :=
    Join[$CorrelationFunctions, {FDOp, Field}];

$MaxDerivativeIterations = 500;

$CanonicalOrdering = "c>ag>g";

Protect @@ $allObjects;

(**********************************************************************************
    Functions to allow the user to add their own objects
**********************************************************************************)

FAddObject[name_Symbol] :=
    Module[{},
        AppendTo[$userObjects, name];
        $userObjects = DeleteDuplicates[$userObjects];
        Protect @@ $allObjects;
    ];

FShowObjects[] :=
    Print[TableForm[Sort @ $allObjects]];

FAddIndexedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userIndexedObjects, name];
        $userIndexedObjects = DeleteDuplicates[$userIndexedObjects];
        Protect @@ $allObjects;
    ];

FShowIndexedObjects[] :=
    Print[TableForm[Sort @ $indexedObjects]];

FAddOrderedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userOrderedObjects, name];
        $userOrderedObjects = DeleteDuplicates[$userOrderedObjects];
        Protect @@ $allObjects;
    ];

FShowOrderedObjects[] :=
    Print[TableForm[Sort @ $userOrderedObjects]];

FAddCorrelationFunction[name_Symbol] :=
    Module[{},
        AppendTo[$userCorrelationFunctions, name];
        $userCorrelationFunctions = DeleteDuplicates[$userCorrelationFunctions];
        Protect @@ $allObjects;
    ];

FShowCorrelationFunctions[] :=
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
