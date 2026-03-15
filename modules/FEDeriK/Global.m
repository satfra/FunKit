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
FTruncate[expr_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FTruncateOpenIndices[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTruncateOpenIndices[$GlobalSetup, expr];
FTruncateOpenIndices[expr_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FTakeDerivatives[expr_, derivativeList_] /; Head[$GlobalSetup] =!= Symbol :=
    FTakeDerivatives[$GlobalSetup, expr, derivativeList, "Symmetries" -> {}];
FTakeDerivatives[expr_, derivativeList_, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    FTakeDerivatives[$GlobalSetup, expr, derivativeList, "Symmetries" -> OptionValue["Symmetries"]];
FTakeDerivatives[expr_, derivativeList_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);
FTakeDerivatives[expr_, derivativeList_, OptionsPattern[]] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

QMeSForm[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    QMeSForm[$GlobalSetup, expr];
QMeSForm[expr_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FExpand[expr_, order_Integer] /; Head[$GlobalSetup] =!= Symbol :=
    FExpand[$GlobalSetup, expr, order];
FExpand[expr_, order_Integer] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

DExpand[expr_, order_Integer] /; Head[$GlobalSetup] =!= Symbol :=
    DExpand[$GlobalSetup, expr, order];
DExpand[expr_, order_Integer] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

MakeClassicalAction[] /; Head[$GlobalSetup] =!= Symbol :=
    MakeClassicalAction[$GlobalSetup];
MakeClassicalAction[] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FMakeDSE[field_] /; Head[$GlobalSetup] =!= Symbol :=
    FMakeDSE[$GlobalSetup, field];
FMakeDSE[field_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FResolveDerivatives[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveDerivatives[$GlobalSetup, expr, "Symmetries" -> {}];
FResolveDerivatives[expr_, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveDerivatives[$GlobalSetup, expr, "Symmetries" -> OptionValue["Symmetries"]];
FResolveDerivatives[expr_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);
FResolveDerivatives[expr_, OptionsPattern[]] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FResolveFDOp[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveFDOp[$GlobalSetup, expr];
FResolveFDOp[expr_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FOrderFields[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FOrderFields[$GlobalSetup, expr];
FOrderFields[expr_] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

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

FAddObject::notSymbol = "The argument `1` must be a Symbol, not a `2`.";
FAddIndexedObject::notSymbol = "The argument `1` must be a Symbol, not a `2`.";
FAddOrderedObject::notSymbol = "The argument `1` must be a Symbol, not a `2`.";
FAddCorrelationFunction::notSymbol = "The argument `1` must be a Symbol, not a `2`.";

FAddObject[name_Symbol] :=
    Module[{},
        AppendTo[$userObjects, name];
        $userObjects = DeleteDuplicates[$userObjects];
        Protect @@ $allObjects;
    ];

FAddObject[name_] :=
    (Message[FAddObject::notSymbol, name, Head[name]]; Abort[]);

FShowObjects[] :=
    Print[TableForm[Sort @ $allObjects]];

FAddIndexedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userIndexedObjects, name];
        $userIndexedObjects = DeleteDuplicates[$userIndexedObjects];
        Protect @@ $allObjects;
    ];

FAddIndexedObject[name_] :=
    (Message[FAddIndexedObject::notSymbol, name, Head[name]]; Abort[]);

FShowIndexedObjects[] :=
    Print[TableForm[Sort @ $indexedObjects]];

FAddOrderedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userOrderedObjects, name];
        $userOrderedObjects = DeleteDuplicates[$userOrderedObjects];
        Protect @@ $allObjects;
    ];

FAddOrderedObject[name_] :=
    (Message[FAddOrderedObject::notSymbol, name, Head[name]]; Abort[]);

FShowOrderedObjects[] :=
    Print[TableForm[Sort @ $userOrderedObjects]];

FAddCorrelationFunction[name_Symbol] :=
    Module[{},
        AppendTo[$userCorrelationFunctions, name];
        $userCorrelationFunctions = DeleteDuplicates[$userCorrelationFunctions];
        Protect @@ $allObjects;
    ];

FAddCorrelationFunction[name_] :=
    (Message[FAddCorrelationFunction::notSymbol, name, Head[name]]; Abort[]);

FShowCorrelationFunctions[] :=
    Print[TableForm[Sort @ $CorrelationFunctions]];

(**********************************************************************************
    Set/unset automatic simplification and 
    construction of symmetry lists when taking derivatives
**********************************************************************************)

$AutoBuildSymmetryList = True;

FSetAutoBuildSymmetryList::notBoolean = "The argument `1` must be True or False.";

FSetAutoBuildSymmetryList[] :=
    $AutoBuildSymmetryList = True;

FSetAutoBuildSymmetryList[flag_] /; BooleanQ[flag] :=
    $AutoBuildSymmetryList = flag;

FSetAutoBuildSymmetryList[flag_] :=
    (Message[FSetAutoBuildSymmetryList::notBoolean, flag]; Abort[]);

$AutoSimplify = True;

FSetAutoSimplify::notBoolean = "The argument `1` must be True or False.";

FSetAutoSimplify[] :=
    $AutoSimplify = True;

FSetAutoSimplify[flag_] /; BooleanQ[flag] :=
    $AutoSimplify = flag;

FSetAutoSimplify[flag_] :=
    (Message[FSetAutoSimplify::notBoolean, flag]; Abort[]);

(**********************************************************************************
    An empty setup for default use, testing, etc.
**********************************************************************************)

FEmptySetup :=
    <|"FieldSpace" -> <|"Commuting" -> {}, "Grassmann" -> {}|>|>;
