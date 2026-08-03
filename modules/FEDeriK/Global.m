(**********************************************************************************
    Global.m -- $GlobalSetup dispatch wrappers and global object registry

    Provides setup-free overloads for:
      FTruncate, FTruncateOpenIndices, FTakeDerivatives, QMeSForm,
      FExpand, DExpand, FMakeClassicalAction, FMakeDSE,
      FResolveDerivatives, FResolveFDOp, FOrderFields

    Also defines:
      FSetGlobalSetup            -- Sets or clears $GlobalSetup
      FAddObject                 -- Registers a user-defined symbol
      FShowObjects               -- Prints all registered objects
      FAddIndexedObject          -- Registers a user-defined indexed object
      FShowIndexedObjects        -- Prints all indexed objects
      FAddOrderedObject          -- Registers a user-defined ordered object
      FShowOrderedObjects        -- Prints all ordered objects
      FAddCorrelationFunction    -- Registers a user-defined correlation function
      FShowCorrelationFunctions  -- Prints all correlation functions
      FSetAutoBuildSymmetryList   -- Toggles auto symmetry list construction
      FSetAutoSimplify           -- Toggles auto simplification after derivatives
      FEmptySetup                -- Returns a minimal empty setup Association

    Variables:
      $GlobalSetup               -- Currently active setup (protected)
      $userCorrelationFunctions  -- User-registered correlation functions
      $userIndexedObjects        -- User-registered indexed objects
      $userOrderedObjects        -- User-registered ordered objects
      $userObjects               -- User-registered general objects
      $CorrelationFunctions      -- All correlation functions (built-in + user)
      $OrderedObjects            -- All ordered objects
      $indexedObjects            -- All indexed objects
      $allObjects                -- All recognized objects
      $nonCommutingObjects       -- Objects with non-commuting ordering
      $MaxDerivativeIterations   -- Max iterations for derivative resolution (500)
      $CanonicalOrdering         -- Current canonical field ordering string
      $AutoBuildSymmetryList     -- Auto build symmetries flag (default True)
      $AutoSimplify              -- Auto simplify flag (default True)
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
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FTruncateOpenIndices[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTruncateOpenIndices[$GlobalSetup, expr];

FTruncateOpenIndices[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FTakeDerivatives[expr_, derivativeList_] /; Head[$GlobalSetup] =!= Symbol :=
    FTakeDerivatives[$GlobalSetup, expr, derivativeList];

FTakeDerivatives[expr_, derivativeList_, opts : OptionsPattern[FTakeDerivatives]] /; Head[$GlobalSetup] =!= Symbol :=
    FTakeDerivatives[$GlobalSetup, expr, derivativeList, opts];

FTakeDerivatives[expr_, derivativeList_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FTakeDerivatives[expr_, derivativeList_, OptionsPattern[]] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

QMeSForm[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    QMeSForm[$GlobalSetup, expr];

QMeSForm[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

DoFunForm[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    DoFunForm[$GlobalSetup, expr];

DoFunForm[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FunKitForm[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FunKitForm[$GlobalSetup, expr];

FunKitForm[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FExpand[expr_, order_Integer] /; Head[$GlobalSetup] =!= Symbol :=
    FExpand[$GlobalSetup, expr, order];

FExpand[expr_, order_Integer] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

DExpand[expr_, order_Integer] /; Head[$GlobalSetup] =!= Symbol :=
    DExpand[$GlobalSetup, expr, order];

DExpand[expr_, order_Integer] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FMakeClassicalAction[] /; Head[$GlobalSetup] =!= Symbol :=
    FMakeClassicalAction[$GlobalSetup];

FMakeClassicalAction[] /; Head[$GlobalSetup] === Symbol :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FMakeDSE[field_] /; Head[$GlobalSetup] =!= Symbol :=
    FMakeDSE[$GlobalSetup, field];

FMakeDSE[field_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FResolveDerivatives[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveDerivatives[$GlobalSetup, expr];

FResolveDerivatives[expr_, opts : OptionsPattern[FResolveDerivatives]] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveDerivatives[$GlobalSetup, expr, opts];

FResolveDerivatives[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FResolveDerivatives[expr_, OptionsPattern[]] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FResolveFDOp[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FResolveFDOp[$GlobalSetup, expr];

FResolveFDOp[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

FOrderFields[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FOrderFields[$GlobalSetup, expr];

FOrderFields[expr_] :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

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
    $CorrelationFunctions = Join[{Propagator, GammaN}, $userCorrelationFunctions];

$OrderedObjects :=
    $OrderedObjects = Join[$CorrelationFunctions, {R, Rdot, S}, $userOrderedObjects];

$indexedObjects :=
    $indexedObjects = Join[$OrderedObjects, {\[Gamma], Field}, $userIndexedObjects];

$allObjects :=
    $allObjects = Join[$indexedObjects, {FMinus, SymmetryFactor}, $userObjects];

$nonCommutingObjects :=
    $nonCommutingObjects = Join[$CorrelationFunctions, {FDOp, Field}];

$ConstantObjects :=
    Join[{R, Rdot, S}, $userOrderedObjects, {\[Gamma], FMinus, SymmetryFactor}, $userObjects];

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
        $allObjects := $allObjects = Join[$indexedObjects, {FMinus, SymmetryFactor}, $userObjects];
        Protect @@ $allObjects;
    ];

FAddObject[name_] :=
    (
        Message[FAddObject::notSymbol, name, Head[name]];
        Abort[]
    );

FShowObjects[] :=
    Print[TableForm[Sort @ $allObjects]];

FAddIndexedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userIndexedObjects, name];
        $userIndexedObjects = DeleteDuplicates[$userIndexedObjects];
        $indexedObjects := $indexedObjects = Join[$OrderedObjects, {\[Gamma], Field}, $userIndexedObjects];
        $allObjects := $allObjects = Join[$indexedObjects, {FMinus, SymmetryFactor}, $userObjects];
        Protect @@ $allObjects;
    ];

FAddIndexedObject[name_] :=
    (
        Message[FAddIndexedObject::notSymbol, name, Head[name]];
        Abort[]
    );

FShowIndexedObjects[] :=
    Print[TableForm[Sort @ $indexedObjects]];

FAddOrderedObject[name_Symbol] :=
    Module[{},
        AppendTo[$userOrderedObjects, name];
        $userOrderedObjects = DeleteDuplicates[$userOrderedObjects];
        $OrderedObjects := $OrderedObjects = Join[$CorrelationFunctions, {R, Rdot, S}, $userOrderedObjects];
        $indexedObjects := $indexedObjects = Join[$OrderedObjects, {\[Gamma], Field}, $userIndexedObjects];
        $allObjects := $allObjects = Join[$indexedObjects, {FMinus, SymmetryFactor}, $userObjects];
        Protect @@ $allObjects;
    ];

FAddOrderedObject[name_] :=
    (
        Message[FAddOrderedObject::notSymbol, name, Head[name]];
        Abort[]
    );

FShowOrderedObjects[] :=
    Print[TableForm[Sort @ $userOrderedObjects]];

FAddCorrelationFunction[name_Symbol] :=
    Module[{},
        AppendTo[$userCorrelationFunctions, name];
        $userCorrelationFunctions = DeleteDuplicates[$userCorrelationFunctions];
        $CorrelationFunctions := $CorrelationFunctions = Join[{Propagator, GammaN}, $userCorrelationFunctions];
        $OrderedObjects := $OrderedObjects = Join[$CorrelationFunctions, {R, Rdot, S}, $userOrderedObjects];
        $indexedObjects := $indexedObjects = Join[$OrderedObjects, {\[Gamma], Field}, $userIndexedObjects];
        $allObjects := $allObjects = Join[$indexedObjects, {FMinus, SymmetryFactor}, $userObjects];
        $nonCommutingObjects := $nonCommutingObjects = Join[$CorrelationFunctions, {FDOp, Field}];
        Protect @@ $allObjects;
    ];

FAddCorrelationFunction[name_] :=
    (
        Message[FAddCorrelationFunction::notSymbol, name, Head[name]];
        Abort[]
    );

FShowCorrelationFunctions[] :=
    Print[TableForm[Sort @ $CorrelationFunctions]];

(**********************************************************************************
    Set/unset automatic simplification and 
    construction of symmetry lists when taking derivatives
**********************************************************************************)

(*Default OFF. Auto-building the full permutation group of the correlator and reducing
  with it is only correct if the contraction the user will apply is covariant under every
  element of that group -- which FunKit cannot know. The group is a property of the
  correlation function, not of an individual diagram, so the reduced expression equals the
  original only after symmetrisation. Supply the symmetries you can license by hand:

      syms = FMakeSymmetryList[FSymmetry[Symmetric, {i1,i2}], ...];
      FTakeDerivatives[WetterichEquation, {...}, "Symmetries" -> syms]

  See SYMMETRY-REDUCTION-DESIGN.md.*)

$AutoBuildSymmetryList = False;

FSetAutoBuildSymmetryList::notBoolean = "The argument `1` must be True or False.";

FSetAutoBuildSymmetryList[] :=
    $AutoBuildSymmetryList = True;

FSetAutoBuildSymmetryList[flag_] /; BooleanQ[flag] :=
    $AutoBuildSymmetryList = flag;

FSetAutoBuildSymmetryList[flag_] :=
    (
        Message[FSetAutoBuildSymmetryList::notBoolean, flag];
        Abort[]
    );

$AutoSimplify = True;

FSetAutoSimplify::notBoolean = "The argument `1` must be True or False.";

FSetAutoSimplify[] :=
    $AutoSimplify = True;

FSetAutoSimplify[flag_] /; BooleanQ[flag] :=
    $AutoSimplify = flag;

FSetAutoSimplify[flag_] :=
    (
        Message[FSetAutoSimplify::notBoolean, flag];
        Abort[]
    );

(**********************************************************************************
    Computation backend selection. The default is "Automatic": the CoBra
    module activates the C++ backend on first pipeline use (building it if
    necessary) and falls back to the pure-Mathematica implementation with a
    warning when that fails. FSetBackendCpp[]/FSetBackendMathematica[] switch
    explicitly. Declared here so FEDeriK works standalone even when CoBra is
    not loaded (in which case "Automatic" resolves to the Mathematica path).
**********************************************************************************)

$FunKitBackend = "Automatic";

(*Guard used at the pipeline branch points; the per-call "Backend" option
  overrides the global flag. CppBackendAutoQ is redefined by CoBra with the
  real auto-activation.*)

CppBackendAutoQ[] :=
    False;

CppBackendActiveQ[Automatic] :=
    Which[
        $FunKitBackend === "Cpp",
            True
        ,
        $FunKitBackend === "Mathematica",
            False
        ,
        $FunKitBackend === "Automatic",
            CppBackendAutoQ[]
        ,
        True,
            Message[FunKit::invalidBackend, $FunKitBackend];
            Abort[]
    ];

CppBackendActiveQ["Cpp"] :=
    True;

CppBackendActiveQ["Mathematica"] :=
    False;

FunKit::invalidBackend = "The \"Backend\" option must be Automatic, \"Cpp\" or \"Mathematica\", but `1` was given.";

CppBackendActiveQ[x_] :=
    (
        Message[FunKit::invalidBackend, x];
        Abort[]
    );

(**********************************************************************************
    An empty setup for default use, testing, etc.
**********************************************************************************)

FEmptySetup :=
    <|"FieldSpace" -> <|"Commuting" -> {}, "Grassmann" -> {}|>|>;
