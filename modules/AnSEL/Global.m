(**********************************************************************************
    Global Setup: If $GlobalSetup is set, all functions that take a setup as first
    argument will use this setup automatically if called without setup.
**********************************************************************************)

FDisconnectedQ[expr_FTerm] /; Head[$GlobalSetup] =!= Symbol :=
    FDisconnectedQ[$GlobalSetup, expr];

FDisconnectedQ[expr_FEx] /; Head[$GlobalSetup] =!= Symbol :=
    FDisconnectedQ[$GlobalSetup, expr];

FDisconnectedQ[expr_FTerm] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FDisconnectedQ[expr_FEx] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FRoute[expr_FEx] /; Head[$GlobalSetup] =!= Symbol :=
    FRoute[$GlobalSetup, expr];

FRoute[expr_FTerm] /; Head[$GlobalSetup] =!= Symbol :=
    FRoute[$GlobalSetup, expr];

FUnroute[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FUnroute[$GlobalSetup, expr];

FSimplify[expr_FEx] /; Head[$GlobalSetup] =!= Symbol :=
    FSimplify[$GlobalSetup, expr];

FSimplify[expr_FTerm] /; Head[$GlobalSetup] =!= Symbol :=
    FSimplify[$GlobalSetup, expr];

FSimplify[expr_FEx, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    FSimplify[$GlobalSetup, expr, (Sequence @@ Thread[Rule @@ {#, OptionValue[FSimplify, #]}]& @ Keys[Options[FSimplify]])];

FMakeSymmetryList[fields_List] /; Head[$GlobalSetup] =!= Symbol :=
    FMakeSymmetryList[$GlobalSetup, fields];

FMakeSymmetryList[fields_List, indices_List] /; Head[$GlobalSetup] =!= Symbol :=
    FMakeSymmetryList[$GlobalSetup, fields, indices];

(* Fallback definitions when $GlobalSetup is not set *)

FRoute[expr_FEx] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FRoute[expr_FTerm] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FUnroute[expr_FEx] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FUnroute[expr_FTerm] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FUnroute[expr_Association] /; isLoopAssociation[expr] || isRoutedAssociation[expr] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FSimplify[expr_FEx] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FSimplify[expr_FTerm] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

FMakeSymmetryList[fields_List] :=
    (Message[FunKit::noGlobalSetup]; Abort[]);

(**********************************************************************************
    Global Variables
**********************************************************************************)

FSetLoopMomentumName[name_String] :=
    Module[{},
        If[StringQ[$loopMomentumName],
            Unprotect @@ Table[$loopMomentumName <> ToString[idx], {idx, 1, 50}];
            Unprotect @@ Table[$loopMomentumName <> "f" <> ToString[idx], {idx, 1, 50}];
            Unprotect @@ Table[$loopMomentumName <> "fb" <> ToString[idx], {idx, 1, 50}];
        ];
        $loopMomentumName = name;
        ClearAll @@ Table[$loopMomentumName <> ToString[idx], {idx, 1, 50}];
        ClearAll @@ Table[$loopMomentumName <> "f" <> ToString[idx], {idx, 1, 50}];
        ClearAll @@ Table[$loopMomentumName <> "fb" <> ToString[idx], {idx, 1, 50}];
        Protect @@ Table[$loopMomentumName <> ToString[idx], {idx, 1, 50}];
        Protect @@ Table[$loopMomentumName <> "f" <> ToString[idx], {idx, 1, 50}];
        Protect @@ Table[$loopMomentumName <> "fb" <> ToString[idx], {idx, 1, 50}];
        Unprotect[$availableLoopMomenta, $availableLoopMomentaf, $availableLoopMomentafb];
        $availableLoopMomenta := Table[Symbol[$loopMomentumName <> ToString[idx]], {idx, 1, 50}];
        $availableLoopMomentaf := Table[Symbol[$loopMomentumName <> "f" <> ToString[idx]], {idx, 1, 50}];
        $availableLoopMomentafb := Table[Symbol[$loopMomentumName <> "fb" <> ToString[idx]], {idx, 1, 50}];
        Protect[$availableLoopMomenta, $availableLoopMomentaf, $availableLoopMomentafb];
    ];

FSetLoopMomentumName["l"];

FSetLoopMomentumName[___] :=
    (
        Message[FunKit::invalidArguments, FSetLoopMomentumName];
        Abort[]
    );
