(**********************************************************************************
    Global Setup
**********************************************************************************)

FRoute[expr_FEx] /; Head[$GlobalSetup] =!= Symbol :=
    FRoute[$GlobalSetup, expr];

FUnroute[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FUnroute[$GlobalSetup, expr];

FSimplify[expr_FEx] /; Head[$GlobalSetup] =!= Symbol :=
    FSimplify[$GlobalSetup, expr];

FSimplify[expr_FEx, OptionsPattern[]] /; Head[$GlobalSetup] =!= Symbol :=
    FSimplify[$GlobalSetup, expr, (Sequence @@ Thread[Rule @@ {#, OptionValue[FSimplify, #]}]& @ Keys[Options[FSimplify]])];

(**********************************************************************************
    Global Variables
**********************************************************************************)

SetLoopMomentumName[name_String] :=
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

SetLoopMomentumName["l"];
