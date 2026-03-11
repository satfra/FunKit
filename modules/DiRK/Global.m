(**********************************************************************************
    Global Setup: If $GlobalSetup is set, all functions that take a setup as first
    argument will use this setup automatically if called without setup.
**********************************************************************************)

FMakeDiagrammaticRules[opts___?OptionQ] /; Head[$GlobalSetup] =!= Symbol :=
    FMakeDiagrammaticRules[$GlobalSetup, opts];

FMakeDiagrammaticRules[] /; Head[$GlobalSetup] === Symbol :=
    (
        Message[FunKit::noGlobalSetup];
        Abort[]
    );

(*Make sure dressing, InverseProp is properly (not) defined*)

Unprotect @ dressing;

ClearAll[dressing];

Protect @ dressing;

Unprotect @ InverseProp;

ClearAll[InverseProp];

Protect @ InverseProp;
