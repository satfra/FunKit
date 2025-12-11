(**********************************************************************************
    Global Setup: If $GlobalSetup is set, all functions that take a setup as first
    argument will use this setup automatically if called without setup.
**********************************************************************************)

MakeDiagrammaticRules[] /; Head[$GlobalSetup] =!= Symbol :=
    MakeDiagrammaticRules[$GlobalSetup];

(*Make sure dressing, InverseProp is properly (not) defined*)

Unprotect @ dressing;

ClearAll[dressing];

Protect @ dressing;

Unprotect @ InverseProp;

ClearAll[InverseProp];

Protect @ InverseProp;
