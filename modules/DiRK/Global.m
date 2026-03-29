(**********************************************************************************
    Global.m -- $GlobalSetup dispatch wrappers for DiRK

    Provides setup-free overloads for:
      FMakeDiagrammaticRules

    Also clears and protects:
      dressing, InverseProp
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
