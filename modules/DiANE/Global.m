(**********************************************************************************
    Global Setup: If $GlobalSetup is set, all functions that take a setup as first
    argument will use this setup automatically if called without setup.
**********************************************************************************)

FPrint[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FPrint[$GlobalSetup, expr];

FTex[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTex[$GlobalSetup, expr];

FPlot[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FPlot[$GlobalSetup, expr];
