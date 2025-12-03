(**********************************************************************************
    Global Setup
**********************************************************************************)

FPrint[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FPrint[$GlobalSetup, expr];

FTex[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTex[$GlobalSetup, expr];

FPlot[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FPlot[$GlobalSetup, expr];
