(**********************************************************************************
    Global.m -- $GlobalSetup dispatch wrappers for DiANE

    Provides setup-free overloads for:
      FPrint, FTex, FPlot
**********************************************************************************)

FPrint[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FPrint[$GlobalSetup, expr];

FTex[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FTex[$GlobalSetup, expr];

FPlot[expr_] /; Head[$GlobalSetup] =!= Symbol :=
    FPlot[$GlobalSetup, expr];
