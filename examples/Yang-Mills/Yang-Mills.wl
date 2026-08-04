(* ::Package:: *)

(* ::Title:: *)
(*Yang-Mills flows*)


(* ::Text:: *)
(*Derives the propagator and vertex flows of Yang-Mills theory in a vertex expansion and generates*)
(*the DiFfRG C++ kernels for them, writing into flows/.*)
(**)
(*This file is a .wl package: it carries the same section structure as a notebook and opens as one*)
(*in the front end, but it is plain text and runs headless:*)
(**)
(*    wolfram -script Yang-Mills.wl*)
(**)
(*It needs DiFfRG, a FORM executable (see $FormExecutable below) and, for the kernels, a compiler.*)


(* ::Chapter:: *)
(*Setup*)


(* ::Section:: *)
(*General*)


(* ::Input::Initialization:: *)
Get["DiFfRG`"]


(* ::Text:: *)
(*Everything is written relative to this file, so the script works from any working directory and*)
(*needs no front end. $InputFileName is set both under "wolfram -script" and under Get.*)


(* ::Input::Initialization:: *)
$YangMillsDirectory=
If[$InputFileName==="",
Directory[]
,
DirectoryName[$InputFileName]
];
SetDirectory[$YangMillsDirectory];


(* ::Text:: *)
(*FORM is invoked through tform (its threaded variant). Override the executable with the*)
(*FUNKIT_FORM environment variable if it lives elsewhere.*)


(* ::Input::Initialization:: *)
$FormExecutable=
If[StringQ[Environment["FUNKIT_FORM"]],
Environment["FUNKIT_FORM"]
,
"/usr/bin/tform -w16"
];
DefineFormExecutable[$FormExecutable]


(* ::Section:: *)
(*Defining the theory*)


(* ::Input::Initialization:: *)
fields= <|
"Commuting"-> {A[p,{v, c}]},
"Grassmann"->{{cb[p,{c}],c[p,{c}]}},
(*Ghosts anticommute, but they are periodic in imaginary time and so carry bosonic
Matsubara frequencies. Declaring this routes the ghost loop with a bosonic momentum (l1
rather than lf1). It matters at finite temperature; in the vacuum it only spares us the
lf1->l1 substitution below.*)
"BoseStatistics"->{c}
|>;


truncation=<|
GammaN->{{A,A},{A,A,A},{A,A,A,A},{A,cb,c},{cb,c}},
Propagator->{{A,A},{cb,c}},Rdot->{{A,A},{cb,c}},
S->{{A,A},{A,A,A},{A,A,A,A},{cb,c},{cb,c,A}},
Field->{{}}
|>;


bases=<|
GammaN->{{A,A}->{"AA",1},{A,A,A}->"AAAClass",{A,A,A,A}->"AAAAClass",{A,cb,c}->{"Acbc",1},{cb,c}->"cbc"},
S->{{A,A}->{"AA",1},{A,A,A}->"AAAClass",{A,A,A,A}->"AAAAClass",{A,cb,c}->{"Acbc",1},{cb,c}->"cbc"},
Propagator->{{A,A}->{"AA",1},{cb,c}->"cbc"},
Rdot->{{A,A}->{"AA",1},{cb,c}->"cbc"}
|>;


diagramStyling=<|"Styles"->{A->{Orange},c->{Black,Dashed}}|>;
FSetTexStyles[cb->"\\bar{c}"];


Setup=<|
"FieldSpace"->fields,
"Truncation"->truncation,
"FeynmanRules"->bases,
"DiagramStyling"->diagramStyling
|>;
FSetGlobalSetup[Setup];


(* ::Section:: *)
(*Symmetries*)


(* ::Text:: *)
(*Symmetries are stated by hand: FunKit does not assume any, because which permutations of the*)
(*external legs a diagram may be reduced with depends on the contraction that is applied to it,*)
(*and that is the user's choice.*)
(**)
(*Here every gluonic flow below is projected onto a symmetric momentum configuration with the*)
(*fully symmetric transverse projector ("AAAClassTrans", "AAAAClassTrans"), so the full permutation*)
(*group of the identical gluon legs is a legitimate reduction. Without it the three- and four-gluon*)
(*flows come out as 12 and 57 diagrams instead of 4 and 6, and the generated kernels grow with them.*)
(**)
(*The ghost flows get none: cb, c and A are distinct fields, so their correlators have no leg*)
(*permutation symmetry to exploit.*)


(* ::Input::Initialization:: *)
symsAA=FMakeSymmetryList[Setup,{A[i1],A[i2]}];
symsA3=FMakeSymmetryList[Setup,{A[i1],A[i2],A[i3]}];
symsA4=FMakeSymmetryList[Setup,{A[i1],A[i2],A[i3],A[i4]}];


(* ::Section:: *)
(*Feynman rules*)


(* ::Subsection:: *)
(*Momentum configurations*)


(* ::Input::Initialization:: *)
SP3Patt[p1e_,p2e_,p3e_]:={Sqrt[(sp[p1,p1]+sp[p2,p2]+sp[p3,p3])/3]}/.{p1:>p1e,p2:>p2e,p3:>p3e}//UseLorentzLinearity//FullSimplify;
SP4Patt[p1e_,p2e_,p3e_,p4e_]:={Sqrt[(sp[p1,p1]+sp[p2,p2]+sp[p3,p3]+sp[p4,p4])/4]}/.{p1:>p1e,p2:>p2e,p3:>p3e,p4:>p4e}//UseLorentzLinearity//FullSimplify;


(* ::Subsection:: *)
(*Rules*)


(* ::Input::Initialization:: *)
dressingRules=ReplaceRepeated[#,{
dressing[GammaN,{cb,c},1,{p1_,p2_}]:>-Zc[Sqrt[sp[p2,p2]]]sp[p2,p2],
dressing[GammaN,{A,A},1,{p1_,p2_}]:>ZA[Sqrt[sp[p2,p2]]]sp[p2,p2],

dressing[InverseProp,{cb,c},1,{p1_,p2_}]:>-(Zc[Sqrt[sp[p2,p2]]]sp[p2,p2]+RB[k^2,sp[p2,p2]]Zc[k]),
dressing[InverseProp,{A,A},1,{p1_,p2_}]:>ZA[Sqrt[sp[p2,p2]]]sp[p2,p2]+RB[k^2,sp[p2,p2]]ZA[evP],

dressing[GammaN,{A,cb,c},1,{p1_,p2_,p3_}]:>ZAcbc[p1,p2],
dressing[GammaN,{A,A,A},1,{p1_,p2_,p3_}]:>ZA3[p1,p2],
dressing[GammaN,{A,A,A,A},1,{p1_,p2_,p3_,p4_}]:>ZA4[p1,p2,p3] ,

ZAcbc[p1_,p2_]:>ZAcbc@@SP3Patt[p1,p2,-p1-p2],
ZA3[p1_,p2_]:>ZA3@@SP3Patt[p1,p2,-p1-p2],
ZA4[p1_,p2_,p3_]:>ZA4@@SP4Patt[p1,p2,p3,-p1-p2-p3],

nZA->6,
evP:>(k^nZA+1)^(1/nZA),
devP:>k^(-1+nZA) (1+k^nZA)^(-1+1/nZA),
dressing[Rdot,{A,A},1,{p1_,p2_}]:>ZA[evP]RBdot[k^2,sp[p2,p2]]+RB[k^2,sp[p2,p2]](dtZA[evP]+k*devP*(ZA[1.02evP]-ZA[evP])/(0.02*evP)),
dressing[Rdot,{cb,c},1,{p1_,p2_}]:>Zc[k]RBdot[k^2,sp[p2,p2]]+RB[k^2,sp[p2,p2]](dtZc[k]+k (Zc[1.02*k]-Zc[k])/(0.02*k))
}]&;

FSetSymmetricDressing[GammaN,{A,A}]


(* ::Subsection:: *)
(*Parametrizations*)


(* ::Input::Initialization:: *)
PropParam[expr_]:=UseLorentzLinearity[expr]//.{
lf1->l1,(*We don't care about this in vacuum*)
sp[p1,p1]->p^2,sp[l1,l1]->l1^2,
sp[l1,p1]->l1 p cos[p,l1],
sp[p1,l1]->l1 p cos[p,l1],
Sqrt[a_^2]:>a,(a_^2)^(n_/2):>a^n,
cos[l1,p]:>cos1
};

SP3FormRule=FMakeSPFormRule[{l1,lf1},p,{p1,p2,p3}];
SP4FormRule=FMakeSPFormRule[{l1,lf1},p,{p1,p2,p3,p4}];
SPParam[expr_]:=UseLorentzLinearity[expr]//.{
lf1->l1,(*We don't care about this in vacuum*)
sp[p,p]->p^2,sp[l1,l1]->l1^2,
sp[l1,p1]->p l1 cos[l1,p1],
sp[l1,p2]->p l1 cos[l1,p2],
sp[l1,p3]->p l1 cos[l1,p3],
sp[l1,p4]->p l1 cos[l1,p4],

Sqrt[a_^2]:>a,(a_^2)^(n_/2):>a^n,(a_^2)^(n_/2):>a^n,Power[Power[l1_,2],Rational[n_,2]]:>l1^n,
cos[l1,p1]:>cosl1p1,
cos[l1,p2]:>cosl1p2,
cos[l1,p3]:>cosl1p3,
cos[l1,p4]:>cosl1p4
};

SetNc[3]
$Assumptions=k>0&&p>0&&l1>0&&-1<cos1<1&&-1<cos2<1&&-1<cos3<1;


(* ::Section:: *)
(*Code generation*)


(* ::Input::Initialization:: *)
interpolatorType="SplineInterpolator1D<double, LogarithmicCoordinates1D<double>, GPU_memory>";

kernelParameterList={
<|"Name"->"k","Type"->"double"|>,
(*strong couplings*)
<|"Name"->"ZA3","Type"->interpolatorType,"Const"->True,"Reference"->True|>,
<|"Name"->"ZAcbc","Type"->interpolatorType,"Const"->True,"Reference"->True|>,
<|"Name"->"ZA4","Type"->interpolatorType,"Const"->True,"Reference"->True|>,
(*ghost propagator*)
<|"Name"->"dtZc","Type"->interpolatorType,"Const"->True,"Reference"->True|>,
<|"Name"->"Zc","Type"->interpolatorType,"Const"->True,"Reference"->True|>,
(*glue propagator*)
<|"Name"->"dtZA","Type"->interpolatorType,"Const"->True,"Reference"->True|>,
<|"Name"->"ZA","Type"->interpolatorType,"Const"->True,"Reference"->True|>
};

SP4Defs=DeclareSymmetricPoints4DP4[l1,p,{p1,p2,p3,p4}];
SP3Defs=DeclareSymmetricPoints4DP3[l1,p,{p1,p2,p3}];


FSetCacheDirectory[FileNameJoin[{$YangMillsDirectory,"TraceCache"}]<>"/"]


FSetRegisterSize[64]


(* ::Chapter:: *)
(*Flows*)


(* ::Section:: *)
(*Propagators*)


(* ::Subsection:: *)
(*Gluon propagator*)


(* ::Input::Initialization:: *)
fRGAA=FTakeDerivatives[WetterichEquation,{A[i1],A[i2]},"Symmetries"->symsAA]//FTruncate//FPlot//FRoute//FPrint;

traceExprAA=FTerm[TBGetProjector["AA",1,{i1,i2}/.fRGAA["1-Loop"]["ExternalIndices"]]]**(fRGAA["1-Loop"]["Expression"]/.FMakeDiagrammaticRules[]);
FlowAA=FormTrace[traceExprAA]//dressingRules//PropParam//Simplify;

MakeKernel[FlowAA/p^2,

"Name"->"ZA",
"Integrator"->"Integrator_p2_1ang",
"d"->4,
"AD"->False,
"ctype"->"double",
"Device"->"GPU",
"Type"->"double",

"Parameters"->kernelParameterList,
"IntegrationVariables"->{"l1","cos1"},
"Coordinates"->{"LogarithmicCoordinates1D<double>"},
"CoordinateArguments"->{"p"}]
UpdateFlows["YangMillsFlows"]


(* ::Subsection:: *)
(*Ghost propagator*)


(* ::Input::Initialization:: *)
fRGcbc=FTakeDerivatives[WetterichEquation,{cb[i1],c[i2]}]//FTruncate//FPlot//FRoute//FPrint;

traceExprcbc=FTerm[TBGetProjector["cbc",1,{i1,i2}/.fRGcbc["1-Loop"]["ExternalIndices"]]]**(fRGcbc["1-Loop"]["Expression"]/.FMakeDiagrammaticRules[]);
Flowcbc=FormTrace[traceExprcbc]//dressingRules//PropParam//Simplify;

MakeKernel[-(Flowcbc/p^2),

"Name"->"Zc",
"Integrator"->"Integrator_p2_1ang",
"d"->4,
"AD"->False,
"ctype"->"double",
"Device"->"GPU",
"Type"->"double",

"Parameters"->kernelParameterList,
"IntegrationVariables"->{"l1","cos1"},
"Coordinates"->{"LogarithmicCoordinates1D<double>"},
"CoordinateArguments"->{"p"}]
UpdateFlows["YangMillsFlows"]


(* ::Section:: *)
(*Strong couplings*)


(* ::Subsection:: *)
(*Ghost-gluon vertex*)


(* ::Input::Initialization:: *)
fRGAcbc=FTakeDerivatives[WetterichEquation,{A[i1],cb[i2],c[i3]}]//FTruncate//FSimplify//FPlot//FRoute//FPrint;

projectorAcbc=FTerm[TBGetProjector["Acbc",1,{i1,i2,i3}/.fRGAcbc["1-Loop"]["ExternalIndices"]]];
traceExprAcbc=projectorAcbc**(fRGAcbc["1-Loop"]["Expression"]/.FMakeDiagrammaticRules[]);

FlowAcbc=FormTrace[traceExprAcbc,{},SP3FormRule]//dressingRules//TBProjectToSymmetricPoint[#,l1,p,p1,p2,p3]&//SPParam//Simplify;

MakeKernel[FlowAcbc,

"Name"->"ZAcbc",
"Integrator"->"Integrator_p2_4D_2ang",
"d"->4,
"AD"->False,
"ctype"->"double",
"Device"->"GPU",
"Type"->"double",

"Parameters"->kernelParameterList,
"KernelBody"->SP3Defs,
"IntegrationVariables"->{"l1","cos1","cos2"},
"Coordinates"->{"LogarithmicCoordinates1D<double>"},
"CoordinateArguments"->{"p"}]
UpdateFlows["YangMillsFlows"]


(* ::Subsection:: *)
(*Three-gluon vertex*)


(* ::Input::Initialization:: *)
fRGA3=FTakeDerivatives[WetterichEquation,{A[i1],A[i2],A[i3]},"Symmetries"->symsA3]//FTruncate//FPlot//FRoute//FPrint;

projectorA3=FTerm[TBGetProjector["AAAClassTrans",1,{i1,i2,i3}/.fRGA3["1-Loop"]["ExternalIndices"]]]//TBProjectToSymmetricPoint[#,l1,p,p1,p2,p3]&//Simplify;
traceExprA3=projectorA3**(fRGA3["1-Loop"]["Expression"]/.FMakeDiagrammaticRules[]);
FlowA3=FormTrace["ZA3",traceExprA3,{},SP3FormRule]//dressingRules//TBProjectToSymmetricPoint[#,l1,p,p1,p2,p3]&//SPParam//Simplify;

MakeKernel[FlowA3,

"Name"->"ZA3",
"Integrator"->"Integrator_p2_4D_2ang",
"d"->4,
"AD"->False,
"ctype"->"double",
"Device"->"GPU",
"Type"->"double",

"Parameters"->kernelParameterList,
"KernelBody"->SP3Defs,
"IntegrationVariables"->{"l1","cos1","cos2"},
"Coordinates"->{"LogarithmicCoordinates1D<double>"},
"CoordinateArguments"->{"p"}]
UpdateFlows["YangMillsFlows"]


(* ::Subsection:: *)
(*Four-gluon vertex*)


(* ::Input::Initialization:: *)
fRGA4=FTakeDerivatives[WetterichEquation,{A[i1],A[i2],A[i3],A[i4]},"Symmetries"->symsA4]//FTruncate//FPlot//FRoute//FPrint;

projectorA4=FTerm[TBGetProjector["AAAAClassTrans",1,{i1,i2,i3,i4}/.fRGA4["1-Loop"]["ExternalIndices"]]]//TBProjectToSymmetricPoint[#,l1,p,p1,p2,p3,p4]&//Simplify;
traceExprA4=projectorA4**(fRGA4["1-Loop"]["Expression"]/.FMakeDiagrammaticRules[])//TBProjectToSymmetricPoint[#,l1,p,p1,p2,p3,p4]&;
FlowA4=FormTrace["ZA4",traceExprA4,{},SP4FormRule]//dressingRules//TBProjectToSymmetricPoint[#,l1,p,p1,p2,p3,p4]&//SPParam//Simplify;

MakeKernel[FlowA4,

"Name"->"ZA4",
"Integrator"->"Integrator_p2_4D_3ang",
"d"->4,
"AD"->False,
"ctype"->"double",
"Device"->"GPU",
"Type"->"double",

"Parameters"->kernelParameterList,
"KernelBody"->SP4Defs,
"IntegrationVariables"->{"l1","cos1","cos2","phi"},
"Coordinates"->{"LogarithmicCoordinates1D<double>"},
"CoordinateArguments"->{"p"}]
UpdateFlows["YangMillsFlows"]
