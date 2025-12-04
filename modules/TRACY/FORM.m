(*FORM*)

(* ::Subsection:: *)

(*FormTracer PostReplacement Code*)

(* ::Subsubsection:: *)

(*Tools*)

(* ::Input::Initialization:: *)

GetFTSynonym[symbol_] :=
    Module[{},
        If[symbol === I,
            Return[FTxI // ToString]
        ];
        If[Head[symbol] =!= Symbol,
            Print["The value \"" <> ToString[symbol] <> "\" is not a symbol!"];
            Abort[]
        ];
        If[Not @ MemberQ[FormTracer`GetExtraVarsSynonyms[], symbol, Infinity],
            FormTracer`AddExtraVars[symbol]
        ];
        ToString @ Select[FormTracer`GetExtraVarsSynonyms[], #[[1]] == symbol&][[1, 2]]
    ];

(* ::Input::Initialization:: *)

RemoveFromExtraVars[obj_] :=
    Module[{extraVars, postExtraVars},
        extraVars = FormTracer`GetExtraVars[];
        postExtraVars =
            If[MemberQ[extraVars, obj],
                DeleteCases[extraVars, obj]
                ,
                extraVars
            ];
        If[extraVars =!= postExtraVars,
            FormTracer`DefineExtraVars[postExtraVars];
            Print["Error: Momentum \"" <> ToString[obj] <> "\" had been defined as an extra variable in FormTracer!"];
            Abort[];
        ];
    ];

scallDef = "*** take care to do replacements at all nested levels
#procedure SCALL(F)
#call `F'
argument;
#call `F'
argument;
#call `F'
argument;
#call `F'
argument;
#call `F'
argument;
#call `F'
argument;
#call `F'
argument;
#call `F'
endargument;
argument;
#call `F'
endargument;
endargument;
endargument;
endargument;
endargument;
endargument;
endargument;
#endprocedure
";

(* ::Subsubsection:: *)

(*Finite T Projections*)

(* ::Input::Initialization:: *)

FiniteTFormMomentumExpansion[momenta___] :=
    Module[{Defs, code},
        RemoveFromExtraVars /@ {momenta};
        code = StringTemplate["
Vector `mom`;
AutoDeclare CFunction cos;
Set momFT : `mom`;
"][<|"mom" -> StringRiffle[{momenta}, ","]|>];
        (*The actual procedure can be then appended*)
        code = code ~~ "*** expand scalar products
#procedure ExpandFiniteT()
id FTxsp(p1?momFT,p2?momFT) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?momFT,p2?momFT)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;
#endprocedure

#call SCALL(ExpandFiniteT)
.sort";
        Return[{code}];
    ];

(* ::Input::Initialization:: *)

MakeP0FormRule[{momenta__}, {projections__}] :=
    Module[{momentaList, projectionsList, code},
        RemoveFromExtraVars /@ {momenta};
        momentaList = {momenta};
        projectionsList = {projections};
        projectionsList = Map[ToString[CForm[# /. Complex[re_, im_] :> re + FTxI im /. Thread[FormTracer`GetExtraVarsSynonyms[][[All, 1]] -> FormTracer`GetExtraVarsSynonyms[][[All, 2]]]]]&, projectionsList];
        code = TemplateApply["Vectors `mom`;\nSet momP0: `mom`;\n\n", <|"mom" -> StringRiffle[momentaList, ","]|>];
        code = code ~~ "
*** Expand out all involved momenta
#procedure ExpandP0()
id FTxsp(p1?momP0,p2?momP0) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?momP0,p2?momP0)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;

id FTxsp(p1?momP0,p2?) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?momP0,p2?)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;

id FTxsp(p1?,p2?momP0) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?,p2?momP0)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;
#endprocedure";
        code = code ~~ "
*** Project the zeroth components of the given vectors
#procedure ProjP0\n" ~~ (Table[TemplateApply["id FTxvec(`mom`, 0) = `proj`;
id `mom`(0) = `proj`;
id FTxvec(`mom`, 0)^-1 = (`proj`)^-1;
id `mom`(0)^-1 = (`proj`)^-1;", <|"mom" -> momentaList[[i]], "proj" -> projectionsList[[i]]|>], {i, 1, Length[momentaList]}] // StringRiffle[#, "\n"]&) ~~ "\n#endprocedure\n";
        code = code ~~ "
#call SCALL(ExpandP0)
#call SCALL(ProjP0)
.sort";
        Return[{code}];
    ];

MakeP0Rule[{momenta__}, {projections__}] :=
    Thread[Map[Global`vec[#, 0]&, {momenta}] -> {projections}];

(* ::Subsubsection:: *)

(*Symmetric point projections*)

(* ::Input::Initialization:: *)

MakeSPFormRule[{loopMomenta__}, p_, {momenta__}] :=
    Module[{momentaList, loopMomentaList, Defs, nPt, nLoops, nPtId, nPtCrossId, nPtqId, SPFormRule, repRules, depth = 8, i, j},
        RemoveFromExtraVars /@ {momenta};
        momentaList = {momenta};
        loopMomentaList = {loopMomenta};
        nPt = Length[momentaList];
        (*Definitions need to be customized with the input*)
        SPFormRule = StringTemplate["Vector `exMom`, `loopMom` ,`p`;
Symbol n;
AutoDeclare CFunction cos;
Set exMom : `exMom`;
Set loopMom : `loopMom`;

#define SPOrd \"`order`\"
"][<|"exMom" -> StringRiffle[momentaList, ","], "loopMom" -> StringRiffle[loopMomentaList, ","], "p" -> ToString[p], "order" -> ToString[nPt]|>];
        (*The actual procedure can be then appended*)
        SPFormRule = SPFormRule ~~ "
*** project to the SP
#procedure ProjSP()
id FTxsp(p1?exMom,p1?exMom)^-1 = FTxsp(p,p)^-1;
id FTxsp(p1?exMom,p1?exMom) = FTxsp(p,p);
id FTxsp(p1?exMom,p2?exMom)^-1 = (-FTxsp(p,p)/(`SPOrd'-1))^-1;
id FTxsp(p1?exMom,p2?exMom) = -FTxsp(p,p)/(`SPOrd'-1);

id FTxsp(p1?exMom,l1?loopMom)^-1 = (sqrt(FTxsp(p,p))*sqrt(FTxsp(l1,l1))*cos(p1,l1))^-1;
id FTxsp(p1?exMom,l1?loopMom) = (sqrt(FTxsp(p,p))*sqrt(FTxsp(l1,l1))*cos(p1,l1));
#endprocedure

#call SCALL(ProjSP)
.sort";
        Return[{SPFormRule}];
    ];

MakeSPFiniteTFormRule[{loopMomenta__}, p_, {momenta__}] :=
    Module[{momentaList, loopMomentaList, Defs, nPt, nLoops, nPtId, nPtCrossId, nPtqId, SPFormRule, repRules, depth = 8, i, j},
        RemoveFromExtraVars /@ {momenta};
        momentaList = {momenta};
        loopMomentaList = {loopMomenta};
        nPt = Length[momentaList];
        (*Definitions need to be customized with the input*)
        SPFormRule = StringTemplate["Vector `exMom`, `loopMom` ,`p`;
Symbol n;
AutoDeclare CFunction cos;
Set exMom : `exMom`;
Set loopMom : `loopMom`;

#define SPOrd \"`order`\"
"][<|"exMom" -> StringRiffle[momentaList, ","], "loopMom" -> StringRiffle[loopMomentaList, ","], "p" -> ToString[p], "order" -> ToString[nPt]|>];
        (*The actual procedure can be then appended*)
        SPFormRule = SPFormRule ~~ "

*** expand scalar products
#procedure ExpandSPFiniteT()
id FTxsp(p1?exMom,p2?exMom) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?exMom,p2?loopMom) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?loopMom,p2?exMom) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);
id FTxsp(p1?loopMom,p2?loopMom) = FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0);

id FTxsp(p1?exMom,p2?exMom)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;
id FTxsp(p1?exMom,p2?loopMom)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;
id FTxsp(p1?loopMom,p2?exMom)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;
id FTxsp(p1?loopMom,p2?loopMom)^-1 = (FTxsps(p1,p2) + FTxvec(p1,0)*FTxvec(p2,0))^-1;
#endprocedure

*** project to the SP
#procedure ProjSPFiniteT()
id FTxsps(p1?exMom,p1?exMom)^-1 = FTxsps(p,p)^-1;
id FTxsps(p1?exMom,p1?exMom) = FTxsps(p,p);                                                                    
                                                                 
id FTxsps(p1?exMom,p2?exMom)^-1 = (-FTxsps(p,p)/(`SPOrd'-1))^-1;                                                      
id FTxsps(p1?exMom,p2?exMom) = -FTxsps(p,p)/(`SPOrd'-1);

id FTxsps(p1?exMom,l1?loopMom)^-1 = (sqrt(FTxsps(p,p))*sqrt(FTxsps(l1,l1))*cos(p1,l1))^-1;                      
id FTxsps(p1?exMom,l1?loopMom) = (sqrt(FTxsps(p,p))*sqrt(FTxsps(l1,l1))*cos(p1,l1));
#endprocedure

#call SCALL(ExpandSPFiniteT)
#call SCALL(ProjSPFiniteT)
.sort";
        Return[{SPFormRule}];
    ];

CreateFormSet[] :=
    Module[{session},
        If[FileExistsQ[Directory[] <> "/form.set"],
            Return[]
        ];
        session = StartExternalSession[{"Python", "Evaluator" -> <|"Dependencies" -> {"formtools-formset"}, "EnvironmentName" -> "FunKit"|>}];
        ExternalEvaluate[session, {"import formset", "formset.main(['-o','" <> Directory[] <> "/form.set" <> "','-p','100'])"}];
        DeleteObject[session];
    ];
