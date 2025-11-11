(*FunctionalD*)

(* ::Input::Initialization:: *)

FunctionalD::malformed = "Cannot take a derivative of `1`. Expression is either malformed or this is a bug.";

ClearAll[FunctionalD]

FunctionalD[setup_, expr_, v : (f_[_] | {f_[_], _Integer}).., OptionsPattern[]] :=
    Internal`InheritedBlock[
        {f, nonConst}
        ,
        Unprotect[f];
        nonConst = DeleteDuplicates @ Sort @ ({f, Power} \[Union] $CorrelationFunctions);
        (*Rule for normal functional derivatives*)
        f /: D[f[x_], f[y_], NonConstants -> nonConst] := \[Gamma][{f, f}, {-y, x}];
        (*Rule for normal functional derivatives, but AnyField*)
        f /: D[AnyField[x_], f[y_], NonConstants -> nonConst] := \[Gamma][{f, AnyField}, {-y, x}];
        (*Rule for taking derivatives with AnyField*)
        If[f === AnyField,
            Map[(f /: D[#[x_], f[y_], NonConstants -> nonConst] := \[Gamma][{f, #}, {-y, x}])&, GetAllFields[setup]];
        ];
        (*Ignore fields without indices. These are usually tags*)
        f /: D[f, f[y_], NonConstants -> nonConst] := 0; (*\[Delta][#,y]&;
            
            
            
            
            
            *)
        (*Derivative rules for Correlation functions*)
        Map[(f /: D[#[{a__}, {b__}], f[if_], NonConstants -> nonConst] := #[{f, a}, {-if, b}])&, $CorrelationFunctions];
        (*Special derivative rule for Propagator*)
        f /: D[Propagator[{b_, a_}, {ib_, ia_}], f[if_], NonConstants -> nonConst] :=
            Module[{ic, id, ie, ig},
                ic = Symbol @ SymbolName @ Unique["i"];
                id = Symbol @ SymbolName @ Unique["i"];
                ie = Symbol @ SymbolName @ Unique["i"];
                ig = Symbol @ SymbolName @ Unique["i"];
                FTerm[((-1) FMinus[{a, a}, {id, id}] FMinus[{f, b}, {if, ib}]), Propagator[{b, AnyField}, {ib, ic}], GammaN[{AnyField, f, AnyField}, {-ic, -if, -id}], Propagator[{AnyField, a}, {id, ia}]]
            ];
        (*No derivatives of FTerm, FEx*)
        f /: D[FTerm[a___], f[y_], NonConstants -> nonConst] :=
            (
                Message[FunctionalD::malformed, FTerm[a]];
                Abort[]
            );
        f /: D[FEx[a___], f[y_], NonConstants -> nonConst] :=
            (
                Message[FunctionalD::malformed, FEx[a]];
                Abort[]
            );
        (*Chain rules*)
        f /: D[g_[FTerm[a___]], f[y_], NonConstants -> nonConst] := (FTerm[g'[FTerm[a]], FDOp[f[y]], a]);
        f /: D[Power[FTerm[a___], b_], f[y_], NonConstants -> nonConst] := (FTerm[b, Power[FTerm[a], b - 1], FDOp[f[y]], a]);
        f /: D[Power[a_, FTerm[b___]], f[y_], NonConstants -> nonConst] := (FTerm[Log[a], Power[a, FTerm[b]], FDOp[f[y]], b]);
        Protect[f];
        D[expr, v, NonConstants -> nonConst]
    ];

FunctionalD[setup_, expr_, v : (f_[_List, _List] | {f_[_List, _List], _Integer}).., OptionsPattern[]] :=
    Internal`InheritedBlock[
        {f, nonConst}
        ,
        nonConst = DeleteDuplicates @ Sort @ ({f, Power} \[Union] $CorrelationFunctions);
        (*Rule for normal functional derivatives*)
        f /: D[f[{f1_, f2_}, {i_, j_}], f[{f3_, f4_}, {k_, l_}], NonConstants -> nonConst] := \[Gamma][{f1, f3}, {-k, i}] \[Gamma][{f2, f4}, {-l, j}];
        (*Rule for normal functional derivatives, but AnyField*)
        (*No derivatives of FTerm, FEx*)
        f /: D[FTerm[a___], f[y_], NonConstants -> nonConst] :=
            (
                Message[FunctionalD::malformed, FTerm[a]];
                Abort[]
            );
        f /: D[FEx[a___], f[y_], NonConstants -> nonConst] :=
            (
                Message[FunctionalD::malformed, FEx[a]];
                Abort[]
            );
        D[expr, v, NonConstants -> nonConst]
    ];

FunctionalD::badArgumentFTerm = "Cannot take derivative of an FTerm. Use TakeDerivatives instead.";

FunctionalD[setup_, FTerm[expr_], v : (f_[__] | {f_[__], _Integer}).., OptionsPattern[]] :=
    (
        Message[FunctionalD::badArgumentFTerm];
        Abort[]
    );

FunctionalD::badArgumentFEx = "Cannot take derivative of an FEx. Use TakeDerivatives instead.";

FunctionalD[setup_, FEx[___], v : (f_[__] | {f_[__], _Integer}).., OptionsPattern[]] :=
    (
        Message[FunctionalD::badArgumentFEx];
        Abort[]
    );
