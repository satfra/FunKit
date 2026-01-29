(**********************************************************************************
    Taking functional derivatives of expressions.
**********************************************************************************)

$userRules = {};

FAddFDRule[obj_, wrt_, res_] :=
    Module[{},
        AppendTo[$userRules, {obj, wrt, res}];
        Print["$userRules is now: ", $userRules];
    ];

SetAttributes[FAddFDRule, HoldAll];

FClearFDRules[] :=
    Module[{},
        $userRules = {};
    ];

FunctionalD::malformed = "Cannot take a derivative of `1`. Expression is either malformed or this is a bug.";

SymmetricDerivative::malformed = "Cannot take symmetric derivative with given arguments: number of fields do not match.";

SymmetricDerivative[fields1_, ind1_, fields2_, ind2_] :=
    Module[{ret, perms,pidx,idx},
        (*Build all permuatations*)
        If[Length[fields1] =!= Length[fields2],
            Message[SymmetricDerivative::malformed];
            Abort[];
        ];
        perms = Permutations[Range[Length[fields2]]];
        ret = Table[Times@@Table[
            \[Gamma][{fields1[[idx]] , fields2[[perms[[pidx, idx]]]]} , {ind1[[idx]], -ind2[[perms[[pidx, idx]]]]}]
            ,
            {idx, 1, Length[perms[[pidx]]]}],{pidx, 1, Length[perms]}];
        Return[SymmetryFactor[fields2, ind2] * Total[ret]];
    ];

FunctionalD[setup_, expr_, v : (f_[_] | {f_[_], _Integer}).., OptionsPattern[]] :=
    Internal`InheritedBlock[
        {f, nonConst, rule, i}
        ,
        FunKitDebug[3, "Taking functional derivative of ", expr, " w.r.t. ", {v}];
        Unprotect[f];
        nonConst = DeleteDuplicates @ Sort @ ({f, Power} \[Union] $CorrelationFunctions);
        (*Apply user-defined rules*)
        Do[
            If[MemberQ[rule, f, Infinity],
                f /: D[rule[[1]], rule[[2]], NonConstants -> nonConst] := Evaluate[rule[[3]]]
            ]
            ,
            {rule, $userRules}
        ];
        (*Rule for normal functional derivatives*)
        f /: D[f[x_], f[y_], NonConstants -> nonConst] := \[Gamma][{f, f}, {-y, x}];
        (*Rule for normal functional derivatives, but AnyField*)
        f /: D[AnyField[x_], f[y_], NonConstants -> nonConst] := \[Gamma][{f, AnyField}, {-y, x}];
        (*Rule for taking derivatives with AnyField*)
        If[f === AnyField,
            Map[(f /: D[#[x_], f[y_], NonConstants -> nonConst] := \[Gamma][{f, #}, {-y, x}])&, GetAllFields[setup]];
        ];
        (*Ignore fields without indices. These are usually tags*)
        f /: D[f, f[y_], NonConstants -> nonConst] := 0;
        (*\[Delta][#,y]&;*)
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
        f /: D[FTerm[a___], f[y_], NonConstants -> nonConst] := FTerm[FDOp[f[y]], a];
        f /: D[FEx[a___], f[y_], NonConstants -> nonConst] :=
            (
                Message[FunctionalD::malformed, FEx[a]];
                Abort[]
            );
        (*Chain rules*)
        f /: D[Divide[a_, b_], f[y_], NonConstants -> nonConst] :=
            Module[{da, db},
                FunKitDebug[6, "NestRule: Divide"];
                da = D[a, f[y], NonConstants -> nonConst];
                db = D[b, f[y], NonConstants -> nonConst];
                FEx[FTerm[da, b] / b^2, FTerm[-1, a, db] / b^2]
            ];
        f /: D[Times[any__], f[y_], NonConstants -> nonConst] /; Length[{any}] > 2 :=
            Module[{aa},
                aa = {any};
                aa =
                    Flatten @
                        Map[
                            If[Head[#] === Times,
                                List @@ #
                                ,
                                #
                            ]&
                            ,
                            aa
                        ];
                FunKitDebug[6, "NestRule: ", Times];
                FEx @@ Table[FTerm[Sequence @@ ReplacePart[aa, i -> D[{aa[[i]]}, f[y], NonConstants -> nonConst]]], {i, 1, Length[aa]}]
            ];
        f /: D[g_[FTerm[a___]], f[y_], NonConstants -> nonConst] :=
            (
                FunKitDebug[6, "NestRule: ", g];
                FTerm[g'[FTerm[a]], FDOp[f[y]], a]
            );
        f /: D[Power[FTerm[a___], b_], f[y_], NonConstants -> nonConst] :=
            (
                FunKitDebug[6, "NestRule: Power"];
                FTerm[b, Power[FTerm[a], b - 1], FDOp[f[y]], a]
            );
        f /: D[Power[a_, FTerm[b___]], f[y_], NonConstants -> nonConst] :=
            (
                FunKitDebug[6, "NestRule: Power base"];
                FTerm[Log[a], Power[a, FTerm[b]], FDOp[f[y]], b]
            );
        Protect[f];
        D[expr, v, NonConstants -> nonConst]
    ];

FunctionalD[setup_, expr_, v : (f_[_List, _List] | {f_[_List, _List], _Integer}).., OptionsPattern[]] :=
    Internal`InheritedBlock[
        {f, nonConst, rule}
        ,
        Unprotect[f];
        nonConst = DeleteDuplicates @ Sort @ ({f, Power} \[Union] $CorrelationFunctions);
        (*Apply user-defined rules*)
        Do[
            If[MemberQ[rule, f, Infinity],
                f /: D[rule[[1]], rule[[2]], NonConstants -> nonConst] := Evaluate[rule[[3]]]
            ]
            ,
            {rule, $userRules}
        ];
        (*Rule for normal functional derivatives*)
        (*f /: D[f[{f1_, f2_}, {i_, j_}], f[{f3_, f4_}, {k_, l_}], NonConstants -> nonConst] := SymmetryFactor[{f1, f2}, {k, l}] \[Gamma][{f1, f3}, {-k, -i}] \[Gamma][{f2, f4}, {-l, -j}];*)
        f /: D[f[{f1__}, {i1__}], f[{f2__}, {i2__}], NonConstants -> nonConst] :=
            SymmetricDerivative[{f1}, {i1}, {f2}, {i2}];
        (*Rule for normal functional derivatives, but AnyField*)
        (*No derivatives of FTerm, FEx*)
        f /: D[FTerm[a___], f[y__], NonConstants -> nonConst] := FTerm[FDOp[f[y]], a];
        f /: D[FEx[a___], f[y__], NonConstants -> nonConst] :=
            (
                Message[FunctionalD::malformed, FEx[a]];
                Abort[]
            );
        (*Chain rules*)
        f /: D[Divide[a_, b_], f[y__], NonConstants -> nonConst] :=
            Module[{da, db},
                FunKitDebug[6, "NestRule: Divide"];
                da = D[a, f[y], NonConstants -> nonConst];
                db = D[b, f[y], NonConstants -> nonConst];
                FEx[FTerm[da, b] / b^2, FTerm[-1, a, db] / b^2]
            ];
        f /: D[Times[any__], f[y__], NonConstants -> nonConst] /; Length[{any}] > 2 :=
            Module[{aa},
                aa = {any};
                aa =
                    Flatten @
                        Map[
                            If[Head[#] === Times,
                                List @@ #
                                ,
                                #
                            ]&
                            ,
                            aa
                        ];
                FunKitDebug[6, "NestRule: ", Times];
                FEx @@ Table[FTerm[Sequence @@ ReplacePart[aa, i -> D[{aa[[i]]}, f[y], NonConstants -> nonConst]]], {i, 1, Length[aa]}]
            ];
        f /: D[g_[FTerm[a___]], f[y__], NonConstants -> nonConst] :=
            (
                FunKitDebug[6, "NestRule: ", g];
                FTerm[g'[FTerm[a]], FDOp[f[y]], a]
            );
        f /: D[Power[FTerm[a___], b_], f[y__], NonConstants -> nonConst] :=
            (
                FunKitDebug[6, "NestRule: Power"];
                FTerm[b, Power[FTerm[a], b - 1], FDOp[f[y]], a]
            );
        f /: D[Power[a_, FTerm[b___]], f[y__], NonConstants -> nonConst] :=
            (
                FunKitDebug[6, "NestRule: Power base"];
                FTerm[Log[a], Power[a, FTerm[b]], FDOp[f[y]], b]
            );
        Protect[f];
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
