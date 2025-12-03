(**********************************************************************************
     Expand Powers of FTerm and FEx up to a given order
**********************************************************************************)

FExpand[setup_, expr_FTerm, order_Integer] :=
    Module[{ret = expr, n, i, dummy},
        ret = ret //. Power[a_FTerm, b_] /; FreeQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
        ret = ret //. Power[a_, b_FTerm] /; FreeQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
        ret = ret //. Power[a_FEx, b_] /; FreeQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
        ret = ret //. Power[a_, b_FEx] /; FreeQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
        ret
    ];

FExpand[setup_, expr_FEx, order_Integer] :=
    Map[FExpand[setup, #, order]&, expr]

(**********************************************************************************
     Expand Powers of derivative operators up to a given order
**********************************************************************************)

DExpand[setup_, expr_FTerm, order_Integer] :=
    Module[
        {ret = expr, n, i, dummy}
        ,
        (*We need to block the FDOp definitions to use SeriesCoefficient with FDOp*)
        Block[{FDOp},
            ret = ret //. Power[a_FTerm, b_] /; MemberQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
            ret = ret //. Power[a_, b_FTerm] /; MemberQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
            ret = ret //. Power[a_FEx, b_] /; MemberQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
            ret = ret //. Power[a_, b_FEx] /; MemberQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
        ];
        ret
    ];

DExpand[setup_, expr_FEx, order_Integer] :=
    Map[DExpand[setup, #, order]&, expr]
