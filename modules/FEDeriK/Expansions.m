(**********************************************************************************
     Expand Powers of FTerm and FEx up to a given order
**********************************************************************************)

FExpand[setup_, expr_FTerm, order_Integer] /; order >= 0 :=
    Module[{ret = expr, n, i, dummy},
        AssertFSetup[setup];
        ret = ret //. Power[a_FTerm, b_] /; FreeQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
        ret = ret //. Power[a_, b_FTerm] /; FreeQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
        ret = ret //. Power[a_FEx, b_] /; FreeQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
        ret = ret //. Power[a_, b_FEx] /; FreeQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
        ret
    ];

FExpand[setup_, expr_FEx, order_Integer] /; order >= 0 :=
    Map[FExpand[setup, #, order]&, expr]

FExpand::negativeOrder = "FExpand requires a non-negative expansion order. Got `1`.";

FExpand[setup_, expr_, order_Integer] /; order < 0 :=
    (Message[FExpand::negativeOrder, order]; Abort[]);

FExpand::missingOrder = "FExpand requires an expansion order. Use FExpand[setup, expr, order] where order is a non-negative integer.";

FExpand[setup_, expr_, order_] :=
    (Message[FunKit::invalidArguments, FExpand]; Abort[]);

FExpand[setup_, expr_] :=
    (Message[FExpand::missingOrder]; Abort[]);

(**********************************************************************************
     Expand Powers of derivative operators up to a given order
**********************************************************************************)

DExpand[setup_, expr_FTerm, order_Integer] /; order >= 0 :=
    Module[
        {ret = expr, n, i, dummy}
        ,
        AssertFSetup[setup];
        (*We need to block the FDOp definitions to use SeriesCoefficient with FDOp*)
        Block[{FDOp},
            ret = ret //. Power[a_FTerm, b_] /; MemberQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
            ret = ret //. Power[a_, b_FTerm] /; MemberQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
            ret = ret //. Power[a_FEx, b_] /; MemberQ[{a}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[dummy^b, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, a], {i, 1, n}]], {n, 0, order}];
            ret = ret //. Power[a_, b_FEx] /; MemberQ[{b}, FDOp[__], Infinity] :> FEx @@ Table[FTerm[SeriesCoefficient[a^dummy, {dummy, 0, n}] ** NonCommutativeMultiply @@ Table[FixIndices[setup, b], {i, 1, n}]], {n, 0, order}];
        ];
        ret
    ];

DExpand[setup_, expr_FEx, order_Integer] /; order >= 0 :=
    Map[DExpand[setup, #, order]&, expr]

DExpand::negativeOrder = "DExpand requires a non-negative expansion order. Got `1`.";

DExpand[setup_, expr_, order_Integer] /; order < 0 :=
    (Message[DExpand::negativeOrder, order]; Abort[]);

DExpand::missingOrder = "DExpand requires an expansion order. Use DExpand[setup, expr, order] where order is a non-negative integer.";

DExpand[setup_, expr_, order_] :=
    (Message[FunKit::invalidArguments, DExpand]; Abort[]);

DExpand[setup_, expr_] :=
    (Message[DExpand::missingOrder]; Abort[]);
