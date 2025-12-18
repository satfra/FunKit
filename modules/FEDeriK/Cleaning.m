(*Given a user-defined term or master equation, give all (closed) indices unique names.*)

FixIndices[setup_, expr_List] :=
    Module[
        {}
        ,
        (*Indices should be fixed on a per-term basis to ensure we do not mess up things*)
        Return[FixIndices[setup, #]& /@ expr];
    ];

FixIndices[setup_, expr_FEx] :=
    Module[
        {}
        ,
        (*Indices should be fixed on a per-term basis to ensure we do not mess up things*)
        Return[FixIndices[setup, #]& /@ expr];
    ];

FixIndices::invalidIndices = "Invalid superindices: `1`"

FixIndices[setup_, expr_FTerm] :=
    Module[
        {indices, newIndices, replacements, indexedObjects, ret = expr}
        ,
        (*First, take care of nested sub-terms*)
        ret = FTerm @@ ((List @@ ret) /. FTerm[a__] :> FixIndices[setup, FTerm[a]]);
        (*Now check if everything is alright*)
        If[Not @ SuperIndicesValid[setup, ret],
            Message[FixIndices::invalidIndices, ret];
            Abort[]
        ];
        indices = GetClosedSuperIndices[setup, ret];
        newIndices = Map[Symbol @ SymbolName @ Unique[StringReplace[ToString[#], i : DigitCharacter.. :> ""]]&, indices];
        replacements = Thread[indices -> newIndices];
        Return[ret /. replacements];
    ];

FixIndices::noTerm = "The given expression \"`1`\" is neither an FTerm nor a FEx!"

FixIndices[setup_, expr_] :=
    Module[{},
        Message[FixIndices::noTerm, expr];
        Abort[];
    ];

FDOp::cannotProcess = "The FDOp in `1` cannot be processed. Please expand the term first";

FDOpCount[expr_] :=
    Module[{i},
        expr //.
            Power[FDOp[a__], n_Integer] /; n < 0 :>
                (
                    Message[FDOp::cannotProcess, expr];
                    Abort[]
                );
        expr //. Power[FDOp[a__], n_Integer] :> NonCommutativeMultiply @@ (Table[FDOp[a], {i, 1, n}]) // Count[{#}, FDOp[___], Infinity]&
    ];

(* ::Input::Initialization:: *)

FTerm::GrassmannCountError = "The factor `1` has multiple Grassmanns in a single factor.";

FTerm::FDOpCountError = "The factor `1` has multiple FDOps in a single factor."; FTerm::GrassmannOpen = "The factor `1` has open Grassmann factors.";

(* Simplify a term appearing in an equation. Try to merge as many factors as possible, while not changing the Grassmann structure of the term.*)

ReduceFTerm[setup_, term_] :=
    Module[{reduced = Join[{1}, List @@ term], fields, mergeGrassmanFactors, curRedIg, nextRedIg, curGCount, nextGCount, i, ignore},
        AssertFSetup[setup];
        AssertFTerm[term];
        fields = GetAllFields[setup];
        (*Reduce nested FTerms and such first*)
        reduced = reduced /. FEx[a__] :> ReduceFEx[setup, FEx[a]];
        (*TODO: find a way to not reduce terms twice*)
        reduced = reduced /. FTerm[a__] :> ReduceFTerm[setup, FTerm[a]];
        (*TODO: Ensure that nested terms are Grassmann-neutral*)
        reduced /.
            FTerm[a__] :>
                If[Mod[GrassmannCount[setup, FTerm[a]], 2] =!= 0,
                    Message[FTerm::GrassmannOpen, FTerm[a]];
                    Abort[]
                ];
        (*Merge scalar terms with the closest Grassman term. We need to "vanish" nested FTerms, to make sure we do not overcount.*)
        i = 1;
        While[
            i < Length[reduced]
            ,
            curRedIg = reduced[[-i]] /. FTerm[__] :> ignore;
            nextRedIg = reduced[[-i - 1]] //. FTerm[__] :> 1;
            curGCount = GrassmannCount[setup, curRedIg];
            nextGCount = GrassmannCount[setup, nextRedIg];
            If[FDOpCount[curRedIg] > 1,
                Message[FTerm::FDOpCountError, curRedIg];
                Abort[]
            ];
            If[curGCount > 1,
                Message[FTerm::GrassmannCountError, reduced[[-i]]];
                Abort[]
            ];
            If[(curGCount == 0 || nextGCount == 0) && (FDOpCount[curRedIg] == 0 && FDOpCount[nextRedIg] == 0) && (And @@ Map[FreeQ[curRedIg, #, {1, 5}]&, $OrderedObjects]) && (And @@ Map[FreeQ[nextRedIg, #, {1, 5}]&, $OrderedObjects]),
                reduced = Join[reduced[[ ;; -i - 2]], {reduced[[-i - 1]] * reduced[[-i]]}, reduced[[-i + 1 ;; ]]]
                ,
                i++
            ];
        ];
        If[FDOpCount[reduced[[1]] //. FTerm[__] :> 1] > 1,
            Message[FTerm::FDOpCountError, reduced[[1]]];
            Abort[]
        ];
        If[GrassmannCount[setup, reduced[[1]] //. FTerm[__] :> 1] > 1,
            Message[FTerm::GrassmannCountError, reduced[[1]]];
            Abort[]
        ];
        reduced = OrderFields[setup, reduced];
        Return[FTerm @@ reduced];
    ];

(* ::Input::Initialization:: *)

ReduceFEx[setup_, equation_] :=
    Module[{reduced = equation},
        AssertFSetup[setup];
        AssertFEx[reduced];
        reduced = Select[reduced, # =!= {} && # =!= FTerm[0]&];
        (*Amend the index structure*)
        reduced = FixIndices[setup, reduced];
        (*Make sure all terms are reduced*)
        reduced = ReduceFTerm[setup, #]& /@ reduced;
        Return[reduced];
    ];
