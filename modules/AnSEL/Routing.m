(**********************************************************************************
    Routing.m -- Momentum and index routing through diagrams

    Public API:
      FRoute                     -- Routes momenta/indices through FTerm/FEx diagrams
      FUnroute                   -- Reverses routing, restoring superindex form

    Internal:
      FieldSetupIndices          -- Gets the index structure of a field from setup
                                    (used by FRoute)
      fermionicExtMomRouting     -- Detects if external momentum routing is fermionic
                                    (used by FRoute)
      makeMomentaAlternatives    -- Creates bosonic/fermionic momentum alternatives
                                    (used by FRoute for FEx)
      isLoopAssociation          -- Tests if Association is a loop association
                                    (used by FUnroute, DiANE/FPlot, DiANE/FPrint)
      isRoutedAssociation        -- Tests if Association is a routed association
                                    (used by FUnroute, DiANE/FPlot, DiANE/FPrint)
**********************************************************************************)

(*A convenience function to quickly obtain the index structure of a given field.*)

FieldSetupIndices::notFound = "The field `1` was not found in the setup's field space.";

(*Solve a single linear momentum conservation equation sum == 0 for variable mom.
  Faster than Solve[] which dispatches through the full CAS solver.*)

solveLinearMomConservation[sum_, mom_] :=
    Module[{coeff = Coefficient[sum, mom]},
        mom -> -(sum /. mom -> 0) / coeff
    ];

(**********************************************************************************
    Canonicalisation of the routing (used by $routingAlgorithm === "Canonical")

    A routed diagram still carries an unphysical freedom: the loop momentum may be
    relabelled as  l -> sigma l + Delta,  with sigma = +-1 and Delta a combination of
    external momenta. This is a change of integration variable, so it leaves every
    loop integral invariant -- but it moves the integrand around, and FRoute currently
    resolves it by accident (see the solve-for choice `availMomenta[[1]]` below, which
    inherits Mathematica's canonical order over Unique-generated symbol names, and the
    greedy object reordering, which inherits the order of the objects in the FTerm).
    The routing is therefore a function of incidental naming, not of the diagram.

    Two things pin it down.

    (1) PHYSICS. At finite temperature every line carries a Matsubara frequency whose parity
        is fixed by its field's STATISTICS: Fermi lines odd, Bose lines even. (Statistics, not
        Grassmann parity -- a ghost anticommutes but is periodic in imaginary time, hence Bose.
        See HasFermiStatistics.) The naive reading is that a shift is legal only if Delta is
        parity-even, which would make the constraint bite: with fermionic external legs the
        regulator could then never reach the bare loop momentum. That reading is too strong.

        Relabel  l -> sigma l' + Delta  and RE-TAG the loop momentum, t' = t XOR parity(Delta).
        Then for every line  m_i = c_i l + delta_i  (with c_i = +-1):

            m_i    ->  c_i sigma l' + c_i Delta + delta_i
            parity ->  t' XOR parity(Delta) XOR parity(delta_i)
                    =  t XOR parity(delta_i)
                    =  the old parity of m_i.                                    QED

        Every line keeps its statistics, so EVERY shift is legal provided the loop momentum is
        re-tagged. (FSetRoutingAlgorithm["Regulator"] freezes the regulator's momentum WITHOUT
        re-tagging, which is precisely why it is wrong at finite T.)

    (2) QUALITY. d_t R_k is the sharply localised factor in the flow: a shell of radius ~k in
        the loop momentum. Loop integrals are done in spherical coordinates centred at l = 0
        with a radial grid adapted to k, so a regulator sitting on the bare loop momentum gives
        a radially localised, angularly flat integrand, whereas one sitting on l + P with
        |P| >> k gives a thin off-centre shell that is sharply peaked in the angles.

    Together these give a rule with no free parameters left:

        THE LOOP MOMENTUM IS THE MOMENTUM FLOWING THROUGH THE d_t R INSERTION, AND ITS
        STATISTICS IS THAT OF THE REGULATED FIELD.

    The required tag is not an extra assumption: if the incoming routing was valid, the
    regulated line's parity already equals the statistics of its field, t XOR parity(delta_R)
    = t_R -- which is exactly the t' the shift produces. canonicaliseRouting asserts this
    identity rather than trusting it.

    A loop with no d_t R on it (a DSE, or the second loop of a two-loop term) has no such
    anchor, so there we keep the older behaviour: minimise the external content of the lines,
    over parity-EVEN shifts only, leaving the tag alone.
**********************************************************************************)

(*The internal lines of a diagram. The d_t R insertion (Rdot) is what anchors the loop momentum;
  R -- the undifferentiated regulator of the generalized flow -- is a line like any other, and
  only enters the soft cost below.*)

$routingLineHeads = {Propagator, Rdot, R};

$routingRegulatorHeads = {Rdot, R};

$routingAnchorHead = Rdot;

(*Matsubara parity (Z2) of a routed momentum. Both momentum atoms carry their own statistics tag
  -- loopMomentum[sym, isFermi], externalMomentum[sym, isFermi] -- so the parity is just a Z2 dot
  product of the coefficients with those tags.*)

momentumAtoms[m_] :=
    DeleteDuplicates @ Cases[m, _externalMomentum | _loopMomentum, {0, Infinity}];

momentumParity[m_] :=
    Mod[
        Total[
            Map[
                Boole[#[[2]]] * Coefficient[m, #]&
                ,
                momentumAtoms[m]
            ]
        ]
        ,
        2
    ];

(*The {field, momentum} pair of every leg of an indexed object.*)

legFieldMomenta[obj_] :=
    Transpose[{getFields[obj], getIndices[obj][[All, 1]]}];

(*The momentum of the first leg of every line of the given heads. Taking only the first
  leg avoids double-counting the {l, -l} pair of a two-point line.*)

lineMomenta[objects_, heads_] :=
    Cases[objects, o_ /; MemberQ[heads, Head[o]] :> getIndices[o][[1, 1]], {1}];

(*L1 weight of the external content of a momentum, measured in the SYMMETRIC external
  basis. FRoute eliminates the last external leg as p_N = -(p_1 + ... + p_(N-1)), so
  l1 - p1 - p2 - p3 is really l1 + p4: one external insertion, not three. Scoring in the
  eliminated basis would be biased and would reward hiding externals in the dependent leg.
  So restore p_N and minimise over adding lambda*(p_1 + ... + p_N) == 0.*)

externalWeight[m_, extAtoms_] :=
    Module[{coeffs, lambda},
        If[Length[extAtoms] === 0,
            Return[0]
        ];
        coeffs = Append[Coefficient[m, #]& /@ extAtoms, 0];
        Min @ Table[Total[Abs[coeffs + lambda]], {lambda, -2, 2}]
    ];

(*A line is a {l, -l} pair, and which of the two legs comes first is a leg-order convention
  -- one that the C++ and the Mathematica backend happen to choose differently. Orient every
  line the same way, by making the coefficient of the first loop momentum it carries positive,
  so that anything keyed on the line momenta is blind to that convention. Note this still
  distinguishes l from -l as a *relabelling* (l -> -l maps the line l - p to l + p), which is
  what lets the tie-break below fix the overall sign.*)

orientMomentum[m_, loops_] :=
    Module[{c},
        c = FirstCase[Coefficient[m, #]& /@ loops, x_ /; x =!= 0, 0];
        If[c < 0,
            -m
            ,
            m
        ]
    ];

(*Cost of the candidate relabelling l -> sigma l + delta, lexicographic and minimised:
  regulator lines first, then propagators, then a canonical tie-break (which also fixes
  the overall sign).*)

routingCost[objects_, l_, {sigma_, delta_}, extAtoms_, loops_] :=
    Module[{shifted},
        shifted = ExpandAll[objects /. (l -> sigma * l + delta)];
        {
            Total[externalWeight[#, extAtoms]& /@ lineMomenta[shifted, $routingRegulatorHeads]]
            ,
            Total[externalWeight[#, extAtoms]& /@ lineMomenta[shifted, {Propagator}]]
            ,
            Sort[orientMomentum[#, loops]& /@ lineMomenta[shifted, $routingLineHeads]]
        }
    ];

(*The d_t R line that anchors a given loop momentum, if there is one: an Rdot whose leg momentum
  is sigma*l + delta with sigma = +-1 and delta free of every loop momentum (so that the shift
  that makes it bare is a pure external one, and the loop momenta stay independent of each other).
  Returns {field, sigma, delta}, or Missing[] if this loop carries no usable regulator.*)

regulatorAnchor[objects_, l_] :=
    Module[{anchors},
        anchors =
            Cases[
                objects
                ,
                o_ /; Head[o] === $routingAnchorHead :>
                    Module[{m = getIndices[o][[1, 1]], c, delta},
                        c = Coefficient[m, l];
                        delta = ExpandAll[m - c * l];
                        If[(c === 1 || c === -1) && FreeQ[delta, _loopMomentum],
                            {getFields[o][[1]], c, delta}
                            ,
                            Nothing
                        ]
                    ]
                ,
                {1}
            ];
        If[Length[anchors] === 0,
            Missing[]
            ,
            First[anchors]
        ]
    ];

(*Candidate shifts for a loop with NO regulator on it. Read off the diagram: a line carrying
  sigma*l + delta is brought to a bare +-l by the shift Delta = -sigma*delta. Here we keep the
  loop momentum's tag fixed, so only parity-even deltas are admissible.*)

canonicalShiftCandidates[objects_, l_] :=
    Module[{cands = {0}, c, delta},
        Do[
            c = Coefficient[m, l];
            If[c === 1 || c === -1,
                delta = ExpandAll[m - c * l];
                If[FreeQ[delta, _loopMomentum] && momentumParity[delta] === 0,
                    AppendTo[cands, ExpandAll[-c * delta]]
                ]
            ]
            ,
            {m, lineMomenta[objects, $routingLineHeads]}
        ];
        DeleteDuplicates[cands]
    ];

FRoute::retag = "Internal error while canonicalising the routing: the regulated `1` line was expected to carry statistics parity `2`, but the incoming routing gives `3`. The routing handed to the canonicaliser was already inconsistent.";

canonicaliseRouting[setup_, retIn_, objectsIn_, extAtoms_] :=
    Module[{ret = retIn, objects = objectsIn, loopsOrig, loops, l, anchor, tag, newTag, shifted, cands, best, sub},
        (*Iterate over a snapshot. Each substitution below rewrites only its own atom l (the shift
          Delta is purely external), so the loop momenta we have not reached yet are untouched and
          the snapshot stays valid. `loops` is refreshed each round only to orient the tie-break key
          against the atoms currently in the term.*)
        loopsOrig = DeleteDuplicates @ Cases[objects, _loopMomentum, Infinity];
        Do[
            l = loopsOrig[[j]];
            loops = DeleteDuplicates @ Cases[objects, _loopMomentum, Infinity];
            tag = l[[2]];
            anchor = regulatorAnchor[objects, l];
            If[MissingQ[anchor],
                (*No d_t R on this loop -- a DSE, or the extra loop of a two-loop term. Nothing
                  anchors the tag, so hold it fixed and just flatten the lines.*)
                cands = Flatten[
                    Table[{sigma, delta}, {sigma, {1, -1}}, {delta, canonicalShiftCandidates[objects, l]}]
                    ,
                    1
                ];
                best = First @ SortBy[cands, routingCost[objects, l, #, extAtoms, loops]&];
                shifted = best[[1]] * l + best[[2]];
                ,
                (*The hard rule: put d_t R on the bare loop momentum, re-tagging l as we go so that
                  every line keeps its Matsubara statistics. The regulated line reads
                  sigma_R * l + delta_R, so the shift Delta = -sigma_R * delta_R makes it bare.*)
                Module[{fld = anchor[[1]], sigmaR = anchor[[2]], deltaR = anchor[[3]], delta},
                    delta = ExpandAll[-sigmaR * deltaR];
                    newTag = HasFermiStatistics[setup, fld];
                    (*The new tag must equal t XOR parity(delta_R) -- see the header. If it does
                      not, the routing we were handed was already statistics-inconsistent.*)
                    If[newTag =!= Xor[tag, momentumParity[deltaR] === 1],
                        Message[FRoute::retag, fld, Boole[newTag], Mod[Boole[tag] + momentumParity[deltaR], 2]];
                        Abort[]
                    ];
                    (*Both signs of the (re-tagged) loop momentum leave d_t R bare; pick one canonically.*)
                    best = First @ SortBy[
                        {{1, delta}, {-1, delta}}
                        ,
                        routingCost[objects, l, #, extAtoms, loops]&
                    ];
                    shifted = best[[1]] * loopMomentum[l[[1]], newTag] + delta;
                ];
            ];
            FunKitDebug[3, "  FRoute Canonical: relabelling ", l, " as ", shifted];
            sub = l -> shifted;
            objects = ExpandAll[objects /. sub];
            ret = ExpandAll[ret /. sub];
            ,
            {j, 1, Length[loopsOrig]}
        ];
        Return[{ret, objects}];
    ];

(*Canonical numbering of the loop momenta: replaces the discovery-order numbering, which
  inherits the order of the objects in the FTerm.*)

canonicalLoopKey[objects_, l_, extAtoms_] :=
    Module[{ms},
        ms = Select[lineMomenta[objects, $routingLineHeads], Not @ FreeQ[#, l]&];
        {-Length[ms], Total[externalWeight[#, extAtoms]& /@ ms]}
    ];

(*Post-condition: every leg's momentum must carry the Matsubara parity of its field's STATISTICS
  (Fermi -> odd, Bose -> even). Note this keys off HasFermiStatistics, not IsGrassmann: a ghost
  anticommutes but is periodic in imaginary time, so a ghost line must carry an even (bosonic)
  Matsubara frequency. Cheap (a Z2 dot product per leg) and run in every routing mode, so that a
  routing which sends a fermionic momentum through a bosonic line -- or vice versa -- fails loudly
  at derivation time instead of silently producing a wrong finite-T kernel.*)

routingStatisticsViolations[setup_, objects_] :=
    Module[{res = {}, fld, mom, req},
        Do[
            Do[
                fld = lm[[1]];
                mom = lm[[2]];
                If[fld =!= AnyField,
                    req = Boole @ HasFermiStatistics[setup, fld];
                    If[momentumParity[mom] =!= req,
                        AppendTo[res, {Head[obj], fld, mom, momentumParity[mom], req}]
                    ]
                ]
                ,
                {lm, legFieldMomenta[obj]}
            ]
            ,
            {obj, objects}
        ];
        Return[res];
    ];

FieldSetupIndices[setup_, field_] :=
    FieldSetupIndices[setup, field] =
        Module[{result},
            (*Only the field-definition keys carry index structures. "FermiStatistics" and
              "BoseStatistics" hold bare heads, so they must not be scanned here.*)
            result =
                SelectFirst[
                    Flatten @ Map[Lookup[setup["FieldSpace"], #, {}]&, {"Commuting", "Grassmann", "CommutingSource", "GrassmannSource"}]
                    ,
                    Head[#] === field&
                ];
            If[MissingQ[result],
                Message[FieldSetupIndices::notFound, field];
                Abort[]
            ];
            List @@ result
        ];

FRoute::undeterminedFields = "Cannot route indices in expressions with undetermined fields.";

FRoute::momentaFailed = "Cannot route momenta in the given expression. Final momentum conservation read `1`";

FRoute::conservationFail = "Momentum conservation could not be fulfilled. Error in `1`.
Full Expression:
    `2`";

FRoute::noObjectForIndex = "No indexed object could be found for the index `1`.";

FRoute::noLoopMomentum = "No loop momentum could be found for momentum routing at vertex `1`.";

FRoute::statistics = "The momentum routing violates Matsubara statistics: a `1` leg carrying the field `2` was routed with the momentum `3`, whose Matsubara parity is `4`, but the field's statistics requires parity `5` (Fermi -> 1, Bose -> 0). A routing that sends a fermionic momentum through a bosonic line (or vice versa) is wrong at finite temperature. If the field in question is a ghost, declare it in the setup's \"BoseStatistics\": ghosts anticommute but are periodic in imaginary time. Note also that FSetRoutingAlgorithm[\"Regulator\"] is known to produce such routings whenever the diagram has fermionic external legs.";

(*detect if we should route into a fermionic loopMomentum (True) or a bosonic one (False)*)

fermionicExtMomRouting[setup_, vertex_] :=
    Module[{momsum, factor},
        momsum = Total[vertex[[2, All, 1]]];
        (*for this, we can set all external momenta with fermions equal and all external momenta with bosons to 0*)
        momsum = momsum //. externalMomentum[p_, True] :> externalMomentum[1, True] //. externalMomentum[p_, False] :> 0;
        (*Extract the prefactor of externalMomentum[1, True] (or 0, if it does not appear)*)
        momsum = Flatten[momsum /. Plus[a_, b__] :> List[a, b]];
        factor =
            Cases[
                momsum
                ,
                Times[c___, externalMomentum[1, True]] | externalMomentum[1, True] :>
                    If[Length[{c}] > 0,
                        Times[c]
                        ,
                        1
                    ]
            ];
        factor =
            If[Length[factor] > 0,
                Total[factor]
                ,
                0
            ];
        Return[Mod[factor, 2] === 1];
    ];

(* The main routing function *)

(* Disconnected fast path: split the term into connected components, route
   each independently, then merge. Each component is routed in isolation so
   per-vertex momentum conservation holds locally; externals/loops in
   subsequent components are renumbered to avoid collisions. *)

(* AssertFSetup runs inside the condition, before FDisconnectedQ touches the setup: this is the
   first definition that consumes it, and an invalid setup reaching FDisconnectedQ produces a
   cascade of raw Lookup/Join errors before the FunKit message the user should be reading. *)

FRoute[setup_, expr_FTerm] /; (AssertFSetup[setup]; FDisconnectedQ[setup, expr]) :=
    Module[{components, mergedTerm = FTerm[], mergedExt = {}, mergedLoops = {}, extOffset = 0, loopOffset = 0, comp, routed, extNew, loopNew, nExt, nLoops, renameRules, k, routedTerm, newExtPairs, newLoopMoms},
        FunKitDebug[1, "FRoute: term is disconnected, splitting into connected components"];
        components = partitionFTermByConnectivity[setup, expr];
        FunKitDebug[2, "  FRoute: ", Length[components], " components"];
        Do[
            comp = components[[i]];
            routed = FRoute[setup, comp];
            extNew = routed["ExternalIndices"];
            loopNew = routed["LoopMomenta"];
            nExt = Length[extNew];
            nLoops = Length[loopNew];
            (* Rename only the independent base momenta: p1..p_(N-1) for externals
               (leg N is -(sum of others) and follows automatically through the
               substitution) and l1..l_M for loops (all loops are independent). *)
            renameRules =
                Join[
                    Table[
                        Symbol["p" <> ToString[k]] -> Symbol["p" <> ToString[extOffset + k]]
                        ,
                        {k, 1, nExt - 1}
                    ]
                    ,
                    (*Both families must be renumbered: a component's loop momentum is named l<k> or
                      lf<k> depending on the statistics of the field it flows through, and only the
                      bosonic family used to be offset here -- so a fermionic loop in a second
                      component kept its old number and collided with the first component's.*)
                    Table[
                        Symbol[$loopMomentumName <> ToString[k]] -> Symbol[$loopMomentumName <> ToString[loopOffset + k]]
                        ,
                        {k, 1, nLoops}
                    ]
                    ,
                    Table[
                        Symbol[$loopMomentumName <> "f" <> ToString[k]] -> Symbol[$loopMomentumName <> "f" <> ToString[loopOffset + k]]
                        ,
                        {k, 1, nLoops}
                    ]
                ];
            routedTerm = First @ routed["Expression"];
            mergedTerm = mergedTerm ** (routedTerm /. renameRules);
            newExtPairs = (extNew /. renameRules);
            mergedExt = Join[mergedExt, newExtPairs];
            newLoopMoms = loopNew /. renameRules;
            mergedLoops = Join[mergedLoops, newLoopMoms];
            extOffset += Max[nExt - 1, 0];
            loopOffset += nLoops;
            ,
            {i, 1, Length[components]}
        ];
        Return[<|"Expression" -> FEx[mergedTerm], "ExternalIndices" -> Sort @ mergedExt, "LoopMomenta" -> Sort @ mergedLoops|>]
    ];

FRoute[setup_, expr_FTerm] :=
    Module[{openIndices, closedIndices, objects, ret = ReduceFTerm[setup, ReduceIndices[setup, expr]], idx, indPos, assocField, subObj, subMom, subExtMom, indStruct, externalIndices, externalMomenta, kind, momRepl, mom, loopMomenta, rightMomenta, closedIndex, nextObj, tmp, flag, availMomenta, frozenMomenta = {}},
        AssertFSetup[setup];
        FunKitDebug[1, "FRoute: routing the sub-term ", expr];
        (*We first get all closed, open indices and all indexed objects. *)
        openIndices = Sort @ GetOpenSuperIndices[setup, ret];
        closedIndices = GetClosedSuperIndices[setup, ret];
        objects = FixedPoint[replFields[setup, #]&, ExtractObjectsWithIndex[setup, ret]];
        (*If there are any undetermined fields, we cannot route indices. *)
        If[MemberQ[objects[[All, 1]], AnyField, {1, 4}],
            Message[FRoute::undeterminedFields];
            Abort[]
        ];
        (*We need to reorder the objects list. In particular, (if possible) the next object should always share a closed index with the one before it.*)
        Do[
            subObj = objects[[idx]];
            (*find the closed index in the subObj*)
            closedIndex = Cases[getIndices[subObj], x_ /; MemberQ[closedIndices, x], Infinity];
            (*find the first object after with a shared closedIndex*)
            nextObj = Select[objects[[idx + 1 ;; ]], ContainsAny[makePosIdx /@ getIndices[#], closedIndex]&];
            If[Length[nextObj] === 0,
                Continue[]
            ];
            nextObj = FirstPosition[objects, nextObj[[1]]];
            (*Swap the object right after with nextObj*)
            If[Not @ MissingQ[nextObj] && nextObj[[1]] =!= idx + 1,
                nextObj = nextObj[[1]];
                FunKitDebug[2, "  FRoute: Swapping objects at positions ", idx + 1, " and ", nextObj];
                tmp = objects[[idx + 1]];
                objects[[idx + 1]] = objects[[nextObj]];
                objects[[nextObj]] = tmp;
            ]
            ,
            {idx, 1, Length[objects] - 1}
        ];
        (*Now, momenta. Collect all closed-index replacement rules in one pass, then apply at once. *)
        Module[{allClosedRules = {}},
            Do[
                subObj = Select[objects, MemberQ[#, closedIndices[[idx]], Infinity]&];
                If[Length[subObj] === 0,
                    Message[FRoute::noObjectForIndex, closedIndices[[idx]]];
                    Abort[]
                ];
                subObj = subObj[[1]];
                (*The indexed object we currently modify. There are always two and we simply grab the first. *)
                (*The position of the current index inside the subObj*)
                indPos = FirstPosition[getIndices[subObj], closedIndices[[idx]]][[1]];
                (*See what kind of field is associated with the index*)
                assocField = getField[subObj, indPos];
                (*Grab the index structure of this field from the setup and assign a new momentum variable*)
                indStruct =
                    Map[
                        If[MatchQ[#, _Symbol],
                            Unique[SymbolName[#]]
                            ,
                            #
                        ]&
                        ,
                        FieldSetupIndices[setup, assocField]
                        ,
                        {1, 3}
                    ];
                indStruct[[1]] = loopMomentum[indStruct[[1]], HasFermiStatistics[setup, assocField]];
                (* We want to keep the index sign in the momenta, but remove it from the group indices *)
                Module[{rules = {closedIndices[[idx]] -> indStruct}},
                    If[Length[indStruct] > 1,
                        rules = Join[rules, Thread[-(makePosIdx /@ indStruct[[2]]) -> (makePosIdx /@ indStruct[[2]])]];
                    ];
                    allClosedRules = Join[allClosedRules, rules];
                ];
                ,
                {idx, 1, Length[closedIndices]}
            ];
            (*Apply all closed-index rules at once — two passes for sign propagation in group indices*)
            ret = ret /. allClosedRules /. allClosedRules;
            objects = objects /. allClosedRules /. allClosedRules;
        ];
(* Treat the external superindices: assign each an open group structure and
   a new momentum p1, p2, ...  Enforce momentum conservation directly by
   choosing the last leg as p_N = -(p_1 + ... + p_(N-1)). *)
        (*Collect ALL open-index replacement rules, then apply at once.*)
        externalIndices = Table[{}, {idx, 1, Length[openIndices]}];
        Module[{allOpenRules = {}},
            Do[
                subObj = Select[objects, MemberQ[#, openIndices[[idx]], Infinity]&];
                If[Length[subObj] === 0,
                    Message[FRoute::noObjectForIndex, openIndices[[idx]]];
                    Abort[]
                ];
                subObj = subObj[[1]];
                indPos = FirstPosition[getIndices[subObj], openIndices[[idx]]][[1]];
                assocField = getField[subObj, indPos];
                indStruct =
                    Map[
                        If[MatchQ[#, _Symbol],
                            (* Strip Module-local "$nnn" suffix so that user-named field
                               indices (e.g. p, a from Module[{p,a},...] in a setup helper)
                               yield clean externals p1, a1 rather than p$120491. *)
                            Symbol[StringSplit[SymbolName[#], "$"][[1]] <> ToString[idx]]
                            ,
                            #
                        ]&
                        ,
                        FieldSetupIndices[setup, assocField]
                        ,
                        {1, 3}
                    ];
                If[idx === Length[openIndices],
                    indStruct[[1]] = -Total[Values[externalIndices][[ ;; idx - 1, 1]]]
                    ,
                    indStruct[[1]] = externalMomentum[indStruct[[1]], HasFermiStatistics[setup, assocField]];
                ];
                AppendTo[allOpenRules, (-openIndices[[idx]]) -> indStruct];
                AppendTo[allOpenRules, openIndices[[idx]] -> indStruct];
                externalIndices[[idx]] = openIndices[[idx]] -> indStruct;
                ,
                {idx, 1, Length[openIndices]}
            ];
            ret = ret /. allOpenRules;
            objects = objects /. allOpenRules;
        ];
        (*extract a list of all new external momenta*)
        externalMomenta = Values[externalIndices][[All, 1]];
        FunKitDebug[2, "  FRoute: Determined external momenta as ", externalMomenta];
        (*Now, we do the momentum routing. We iterate over all objects in subObj and fully resolve them.*)

        If[$routingAlgorithm === "Regulator",
            (*In the regulator routing algorithm, we never route through regulators. We resolve momentum conservation
              at every Rdot first, reducing each Rdot's two legs to a {l, -l} pure-loop pair, then mark the surviving
              loop momentum as frozen so the main conservation loop below never picks it as a solve-for variable. This
              prevents external momenta from leaking into the regulator via chained substitutions at other vertices.*)
            Module[{rdotPositions, ridx, rdotPos, rdotMoms, rdotMomRepl, freezeMom, otherMom},
                rdotPositions = Flatten @ Position[objects, x_ /; Head[x] === Rdot, {1}, Heads -> False];
                FunKitDebug[3, "  FRoute Regulator: found Rdot objects at positions ", rdotPositions];
                Do[
                    rdotPos = rdotPositions[[ridx]];
                    rdotMoms = getIndices[objects[[rdotPos]]][[All, 1]];
                    If[Length[rdotMoms] < 2,
                        Continue[]
                    ];
                    If[Total[rdotMoms] =!= 0,
                        otherMom = FirstCase[rdotMoms, loopMomentum[__, _], Missing[], Infinity];
                        If[!MissingQ[otherMom],
                            rdotMomRepl = solveLinearMomConservation[Total[rdotMoms], otherMom];
                            FunKitDebug[3, "    FRoute Regulator: resolving Rdot conservation as ", rdotMomRepl];
                            objects = objects /. rdotMomRepl;
                            ret = ret /. rdotMomRepl;
                        ];
                    ];
                    freezeMom = FirstCase[
                        getIndices[objects[[rdotPos]]][[All, 1]],
                        loopMomentum[__, _],
                        Missing[],
                        Infinity
                    ];
                    If[!MissingQ[freezeMom],
                        AppendTo[frozenMomenta, freezeMom];
                        FunKitDebug[3, "    FRoute Regulator: frozen loop momentum on Rdot is ", freezeMom];
                    ];
                    ,
                    {ridx, 1, Length[rdotPositions]}
                ];
                frozenMomenta = DeleteDuplicates[frozenMomenta];
            ];
        ];

        Do[
            subObj = objects[[idx]];
            subMom = getIndices[subObj][[All, 1]];
            (*See if the object has any external (sub-)momenta*)
            subExtMom = Select[subMom, (ContainsAny[externalMomenta, makePosIdx /@ Flatten[{# /. Plus[a_, b__] :> List[a, b]}]])&];
            FunKitDebug[3, "  FRoute: routing the subObj ", subObj];
            FunKitDebug[3, "    FRoute: subExtMom are ", subExtMom];
            (*********************************************************************************)
            (* CASE 0: If momentum conservation is already fulfilled, do nothing *)
            (*********************************************************************************)
            If[Total @ subMom === 0,
                FunKitDebug[3, "      FRoute: Have only external momenta"];
                If[Total @ getIndices[subObj][[All, 1]] =!= 0,
                    Message[FRoute::conservationFail, subObj, ret];
                    Abort[];
                ];
                Continue[]
            ];
            (*********************************************************************************)
            (*CASE 1: we have no external momentum anywhere in the legs of the subObject *)
            (*********************************************************************************)
            If[Length[subExtMom] === 0,
                (*If we have nothing to enforce, skip this object. This is the case for 1-Point functions*)
                availMomenta = Total[getIndices[subObj][[All, 1]]];
                availMomenta = makePosIdx /@ Flatten[{availMomenta //. {Plus[a_, b__] :> List[a, b], Times[a_loopMomentum, b__] :> List[a, b]}}];
                availMomenta = Select[availMomenta, MatchQ[#, loopMomentum[__, _]]&];
                FunKitDebug[5, "        available loop momenta =  ", availMomenta];
                If[Length[availMomenta] < 2,
                    Continue[]
                ];
                FunKitDebug[3, "      FRoute: No external momenta"];
                (*Grab the first loopMomentum that is fermionic; in Regulator mode skip frozen ones so external momenta don't leak via substitution*)
                mom = Select[availMomenta, MatchQ[#, loopMomentum[_, True]] && Not @ MemberQ[frozenMomenta, #]&];
                If[Length[mom] === 0,
                    (*Otherwise, we have no (purely) fermionic loop momenta, so just grab the first bosonic one*)
                    mom = Select[availMomenta, MatchQ[#, loopMomentum[_, False]] && Not @ MemberQ[frozenMomenta, #]&];
                ];
                If[Length[mom] === 0,
                    Message[FRoute::noLoopMomentum, subObj];
                    Abort[]
                ];
                mom = mom[[1]];
                (*Now create the replacement rule*)
                momRepl = solveLinearMomConservation[Total[getIndices[subObj][[All, 1]]], mom];
                objects = objects /. momRepl;
                ret = ret /. momRepl;
                FunKitDebug[3, "      FRoute: routing a momentum as ", momRepl];
                Continue[];
            ];
            (*********************************************************************************)
            (*Case 2: We have both internal and external momenta *)
            (*********************************************************************************)
            If[Length[subExtMom] <= Length[subMom],
                FunKitDebug[3, "      FRoute: Have both internal and external momenta"];
                flag = fermionicExtMomRouting[setup, subObj];
                FunKitDebug[3, "        Are we routing a fermionic external momentum? ", flag];
                (*If we have a fermionic external momentum, we need to route it correctly. In that case, try to find a fermionic loopMomentum*)
                availMomenta = Total[getIndices[subObj][[All, 1]]];
                availMomenta = makePosIdx /@ Flatten[{availMomenta //. {Plus[a_, b__] :> List[a, b], Times[a_loopMomentum, b__] :> List[a, b]}}];
                availMomenta = Select[availMomenta, MatchQ[#, loopMomentum[__, _]]&];
                FunKitDebug[5, "        available loop momenta =  ", availMomenta];
                (*Grab one of the momenta which is a loopMomentum; in Regulator mode skip frozen ones so external momenta don't leak via substitution*)
                If[flag,
                    mom = Select[availMomenta, MatchQ[#, loopMomentum[_, True]] && Not @ MemberQ[frozenMomenta, #]&];
                ];
                If[Not @ flag || Length[mom] === 0,
                    mom = Select[availMomenta, MatchQ[#, loopMomentum[_, False]] && Not @ MemberQ[frozenMomenta, #]&];
                    If[Length[mom] === 0,
                        mom = Select[availMomenta, MatchQ[#, loopMomentum[_, True]] && Not @ MemberQ[frozenMomenta, #]&];
                    ];
                ];
                If[Length[mom] === 0,
                    Message[FRoute::noLoopMomentum, subObj];
                    Abort[]
                ];
                mom = mom[[1]];
                (*now build the replacement rule*)
                momRepl = solveLinearMomConservation[Total[getIndices[subObj][[All, 1]]], mom];
                (*if the given momentum is NOT a fermionic one, we will need to replace all the momenta on the right-hand-side with NOT fermionic ones*)
                If[Not @ mom[[2]],
                    rightMomenta = Cases[momRepl[[2]], loopMomentum[__, True], Infinity] // DeleteDuplicates;
                    rightMomenta = Map[# -> Head[#][#[[1]], False]&, rightMomenta];
                    subObj = subObj /. rightMomenta;
                    objects = objects /. rightMomenta;
                    ret = ret /. rightMomenta;
                    (*Keep frozenMomenta in sync — Regulator-mode prologue may have stored
                      a fermionic-tagged frozen momentum that just got re-tagged to bosonic here.*)
                    frozenMomenta = frozenMomenta /. rightMomenta;
                    momRepl = solveLinearMomConservation[Total[getIndices[subObj][[All, 1]]], mom];
                ];
                objects = objects /. momRepl;
                ret = ret /. momRepl;
                FunKitDebug[3, "      FRoute: routing a momentum as ", momRepl];
                Continue[];
            ];
            ,
            {idx, 1, Length[objects]}
        ];
        (*The routing is now valid, but not yet unique: the loop momenta may still be
          relabelled as l -> +-l + Delta. Fix that freedom canonically -- by a physical
          criterion rather than by whichever momentum the solver above happened to pick --
          so that the routing is a function of the diagram alone.*)
        Module[{extAtoms = Select[externalMomenta, Head[#] === externalMomentum&]},
            If[$routingAlgorithm === "Canonical",
                {ret, objects} = canonicaliseRouting[setup, ret, objects, extAtoms];
            ];
            (*Sanity check to see that we did not make an error*)
            Do[
                subObj = objects[[idx]];
                (*Skip again Fields and such*)
                If[Length[getIndices[subObj][[All, 1]]] < 2,
                    Continue[]
                ];
                (*Check the conservation of momentum at all vertices*)
                If[Total[getIndices[subObj][[All, 1]]] =!= 0,
                    Message[FRoute::momentaFailed, Total[getIndices[subObj][[All, 1]]]];
                    Abort[]
                ];
                ,
                {idx, 1, Length[objects]}
            ];
            (*Every line must carry the Matsubara parity of its field. Checked in all modes.*)
            Module[{violations = routingStatisticsViolations[setup, objects]},
                If[Length[violations] > 0,
                    Message[FRoute::statistics, Sequence @@ violations[[1]]];
                    Abort[]
                ]
            ];
            (*replace the loopMomenta[...] by l1, l2, ...*)
            loopMomenta = Cases[objects[[All, 2, 1]], loopMomentum[__], Infinity] // DeleteDuplicates;
            (*...numbering them canonically too, rather than by order of discovery*)
            If[$routingAlgorithm === "Canonical",
                loopMomenta = SortBy[loopMomenta, canonicalLoopKey[objects, #, extAtoms]&];
            ];
        ];
        kind =
            If[#[[2]],
                "f"
                ,
                ""
            ]&;
        (*Insert loop momenta*)
        ret = ret /. Thread[loopMomenta -> Table[Symbol[$loopMomentumName <> kind[loopMomenta[[idx]]] <> ToString[idx]], {idx, 1, Length[loopMomenta]}]];
        (*Insert external momenta*)
        ret = ret //. externalMomentum[p_, _] :> p;
        externalIndices = externalIndices //. externalMomentum[p_, _] :> p;
        loopMomenta = loopMomenta /. Thread[loopMomenta -> Table[Symbol[$loopMomentumName <> ToString[idx]], {idx, 1, Length[loopMomenta]}]];
        Return[<|"Expression" -> FEx[ret], "ExternalIndices" -> Sort @ externalIndices, "LoopMomenta" -> Sort @ loopMomenta|>];
    ];

makeMomentaAlternatives[mom_] :=
    Module[{idx},
        idx = StringSplit[SymbolName[mom], x:NumberString :> ToExpression @ x][[2]];
        Return[Alternatives[Symbol[$loopMomentumName <> ToString[idx]], Symbol[$loopMomentumName <> "f" <> ToString[idx]]]]
    ];

FRoute[setup_, expr_FEx] :=
    Module[{results, ret, idx, subidx},
        results = FEx @@ DropFExAnnotations[expr];
        results = FRoute[setup, #]& /@ (List @@ results);
        results = GatherBy[results, Length[#["LoopMomenta"]]&];
        results = Map[<|"Expression" -> FEx @@ #[[All, Key["Expression"]]], "ExternalIndices" -> #[[1, Key["ExternalIndices"]]], "LoopMomenta" -> makeMomentaAlternatives /@ #[[1, Key["LoopMomenta"]]]|>&, results];
        results = Association @@ Map[ToString[Length[#["LoopMomenta"]]] ~~ "-Loop" -> #&, results];
        Return[results];
    ];

(* Check if an expression is a loop association, i.e. a distinct routed n-loop term (used also in DiANE) *)

isLoopAssociation[expr_] :=
    Module[{},
        If[Head[expr] =!= Association,
            Return[False]
        ];
        If[FreeQ[Keys[expr], "Expression"],
            Return[False]
        ];
        If[FreeQ[Keys[expr], "ExternalIndices"],
            Return[False]
        ];
        If[FreeQ[Keys[expr], "LoopMomenta"],
            Return[False]
        ];
        Return[True];
    ];

(* Check if an expression is a association of loop associations, i.e. several distinct n-loop terms (used also in DiANE) *)

isRoutedAssociation[expr_] :=
    Module[{},
        If[Head[expr] =!= Association,
            Return[False]
        ];
        Return @ AllTrue[expr, isLoopAssociation]
    ];

(* Functions to try and undo routing. Note that this can fail, e.g. when we have multiple fields with no index structure but identical momenta. *)

FUnroute[setup_, assoc_Association] /; isLoopAssociation[assoc] :=
    Module[{},
        AssertFSetup[setup];
        Return @ FUnroute[setup, assoc["Expression"] /. Map[#[[2]] -> #[[1]]&, assoc["ExternalIndices"]]];
    ];

FUnroute[setup_, assoc_Association] /; isRoutedAssociation[assoc] :=
    FEx @@ (FUnroute[setup, #]& /@ (List @@ assoc));

FUnroute[setup_, term_FEx] :=
    Module[{},
        AssertFSetup[setup];
        FUnroute[setup, #]& /@ term
    ];

FUnroute[setup_, term_FTerm] :=
    Module[{fw, bw},
        AssertFSetup[setup];
        {fw, bw} = GetSuperIndexTermTransformations[setup, term];
        Return[term // fw];
    ];

(* Catch-all definitions *)

FRoute[___] :=
    (
        Message[FunKit::invalidArguments, FRoute];
        Abort[]
    );

FUnroute[___] :=
    (
        Message[FunKit::invalidArguments, FUnroute];
        Abort[]
    );
