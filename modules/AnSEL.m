(* ::Package:: *)

(* ::Title:: *)

(*AnSEL - Analysis and Simplification of Equations with Loops*)

(* ::Section:: *)

(*Exports*)

(* ::Input::Initialization:: *)

FSetLoopMomentumName::usage = "FSetLoopMomentumName[name]
Sets the base name for loop momentum variables used in functional calculations.
The name should be a string (e.g., \"l\", \"k\", \"q\") that will be used to generate loop momenta l1, l2, l3, etc.
Also creates fermionic variants (lf1, lf2, ...) and bosonic variants for different loop types.
Default setting is \"l\".";

FMakeSymmetryList::usage = "FMakeSymmetryList[s1, s2, ...]
Assembles FSymmetry objects into a symmetry list, adding the identity if it is missing.
This is the recommended form: symmetries must normally be stated by hand, because whether a
given symmetry may be used depends on the contraction you will apply, which FunKit cannot know.

FMakeSymmetryList[setup, {f1, f2, ...}]
Generates the FULL permutation group of the given fields. Use with care: that group is a
property of the correlation function, not of an individual diagram, so reducing with it yields
an expression equal to the original only after symmetrisation. It is correct only if your
contraction is covariant under every element. See SYMMETRY-REDUCTION-DESIGN.md.
Returns a list of symmetry rules that can be used in FSimplify to identify identical diagrams.";

FSymmetry::usage = "FSymmetry[Symmetric, {i1,i2}, {i3,i4}]
Describes a single symmetry of a correlation function: the exchange i1<->i2 performed together
with i3<->i4, carrying the factor +1. Each argument after the head is a cycle of superindices;
all cycles are applied simultaneously and must be disjoint.
The head fixes the factor: Symmetric -> +1, Antisymmetric -> -1, or give a number directly.
  FSymmetry[Antisymmetric, {i1,i2}]     the exchange i1<->i2 with factor -1
  FSymmetry[Symmetric, {i1,i2,i3}]      the 3-cycle i1->i2->i3->i1 with factor +1
Pass the assembled list via FMakeSymmetryList to FTakeDerivatives[..., \"Symmetries\" -> syms];
it is carried on the resulting FEx and picked up by FTruncate and FSimplify.";

FSymmetrise::usage = "FSymmetrise[setup, expr, syms]
Applies (1/|G|) sum_sigma f_sigma sigma(.) to an FEx, i.e. explicitly symmetrises it over the
given symmetry list. FSymmetrise[setup, expr] uses the symmetry list carried by expr.
An expression that was simplified with symmetries equals the original only after this
operation, so this is the way to repair one that must be contracted with something that does
not share those symmetries.";

FCheckSymmetry::usage = "FCheckSymmetry[setup, expr, syms]
Returns True if expr already has the given symmetries, i.e. if FSymmetrise leaves it unchanged.
Reducing with a symmetry that the expression does not actually possess silently returns a
different object, so this is the precondition to check before passing a hand-made symmetry list
to FSimplify -- in particular for hand-built expressions or subsets of terms, which generally
carry only part of an orbit.";

FDisconnectedQ::usage = "FDisconnectedQ[setup, expr]
Checks whether a functional expression contains disconnected diagrams.
For an FTerm, returns True if the indexed objects partition into two or more groups
with no shared closed superindex between groups.
For an FEx, returns True if any constituent FTerm is disconnected.
Uses BFS on the index-connectivity graph for fast evaluation.";

FRoute::usage = "FRoute[setup, expr]
Routes indices and momenta in functional expressions, organizing terms by loop order.
For FTerm expressions, returns an Association with keys \"Expression\", \"ExternalIndices\", and \"LoopMomenta\".
For FEx expressions, returns an Association with keys like \"0-Loop\", \"1-Loop\", \"2-Loop\", etc.
Automatically enforces momentum conservation and assigns unique loop momentum variables.
Essential for organizing diagrammatic calculations by perturbative order.";

FUnroute::usage = "FUnroute[setup, expr]
Reverses the index and momentum routing performed by FRoute.
Converts routed expressions (with explicit momenta and indices) back to superindex notation.
Can handle both individual loop-order associations and complete routed expressions.
Used when you need to go back from explicit momentum space to abstract superindex form.";

FSimplify::usage = "FSimplify[setup, expr]
Simplifies functional expressions by identifying and combining terms.
Uses diagram comparison to detect terms that differ only by index relabeling or prefactor.
FSimplify[setup, expr, \"Symmetries\" -> symmetries] allows specifying symmetries to enhance simplification.
Significantly reduces the number of terms in complex functional calculations.
Essential for making large diagrammatic expressions manageable.";

FSetRoutingAlgorithm::usage = "FSetRoutingAlgorithm[algorithm]
Sets the algorithm used for routing indices and momenta in FRoute.
Options for algorithm include:
- \"Canonical\" (the default): as \"Default\", but the leftover relabelling freedom of the loop
  momenta (l -> +-l + Delta, a change of integration variable) is then fixed by a physical criterion
  instead of by whichever momentum the solver happened to eliminate first:

      THE LOOP MOMENTUM IS THE MOMENTUM FLOWING THROUGH THE d_t R INSERTION, AND ITS STATISTICS IS
      THAT OF THE REGULATED FIELD.

  So d_t R always carries a bare loop momentum -- l_i if the regulated field has Bose statistics,
  lf_i if it has Fermi statistics. That keeps the d_t R shell centred on the radial integration
  variable, which is what makes the resulting kernel cheap to integrate; a regulator displaced by an
  external momentum p with |p| >> k turns it into a thin off-centre shell. Shifting by a fermionic
  external flips the loop momentum's statistics, and the loop momentum is re-tagged accordingly, so
  every line keeps the correct Matsubara character. The routing is thus a function of the diagram
  alone -- independent of leg order, object order and the evaluation backend.
  Note that statistics here means Fermi/Bose, NOT Grassmann parity: ghosts anticommute but are
  periodic in imaginary time. Declare that with \"BoseStatistics\" in the setup's field space.
- \"Default\": Routes momenta such, that fermionic momenta are routed through fermionic lines.
  Correct, but the routing it picks depends on incidental orderings of its input.
- \"Regulator\": Never routes momenta through regulators. UNSAFE at finite temperature: it freezes
  the regulator's loop momentum without regard to its statistics, so it can route a fermionic
  momentum through a bosonic line whenever the diagram has fermionic external legs. FRoute will
  abort with FRoute::statistics if that happens.";


loopMomentum::usage = "loopMomentum[momentum, isFermi]
Internal representation for loop momentum variables during the routing process.
The first argument is the momentum symbol, the second indicates whether the momentum is a fermionic
Matsubara frequency (True) or a bosonic one (False). This is the field's Fermi/Bose STATISTICS, which
is not the same as its Grassmann parity: a ghost anticommutes but is periodic in imaginary time, so it
is Grassmann with Bose statistics. Declare that via the field space's \"BoseStatistics\"; see
HasFermiStatistics.
This is automatically generated by FRoute and should not be used directly by users.
Gets converted to standard momentum notation (l1, l2, lf1, lf2, etc.) at the end of routing.";

externalMomentum::usage = "externalMomentum[momentum, isFermi]
Internal representation for external momentum variables during the routing process.
The first argument is the momentum symbol, the second indicates whether the momentum is a fermionic
Matsubara frequency (True) or a bosonic one (False) -- the field's statistics, not its Grassmann parity.
This is automatically generated by FRoute and should not be used directly by users.
Gets converted to standard momentum notation at the end of routing.";

(* ::Section:: *)

(*Begin Private*)

(* ::Input::Initialization:: *)

Begin["`Private`"];

ModuleLoaded::dependency = "The module `1` requires `2`, which has not been loaded.";

If[ModuleLoaded[FunKit] =!= True,
    Message[ModuleLoaded::dependency, "AnSEL", "FunKit"];
    Abort[];
];

If[ModuleLoaded[FEDeriK] =!= True,
    Message[ModuleLoaded::dependency, "AnSEL", "FEDeriK"];
    Abort[];
];

ModuleLoaded[AnSEL] = True;

(* ::Section:: *)

(*Loading Components*)

(* ::Input::Initialization:: *)

(* Global setup *)

Get[$FunKitDirectory <> "modules/AnSEL/Global.m"];

(* Disconnected diagram check + connectivity partitioning *)

Get[$FunKitDirectory <> "modules/AnSEL/Disconnected.m"];

(* Routing *)

Get[$FunKitDirectory <> "modules/AnSEL/Routing.m"];

(* Simplification *)

Get[$FunKitDirectory <> "modules/AnSEL/Simplify.m"];

(* ::Section:: *)

(*End Private*)

(* ::Input::Initialization:: *)

End[];
