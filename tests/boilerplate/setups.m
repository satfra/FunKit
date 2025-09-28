(* ::Package:: *)

GetQMeSWetterichSetupScalar[]:=Module[
  {i,j,p,fields,eq,trunc},
  eq={
    "Prefactor"->{1/2},
    <|"type"->"Regulatordot", "indices"->{i,j}|>,
    <|"type"->"Propagator", "indices"->{i,j}|>
  };
  fields = <|"bosonic"-> {Phi[p],Phi[p]},
              "fermionic"->{}|>;
  trunc = {{Phi},{Phi,Phi},{Phi,Phi,Phi},{Phi,Phi,Phi,Phi}};
  Return[<|"MasterEquation"->eq,
          "FieldSpace"->fields,
          "Truncation"->trunc|>];
];


GetFunKitSetupScalar[]:=Module[
  {p,fields,eq,trunc},
  fields=<|"Commuting"->{Phi[p]},"Grassmann"->{}|>;
  trunc=<|
Rdot->{{Phi,Phi}},
Propagator->{{Phi,Phi}},
GammaN->{{Phi},{Phi,Phi},{Phi,Phi,Phi},{Phi,Phi,Phi,Phi}}
|>;
  Return[<|
"FieldSpace"->fields,
"Truncation"->trunc
|>];
]
