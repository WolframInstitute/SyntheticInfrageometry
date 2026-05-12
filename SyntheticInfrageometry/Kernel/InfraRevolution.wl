Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findRevolutionCore]


(* ===================== InfraRevolution wrapper ===================== *)

InfraRevolution[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRevolution[ _List ] ] ] :=
  InfraRevolution[ Flatten[ reps /. InfraRevolution[ xs_List ] :> xs, 1 ] ]


(* ===================== FindRevolution ===================== *)

(* Rotational object around axis [a_1, ..., a_{L+1}] with radius profile
   [r_1, ..., r_{L+1}].  Solid: vertex v is in iff for some i with d(v, a_i)
   minimal over j, d(v, a_i) <= r_i.  Surface: same with == in place of <=. *)

Options[ FindRevolution ] = { "Form" -> "Surface", Method -> "Metric" };

FindRevolution[ graph_Graph, axis_, profile_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraRevolution, count,
    findRevolutionCore[ graph, ##, opts ] &, axis, profile ]


findRevolutionCore[ graph_Graph, axis_, profile_,
    opts : OptionsPattern[ FindRevolution ] ] :=
  With[ { axisList = toVertexSet @ axis,
          cmp = If[ OptionValue[ FindRevolution, { opts }, "Form" ] === "Solid", LessEqual, Equal ],
          dmat = GraphDistanceMatrix @ graph,
          vIdx = AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ] },
    With[ { radii = resolveProfile[ axisList, profile ],
            axisIdx = vIdx /@ axisList },
      { Sort @ DeleteDuplicates @ Join[
          Pick[ axisList, cmp[ 0, # ] & /@ radii ],
          Select[ Complement[ VertexList @ graph, axisList ],
            v |-> With[ { dists = dmat[[ vIdx[ v ], axisIdx ]] },
              AnyTrue[ Range @ Length @ axisList,
                i |-> dists[[ i ]] === Min @ dists && cmp[ dists[[ i ]], radii[[ i ]] ] ] ] ] ] }
    ]
  ]


resolveProfile[ axis_List, r_?NumericQ ]      := ConstantArray[ r, Length @ axis ]
resolveProfile[ axis_List, prof_Association ] := Lookup[ prof, axis ]
resolveProfile[ axis_List, prof_List ]        := prof
resolveProfile[ axis_List, prof_ ]            := prof /@ Range @ Length @ axis


(* ===================== FindCylinder ===================== *)

Options[ FindCylinder ] = Options[ FindRevolution ];

FindCylinder[ graph_Graph, axis_, radius_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] :=
  FindRevolution[ graph, axis, radius, count, opts ]


(* ===================== FindCone ===================== *)

(* Linear profile with apex at one end of the axis.  Multi-axis is unpacked
   here because the radii list depends on per-path length. *)

Options[ FindCone ] = Join[ Options[ FindRevolution ], { "Apex" -> First } ];

FindCone[ graph_Graph, axis_, slope_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] :=
  With[ { apex = OptionValue[ FindCone, { opts }, "Apex" ],
          revOpts = FilterRules[ { opts }, Options[ FindRevolution ] ],
          paths = Replace[ axis, { InfraSegment[ ps_List ] :> ps, l_List :> { l } } ] },
    With[ { capped = infraCap[ DeleteDuplicates @ Flatten[
        #[[ 1, 1 ]] & /@ FindRevolution[ graph, #,
          slope * If[ apex === Last, Range[ Length @ # - 1, 0, -1 ], Range[ 0, Length @ # - 1 ] ],
          All, Sequence @@ revOpts ] & /@ paths, 1 ], count ] },
      If[ capped === $Failed, $Failed, InfraRevolution[ { # } ] & /@ capped ]
    ]
  ]


(* ===================== RevolutionQ ===================== *)

RevolutionQ[ graph_Graph, vs_List, axis_, profile_,
    opts : OptionsPattern[ FindRevolution ] ] :=
  Sort @ vs === FindRevolution[ graph, axis, profile, 1, opts ][[ 1, 1, 1 ]]
