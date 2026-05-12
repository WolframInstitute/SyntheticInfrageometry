Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraRevolution wrapper ===================== *)

InfraRevolution[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRevolution[ _List ] ] ] :=
  InfraRevolution[ Flatten[ reps /. InfraRevolution[ xs_List ] :> xs, 1 ] ]


(* ===================== FindRevolution ===================== *)

(* Multi-axis rotational object.  At position i along the axis, the position
   vertex set is the union of axes[[k, i]] over all axes k.  Profile is
   indexed by position (1..L+1).  Membership: v is in iff for some position
   i with d(v, positions[[i]]) minimal across positions, the form test
   holds on (that distance, profile[i]).                                  *)

Options[ FindRevolution ] = { "Form" -> "Surface", Method -> "Metric" };

FindRevolution[ graph_Graph, axis_, profile_, opts : OptionsPattern[ ] ] :=
  With[ { cmp = If[ OptionValue[ "Form" ] === "Solid", LessEqual, Equal ],
          positions = DeleteDuplicates /@ Transpose @ parseAxes @ axis },
    With[ { radii = profileRadii[ profile, Length @ positions ] },
      InfraRevolution[ { Sort[ Union @@ MapThread[
        Function[ { posVerts, r, i },
          Select[ VertexList @ NeighborhoodGraph[ graph, posVerts, r ],
            v |-> With[ { dists = Min[ GraphDistance[ graph, v, # ] & /@ # ] & /@ positions },
              dists[[ i ]] === Min @ dists && cmp[ dists[[ i ]], r ] ] ] ],
        { positions, radii, Range @ Length @ positions } ] ] } ]
    ]
  ]


parseAxes[ InfraSegment[ paths_List ] ] := paths
parseAxes[ paths : { _List, ___List } ] := paths
parseAxes[ path_List ]                  := { path }


profileRadii[ r_?NumericQ, n_Integer ] := ConstantArray[ r, n ]
profileRadii[ prof_List, n_Integer ]   := prof
profileRadii[ prof_, n_Integer ]       := prof /@ Range[ n ]


(* ===================== FindCylinder ===================== *)

Options[ FindCylinder ] = Options[ FindRevolution ];

FindCylinder[ graph_Graph, axis_, radius_, opts : OptionsPattern[ ] ] :=
  FindRevolution[ graph, axis, radius, opts ]


(* ===================== FindCone ===================== *)

Options[ FindCone ] = Join[ Options[ FindRevolution ], { "Apex" -> First } ];

FindCone[ graph_Graph, axis_, slope_, opts : OptionsPattern[ ] ] :=
  With[ { n = Length @ First @ parseAxes @ axis,
          apex = OptionValue[ "Apex" ] },
    FindRevolution[ graph, axis,
      slope * If[ apex === Last, Range[ n - 1, 0, -1 ], Range[ 0, n - 1 ] ],
      FilterRules[ { opts }, Options[ FindRevolution ] ] ]
  ]


(* ===================== RevolutionQ ===================== *)

RevolutionQ[ graph_Graph, vs_List, axis_, profile_, opts : OptionsPattern[ FindRevolution ] ] :=
  Sort @ vs === FindRevolution[ graph, axis, profile, opts ][[ 1, 1 ]]
