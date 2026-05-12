Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraRevolution wrapper ===================== *)

InfraRevolution[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRevolution[ _List ] ] ] :=
  InfraRevolution[ Flatten[ reps /. InfraRevolution[ xs_List ] :> xs, 1 ] ]


(* ===================== FindRevolution ===================== *)

(* For each axis vertex a, take its Voronoi slab in the axis ({v : a is closest
   axis vertex to v}) intersected with the closed ball of radius profile[a]
   (Solid) or the sphere of that radius (Surface).  Union over axis vertices. *)

Options[ FindRevolution ] = { "Form" -> "Surface", Method -> "Metric" };

FindRevolution[ graph_Graph, axis_, profile_, opts : OptionsPattern[ ] ] :=
  With[ { axisList = toVertexSet @ axis,
          cmp = If[ OptionValue[ "Form" ] === "Solid", LessEqual, Equal ] },
    With[ { radii = profileRadii[ profile, axisList ] },
      InfraRevolution[ { Sort[ Union @@ ( ( axisPoint |->
        Select[ VertexList @ NeighborhoodGraph[ graph, axisPoint, radii[ axisPoint ] ],
          v |-> Min[ GraphDistance[ graph, v, # ] & /@ axisList ] === GraphDistance[ graph, v, axisPoint ]
                  && cmp[ GraphDistance[ graph, v, axisPoint ], radii[ axisPoint ] ] ]
      ) /@ axisList ) ] } ]
    ]
  ]


profileRadii[ r_?NumericQ, axis_ ]      := AssociationThread[ axis, ConstantArray[ r, Length @ axis ] ]
profileRadii[ prof_List, axis_ ]        := AssociationThread[ axis, prof ]
profileRadii[ prof_Association, axis_ ] := prof
profileRadii[ prof_, axis_ ]            := AssociationThread[ axis, prof /@ Range @ Length @ axis ]


(* ===================== FindCylinder ===================== *)

Options[ FindCylinder ] = Options[ FindRevolution ];

FindCylinder[ graph_Graph, axis_, radius_, opts : OptionsPattern[ ] ] :=
  FindRevolution[ graph, axis, radius, opts ]


(* ===================== FindCone ===================== *)

Options[ FindCone ] = Join[ Options[ FindRevolution ], { "Apex" -> First } ];

FindCone[ graph_Graph, axis_, slope_, opts : OptionsPattern[ ] ] :=
  With[ { axisList = toVertexSet @ axis },
    FindRevolution[ graph, axisList,
      slope * If[ OptionValue[ "Apex" ] === Last,
        Range[ Length @ axisList - 1, 0, -1 ],
        Range[ 0, Length @ axisList - 1 ] ],
      FilterRules[ { opts }, Options[ FindRevolution ] ] ]
  ]


(* ===================== RevolutionQ ===================== *)

RevolutionQ[ graph_Graph, vs_List, axis_, profile_, opts : OptionsPattern[ FindRevolution ] ] :=
  Sort @ vs === FindRevolution[ graph, axis, profile, opts ][[ 1, 1 ]]
