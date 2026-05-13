Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindRevolution ===================== *)

(* Multi-axis rotational object.  Each axis path is extended by a SET of
   valid one-step continuations on each side -- vertices adjacent to the
   endpoint that satisfy the triangle equality d(v, path[[k]]) = k (left)
   or n - k + 1 (right) for every k along the axis (i.e., v, axis, axis[end]
   is a geodesic of length n + 1).  Method dispatches:
     "Voronoi" -- closest extended position must be in the original range.
     "PerpendicularBisector" -- d(u, v_{i-1}) == d(u, v_{i+1}) at original i. *)

Options[ FindRevolution ] = { "Form" -> "Solid", Method -> "Voronoi" };

FindRevolution[ graph_Graph, axis_, profile_, opts : OptionsPattern[ ] ] :=
  With[ { axisPaths = parseAxes @ axis,
          cmp = If[ OptionValue[ "Form" ] === "Surface", Equal, LessEqual ],
          method = OptionValue[ Method ] },
    With[ { n = Length @ First @ axisPaths,
            ext = extendAxisByOne[ graph, parseAxes @ axis ] },
      With[ { radii = profileRadii[ profile, n ],
              positions = First @ ext,
              origRange = Last @ ext },
        InfraObject[ Sort[ Union @@ MapThread[
          Function[ { posVerts, r, i },
            Select[ VertexList @ NeighborhoodGraph[ graph, posVerts, r ],
              v |-> With[ { dists = Min[ GraphDistance[ graph, v, # ] & /@ # ] & /@ positions },
                cmp[ dists[[ i ]], r ] && Switch[ method,
                  "Voronoi",                dists[[ i ]] === Min @ dists,
                  "PerpendicularBisector",  bisectorPasses[ dists, i, Length @ positions ] ] ] ] ],
          { positions[[ origRange ]], radii, origRange } ] ] ]
      ]
    ]
  ]


parseAxes[ InfraSegment[ paths_List ] ] := paths
parseAxes[ paths : { _List, ___List } ] := paths
parseAxes[ path_List ]                  := { path }


profileRadii[ r_?NumericQ, n_Integer ] := ConstantArray[ Round @ r, n ]
profileRadii[ prof_List, n_Integer ]   := Round /@ prof
profileRadii[ prof_, n_Integer ]       := Round /@ ( prof /@ Range[ n ] )


(* Direct +1 extension: for each axis path, collect all vertices adjacent
   to the endpoint that extend the axis as a geodesic.  Returns
   { positions, origRange } where positions is a list of vertex SETS
   (one per along-axis position, plus a left/right extension set if any)
   and origRange picks the indices corresponding to the original axis. *)

extendAxisByOne[ graph_Graph, axisPaths : { { _ }, ___ } ] :=
  { DeleteDuplicates /@ Transpose @ axisPaths, Range @ Length @ First @ axisPaths }

extendAxisByOne[ graph_Graph, axisPaths_List ] :=
  With[ { n = Length @ First @ axisPaths,
          origPositions = DeleteDuplicates /@ Transpose @ axisPaths },
    With[ { leftExt  = Union @@ ( oneStepExtensionLeft [ graph, # ] & /@ axisPaths ),
            rightExt = Union @@ ( oneStepExtensionRight[ graph, # ] & /@ axisPaths ) },
      Which[
        leftExt === { } && rightExt === { },  { origPositions, Range @ n },
        leftExt === { },                       { Append[ origPositions, rightExt ], Range @ n },
        rightExt === { },                      { Prepend[ origPositions, leftExt  ], Range[ 2, n + 1 ] },
        True,                                  { Join[ { leftExt }, origPositions, { rightExt } ], Range[ 2, n + 1 ] } ]
    ]
  ]


oneStepExtensionLeft[ graph_Graph, path_List ] :=
  With[ { n = Length @ path },
    Select[ AdjacencyList[ graph, First @ path ],
      v |-> ! MemberQ[ path, v ] &&
            AllTrue[ Range @ n, GraphDistance[ graph, v, path[[ # ]] ] === # & ] ] ]


oneStepExtensionRight[ graph_Graph, path_List ] :=
  With[ { n = Length @ path },
    Select[ AdjacencyList[ graph, Last @ path ],
      v |-> ! MemberQ[ path, v ] &&
            AllTrue[ Range @ n, GraphDistance[ graph, v, path[[ # ]] ] === n - # + 1 & ] ] ]


bisectorPasses[ dists_, i_, totalPos_ ] :=
  Which[
    i == 1,         dists[[ i ]] === Min @ dists,
    i == totalPos,  dists[[ i ]] === Min @ dists,
    True,           dists[[ i - 1 ]] === dists[[ i + 1 ]] ]


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
  Sort @ vs === FindRevolution[ graph, axis, profile, opts ][[ 1 ]]
