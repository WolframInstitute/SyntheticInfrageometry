Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraObject wrapper ===================== *)


InfraObject[ vs_List ][ "Volume" ] := { Length @ vs }

InfraObject[ vs_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraObject[ vs ] ]
InfraObject[ vs_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraObject[ vs ] ]
InfraObject[ vs_List ][ "Measure" ] := InfraMeasure[ InfraObject[ vs ] ]
InfraObject[ vs_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraObject[ vs ], Method -> "Probability" ]
(* ===================== FindInfraRevolution ===================== *)

(* each axis path is extended by the vertices v adjacent to its endpoint with d(v, path[[k]]) = k (left) or n - k + 1 (right) for every k, i.e. those prolonging the axis as a geodesic *)

Options[ FindInfraRevolution ] = { "Form" -> "Solid", Method -> "Voronoi" };

(* "Balls": the sublevel set { v : min_i (d(v, c_i) - r_i) <= 0 } of the varying-radius tube function; no geodesic extension, so cyclic and non-extendable axes work too *)

FindInfraRevolution[ graph_Graph, axis_, profile_, opts : OptionsPattern[ ] ] /;
  OptionValue[ FindInfraRevolution, { opts }, Method ] === "Balls" :=
  With[
    { positions = DeleteDuplicates /@ Transpose @ parseAxes @ axis,
      surface = OptionValue[ FindInfraRevolution, { opts }, "Form" ] === "Surface" },
    { radii = profileRadii[ profile, Length @ positions ] },
    { candidates = VertexList @ NeighborhoodGraph[ graph, Union @@ positions, Max @ radii ],
      slack = v |-> Min @ MapThread[
        { posVerts, r } |-> Min[ GraphDistance[ graph, v, # ] & /@ posVerts ] - r,
        { positions, radii } ] },
    (* constant radius: the r-neighborhood of the axis IS the union of balls, so the candidate set is already the answer *)
    InfraObject @ Sort @ If[ ! surface && Equal @@ radii,
      candidates,
      Select[ candidates, If[ surface, slack[ # ] == 0, slack[ # ] <= 0 ] & ] ]
  ]

FindInfraRevolution[ graph_Graph, axis_, profile_, opts : OptionsPattern[ ] ] :=
  With[ { axisPaths = parseAxes @ axis,
          cmp = If[ OptionValue[ "Form" ] === "Surface", Equal, LessEqual ],
          method = OptionValue[ Method ] },
    { n = Length @ First @ axisPaths,
      ext = extendAxisByOne[ graph, parseAxes @ axis ] },
    { radii = profileRadii[ profile, n ],
      positions = First @ ext,
      origRange = Last @ ext },
    InfraObject[ Sort[ Union @@ MapThread[
      { posVerts, r, i } |->
        Select[ VertexList @ NeighborhoodGraph[ graph, posVerts, r ],
          v |-> With[ { dists = Min[ GraphDistance[ graph, v, # ] & /@ # ] & /@ positions },
            cmp[ dists[[ i ]], r ] && Switch[ method,
              "Voronoi",                dists[[ i ]] === Min @ dists,
              "PerpendicularBisector",  bisectorPasses[ dists, i, Length @ positions ] ] ] ],
      { positions[[ origRange ]], radii, origRange } ] ] ]
  ]


parseAxes[ ( InfraSegment | InfraLine | InfraWalk )[ paths_List ] ] := paths
parseAxes[ paths : { _List, ___List } ] := paths
parseAxes[ path_List ]                  := { path }


profileRadii[ r_?NumericQ, n_Integer ] := ConstantArray[ Round @ r, n ]
profileRadii[ prof_List, n_Integer ]   := Round /@ prof
profileRadii[ prof_, n_Integer ]       := Round /@ ( prof /@ Range[ n ] )


(* for each axis path, the vertices adjacent to its endpoint that extend it as a geodesic; origRange picks the indices of the original axis *)

extendAxisByOne[ graph_Graph, axisPaths : { { _ }, ___ } ] :=
  { DeleteDuplicates /@ Transpose @ axisPaths, Range @ Length @ First @ axisPaths }

extendAxisByOne[ graph_Graph, axisPaths_List ] :=
  With[ { n = Length @ First @ axisPaths,
          origPositions = DeleteDuplicates /@ Transpose @ axisPaths },
    { leftExt  = Union @@ ( oneStepExtensionLeft [ graph, # ] & /@ axisPaths ),
      rightExt = Union @@ ( oneStepExtensionRight[ graph, # ] & /@ axisPaths ) },
    Which[
      leftExt === { } && rightExt === { },  { origPositions, Range @ n },
      leftExt === { },                       { Append[ origPositions, rightExt ], Range @ n },
      rightExt === { },                      { Prepend[ origPositions, leftExt  ], Range[ 2, n + 1 ] },
      True,                                  { Join[ { leftExt }, origPositions, { rightExt } ], Range[ 2, n + 1 ] } ]
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


(* ===================== FindInfraCylinder ===================== *)

Options[ FindInfraCylinder ] = Join[ FilterRules[ Options[ FindInfraRevolution ], Except[ Method ] ], { Method -> "Balls" } ];

FindInfraCylinder[ graph_Graph, axis_, radius_, opts : OptionsPattern[ ] ] :=
  FindInfraRevolution[ graph, axis, radius, Method -> OptionValue[ Method ],
    FilterRules[ { opts }, Except[ Method ] ] ]


(* ===================== FindInfraCone ===================== *)

Options[ FindInfraCone ] = Join[ Options[ FindInfraRevolution ], { "Apex" -> First } ];

FindInfraCone[ graph_Graph, axis_, slope_, opts : OptionsPattern[ ] ] :=
  With[ { n = Length @ First @ parseAxes @ axis,
          apex = OptionValue[ "Apex" ] },
    FindInfraRevolution[ graph, axis,
      slope * If[ apex === Last, Range[ n - 1, 0, -1 ], Range[ 0, n - 1 ] ],
      FilterRules[ { opts }, Options[ FindInfraRevolution ] ] ]
  ]


(* ===================== InfraRevolutionQ ===================== *)

InfraRevolutionQ[ graph_Graph, vs_List, axis_, profile_, opts : OptionsPattern[ FindInfraRevolution ] ] :=
  Sort @ vs === FindInfraRevolution[ graph, axis, profile, opts ][[ 1 ]]

InfraRevolutionQ[ graph_Graph, o : _InfraObject | _InfraSet, axis_, profile_,
    opts : OptionsPattern[ FindInfraRevolution ] ] :=
  InfraRevolutionQ[ graph, First @ o, axis, profile, opts ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraRevolution[ axis_, profile_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      { FindInfraRevolution[ graph, axis, profile,
          Sequence @@ FilterRules[ { opts }, Options[ FindInfraRevolution ] ] ][[ 1 ]] },
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Axis" -> axis, "Profile" -> profile |> ],
    extractBranches[ { opts } ] ]
