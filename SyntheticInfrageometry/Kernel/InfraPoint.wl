Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findPointPool]
PackageScope[findMidpointCore]
PackageScope[findReflectionCore]
PackageScope[completeEquilateralTriangleCore]


(* ===================== InfraPoint wrapper ===================== *)

InfraPoint[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPoint[ _List ] ] ] :=
  InfraPoint[ Flatten[ reps /. InfraPoint[ xs_List ] :> xs, 1 ] ]


(* ===================== FindPoint ===================== *)

(* FindPoint[g, n] returns n unary InfraPoint[{v}] wrappers.  With
   "Distance" -> r the n vertices are mutually at exactly distance r;
   with {dMin, dMax} mutually within that range; with "Max" at the
   maximum finite mutual distance.  "From" restricts the candidate pool. *)

Options[ FindPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { pool = findPointPool[ graph, OptionValue[ "From" ] ],
            dist = OptionValue[ "Distance" ],
            maxCl = OptionValue[ "MaxCliques" ],
            distMatrix, finiteMax, cliques },
    InfraPoint[ { # } ] & /@ If[ n == 1 || dist === None,
      RandomSample[ pool, UpTo[ n ] ],
      With[ { vertexIndex = Lookup[ AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ], pool ] },
        distMatrix = GraphDistanceMatrix[ graph ][[ vertexIndex, vertexIndex ]];
        finiteMax = Max @ Select[ Flatten @ distMatrix, # < Infinity & ];
        distMatrix = Replace[ distMatrix, Infinity -> finiteMax + 1, { 2 } ];
        With[ { mask = 1 - IdentityMatrix @ Length @ vertexIndex },
          If[ dist === "Max",
            cliques = {};
            Do[
              cliques = FindClique[
                AdjacencyGraph[ pool, UnitStep[ distMatrix - d ] * UnitStep[ finiteMax - distMatrix ] * mask ],
                { n, Length @ pool }, maxCl ];
              If[ cliques =!= {}, Break[] ],
              { d, Reverse @ DeleteCases[ Union @@ distMatrix, 0 | _?( # > finiteMax & ) ] } ];
            If[ cliques === {}, {}, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ],
            With[ { range = Replace[ dist,
                    { d_?NumericQ :> { d, d },
                      { dMin_, dMax_ } :> { dMin, dMax /. Infinity -> finiteMax } } ] },
              cliques = FindClique[
                AdjacencyGraph[ pool, UnitStep[ distMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - distMatrix ] * mask ],
                { Min[ n, Length @ pool ], Length @ pool }, maxCl ];
              If[ cliques === {}, {}, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ]
            ]
          ]
        ]
      ]
    ]
  ]

FindPoint[ graph_Graph, All, opts : OptionsPattern[] ] :=
  FindPoint[ graph, UpTo[ VertexCount[ graph ] ], opts ]

FindPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindPoint[ graph, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ] ]


(* "From" option dispatch -- vertex pool to draw points from. *)

findPointPool[ graph_Graph, "Center" ]    := GraphCenter[ graph ]
findPointPool[ graph_Graph, "Periphery" ] := GraphPeriphery[ graph ]
findPointPool[ graph_Graph, _String ]     := VertexList[ graph ]

findPointPool[ graph_Graph, InfraPoint[ reps_List ] ] := reps

findPointPool[ graph_Graph, ( origin_ -> spec_ ) ] :=
  With[ { anchors = infraSpread[ origin ],
          vertexIndex = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount[ graph ] ] },
    With[ { anchorDists = Association[ # -> GraphDistance[ graph, # ] & /@ anchors ] },
      Select[ VertexList[ graph ],
        v |-> AllTrue[ anchors, a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ] ] ]
    ]
  ]

findPointPool[ graph_Graph, v_ ] /; MemberQ[ VertexList[ graph ], v ] := { v }
findPointPool[ graph_Graph, list_List ] /; AllTrue[ list, MatchQ[ InfraPoint[ { _ } ] ] ] := First /@ list
findPointPool[ graph_Graph, list_List ] := list
findPointPool[ graph_Graph, _ ]         := VertexList[ graph ]


anchorDistMatchQ[ allDists_List, idx_Integer, d_?NumericQ ]                  := allDists[[ idx ]] == d
anchorDistMatchQ[ allDists_List, idx_Integer, { lo_?NumericQ, hi_?NumericQ } ] := lo <= allDists[[ idx ]] <= hi
anchorDistMatchQ[ allDists_List, idx_Integer, "Max" ]                        :=
  allDists[[ idx ]] == Max @ Select[ allDists, # < Infinity & ]


(* ===================== FindMidpoint ===================== *)

(* Midpoint of a segment of length k: central interval element, index Ceiling[(k+1)/2].
   FindMidpoint[g, p1, p2, n] collects midpoints across every geodesic from p1 to p2. *)

Options[ FindMidpoint ] = { Method -> "Metric" };

FindMidpoint[ graph_Graph, segment_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  With[ { method = methodName @ OptionValue[ Method ] },
    Switch[ method,
      "Metric",   { InfraPoint[ { segment[[ Ceiling[ Length[ segment ] / 2 ] ]] } ] },
      "Embedding", { InfraPoint[ { First @ embeddingRankMidpoints[ graph,
                       First[ segment ], Last[ segment ], parseEmbeddingMethod[ OptionValue[ Method ] ] ] } ] }
    ]
  ]

FindMidpoint[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count, findMidpointCore[ graph, ##, opts ] &, p1, p2 ]


findMidpointCore[ graph_Graph, p1_, p2_, opts : OptionsPattern[ FindMidpoint ] ] :=
  With[ { method = methodName @ OptionValue[ FindMidpoint, { opts }, Method ] },
    Switch[ method,
      "Metric",
        DeleteDuplicates[ #[[ Ceiling[ Length[ # ] / 2 ] ]] & /@ allGeodesics[ graph, p1, p2 ] ],
      "Tarski",
        Select[ VertexList[ graph ],
          m |-> BetweennessQ[ graph, p1, m, p2 ] && GraphDistance[ graph, p1, m ] === GraphDistance[ graph, m, p2 ] ],
      "Embedding",
        embeddingRankMidpoints[ graph, p1, p2, parseEmbeddingMethod @ OptionValue[ FindMidpoint, { opts }, Method ] ]
    ]
  ]


methodName[ m_String ]         := m
methodName[ { m_String, ___ } ] := m


(* Sort the metric interval (Pool -> "ShortestPaths") or every vertex (Pool -> "AllPaths")
   by Euclidean distance from each vertex's embedding coordinate to (coord(p1) + coord(p2)) / 2. *)

embeddingRankMidpoints[ graph_Graph, p1_, p2_, embOpts_Association ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ],
          total = GraphDistance[ graph, p1, p2 ] },
    With[ { target = ( coords[[ vertexIndex[ p1 ] ]] + coords[[ vertexIndex[ p2 ] ]] ) / 2 },
      SortBy[
        If[ embOpts[ "Pool" ] === "AllPaths", VertexList[ graph ],
          Select[ VertexList[ graph ],
            GraphDistance[ graph, p1, # ] + GraphDistance[ graph, #, p2 ] == total & ] ],
        v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ]
    ]
  ]


(* ===================== FindReflection ===================== *)

(* Reflection of x through a: vertex y with BetweennessQ[x, a, y] and d(a, y) = d(a, x).
   On a graph this is the geodesic continuation of x past a at the same distance. *)

FindReflection[ graph_Graph, x_, a_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  infraSpreadAndCartesian[ InfraPoint, count, findReflectionCore[ graph, ##] &, x, a ]


findReflectionCore[ graph_Graph, x_, a_ ] :=
  With[ { r = GraphDistance[ graph, a, x ] },
    If[ r === Infinity, {},
      Select[ VertexList[ graph ],
        y |-> BetweennessQ[ graph, x, a, y ] && GraphDistance[ graph, a, y ] === r ] ]
  ]


(* ===================== CompleteEquilateralTriangle ===================== *)

(* Apex of an equilateral triangle on p1, p2 (Euclid I.1): vertex c with
   d(p1, c) = d(p2, c) = d(p1, p2) -- the intersection of the two spheres. *)

Options[ CompleteEquilateralTriangle ] = { Method -> "Metric" };

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count, completeEquilateralTriangleCore[ graph, ##, opts ] &, p1, p2 ]


completeEquilateralTriangleCore[ graph_Graph, p1_, p2_,
    opts : OptionsPattern[ CompleteEquilateralTriangle ] ] :=
  With[ { r = GraphDistance[ graph, p1, p2 ] },
    If[ r === Infinity, {},
      Intersection[
        Select[ VertexList[ graph ], GraphDistance[ graph, p1, # ] == r & ],
        Select[ VertexList[ graph ], GraphDistance[ graph, p2, # ] == r & ] ]
    ]
  ]


(* ===================== FindCommonPoint ===================== *)

(* Vertices common to every listed line: the intersection of the lines.
   Each input line is a bare vertex sequence or a wrapped InfraSegment / InfraRay. *)

linePointSet[ InfraSegment[ reps_List ] ] := Union @@ reps
linePointSet[ InfraRay    [ reps_List ] ] := Union @@ reps
linePointSet[ line_List ] := line

FindCommonPoint[ graph_Graph, lines_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { vs = If[ Length[ lines ] == 0, {},
        Apply[ Intersection, linePointSet /@ lines ] ] },
    With[ { capped = infraCap[ vs, count ] },
      If[ capped === $Failed, $Failed, InfraPoint[ { # } ] & /@ capped ]
    ]
  ]
