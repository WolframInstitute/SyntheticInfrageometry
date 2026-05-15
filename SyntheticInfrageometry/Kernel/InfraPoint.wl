Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findPointPool]
PackageScope[findMidpointCore]
PackageScope[findReflectionCore]
PackageScope[completeEquilateralTriangleCore]
PackageScope[selectFromPointSpace]


(* ===================== InfraPoint wrapper ===================== *)

InfraPoint[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPoint[ _List ] ] ] :=
  InfraPoint[ Flatten[ reps /. InfraPoint[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraPoint ===================== *)

(* FindInfraPoint[g, n] returns n unary InfraPoint[{v}] wrappers.  With
   "Distance" -> r the n vertices are mutually at exactly distance r;
   with {dMin, dMax} mutually within that range; with "Max" at the
   maximum finite mutual distance.  "From" restricts the candidate pool. *)

Options[ FindInfraPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindInfraPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
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

FindInfraPoint[ graph_Graph, All, opts : OptionsPattern[] ] :=
  FindInfraPoint[ graph, UpTo[ VertexCount[ graph ] ], opts ]

FindInfraPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindInfraPoint[ graph, UpTo[ n ], opts ] },
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


(* ===================== FindInfraMidpoint ===================== *)

(* Midpoint of a segment of length k: central interval element, index Ceiling[(k+1)/2].
   FindInfraMidpoint[g, p1, p2, n] collects midpoints across every geodesic from p1 to p2. *)

Options[ FindInfraMidpoint ] = { Method -> "Metric" };

FindInfraMidpoint[ graph_Graph, segment_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  With[ { method = methodName @ OptionValue[ Method ] },
    Switch[ method,
      "Metric",   { InfraPoint[ { segment[[ Ceiling[ Length[ segment ] / 2 ] ]] } ] },
      "Embedding", { InfraPoint[ { First @ embeddingRankMidpoints[ graph,
                       First[ segment ], Last[ segment ], parseEmbeddingMethod[ OptionValue[ Method ] ] ] } ] }
    ]
  ]

FindInfraMidpoint[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count, findMidpointCore[ graph, ##, opts ] &, p1, p2 ]


findMidpointCore[ graph_Graph, p1_, p2_, opts : OptionsPattern[ FindInfraMidpoint ] ] :=
  With[ { method = methodName @ OptionValue[ FindInfraMidpoint, { opts }, Method ] },
    Switch[ method,
      "Metric",
        DeleteDuplicates[ #[[ Ceiling[ Length[ # ] / 2 ] ]] & /@ allGeodesics[ graph, p1, p2 ] ],
      "Tarski",
        (* Localize: midpoints live on shortest p1-p2 paths, all within B(p1, d(p1,p2)). *)
        With[ { localG = NeighborhoodGraph[ graph, p1, GraphDistance[ graph, p1, p2 ] ] },
          Select[ VertexList[ localG ],
            m |-> BetweennessQ[ localG, p1, m, p2 ] && GraphDistance[ localG, p1, m ] === GraphDistance[ localG, m, p2 ] ] ],
      "Embedding",
        embeddingRankMidpoints[ graph, p1, p2, parseEmbeddingMethod @ OptionValue[ FindInfraMidpoint, { opts }, Method ] ]
    ]
  ]


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


(* ===================== FindInfraReflection ===================== *)

(* Reflection of x through a: vertex y with BetweennessQ[x, a, y] and d(a, y) = d(a, x).
   On a graph this is the geodesic continuation of x past a at the same distance. *)

FindInfraReflection[ graph_Graph, x_, a_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  infraSpreadAndCartesian[ InfraPoint, count, findReflectionCore[ graph, ##] &, x, a ]


findReflectionCore[ graph_Graph, x_, a_ ] :=
  With[ { r = GraphDistance[ graph, a, x ] },
    If[ r === Infinity, {},
      (* Localize: reflection lives in B(a, r), straddle-paths stay in B(a, 2 r). *)
      With[ { localG = NeighborhoodGraph[ graph, a, 2 r ] },
        Select[ VertexList[ localG ],
          y |-> BetweennessQ[ localG, x, a, y ] && GraphDistance[ localG, a, y ] === r ] ]
    ]
  ]


(* ===================== CompleteInfraEquilateralTriangle ===================== *)

(* Apex of an equilateral triangle on p1, p2 (Euclid I.1): vertex c with
   d(p1, c) = d(p2, c) = d(p1, p2) -- the intersection of the two spheres. *)

Options[ CompleteInfraEquilateralTriangle ] = { Method -> "Metric" };

CompleteInfraEquilateralTriangle[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count, completeEquilateralTriangleCore[ graph, ##, opts ] &, p1, p2 ]


completeEquilateralTriangleCore[ graph_Graph, p1_, p2_,
    opts : OptionsPattern[ CompleteInfraEquilateralTriangle ] ] :=
  With[ { r = GraphDistance[ graph, p1, p2 ] },
    If[ r === Infinity, {},
      Intersection[
        Select[ VertexList[ graph ], GraphDistance[ graph, p1, # ] == r & ],
        Select[ VertexList[ graph ], GraphDistance[ graph, p2, # ] == r & ] ]
    ]
  ]


(* ===================== FindInfraCommonPoint ===================== *)

(* Vertices common to every listed line: the intersection of the lines.
   Each input line is a bare vertex sequence or a wrapped InfraLine /
   InfraSegment / InfraPath / InfraRay. *)

linePointSet[ InfraLine   [ reps_List ] ] := Union @@ reps
linePointSet[ InfraSegment[ reps_List ] ] := Union @@ reps
linePointSet[ InfraPath   [ reps_List ] ] := Union @@ reps
linePointSet[ InfraRay    [ reps_List ] ] := Union @@ reps
linePointSet[ line_List ] := line

FindInfraCommonPoint[ graph_Graph, lines_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { vs = If[ Length[ lines ] == 0, {},
        Apply[ Intersection, linePointSet /@ lines ] ] },
    With[ { capped = infraCap[ vs, count ] },
      If[ capped === $Failed, $Failed, InfraPoint[ { # } ] & /@ capped ]
    ]
  ]


(* ===================== SelectInfraPoint ===================== *)

(* Chainable post-filter on a bundle of vertices treated as a finite metric
   space under the graph distance.  Pool selectors mirror FindInfraPoint;
   "Center" / "Periphery" use sub-bundle eccentricity, "Distance" enforces a
   mutual-distance clique on the n returned vertices. *)

Options[ SelectInfraPoint ] = { "From" -> All, "Distance" -> None, "MaxCliques" -> All };

SelectInfraPoint[ graph_Graph, vertices_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  InfraPoint[ { # } ] & /@ selectFromPointSpace[ graph, vertices, n,
    OptionValue[ "From" ], OptionValue[ "Distance" ], OptionValue[ "MaxCliques" ] ]

SelectInfraPoint[ graph_Graph, vertices_List, All, opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  SelectInfraPoint[ graph, vertices, UpTo[ Length[ vertices ] ], opts ]

SelectInfraPoint[ graph_Graph, vertices_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    vertices === { } || ! AllTrue[ vertices, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  With[ { result = SelectInfraPoint[ graph, vertices, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectInfraPoint[ graph_Graph, InfraPoint[ vs_List ],
                  countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectInfraPoint[ graph, vs, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraPoint[ First /@ result ] ] ]

SelectInfraPoint[ graph_Graph, list_List,
                  countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  SelectInfraPoint[ graph, First /@ list, countSpec, opts ]

SelectInfraPoint[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectInfraPoint[ graph, #, countSpec, opts ] &


selectFromPointSpace[ _Graph, vertices_List, _Integer, _, _, _ ] /; Length[ vertices ] <= 1 := vertices

selectFromPointSpace[ graph_Graph, vertices_List, nMax_Integer,
                      fromSpec_, distSpec_, maxCl_ ] :=
  Module[ { vIdx, subMatrix, poolIdx, pool, poolSubMatrix, finiteMax, range,
            auxiliaryGraph, cliques, thresholds, n },
    vIdx = Lookup[ AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ], vertices ];
    subMatrix = GraphDistanceMatrix[ graph ][[ vIdx, vIdx ]];
    poolIdx = pointPoolPositions[ graph, vertices, fromSpec, subMatrix ];
    If[ poolIdx === { }, Return[ { } ] ];
    pool = vertices[[ poolIdx ]];
    n = Min[ nMax, Length[ pool ] ];
    If[ distSpec === None || n <= 1,
      Return[ If[ n >= Length[ pool ], pool, RandomSample[ pool, n ] ] ] ];
    poolSubMatrix = subMatrix[[ poolIdx, poolIdx ]];
    finiteMax = Replace[ Max @ Select[ Flatten @ poolSubMatrix, # < Infinity & ],
      _?( ! NumericQ @ # & ) -> 0 ];
    poolSubMatrix = Replace[ poolSubMatrix, Infinity -> finiteMax + 1, { 2 } ];
    Which[
      distSpec === "Max",
        thresholds = Reverse @ DeleteCases[ Union @@ poolSubMatrix, 0 | _?( # > finiteMax & ) ];
        cliques = { };
        Do[
          auxiliaryGraph = AdjacencyGraph[ pool,
            UnitStep[ poolSubMatrix - d ] * UnitStep[ finiteMax - poolSubMatrix ]
              * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
          cliques = FindClique[ auxiliaryGraph, { n, VertexCount[ auxiliaryGraph ] }, maxCl ];
          If[ cliques =!= { }, Break[ ] ],
          { d, thresholds } ];
        If[ cliques === { }, { }, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ],
      True,
        range = Replace[ distSpec,
          { d_?NumericQ                  :> { d, d },
            { dMin_?NumericQ, Infinity } :> { dMin, finiteMax },
            { dMin_?NumericQ, dMax_?NumericQ } :> { dMin, dMax },
            _ :> { 0, finiteMax } } ];
        auxiliaryGraph = AdjacencyGraph[ pool,
          UnitStep[ poolSubMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - poolSubMatrix ]
            * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
        cliques = FindClique[ auxiliaryGraph,
          { Min[ n, VertexCount[ auxiliaryGraph ] ], VertexCount[ auxiliaryGraph ] }, maxCl ];
        If[ cliques === { }, { }, RandomSample[ RandomChoice @ cliques, UpTo[ n ] ] ]
    ]
  ]


pointPoolPositions[ _Graph, vertices_List, All, _ ] := Range @ Length @ vertices

pointPoolPositions[ _, _, "Center", subMatrix_ ] :=
  With[ { scores = Max /@ subMatrix },
    Flatten @ Position[ scores, Min @ scores, { 1 }, Heads -> False ] ]

pointPoolPositions[ _, _, "Periphery", subMatrix_ ] :=
  With[ { scores = Max /@ subMatrix },
    Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ] ]

pointPoolPositions[ graph_Graph, vertices_List, ( anchor_ -> spec_ ), _ ] :=
  With[ { anchors = infraSpread[ anchor ],
          vertexIndex = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount[ graph ] ] },
    With[ { anchorDists = Association[ # -> GraphDistance[ graph, # ] & /@ anchors ] },
      Flatten @ Position[ vertices,
        v_ /; AllTrue[ anchors, a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ] ],
        { 1 }, Heads -> False ]
    ]
  ]

pointPoolPositions[ _, vertices_List, InfraPoint[ reps_List ], _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ reps, { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, v_, _ ] /; MemberQ[ vertices, v ] :=
  { First @ FirstPosition[ vertices, v ] }

pointPoolPositions[ _, vertices_List, list_List, _ ] :=
  Flatten @ Position[ vertices, Alternatives @@ list, { 1 }, Heads -> False ]

pointPoolPositions[ _, vertices_List, _, _ ] := Range @ Length @ vertices
