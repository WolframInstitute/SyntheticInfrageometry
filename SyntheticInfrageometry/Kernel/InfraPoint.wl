Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findPointPool]
PackageScope[findMidpointCore]
PackageScope[completeEquilateralTriangleCore]


(* ===================== InfraPoint wrapper ===================== *)

(* InfraPoint[reps_List] is the multi-realisation wrapper for points:
   auto-flatten on nested wrappers, Part returns wrapped sub-list,
   accessors ["Realizations"] / ["Length"] / ["Expand"] / ["First"]. *)

InfraPoint[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPoint[ _List ] ] ] :=
  InfraPoint[ Flatten[ reps /. InfraPoint[ xs_List ] :> xs, 1 ] ]

InfraPoint /: Part[ InfraPoint[ reps_List ], i_Integer ] := InfraPoint[ { reps[[ i ]] } ]
InfraPoint /: Part[ InfraPoint[ reps_List ], spec_ ]     := InfraPoint[ reps[[ spec ]] ]

InfraPoint[ reps_List ][ "Realizations" ] := reps
InfraPoint[ reps_List ][ "Length" ]       := Length @ reps
InfraPoint[ reps_List ][ "Expand" ]       := InfraPoint[ { # } ] & /@ reps
InfraPoint[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindPoint ===================== *)

(* FindPoint[g, n] returns n vertices of the graph (the existence postulate
   for points), wrapped as InfraPoint[{v1, ..., vn}].  With "Distance" -> r
   the n vertices form a clique in the r-distance graph (mutually at least
   r apart), realising "n points spread out by r"; with "From" the candidate
   pool is restricted -- "Center", "Periphery", a vertex, a vertex list,
   InfraPoint[{a, b, ...}] (multi-anchor pool), origin -> dist (single
   anchor at given distance, where dist is a number, {dMin, dMax} range, or
   "Max"), or InfraPoint[{a, b, ...}] -> dist (multi-anchor intersection:
   vertices satisfying dist from EVERY anchor).                             *)

Options[ FindPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { pool, dist, range, distMatrix, vertexIndex, auxiliaryGraph, cliques, thresholds, finiteMax, maxCl, picked },
    pool = findPointPool[ graph, OptionValue[ "From" ] ];
    dist = OptionValue[ "Distance" ];
    maxCl = OptionValue[ "MaxCliques" ];
    picked = If[ n == 1 || dist === None,
      RandomSample[ pool, UpTo[ n ] ],
      vertexIndex = Lookup[ AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ], pool ];
      distMatrix = GraphDistanceMatrix[ graph ][[ vertexIndex, vertexIndex ]];
      finiteMax = Max[ Select[ Flatten[ distMatrix ], # < Infinity & ] ];
      distMatrix = Replace[ distMatrix, Infinity -> finiteMax + 1, {2} ];
      If[ dist === "Max",
        thresholds = Reverse @ DeleteCases[ Union @@ distMatrix, 0 | _?( # > finiteMax & ) ];
        cliques = {};
        Do[
          auxiliaryGraph = AdjacencyGraph[ pool,
            UnitStep[ distMatrix - d ] * UnitStep[ finiteMax - distMatrix ] * (1 - IdentityMatrix[ Length[ vertexIndex ] ]) ];
          cliques = FindClique[ auxiliaryGraph, { n, VertexCount[ auxiliaryGraph ] }, maxCl ];
          If[ cliques =!= {}, Break[] ],
          { d, thresholds }
        ];
        If[ cliques === {}, {}, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ],
        range = Replace[ dist, { d_?NumericQ :> { d, finiteMax }, { dMin_, dMax_ } :> { dMin, dMax /. Infinity -> finiteMax } } ];
        auxiliaryGraph = AdjacencyGraph[ pool,
          UnitStep[ distMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - distMatrix ] *
            (1 - IdentityMatrix[ Length[ vertexIndex ] ]) ];
        cliques = FindClique[ auxiliaryGraph, { Min[ n, VertexCount[ auxiliaryGraph ] ], VertexCount[ auxiliaryGraph ] }, maxCl ];
        If[ cliques === {}, {}, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ]
      ]
    ];
    InfraPoint[ picked ]
  ]

FindPoint[ graph_Graph, All, opts : OptionsPattern[] ] :=
  FindPoint[ graph, UpTo[ VertexCount[ graph ] ], opts ]

FindPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindPoint[ graph, UpTo[ n ], opts ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


(* findPointPool resolves the "From" option to a vertex pool. *)

findPointPool[ graph_Graph, "Center" ]    := GraphCenter[ graph ]
findPointPool[ graph_Graph, "Periphery" ] := GraphPeriphery[ graph ]
findPointPool[ graph_Graph, _String ]     := VertexList[ graph ]

findPointPool[ graph_Graph, InfraPoint[ reps_List ] ] := reps

findPointPool[ graph_Graph, ( origin_ -> spec_ ) ] :=
  With[ {
      anchors = If[ MatchQ[ origin, InfraPoint[ _List ] ],
        origin[ "Realizations" ], { origin } ] },
    With[ {
        anchorDists = Association @ Map[ a |-> a -> GraphDistance[ graph, a ], anchors ],
        vertexIndex = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount[ graph ] ] },
      Select[ VertexList[ graph ],
        v |-> And @@ Map[
          a |-> anchorDistMatchQ[ anchorDists[ a ], vertexIndex[ v ], spec ],
          anchors ] ]
    ]
  ]

findPointPool[ graph_Graph, v_ ] /; MemberQ[ VertexList[ graph ], v ] := { v }
findPointPool[ graph_Graph, list_List ] := list
findPointPool[ graph_Graph, _ ]         := VertexList[ graph ]


anchorDistMatchQ[ allDists_List, idx_Integer, d_?NumericQ ] :=
  allDists[[ idx ]] == d

anchorDistMatchQ[ allDists_List, idx_Integer, { lo_?NumericQ, hi_?NumericQ } ] :=
  lo <= allDists[[ idx ]] <= hi

anchorDistMatchQ[ allDists_List, idx_Integer, "Max" ] :=
  allDists[[ idx ]] == Max @ Select[ allDists, # < Infinity & ]


(* ===================== FindMidpoint ===================== *)

(* The midpoint of a segment of length k is the central vertex
   (Ceiling[(k+1)/2]).  FindMidpoint[g, p1, p2, *] collects midpoints
   across every geodesic from p1 to p2 (multi-valued in general).
   Method axis: "Metric" (default, central interval element) | "Tarski"
   (synthetic from BetweennessQ + equidistance) | "Embedding"
   (Euclidean midpoint of embedding coordinates).                       *)

FindMidpoint::nyi = "Method `1` is not yet implemented for FindMidpoint; only \"Metric\" is currently available.";
FindMidpoint::badmethod = "Method `1` is not supported by FindMidpoint.";

Options[ FindMidpoint ] = { Method -> "Metric" };

FindMidpoint[ graph_Graph, segment_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  Module[ { spec = OptionValue[ Method ], methodName },
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    Switch[ methodName,
      "Metric",   InfraPoint[ { segment[[ Ceiling[ Length[ segment ] / 2 ] ]] } ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          InfraPoint[ { First @ embeddingRankMidpoints[ graph, First[ segment ], Last[ segment ], embOpts ] } ]
        ],
      "Spectral" | "Resistance", Message[ FindMidpoint::nyi, methodName ]; $Failed,
      _, Message[ FindMidpoint::badmethod, methodName ]; $Failed
    ]
  ]

FindMidpoint[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    findMidpointCore[ graph, ##, count, opts ] &, p1, p2 ]


findMidpointCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindMidpoint ] ] :=
  Module[ { spec = OptionValue[ FindMidpoint, { opts }, Method ], methodName, segs, midpoints },
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    midpoints = Switch[ methodName,
      "Metric",
        segs = allGeodesics[ graph, p1, p2 ];
        If[ segs === {}, {},
          DeleteDuplicates[ #[[ Ceiling[ Length[ # ] / 2 ] ]] & /@ segs ]
        ],
      "Tarski",
        Select[ VertexList[ graph ],
          m |-> BetweennessQ[ graph, p1, m, p2 ] &&
                GraphDistance[ graph, p1, m ] === GraphDistance[ graph, m, p2 ] ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          embeddingRankMidpoints[ graph, p1, p2, embOpts ]
        ],
      "Spectral" | "Resistance", Message[ FindMidpoint::nyi, methodName ]; $Failed,
      _, Message[ FindMidpoint::badmethod, methodName ]; $Failed
    ];
    Which[
      midpoints === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ midpoints ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ midpoints, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ midpoints, count ],
      count === All,                       midpoints,
      True,                                midpoints
    ]
  ]


(* embeddingRankMidpoints sorts vertices by their Euclidean distance to
   (coord(p1) + coord(p2)) / 2.  Pool "ShortestPaths" ranks only the
   metric interval { w : d(p1, w) + d(w, p2) == d(p1, p2) } -- vertices
   that lie on at least one geodesic; "AllPaths" ranks every vertex. *)

embeddingRankMidpoints[ graph_Graph, p1_, p2_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, target, pool, total },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    target = ( coords[[ vertexIndex[ p1 ] ]] + coords[[ vertexIndex[ p2 ] ]] ) / 2;
    pool = If[ embOpts[ "Pool" ] === "AllPaths", VertexList[ graph ],
      total = GraphDistance[ graph, p1, p2 ];
      Select[ VertexList[ graph ],
        GraphDistance[ graph, p1, # ] + GraphDistance[ graph, #, p2 ] == total & ]
    ];
    SortBy[ pool,
      v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ]
  ]


(* ===================== FindReflection ===================== *)

(* The reflection of x through a: a vertex x' such that B(x, a, x') and
   ax == ax'.  On a graph, x' lies on the geodesic continuation of x past
   a at distance d(a, x).  Multi-valued in general (cycles and graphs
   with multiple geodesics admit several).  Synthetic recipe -- built
   purely from BetweennessQ and graph distance. *)

FindReflection[ graph_Graph, x_, a_, All ] :=
  With[ { r = GraphDistance[ graph, a, x ] },
    If[ r === Infinity, InfraPoint[ {} ],
      InfraPoint @ Select[ VertexList[ graph ],
        y |-> BetweennessQ[ graph, x, a, y ] && GraphDistance[ graph, a, y ] === r
      ]
    ]
  ]

FindReflection[ graph_Graph, x_, a_, UpTo[ n_Integer ] ] :=
  With[ { result = FindReflection[ graph, x, a, All ] },
    InfraPoint @ Take[ result[ "Realizations" ], UpTo[ n ] ]
  ]

FindReflection[ graph_Graph, x_, a_, n_Integer : 1 ] :=
  With[ { result = FindReflection[ graph, x, a, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


(* ===================== CompleteEquilateralTriangle ===================== *)

(* Apex of an equilateral triangle on segment p1 p2 (Euclid I.1): the
   intersection of the spheres of radius d(p1, p2) around p1 and p2 -
   vertices c with d(p1, c) == d(p2, c) == d(p1, p2). *)

CompleteEquilateralTriangle::nyi = "Method `1` is not yet implemented for CompleteEquilateralTriangle; only \"Metric\" is currently available.";
CompleteEquilateralTriangle::badmethod = "Method `1` is not supported by CompleteEquilateralTriangle.";

Options[ CompleteEquilateralTriangle ] = { Method -> "Metric" };

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    completeEquilateralTriangleCore[ graph, ##, count, opts ] &, p1, p2 ]


completeEquilateralTriangleCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ CompleteEquilateralTriangle ] ] :=
  Module[ { method = OptionValue[ CompleteEquilateralTriangle, { opts }, Method ], r, apexes },
    apexes = Switch[ method,
      "Metric",
        r = GraphDistance[ graph, p1, p2 ];
        If[ r === Infinity, {},
          Intersection[
            Select[ VertexList[ graph ], GraphDistance[ graph, p1, # ] == r & ],
            Select[ VertexList[ graph ], GraphDistance[ graph, p2, # ] == r & ]
          ]
        ],
      "Spectral" | "Resistance", Message[ CompleteEquilateralTriangle::nyi, method ]; $Failed,
      _, Message[ CompleteEquilateralTriangle::badmethod, method ]; $Failed
    ];
    Which[
      apexes === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ apexes ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ apexes, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ apexes, count ],
      count === All,                       apexes,
      True,                                apexes
    ]
  ]


(* ===================== FindCommonPoint ===================== *)

(* Vertices common to every listed line -- the intersection of the lines.
   Constructive companion of ConcurrentQ.  Each input line is either a
   bare vertex sequence or a wrapped InfraSegment / InfraRay; wrapped
   entries contribute the union of their vertex realisations.            *)

linePointSet[ InfraSegment[ reps_List ] ] := Union @@ reps
linePointSet[ InfraRay    [ reps_List ] ] := Union @@ reps
linePointSet[ line_List ] := line

FindCommonPoint[ graph_Graph, lines_List, All ] :=
  If[ Length[ lines ] == 0,
    InfraPoint[ {} ],
    InfraPoint[ Apply[ Intersection, linePointSet /@ lines ] ]
  ]

FindCommonPoint[ graph_Graph, lines_List, UpTo[ n_Integer ] ] :=
  With[ { result = FindCommonPoint[ graph, lines, All ] },
    InfraPoint[ Take[ result[ "Realizations" ], UpTo[ n ] ] ]
  ]

FindCommonPoint[ graph_Graph, lines_List, n_Integer : 1 ] :=
  With[ { result = FindCommonPoint[ graph, lines, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]
