Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]
PackageScope[findSegmentCore]
PackageScope[findLineCore]
PackageScope[extendSegmentCore]
PackageScope[findShellCore]
PackageScope[findCircleCore]
PackageScope[findParallelCore]
PackageScope[infraSpreadAndCartesian]


(* ===================== Messages ===================== *)

FindSegment::badmethod = "Method `1` is not supported by FindSegment.";
FindSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
FindSegment::badwindow = "ShortestPathWindow specification `1` is not supported; use a positive integer or All.";
FindSegment::badpool = "Pool specification `1` is not supported; use \"ShortestPaths\" or \"AllPaths\".";
FindSegment::badcurvature = "Curvature specification `1` is not supported; use \"Forman\", \"Wolfram\", {\"Forman\", Method -> \"Simple\" | \"Triangles\"}, or {\"Wolfram\", \"Dimension\" -> Automatic | d, \"Radii\" -> Automatic | {rmin, rmax}}.";


(* ===================== Points ===================== *)

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


(* findPointPool resolves the "From" option to a vertex pool.
   Anchors: "Center" / "Periphery" / "Random" string pools, a bare vertex,
   a bare vertex list, InfraPoint[{...}] (multi-anchor pool, the realisations
   themselves are the pool when no distance constraint is supplied).
   With a distance clause origin -> dist, the pool is restricted to vertices
   satisfying the distance constraint from origin (a single vertex) or from
   EVERY realisation in InfraPoint[{...}] (multi-anchor intersection).      *)

PackageScope[findPointPool]

findPointPool[ graph_Graph, "Center" ]    := GraphCenter[ graph ]
findPointPool[ graph_Graph, "Periphery" ] := GraphPeriphery[ graph ]
findPointPool[ graph_Graph, _String ]     := VertexList[ graph ]

findPointPool[ graph_Graph, InfraPoint[ reps_List ] ] := reps

findPointPool[ graph_Graph, ( origin_ -> spec_ ) ] :=
  With[ {
      anchors = If[ MatchQ[ origin, InfraPoint[ _List ] ],
        origin[ "Realisations" ], { origin } ] },
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


(* ===================== Segments ===================== *)

(* A segment between p1 and p2 is a geodesic vertex sequence
   (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2) and consecutive vi
   adjacent.  Method -> "Shortest" (default) enumerates geodesics:
   count = 1 takes the built-in FindShortestPath; count > 1 (or UpTo[n]
   / All) enumerates via FindPath at exact length d, and the result
   pipes through SelectPaths / EmbeddingClosestPaths for further path-space
   filtering.  Method -> "ShortestPathExtension" (or
   {"ShortestPathExtension", "ShortestPathWindow" -> K, "Pruning" -> spec})
   enumerates simple paths from p1 to p2 via a constructive frontier sweep
   that admits a step v_i -> w iff w is unvisited and the recency-lex
   distance tuple ( d(v_{i-1}, w), ..., d(v_{i-K+1}, w) ) is maximal among
   unvisited neighbours.  K = 1 has empty tuple (no filter, all simple
   paths), K = 2 (default) excludes triangle shortcuts, K = All compares
   against every available predecessor and on most graphs collapses to
   geodesics.  Pruning spec is Infinity (default), a positive integer beam
   width, or a Bernoulli keep probability.  Method -> "CurvatureMinimizing"
   (or {"CurvatureMinimizing", "Curvature" -> spec, "Pool" -> p,
   "Pruning" -> q}) traces the rope being pulled tight from p1 toward p2 by
   a frontier sweep that admits step v_i -> w iff w is unvisited and
   minimises an edge-level curvature kappa(v_i, w) among the candidate
   neighbours of v_i; ties branch the frontier.  Curvature spec is
   "Forman" / {"Forman", Method -> "Simple"|"Triangles"} (forwarded to
   FormanRicci) or "Wolfram" / {"Wolfram", "Dimension" -> ..., "Radii" ->
   ...} (forwarded to WolframRicci).  Pool is "ShortestPaths" (default;
   restrict candidates to the geodesic DAG) or "AllPaths" (full
   adjacency, paths simple but possibly detouring).  *)

Options[ FindSegment ] = { Method -> "Shortest" };

(* FindSegment public entry: dispatches over multi-realisation endpoints
   (InfraPoint / InfraSegment wrappers).  For each (a, b) in the Cartesian
   product of the realisations of p1 and p2, the single-pair core
   findSegmentCore is invoked with the requested count.  Strict-n shortfall
   on any pair propagates as bare $Failed; otherwise the per-pair bare-list
   results are union-deduplicated and wrapped as InfraSegment[...].         *)

FindSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findSegmentCore[ graph, ##, count, opts ] &, p1, p2 ]


(* findSegmentCore is the single-pair body: returns a bare list of vertex
   sequences, or $Failed on strict-n shortfall.                              *)

findSegmentCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindSegment ] ] :=
  Module[ { spec = OptionValue[ FindSegment, { opts }, Method ], methodName, prune, shortestPathWindow, pool, curvature, curvatureSpec, dagNbrs, edgeKappa, d, paths },
    If[ p1 === p2, Return[ { } ] ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    { prune, shortestPathWindow, pool, curvature } = Replace[ spec, {
      _String :> { Infinity, 2, "ShortestPaths", "Forman" },
      { _String, subOpts___ } :> {
        "Pruning"            /. { subOpts } /. "Pruning"            -> Infinity,
        "ShortestPathWindow" /. { subOpts } /. "ShortestPathWindow" -> 2,
        "Pool"               /. { subOpts } /. "Pool"               -> "ShortestPaths",
        "Curvature"          /. { subOpts } /. "Curvature"          -> "Forman"
      },
      _ :> { Infinity, 2, "ShortestPaths", "Forman" }
    } ];
    Switch[ methodName,
      "Shortest",
        paths = If[ count === 1,
          With[ { path = FindShortestPath[ graph, p1, p2 ] },
            If[ path === { }, { }, { path } ]
          ],
          d = GraphDistance[ graph, p1, p2 ];
          If[ d === Infinity, { },
            FindPath[ graph, p1, p2, { d }, count /. UpTo[ k_ ] :> k ]
          ]
        ];
        If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ],
      "ShortestPathExtension",
        Which[
          ! pruningSpecQ[ prune ],
            Message[ FindSegment::badpruning, prune ]; $Failed,
          ! shortestPathWindowSpecQ[ shortestPathWindow ],
            Message[ FindSegment::badwindow, shortestPathWindow ]; $Failed,
          True,
            paths = extendedOutPaths[ graph, p1, p2, prune, shortestPathWindow, countLimit[ count ] ];
            If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ]
        ],
      "CurvatureMinimizing",
        curvatureSpec = parseCurvatureSpec[ curvature ];
        Which[
          curvatureSpec === $Failed,
            Message[ FindSegment::badcurvature, curvature ]; $Failed,
          ! pruningSpecQ[ prune ],
            Message[ FindSegment::badpruning, prune ]; $Failed,
          ! poolSpecQ[ pool ],
            Message[ FindSegment::badpool, pool ]; $Failed,
          True,
            dagNbrs = If[ pool === "ShortestPaths", geodesicDAGNeighbors[ graph, p1, p2 ], Automatic ];
            edgeKappa = buildEdgeKappa[ graph, curvatureSpec ];
            paths = pulledPaths[ graph, p1, p2, edgeKappa, prune, countLimit[ count ], dagNbrs ];
            If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ]
        ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          Which[
            ! pruningSpecQ[ embOpts[ "Pruning" ] ],
              Message[ FindSegment::badpruning, embOpts[ "Pruning" ] ]; $Failed,
            ! poolSpecQ[ embOpts[ "Pool" ] ],
              Message[ FindSegment::badpool, embOpts[ "Pool" ] ]; $Failed,
            True,
              paths = embeddingFindSegmentPaths[ graph, p1, p2, embOpts ];
              With[ { result = takeUpTo[ paths, countLimit[ count ] ] },
                If[ MatchQ[ count, _Integer ] && Length[ result ] < count, $Failed, result ]
              ]
          ]
        ],
      _, Message[ FindSegment::badmethod, spec ]; $Failed
    ]
  ]


(* buildEdgeKappa returns the (v, w) |-> Real closure that the
   CurvatureMinimizing frontier sweep MinimalBy's on.  Dispatches on the
   normalised curvature spec produced by parseCurvatureSpec: "Forman"
   forwards Method to FormanRicci and uses the symmetric edge curvature
   F(v, w); "Wolfram" forwards Dimension and Radii to WolframRicci and
   uses the target-vertex Wolfram-Ricci scalar (the rope at v feels the
   curvature of the cell it is about to enter). *)

buildEdgeKappa[ graph_Graph, KeyValuePattern[ { "Head" -> "Forman", "Method" -> formanMethod_ } ] ] :=
  With[ { fEdges = FormanRicci[ graph, Method -> formanMethod ] },
    With[ { fSym = Join[
        fEdges,
        AssociationThread[
          ( UndirectedEdge[ #[[ 2 ]], #[[ 1 ]] ] & ) /@ Keys[ fEdges ],
          Values[ fEdges ]
        ]
      ] },
      { v, w } |-> fSym[ UndirectedEdge[ v, w ] ]
    ]
  ]

buildEdgeKappa[ graph_Graph, KeyValuePattern[ { "Head" -> "Wolfram", "Dimension" -> dim_, "Radii" -> radii_ } ] ] :=
  With[ { vertexKappa = If[ radii === Automatic,
        WolframRicci[ graph, "Dimension" -> dim ],
        WolframRicci[ graph, radii, "Dimension" -> dim ]
      ] },
    { v, w } |-> vertexKappa[ w ]
  ]


(* embeddingFindSegmentPaths returns paths from p1 to p2 sorted by their
   embedding-Hausdorff distance to the straight Euclidean segment p1 p2.
   Pool "ShortestPaths" recurses through the geodesic DAG; "AllPaths"
   recurses through the whole graph along simple-path extensions. *)

embeddingFindSegmentPaths[ graph_Graph, p1_, p2_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, dagNbrs, extendFn, prune, paths, ep },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    prune = embOpts[ "Pruning" ];
    extendFn = If[ embOpts[ "Pool" ] === "ShortestPaths",
      dagNbrs = geodesicDAGNeighbors[ graph, p1, p2 ];
      ( path |-> Lookup[ dagNbrs, Key[ Last[ path ] ], { } ] ),
      ( path |-> Complement[ AdjacencyList[ graph, Last[ path ] ], path ] )
    ];
    paths = generateEmbeddingPaths[ extendFn, { p1 }, ( Last[ # ] === p2 & ), prune ];
    ep = Lookup[ vertexIndex, { p1, p2 } ];
    SortBy[ paths,
      path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
  ]

(* ===================== Lines ===================== *)

(* A line through p1 and p2 is a maximal geodesic extension: a vertex
   sequence (a, ..., p1, ..., p2, ..., b) every contiguous sub-sequence of
   which is a geodesic and that cannot be extended at either end without
   breaking the geodesic property.  FindLine enumerates such maximal
   extensions; "Maximality" -> "Diameter" further restricts to those whose
   length equals GraphDiameter[g].
     Method axis is shared with FindSegment / ExtendSegment.
   "Shortest" (default) returns every maximal geodesic extension.
   "ShortestPathExtension" returns every maximal geodesic extension of every
   "ShortestPathExtension"-segment from p1 to p2 (the underlying middle path is then a
   recency-lex-certified simple path, the prefix and suffix are still
   geodesic).  "CurvatureMinimizing" does the same with curvature-pulled middle paths.
   "Embedding" ranks the extensions by EmbeddingHausdorffDistance to the
   infinite Euclidean line through p1 and p2 under the graph drawing.
   To extend a given segment to a line, use ExtendSegment. *)

FindLine::badmethod = "Method `1` is not supported by FindLine.";
FindLine::nyi = "Pool `1` is not yet implemented for FindLine; use \"ShortestPaths\".";

Options[ FindLine ] = { "Maximality" -> "Extension", Method -> "Shortest" };

(* FindLine public dispatcher: spreads multi-realisation endpoints over the
   Cartesian product of realisations, runs the single-pair core for each,
   unions the bare-list results, and wraps as InfraSegment[...].  Strict-n
   shortfall on any pair propagates as $Failed.                              *)

FindLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findLineCore[ graph, ##, count, opts ] &, p1, p2 ]


(* findLineCore is the single-pair body: returns a bare list of maximal
   geodesic extensions through p1 and p2, capped to count.  $Failed on
   strict-n shortfall or unknown method.                                     *)

findLineCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindLine ] ] /;
    MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { spec, methodName, middles, allExtensions, diam, ranked },
    spec = OptionValue[ FindLine, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, Automatic -> "Shortest", _ :> spec } ];
    middles = Switch[ methodName,
      "Shortest" | "Embedding", allGeodesics[ graph, p1, p2 ],
      "ShortestPathExtension" | "CurvatureMinimizing",
        With[ { paths = findSegmentCore[ graph, p1, p2, All, Method -> spec ] },
          If[ paths === $Failed || ! ListQ[ paths ], { }, paths ]
        ],
      _, $Failed
    ];
    If[ middles === $Failed, Message[ FindLine::badmethod, spec ]; Return[ $Failed ] ];
    allExtensions = Union @ Flatten[ findLineExtensions[ graph, # ] & /@ middles, 1 ];
    If[ OptionValue[ FindLine, { opts }, "Maximality" ] === "Diameter",
      diam = GraphDiameter[ graph ];
      allExtensions = Select[ allExtensions, line |-> Length[ line ] - 1 == diam ]
    ];
    ranked = Switch[ methodName,
      "Shortest" | "ShortestPathExtension" | "CurvatureMinimizing", allExtensions,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          If[ embOpts[ "Pool" ] =!= "ShortestPaths",
            Message[ FindLine::nyi, embOpts[ "Pool" ] ]; allExtensions,
            embeddingRankLines[ graph, allExtensions, p1, p2, embOpts ]
          ]
        ]
    ];
    Which[
      MatchQ[ count, _Integer ] && Length[ ranked ] < count, $Failed,
      MatchQ[ count, _Integer ],                              Take[ ranked, count ],
      MatchQ[ count, UpTo[ _Integer ] ],                      Take[ ranked, count ],
      count === All,                                          ranked,
      True,                                                   ranked
    ]
  ]

findLineCore[ _Graph, _, _, _, OptionsPattern[ FindLine ] ] := $Failed


(* embeddingRankLines sorts maximal extensions by their Hausdorff distance
   to the infinite Euclidean line through p1 and p2 under the supplied
   embedding.  We use a Line[{coord(p1), coord(p2)}] segment as the
   reference (RegionHausdorffDistance against an infinite line is the
   same as against any segment containing both projection feet for our
   bounded path). *)

embeddingRankLines[ graph_Graph, lines_List, p1_, p2_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, ep },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    ep = Lookup[ vertexIndex, { p1, p2 } ];
    SortBy[ lines,
      line |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, line ], ep ] ]
  ]

findLineExtensions[ graph_Graph, segment_List ] :=
  findLineExtensionsWith[ graph, segment,
    { s, p, db } |-> FindPath[ graph, s, p, { db }, All ],
    { p, e, da } |-> FindPath[ graph, p, e, { da }, All ] ]


(* findLineExtensionsWith generalises findLineExtensions: candidate before /
   after vertices are still found via the geodesic relation (so the
   maximality target is graph distance), but the prefix / suffix path
   enumeration is delegated to caller-supplied closures. Used by
   ExtendSegment to swap geodesic FindPath for extended / pulled simple-path
   enumeration. *)

findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_ ] :=
  Module[ { p1, p2, d, extendBefore, extendAfter, pairs, maxPairs },
    If[ Length[ segment ] < 2, Return[ { segment } ] ];
    p1 = First[ segment ];
    p2 = Last[ segment ];
    d = GraphDistance[ graph, p1, p2 ];
    extendBefore = Select[ VertexList[ graph ],
      candidate |-> GraphDistance[ graph, candidate, p1 ] + d == GraphDistance[ graph, candidate, p2 ]
    ];
    extendAfter = Select[ VertexList[ graph ],
      candidate |-> GraphDistance[ graph, candidate, p2 ] + d == GraphDistance[ graph, p1, candidate ]
    ];
    pairs = Tuples[ { extendBefore, extendAfter } ];
    maxPairs = MaximalBy[ pairs, GraphDistance[ graph, #[[ 1 ]], #[[ 2 ]] ] & ];
    If[ maxPairs === { { p1, p2 } }, Return[ { segment } ] ];
    Flatten[
      With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
        With[ { db = GraphDistance[ graph, s, p1 ], da = GraphDistance[ graph, p2, e ] },
          With[ { bp = If[ db == 0, { {} }, Most /@ prefixFn[ s, p1, db ] ],
                  ap = If[ da == 0, { {} }, Rest /@ suffixFn[ p2, e, da ] ] },
            Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ]
          ]
        ]
      ] & /@ maxPairs,
      1
    ]
  ]


(* ===================== Extend segment ===================== *)

(* ExtendSegment[g, segment] takes a vertex sequence and extends it to a
   line: a maximal vertex sequence containing segment as a contiguous
   sub-sequence, every prefix / suffix of which is itself a path (geodesic
   for "Shortest", recency-lex-certified for "ShortestPathExtension", curvature-pulled
   for "CurvatureMinimizing", embedding-line-aligned for "Embedding"). The Method axis
   matches FindSegment / FindLine: every method is a path-enumeration
   strategy on the prefix and suffix; the candidate before / after vertices
   are selected by the geodesic relation
     d(s, p1) + d(p1, p2) == d(s, p2),  d(p2, e) + d(p1, p2) == d(p1, e)
   so the line's outer endpoints are graph-distance-maximal from each
   other regardless of method. *)

ExtendSegment::badmethod = "Method `1` is not supported by ExtendSegment.";
ExtendSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
ExtendSegment::badwindow = "ShortestPathWindow specification `1` is not supported; use a positive integer or All.";
ExtendSegment::badcurvature = "Curvature specification `1` is not supported; use \"Forman\", \"Wolfram\", {\"Forman\", Method -> \"Simple\" | \"Triangles\"}, or {\"Wolfram\", \"Dimension\" -> Automatic | d, \"Radii\" -> Automatic | {rmin, rmax}}.";

Options[ ExtendSegment ] = { Method -> "Shortest" };

(* ExtendSegment public dispatcher: spreads multi-realisation segment input
   (InfraSegment[{seg1, seg2, ...}]) over each realisation, runs the
   single-segment core for each, unions the bare-list results, and wraps as
   InfraSegment[...].  Strict-n shortfall on any input segment propagates as
   $Failed.                                                                  *)

ExtendSegment[ graph_Graph, segment_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    extendSegmentCore[ graph, ##, count, opts ] &, segment ]


(* extendSegmentCore is the single-segment body. *)

extendSegmentCore[ graph_Graph, segment_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ExtendSegment ] ] :=
  Module[ { spec, methodName, prune, shortestPathWindow, curvature, curvatureSpec, edgeKappa, paths },
    If[ Length[ segment ] < 2, Return[ { segment } ] ];
    spec = OptionValue[ ExtendSegment, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, Automatic -> "Shortest", _ :> spec } ];
    { prune, shortestPathWindow, curvature } = Replace[ spec, {
      _String :> { Infinity, 2, "Forman" },
      { _String, subOpts___ } :> {
        "Pruning"            /. { subOpts } /. "Pruning"            -> Infinity,
        "ShortestPathWindow" /. { subOpts } /. "ShortestPathWindow" -> 2,
        "Curvature"          /. { subOpts } /. "Curvature"          -> "Forman"
      },
      _ :> { Infinity, 2, "Forman" }
    } ];
    paths = Switch[ methodName,
      "Shortest",
        findLineExtensions[ graph, segment ],
      "ShortestPathExtension",
        Which[
          ! pruningSpecQ[ prune ],
            Message[ ExtendSegment::badpruning, prune ]; $Failed,
          ! shortestPathWindowSpecQ[ shortestPathWindow ],
            Message[ ExtendSegment::badwindow, shortestPathWindow ]; $Failed,
          True,
            findLineExtensionsWith[ graph, segment,
              { s, p, db } |-> extendedOutPaths[ graph, s, p, prune, shortestPathWindow, Infinity ],
              { p, e, da } |-> extendedOutPaths[ graph, p, e, prune, shortestPathWindow, Infinity ] ]
        ],
      "CurvatureMinimizing",
        curvatureSpec = parseCurvatureSpec[ curvature ];
        Which[
          curvatureSpec === $Failed,
            Message[ ExtendSegment::badcurvature, curvature ]; $Failed,
          ! pruningSpecQ[ prune ],
            Message[ ExtendSegment::badpruning, prune ]; $Failed,
          True,
            edgeKappa = buildEdgeKappa[ graph, curvatureSpec ];
            findLineExtensionsWith[ graph, segment,
              { s, p, db } |-> pulledPaths[ graph, s, p, edgeKappa, prune, Infinity, Automatic ],
              { p, e, da } |-> pulledPaths[ graph, p, e, edgeKappa, prune, Infinity, Automatic ] ]
        ],
      "Embedding",
        embeddingExtendSegment[ graph, segment, parseEmbeddingMethod[ spec ] ],
      _, Message[ ExtendSegment::badmethod, spec ]; $Failed
    ];
    Which[
      paths === $Failed, $Failed,
      MatchQ[ count, _Integer ],
        With[ { result = Take[ paths, UpTo[ count ] ] },
          If[ Length[ result ] < count, $Failed, result ] ],
      MatchQ[ count, UpTo[ _Integer ] ], Take[ paths, count ],
      count === All, paths,
      True, paths
    ]
  ]


(* embeddingExtendSegment fits a regression line through the segment's
   embedding coordinates (principal direction obtained from the SVD of the
   centered point cloud, falling back to the endpoint-to-endpoint vector
   when the segment is short or coplanar) and greedily walks outward from
   each endpoint along that direction in the graph: at every step the
   neighbour is chosen by maximum signed projection along the line, with
   minimum perpendicular distance as the tie-breaker. The walk stops when
   no unvisited neighbour advances the projection further. *)

embeddingExtendSegment[ graph_Graph, segment_List, embOpts_Association ] :=
  Module[ { coords, vertexIndex, segIdx, segCoords, centroid, centered, direction, basePoint, signedProj, perpDist, walkOut, walkIn, prefix, suffix },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    segIdx = Lookup[ vertexIndex, segment ];
    segCoords = coords[[ segIdx ]];
    centroid = Mean[ segCoords ];
    centered = ( # - centroid ) & /@ segCoords;
    direction = With[ { diff = Last[ segCoords ] - First[ segCoords ] },
      Which[
        Length[ segCoords ] >= 3 && Norm[ diff ] > 0,
          With[ { svd = SingularValueDecomposition[ centered ] },
            Normalize @ svd[[ 3, All, 1 ]] * Sign[ diff . svd[[ 3, All, 1 ]] ] ],
        Norm[ diff ] > 0, Normalize[ diff ],
        True, ConstantArray[ 0., Length @ First @ segCoords ]
      ] ];
    If[ Norm[ direction ] == 0, Return[ { segment } ] ];
    basePoint = First[ segCoords ];
    signedProj = v |-> ( coords[[ vertexIndex[ v ] ]] - basePoint ) . direction;
    perpDist = v |-> With[ { c = coords[[ vertexIndex[ v ] ]] - basePoint },
      Norm[ c - ( c . direction ) direction ] ];
    walkOut[ current_, visited_ ] :=
      With[ { adj = Complement[ AdjacencyList[ graph, current ], visited ],
              currentP = signedProj[ current ] },
        With[ { candidates = Select[ adj, signedProj[ # ] > currentP & ] },
          If[ candidates === { }, { },
            With[ { best = First @ MinimalBy[
                MaximalBy[ candidates, signedProj ], perpDist ] },
              Prepend[ walkOut[ best, Append[ visited, best ] ], best ] ] ] ] ];
    walkIn[ current_, visited_ ] :=
      With[ { adj = Complement[ AdjacencyList[ graph, current ], visited ],
              currentP = signedProj[ current ] },
        With[ { candidates = Select[ adj, signedProj[ # ] < currentP & ] },
          If[ candidates === { }, { },
            With[ { best = First @ MinimalBy[
                MinimalBy[ candidates, signedProj ], perpDist ] },
              Prepend[ walkIn[ best, Append[ visited, best ] ], best ] ] ] ] ];
    prefix = walkIn[ First[ segment ], segment ];
    suffix = walkOut[ Last[ segment ], segment ];
    { Join[ Reverse[ prefix ], segment, suffix ] }
  ]


(* ===================== Shells ===================== *)

(* A shell of radius r around c is a vertex set carved out of the level
   surface { v : d(c, v) = r }.  Three recipes:
   "Metric" (default) returns the level surface itself as a singleton;
   "Separating" returns connected subsets of the level surface whose
   removal disconnects c from { v : d(c, v) > r }, kept minimal under
   inclusion;
   "Embedding" returns single-vertex sets ranked by how close each vertex
   is to the Euclidean sphere of radius r around c under the graph
   embedding, i.e. by | |coord(v) - coord(c)| - r |.
   The cyclic case (separating cycles, the 2D-style spheres) has its
   own head, FindCircle. *)

FindShell::badmethod = "Method `1` is not supported by FindShell.";

Options[ FindShell ] = { Method -> "Metric" };

FindShell[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraShell, count,
    findShellCore[ graph, ##, count, opts ] &, p, r ]


findShellCore[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindShell ] ] :=
  Module[ { spec, methodName, range, levelSet, radius, shells },
    spec = OptionValue[ FindShell, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    shells = Switch[ methodName,
      "Metric",     { levelSet },
      "Separating", FindMinimalSeparatingSubgraphs[ graph, levelSet, p, radius ],
      "Embedding",  embeddingRankShellVertices[ graph, p, radius, levelSet, parseEmbeddingMethod[ spec, "LevelSet" ] ],
      _,            Message[ FindShell::badmethod, spec ]; $Failed
    ];
    Which[
      shells === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ shells ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ shells, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ shells, count ],
      count === All,                       shells,
      True,                                shells
    ]
  ]


(* embeddingRankShellVertices returns single-vertex sets {v}, ordered by
   how close v is to the Euclidean sphere of radius r centred at coord(p):
   score = | |coord(v) - coord(p)| - r |.  Pool "LevelSet" restricts to
   vertices on the metric level surface; "AllVertices" ranks every vertex. *)

embeddingRankShellVertices[ graph_Graph, p_, radius_, levelSet_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, centerPt, pool },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerPt = coords[[ vertexIndex[ p ] ]];
    pool = If[ embOpts[ "Pool" ] === "AllVertices", VertexList[ graph ], levelSet ];
    List /@ SortBy[ pool,
      v |-> Abs[ EuclideanDistance[ coords[[ vertexIndex[ v ] ]], centerPt ] - radius ] ]
  ]


(* ===================== Circles ===================== *)

(* A circle of radius r around c is a separating cycle in the level-surface
   subgraph: a cyclic vertex sequence whose removal disconnects c from
   { v : d(c, v) > r }.  Returns open vertex sequences { v0, v1, ..., vk }
   (rotation-invariant via path-space selectors); the wrap-around edge
   { vk, v0 } is implicit.  Method -> "Embedding" ranks the separating
   cycles by EmbeddingCircleDistance to the Euclidean circle of radius r
   centred at coord(p) under the graph drawing. *)

FindCircle::badmethod = "Method `1` is not supported by FindCircle.";
FindCircle::nyi = "Pool `1` is not yet implemented for FindCircle; use \"LevelSet\".";

Options[ FindCircle ] = { Method -> "Metric" };

FindCircle[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraCircle, count,
    findCircleCore[ graph, ##, count, opts ] &, p, r ]


findCircleCore[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindCircle ] ] :=
  Module[ { range, levelSet, radius, levelGraph, allCycles, vertexCycles, separating, spec, methodName, ranked },
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    levelGraph = Subgraph[ graph, levelSet ];
    allCycles = FindCycle[ levelGraph, Infinity, All ];
    separating = If[ allCycles === {}, {},
      vertexCycles = (First /@ #) & /@ allCycles;
      FindSeparatingCycles[ graph, vertexCycles, p, radius ]
    ];
    spec = OptionValue[ FindCircle, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    ranked = Switch[ methodName,
      "Metric", separating,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec, "LevelSet" ] },
          If[ embOpts[ "Pool" ] =!= "LevelSet",
            Message[ FindCircle::nyi, embOpts[ "Pool" ] ]; separating,
            embeddingRankCircles[ graph, separating, p, radius, embOpts ]
          ]
        ],
      _, Message[ FindCircle::badmethod, spec ]; $Failed
    ];
    Which[
      ranked === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ ranked ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ ranked, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ ranked, count ],
      count === All,                       ranked,
      True,                                ranked
    ]
  ]


(* embeddingRankCircles sorts separating cycles by their Hausdorff distance
   to the Euclidean circle of radius r centred at coord(center) under the
   supplied embedding. *)

embeddingRankCircles[ graph_Graph, cycles_List, center_, radius_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, centerIdx },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerIdx = vertexIndex[ center ];
    SortBy[ cycles,
      cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
  ]


(* ===================== Parallels ===================== *)

(* FindParallel[g, line, p] constructs parallels to line through p.
   Method -> "Metric" (default): a parallel is the maximal sub-segment of a
   maximal geodesic through p whose vertices all lie at distance
   r = d(p, line) from line -- the local portion of a line-through-p that
   stays on the distance-to-line level surface.  No perpendiculars, no
   auxiliary segments. *)

FindParallel::badmethod = "Method `1` is not supported by FindParallel.";

Options[ FindParallel ] = { Method -> "Metric" };

FindParallel[ graph_Graph, line_, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findParallelCore[ graph, ##, count, opts ] &, line, p ]


findParallelCore[ graph_Graph, line_List, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindParallel ] ] :=
  Module[ { spec, methodName, parallels },
    spec = OptionValue[ FindParallel, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    parallels = Switch[ methodName,
      "Metric", findParallelMetric[ graph, line, p ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec, "LevelSet" ] },
          embeddingRankParallels[ graph, findParallelMetric[ graph, line, p ], line, p, embOpts ]
        ],
      _, Message[ FindParallel::badmethod, methodName ]; $Failed
    ];
    Which[
      parallels === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ parallels ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ parallels, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ parallels, count ],
      count === All,                       parallels,
      True,                                parallels
    ]
  ]


findParallelMetric[ graph_Graph, line_List, p_ ] :=
  Module[ { lineDist, r, levelSet, segments, dedup, maximalThrough },
    maximalThrough = { l, q, S } |-> With[
      { idx = First[ FirstPosition[ l, q, { 0 } ], 0 ] },
      If[ idx === 0, {},
        With[
          { lo = idx - LengthWhile[ Reverse @ Take[ l, idx - 1 ], MemberQ[ S, # ] & ],
            hi = idx + LengthWhile[ Drop[ l, idx ], MemberQ[ S, # ] & ] },
          l[[ lo ;; hi ]]
        ]
      ]
    ];
    lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ];
    r = lineDist[ p ];
    If[ r === Infinity, Return[ {} ] ];
    levelSet = Select[ VertexList[ graph ], lineDist[ # ] == r & ];
    segments = maximalThrough[ #, p, levelSet ] & /@ PencilDirections[ graph, p ];
    dedup = DeleteDuplicates[ canonicalLine /@ Select[ segments, Length[ # ] >= 2 & ] ];
    Select[ dedup,
      a |-> ! AnyTrue[ dedup, b |-> Length[ b ] > Length[ a ] && SubsetQ[ b, a ] ]
    ]
  ]


(* embeddingRankParallels sorts parallel candidates by Hausdorff distance
   to the Euclidean line that goes through coord(p) parallel to the line
   coord(line[[1]]) -> coord(line[[-1]]).  Implemented by translating the
   reference segment so it passes through p and using
   EmbeddingHausdorffDistance against virtual endpoint indices appended to
   the coordinate matrix. *)

embeddingRankParallels[ graph_Graph, parallels_List, line_List, p_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, lineDir, pCoord, refStart, refEnd, augmented, n },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    lineDir = coords[[ vertexIndex[ Last[ line ] ] ]] - coords[[ vertexIndex[ First[ line ] ] ]];
    pCoord = coords[[ vertexIndex[ p ] ]];
    refStart = pCoord - lineDir / 2;
    refEnd   = pCoord + lineDir / 2;
    augmented = Append[ Append[ coords, refStart ], refEnd ];
    n = Length[ coords ];
    SortBy[ parallels,
      par |-> EmbeddingHausdorffDistance[ augmented,
        Lookup[ vertexIndex, par ], { n + 1, n + 2 } ] ]
  ]

