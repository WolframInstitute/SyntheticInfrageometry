Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]


(* ===================== Messages ===================== *)

FindSegment::badmethod = "Method `1` is not supported by FindSegment.";
FindSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
FindSegment::badlookback = "Lookback specification `1` is not supported; use a positive integer or All.";
FindSegment::badconstraint = "Constraint specification `1` is not supported; use \"Geodesic\" or \"Free\".";
FindSegment::badforman = "FormanMethod specification `1` is not supported; use \"Simple\" or \"Triangles\".";


(* ===================== Points ===================== *)

(* FindPoint[g, n] returns n vertices of the graph (the existence postulate
   for points).  With "Distance" -> r the n vertices form a clique in the
   r-distance graph (mutually at least r apart), realising "n points spread
   out by r"; with "From" the candidate pool is restricted (Center, Periphery,
   a vertex list, a single vertex, or origin -> dist where dist is a number,
   {dMin, dMax} range, or "Max" -- vertices at that graph distance from origin). *)

Options[ FindPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { from, pool, dist, range, distMatrix, vertexIndex, auxiliaryGraph, cliques, thresholds, finiteMax, maxCl },
    from = OptionValue[ "From" ];
    pool = Which[
      StringQ[ from ] && from == "Center", GraphCenter[ graph ],
      StringQ[ from ] && from == "Periphery", GraphPeriphery[ graph ],
      StringQ[ from ], VertexList[ graph ],
      MemberQ[ VertexList[ graph ], from ], { from },
      MatchQ[ from, _ -> (_?NumericQ | { _?NumericQ, _?NumericQ } | "Max") ] && MemberQ[ VertexList[ graph ], First[ from ] ],
        With[ { allDists = GraphDistance[ graph, First[ from ] ] },
          With[ { spec = Last[ from ] /. "Max" :> Max[ Select[ allDists, # < Infinity & ] ] },
            Pick[ VertexList[ graph ],
              Replace[ spec, { d_?NumericQ :> Map[ # == d &, allDists ], { lo_, hi_ } :> Map[ lo <= # <= hi &, allDists ] } ] ]
          ]
        ],
      ListQ[ from ], from,
      True, VertexList[ graph ]
    ];
    dist = OptionValue[ "Distance" ];
    maxCl = OptionValue[ "MaxCliques" ];
    If[ n == 1 || dist === None,
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
    ]
  ]

FindPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindPoint[ graph, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== Segments ===================== *)

(* A segment between p1 and p2 is a geodesic vertex sequence
   (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2) and consecutive vi
   adjacent.  Method -> "Shortest" (default) enumerates geodesics:
   count = 1 takes the built-in FindShortestPath; count > 1 (or UpTo[n]
   / All) enumerates via FindPath at exact length d, and the result
   pipes through CentralPaths / EmbeddingClosestPaths / ... for further
   path-space filtering.  Method -> "Stretched" (or {"Stretched",
   "Lookback" -> k, "Pruning" -> spec}) enumerates simple paths from p1
   to p2 via a constructive frontier sweep that admits a step
   v_i -> w iff w is unvisited and the recency-lex distance tuple
   ( d(v_{i-1}, w), ..., d(v_{i-K+1}, w) ) is maximal among unvisited
   neighbours.  K = 1 has empty tuple (no filter, all simple paths),
   K = 2 (default) excludes triangle shortcuts, K = All compares
   against every available predecessor and on most graphs collapses to
   geodesics.  Pruning spec is Infinity (default), a positive integer
   beam width, or a Bernoulli keep probability.  Method -> "Pulled"
   (or {"Pulled", "FormanMethod" -> "Simple" | "Triangles"}) traces the
   rope being pulled tight from p1 toward p2 by a frontier sweep that
   admits step v_i -> w iff w is unvisited and minimises the
   Forman-Ricci edge curvature F(v_i, w) among unvisited neighbours of
   v_i.  Ties branch the frontier; the result is every walk whose
   every step is curvature-minimal in its candidate set. *)

Options[ FindSegment ] = { Method -> "Shortest" };

FindSegment[ graph_Graph, p1_, p2_,
    count : (_Integer | UpTo[ _Integer ] | All) : 1, opts : OptionsPattern[] ] :=
  Module[ { spec = OptionValue[ Method ], methodName, prune, lookback, formanMethod, d, paths },
    If[ p1 === p2, Return[ { } ] ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    { prune, lookback, formanMethod } = Replace[ spec, {
      _String :> { Infinity, 2, "Simple" },
      { _String, subOpts___ } :> {
        "Pruning"      /. { subOpts } /. "Pruning"      -> Infinity,
        "Lookback"     /. { subOpts } /. "Lookback"     -> 2,
        "FormanMethod" /. { subOpts } /. "FormanMethod" -> "Simple"
      },
      _ :> { Infinity, 2, "Simple" }
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
      "Stretched",
        Which[
          ! pruningSpecQ[ prune ],
            Message[ FindSegment::badpruning, prune ]; $Failed,
          ! lookbackSpecQ[ lookback ],
            Message[ FindSegment::badlookback, lookback ]; $Failed,
          True,
            paths = stretchedOutPaths[ graph, p1, p2, prune, lookback, countLimit[ count ] ];
            If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ]
        ],
      "Pulled",
        Which[
          ! formanMethodSpecQ[ formanMethod ],
            Message[ FindSegment::badforman, formanMethod ]; $Failed,
          ! pruningSpecQ[ prune ],
            Message[ FindSegment::badpruning, prune ]; $Failed,
          True,
            paths = pulledPaths[ graph, p1, p2, formanMethod, prune, countLimit[ count ] ];
            If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ]
        ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          Which[
            ! pruningSpecQ[ embOpts[ "Pruning" ] ],
              Message[ FindSegment::badpruning, embOpts[ "Pruning" ] ]; $Failed,
            ! MemberQ[ { "Geodesic", "Free" }, embOpts[ "Constraint" ] ],
              Message[ FindSegment::badconstraint, embOpts[ "Constraint" ] ]; $Failed,
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


(* embeddingFindSegmentPaths returns paths from p1 to p2 sorted by their
   embedding-Hausdorff distance to the straight Euclidean segment p1 p2.
   Constraint "Geodesic" recurses through the geodesic DAG; "Free"
   recurses through the whole graph along simple-path extensions. *)

embeddingFindSegmentPaths[ graph_Graph, p1_, p2_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, dagNbrs, extendFn, prune, paths, ep },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    prune = embOpts[ "Pruning" ];
    extendFn = If[ embOpts[ "Constraint" ] === "Geodesic",
      dagNbrs = geodesicDAGNeighbors[ graph, p1, p2 ];
      ( path |-> Lookup[ dagNbrs, Last[ path ], { } ] ),
      ( path |-> Complement[ AdjacencyList[ graph, Last[ path ] ], path ] )
    ];
    paths = generateEmbeddingPaths[ extendFn, { p1 }, ( Last[ # ] === p2 & ), prune ];
    ep = Lookup[ vertexIndex, { p1, p2 } ];
    SortBy[ paths,
      path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
  ]

FindSegment[ graph_Graph, { p1_, p2_ }, args___ ] :=
  FindSegment[ graph, p1, p2, args ]


(* ===================== Lines ===================== *)

(* A line through p1 and p2 is a maximal geodesic extension: a vertex
   sequence (a, ..., p1, ..., p2, ..., b) every contiguous sub-sequence of
   which is a geodesic and that cannot be extended at either end without
   breaking the geodesic property.  FindLine enumerates such maximal
   extensions; "Maximality" -> "Diameter" further restricts to those whose
   length equals GraphDiameter[g].  Method -> "Embedding" ranks the
   extensions by EmbeddingHausdorffDistance to the infinite Euclidean line
   through p1 and p2 under the graph drawing. *)

FindLine::badmethod = "Method `1` is not supported by FindLine.";
FindLine::nyi = "Constraint `1` is not yet implemented for FindLine; use \"Geodesic\".";

Options[ FindLine ] = { "Maximality" -> "Extension", Method -> Automatic };

FindLine[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { geodesics, allExtensions, diam, spec, methodName },
    geodesics = allGeodesics[ graph, p1, p2 ];
    allExtensions = Union @ Flatten[
      findLineExtensions[ graph, # ] & /@ geodesics, 1 ];
    If[ OptionValue[ "Maximality" ] === "Diameter",
      diam = GraphDiameter[ graph ];
      allExtensions = Select[ allExtensions, line |-> Length[ line ] - 1 == diam ]
    ];
    spec = OptionValue[ Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    Switch[ methodName,
      Automatic, allExtensions,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          If[ embOpts[ "Constraint" ] =!= "Geodesic",
            Message[ FindLine::nyi, embOpts[ "Constraint" ] ]; allExtensions,
            embeddingRankLines[ graph, allExtensions, p1, p2, embOpts ]
          ]
        ],
      _, Message[ FindLine::badmethod, spec ]; $Failed
    ]
  ]


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

FindLine[ graph_Graph, p1_, p2_, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Take[ FindLine[ graph, p1, p2, All, opts ], UpTo[ n ] ]

FindLine[ graph_Graph, p1_, p2_, n_Integer : 1, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  With[ { result = FindLine[ graph, p1, p2, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]

findLineExtensions[ graph_Graph, segment_List ] :=
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
          With[ { bp = If[ db == 0, { {} }, Most /@ FindPath[ graph, s, p1, { db }, All ] ],
                  ap = If[ da == 0, { {} }, Rest /@ FindPath[ graph, p2, e, { da }, All ] ] },
            Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ]
          ]
        ]
      ] & /@ maxPairs,
      1
    ]
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

FindShell[ graph_Graph, p_, r_, All, opts : OptionsPattern[] ] :=
  Module[ { spec, methodName, range, levelSet, radius },
    spec = OptionValue[ Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    Switch[ methodName,
      "Metric",     { levelSet },
      "Separating", FindMinimalSeparatingSubgraphs[ graph, levelSet, p, radius ],
      "Embedding",  embeddingRankShellVertices[ graph, p, radius, levelSet, parseEmbeddingMethod[ spec ] ],
      _,            Message[ FindShell::badmethod, spec ]; $Failed
    ]
  ]


(* embeddingRankShellVertices returns single-vertex sets {v}, ordered by
   how close v is to the Euclidean sphere of radius r centred at coord(p):
   score = | |coord(v) - coord(p)| - r |.  Constraint "Geodesic" restricts
   to vertices on the metric level surface; "Free" ranks every vertex. *)

embeddingRankShellVertices[ graph_Graph, p_, radius_, levelSet_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, centerPt, pool },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerPt = coords[[ vertexIndex[ p ] ]];
    pool = If[ embOpts[ "Constraint" ] === "Free", VertexList[ graph ], levelSet ];
    List /@ SortBy[ pool,
      v |-> Abs[ EuclideanDistance[ coords[[ vertexIndex[ v ] ]], centerPt ] - radius ] ]
  ]

FindShell[ graph_Graph, p_, r_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindShell[ graph, p, r, All, opts ], UpTo[ n ] ]

FindShell[ graph_Graph, p_, r_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindShell[ graph, p, r, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
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
FindCircle::nyi = "Constraint `1` is not yet implemented for FindCircle; use \"Geodesic\".";

Options[ FindCircle ] = { Method -> "Metric" };

FindCircle[ graph_Graph, p_, r_, All, opts : OptionsPattern[] ] :=
  Module[ { range, levelSet, radius, levelGraph, allCycles, vertexCycles, separating, spec, methodName },
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
    spec = OptionValue[ Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    Switch[ methodName,
      "Metric", separating,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          If[ embOpts[ "Constraint" ] =!= "Geodesic",
            Message[ FindCircle::nyi, embOpts[ "Constraint" ] ]; separating,
            embeddingRankCircles[ graph, separating, p, radius, embOpts ]
          ]
        ],
      _, Message[ FindCircle::badmethod, spec ]; $Failed
    ]
  ]

FindCircle[ graph_Graph, p_, r_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindCircle[ graph, p, r, All, opts ], UpTo[ n ] ]

FindCircle[ graph_Graph, p_, r_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindCircle[ graph, p, r, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
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
   auxiliary segments.  "Spectral" and "Resistance" are accepted by the
   option but not yet implemented. *)

FindParallel::nyi = "Method `1` is not yet implemented for FindParallel; only \"Metric\" is currently available.";
FindParallel::badmethod = "Method `1` is not supported by FindParallel.";

Options[ FindParallel ] = { Method -> "Metric" };

FindParallel[ graph_Graph, line_List, p_, All, opts : OptionsPattern[] ] :=
  Module[ { spec = OptionValue[ Method ], methodName },
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    Switch[ methodName,
      "Metric", findParallelMetric[ graph, line, p ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          embeddingRankParallels[ graph, findParallelMetric[ graph, line, p ], line, p, embOpts ]
        ],
      "Spectral" | "Resistance", Message[ FindParallel::nyi, methodName ]; $Failed,
      _, Message[ FindParallel::badmethod, methodName ]; $Failed
    ]
  ]


findParallelMetric[ graph_Graph, line_List, p_ ] :=
  Module[ { lineDist, r, levelSet, linesThroughP, segments, dedup, maximalThrough },
    maximalThrough = Function[ { l, q, S },
      Module[ { idx = First @ FirstPosition[ l, q, { 0 } ], lo, hi },
        If[ idx == 0, Return[ {} ] ];
        lo = idx; hi = idx;
        While[ lo > 1 && MemberQ[ S, l[[ lo - 1 ]] ], lo-- ];
        While[ hi < Length[ l ] && MemberQ[ S, l[[ hi + 1 ]] ], hi++ ];
        l[[ lo ;; hi ]]
      ]
    ];
    lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ];
    r = lineDist[ p ];
    If[ r === Infinity, Return[ {} ] ];
    levelSet = Select[ VertexList[ graph ], lineDist[ # ] == r & ];
    linesThroughP = Keys @ FindPencil[ graph, p ];
    segments = maximalThrough[ #, p, levelSet ] & /@ linesThroughP;
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

FindParallel[ graph_Graph, line_List, p_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  With[ { result = FindParallel[ graph, line, p, All, opts ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

FindParallel[ graph_Graph, line_List, p_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindParallel[ graph, line, p, UpTo[ n ], opts ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
  ]
