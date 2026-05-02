Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]


(* ===================== Messages ===================== *)

FindSegment::badmethod = "Method `1` is not supported by FindSegment.";
FindSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
FindSegment::badlookback = "Lookback specification `1` is not supported; use a positive integer or All.";


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
   beam width, or a Bernoulli keep probability. *)

Options[ FindSegment ] = { Method -> "Shortest" };

FindSegment[ graph_Graph, p1_, p2_,
    count : (_Integer | UpTo[ _Integer ] | All) : 1, opts : OptionsPattern[] ] :=
  Module[ { spec = OptionValue[ Method ], methodName, prune, lookback, d, paths },
    If[ p1 === p2, Return[ { } ] ];
    { methodName, prune, lookback } = Replace[ spec, {
      m_String :> { m, Infinity, 2 },
      { m_String, subOpts___ } :> {
        m,
        "Pruning"  /. { subOpts } /. "Pruning"  -> Infinity,
        "Lookback" /. { subOpts } /. "Lookback" -> 2
      },
      _ :> { spec, Infinity, 2 }
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
            paths = stretchedOutPaths[ graph, p1, p2, prune, lookback ];
            With[ { result = takeUpTo[ paths, countLimit[ count ] ] },
              If[ MatchQ[ count, _Integer ] && Length[ result ] < count, $Failed, result ]
            ]
        ],
      _, Message[ FindSegment::badmethod, spec ]; $Failed
    ]
  ]

FindSegment[ graph_Graph, { p1_, p2_ }, args___ ] :=
  FindSegment[ graph, p1, p2, args ]


(* ===================== Lines ===================== *)

(* A line through p1 and p2 is a maximal geodesic extension: a vertex
   sequence (a, ..., p1, ..., p2, ..., b) every contiguous sub-sequence of
   which is a geodesic and that cannot be extended at either end without
   breaking the geodesic property.  FindLine enumerates such maximal
   extensions; "Maximality" -> "Diameter" further restricts to those whose
   length equals GraphDiameter[g]. *)

Options[ FindLine ] = { "Maximality" -> "Extension" };

FindLine[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { geodesics, allExtensions, diam },
    geodesics = allGeodesics[ graph, p1, p2 ];
    allExtensions = Union @ Flatten[
      findLineExtensions[ graph, # ] & /@ geodesics, 1 ];
    If[ OptionValue[ "Maximality" ] === "Diameter",
      diam = GraphDiameter[ graph ];
      allExtensions = Select[ allExtensions, line |-> Length[ line ] - 1 == diam ]
    ];
    allExtensions
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
   surface { v : d(c, v) = r }.  Two recipes:
   "Metric" (default) returns the level surface itself as a singleton;
   "Separating" returns connected subsets of the level surface whose
   removal disconnects c from { v : d(c, v) > r }, kept minimal under
   inclusion.  The cyclic case (separating cycles, the 2D-style spheres)
   has its own head, FindCircle, since cycles are vertex sequences rather
   than vertex sets. *)

Options[ FindShell ] = { Method -> "Metric" };

FindShell[ graph_Graph, p_, r_, All, opts : OptionsPattern[] ] :=
  Module[ { method, range, levelSet, radius },
    method = OptionValue[ Method ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    Switch[ method,
      "Metric",     { levelSet },
      "Separating", FindMinimalSeparatingSubgraphs[ graph, levelSet, p, radius ],
      _,            $Failed
    ]
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
   { vk, v0 } is implicit. *)

FindCircle[ graph_Graph, p_, r_, All ] :=
  Module[ { range, levelSet, radius, levelGraph, allCycles, vertexCycles },
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    levelGraph = Subgraph[ graph, levelSet ];
    allCycles = FindCycle[ levelGraph, Infinity, All ];
    If[ allCycles === {}, Return[ {} ] ];
    vertexCycles = (First /@ #) & /@ allCycles;
    FindSeparatingCycles[ graph, vertexCycles, p, radius ]
  ]

FindCircle[ graph_Graph, p_, r_, UpTo[ n_Integer ] ] :=
  Take[ FindCircle[ graph, p, r, All ], UpTo[ n ] ]

FindCircle[ graph_Graph, p_, r_, n_Integer : 1 ] :=
  With[ { result = FindCircle[ graph, p, r, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
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
  Module[ { method = OptionValue[ Method ], lineDist, r, levelSet, linesThroughP, segments, dedup, maximalThrough },
    Switch[ method,
      "Metric",
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
        ],
      "Spectral" | "Resistance", Message[ FindParallel::nyi, method ]; $Failed,
      _, Message[ FindParallel::badmethod, method ]; $Failed
    ]
  ]

FindParallel[ graph_Graph, line_List, p_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  With[ { result = FindParallel[ graph, line, p, All, opts ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

FindParallel[ graph_Graph, line_List, p_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindParallel[ graph, line, p, UpTo[ n ], opts ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
  ]
