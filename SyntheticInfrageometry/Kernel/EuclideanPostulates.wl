Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]


(* ===================== Points ===================== *)

(* FindPoint[g, n] returns n vertices of the graph (the existence postulate
   for points).  With "Distance" -> r the n vertices form a clique in the
   r-distance graph (mutually at least r apart), realising "n points spread
   out by r"; with "From" the candidate pool is restricted (Center, Periphery,
   a vertex list, or a single vertex). *)

Options[ FindPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { from, pool, dist, range, distMatrix, vertexIndex, auxiliaryGraph, cliques, thresholds, finiteMax, maxCl },
    from = OptionValue[ "From" ];
    pool = Which[
      StringQ[ from ] && from == "Center", GraphCenter[ graph ],
      StringQ[ from ] && from == "Periphery", GraphPeriphery[ graph ],
      StringQ[ from ], VertexList[ graph ],
      MemberQ[ VertexList[ graph ], from ], { from },
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
   adjacent.  Count = 1 takes the built-in FindShortestPath; count > 1
   (or UpTo[n] / All) enumerates via FindPath at exact length d.  Pipe
   the result through CentralPaths / EmbeddingClosestPaths / ... to
   filter by a path-space metric. *)

FindSegment[ graph_Graph, p1_, p2_,
    count : (_Integer | UpTo[ _Integer ] | All) : 1 ] :=
  Module[ { d, paths },
    If[ p1 === p2, Return[ { } ] ];
    paths = If[ count === 1,
      With[ { path = FindShortestPath[ graph, p1, p2 ] },
        If[ path === { }, { }, { path } ]
      ],
      d = GraphDistance[ graph, p1, p2 ];
      If[ d === Infinity, { },
        FindPath[ graph, p1, p2, { d }, count /. UpTo[ k_ ] :> k ]
      ]
    ];
    If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ]
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


(* ===================== Spheres ===================== *)

(* A sphere of radius r around c is a vertex set carved out of the level
   surface { v : d(c, v) = r }.  Three graded recipes are offered:
   "Metric" (default) returns the level surface itself; "SeparatingGraph"
   returns connected subsets of the level surface whose removal disconnects
   c from { v : d(c, v) > r }, kept minimal under inclusion;
   "SeparatingCycle" further restricts to those that form a cycle in the
   level-surface subgraph (the 2D-style spheres). *)

Options[ FindSphere ] = { Method -> "Metric" };

FindSphere[ graph_Graph, p_, r_, All, opts : OptionsPattern[] ] :=
  Module[ { method, range, levelSet, radius, levelGraph, allCycles, vertexCycles },
    method = OptionValue[ Method ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    Switch[ method,
      "Metric", { levelSet },
      "SeparatingGraph",
        FindMinimalSeparatingSubgraphs[ graph, levelSet, p, radius ],
      "SeparatingCycle",
        levelGraph = Subgraph[ graph, levelSet ];
        allCycles = FindCycle[ levelGraph, Infinity, All ];
        If[ allCycles === {}, Return[ {} ] ];
        vertexCycles = (First /@ #) & /@ allCycles;
        FindSeparatingCycles[ graph, vertexCycles, p, radius ],
      _, $Failed
    ]
  ]

FindSphere[ graph_Graph, p_, r_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindSphere[ graph, p, r, All, opts ], UpTo[ n ] ]

FindSphere[ graph_Graph, p_, r_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindSphere[ graph, p, r, UpTo[ n ], opts ] },
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
