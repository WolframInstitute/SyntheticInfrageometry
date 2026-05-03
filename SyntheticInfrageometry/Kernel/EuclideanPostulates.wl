Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]


(* ===================== Messages ===================== *)

FindSegment::badmethod = "Method `1` is not supported by FindSegment.";
FindSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
FindSegment::badlookback = "Lookback specification `1` is not supported; use a positive integer or All.";
FindSegment::badconstraint = "Constraint specification `1` is not supported; use \"Geodesic\" or \"Free\".";
FindSegment::badforman = "FormanMethod specification `1` is not supported; use \"Simple\" or \"Triangles\".";
FindSegment::badcurvature = "CurvatureMethod specification `1` is not supported; use \"Forman\" or \"Wolfram\".";
FindSegment::baddim = "Dimension specification `1` is not supported; use Automatic or a positive number.";
FindSegment::badradii = "Radii specification `1` is not supported; use Automatic or {rmin, rmax} with 1 <= rmin <= rmax.";


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

FindPoint[ graph_Graph, All, opts : OptionsPattern[] ] :=
  FindPoint[ graph, UpTo[ VertexCount[ graph ] ], opts ]

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
   path-space filtering.  Method -> "Extended" (or {"Extended",
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
  Module[ { spec = OptionValue[ Method ], methodName, prune, lookback, formanMethod, constraint, curvatureMethod, dim, radii, dagNbrs, edgeKappa, d, paths },
    If[ p1 === p2, Return[ { } ] ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    { prune, lookback, formanMethod, constraint, curvatureMethod, dim, radii } = Replace[ spec, {
      _String :> { Infinity, 2, "Simple", "Geodesic", "Forman", Automatic, Automatic },
      { _String, subOpts___ } :> {
        "Pruning"         /. { subOpts } /. "Pruning"         -> Infinity,
        "Lookback"        /. { subOpts } /. "Lookback"        -> 2,
        "FormanMethod"    /. { subOpts } /. "FormanMethod"    -> "Simple",
        "Constraint"      /. { subOpts } /. "Constraint"      -> "Geodesic",
        "CurvatureMethod" /. { subOpts } /. "CurvatureMethod" -> "Forman",
        "Dimension"       /. { subOpts } /. "Dimension"       -> Automatic,
        "Radii"           /. { subOpts } /. "Radii"           -> Automatic
      },
      _ :> { Infinity, 2, "Simple", "Geodesic", "Forman", Automatic, Automatic }
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
      "Extended",
        Which[
          ! pruningSpecQ[ prune ],
            Message[ FindSegment::badpruning, prune ]; $Failed,
          ! lookbackSpecQ[ lookback ],
            Message[ FindSegment::badlookback, lookback ]; $Failed,
          True,
            paths = extendedOutPaths[ graph, p1, p2, prune, lookback, countLimit[ count ] ];
            If[ MatchQ[ count, _Integer ] && Length[ paths ] < count, $Failed, paths ]
        ],
      "Pulled",
        Which[
          ! curvatureMethodSpecQ[ curvatureMethod ],
            Message[ FindSegment::badcurvature, curvatureMethod ]; $Failed,
          ! formanMethodSpecQ[ formanMethod ],
            Message[ FindSegment::badforman, formanMethod ]; $Failed,
          ! dimensionSpecQ[ dim ],
            Message[ FindSegment::baddim, dim ]; $Failed,
          ! radiiSpecQ[ radii ],
            Message[ FindSegment::badradii, radii ]; $Failed,
          ! pruningSpecQ[ prune ],
            Message[ FindSegment::badpruning, prune ]; $Failed,
          ! constraintSpecQ[ constraint ],
            Message[ FindSegment::badconstraint, constraint ]; $Failed,
          True,
            dagNbrs = If[ constraint === "Geodesic", geodesicDAGNeighbors[ graph, p1, p2 ], Automatic ];
            edgeKappa = buildEdgeKappa[ graph, curvatureMethod, formanMethod, dim, radii ];
            paths = pulledPaths[ graph, p1, p2, edgeKappa, prune, countLimit[ count ], dagNbrs ];
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


(* buildEdgeKappa returns the (v, w) |-> Real closure that the Pulled
   frontier sweep MinimalBy's on.  For "Forman" it is the symmetric
   edge curvature F(v, w) returned by FormanRicciCurvature.  For
   "Wolfram" it is the target-vertex Wolfram-Ricci scalar averaged
   over the chosen radii range, R-bar(w); the rope at v feels the
   curvature of the cell it is about to enter. *)

buildEdgeKappa[ graph_Graph, "Forman", formanMethod_, _, _ ] :=
  With[ { fEdges = FormanRicciCurvature[ graph, Method -> formanMethod ] },
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

buildEdgeKappa[ graph_Graph, "Wolfram", _, dim_, radii_ ] :=
  With[ { vertexScalars = If[ radii === Automatic,
        WolframRicciScalar[ graph, All, "Dimension" -> dim ],
        WolframRicciScalar[ graph, All, radii, "Dimension" -> dim ]
      ] },
    With[ { vertexKappa = Mean[ Values[ # ] ] & /@ vertexScalars },
      { v, w } |-> vertexKappa[ w ]
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
   length equals GraphDiameter[g].
     Method axis is shared with FindSegment / ExtendSegment.
   "Shortest" (default) returns every maximal geodesic extension.
   "Extended" returns every maximal geodesic extension of every
   "Extended"-segment from p1 to p2 (the underlying middle path is then a
   recency-lex-certified simple path, the prefix and suffix are still
   geodesic).  "Pulled" does the same with curvature-pulled middle paths.
   "Embedding" ranks the extensions by EmbeddingHausdorffDistance to the
   infinite Euclidean line through p1 and p2 under the graph drawing.
   To extend a given segment to a line, use ExtendSegment. *)

FindLine::badmethod = "Method `1` is not supported by FindLine.";
FindLine::nyi = "Constraint `1` is not yet implemented for FindLine; use \"Geodesic\".";

Options[ FindLine ] = { "Maximality" -> "Extension", Method -> "Shortest" };

FindLine[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { spec, methodName, middles, allExtensions, diam },
    spec = OptionValue[ Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, Automatic -> "Shortest", _ :> spec } ];
    middles = Switch[ methodName,
      "Shortest" | "Embedding", allGeodesics[ graph, p1, p2 ],
      "Extended" | "Pulled",
        With[ { paths = FindSegment[ graph, p1, p2, All, Method -> spec ] },
          If[ paths === $Failed || ! ListQ[ paths ], { }, paths ]
        ],
      _, $Failed
    ];
    If[ middles === $Failed, Message[ FindLine::badmethod, spec ]; Return[ $Failed ] ];
    allExtensions = Union @ Flatten[ findLineExtensions[ graph, # ] & /@ middles, 1 ];
    If[ OptionValue[ "Maximality" ] === "Diameter",
      diam = GraphDiameter[ graph ];
      allExtensions = Select[ allExtensions, line |-> Length[ line ] - 1 == diam ]
    ];
    Switch[ methodName,
      "Shortest" | "Extended" | "Pulled", allExtensions,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          If[ embOpts[ "Constraint" ] =!= "Geodesic",
            Message[ FindLine::nyi, embOpts[ "Constraint" ] ]; allExtensions,
            embeddingRankLines[ graph, allExtensions, p1, p2, embOpts ]
          ]
        ]
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
   for "Shortest", recency-lex-certified for "Extended", curvature-pulled
   for "Pulled", embedding-line-aligned for "Embedding"). The Method axis
   matches FindSegment / FindLine: every method is a path-enumeration
   strategy on the prefix and suffix; the candidate before / after vertices
   are selected by the geodesic relation
     d(s, p1) + d(p1, p2) == d(s, p2),  d(p2, e) + d(p1, p2) == d(p1, e)
   so the line's outer endpoints are graph-distance-maximal from each
   other regardless of method. *)

ExtendSegment::badmethod = "Method `1` is not supported by ExtendSegment.";
ExtendSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
ExtendSegment::badlookback = "Lookback specification `1` is not supported; use a positive integer or All.";
ExtendSegment::badforman = "FormanMethod specification `1` is not supported; use \"Simple\" or \"Triangles\".";
ExtendSegment::badcurvature = "CurvatureMethod specification `1` is not supported; use \"Forman\" or \"Wolfram\".";
ExtendSegment::baddim = "Dimension specification `1` is not supported; use Automatic or a positive number.";
ExtendSegment::badradii = "Radii specification `1` is not supported; use Automatic or {rmin, rmax} with 1 <= rmin <= rmax.";

Options[ ExtendSegment ] = { Method -> "Shortest" };

ExtendSegment[ graph_Graph, segment_List,
    count : (_Integer | UpTo[ _Integer ] | All) : 1, opts : OptionsPattern[] ] :=
  Module[ { spec, methodName, prune, lookback, formanMethod, curvatureMethod, dim, radii, edgeKappa, paths },
    If[ Length[ segment ] < 2, Return[ { segment } ] ];
    spec = OptionValue[ Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, Automatic -> "Shortest", _ :> spec } ];
    { prune, lookback, formanMethod, curvatureMethod, dim, radii } = Replace[ spec, {
      _String :> { Infinity, 2, "Simple", "Forman", Automatic, Automatic },
      { _String, subOpts___ } :> {
        "Pruning"         /. { subOpts } /. "Pruning"         -> Infinity,
        "Lookback"        /. { subOpts } /. "Lookback"        -> 2,
        "FormanMethod"    /. { subOpts } /. "FormanMethod"    -> "Simple",
        "CurvatureMethod" /. { subOpts } /. "CurvatureMethod" -> "Forman",
        "Dimension"       /. { subOpts } /. "Dimension"       -> Automatic,
        "Radii"           /. { subOpts } /. "Radii"           -> Automatic
      },
      _ :> { Infinity, 2, "Simple", "Forman", Automatic, Automatic }
    } ];
    paths = Switch[ methodName,
      "Shortest",
        findLineExtensions[ graph, segment ],
      "Extended",
        Which[
          ! pruningSpecQ[ prune ],
            Message[ ExtendSegment::badpruning, prune ]; $Failed,
          ! lookbackSpecQ[ lookback ],
            Message[ ExtendSegment::badlookback, lookback ]; $Failed,
          True,
            findLineExtensionsWith[ graph, segment,
              { s, p, db } |-> extendedOutPaths[ graph, s, p, prune, lookback, Infinity ],
              { p, e, da } |-> extendedOutPaths[ graph, p, e, prune, lookback, Infinity ] ]
        ],
      "Pulled",
        Which[
          ! curvatureMethodSpecQ[ curvatureMethod ],
            Message[ ExtendSegment::badcurvature, curvatureMethod ]; $Failed,
          ! formanMethodSpecQ[ formanMethod ],
            Message[ ExtendSegment::badforman, formanMethod ]; $Failed,
          ! dimensionSpecQ[ dim ],
            Message[ ExtendSegment::baddim, dim ]; $Failed,
          ! radiiSpecQ[ radii ],
            Message[ ExtendSegment::badradii, radii ]; $Failed,
          ! pruningSpecQ[ prune ],
            Message[ ExtendSegment::badpruning, prune ]; $Failed,
          True,
            edgeKappa = buildEdgeKappa[ graph, curvatureMethod, formanMethod, dim, radii ];
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
   auxiliary segments. *)

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
