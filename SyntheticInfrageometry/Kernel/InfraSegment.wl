Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findSegmentCore]
PackageScope[extendSegmentCore]


(* ===================== InfraSegment wrapper ===================== *)

InfraSegment[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraSegment[ _List ] ] ] :=
  InfraSegment[ Flatten[ reps /. InfraSegment[ xs_List ] :> xs, 1 ] ]

InfraSegment /: Part[ InfraSegment[ reps_List ], i_Integer ] := InfraSegment[ { reps[[ i ]] } ]
InfraSegment /: Part[ InfraSegment[ reps_List ], spec_ ]     := InfraSegment[ reps[[ spec ]] ]

InfraSegment[ reps_List ][ "Realizations" ] := reps
InfraSegment[ reps_List ][ "Length" ]       := Length @ reps
InfraSegment[ reps_List ][ "Expand" ]       := InfraSegment[ { # } ] & /@ reps
InfraSegment[ reps_List ][ "First" ]        := First @ reps


(* ===================== Messages ===================== *)

FindSegment::badmethod = "Method `1` is not supported by FindSegment.";
FindSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
FindSegment::badwindow = "ShortestPathWindow specification `1` is not supported; use a positive integer or All.";
FindSegment::badpool = "Pool specification `1` is not supported; use \"ShortestPaths\" or \"AllPaths\".";
FindSegment::badcurvature = "Curvature specification `1` is not supported; use \"Forman\", \"Wolfram\", {\"Forman\", Method -> \"Simple\" | \"Triangles\"}, or {\"Wolfram\", \"Dimension\" -> Automatic | d, \"Radii\" -> Automatic | {rmin, rmax}}.";


(* ===================== FindSegment ===================== *)

(* A segment between p1 and p2 is a geodesic vertex sequence
   (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2) and consecutive vi
   adjacent.  Method axis: "Shortest" (default) | "ShortestPathExtension"
   | "CurvatureMinimizing" | "Embedding". *)

Options[ FindSegment ] = { Method -> "Shortest" };

FindSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findSegmentCore[ graph, ##, count, opts ] &, p1, p2 ]


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
   CurvatureMinimizing frontier sweep MinimalBy's on.  Forwards Forman
   spec via "MaxCellDimension" to the sister paclet's curvature symbols. *)

buildEdgeKappa[ graph_Graph, KeyValuePattern[ { "Head" -> "Forman", "Method" -> formanMethod_ } ] ] :=
  With[ { fEdges = WolframInstitute`Infrageometry`FormanRicciCurvature[
        graph,
        "MaxCellDimension" -> If[ formanMethod === "Triangles", 2, 1 ]
      ] },
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
        WolframInstitute`Infrageometry`WolframRicciCurvature[ graph, "Dimension" -> dim ],
        WolframInstitute`Infrageometry`WolframRicciCurvature[ graph, radii, "Dimension" -> dim ]
      ] },
    { v, w } |-> vertexKappa[ w ]
  ]


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


(* ===================== ExtendSegment ===================== *)

(* ExtendSegment[g, segment] takes a vertex sequence and extends it to a
   line.  Method axis matches FindSegment / FindLine.  ExtendSegment[g,
   a, b, c, d, n] is the Tarski synthetic-extension form (axiom A4):
   returns InfraPoint of vertices x with B(a, b, x) and bx == cd. *)

ExtendSegment::badmethod = "Method `1` is not supported by ExtendSegment.";
ExtendSegment::badpruning = "Pruning specification `1` is not supported; use Infinity, a positive integer (beam width), or a number 0 < p < 1 (Bernoulli keep probability).";
ExtendSegment::badwindow = "ShortestPathWindow specification `1` is not supported; use a positive integer or All.";
ExtendSegment::badcurvature = "Curvature specification `1` is not supported; use \"Forman\", \"Wolfram\", {\"Forman\", Method -> \"Simple\" | \"Triangles\"}, or {\"Wolfram\", \"Dimension\" -> Automatic | d, \"Radii\" -> Automatic | {rmin, rmax}}.";

Options[ ExtendSegment ] = { Method -> "Shortest" };

ExtendSegment[ graph_Graph, segment_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    extendSegmentCore[ graph, ##, count, opts ] &, segment ]


(* 5-vertex ExtendSegment: Tarski synthetic extension (axiom A4). *)

ExtendSegment[ graph_Graph, a_, b_, c_, d_, All, OptionsPattern[] ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    If[ target === Infinity, InfraPoint[ {} ],
      InfraPoint @ Select[ VertexList[ graph ],
        x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target ]
    ]
  ]

ExtendSegment[ graph_Graph, a_, b_, c_, d_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  With[ { result = ExtendSegment[ graph, a, b, c, d, All, opts ] },
    InfraPoint @ Take[ result[ "Realizations" ], UpTo[ n ] ]
  ]

ExtendSegment[ graph_Graph, a_, b_, c_, d_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = ExtendSegment[ graph, a, b, c, d, UpTo[ n ], opts ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


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


(* ===================== SegmentQ ===================== *)

(* SegmentQ tests whether a vertex sequence (v0, ..., vk) realises a
   geodesic from v0 to vk: consecutive vertices adjacent and total length
   equal to d(v0, vk). *)

SegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

SegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False


(* ===================== UniqueSegmentQ ===================== *)

(* UniqueSegmentQ[g, u, v]: there is a unique geodesic from u to v
   (GeodesicMultiplicity == 1).  Whole-graph form is the geodetic-graph
   predicate: every pair of vertices admits a unique geodesic. *)

UniqueSegmentQ[ graph_Graph, u_, v_ ] := GeodesicMultiplicity[ graph, u, v ] == 1

UniqueSegmentQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> UniqueSegmentQ[ graph, pair[[ 1 ]], pair[[ 2 ]] ]
  ]
