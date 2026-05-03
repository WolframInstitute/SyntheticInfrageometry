Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[HausdorffDistance]
PackageScope[FrechetDistance]
PackageScope[MinimalSeparationDistance]
PackageScope[EmbeddingHausdorffDistance]
PackageScope[EmbeddingCircleDistance]
PackageScope[pathFilterPairwiseDistances]
PackageScope[applyPathSpaceSelector]
PackageScope[geodesicDAGNeighbors]
PackageScope[generateEmbeddingPaths]
PackageScope[resolveEmbeddingCoords]
PackageScope[parseEmbeddingMethod]


(* ===================== Path-space distances ===================== *)

(* Hausdorff distance between two vertex subsets X, Y under graph distance:
   max{ sup_{x in X} d(x, Y),  sup_{y in Y} d(y, X) }.  For two paths in a
   graph, this is the symmetric "max one-sided gap" between their vertex
   sets. *)

HausdorffDistance[ d_List, setX_, setY_ ] :=
  With[ { distSubMatrix = d[[ setX, setY ]] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]

HausdorffDistance[ g_Graph, setX_List, setY_List ] :=
  With[ { distSubMatrix = Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]


(* Frechet-like distance between two equilength sequences: aggregator f
   applied to the diagonal of pairwise distances (Max -> Frechet, Mean ->
   averaged Frechet).  Index alignment is positional. *)

FrechetDistance[ d_List, setX_, setY_, f_ : Max ] :=
  f[ Diagonal[ d[[ setX, setY ]] ] ]

FrechetDistance[ g_Graph, setX_List, setY_List, f_ : Max ] :=
  f[ Diagonal[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ] ]


(* Minimum graph distance between two vertex subsets. *)

MinimalSeparationDistance[ d_List, setX_, setY_ ] :=
  Min[ d[[ setX, setY ]] ]

MinimalSeparationDistance[ g_Graph, setX_List, setY_List ] :=
  Min[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ]


(* ===================== Embedding distances ===================== *)

(* Hausdorff distance in the plane between the polyline of a path under a
   graph drawing and the straight segment between two endpoint vertices --
   how far the path deviates from the Euclidean straight line.  Degenerate
   one-vertex paths score 0. *)

EmbeddingHausdorffDistance[ coords_List, path_List, { p1_, p2_ } ] /; Length[ path ] >= 2 :=
  RegionHausdorffDistance[ Line[ coords[[ path ]] ], Line[ { coords[[ p1 ]], coords[[ p2 ]] } ] ]

EmbeddingHausdorffDistance[ _List, path_List, { _, _ } ] /; Length[ path ] < 2 := 0


(* Hausdorff distance in the plane between a graph cycle (drawn as a closed
   polyline under the graph embedding) and the Euclidean circle of given
   centre and radius -- how round the cycle looks when drawn. *)

EmbeddingCircleDistance[ coords_List, cycle_List, centerIdx_Integer, radius_ ] /; Length[ cycle ] >= 3 :=
  Module[ { centerPt, cyclePts, cycleRegion, nPts, circlePoints, circleRegion },
    centerPt = coords[[ centerIdx ]];
    cyclePts = coords[[ cycle ]];
    cycleRegion = Line[ Append[ cyclePts, First[ cyclePts ] ] ];
    nPts = Max[ 64, 4 * Length[ cycle ] ];
    circlePoints = Table[
      centerPt + radius * { Cos[ t ], Sin[ t ] },
      { t, 0, 2 Pi - 2 Pi / nPts, 2 Pi / nPts }
    ];
    circleRegion = Line[ Append[ circlePoints, First[ circlePoints ] ] ];
    RegionHausdorffDistance[ cycleRegion, circleRegion ]
  ]

EmbeddingCircleDistance[ _List, cycle_List, _Integer, _ ] /; Length[ cycle ] < 3 := Infinity


(* ===================== Pairwise path distance matrix ===================== *)

(* pathFilterPairwiseDistances builds the full symmetric matrix of pairwise
   path-space distances between the supplied paths under baseDist.  When
   cyclic is True, every cyclic rotation of the second argument is tried
   and the minimum is kept (ensures cycle distance is rotation-invariant). *)

pathFilterPairwiseDistances[ graph_Graph, paths_List, baseDist_, cyclic_ ] :=
  Module[ { distMatrix, vertexIndex, pathDistance },
    distMatrix = GraphDistanceMatrix[ graph ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    pathDistance = If[ cyclic,
      ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
      baseDist
    ];
    (# + Transpose[ # ]) & @ PadRight[ Table[
      pathDistance[ distMatrix, Lookup[ vertexIndex, paths[[ i ]] ], Lookup[ vertexIndex, paths[[ j ]] ] ],
      { i, Length[ paths ] }, { j, i - 1 } ], { Length[ paths ], Length[ paths ] } ]
  ]


(* ===================== Path-space selectors: paths ===================== *)

(* CentralPaths[g, paths] keeps the paths that minimise their maximum
   path-space distance to the rest of the input -- the metric centres of
   the path bundle.  PeripheralPaths keeps those that maximise the same
   eccentricity instead.  Method picks the underlying base metric:
   "Frechet" (default) is the Hausdorff-of-aligned-pairs distance,
   "Hausdorff" is the unaligned subset Hausdorff, "MeanFrechet" averages
   the aligned pair-distances instead of taking the max.  Operator form
   CentralPaths[g, opts][paths] is provided for chaining. *)

Options[ CentralPaths ] = { Method -> "Frechet" };
Options[ PeripheralPaths ] = { Method -> "Frechet" };

CentralPaths[ graph_Graph, paths_List, opts : OptionsPattern[] ] :=
  pathSpaceExtremalPick[ graph, paths, OptionValue[ Method ], False, Min ]

CentralPaths[ graph_Graph, opts : OptionsPattern[] ] :=
  CentralPaths[ graph, #, opts ] &

PeripheralPaths[ graph_Graph, paths_List, opts : OptionsPattern[] ] :=
  pathSpaceExtremalPick[ graph, paths, OptionValue[ Method ], False, Max ]

PeripheralPaths[ graph_Graph, opts : OptionsPattern[] ] :=
  PeripheralPaths[ graph, #, opts ] &


(* ===================== Path-space selectors: cycles ===================== *)

(* Cycle versions use the same path-space metrics with cyclic rotation. *)

Options[ CentralCycles ] = { Method -> "Frechet" };
Options[ PeripheralCycles ] = { Method -> "Frechet" };

CentralCycles[ graph_Graph, cycles_List, opts : OptionsPattern[] ] :=
  pathSpaceExtremalPick[ graph, cycles, OptionValue[ Method ], True, Min ]

CentralCycles[ graph_Graph, opts : OptionsPattern[] ] :=
  CentralCycles[ graph, #, opts ] &

PeripheralCycles[ graph_Graph, cycles_List, opts : OptionsPattern[] ] :=
  pathSpaceExtremalPick[ graph, cycles, OptionValue[ Method ], True, Max ]

PeripheralCycles[ graph_Graph, opts : OptionsPattern[] ] :=
  PeripheralCycles[ graph, #, opts ] &


pathSpaceExtremalPick[ _Graph, paths_List, _, _, _ ] /; Length[ paths ] <= 1 := paths

pathSpaceExtremalPick[ graph_Graph, paths_List, method_String, cyclic_, extremum_ ] :=
  Module[ { baseDist, pd, scores },
    baseDist = Switch[ method,
      "Frechet",     FrechetDistance,
      "Hausdorff",   HausdorffDistance,
      "MeanFrechet", FrechetDistance[ ##, Mean ] &,
      _,             FrechetDistance
    ];
    pd = pathFilterPairwiseDistances[ graph, paths, baseDist, cyclic ];
    scores = Max /@ pd;
    Pick[ paths, scores, extremum[ scores ] ]
  ]


(* ===================== Embedding-aware selectors ===================== *)

(* EmbeddingClosestPaths picks the paths closest to the straight Euclidean
   segment between p1 and p2 under the graph's drawing -- i.e. the paths
   that look most like the chord. *)

EmbeddingClosestPaths[ graph_Graph, paths_List, { p1_, p2_ } ] /; Length[ paths ] <= 1 := paths

EmbeddingClosestPaths[ graph_Graph, paths_List, { p1_, p2_ } ] :=
  Module[ { coords, vertexIndex, ep },
    coords = resolveEmbeddingCoords[ graph, Automatic ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    ep = Lookup[ vertexIndex, { p1, p2 } ];
    MinimalBy[ paths,
      path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
  ]

EmbeddingClosestPaths[ graph_Graph, { p1_, p2_ } ] :=
  EmbeddingClosestPaths[ graph, #, { p1, p2 } ] &


(* EmbeddingClosestCycles picks the cycles closest to the Euclidean circle
   of given centre and radius under the graph's drawing -- the visually
   roundest cycles. *)

EmbeddingClosestCycles[ graph_Graph, cycles_List, { center_, radius_ } ] /; Length[ cycles ] <= 1 := cycles

EmbeddingClosestCycles[ graph_Graph, cycles_List, { center_, radius_ } ] :=
  Module[ { coords, vertexIndex, centerIdx },
    coords = resolveEmbeddingCoords[ graph, Automatic ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerIdx = vertexIndex[ center ];
    MinimalBy[ cycles,
      cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
  ]

EmbeddingClosestCycles[ graph_Graph, { center_, radius_ } ] :=
  EmbeddingClosestCycles[ graph, #, { center, radius } ] &


(* ===================== Length filters (cycles) ===================== *)

(* Combinatorial circumference filters: paths from FindSegment / FindLine
   are equilength so length filtering is degenerate there, but cycles from
   FindCircle can vary. *)

ShortestCircumferenceCycles[ cycles_List ] /; Length[ cycles ] <= 1 := cycles
ShortestCircumferenceCycles[ cycles_List ] := MinimalBy[ cycles, Length ]

LongestCircumferenceCycles[ cycles_List ] /; Length[ cycles ] <= 1 := cycles
LongestCircumferenceCycles[ cycles_List ] := MaximalBy[ cycles, Length ]


(* ===================== Embedding-method helpers ===================== *)

(* parseEmbeddingMethod[spec] extracts the suboptions of Method -> "Embedding"
   (or Method -> {"Embedding", ...}) into an Association with keys
   "Coordinates", "Constraint", "Pruning". *)

parseEmbeddingMethod[ spec_ ] :=
  Replace[ spec, {
    "Embedding" -> <| "Coordinates" -> Automatic, "Constraint" -> "Geodesic", "Pruning" -> Infinity |>,
    { "Embedding", subOpts___ } :> <|
      "Coordinates" -> ( "Coordinates" /. { subOpts } /. "Coordinates" -> Automatic ),
      "Constraint"  -> ( "Constraint"  /. { subOpts } /. "Constraint"  -> "Geodesic" ),
      "Pruning"     -> ( "Pruning"     /. { subOpts } /. "Pruning"     -> Infinity )
    |>,
    _ -> <| "Coordinates" -> Automatic, "Constraint" -> "Geodesic", "Pruning" -> Infinity |>
  } ]


(* resolveEmbeddingCoords[graph, spec] returns the coordinate matrix from the
   Coordinates suboption value: Automatic -> the unit-edge-faithful
   GraphEmbedding under SpringEmbedding (the closest built-in to the
   edge-length-preserving criterion that reflects intrinsic graph
   geometry; see Wiki/Concepts/GraphEmbeddings.md), else the
   user-supplied matrix. *)

resolveEmbeddingCoords[ graph_Graph, Automatic ] :=
  GraphEmbedding[ Graph[ graph, GraphLayout -> "SpringEmbedding" ] ]
resolveEmbeddingCoords[ _, coords_List ] := coords


(* geodesicDAGNeighbors[graph, u, v] returns an Association
   vertex -> {downstream DAG neighbors} for every vertex w on some geodesic
   from u to v.  A vertex w is in the DAG iff dist(u, w) + dist(w, v) =
   dist(u, v); edges go from layer k = dist(u, w) to layer k + 1.  Paths
   from u to v through this DAG are exactly the geodesics. *)

geodesicDAGNeighbors[ graph_Graph, u_, v_ ] :=
  Module[ { du, dv, total, dagVerts, dagSet },
    du = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, u ] ];
    dv = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, v ] ];
    total = du[ v ];
    If[ total === Infinity, Return[ <||> ] ];
    dagVerts = Select[ VertexList[ graph ], du[ # ] + dv[ # ] == total & ];
    dagSet = AssociationThread[ dagVerts, True ];
    AssociationMap[
      w |-> Select[ AdjacencyList[ graph, w ],
        TrueQ[ dagSet[ # ] ] && du[ # ] == du[ w ] + 1 & ],
      dagVerts
    ]
  ]


(* generateEmbeddingPaths[extendFn, startPath, goalQ, prune] runs a depth-first
   recursion that extends each partial path via extendFn[path]; complete
   candidates (those satisfying goalQ[path]) are collected.  At every
   branching step the extension list is filtered by applyPruning, so the
   same Pruning spec used by Method -> "Extended" controls the search. *)

generateEmbeddingPaths[ extendFn_, startPath_List, goalQ_, prune_ ] :=
  Module[ { results = {}, recurse },
    recurse[ path_ ] :=
      If[ TrueQ[ goalQ[ path ] ],
        AppendTo[ results, path ],
        Scan[
          candidate |-> recurse[ Append[ path, candidate ] ],
          applyPruning[ extendFn[ path ], prune ]
        ]
      ];
    recurse[ startPath ];
    results
  ]


(* ===================== String-named selector dispatch (internal) ===================== *)

(* Legacy string-name dispatch.  Used only by Scenes.wl (InfraScene
   hypothesis "Select" option) and Viewers.wl (SetterBar widgets) to keep
   the user-visible string interface working after the public API moved
   to chainable functions.  ctx supplies the side data the embedding /
   cycle selectors need: "Cyclic", "Endpoints" (paths), "Center",
   "Radius" (cycles). *)

applyPathSpaceSelector[ _Graph, paths_List, None, _Association ] := paths

applyPathSpaceSelector[ graph_Graph, paths_List, methods_List, ctx_Association ] :=
  Fold[ applyPathSpaceSelector[ graph, #1, #2, ctx ] &, paths, methods ]

applyPathSpaceSelector[ graph_Graph, paths_List, name_String, ctx_Association ] :=
  With[ { cyclic = TrueQ @ ctx[ "Cyclic" ] },
    Switch[ name,
      "FrechetCentral",
        If[ cyclic, CentralCycles[ graph, paths, Method -> "Frechet" ],
                    CentralPaths [ graph, paths, Method -> "Frechet" ] ],
      "FrechetPeripheral",
        If[ cyclic, PeripheralCycles[ graph, paths, Method -> "Frechet" ],
                    PeripheralPaths [ graph, paths, Method -> "Frechet" ] ],
      "MeanFrechetCentral",
        If[ cyclic, CentralCycles[ graph, paths, Method -> "MeanFrechet" ],
                    CentralPaths [ graph, paths, Method -> "MeanFrechet" ] ],
      "MeanFrechetPeripheral",
        If[ cyclic, PeripheralCycles[ graph, paths, Method -> "MeanFrechet" ],
                    PeripheralPaths [ graph, paths, Method -> "MeanFrechet" ] ],
      "HausdorffCentral",
        If[ cyclic, CentralCycles[ graph, paths, Method -> "Hausdorff" ],
                    CentralPaths [ graph, paths, Method -> "Hausdorff" ] ],
      "HausdorffPeripheral",
        If[ cyclic, PeripheralCycles[ graph, paths, Method -> "Hausdorff" ],
                    PeripheralPaths [ graph, paths, Method -> "Hausdorff" ] ],
      "ShortestCircumference", ShortestCircumferenceCycles[ paths ],
      "LongestCircumference",  LongestCircumferenceCycles [ paths ],
      "EmbeddingClosest",
        If[ cyclic,
          EmbeddingClosestCycles[ graph, paths, { ctx[ "Center" ], ctx[ "Radius" ] } ],
          EmbeddingClosestPaths [ graph, paths, ctx[ "Endpoints" ] ] ],
      _, paths
    ]
  ]


(* ===================== Path-domain subgraph constructors ===================== *)

(* GeodesicSubgraph[g, pairs] returns the union of geodesics between the
   listed vertex pairs, as a single graph.  "PathThickness" -> 0 keeps one
   shortest path per pair (the built-in FindPath); "PathThickness" -> Infinity
   keeps every shortest path; a finite positive value keeps the geodesics
   whose path-Hausdorff distance to the first geodesic is at most that
   threshold.  "Directed" -> True orients each path from the first vertex
   of the pair to the second. *)

Options[ GeodesicSubgraph ] = { "PathThickness" -> 0, "Directed" -> True };

GeodesicSubgraph[ g_Graph, pairs_List, OptionsPattern[] ] :=
  Module[ { distMatrix, thickness, vertexToIndex, selectedPaths, directed, hausdorff },
    thickness = OptionValue[ "PathThickness" ];
    directed = OptionValue[ "Directed" ];
    vertexToIndex = AssociationThread[ VertexList[ g ], Range @ VertexCount[ g ] ];
    distMatrix = GraphDistanceMatrix[ g ];
    hausdorff = With[ { dm = distMatrix[[ #1, #2 ]] },
      Max[ Max[ Min /@ dm ], Max[ Min /@ Transpose @ dm ] ] ] &;
    selectedPaths = Which[
      thickness === 0,
      ( First @ FindPath[ g, #1, #2, { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, 1 ] & ) @@@ pairs,
      thickness === Infinity,
      Flatten[ ( FindPath[ g, #1, #2, { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, All ] & ) @@@ pairs, 1 ],
      True,
      Flatten[
        ( If[ # === { }, { },
            With[ { ref = vertexToIndex /@ First[ # ] },
              Select[ #, path |-> hausdorff[ vertexToIndex /@ path, ref ] <= thickness ]
            ]
          ] & ) /@
        ( ( FindPath[ g, #1, #2, { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, All ] & ) @@@ pairs ),
        1
      ]
    ];
    GraphUnion @@ ( PathGraph[ #, DirectedEdges -> directed ] & /@ selectedPaths )
  ]


(* PathSubgraph[g, u, v, lengthSpec] returns the union of all simple u-v paths
   of length at most k -- the k-path subgraph.  At lengthSpec -> Automatic
   (default) k = d(u, v), giving the geodesic subgraph; an integer or UpTo[k]
   sets the cap; All gives every simple u-v path.  "Directed" -> True orients
   each path u -> v. *)

Options[ PathSubgraph ] = { "Directed" -> True };

PathSubgraph[ g_Graph, u_, v_, lengthSpec : ( _Integer | UpTo[ _Integer ] | All ) : Automatic, OptionsPattern[] ] :=
  Module[ { k, paths },
    k = Replace[ lengthSpec, {
      Automatic     :> GraphDistance[ g, u, v ],
      UpTo[ n_Integer ] :> n,
      All           -> Infinity,
      n_Integer     :> n
    } ];
    If[ u === v, Return @ Graph[ { u }, { } ] ];
    paths = FindPath[ g, u, v, k, All ];
    If[ paths === { },
      Graph[ { }, { } ],
      GraphUnion @@ ( PathGraph[ #, DirectedEdges -> OptionValue[ "Directed" ] ] & /@ paths )
    ]
  ]
