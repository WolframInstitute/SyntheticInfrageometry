Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[HausdorffDistance]
PackageScope[FrechetDistance]
PackageScope[MinimalSeparationDistance]
PackageScope[EmbeddingHausdorffDistance]
PackageScope[EmbeddingCircleDistance]
PackageScope[pathFilterPairwiseDistances]
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


(* Frechet-like distance between two sequences.  When lengths match the
   aggregator f is applied to the diagonal of the pairwise distance matrix
   (Max -> classical Frechet, Mean -> averaged Frechet).  When lengths
   differ both sequences are linearly resampled to the common length
   max(|X|, |Y|) before the diagonal is taken -- so the metric remains
   well-defined on bundles with mixed path lengths. *)

FrechetDistance[ d_List, setX_, setY_, f_ : Max ] :=
  If[ Length[ setX ] === Length[ setY ],
    f[ Diagonal[ d[[ setX, setY ]] ] ],
    With[ { m = Max[ Length[ setX ], Length[ setY ] ] },
      f[ MapThread[ d[[ #1, #2 ]] &, { resamplePath[ setX, m ], resamplePath[ setY, m ] } ] ]
    ]
  ]

FrechetDistance[ g_Graph, setX_List, setY_List, f_ : Max ] :=
  If[ Length[ setX ] === Length[ setY ],
    f[ MapThread[ GraphDistance[ g, #1, #2 ] &, { setX, setY } ] ],
    With[ { m = Max[ Length[ setX ], Length[ setY ] ] },
      f[ MapThread[ GraphDistance[ g, #1, #2 ] &,
        { setX[[ resamplePath[ setX, m ] ]], setY[[ resamplePath[ setY, m ] ]] } ] ]
    ]
  ]


(* resamplePath[seq, m] returns m positional indices into seq, evenly
   distributed (rounded), so a length-Length[seq] sequence stretches to
   length m by linear sampling.  Used to align unequal-length sequences
   before computing Frechet/MeanFrechet distance. *)

resamplePath[ seq_List, m_Integer ] :=
  If[ Length[ seq ] === m, Range[ m ],
    Round @ Rescale[ Range[ m ], { 1, m }, { 1, Length[ seq ] } ]
  ]


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


(* ===================== Path-space selectors ===================== *)

(* SelectPath[g, paths, n] / SelectCycle[g, cycles, n] are chainable
   post-filters on path-space treated as a finite metric space.  The bundle
   is the set of "points"; the path-space metric ("Frechet", "Hausdorff",
   "MeanFrechet") is the option "Metric" (default "Hausdorff", because it
   is well-defined for mixed-length bundles without resampling).  The API
   mirrors FindPoint exactly:

     SelectPath[g, paths]              -- exactly 1 or $Failed (default n = 1)
     SelectPath[g, paths, n_Integer]   -- exactly n or $Failed
     SelectPath[g, paths, UpTo[n]]     -- up to n
     SelectPath[g, paths, All]         -- whole pool

   Options:
     "From"     -> pool selector (default All).
                   All                            : whole bundle
                   "Center"                       : medoid pool (paths minimising
                                                    max path-space distance)
                   "Periphery"                    : eccentric pool (max it)
                   "MostVisited"                  : mode of the visit measure
                   path_List -> spec              : paths at distance spec
                                                    from an anchor path; spec is
                                                    a number, {dMin, dMax} range,
                                                    or "Max"
                   InfraSegment[{...}] -> spec    : multi-anchor intersection
                                                    (every realisation matches)
                   SelectCycle adds:
                   "ShortestCircumference" / "LongestCircumference"
     "Distance" -> mutual-distance constraint between the n returned paths.
                   None (default), "Max" (max-spread n-clique), d, or {dMin, dMax}.
     "Metric"   -> path-space metric: "Hausdorff" (default), "Frechet", "MeanFrechet"
     "MaxCliques" -> All (default) -- mirrors FindPoint

   Wrappers: SelectPath accepts InfraSegment[paths] / InfraRay[paths];
   SelectCycle accepts InfraCircle[cycles].  Each preserves its wrapper.
   Operator form takes the count token: SelectPath[g, n, opts][paths]. *)

Options[ SelectPath ] = {
  "From"       -> All,
  "Distance"   -> None,
  "Metric"     -> "Hausdorff",
  "MaxCliques" -> All
};

Options[ SelectCycle ] = Options[ SelectPath ];


SelectPath[ graph_Graph, paths_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  selectFromPathSpace[ graph, paths, n, False,
    OptionValue[ "From" ], OptionValue[ "Distance" ],
    OptionValue[ "Metric" ], OptionValue[ "MaxCliques" ] ]

SelectPath[ graph_Graph, paths_List, All, opts : OptionsPattern[] ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  SelectPath[ graph, paths, UpTo[ Length[ paths ] ], opts ]

SelectPath[ graph_Graph, paths_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  With[ { result = SelectPath[ graph, paths, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectPath[ graph_Graph, ( head : InfraSegment | InfraRay )[ paths_List ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectPath[ graph, paths, countSpec, opts ] },
    If[ result === $Failed, $Failed, head[ result ] ] ]

(* Symmetric form: a List of unary InfraSegment / InfraRay wrappers (the Find*
   return shape) routes through the bare-paths core and re-wraps each result
   under the matching head. *)

SelectPath[ graph_Graph, list_List,
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list,
          result = SelectPath[ graph, #[[ 1, 1 ]] & /@ list, countSpec, opts ] },
    If[ result === $Failed, $Failed, head[ { # } ] & /@ result ] ]

SelectPath[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectPath[ graph, #, countSpec, opts ] &


SelectCycle[ graph_Graph, cycles_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  selectFromPathSpace[ graph, cycles, n, True,
    OptionValue[ "From" ], OptionValue[ "Distance" ],
    OptionValue[ "Metric" ], OptionValue[ "MaxCliques" ] ]

SelectCycle[ graph_Graph, cycles_List, All, opts : OptionsPattern[] ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  SelectCycle[ graph, cycles, UpTo[ Length[ cycles ] ], opts ]

SelectCycle[ graph_Graph, cycles_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { result = SelectCycle[ graph, cycles, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectCycle[ graph_Graph, InfraCircle[ cycles_List ],
             countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectCycle[ graph, cycles, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraCircle[ result ] ] ]

(* Symmetric form: a List of unary InfraCircle wrappers routes through the
   bare-cycles core and re-wraps each result under InfraCircle. *)

SelectCycle[ graph_Graph, list_List,
             countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { result = SelectCycle[ graph, #[[ 1, 1 ]] & /@ list, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraCircle[ { # } ] & /@ result ] ]

SelectCycle[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectCycle[ graph, #, countSpec, opts ] &


(* selectFromPathSpace[graph, paths, nMax, cyclic, fromSpec, distSpec, metric, maxCl]
   is the shared core for SelectPath and SelectCycle.  Returns up to nMax
   paths, never fails; strict-n shortfall handling is left to the caller. *)

selectFromPathSpace[ _Graph, paths_List, _Integer, _, _, _, _, _ ] /; Length[ paths ] <= 1 := paths

selectFromPathSpace[ graph_Graph, paths_List, nMax_Integer, cyclic_,
                     fromSpec_, distSpec_, metric_, maxCl_ ] :=
  Module[ { baseDist, needsMatrix, pathMatrix, poolIdx, pool, subMatrix,
            finiteMax, range, auxiliaryGraph, cliques, thresholds, picked, n },
    baseDist = pathSpaceMetric[ metric ];
    needsMatrix = MatchQ[ fromSpec, "Center" | "Periphery" | _Rule ]
                || distSpec =!= None;
    pathMatrix = If[ needsMatrix,
      pathFilterPairwiseDistances[ graph, paths, baseDist, cyclic ], None ];
    poolIdx = poolPositions[ graph, paths, fromSpec, pathMatrix, baseDist, cyclic ];
    If[ poolIdx === {}, Return[ {} ] ];
    pool = paths[[ poolIdx ]];
    n = Min[ nMax, Length[ pool ] ];
    If[ distSpec === None || n <= 1,
      Return[ If[ n >= Length[ pool ], pool, RandomSample[ pool, n ] ] ] ];
    subMatrix = pathMatrix[[ poolIdx, poolIdx ]];
    finiteMax = Replace[ Max @ Select[ Flatten @ subMatrix, # < Infinity & ],
      _?( ! NumericQ @ # & ) -> 0 ];
    subMatrix = Replace[ subMatrix, Infinity -> finiteMax + 1, { 2 } ];
    picked = Which[
      distSpec === "Max",
        thresholds = Reverse @ DeleteCases[ Union @@ subMatrix, 0 | _?( # > finiteMax & ) ];
        cliques = {};
        Do[
          auxiliaryGraph = AdjacencyGraph[ pool,
            UnitStep[ subMatrix - d ] * UnitStep[ finiteMax - subMatrix ]
              * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
          cliques = FindClique[ auxiliaryGraph, { n, VertexCount[ auxiliaryGraph ] }, maxCl ];
          If[ cliques =!= {}, Break[] ],
          { d, thresholds }
        ];
        If[ cliques === {}, {}, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ],
      True,
        range = Replace[ distSpec,
          { d_?NumericQ                  :> { d, finiteMax },
            { dMin_?NumericQ, Infinity } :> { dMin, finiteMax },
            { dMin_?NumericQ, dMax_?NumericQ } :> { dMin, dMax },
            _ :> { 0, finiteMax } } ];
        auxiliaryGraph = AdjacencyGraph[ pool,
          UnitStep[ subMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - subMatrix ]
            * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
        cliques = FindClique[ auxiliaryGraph,
          { Min[ n, VertexCount[ auxiliaryGraph ] ], VertexCount[ auxiliaryGraph ] }, maxCl ];
        If[ cliques === {}, {}, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ]
    ];
    picked
  ]


(* pathSpaceMetric -- resolve the "Metric" option to a base path-space
   distance function over a graph-distance matrix. *)

pathSpaceMetric[ "Hausdorff"   ] := HausdorffDistance
pathSpaceMetric[ "Frechet"     ] := FrechetDistance
pathSpaceMetric[ "MeanFrechet" ] := FrechetDistance[ ##, Mean ] &
pathSpaceMetric[ _             ] := HausdorffDistance


(* poolPositions -- positions in paths satisfying the From specification.
   All -> every index; "Center" / "Periphery" -> eccentricity extremes;
   "MostVisited" -> max visit-score paths; anchor -> spec -> distance
   constraint from one or more anchor paths. *)

poolPositions[ _Graph, paths_List, All, _, _, _ ] := Range @ Length @ paths

poolPositions[ _Graph, _List, "Center", pathMatrix_List, _, _ ] :=
  With[ { scores = Max /@ pathMatrix },
    Flatten @ Position[ scores, Min @ scores, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, _List, "Periphery", pathMatrix_List, _, _ ] :=
  With[ { scores = Max /@ pathMatrix },
    Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, paths_List, "MostVisited", _, _, cyclic_ ] :=
  visitPoolPositions[ paths, cyclic ]

poolPositions[ _Graph, paths_List, "ShortestCircumference", _, _, _ ] :=
  With[ { lens = Length /@ paths },
    Flatten @ Position[ lens, Min @ lens, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, paths_List, "LongestCircumference", _, _, _ ] :=
  With[ { lens = Length /@ paths },
    Flatten @ Position[ lens, Max @ lens, { 1 }, Heads -> False ] ]

poolPositions[ graph_Graph, paths_List, ( anchor_ -> spec_ ), _, baseDist_, cyclic_ ] :=
  anchorDistancePool[ graph, paths, anchor, spec, baseDist, cyclic ]

poolPositions[ _, paths_List, _, _, _, _ ] := Range @ Length @ paths


(* visitPoolPositions -- positions of paths whose total vertex+edge visit
   count across the bundle is maximal.  Uses sequentialEdges for paths and
   cycleEdges for cycles (auto-closing the cycle list). *)

visitPoolPositions[ paths_List, cyclic_ ] :=
  With[ { edgesOf = If[ cyclic, cycleEdges, sequentialEdges ] },
    With[ { edgeSeqs = edgesOf /@ paths },
      With[ {
          vCounts = Counts @ Catenate @ paths,
          eCounts = Counts @ Catenate @ edgeSeqs },
        With[ { scores = MapThread[
            Total @ Lookup[ vCounts, #1, 0 ] + Total @ Lookup[ eCounts, #2, 0 ] &,
            { paths, edgeSeqs } ] },
          Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ] ] ] ] ]


(* anchorDistancePool -- positions of paths whose path-space distance to
   each anchor (vertex sequence or InfraSegment realisation) satisfies the
   given spec (numeric d, {dMin, dMax} range, or "Max" -- max distance from
   each anchor individually). *)

anchorDistancePool[ graph_Graph, paths_List, anchor_, spec_, baseDist_, cyclic_ ] :=
  Module[ { anchors, distMatrix, vertexIndex, distFn, anchorRows },
    anchors = Replace[ anchor, {
      ( InfraSegment | InfraRay | InfraCircle )[ reps_List ] :> reps,
      seq_List /; AllTrue[ seq, ListQ ] :> seq,
      seq_List :> { seq } } ];
    If[ anchors === { }, Return[ { } ] ];
    distMatrix = GraphDistanceMatrix[ graph ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    distFn = If[ cyclic,
      ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
      baseDist ];
    anchorRows = Table[
      Map[ p |-> distFn[ distMatrix,
        Lookup[ vertexIndex, a ], Lookup[ vertexIndex, p ] ], paths ],
      { a, anchors } ];
    Select[ Range @ Length @ paths,
      i |-> AllTrue[ Range @ Length @ anchors,
        a |-> anchorMatchQ[ anchorRows[[ a, i ]], anchorRows[[ a ]], spec ] ] ]
  ]


anchorMatchQ[ d_?NumericQ, _, target_?NumericQ ] := d == target
anchorMatchQ[ d_?NumericQ, _, { lo_?NumericQ, hi_?NumericQ } ] := lo <= d <= hi
anchorMatchQ[ d_?NumericQ, allDistsForAnchor_List, "Max" ] :=
  d == Max @ Select[ allDistsForAnchor, NumericQ ]
anchorMatchQ[ _, _, _ ] := False


(* sequentialEdges[path] returns sorted undirected edges along the vertex
   sequence; cycleEdges[cycle] does the same after auto-closing. *)

sequentialEdges[ path_List ] :=
  If[ Length[ path ] >= 2, Sort /@ Partition[ path, 2, 1 ], { } ]

cycleEdges[ cycle_List ] :=
  With[ { closed = If[ Length[ cycle ] >= 2 && First @ cycle === Last @ cycle,
                       cycle, Append[ cycle, First @ cycle ] ] },
    If[ Length[ closed ] >= 2, Sort /@ Partition[ closed, 2, 1 ], { } ]
  ]


(* ===================== Embedding-aware selectors ===================== *)

(* EmbeddingClosestPaths picks the paths closest to the straight Euclidean
   segment between p1 and p2 under the graph's drawing -- i.e. the paths
   that look most like the chord. *)

EmbeddingClosestPaths[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    Length[ paths ] <= 1 &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] ) := paths

EmbeddingClosestPaths[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  Module[ { coords, vertexIndex, ep },
    coords = resolveEmbeddingCoords[ graph, Automatic ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    ep = Lookup[ vertexIndex, { p1, p2 } ];
    MinimalBy[ paths,
      path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
  ]

EmbeddingClosestPaths[ graph_Graph, InfraSegment[ paths_List ], { p1_, p2_ } ] :=
  InfraSegment[ EmbeddingClosestPaths[ graph, paths, { p1, p2 } ] ]

(* List of unary InfraSegment / InfraRay wrappers: route through bare core,
   re-wrap each result. *)

EmbeddingClosestPaths[ graph_Graph, list_List, { p1_, p2_ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list },
    head[ { # } ] & /@ EmbeddingClosestPaths[ graph, #[[ 1, 1 ]] & /@ list, { p1, p2 } ] ]

EmbeddingClosestPaths[ graph_Graph, { p1_, p2_ } ] :=
  EmbeddingClosestPaths[ graph, #, { p1, p2 } ] &


(* EmbeddingClosestCycles picks the cycles closest to the Euclidean circle
   of given centre and radius under the graph's drawing -- the visually
   roundest cycles. *)

EmbeddingClosestCycles[ graph_Graph, cycles_List, { center_, radius_ } ] /;
    Length[ cycles ] <= 1 &&
    ( cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] ) := cycles

EmbeddingClosestCycles[ graph_Graph, cycles_List, { center_, radius_ } ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  Module[ { coords, vertexIndex, centerIdx },
    coords = resolveEmbeddingCoords[ graph, Automatic ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerIdx = vertexIndex[ center ];
    MinimalBy[ cycles,
      cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
  ]

EmbeddingClosestCycles[ graph_Graph, InfraCircle[ cycles_List ], { center_, radius_ } ] :=
  InfraCircle[ EmbeddingClosestCycles[ graph, cycles, { center, radius } ] ]

EmbeddingClosestCycles[ graph_Graph, list_List, { center_, radius_ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  InfraCircle[ { # } ] & /@
    EmbeddingClosestCycles[ graph, #[[ 1, 1 ]] & /@ list, { center, radius } ]

EmbeddingClosestCycles[ graph_Graph, { center_, radius_ } ] :=
  EmbeddingClosestCycles[ graph, #, { center, radius } ] &


(* ===================== Embedding-method helpers ===================== *)

(* parseEmbeddingMethod[spec, poolDefault] extracts the suboptions of
   Method -> "Embedding" (or Method -> {"Embedding", ...}) into an Association
   with keys "Coordinates", "Pool", "Pruning".  The poolDefault parameter sets
   the context-dependent default Pool value: "ShortestPaths" for path-shaped
   constructors (FindSegment / FindLine / ExtendSegment / FindMidpoint /
   FindPerpendicular), "LevelSet" for shell-shaped ones (FindShell / FindCircle
   / FindParallel). *)

parseEmbeddingMethod[ spec_, poolDefault_String : "ShortestPaths" ] :=
  Replace[ spec, {
    "Embedding" -> <| "Coordinates" -> Automatic, "Pool" -> poolDefault, "Pruning" -> Infinity |>,
    { "Embedding", subOpts___ } :> <|
      "Coordinates" -> ( "Coordinates" /. { subOpts } /. "Coordinates" -> Automatic ),
      "Pool"        -> ( "Pool"        /. { subOpts } /. "Pool"        -> poolDefault ),
      "Pruning"     -> ( "Pruning"     /. { subOpts } /. "Pruning"     -> Infinity )
    |>,
    _ -> <| "Coordinates" -> Automatic, "Pool" -> poolDefault, "Pruning" -> Infinity |>
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
   from u to v through this DAG are exactly the geodesics.

   geodesicDAGNeighbors[graph, c] is the single-source form: returns
   vertex -> {downstream DAG neighbors} for every vertex reachable from c.
   Edges go from layer k = dist(c, w) to layer k + 1.  Directed paths
   c -> sink in this DAG are exactly the maximal geodesics from c.  The
   form geodesicDAGNeighbors[graph, c, depth] truncates the DAG at the
   given depth (vertices with dist(c, w) > depth are excluded). *)

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

geodesicDAGNeighbors[ graph_Graph, c_ ] :=
  Module[ { dc, dagVerts, dagSet },
    dc = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, c ] ];
    dagVerts = Select[ VertexList[ graph ], dc[ # ] < Infinity & ];
    dagSet = AssociationThread[ dagVerts, True ];
    AssociationMap[
      w |-> Select[ AdjacencyList[ graph, w ],
        TrueQ[ dagSet[ # ] ] && dc[ # ] == dc[ w ] + 1 & ],
      dagVerts
    ]
  ]


(* generateEmbeddingPaths[extendFn, startPath, goalQ, prune] runs a depth-first
   recursion that extends each partial path via extendFn[path]; complete
   candidates (those satisfying goalQ[path]) are collected.  At every
   branching step the extension list is filtered by applyPruning, so the
   same Pruning spec used by Method -> "ShortestPathExtension" controls the search. *)

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


(* ===================== Path-domain subgraph constructors ===================== *)

(* GeodesicGraph[g, c] is the BFS DAG of all geodesics from the source c:
   a directed graph on the vertices reachable from c in g, with edge
   u -> v whenever d(c, v) = d(c, u) + 1 and u-v is an edge of g.
   Sources = {c}; sinks = vertices with no outgoing DAG edge (= peripheral
   vertices reachable from c).  Directed paths c -> sink are exactly the
   maximal geodesics from c.

   GeodesicGraph[g, InfraPoint[{c1, ..., ck}]] is the multi-source variant:
   distance from the source set is min_i d(ci, v); each vertex sits at its
   minimum distance to the InfraPoint anchors.

   Option "AxisLength" -> All | k truncates the DAG at depth k; vertices with
   d(sources, v) > k are excluded. *)

Options[ GeodesicGraph ] = { "AxisLength" -> All };

GeodesicGraph[ g_Graph, c_, OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  geodesicGraphFromDistances[ g, AssociationThread[ VertexList[ g ], GraphDistance[ g, c ] ],
    OptionValue[ "AxisLength" ] ]

GeodesicGraph[ g_Graph, InfraPoint[ vs_List ], OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  geodesicGraphFromDistances[ g,
    AssociationThread[ VertexList[ g ],
      Min /@ Transpose[ GraphDistance[ g, # ] & /@ vs ] ],
    OptionValue[ "AxisLength" ] ]

geodesicGraphFromDistances[ g_Graph, dist_Association, depthSpec_ ] :=
  Module[ { depth, dagVerts },
    depth = Replace[ depthSpec, All -> Infinity ];
    dagVerts = Select[ VertexList[ g ], dist[ # ] < Infinity && dist[ # ] <= depth & ];
    Graph[ dagVerts,
      Map[
        e |-> With[ { u = e[[ 1 ]], v = e[[ 2 ]] },
          Which[
            dist[ v ] == dist[ u ] + 1, DirectedEdge[ u, v ],
            dist[ u ] == dist[ v ] + 1, DirectedEdge[ v, u ],
            True, Nothing
          ]
        ],
        EdgeList @ UndirectedGraph @ Subgraph[ g, dagVerts ]
      ]
    ]
  ]


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
