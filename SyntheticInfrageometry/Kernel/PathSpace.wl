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


(* ===================== SelectPath / SelectCycle ===================== *)

(* Chainable post-filters on the bundle of paths treated as a finite metric
   space.  Calling triple n_Integer | UpTo[n] | All (default n = 1); options
   "From" (pool selector: All, "Center", "Periphery", "MostVisited", anchor
   -> spec, multi-anchor InfraSegment[{...}] -> spec; "Shortest/LongestCircumference"
   on SelectCycle), "Distance" (mutual-distance constraint: None, "Max",
   numeric, range), "Metric" ("Hausdorff" default, "Frechet", "MeanFrechet"),
   "MaxCliques".  Wrappers preserved: SelectPath accepts InfraSegment[paths]
   / InfraRay[paths]; SelectCycle accepts InfraCircle[cycles].  Operator
   form: SelectPath[g, n, opts][paths]. *)

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

(* List of unary InfraSegment / InfraRay wrappers: route through the bare-paths
   core, re-wrap each result under the matching head. *)

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

SelectCycle[ graph_Graph, list_List,
             countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { result = SelectCycle[ graph, #[[ 1, 1 ]] & /@ list, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraCircle[ { # } ] & /@ result ] ]

SelectCycle[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectCycle[ graph, #, countSpec, opts ] &


(* ===================== EmbeddingClosestPaths / EmbeddingClosestCycles ===================== *)

(* Paths closest to the straight Euclidean segment between p1 and p2 under
   the graph's drawing -- the paths that look most like the chord. *)

EmbeddingClosestPaths[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    Length[ paths ] <= 1 &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] ) := paths

EmbeddingClosestPaths[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { ep = Lookup[ vertexIndex, { p1, p2 } ] },
      MinimalBy[ paths,
        path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
    ]
  ]

EmbeddingClosestPaths[ graph_Graph, InfraSegment[ paths_List ], { p1_, p2_ } ] :=
  InfraSegment[ EmbeddingClosestPaths[ graph, paths, { p1, p2 } ] ]

EmbeddingClosestPaths[ graph_Graph, list_List, { p1_, p2_ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list },
    head[ { # } ] & /@ EmbeddingClosestPaths[ graph, #[[ 1, 1 ]] & /@ list, { p1, p2 } ] ]

EmbeddingClosestPaths[ graph_Graph, { p1_, p2_ } ] :=
  EmbeddingClosestPaths[ graph, #, { p1, p2 } ] &


(* Cycles closest to the Euclidean circle of given centre and radius under
   the graph's drawing -- the visually roundest cycles. *)

EmbeddingClosestCycles[ graph_Graph, cycles_List, { center_, radius_ } ] /;
    Length[ cycles ] <= 1 &&
    ( cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] ) := cycles

EmbeddingClosestCycles[ graph_Graph, cycles_List, { center_, radius_ } ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { centerIdx = vertexIndex[ center ] },
      MinimalBy[ cycles,
        cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
    ]
  ]

EmbeddingClosestCycles[ graph_Graph, InfraCircle[ cycles_List ], { center_, radius_ } ] :=
  InfraCircle[ EmbeddingClosestCycles[ graph, cycles, { center, radius } ] ]

EmbeddingClosestCycles[ graph_Graph, list_List, { center_, radius_ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  InfraCircle[ { # } ] & /@
    EmbeddingClosestCycles[ graph, #[[ 1, 1 ]] & /@ list, { center, radius } ]

EmbeddingClosestCycles[ graph_Graph, { center_, radius_ } ] :=
  EmbeddingClosestCycles[ graph, #, { center, radius } ] &


(* ===================== GeodesicGraph ===================== *)

(* BFS DAG of all geodesics from a source: directed graph on vertices reachable
   from c, with edge u -> v whenever d(c, v) = d(c, u) + 1 and u-v is a g-edge.
   InfraPoint[{c1, ..., ck}] uses the source-set distance min_i d(ci, v).
   "AxisLength" -> All | k truncates the DAG at depth k. *)

Options[ GeodesicGraph ] = { "AxisLength" -> All };

GeodesicGraph[ g_Graph, c_, OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  geodesicGraphFromDistances[ g, AssociationThread[ VertexList[ g ], GraphDistance[ g, c ] ],
    OptionValue[ "AxisLength" ] ]

GeodesicGraph[ g_Graph, InfraPoint[ vs_List ], OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  geodesicGraphFromDistances[ g,
    AssociationThread[ VertexList[ g ],
      Min /@ Transpose[ GraphDistance[ g, # ] & /@ vs ] ],
    OptionValue[ "AxisLength" ] ]


(* ===================== GeodesicSubgraph ===================== *)

(* Union of geodesics between the listed vertex pairs.  "PathThickness" -> 0
   keeps one shortest path per pair; Infinity keeps every shortest path;
   a finite positive value keeps geodesics whose path-Hausdorff distance to
   the first geodesic is at most that threshold.  "Directed" -> True orients
   each path from the first vertex of the pair to the second. *)

Options[ GeodesicSubgraph ] = { "PathThickness" -> 0, "Directed" -> True };

GeodesicSubgraph[ g_Graph, pairs_List, OptionsPattern[] ] :=
  With[ { thickness = OptionValue[ "PathThickness" ],
          directed  = OptionValue[ "Directed" ],
          vertexToIndex = AssociationThread[ VertexList[ g ], Range @ VertexCount[ g ] ],
          distMatrix = GraphDistanceMatrix[ g ] },
    With[ { hausdorff = With[ { dm = distMatrix[[ #1, #2 ]] },
              Max[ Max[ Min /@ dm ], Max[ Min /@ Transpose @ dm ] ] ] & },
      With[ { selectedPaths = Which[
              thickness === 0,
                ( First @ FindPath[ g, #1, #2,
                    { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, 1 ] & ) @@@ pairs,
              thickness === Infinity,
                Flatten[ ( FindPath[ g, #1, #2,
                    { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, All ] & ) @@@ pairs, 1 ],
              True,
                Flatten[
                  ( If[ # === { }, { },
                      With[ { ref = vertexToIndex /@ First[ # ] },
                        Select[ #, path |-> hausdorff[ vertexToIndex /@ path, ref ] <= thickness ] ]
                    ] & ) /@
                  ( ( FindPath[ g, #1, #2,
                      { distMatrix[[ vertexToIndex[ #1 ], vertexToIndex[ #2 ] ]] }, All ] & ) @@@ pairs ),
                  1 ]
            ] },
        GraphUnion @@ ( PathGraph[ #, DirectedEdges -> directed ] & /@ selectedPaths )
      ]
    ]
  ]


(* ===================== PathSubgraph ===================== *)

(* Union of all simple u-v paths of length at most k.  Automatic = geodesic
   case (k = d(u, v)); an integer or UpTo[k] sets the cap; All gives every
   simple u-v path.  "Directed" -> True orients each path u -> v. *)

Options[ PathSubgraph ] = { "Directed" -> True };

PathSubgraph[ g_Graph, u_, v_, lengthSpec : ( _Integer | UpTo[ _Integer ] | All ) : Automatic, OptionsPattern[] ] :=
  With[ { k = Replace[ lengthSpec, {
            Automatic         :> GraphDistance[ g, u, v ],
            UpTo[ n_Integer ] :> n,
            All               -> Infinity,
            n_Integer         :> n } ] },
    If[ u === v, Graph[ { u }, { } ],
      With[ { paths = FindPath[ g, u, v, k, All ] },
        If[ paths === { },
          Graph[ { }, { } ],
          GraphUnion @@ ( PathGraph[ #, DirectedEdges -> OptionValue[ "Directed" ] ] & /@ paths )
        ]
      ]
    ]
  ]


(* ===================== Helpers: path-space metrics ===================== *)

(* HausdorffDistance: symmetric "max one-sided gap" between two vertex sets. *)

HausdorffDistance[ d_List, setX_, setY_ ] :=
  With[ { distSubMatrix = d[[ setX, setY ]] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]

HausdorffDistance[ g_Graph, setX_List, setY_List ] :=
  With[ { distSubMatrix = Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]


(* FrechetDistance: f-reduced order-respecting pairing distance.  Equal-length
   sequences pair element-wise; unequal lengths are aligned by linear
   resampling to the common length max(|X|, |Y|).  f = Max yields the
   classical discrete Frechet; f = Mean yields the mean-Frechet variant. *)

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


resamplePath[ seq_List, m_Integer ] :=
  If[ Length[ seq ] === m, Range[ m ],
    Round @ Rescale[ Range[ m ], { 1, m }, { 1, Length[ seq ] } ]
  ]


(* MinimalSeparationDistance: min graph distance between two vertex subsets. *)

MinimalSeparationDistance[ d_List, setX_, setY_ ] :=
  Min[ d[[ setX, setY ]] ]

MinimalSeparationDistance[ g_Graph, setX_List, setY_List ] :=
  Min[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ]


(* EmbeddingHausdorffDistance: plane Hausdorff between the polyline of an
   embedded path and the straight segment between its endpoints' embeddings.
   Degenerate one-vertex paths score 0. *)

EmbeddingHausdorffDistance[ coords_List, path_List, { p1_, p2_ } ] /; Length[ path ] >= 2 :=
  RegionHausdorffDistance[ Line[ coords[[ path ]] ], Line[ { coords[[ p1 ]], coords[[ p2 ]] } ] ]

EmbeddingHausdorffDistance[ _List, path_List, { _, _ } ] /; Length[ path ] < 2 := 0


(* EmbeddingCircleDistance: plane Hausdorff between a graph cycle (drawn as a
   closed polyline under the embedding) and the Euclidean circle of given
   centre / radius. *)

EmbeddingCircleDistance[ coords_List, cycle_List, centerIdx_Integer, radius_ ] /; Length[ cycle ] >= 3 :=
  With[ { centerPt = coords[[ centerIdx ]], cyclePts = coords[[ cycle ]] },
    With[ { nPts = Max[ 64, 4 * Length[ cycle ] ] },
      With[ { circlePoints = Table[
              centerPt + radius * { Cos[ t ], Sin[ t ] },
              { t, 0, 2 Pi - 2 Pi / nPts, 2 Pi / nPts } ] },
        RegionHausdorffDistance[
          Line[ Append[ cyclePts, First[ cyclePts ] ] ],
          Line[ Append[ circlePoints, First[ circlePoints ] ] ] ]
      ]
    ]
  ]

EmbeddingCircleDistance[ _List, cycle_List, _Integer, _ ] /; Length[ cycle ] < 3 := Infinity


(* Pairwise path-space distance matrix between the supplied paths under
   baseDist.  When cyclic, every cyclic rotation of the second argument is
   tried and the minimum kept (cycle distance is rotation-invariant). *)

pathFilterPairwiseDistances[ graph_Graph, paths_List, baseDist_, cyclic_ ] :=
  With[ { distMatrix = GraphDistanceMatrix[ graph ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { pathDistance = If[ cyclic,
              ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
              baseDist ] },
      ( # + Transpose[ # ] ) & @ PadRight[
        Table[
          pathDistance[ distMatrix, Lookup[ vertexIndex, paths[[ i ]] ], Lookup[ vertexIndex, paths[[ j ]] ] ],
          { i, Length[ paths ] }, { j, i - 1 } ],
        { Length[ paths ], Length[ paths ] } ]
    ]
  ]


pathSpaceMetric[ "Hausdorff"   ] := HausdorffDistance
pathSpaceMetric[ "Frechet"     ] := FrechetDistance
pathSpaceMetric[ "MeanFrechet" ] := FrechetDistance[ ##, Mean ] &
pathSpaceMetric[ _             ] := HausdorffDistance


(* ===================== Helpers: SelectPath / SelectCycle core ===================== *)

(* selectFromPathSpace returns up to nMax paths from the bundle; strict-n
   shortfall handling is left to the caller.  When distSpec is set the
   selection is the max-spread n-clique under the path-space metric. *)

selectFromPathSpace[ _Graph, paths_List, _Integer, _, _, _, _, _ ] /; Length[ paths ] <= 1 := paths

selectFromPathSpace[ graph_Graph, paths_List, nMax_Integer, cyclic_,
                     fromSpec_, distSpec_, metric_, maxCl_ ] :=
  Module[ { baseDist, needsMatrix, pathMatrix, poolIdx, pool, subMatrix,
            finiteMax, range, auxiliaryGraph, cliques, thresholds, picked, n },
    baseDist = pathSpaceMetric[ metric ];
    needsMatrix = MatchQ[ fromSpec, "Center" | "Periphery" | _Rule ] || distSpec =!= None;
    pathMatrix = If[ needsMatrix,
      pathFilterPairwiseDistances[ graph, paths, baseDist, cyclic ], None ];
    poolIdx = poolPositions[ graph, paths, fromSpec, pathMatrix, baseDist, cyclic ];
    If[ poolIdx === { }, Return[ { } ] ];
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
        cliques = { };
        Do[
          auxiliaryGraph = AdjacencyGraph[ pool,
            UnitStep[ subMatrix - d ] * UnitStep[ finiteMax - subMatrix ]
              * ( 1 - IdentityMatrix[ Length[ pool ] ] ) ];
          cliques = FindClique[ auxiliaryGraph, { n, VertexCount[ auxiliaryGraph ] }, maxCl ];
          If[ cliques =!= { }, Break[ ] ],
          { d, thresholds } ];
        If[ cliques === { }, { }, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ],
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
        If[ cliques === { }, { }, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ]
    ];
    picked
  ]


(* Pool selection: positions in paths satisfying the "From" specification. *)

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


(* Positions of paths whose total vertex + edge visit count across the
   bundle is maximal. *)

visitPoolPositions[ paths_List, cyclic_ ] :=
  With[ { edgesOf = If[ cyclic, cycleEdges, sequentialEdges ] },
    With[ { edgeSeqs = edgesOf /@ paths },
      With[ { vCounts = Counts @ Catenate @ paths,
              eCounts = Counts @ Catenate @ edgeSeqs },
        With[ { scores = MapThread[
              Total @ Lookup[ vCounts, #1, 0 ] + Total @ Lookup[ eCounts, #2, 0 ] &,
              { paths, edgeSeqs } ] },
          Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ]
        ]
      ]
    ]
  ]


(* Positions of paths whose path-space distance to each anchor satisfies spec
   (numeric d, {dMin, dMax} range, or "Max" -- max distance from each anchor
   individually). *)

anchorDistancePool[ graph_Graph, paths_List, anchor_, spec_, baseDist_, cyclic_ ] :=
  With[ { anchors = Replace[ anchor, {
            ( InfraSegment | InfraRay | InfraCircle )[ reps_List ] :> reps,
            seq_List /; AllTrue[ seq, ListQ ] :> seq,
            seq_List :> { seq } } ] },
    If[ anchors === { }, { },
      With[ { distMatrix = GraphDistanceMatrix[ graph ],
              vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
        With[ { distFn = If[ cyclic,
                  ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
                  baseDist ] },
          With[ { anchorRows = Table[
                  Map[ p |-> distFn[ distMatrix,
                    Lookup[ vertexIndex, a ], Lookup[ vertexIndex, p ] ], paths ],
                  { a, anchors } ] },
            Select[ Range @ Length @ paths,
              i |-> AllTrue[ Range @ Length @ anchors,
                a |-> anchorMatchQ[ anchorRows[[ a, i ]], anchorRows[[ a ]], spec ] ] ]
          ]
        ]
      ]
    ]
  ]


anchorMatchQ[ d_?NumericQ, _, target_?NumericQ ]                  := d == target
anchorMatchQ[ d_?NumericQ, _, { lo_?NumericQ, hi_?NumericQ } ]    := lo <= d <= hi
anchorMatchQ[ d_?NumericQ, allDistsForAnchor_List, "Max" ]        :=
  d == Max @ Select[ allDistsForAnchor, NumericQ ]
anchorMatchQ[ _, _, _ ] := False


sequentialEdges[ path_List ] :=
  If[ Length[ path ] >= 2, Sort /@ Partition[ path, 2, 1 ], { } ]

cycleEdges[ cycle_List ] :=
  With[ { closed = If[ Length[ cycle ] >= 2 && First @ cycle === Last @ cycle,
                       cycle, Append[ cycle, First @ cycle ] ] },
    If[ Length[ closed ] >= 2, Sort /@ Partition[ closed, 2, 1 ], { } ]
  ]


(* ===================== Helpers: embedding methods ===================== *)

(* parseEmbeddingMethod[spec, poolDefault] extracts the suboptions of
   Method -> "Embedding" into <| "Coordinates", "Pool", "Pruning" |>. *)

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


(* Coordinate matrix from the "Coordinates" suboption value: Automatic =
   GraphEmbedding under SpringEmbedding (the closest built-in to the
   edge-length-preserving criterion); else the user-supplied matrix. *)

resolveEmbeddingCoords[ graph_Graph, Automatic ] :=
  GraphEmbedding[ Graph[ graph, GraphLayout -> "SpringEmbedding" ] ]
resolveEmbeddingCoords[ _, coords_List ] := coords


(* generateEmbeddingPaths: depth-first recursion extending each partial path
   via extendFn[path]; complete candidates (goalQ[path] True) are collected.
   The branching extension list is filtered by applyPruning, so the same
   Pruning spec used by Method -> "ShortestPathExtension" controls the
   search. *)

generateEmbeddingPaths[ extendFn_, startPath_List, goalQ_, prune_ ] :=
  Module[ { results = { }, recurse },
    recurse[ path_ ] :=
      If[ TrueQ[ goalQ[ path ] ],
        AppendTo[ results, path ],
        Scan[
          candidate |-> recurse[ Append[ path, candidate ] ],
          applyPruning[ extendFn[ path ], prune ] ] ];
    recurse[ startPath ];
    results
  ]


(* ===================== Helpers: geodesic DAG ===================== *)

(* geodesicDAGNeighbors[g, u, v]: vertex -> {downstream DAG neighbours} for
   every vertex w on some geodesic from u to v.  Directed paths through the
   DAG are exactly the u-v geodesics.
   geodesicDAGNeighbors[g, c]: single-source form; directed paths c -> sink
   are exactly the maximal geodesics from c. *)

geodesicDAGNeighbors[ graph_Graph, u_, v_ ] :=
  With[ { du = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, u ] ],
          dv = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, v ] ] },
    With[ { total = du[ v ] },
      If[ total === Infinity, <||>,
        With[ { dagVerts = Select[ VertexList[ graph ], du[ # ] + dv[ # ] == total & ] },
          With[ { dagSet = AssociationThread[ dagVerts, True ] },
            AssociationMap[
              w |-> Select[ AdjacencyList[ graph, w ],
                TrueQ[ dagSet[ # ] ] && du[ # ] == du[ w ] + 1 & ],
              dagVerts ]
          ]
        ]
      ]
    ]
  ]

geodesicDAGNeighbors[ graph_Graph, c_ ] :=
  With[ { dc = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, c ] ] },
    With[ { dagVerts = Select[ VertexList[ graph ], dc[ # ] < Infinity & ] },
      With[ { dagSet = AssociationThread[ dagVerts, True ] },
        AssociationMap[
          w |-> Select[ AdjacencyList[ graph, w ],
            TrueQ[ dagSet[ # ] ] && dc[ # ] == dc[ w ] + 1 & ],
          dagVerts ]
      ]
    ]
  ]


(* GeodesicGraph construction from a precomputed distance map. *)

geodesicGraphFromDistances[ g_Graph, dist_Association, depthSpec_ ] :=
  With[ { depth = Replace[ depthSpec, All -> Infinity ] },
    With[ { dagVerts = Select[ VertexList[ g ], dist[ # ] < Infinity && dist[ # ] <= depth & ] },
      Graph[ dagVerts,
        Map[
          e |-> With[ { u = e[[ 1 ]], v = e[[ 2 ]] },
            Which[
              dist[ v ] == dist[ u ] + 1, DirectedEdge[ u, v ],
              dist[ u ] == dist[ v ] + 1, DirectedEdge[ v, u ],
              True, Nothing ] ],
          EdgeList @ UndirectedGraph @ Subgraph[ g, dagVerts ] ]
      ]
    ]
  ]
