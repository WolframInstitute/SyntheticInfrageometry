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

PackageScope[forwardEndpoints]
PackageScope[referenceWalk]
PackageScope[augmentedSpray]
PackageScope[weightedSpray]
PackageScope[deformationSize]
PackageScope[parseDeformationSpec]
PackageScope[toDeformationRules]
PackageScope[normDeformAxis]
PackageScope[axisSpec]
PackageScope[axisOK]
PackageScope[deformationsAt]
PackageScope[deltaDriven]
PackageScope[deltaRange]
PackageScope[sizeDriven]
PackageScope[sizeRange]
PackageScope[windowRange]
PackageScope[windowDeform]


(* ===================== SelectInfraPath / SelectInfraCycle ===================== *)

(* Chainable post-filters on the bundle of paths treated as a finite metric
   space.  Calling triple n_Integer | UpTo[n] | All (default n = 1); options
   "From" (pool selector: All, "Center", "Periphery", "MostVisited", anchor
   -> spec, multi-anchor InfraSegment[{...}] -> spec; "MinLength"
   / "MaxLength" (shortest / longest, circumference for cycles); {"Min", scoreFn} / {"Max", scoreFn}
   with user-supplied path-aggregated scoreFn[path] returning a comparable value),
   "Distance" (mutual-distance constraint: None, "Max", numeric, range), "Metric"
   ("Hausdorff" default, "Frechet", "MeanFrechet"), "MaxCliques".  Wrappers
   preserved: SelectInfraPath accepts InfraSegment[paths] / InfraRay[paths];
   SelectInfraCycle accepts InfraCircle[cycles].
   Operator form: SelectInfraPath[g, n, opts][paths]. *)

Options[ SelectInfraPath ] = {
  "From"       -> All,
  "Distance"   -> None,
  "Metric"     -> "Hausdorff",
  "MaxCliques" -> All
};

Options[ SelectInfraCycle ] = Options[ SelectInfraPath ];


SelectInfraPath[ graph_Graph, paths_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  selectFromPathSpace[ graph, paths, n, False,
    OptionValue[ "From" ], OptionValue[ "Distance" ],
    OptionValue[ "Metric" ], OptionValue[ "MaxCliques" ] ]

SelectInfraPath[ graph_Graph, paths_List, All, opts : OptionsPattern[] ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  SelectInfraPath[ graph, paths, UpTo[ Length[ paths ] ], opts ]

SelectInfraPath[ graph_Graph, paths_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  With[ { result = SelectInfraPath[ graph, paths, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectInfraPath[ graph_Graph, ( head : InfraSegment | InfraLine | InfraPath | InfraRay )[ paths_List ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectInfraPath[ graph, paths, countSpec, opts ] },
    If[ result === $Failed, $Failed, head[ result ] ] ]

(* geodesic-DAG segment, "MostVisited" without distance spread: the most-visited
   geodesic(s) are the longest additive-weight source->sink paths of the DAG under
   node weight c(v) and edge weight w->x = f(w) b(x) (f, b = forward / backward path
   counts; both are geodesic-occupation counts), so only the optimal geodesics
   returned are ever enumerated -- never the whole (possibly astronomical) family. *)
SelectInfraPath[ graph_Graph, InfraSegment[ dag_Graph ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    OptionValue[ SelectInfraPath, { opts }, "From" ] === "MostVisited" &&
    OptionValue[ SelectInfraPath, { opts }, "Distance" ] === None :=
  Module[ { topo = TopologicalSort @ dag, edges = List @@@ EdgeList @ dag,
            source, sink, succ, pred, fwd, bwd, vC, eC, suf, pre, sStar, tight, out, tightDFS },
    source = SelectFirst[ topo, VertexInDegree[ dag, # ] == 0 & ];
    sink   = SelectFirst[ topo, VertexOutDegree[ dag, # ] == 0 & ];
    succ = GroupBy[ edges, First -> Last ];
    pred = GroupBy[ edges, Last -> First ];
    fwd = <| source -> 1 |>;
    Do[ fwd[ w ] = Total @ Lookup[ fwd, Lookup[ pred, w, { } ], 0 ], { w, DeleteCases[ topo, source ] } ];
    bwd = <| sink -> 1 |>;
    Do[ bwd[ w ] = Total @ Lookup[ bwd, Lookup[ succ, w, { } ], 0 ], { w, Reverse @ DeleteCases[ topo, sink ] } ];
    vC = AssociationMap[ fwd[ # ] bwd[ # ] &, topo ];
    eC = AssociationThread[ edges -> ( fwd[ #[[ 1 ]] ] bwd[ #[[ 2 ]] ] & /@ edges ) ];
    suf = <| sink -> vC[ sink ] |>;
    Do[ suf[ w ] = vC[ w ] + Max[ ( eC[ { w, # } ] + suf[ # ] & ) /@ Lookup[ succ, w, { } ] ],
        { w, Reverse @ DeleteCases[ topo, sink ] } ];
    pre = <| source -> vC[ source ] |>;
    Do[ pre[ w ] = vC[ w ] + Max[ ( eC[ { #, w } ] + pre[ # ] & ) /@ Lookup[ pred, w, { } ] ],
        { w, DeleteCases[ topo, source ] } ];
    sStar = suf[ source ];
    tight = AssociationMap[ w |-> Select[ Lookup[ succ, w, { } ], x |-> pre[ w ] + eC[ { w, x } ] + suf[ x ] == sStar ], topo ];
    out = { };
    tightDFS[ path_ ] := If[ Length @ out < countLimit @ countSpec,
      If[ Last @ path === sink, AppendTo[ out, path ],
        Scan[ x |-> tightDFS[ Append[ path, x ] ], tight[ Last @ path ] ] ] ];
    tightDFS[ { source } ];
    With[ { result = SelectInfraPath[ graph, out, countSpec, "From" -> All ] },
      If[ result === $Failed, $Failed, InfraSegment[ result ] ] ]
  ]

(* geodesic-DAG segment: enumerate its geodesics, then select as a path bundle. *)
SelectInfraPath[ graph_Graph, InfraSegment[ dag_Graph ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  SelectInfraPath[ graph, InfraSegment[ dagGeodesics[ dag ] ], countSpec, opts ]

(* List of unary InfraSegment / InfraRay wrappers: route through the bare-paths
   core, re-wrap each result under the matching head. *)

SelectInfraPath[ graph_Graph, list_List,
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list,
          result = SelectInfraPath[ graph, #[[ 1, 1 ]] & /@ list, countSpec, opts ] },
    If[ result === $Failed, $Failed, head[ { # } ] & /@ result ] ]

SelectInfraPath[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectInfraPath[ graph, #, countSpec, opts ] &


SelectInfraCycle[ graph_Graph, cycles_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  selectFromPathSpace[ graph, cycles, n, True,
    OptionValue[ "From" ], OptionValue[ "Distance" ],
    OptionValue[ "Metric" ], OptionValue[ "MaxCliques" ] ]

SelectInfraCycle[ graph_Graph, cycles_List, All, opts : OptionsPattern[] ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  SelectInfraCycle[ graph, cycles, UpTo[ Length[ cycles ] ], opts ]

SelectInfraCycle[ graph_Graph, cycles_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { result = SelectInfraCycle[ graph, cycles, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectInfraCycle[ graph_Graph, InfraCircle[ cycles_List ],
             countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectInfraCycle[ graph, cycles, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraCircle[ result ] ] ]

SelectInfraCycle[ graph_Graph, list_List,
             countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { result = SelectInfraCycle[ graph, #[[ 1, 1 ]] & /@ list, countSpec, opts ] },
    If[ result === $Failed, $Failed, InfraCircle[ { # } ] & /@ result ] ]

SelectInfraCycle[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectInfraCycle[ graph, #, countSpec, opts ] &


(* ===================== EmbeddingClosest ===================== *)

(* Polymorphic closest-by-embedding operator.  Reference shape {p1, p2} picks
   bundle elements closest to the Euclidean segment p1-p2 under GraphEmbedding;
   reference shape {center, radius_?NumericQ} picks bundle elements closest to
   the Euclidean circle of given centre and radius; an arbitrary embedded curve
   (a Line / BSplineCurve / BezierCurve, or a list of >= 3 plane points in the
   embedding's coordinates) picks the bundle element whose embedded polyline is
   closest (plane Hausdorff) to that curve -- i.e. draw any curve over the graph
   embedding and get back the best-approximating path.  Bundle elements may be
   bare vertex sequences, InfraSegment / InfraLine / InfraPath / InfraRay /
   InfraCircle wrappers, or homogeneous lists of unary wrappers; wrappers are
   preserved. *)

(* --- segment-shape: bundle of paths, reference {p1, p2} --- *)

EmbeddingClosest[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    Length[ paths ] <= 1 &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] ) := paths

EmbeddingClosest[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { ep = Lookup[ vertexIndex, { p1, p2 } ] },
      MinimalBy[ paths,
        path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
    ]
  ]

EmbeddingClosest[ graph_Graph, ( head : InfraSegment | InfraLine | InfraPath | InfraRay )[ paths_List ], { p1_, p2_ } ] :=
  head[ EmbeddingClosest[ graph, paths, { p1, p2 } ] ]

EmbeddingClosest[ graph_Graph, InfraSegment[ dag_Graph ], ref_ ] :=
  EmbeddingClosest[ graph, InfraSegment[ dagGeodesics[ dag ] ], ref ]

EmbeddingClosest[ graph_Graph, list_List, { p1_, p2_ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list },
    head[ { # } ] & /@ EmbeddingClosest[ graph, #[[ 1, 1 ]] & /@ list, { p1, p2 } ] ]


(* --- circle-shape: bundle of cycles, reference {center, radius_?NumericQ} --- *)

EmbeddingClosest[ graph_Graph, cycles_List, { center_, radius_?NumericQ } ] /;
    Length[ cycles ] <= 1 &&
    ( cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] ) := cycles

EmbeddingClosest[ graph_Graph, cycles_List, { center_, radius_?NumericQ } ] /;
    cycles === { } || ! AllTrue[ cycles, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { centerIdx = vertexIndex[ center ] },
      MinimalBy[ cycles,
        cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
    ]
  ]

EmbeddingClosest[ graph_Graph, InfraCircle[ cycles_List ], { center_, radius_?NumericQ } ] :=
  InfraCircle[ EmbeddingClosest[ graph, cycles, { center, radius } ] ]

EmbeddingClosest[ graph_Graph, list_List, { center_, radius_?NumericQ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraCircle[ { _ } ] ] ] :=
  InfraCircle[ { # } ] & /@
    EmbeddingClosest[ graph, #[[ 1, 1 ]] & /@ list, { center, radius } ]


(* --- shell-shape: bundle of vertex sets, reference {center, radius_?NumericQ}.
   Bare bundles dispatch by InfraShell[{set}] wrapping; sets are ranked by the
   directed Hausdorff distance from the embedded set to the Euclidean sphere
   of radius r, defined as Max over set vertices of |EuclideanDistance(v, c) - r|.
   Dimension-agnostic and independent of vertex order. *)

EmbeddingClosest[ graph_Graph, InfraShell[ sets_List ], { center_, radius_?NumericQ } ] :=
  InfraShell[ embeddingRankShellSets[ graph, sets, center, radius ] ]

EmbeddingClosest[ graph_Graph, list_List, { center_, radius_?NumericQ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ InfraShell[ { _ } ] ] ] :=
  InfraShell[ { # } ] & /@
    embeddingRankShellSets[ graph, #[[ 1, 1 ]] & /@ list, center, radius ]


embeddingRankShellSets[ graph_Graph, sets_List, center_, radius_ ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { centerPt = coords[[ vertexIndex @ center ]] },
      SortBy[ sets,
        set |-> If[ set === { }, Infinity,
          Max[ Abs[ EuclideanDistance[ centerPt, # ] - radius ] & /@
                coords[[ Lookup[ vertexIndex, set ] ]] ] ] ]
    ]
  ]


(* --- curve-shape: bundle of paths, reference an arbitrary embedded curve.
   The curve is a Line / BSplineCurve / BezierCurve, or a bare list of >= 3 plane
   points in the embedding's coordinates (a length-2 bare list stays a {p1, p2}
   vertex reference -- wrap explicit coordinates in Line[..] to force a curve).
   Returns the bundle element whose embedded polyline is closest (plane
   Hausdorff) to the curve, wrapper head preserved. *)

EmbeddingClosest[ graph_Graph, paths_List, crv_ ] /;
    embeddingCurveQ[ crv ] && Length[ paths ] <= 1 &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] ) := paths

EmbeddingClosest[ graph_Graph, paths_List, crv_ ] /;
    embeddingCurveQ[ crv ] &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] ) :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ],
          curvePts = embeddingCurvePoints[ crv ] },
    MinimalBy[ paths,
      path |-> EmbeddingCurveDistance[ coords, Lookup[ vertexIndex, path ], curvePts ] ]
  ]

EmbeddingClosest[ graph_Graph, ( head : InfraSegment | InfraLine | InfraPath | InfraRay )[ paths_List ], crv_ ] /;
    embeddingCurveQ[ crv ] :=
  head[ EmbeddingClosest[ graph, paths, crv ] ]

EmbeddingClosest[ graph_Graph, list_List, crv_ ] /;
    embeddingCurveQ[ crv ] && list =!= { } &&
    AllTrue[ list, MatchQ[ ( InfraSegment | InfraLine | InfraPath | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list },
    head[ { # } ] & /@ EmbeddingClosest[ graph, #[[ 1, 1 ]] & /@ list, crv ] ]


(* --- operator form --- *)

EmbeddingClosest[ graph_Graph, ref_List ] := EmbeddingClosest[ graph, #, ref ] &

EmbeddingClosest[ graph_Graph, crv : ( _Line | _BSplineCurve | _BezierCurve ) ] :=
  EmbeddingClosest[ graph, #, crv ] &


(* ===================== FindEmbeddingClosestPath ===================== *)

(* Snap an arbitrary embedded curve to a graph walk: sample the curve, map each
   sample to its nearest vertex under the embedding, drop consecutive repeats,
   and join successive anchors by geodesics.  Returns InfraPath[{walk}] tracing
   the curve -- the generative counterpart of EmbeddingClosest's curve selection
   (no bundle to choose from, so the path is constructed).  `curve` is a Line /
   BSplineCurve / BezierCurve or a list of plane points in the embedding's
   coordinates. *)

FindEmbeddingClosestPath[ graph_Graph, curve_ ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          curvePts = embeddingCurvePoints[ curve ] },
    With[ { anchors = First /@ Split[
        Nearest[ coords -> VertexList[ graph ], curvePts ][[ All, 1 ]] ] },
      InfraPath[ { Fold[
        Join[ #1, Rest @ FindShortestPath[ graph, Last @ #1, #2 ] ] &,
        { First @ anchors }, Rest @ anchors ] } ]
    ]
  ]


(* ===================== GeodesicSprayGraph ===================== *)

(* Union of geodesics over a source spec.  Three dispatch shapes:
     [g, c]              -- BFS DAG of all geodesics from c: directed graph on
                            vertices reachable from c, with edge u -> v whenever
                            d(c, v) = d(c, u) + 1 and u-v is a g-edge.
     [g, InfraPoint[vs]] -- multi-source spray: same DAG with d_c replaced by
                            min_i d(ci, v).
     [g, pairs]          -- union of geodesics between the listed vertex pairs;
                            "PathThickness" controls per-pair selection.
   "AxisLength" -> All | k truncates the source-spray DAG at depth k.
   "PathThickness" -> 0 keeps one shortest path per pair; Infinity keeps every
   shortest path; a finite positive value keeps geodesics whose path-Hausdorff
   distance to the first geodesic is at most that threshold.
   "Directed" -> True orients DAG/pair edges from source to sink. *)

Options[ GeodesicSprayGraph ] = {
  "AxisLength"    -> All,
  "PathThickness" -> 0,
  "Directed"      -> True
};

GeodesicSprayGraph[ g_Graph, c_, OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  geodesicSprayFromDistances[ g, AssociationThread[ VertexList[ g ], GraphDistance[ g, c ] ],
    OptionValue[ "AxisLength" ], OptionValue[ "Directed" ] ]

GeodesicSprayGraph[ g_Graph, InfraPoint[ vs_List ], OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  geodesicSprayFromDistances[ g,
    AssociationThread[ VertexList[ g ],
      Min /@ Transpose[ GraphDistance[ g, # ] & /@ vs ] ],
    OptionValue[ "AxisLength" ], OptionValue[ "Directed" ] ]

GeodesicSprayGraph[ g_Graph, pairs : { { _, _ } .. }, OptionsPattern[] ] :=
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


(* ===================== FindForwardDeformation ===================== *)

(* Forward deformations of a walk A -> B in the augmented spray from A: every edge raises
   d(A, .) by 1 (radial) or keeps it equal (transverse).  Relative to the reference walk
   (edge count L) a deformation has LengthDelta = #edges - L (negative when the reference
   is longer than a geodesic) and reroutes a reference arc of DeformationSize edges.  spec
   names at least one of "LengthDelta" / "DeformationSize"; the first-named axis groups
   the output, the other filters and orders within each group.  Each axis takes {x} | x
   (<= x) | {lo, hi}, a bare name is the single minimum by that axis, a bare integer /
   integer list is shorthand for "LengthDelta". *)

FindForwardDeformation[ g_Graph, seg_, spec_, count_ : 1 ] := Module[
  { src, tgt, ref, dA, n, h, axes, cap, groups },
  { src, tgt } = forwardEndpoints[ seg ];
  ref = referenceWalk[ g, seg, src, tgt ];
  dA  = AssociationThread[ VertexList[ g ], GraphDistance[ g, src ] ];
  n   = dA[ tgt ];
  h   = augmentedSpray[ g, dA, n, src, tgt ];
  axes = parseDeformationSpec[ spec ];
  cap = Replace[ count, { All -> Infinity, UpTo[ m_ ] :> m, m_Integer :> m } ];
  groups = If[ axes[[ 1, 1 ]] === "LengthDelta",
     deltaDriven[ h, ref, src, tgt, n, axes[[ 1, 2 ]], axisSpec[ axes, "DeformationSize" ], cap ],
     sizeDriven[ h, dA, ref, src, tgt, n, axes[[ 1, 2 ]], axisSpec[ axes, "LengthDelta" ], cap ] ];
  Which[ groups === { }, InfraPath[ { } ], Length[ groups ] == 1, First[ groups ], True, groups ]
]

(* endpoints A, B of the reference path *)
forwardEndpoints[ InfraSegment[ rs_List, ___ ] ] := { First @ First @ rs, Last @ First @ rs }
forwardEndpoints[ InfraSegment[ dag_Graph ] ]    := { First @ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ], First @ Select[ VertexList[ dag ], VertexOutDegree[ dag, # ] == 0 & ] }
forwardEndpoints[ InfraPath[ rs_List, ___ ] ]    := { First @ First @ rs, Last @ First @ rs }
forwardEndpoints[ w_List ]                          := { First @ w, Last @ w }

(* the reference geodesic DeformationSize is measured against *)
referenceWalk[ g_, InfraSegment[ rs_List, ___ ], _, _ ] := First[ rs ]
referenceWalk[ g_, InfraSegment[ dag_Graph ], _, _ ]    := First @ dagGeodesics[ dag ]
referenceWalk[ g_, InfraPath[ rs_List, ___ ], _, _ ]    := First[ rs ]
referenceWalk[ g_, w_List, src_, tgt_ ] := If[ Length[ w ] >= 3, w, FindShortestPath[ g, src, tgt ] ]

(* directed augmented spray from src: radial (low -> high rank) + transverse (same rank, both
   directions), ranks <= n, trimmed to vertices on some src -> tgt path.  Unweighted, so
   FindPath counts edges (a path A -> B has length n + LengthIncrease). *)
augmentedSpray[ g_, dA_, n_, src_, tgt_ ] := Module[ { edges, raw, keep },
  edges = DeleteCases[ Flatten[ Map[
     e |-> With[ { x = e[[ 1 ]], y = e[[ 2 ]] },
       Which[
         dA[ x ] > n || dA[ y ] > n, { },
         dA[ y ] == dA[ x ] + 1, { DirectedEdge[ x, y ] },
         dA[ x ] == dA[ y ] + 1, { DirectedEdge[ y, x ] },
         dA[ x ] == dA[ y ],     { DirectedEdge[ x, y ], DirectedEdge[ y, x ] },
         True, { } ] ],
     EdgeList[ g ] ], 1 ], { } ];
  raw  = Graph[ Select[ VertexList[ g ], dA[ # ] <= n & ], edges ];
  keep = Intersection[ VertexOutComponent[ raw, src ], VertexInComponent[ raw, tgt ] ];
  Subgraph[ raw, keep ]
]

(* same spray with EdgeWeight radial 0 / transverse 1, so FindShortestPath minimizes #transverse *)
weightedSpray[ h_, dA_ ] := Graph[ VertexList[ h ], EdgeList[ h ],
  EdgeWeight -> Map[ If[ dA[ #[[ 1 ]] ] == dA[ #[[ 2 ]] ], 1, 0 ] &, EdgeList[ h ] ] ]

(* DeformationSize: # edges of the reference geodesic a deformation replaces
   = n - sharedPrefixEdges - sharedSuffixEdges *)
deformationSize[ ref_, def_ ] := With[
  { m = Min[ Length @ ref, Length @ def ] },
  { p = LengthWhile[ Transpose @ { Take[ ref, m ], Take[ def, m ] }, Apply @ SameQ ],
    s = LengthWhile[ Transpose @ { Take[ Reverse @ ref, m ], Take[ Reverse @ def, m ] }, Apply @ SameQ ] },
  Max[ 0, ( Length[ ref ] - 1 ) - ( p - 1 ) - ( s - 1 ) ]
]

(* ordered axis list { { name, normalized }, .. }; the first-named axis drives the grouping *)
parseDeformationSpec[ spec_ ] := Map[ { First[ # ], normDeformAxis[ Last[ # ] ] } &, toDeformationRules[ spec ] ]

toDeformationRules[ s : "LengthDelta" | "DeformationSize" ]                   := { s -> "Min" }
toDeformationRules[ r : Rule[ "LengthDelta" | "DeformationSize", _ ] ]        := { r }
toDeformationRules[ l : { Rule[ "LengthDelta" | "DeformationSize", _ ] .. } ] := l
toDeformationRules[ v_Integer ]                                               := { "LengthDelta" -> v }
toDeformationRules[ v : { __Integer } ]                                       := { "LengthDelta" -> v }

normDeformAxis[ "Min" ]                      := "Min"
normDeformAxis[ { x_Integer } ]              := { "Exact", x }
normDeformAxis[ x_Integer ]                  := { "UpTo", x }
normDeformAxis[ { lo_Integer, hi_Integer } ] := { "Range", lo, hi }

axisSpec[ axes_, name_ ] := With[ { hit = SelectFirst[ axes, First[ # ] === name & ] },
  If[ MissingQ[ hit ], None, Last[ hit ] ] ]

axisOK[ None, _ ]                   := True
axisOK[ "Min", _ ]                  := True
axisOK[ { "Exact", x_ }, v_ ]       := v == x
axisOK[ { "UpTo", x_ }, v_ ]        := v <= x
axisOK[ { "Range", lo_, hi_ }, v_ ] := lo <= v <= hi

(* all forward walks src -> tgt with lo <= #edges <= hi, the reference excluded *)
deformationsAt[ h_, ref_, src_, tgt_, { lo_, hi_ } ] := With[ { lo2 = Max[ lo, 1 ] },
  Which[
    hi < lo2, { },
    hi == lo2, DeleteCases[ FindPath[ h, src, tgt, { hi }, All ], ref ],
    True, DeleteCases[ FindPath[ h, src, tgt, { lo2, hi }, All ], ref ] ] ]

(* LengthDelta drives: enumerate forward walks level by level (length L + k), one group
   per k, filtered and ordered by DeformationSize *)
deltaDriven[ h_, ref_, src_, tgt_, n_, delta_, size_, cap_ ] := With[
  { L = Length[ ref ] - 1 },
  Map[
    k |-> InfraPath @ takeUpTo[
       SortBy[ Select[ deformationsAt[ h, ref, src, tgt, { L + k, L + k } ],
                       axisOK[ size, deformationSize[ ref, # ] ] & ],
               deformationSize[ ref, # ] & ],
       cap ],
    deltaRange[ delta, h, ref, src, tgt, n, size ] ] ]

deltaRange[ { "Exact", k_ }, ___ ]                        := { k }
deltaRange[ { "UpTo", k_ }, h_, ref_, src_, tgt_, n_, _ ] := Range[ n - Length[ ref ] + 1, k ]
deltaRange[ { "Range", lo_, hi_ }, ___ ]                  := Range[ lo, hi ]
deltaRange[ "Min", h_, ref_, src_, tgt_, n_, size_ ] := With[
  { L = Length[ ref ] - 1 },
  { found = SelectFirst[ Range[ n - L, VertexCount[ h ] - 1 - L ],
       k |-> AnyTrue[ deformationsAt[ h, ref, src, tgt, { L + k, L + k } ],
                      axisOK[ size, deformationSize[ ref, # ] ] & ] ] },
  If[ MissingQ[ found ], { }, { found } ] ]

(* DeformationSize drives: with a length-bounding LengthDelta co-axis (or a reference that
   is not itself a forward walk) enumerate all forward walks in the length window and
   regroup by size; otherwise reroute one reference window via the cheapest forward detour
   (one minimal-delta deformation per window, possibly revisiting a vertex) *)
sizeDriven[ h_, dA_, ref_, src_, tgt_, n_, size_, delta_, cap_ ] := With[
  { L = Length[ ref ] - 1 },
  { exhaustive = MatchQ[ delta, { "Exact" | "UpTo" | "Range", __ } ] ||
      ! AllTrue[ Partition[ ref, 2, 1 ], EdgeQ[ h, DirectedEdge @@ # ] & ] },
  If[ exhaustive,
    With[ { all = Select[
         deformationsAt[ h, ref, src, tgt, L + Replace[ delta, {
            { "Exact", k_ } :> { k, k },
            { "UpTo", k_ } :> { n - L, k },
            { "Range", lo_, hi_ } :> { Max[ lo, n - L ], hi },
            None | "Min" :> { n - L, VertexCount[ h ] - 1 - L } } ] ],
         axisOK[ delta, Length[ # ] - 1 - L ] & ] },
      Map[
        s |-> InfraPath @ takeUpTo[
           SortBy[ Select[ all, deformationSize[ ref, # ] == s & ], Length ], cap ],
        sizeRange[ size, ref, all ] ] ],
    With[ { hw = weightedSpray[ h, dA ] },
      Map[
        s |-> InfraPath @ takeUpTo[
           SortBy[ DeleteCases[ Table[ windowDeform[ hw, ref, i, s ], { i, 0, L - s } ], $Failed ], Length ],
           cap ],
        windowRange[ size, hw, ref, L ] ] ] ] ]

sizeRange[ "Min", ref_, all_ ]          := If[ all === { }, { }, { Min[ Map[ deformationSize[ ref, # ] &, all ] ] } ]
sizeRange[ { "Exact", s_ }, ___ ]       := { s }
sizeRange[ { "UpTo", s_ }, ___ ]        := Range[ 1, s ]
sizeRange[ { "Range", lo_, hi_ }, ___ ] := Range[ lo, hi ]

windowRange[ { "Exact", s_ }, ___ ]       := { s }
windowRange[ { "UpTo", s_ }, ___ ]        := Range[ 1, s ]
windowRange[ { "Range", lo_, hi_ }, ___ ] := Range[ lo, hi ]
windowRange[ "Min", hw_, ref_, L_ ] := With[
  { s = SelectFirst[ Range[ 1, L ],
     sVal |-> AnyTrue[ Range[ 0, L - sVal ], iVal |-> windowDeform[ hw, ref, iVal, sVal ] =!= $Failed ] ] },
  If[ MissingQ[ s ], { }, { s } ] ]

(* min-LengthDelta deformation rerouting the reference window [g_i, g_{i+s}]: drop the
   original arc, take the cheapest forward reroute, stitch it back in *)
windowDeform[ hw_, ref_, i_, s_ ] := Module[
  { a = ref[[ i + 1 ]], b = ref[[ i + s + 1 ]], sub, p },
  sub = If[ s == 1, EdgeDelete[ hw, DirectedEdge[ a, b ] ], VertexDelete[ hw, ref[[ i + 2 ;; i + s ]] ] ];
  p = FindShortestPath[ sub, a, b ];
  If[ Length[ p ] < 2, $Failed, Join[ ref[[ 1 ;; i + 1 ]], Rest[ p ], ref[[ i + s + 2 ;; ]] ] ]
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


(* embeddingCurveQ: is `crv` an arbitrary embedded curve reference?  A
   Line / BSplineCurve / BezierCurve, or a bare list of >= 3 plane points.  A
   length-2 bare list is left to the {p1, p2} vertex-reference branch. *)

embeddingCurveQ[ _Line | _BSplineCurve | _BezierCurve ] := True
embeddingCurveQ[ pts_ ] := MatrixQ[ pts, NumericQ ] && Last[ Dimensions[ pts ] ] === 2 && Length[ pts ] >= 3

embeddingCurvePoints[ Line[ pts_ ] ] := pts
embeddingCurvePoints[ BSplineCurve[ pts_, opts___ ] ] :=
  BSplineFunction[ pts, opts ] /@ Subdivide[ 0., 1., Max[ 64, 4 Length[ pts ] ] ]
embeddingCurvePoints[ BezierCurve[ pts_, ___ ] ] :=
  BezierFunction[ pts ] /@ Subdivide[ 0., 1., Max[ 64, 4 Length[ pts ] ] ]
embeddingCurvePoints[ pts_ ] := pts


(* EmbeddingCurveDistance: plane Hausdorff between the embedded polyline of a
   path (a single point when degenerate) and the reference curve's polyline. *)

EmbeddingCurveDistance[ coords_List, path_List, curvePts_List ] :=
  RegionHausdorffDistance[
    If[ Length[ path ] >= 2, Line[ coords[[ path ]] ], Point[ coords[[ First @ path ]] ] ],
    Line[ curvePts ] ]


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


(* ===================== Helpers: SelectInfraPath / SelectInfraCycle core ===================== *)

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

poolPositions[ _Graph, paths_List, "MinLength", _, _, _ ] :=
  With[ { lens = Length /@ paths },
    Flatten @ Position[ lens, Min @ lens, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, paths_List, "MaxLength", _, _, _ ] :=
  With[ { lens = Length /@ paths },
    Flatten @ Position[ lens, Max @ lens, { 1 }, Heads -> False ] ]

poolPositions[ graph_Graph, paths_List, ( anchor_ -> spec_ ), _, baseDist_, cyclic_ ] :=
  anchorDistancePool[ graph, paths, anchor, spec, baseDist, cyclic ]

(* Generic min / max selector: user-supplied scoreFn[path] returning a
   comparable value; pool keeps positions where scoreFn is extremal. *)

poolPositions[ _Graph, paths_List, { "Min", scoreFn_ }, _, _, _ ] :=
  With[ { scores = scoreFn /@ paths },
    Flatten @ Position[ scores, Min @ scores, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, paths_List, { "Max", scoreFn_ }, _, _, _ ] :=
  With[ { scores = scoreFn /@ paths },
    Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ] ]

poolPositions[ _, paths_List, _, _, _, _ ] := Range @ Length @ paths


(* Positions of paths whose total vertex + edge visit count across the
   bundle is maximal. *)

visitPoolPositions[ paths_List, cyclic_ ] :=
  With[ { edgeSeqs = infraRepEdges[ None, If[ cyclic, "Cycles", "Paths" ], # ] & /@ paths },
    With[ { vCounts = Counts @ Catenate @ paths,
            eCounts = Counts @ Catenate @ edgeSeqs },
      With[ { scores = MapThread[
            Total @ Lookup[ vCounts, #1, 0 ] + Total @ Lookup[ eCounts, #2, 0 ] &,
            { paths, edgeSeqs } ] },
        Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ]
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


(* ===================== Helpers: embedding methods ===================== *)

(* parseEmbeddingMethod[spec, poolDefault] extracts the suboptions of
   Method -> "Embedding" on FindInfraMidpoint (the only remaining consumer
   after the path-finding consolidation that dropped "Embedding" from
   FindInfraSegment / FindInfraLine / FindInfraParallel).  Returns
   <| "Coordinates", "Pool", "Pruning" |>. *)

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
   The branching extension list is filtered by applyPruning. *)

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


(* GeodesicSprayGraph construction from a precomputed distance map.  When
   directed -> True, each edge points outward from the source (along increasing
   distance); when False, undirected edges are emitted. *)

geodesicSprayFromDistances[ g_Graph, dist_Association, depthSpec_, directed_ ] :=
  With[ { depth = Replace[ depthSpec, All -> Infinity ] },
    With[ { dagVerts = Select[ VertexList[ g ], dist[ # ] < Infinity && dist[ # ] <= depth & ] },
      Graph[ dagVerts,
        Map[
          e |-> With[ { u = e[[ 1 ]], v = e[[ 2 ]] },
            Which[
              ! directed && Abs[ dist[ u ] - dist[ v ] ] == 1, UndirectedEdge[ u, v ],
              directed && dist[ v ] == dist[ u ] + 1, DirectedEdge[ u, v ],
              directed && dist[ u ] == dist[ v ] + 1, DirectedEdge[ v, u ],
              True, Nothing ] ],
          EdgeList @ UndirectedGraph @ Subgraph[ g, dagVerts ] ]
      ]
    ]
  ]
