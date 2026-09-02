Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[HausdorffDistance]
PackageScope[FrechetDistance]
PackageScope[MinimalSeparationDistance]
PackageScope[EmbeddingHausdorffDistance]
PackageScope[EmbeddingCircleDistance]
PackageScope[pathFilterPairwiseDistances]
PackageScope[geodesicDAGNeighbors]
PackageScope[resolveEmbeddingCoords]
PackageScope[parseEmbeddingMethod]

PackageScope[deformationReference]


(* ===================== SelectInfraWalk ===================== *)

(* Chainable post-filters on the bundle of walks treated as a finite metric
   space -- polymorphic on the wrapper head, since a cycle is a closed walk
   and circumference plays the role of length for closed heads.  Calling
   triple n_Integer | UpTo[n] | All (default n = 1); options "From" (pool
   selector: All, "Center", "Periphery", "MostVisited" (max total occupation),
   "Bottleneck" (max-min occupation, widest corridor), anchor -> spec,
   multi-anchor InfraSegment[{...}] -> spec; "MinLength" / "MaxLength"
   (shortest / longest, circumference for closed heads); {"Min", scoreFn} /
   {"Max", scoreFn} with user-supplied walk-aggregated scoreFn[walk] returning
   a comparable value), "Distance" (mutual-distance constraint: None, "Max",
   numeric, range), "Metric" ("Hausdorff" default, "Frechet", "MeanFrechet"),
   "MaxCliques", "Cyclic" -> False (default) | True (rotation-invariant
   metrics and closing edge; only meaningful on a BARE list -- a wrapper head
   already says whether it is closed).  Wrappers preserved: InfraSegment /
   InfraLine / InfraWalk / InfraRay for open walks; InfraCircle / InfraLoop /
   InfraString for closed ones (auto-force "Cyclic" -> True).
   Operator form: SelectInfraWalk[g, n, opts][walks]. *)

SelectInfraWalk::badfrom = "\"From\" specification `1` is not supported by SelectInfraWalk. Supported: All, \"Center\", \"Periphery\", \"MostVisited\", \"Bottleneck\", \"MinLength\", \"MaxLength\", anchor -> spec, {\"Min\", scoreFn}, {\"Max\", scoreFn}.";

Options[ SelectInfraWalk ] = {
  "From"       -> All,
  "Distance"   -> None,
  "Metric"     -> "Hausdorff",
  "MaxCliques" -> All,
  "Cyclic"     -> False
};


SelectInfraWalk[ graph_Graph, walks_List, UpTo[ n_Integer ], opts : OptionsPattern[] ] /;
    walks === { } || ! AllTrue[ walks,
      MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay | InfraCircle | InfraLoop | InfraString )[ { _ } ] ] ] :=
  With[ { from = OptionValue[ "From" ] },
    If[ ! fromWalkSpecQ[ from ],
      Message[ SelectInfraWalk::badfrom, from ]; $Failed,
      selectFromWalkSpace[ graph, walks, n, TrueQ @ OptionValue[ "Cyclic" ], from, OptionValue[ "Distance" ],
        OptionValue[ "Metric" ], OptionValue[ "MaxCliques" ] ] ] ]

SelectInfraWalk[ graph_Graph, walks_List, All, opts : OptionsPattern[] ] /;
    walks === { } || ! AllTrue[ walks,
      MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay | InfraCircle | InfraLoop | InfraString )[ { _ } ] ] ] :=
  SelectInfraWalk[ graph, walks, UpTo[ Length[ walks ] ], opts ]

SelectInfraWalk[ graph_Graph, walks_List, n_Integer : 1, opts : OptionsPattern[] ] /;
    walks === { } || ! AllTrue[ walks,
      MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay | InfraCircle | InfraLoop | InfraString )[ { _ } ] ] ] :=
  With[ { result = SelectInfraWalk[ graph, walks, UpTo[ n ], opts ] },
    If[ ListQ[ result ] && Length[ result ] < n, $Failed, result ] ]

SelectInfraWalk[ graph_Graph, ( head : InfraSegment | InfraLine | InfraWalk | InfraRay )[ walks_List ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectInfraWalk[ graph, walks, countSpec, "Cyclic" -> False, opts ] },
    If[ result === $Failed, $Failed, head[ result ] ] ]

(* closed-walk wrapper: unwrap, select with circumference-as-length, rewrap *)
SelectInfraWalk[ graph_Graph, ( head : InfraCircle | InfraLoop | InfraString )[ cycles_List ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { result = SelectInfraWalk[ graph, cycles, countSpec, "Cyclic" -> True, opts ] },
    If[ result === $Failed, $Failed, head[ result ] ] ]

(* geodesic-DAG segment, "MostVisited" without distance spread: the most-visited
   geodesic(s) are the longest additive-weight source->sink paths of the DAG under
   node weight c(v) and edge weight w->x = f(w) b(x) (f, b = forward / backward path
   counts; both are geodesic-occupation counts), so only the optimal geodesics
   returned are ever enumerated -- never the whole (possibly astronomical) family. *)
SelectInfraWalk[ graph_Graph, InfraSegment[ dag_Graph ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    OptionValue[ SelectInfraWalk, { opts }, "From" ] === "MostVisited" &&
    OptionValue[ SelectInfraWalk, { opts }, "Distance" ] === None :=
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
    With[ { result = SelectInfraWalk[ graph, out, countSpec, "From" -> All ] },
      If[ result === $Failed, $Failed, InfraSegment[ result ] ] ]
  ]

SelectInfraWalk[ graph_Graph, InfraSegment[ dag_Graph ],
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  SelectInfraWalk[ graph, InfraSegment[ dagGeodesics[ dag ] ], countSpec, opts ]

(* List of unary open-walk wrappers: route through the bare-walk core, re-wrap
   each result under the matching head. *)

SelectInfraWalk[ graph_Graph, list_List,
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list,
          result = SelectInfraWalk[ graph, #[[ 1, 1 ]] & /@ list, countSpec, "Cyclic" -> False, opts ] },
    If[ result === $Failed, $Failed, head[ { # } ] & /@ result ] ]

(* List of unary closed-walk wrappers: same, forcing circumference-as-length. *)

SelectInfraWalk[ graph_Graph, list_List,
            countSpec : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraCircle | InfraLoop | InfraString )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list,
          result = SelectInfraWalk[ graph, #[[ 1, 1 ]] & /@ list, countSpec, "Cyclic" -> True, opts ] },
    If[ result === $Failed, $Failed, head[ { # } ] & /@ result ] ]

SelectInfraWalk[ graph_Graph, countSpec : ( _Integer | UpTo[ _Integer ] | All ), opts : OptionsPattern[] ] :=
  SelectInfraWalk[ graph, #, countSpec, opts ] &


(* ===================== EmbeddingClosest ===================== *)

(* Polymorphic closest-by-embedding operator.  Reference shape {p1, p2} picks
   bundle elements closest to the Euclidean segment p1-p2 under GraphEmbedding;
   reference shape {center, radius_?NumericQ} picks bundle elements closest to
   the Euclidean circle of given centre and radius; an arbitrary embedded curve
   (a Line / BSplineCurve / BezierCurve, or a list of >= 3 plane points in the
   embedding's coordinates) picks the bundle element whose embedded polyline is
   closest (plane Hausdorff) to that curve -- i.e. draw any curve over the graph
   embedding and get back the best-approximating path.  Bundle elements may be
   bare vertex sequences, InfraSegment / InfraLine / InfraWalk / InfraRay /
   InfraCircle wrappers, or homogeneous lists of unary wrappers; wrappers are
   preserved. *)

(* --- segment-shape: bundle of paths, reference {p1, p2} --- *)

EmbeddingClosest[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    Length[ paths ] <= 1 &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] ) := paths

EmbeddingClosest[ graph_Graph, paths_List, { p1_, p2_ } ] /;
    paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    { ep = Lookup[ vertexIndex, { p1, p2 } ] },
    MinimalBy[ paths,
      path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
  ]

EmbeddingClosest[ graph_Graph, ( head : InfraSegment | InfraLine | InfraWalk | InfraRay )[ paths_List ], { p1_, p2_ } ] :=
  head[ EmbeddingClosest[ graph, paths, { p1, p2 } ] ]

EmbeddingClosest[ graph_Graph, InfraSegment[ dag_Graph ], ref_ ] :=
  EmbeddingClosest[ graph, InfraSegment[ dagGeodesics[ dag ] ], ref ]

EmbeddingClosest[ graph_Graph, list_List, { p1_, p2_ } ] /;
    list =!= { } && AllTrue[ list, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] :=
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
    { centerIdx = vertexIndex[ center ] },
    MinimalBy[ cycles,
      cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
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
    { centerPt = coords[[ vertexIndex @ center ]] },
    SortBy[ sets,
      set |-> If[ set === { }, Infinity,
        Max[ Abs[ EuclideanDistance[ centerPt, # ] - radius ] & /@
              coords[[ Lookup[ vertexIndex, set ] ]] ] ] ]
  ]


(* --- curve-shape: bundle of paths, reference an arbitrary embedded curve.
   The curve is a Line / BSplineCurve / BezierCurve, or a bare list of >= 3 plane
   points in the embedding's coordinates (a length-2 bare list stays a {p1, p2}
   vertex reference -- wrap explicit coordinates in Line[..] to force a curve).
   Returns the bundle element whose embedded polyline is closest (plane
   Hausdorff) to the curve, wrapper head preserved. *)

EmbeddingClosest[ graph_Graph, paths_List, crv_ ] /;
    embeddingCurveQ[ crv ] && Length[ paths ] <= 1 &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] ) := paths

EmbeddingClosest[ graph_Graph, paths_List, crv_ ] /;
    embeddingCurveQ[ crv ] &&
    ( paths === { } || ! AllTrue[ paths, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] ) :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ],
          curvePts = embeddingCurvePoints[ crv ] },
    MinimalBy[ paths,
      path |-> EmbeddingCurveDistance[ coords, Lookup[ vertexIndex, path ], curvePts ] ]
  ]

EmbeddingClosest[ graph_Graph, ( head : InfraSegment | InfraLine | InfraWalk | InfraRay )[ paths_List ], crv_ ] /;
    embeddingCurveQ[ crv ] :=
  head[ EmbeddingClosest[ graph, paths, crv ] ]

EmbeddingClosest[ graph_Graph, list_List, crv_ ] /;
    embeddingCurveQ[ crv ] && list =!= { } &&
    AllTrue[ list, MatchQ[ ( InfraSegment | InfraLine | InfraWalk | InfraRay )[ { _ } ] ] ] :=
  With[ { head = Head @ First @ list },
    head[ { # } ] & /@ EmbeddingClosest[ graph, #[[ 1, 1 ]] & /@ list, crv ] ]


(* --- operator form --- *)

EmbeddingClosest[ graph_Graph, ref_List ] := EmbeddingClosest[ graph, #, ref ] &

EmbeddingClosest[ graph_Graph, crv : ( _Line | _BSplineCurve | _BezierCurve ) ] :=
  EmbeddingClosest[ graph, #, crv ] &


(* ===================== FindEmbeddingClosestPath ===================== *)

(* Snap an arbitrary embedded curve to a graph walk: sample the curve, map each
   sample to its nearest vertex under the embedding, drop consecutive repeats,
   and join successive anchors by geodesics.  Returns InfraWalk[{walk}] tracing
   the curve -- the generative counterpart of EmbeddingClosest's curve selection
   (no bundle to choose from, so the path is constructed).  `curve` is a Line /
   BSplineCurve / BezierCurve or a list of plane points in the embedding's
   coordinates. *)

FindEmbeddingClosestPath[ graph_Graph, curve_ ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, Automatic ],
          curvePts = embeddingCurvePoints[ curve ] },
    { anchors = First /@ Split[
        Nearest[ coords -> VertexList[ graph ], curvePts ][[ All, 1 ]] ] },
    InfraWalk[ { Fold[
      Join[ #1, Rest @ FindShortestPath[ graph, Last @ #1, #2 ] ] &,
      { First @ anchors }, Rest @ anchors ] } ]
  ]


(* ===================== GeodesicSprayGraph ===================== *)

(* Union of geodesics over a source spec.  Three dispatch shapes:
     [g, c]              -- BFS DAG of all geodesics from c: directed graph on
                            vertices reachable from c, with edge u -> v whenever
                            d(c, v) = d(c, u) + 1 and u-v is a g-edge.
     [g, InfraSet[vs]] -- multi-source spray: same DAG with d_c replaced by
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

(* an InfraPoint atom is the natural single source; an InfraSet or a list of
   atoms is the multi-source form *)
GeodesicSprayGraph[ g_Graph, InfraPoint[ v_ ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], v ] :=
  GeodesicSprayGraph[ g, v, opts ]

GeodesicSprayGraph[ g_Graph, list : { __InfraPoint }, opts : OptionsPattern[] ] :=
  GeodesicSprayGraph[ g, InfraSet[ #[[ 1 ]] & /@ list ], opts ]

GeodesicSprayGraph[ g_Graph, InfraSet[ vs_List ], OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  geodesicSprayFromDistances[ g,
    AssociationThread[ VertexList[ g ],
      Min /@ Transpose[ GraphDistance[ g, # ] & /@ vs ] ],
    OptionValue[ "AxisLength" ], OptionValue[ "Directed" ] ]

GeodesicSprayGraph[ g_Graph, pairs : { { _, _ } .. }, OptionsPattern[] ] :=
  With[ { thickness = OptionValue[ "PathThickness" ],
          directed  = OptionValue[ "Directed" ],
          vertexToIndex = AssociationThread[ VertexList[ g ], Range @ VertexCount[ g ] ],
          distMatrix = GraphDistanceMatrix[ g ] },
    { hausdorff = With[ { dm = distMatrix[[ #1, #2 ]] },
        Max[ Max[ Min /@ dm ], Max[ Min /@ Transpose @ dm ] ] ] & },
    { selectedPaths = Which[
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


(* ===================== FindInfraMonotoneDeformation ===================== *)

(* Monotone deformations of a walk A -> B: the walks A -> B along which a
   potential never decreases.  Under the default potential d(A, .) every edge is
   either radial (rank +1) or transverse (same rank), so a deformation is a walk
   that never steps back toward A -- the discrete variation of a geodesic with
   fixed endpoints, monotone in the radial coordinate.  Relative to the
   reference (edge count L) it has LengthDelta = #edges - L, negative when the
   reference is not itself monotone-shortest.  deltaSpec is a kspec relative to
   L and is what makes the class finite: transverse edges let a walk shuffle
   inside one level set forever, so an unbounded deltaSpec is refused.  Output
   is flat, sorted by (LengthDelta, InfraDeformationSize). *)

FindInfraMonotoneDeformation::unbounded =
  "The monotone deformation class is infinite without a length bound: give a finite deltaSpec (k, {k}, or {kmin, kmax}).";

Options[ FindInfraMonotoneDeformation ] = { "Potential" -> Automatic };

FindInfraMonotoneDeformation[ graph_Graph, ref_, deltaSpec_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : All,
    opts : OptionsPattern[ ] ] :=
  Catch @ Module[ { refWalk, src, tgt, pot, spray, distToTgt, outs, len, minLen, lo, hi },
    refWalk = deformationReference[ graph, ref ];
    { src, tgt } = { First @ refWalk, Last @ refWalk };
    len = Length[ refWalk ] - 1;
    pot = With[ { spec = OptionValue[ FindInfraMonotoneDeformation, { opts }, "Potential" ] },
      AssociationThread[ VertexList @ graph,
        Which[
          spec === Automatic,     GraphDistance[ graph, src ],
          VertexQ[ graph, spec ], GraphDistance[ graph, spec ],
          True,                   spec /@ VertexList @ graph ] ] ];
    (* no monotone walk into tgt ever climbs above pot[tgt], so capping the
       vertex set there is implied by the rule rather than an extra constraint *)
    spray = With[ {
        edges = Flatten[ Map[
          e |-> With[ { x = First @ e, y = Last @ e },
            Which[
              pot[ x ] > pot[ tgt ] || pot[ y ] > pot[ tgt ], { },
              pot[ y ] > pot[ x ], { DirectedEdge[ x, y ] },
              pot[ x ] > pot[ y ], { DirectedEdge[ y, x ] },
              True, { DirectedEdge[ x, y ], DirectedEdge[ y, x ] } ] ],
          EdgeList[ graph ] ], 1 ] },
      { raw = Graph[ Union[ Select[ VertexList @ graph, pot[ # ] <= pot[ tgt ] & ], { src, tgt } ], edges ] },
      Subgraph[ raw, Intersection[ VertexOutComponent[ raw, src ], VertexInComponent[ raw, tgt ] ] ] ];
    If[ ! ( VertexQ[ spray, src ] && VertexQ[ spray, tgt ] ), Throw[ InfraWalk[ { } ] ] ];
    distToTgt = AssociationThread[ VertexList @ spray, GraphDistance[ ReverseGraph @ spray, tgt ] ];
    minLen = distToTgt[ src ];
    { lo, hi } = Replace[ deltaSpec, {
      Automatic         :> { minLen - len, minLen - len },
      { k_Integer }     :> { k, k },
      { l_Integer, h_ } :> { Max[ l, minLen - len ], h },
      k_Integer         :> { minLen - len, k },
      _                 -> { minLen - len, Infinity } } ];
    If[ ! IntegerQ[ hi ],
      Message[ FindInfraMonotoneDeformation::unbounded ]; Throw[ $Failed ] ];
    outs = GroupBy[ EdgeList @ spray, First -> Last ];
    InfraWalk @ takeUpTo[
      SortBy[
        DeleteCases[
          Select[
            (* a step is grown only if tgt stays reachable within the length budget:
               on the cyclic spray this keeps the sweep proportional to the output
               instead of exponential in len + hi *)
            frontierSweep[ spray, src, tgt,
              { g, walk } |-> Select[ Lookup[ outs, Key[ Last @ walk ], { } ],
                Length[ walk ] + distToTgt[ # ] <= len + hi & ],
              Infinity, Infinity ],
            Length[ # ] - 1 >= len + lo & ],
          refWalk ],
        { Length, InfraDeformationSize[ refWalk, # ] & } ],
      countLimit @ count ]
  ]

(* the reference walk: deltas and sizes are measured against it, and its
   endpoints are the deformation endpoints.  A bare pair {A, B} specifies
   endpoints rather than a walk, so it stands for the canonical geodesic. *)
deformationReference[ _, InfraSegment[ rs_List, ___ ] ] := First @ rs
deformationReference[ _, InfraSegment[ dag_Graph ] ]    := First @ dagGeodesics[ dag, 1 ]
deformationReference[ _, InfraWalk[ rs_List, ___ ] ]    := First @ rs
deformationReference[ graph_, w_List ]                  :=
  If[ Length[ w ] >= 3, w, FindShortestPath[ graph, First @ w, Last @ w ] ]


(* InfraDeformationSize: how many edges of the reference a deformation replaces
   = L - sharedPrefixEdges - sharedSuffixEdges.  One-sided -- measured in the
   reference's edges -- so it is not a symmetric walk-space metric. *)

InfraDeformationSize[ ref_, InfraWalk[ rs_List, ___ ] ] := InfraDeformationSize[ ref, # ] & /@ rs

InfraDeformationSize[ InfraWalk[ rs_List, ___ ], def_List ]    := InfraDeformationSize[ First @ rs, def ]
InfraDeformationSize[ InfraSegment[ rs_List, ___ ], def_List ] := InfraDeformationSize[ First @ rs, def ]

InfraDeformationSize[ ref_List, def_List ] := With[
  { m = Min[ Length @ ref, Length @ def ] },
  { p = LengthWhile[ Transpose @ { Take[ ref, m ], Take[ def, m ] }, Apply @ SameQ ],
    s = LengthWhile[ Transpose @ { Take[ Reverse @ ref, m ], Take[ Reverse @ def, m ] }, Apply @ SameQ ] },
  Max[ 0, ( Length[ ref ] - 1 ) - ( p - 1 ) - ( s - 1 ) ]
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
    { nPts = Max[ 64, 4 * Length[ cycle ] ] },
    { circlePoints = Table[
        centerPt + radius * { Cos[ t ], Sin[ t ] },
        { t, 0, 2 Pi - 2 Pi / nPts, 2 Pi / nPts } ] },
    RegionHausdorffDistance[
      Line[ Append[ cyclePts, First[ cyclePts ] ] ],
      Line[ Append[ circlePoints, First[ circlePoints ] ] ] ]
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
    { pathDistance = If[ cyclic,
        ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
        baseDist ] },
    ( # + Transpose[ # ] ) & @ PadRight[
      Table[
        pathDistance[ distMatrix, Lookup[ vertexIndex, paths[[ i ]] ], Lookup[ vertexIndex, paths[[ j ]] ] ],
        { i, Length[ paths ] }, { j, i - 1 } ],
      { Length[ paths ], Length[ paths ] } ]
  ]


pathSpaceMetric[ "Hausdorff"   ] := HausdorffDistance
pathSpaceMetric[ "Frechet"     ] := FrechetDistance
pathSpaceMetric[ "MeanFrechet" ] := FrechetDistance[ ##, Mean ] &
pathSpaceMetric[ _             ] := HausdorffDistance


(* ===================== Helpers: SelectInfraWalk / SelectInfraWalk core ===================== *)

(* selectFromWalkSpace returns up to nMax paths from the bundle; strict-n
   shortfall handling is left to the caller.  When distSpec is set the
   selection is the max-spread n-clique under the path-space metric. *)

selectFromWalkSpace[ _Graph, paths_List, _Integer, _, _, _, _, _ ] /; Length[ paths ] <= 1 := paths

selectFromWalkSpace[ graph_Graph, paths_List, nMax_Integer, cyclic_,
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


(* Admissible "From" specifications, tested before dispatch: an unrecognised
   selector must raise ::badfrom rather than fall through to the whole bundle,
   which reads as a legitimate random draw. *)

fromWalkSpecQ[ spec_ ] :=
  MatchQ[ spec, All | "Center" | "Periphery" | "MostVisited" | "Bottleneck"
                | "MinLength" | "MaxLength" | _Rule | { "Min" | "Max", _ } ]


poolPositions[ _Graph, paths_List, All, _, _, _ ] := Range @ Length @ paths

poolPositions[ _Graph, _List, "Center", pathMatrix_List, _, _ ] :=
  With[ { scores = Max /@ pathMatrix },
    Flatten @ Position[ scores, Min @ scores, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, _List, "Periphery", pathMatrix_List, _, _ ] :=
  With[ { scores = Max /@ pathMatrix },
    Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ] ]

poolPositions[ _Graph, paths_List, "MostVisited", _, _, cyclic_ ] :=
  visitPoolPositions[ paths, cyclic, Total ]

poolPositions[ _Graph, paths_List, "Bottleneck", _, _, cyclic_ ] :=
  visitPoolPositions[ paths, cyclic, Min ]

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


(* Positions of paths extremising the bundle visit counts of their vertices and
   edges: agg = Total picks max total occupation ("MostVisited"), agg = Min picks
   the max-min bottleneck (widest continuous corridor, "Bottleneck"). *)

visitPoolPositions[ paths_List, cyclic_, agg_ ] :=
  With[ { edgeSeqs = infraRepEdges[ None, If[ cyclic, "Cycles", "Paths" ], # ] & /@ paths },
    { vCounts = Counts @ Catenate @ paths,
      eCounts = Counts @ Catenate @ edgeSeqs },
    { scores = MapThread[
        agg @ Join[ Lookup[ vCounts, #1, 0 ], Lookup[ eCounts, #2, 0 ] ] &,
        { paths, edgeSeqs } ] },
    Flatten @ Position[ scores, Max @ scores, { 1 }, Heads -> False ]
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
        { distFn = If[ cyclic,
            ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
            baseDist ] },
        { anchorRows = Table[
            Map[ p |-> distFn[ distMatrix,
              Lookup[ vertexIndex, a ], Lookup[ vertexIndex, p ] ], paths ],
            { a, anchors } ] },
        Select[ Range @ Length @ paths,
          i |-> AllTrue[ Range @ Length @ anchors,
            a |-> anchorMatchQ[ anchorRows[[ a, i ]], anchorRows[[ a ]], spec ] ] ]
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


(* ===================== Helpers: geodesic DAG ===================== *)

(* geodesicDAGNeighbors[g, u, v]: vertex -> {downstream DAG neighbours} for
   every vertex w on some geodesic from u to v.  Directed paths through the
   DAG are exactly the u-v geodesics.
   geodesicDAGNeighbors[g, c]: single-source form; directed paths c -> sink
   are exactly the maximal geodesics from c. *)

geodesicDAGNeighbors[ graph_Graph, u_, v_ ] :=
  With[ { du = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, u ] ],
          dv = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, v ] ] },
    { total = du[ v ] },
    If[ total === Infinity, <||>,
      With[ { dagVerts = Select[ VertexList[ graph ], du[ # ] + dv[ # ] == total & ] },
        { dagSet = AssociationThread[ dagVerts, True ] },
        AssociationMap[
          w |-> Select[ AdjacencyList[ graph, w ],
            TrueQ[ dagSet[ # ] ] && du[ # ] == du[ w ] + 1 & ],
          dagVerts ]
      ]
    ]
  ]

geodesicDAGNeighbors[ graph_Graph, c_ ] :=
  With[ { dc = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, c ] ] },
    { dagVerts = Select[ VertexList[ graph ], dc[ # ] < Infinity & ] },
    { dagSet = AssociationThread[ dagVerts, True ] },
    AssociationMap[
      w |-> Select[ AdjacencyList[ graph, w ],
        TrueQ[ dagSet[ # ] ] && dc[ # ] == dc[ w ] + 1 & ],
      dagVerts ]
  ]


(* GeodesicSprayGraph construction from a precomputed distance map.  When
   directed -> True, each edge points outward from the source (along increasing
   distance); when False, undirected edges are emitted. *)

(* the spray keeps the base graph's embedding, so its figures and every
   NeighborhoodGraph carved out of it stay aligned with the substrate *)
geodesicSprayFromDistances[ g_Graph, dist_Association, depthSpec_, directed_ ] :=
  With[ { depth = Replace[ depthSpec, All -> Infinity ],
          coords = AssociationThread[ VertexList[ g ], GraphEmbedding[ g ] ] },
    { dagVerts = Select[ VertexList[ g ], dist[ # ] < Infinity && dist[ # ] <= depth & ] },
    Graph[ dagVerts,
      Map[
        e |-> With[ { u = e[[ 1 ]], v = e[[ 2 ]] },
          Which[
            ! directed && Abs[ dist[ u ] - dist[ v ] ] == 1, UndirectedEdge[ u, v ],
            directed && dist[ v ] == dist[ u ] + 1, DirectedEdge[ u, v ],
            directed && dist[ u ] == dist[ v ] + 1, DirectedEdge[ v, u ],
            True, Nothing ] ],
        EdgeList @ UndirectedGraph @ Subgraph[ g, dagVerts ] ],
      VertexCoordinates -> Lookup[ coords, dagVerts ]
    ]
  ]
