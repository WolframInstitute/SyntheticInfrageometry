Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[CentralElement]
PackageScope[PeripheralElement]
PackageScope[SeparatingSetQ]
PackageScope[findAllMinimalAdmissible]
PackageScope[findGreedyMinimalAdmissible]
PackageScope[pairAuxiliaryGraph]
PackageScope[countLimit]
PackageScope[takeUpTo]
PackageScope[allGeodesics]
PackageScope[frontierSweep]
PackageScope[greedyFrontierSweep]
PackageScope[makeCandidateFn]
PackageScope[propertyFilter]
PackageScope[applyPruning]
PackageScope[infraSpread]
PackageScope[infraCap]
PackageScope[infraSpreadAndCartesian]
PackageScope[infraUnionSpread]
PackageScope[infraVertexMultiset]
PackageScope[infraRepType]
PackageScope[infraRepSeqs]
PackageScope[infraRepVerts]
PackageScope[infraRepEdges]
PackageScope[infraNumReps]
PackageScope[infraEdgeMultiset]
PackageScope[linePointSet]
PackageScope[methodName]
PackageScope[methodOptions]
PackageScope[propertiesSubOpts]


(* Path-space distances and selectors (HausdorffDistance, FrechetDistance,
   MinimalSeparationDistance, EmbeddingHausdorffDistance,
   EmbeddingCircleDistance, pathFilterPairwiseDistances, applySelect)
   live in PathSpace.wl. *)


(* ===================== Method-spec helper ===================== *)

(* Normalise a Method option value to its leading method-name string:
   "Metric" -> "Metric";  {"Metric", opts___} -> "Metric". *)

methodName[ m_String ]          := m
methodName[ { m_String, ___ } ] := m


(* Sub-options carried by a Method spec.  Bare string -> {};
   {"Name", opts___} -> {opts}.  Consumed by Method-dispatching predicates
   that read per-method "Tolerance", "Equality", "Statistic", etc. via Lookup. *)

methodOptions[ _String ]                := { }
methodOptions[ { _String, opts___ } ]   := { opts }


(* ===================== Centrality ===================== *)

(* CentralElement: n indices into distanceMatrix minimising eccentricity
   (Max-of-row); ties broken by maximin against the running selection.
   PeripheralElement is the symmetric maximiser. *)

CentralElement[ distanceMatrix_List, n_ : 1 ] :=
  Module[ { selected, remaining,
            pool = Flatten @ Position[ Max /@ distanceMatrix, Min[ Max /@ distanceMatrix ] ] },
    If[ Length[ pool ] <= n, pool,
      selected = { First @ pool };
      remaining = Rest @ pool;
      Do[
        With[ { best = First @ MaximalBy[ remaining, idx |-> Min[ distanceMatrix[[ idx, selected ]] ] ] },
          AppendTo[ selected, best ];
          remaining = DeleteCases[ remaining, best ] ],
        { n - 1 } ];
      selected
    ]
  ]

PeripheralElement[ distanceMatrix_List, n_ : 1 ] :=
  Module[ { selected, remaining,
            pool = Flatten @ Position[ Max /@ distanceMatrix, Max[ Max /@ distanceMatrix ] ] },
    If[ Length[ pool ] <= n, pool,
      selected = { First @ pool };
      remaining = Rest @ pool;
      Do[
        With[ { best = First @ MaximalBy[ remaining, idx |-> Max[ distanceMatrix[[ idx, selected ]] ] ] },
          AppendTo[ selected, best ];
          remaining = DeleteCases[ remaining, best ] ],
        { n - 1 } ];
      selected
    ]
  ]


(* ===================== Count semantics ===================== *)

(* Translate a count argument (Integer | UpTo[Integer] | All | Infinity)
   into a numeric upper bound. *)

countLimit[ All ]               = Infinity
countLimit[ Infinity ]          = Infinity
countLimit[ UpTo[ n_Integer ] ] := n
countLimit[ n_Integer ]         := n

takeUpTo[ list_, Infinity ]     := list
takeUpTo[ list_, n_Integer ]    := Take[ list, UpTo[ n ] ]


(* Every geodesic from u to v as a vertex sequence. *)

allGeodesics[ graph_Graph, u_, v_ ] :=
  With[ { d = GraphDistance[ graph, u, v ] },
    If[ d === Infinity, { }, FindPath[ graph, u, v, { d }, All ] ]
  ]


(* Trim a list of partial paths by either a beam width (integer cap, random
   sampling if exceeded) or a Bernoulli keep probability (with a one-element
   floor so the bundle never dies by chance). *)

applyPruning[ paths_List, Infinity ]                     := paths
applyPruning[ paths_List, n_Integer /; n >= 1 ]          :=
  If[ Length[ paths ] <= n, paths, RandomSample[ paths, n ] ]
applyPruning[ { }, p_?NumericQ /; 0 < p < 1 ]            := { }
applyPruning[ paths_List, p_?NumericQ /; 0 < p < 1 ]     :=
  With[ { kept = Select[ paths, RandomReal[ ] < p & ] },
    If[ kept === { }, RandomSample[ paths, 1 ], kept ] ]


(* ===================== Frontier sweep ===================== *)

(* BFS frontier from p1 to p2 with candidateFn[g, path] returning the
   admissible next-vertex set at each step.  applyPruning caps the live
   frontier per layer.  Returns up to `count` complete paths. *)

frontierSweep[ graph_Graph, p1_, p2_, candidateFn_, prune_, count_ ] :=
  Module[ { frontier, completed = { }, extended },
    If[ p1 === p2, Return[ { } ] ];
    If[ ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ], Return[ { } ] ];
    If[ GraphDistance[ graph, p1, p2 ] === Infinity, Return[ { } ] ];
    frontier = { { p1 } };
    While[ frontier =!= { } && Length[ completed ] < count,
      extended = Flatten[
        ( path |-> ( Append[ path, # ] & ) /@ candidateFn[ graph, path ] ) /@ frontier,
        1 ];
      completed = Join[ completed, Select[ extended, Last[ # ] === p2 & ] ];
      frontier  = applyPruning[ Select[ extended, Last[ # ] =!= p2 & ], prune ]
    ];
    Take[ completed, UpTo[ count ] ]
  ]


(* DFS one realisation: pick the first admissible candidate at each step. *)

greedyFrontierSweep[ graph_Graph, p1_, p2_, candidateFn_ ] :=
  If[ p1 === p2 || ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ] ||
      GraphDistance[ graph, p1, p2 ] === Infinity, { },
    Module[ { path = { p1 }, cands },
      While[ Last[ path ] =!= p2,
        cands = candidateFn[ graph, path ];
        If[ cands === { }, Return[ { } ] ];
        AppendTo[ path, First @ cands ]
      ];
      { path }
    ]
  ]


(* ===================== Property-filter machinery ===================== *)

(* Sub-options of a property entry: "Foo" -> { }, {"Foo", opts___} -> {opts}. *)

propertiesSubOpts[ s_String ]              := { }
propertiesSubOpts[ { _String, opts___ } ]  := { opts }


(* makeCandidateFn[g, baseFn, properties, badPropMsg]: closure
     (g, path) -> admissible-next-vertex set
   by Fold-ing per-property filters over the base candidate set baseFn[g, path].
   Each filter shrinks the candidate set in turn (AND-conjunction). *)

makeCandidateFn[ graph_Graph, baseFn_, properties_List, fnSym_ ] :=
  With[ { filters = propertyFilter[ graph, #, fnSym ] & /@ properties },
    Function[ { g, path },
      Fold[ #2[ g, path, #1 ] &, baseFn[ g, path ], filters ]
    ]
  ]


(* propertyFilter[g, propertySpec, fnSym]: dispatch on property name, return a
   closure (g, path, candidates) -> candidates'.  Unknown property raises
   fnSym::badproperty and Throw[$Failed]; caller wraps in Catch.  fnSym is the
   calling head symbol (not fnSym::badproperty -- a MessageName passed as a bare
   argument evaluates to its template string, so Message would emit Message::name). *)

propertyFilter[ _Graph, "Simple", _ ]                          := simpleFilter
propertyFilter[ _Graph, { "Simple" }, _ ]                      := simpleFilter

propertyFilter[ graph_Graph, "ShortestPath", _ ]               := shortestPathFilter[ graph, Infinity ]
propertyFilter[ graph_Graph, { "ShortestPath", subs___ }, _ ]  :=
  shortestPathFilter[ graph, "Window" /. { subs } /. "Window" -> Infinity ]

propertyFilter[ graph_Graph, "LongestPath", _ ]                := longestPathFilter[ graph, 2, "Lex" ]
propertyFilter[ graph_Graph, { "LongestPath", subs___ }, _ ]   :=
  longestPathFilter[ graph,
    "Window"      /. { subs } /. "Window"      -> 2,
    "Aggregation" /. { subs } /. "Aggregation" -> "Lex" ]

propertyFilter[ _Graph, { "EdgeMin", f_ }, _ ]                 := edgeMinFilter[ f ]
propertyFilter[ _Graph, { "EdgeMax", f_ }, _ ]                 := edgeMaxFilter[ f ]

propertyFilter[ _Graph, other_, fnSym_ ] :=
  ( Message[ MessageName[ fnSym, "badproperty" ], other ]; Throw[ $Failed ] )


(* "Simple": disallow revisits. *)
simpleFilter[ _Graph, path_, candidates_ ] :=
  Select[ candidates, ! MemberQ[ path, # ] & ]


(* "ShortestPath", "Window" -> k: strict d(path[[-k]], w) == k. *)
shortestPathFilter[ _Graph, window_ ] :=
  Function[ { g, path, candidates },
    With[ { k = If[ window === All || window === Infinity, Length[ path ],
                    Min[ window, Length[ path ] ] ] },
      Select[ candidates, GraphDistance[ g, path[[ -k ]], # ] == k & ]
    ]
  ]


(* "LongestPath", "Window" -> k, "Aggregation" -> Lex | Sum:
   MaximalBy distance-tuple to the last k vertices. *)
longestPathFilter[ graph_Graph, window_, aggregation_ ] :=
  With[ { vidx = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ],
          dmat = GraphDistanceMatrix[ graph ] },
    Function[ { g, path, candidates },
      With[ { historyIdx = With[ { rev = vidx /@ Reverse @ Most[ path ] },
                If[ window === All || window === Infinity, rev,
                    Take[ rev, UpTo[ window - 1 ] ] ] ] },
        Which[
          candidates === { } || historyIdx === { },  candidates,
          aggregation === "Sum",                     MaximalBy[ candidates, w |-> Total @ dmat[[ historyIdx, vidx[ w ] ]] ],
          True,                                      MaximalBy[ candidates, w |-> dmat[[ historyIdx, vidx[ w ] ]] ]
        ]
      ]
    ]
  ]


(* "EdgeMin", f: MinimalBy f[v, w] over candidates (v = Last @ path). *)
edgeMinFilter[ f_ ] :=
  Function[ { g, path, candidates },
    If[ candidates === { }, candidates,
      MinimalBy[ candidates, w |-> f[ Last @ path, w ] ] ]
  ]


(* "EdgeMax", f: MaximalBy f[v, w] over candidates. *)
edgeMaxFilter[ f_ ] :=
  Function[ { g, path, candidates },
    If[ candidates === { }, candidates,
      MaximalBy[ candidates, w |-> f[ Last @ path, w ] ] ]
  ]


(* ===================== Separating sets ===================== *)

(* SeparatingSetQ[g, vs, center, radius]: removing vs leaves a component
   containing center that lies inside the closed ball B(center, radius),
   and every vertex outside that component lies strictly beyond radius. *)

SeparatingSetQ[ graph_Graph, vs_List, center_, radius_ ] :=
  With[ { rem = VertexDelete[ graph, vs ] },
    With[ { centerComp = SelectFirst[ ConnectedComponents[ rem ], MemberQ[ #, center ] & ] },
      centerComp =!= Missing[ "NotFound" ] &&
      AllTrue[ centerComp, GraphDistance[ graph, center, # ] <= radius & ] &&
      AllTrue[ Complement[ VertexList[ rem ], centerComp ], GraphDistance[ graph, center, # ] > radius & ]
    ]
  ]


(* Top-down peel from `set` toward inclusion-minimal admissible subsets.
   `admissible` is a user-supplied predicate on a vertex subset T.  Both
   helpers terminate when no further admissible single-removal exists --
   inclusion-minimality is automatic at the peel leaves. *)

(* DFS, no backtracking: one realisation, deterministic vertex order. *)

findGreedyMinimalAdmissible[ graph_Graph, set_List, admissible_ ] :=
  If[ ! admissible[ set ], { },
    Module[ { T = set, v },
      While[ True,
        v = SelectFirst[ T, w |-> admissible[ DeleteCases[ T, w ] ], Missing[ ] ];
        If[ MissingQ[ v ], Break[ ] ];
        T = DeleteCases[ T, v ];
      ];
      { T }
    ]
  ]


(* BFS over the peel-DAG with `Sort @ T` as the canonical dedup key.
   `applyPruning` caps the removable-vertex frontier per layer. *)

findAllMinimalAdmissible[ graph_Graph, set_List, admissible_, pruning_ ] :=
  If[ ! admissible[ set ], { },
    Module[ { frontier = { Sort @ set },
              seen = <| Sort @ set -> True |>,
              minimals = { }, next, removable, key },
      While[ frontier =!= { },
        next = { };
        Do[
          removable = Select[ T, v |-> admissible[ DeleteCases[ T, v ] ] ];
          If[ removable === { },
            AppendTo[ minimals, T ],
            Do[
              key = Sort @ DeleteCases[ T, v ];
              If[ ! KeyExistsQ[ seen, key ],
                seen[ key ] = True;
                AppendTo[ next, key ] ],
              { v, applyPruning[ removable, pruning ] } ]
          ],
          { T, frontier } ];
        frontier = next;
      ];
      DeleteDuplicates @ minimals
    ]
  ]


pairAuxiliaryGraph[ graph_Graph, set_List, p1_, p2_ ] :=
  With[ { nodes = Union[ set, { p1, p2 } ] },
    With[ { components = ConnectedComponents @ Subgraph[ graph,
              Complement[ VertexList[ graph ], nodes ] ] },
      With[ { paired = Flatten[
                ( comp |-> UndirectedEdge @@@ Subsets[
                    Intersection[ nodes, Union @@ ( AdjacencyList[ graph, # ] & /@ comp ) ],
                    { 2 } ] ) /@ components, 1 ],
              direct = Cases[ EdgeList[ graph ],
                ( UndirectedEdge | DirectedEdge )[ u_, v_ ] /;
                  MemberQ[ nodes, u ] && MemberQ[ nodes, v ] :> UndirectedEdge[ u, v ] ] },
        Graph[ nodes, DeleteDuplicates[ Join[ paired, direct ] ] ]
      ]
    ]
  ]


(* ===================== Multi-realisation wrapper helpers ===================== *)

(* Find* functions return a List of unary wrappers InfraX[{r}]; the multi-
   realisation wrapper InfraX[{r1, ..., rk}] is constructed by wrapping such
   a list, with the auto-flatten rule in each per-primitive file collapsing
   the result. *)


(* Adapt an anchor for one slot of a Cartesian product: a multi-realisation
   wrapper or a List of unary wrappers spreads into its bare realisations;
   anything else becomes a singleton. *)

infraSpread[ InfraPoint[ verts_List, _List ] ] := verts
infraSpread[ ( InfraPoint | InfraSegment | InfraLine | InfraPath | InfraLoop | InfraString | InfraShell | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse | InfraPolygon | InfraTriangle | InfraRay | InfraPolyline )[ reps_List ] ] := reps
infraSpread[ list_List ] /; AllTrue[ list,
    MatchQ[ ( InfraPoint | InfraSegment | InfraLine | InfraPath | InfraLoop | InfraString | InfraShell | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse | InfraPolygon | InfraTriangle | InfraRay | InfraPolyline )[ { _ } ] ] ] :=
  #[[ 1, 1 ]] & /@ list
infraSpread[ other_ ] := { other }


(* Project a bundle of vertex-sequence realisations onto position i: the
   weighted InfraPoint whose support is the i-th vertices (only realisations
   long enough contribute) and whose masses are their multiplicities.  i may
   be negative (counted from the end). *)

PackageScope[columnInfraPoint]

columnInfraPoint[ reps_List, i_Integer ] :=
  With[ { col = ( #[[ i ]] & ) /@ Select[ reps, Length[ # ] >= Abs[ i ] & ] },
    With[ { m = Counts @ col }, InfraPoint[ Keys @ m, Values @ m ] ] ]


(* Collapse a wrapped entry to the union of its realisations, for set-
   conjunction Find* over a single _List argument (FindInfraCommonLine,
   FindInfraCommonPoint). *)

infraUnionSpread[ InfraPoint[ verts_List, _List ] ] := DeleteDuplicates @ verts
infraUnionSpread[ InfraPoint[ reps_List ] ] := DeleteDuplicates @ reps
infraUnionSpread[ ( InfraSegment | InfraLine | InfraPath | InfraLoop | InfraString | InfraShell | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse | InfraRay )[ reps_List ] ] :=
  Union @@ reps
infraUnionSpread[ ( InfraPolyline | InfraPolygon | InfraTriangle )[ reps_List ] ] :=
  Union @@ polylineToVertexSeqs[ reps ]
infraUnionSpread[ other_ ] := { other }


(* Seed vertex set for the hull family (FindSegmentHull / FindLineHull /
   FindBallHull): a bare vertex list passes through; any Infra* object, a list
   of them, or a bare vertex falls to infraVertexSet. *)

PackageScope[hullVertices]

hullVertices[ s_List ] /; AnyTrue[ s, StringStartsQ[ SymbolName @ Head @ #, "Infra" ] & ] :=
  infraVertexSet[ s ]
hullVertices[ s_List ] := s
hullVertices[ s_ ] := infraVertexSet[ s ]


(* Apply n / UpTo[n] / All count semantics to a bare list of realisations.
   $Failed return is the mathematical "fewer than n exist" case. *)

infraCap[ list_List, All ]                              := list
infraCap[ list_List, UpTo[ n_Integer ] ]                := Take[ list, UpTo[ n ] ]
infraCap[ list_List, n_Integer ] /; n <= Length[ list ] := Take[ list, n ]
infraCap[ _List, _Integer ]                             := $Failed


(* Dispatch shell for source/endpoint anchors: spread each anchor, run the
   single-pair core over the Cartesian product, union-deduplicate, cap, wrap. *)

infraSpreadAndCartesian[ wrapHead_, count_, core_, anchors__ ] :=
  With[ { results = core @@@ Tuples[ infraSpread /@ { anchors } ] },
    If[ MemberQ[ results, $Failed ], $Failed,
      With[ { capped = infraCap[ DeleteDuplicates @ Flatten[ results, 1 ], count ] },
        If[ capped === $Failed, $Failed, wrapHead[ { # } ] & /@ capped ]
      ]
    ]
  ]


(* Diffusion-diagram extractor: the vertex -> total-occurrence Association
   used by InfraEqualQ.  Mirrors the per-head normalization of repVerts
   inside InfraSceneVisualization.wl: realisations of point-shaped wrappers
   are bare vertices; path / set / cycle wrappers carry vertex lists per
   realisation; InfraPolyline realisations are leg-chains flattened via
   polylineToVertexSeqs; InfraObject / InfraSet hold one bare vertex set. *)

infraVertexMultiset[ InfraPoint[ verts_List, weights_List ] ] :=
  AssociationThread[ verts -> weights ]
infraVertexMultiset[ InfraPoint[ reps_List ] ] :=
  Counts @ reps
infraVertexMultiset[ ( InfraSegment | InfraPath | InfraLoop | InfraString | InfraLine | InfraRay |
                       InfraShell | InfraEllipticShell | InfraBall | InfraPlane |
                       InfraCircle | InfraEllipse )[ reps_List ] ] :=
  Counts @ Catenate @ reps
infraVertexMultiset[ ( InfraPolyline | InfraPolygon | InfraTriangle )[ reps_List ] ] :=
  Counts @ Catenate @ polylineToVertexSeqs[ reps ]
infraVertexMultiset[ ( InfraObject | InfraSet )[ vs_List ] ] :=
  Counts @ vs


(* ===================== Visit measure ===================== *)

(* The occupation measure of a wrapper: the marginal of its realization bundle
   onto its vertex / edge set, c(v) = total appearances across realisations.
   Method picks the normalization, same shape either way (a global rescaling):
   "Occupation" (default) m(v) = c(v) / N -- mean occupation per realisation,
   the membership / opacity InfraSceneHighlight draws; "Probability" p(v) =
   c(v) / Total[c] -- the probability distribution on nodes, Sigma = 1.  A lossy
   *view* of the bundle (discards order, co-occurrence) -- the bundle stays
   ground truth. *)

InfraMeasure::badmethod = "Method `1` is not one of \"Occupation\", \"Probability\".";

Options[ InfraMeasure ] = { "On" -> "Vertices", Method -> "Occupation" };

InfraMeasure[ g_Graph, obj_, opts:OptionsPattern[] ] :=
  visitMeasure[ g, obj, OptionValue[ "On" ], OptionValue[ Method ] ]
InfraMeasure[ obj_, opts:OptionsPattern[] ] :=
  visitMeasure[ None, obj, OptionValue[ "On" ], OptionValue[ Method ] ]

visitMeasure[ g_, obj_, on_, method_ ] :=
  With[ { vm = infraVertexMultiset[ obj ],
          em = If[ on === "Vertices", <||>,
                   KeyMap[ UndirectedEdge @@ # &, infraEdgeMultiset[ g, obj ] ] ] },
    Switch[ on,
      "Vertices", normalizeMeasure[ method, vm, obj ],
      "Edges",    normalizeMeasure[ method, em, obj ],
      "Both",     <| "Vertices" -> normalizeMeasure[ method, vm, obj ],
                     "Edges"    -> normalizeMeasure[ method, em, obj ] |> ] ]

normalizeMeasure[ method_, counts_, obj_ ] := Switch[ method,
  "Occupation",  counts / infraNumReps[ obj ],
  "Probability", If[ Length @ counts === 0, counts, counts / Total[ counts ] ],
  _,             Message[ InfraMeasure::badmethod, method ]; counts ]

(* head -> topology type; the single source of truth shared with
   InfraSceneHighlight's repVerts / repEdges dispatch. *)

infraRepType[ InfraPoint ]         = "Points";
infraRepType[ InfraSegment ]       = "Paths";
infraRepType[ InfraLine ]          = "Paths";
infraRepType[ InfraPath ]          = "Paths";
infraRepType[ InfraLoop ]          = "Paths";
infraRepType[ InfraRay ]           = "Paths";
infraRepType[ InfraPolyline ]      = "Paths";
infraRepType[ InfraString ]        = "Cycles";
infraRepType[ InfraCircle ]        = "Cycles";
infraRepType[ InfraEllipse ]       = "Cycles";
infraRepType[ InfraPolygon ]       = "Cycles";
infraRepType[ InfraTriangle ]      = "Cycles";
infraRepType[ InfraShell ]         = "Sets";
infraRepType[ InfraBall ]          = "Sets";
infraRepType[ InfraEllipticShell ] = "Sets";
infraRepType[ InfraPlane ]         = "Sets";
infraRepType[ InfraObject ]        = "Sets";
infraRepType[ InfraSet ]           = "Sets";

(* canonical realisation sequences (one vertex list per realisation), the
   bundle repVerts / repEdges consume after the polyline / set transforms. *)

infraRepSeqs[ ( InfraPolyline | InfraPolygon | InfraTriangle )[ reps_List ] ] := polylineToVertexSeqs @ reps
infraRepSeqs[ ( InfraObject | InfraSet )[ vs_List ] ]                         := { vs }
infraRepSeqs[ head_[ reps_List, ___ ] ]                                       := reps

(* per-type vertex / edge extraction from one canonical realisation -- the
   single source of truth shared with InfraSceneHighlight's repVerts / repEdges
   dispatch.  A point realisation is a bare vertex (wrapped to a singleton);
   path / cycle / set realisations are vertex lists.  Edges are sorted lists
   {a, b} (InfraMeasure remaps them to UndirectedEdge for its public output). *)

infraRepVerts[ "Points", rep_ ] := { rep }
infraRepVerts[ _, rep_ ]        := rep

(* normalization divisor N = number of realisations.  A weighted InfraPoint
   stores one vertex per realisation as multiplicities, so N = Total[weights];
   InfraObject / InfraSet hold a single set, so N = 1. *)

infraNumReps[ InfraPoint[ _List, weights_List ] ] := Max[ Total @ weights, 1 ]
infraNumReps[ ( InfraObject | InfraSet )[ _List ] ] := 1
infraNumReps[ head_[ reps_List, ___ ] ]            := Max[ Length @ reps, 1 ]

(* raw edge multiset, keyed by sorted lists {a, b}.  Sets-type edges are the
   induced subgraph, hence the graph dependency.  InfraMeasure remaps the keys
   to UndirectedEdge; the renderer consumes the sorted-list keys directly. *)

infraEdgeMultiset[ g_, obj_ ] :=
  Counts @ Catenate[ infraRepEdges[ g, infraRepType @ Head @ obj, # ] & /@ infraRepSeqs @ obj ]

infraRepEdges[ _, "Points", _ ]   := { }
infraRepEdges[ _, "PointSet", _ ] := { }
infraRepEdges[ _, "Paths", rep_ ] :=
  If[ Length @ rep >= 2, Sort /@ Partition[ rep, 2, 1 ], { } ]
infraRepEdges[ _, "Cycles", rep_ ] :=
  With[ { closed = If[ Length @ rep >= 2 && First @ rep === Last @ rep, rep, Append[ rep, First @ rep ] ] },
    If[ Length @ closed >= 2, Sort /@ Partition[ closed, 2, 1 ], { } ] ]
infraRepEdges[ g_, "Sets", rep_ ] :=
  Sort /@ ( List @@@ EdgeList @ Subgraph[ g, rep ] )


(* Vertex-set view of a line-like input (FindInfraCommonPoint, InfraPerpendicularQ).
   Bare list = vertex sequence; line wrappers unwrap to the union of realisations. *)

linePointSet[ ( InfraLine | InfraSegment | InfraPath | InfraRay )[ reps_List ] ] := Union @@ reps
linePointSet[ line_List ] := line
