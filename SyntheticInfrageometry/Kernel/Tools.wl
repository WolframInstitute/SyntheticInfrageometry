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
PackageScope[methodName]
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

makeCandidateFn[ graph_Graph, baseFn_, properties_List, badPropMsg_ ] :=
  With[ { filters = propertyFilter[ graph, #, badPropMsg ] & /@ properties },
    Function[ { g, path },
      Fold[ #2[ g, path, #1 ] &, baseFn[ g, path ], filters ]
    ]
  ]


(* propertyFilter[g, propertySpec, badPropMsg]: dispatch on property name,
   return a closure (g, path, candidates) -> candidates'.  Unknown property
   raises badPropMsg and Throw[$Failed]; caller wraps in Catch. *)

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

propertyFilter[ _Graph, other_, badPropMsg_ ] :=
  ( Message[ badPropMsg, other ]; Throw[ $Failed ] )


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

infraSpread[ ( InfraPoint | InfraSegment | InfraPath | InfraLoop | InfraString | InfraShell | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse | InfraRay | InfraPolyline )[ reps_List ] ] := reps
infraSpread[ list_List ] /; AllTrue[ list,
    MatchQ[ ( InfraPoint | InfraSegment | InfraPath | InfraLoop | InfraString | InfraShell | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse | InfraRay | InfraPolyline )[ { _ } ] ] ] :=
  #[[ 1, 1 ]] & /@ list
infraSpread[ other_ ] := { other }


(* Collapse a wrapped entry to the union of its realisations, for set-
   conjunction Find* over a single _List argument (FindInfraCommonLine,
   FindInfraCommonPoint). *)

infraUnionSpread[ InfraPoint[ reps_List ] ] := DeleteDuplicates @ reps
infraUnionSpread[ ( InfraSegment | InfraPath | InfraLoop | InfraString | InfraShell | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse | InfraRay )[ reps_List ] ] :=
  Union @@ reps
infraUnionSpread[ InfraPolyline[ reps_List ] ] := Union @@ polylineToVertexSeqs[ reps ]
infraUnionSpread[ other_ ] := { other }


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
