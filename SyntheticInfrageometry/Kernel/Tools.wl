Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[CentralElement]
PackageScope[PeripheralElement]
PackageScope[SeparatingSetQ]
PackageScope[FindSeparatingCycles]
PackageScope[FindMinimalSeparatingSubgraphs]
PackageScope[FindPairSeparators]
PackageScope[countLimit]
PackageScope[takeUpTo]
PackageScope[allGeodesics]
PackageScope[stretchedOutSegments]
PackageScope[applyPruning]
PackageScope[pruningSpecQ]


(* Path-space distances and selectors (HausdorffDistance, FrechetDistance,
   MinimalSeparationDistance, EmbeddingHausdorffDistance,
   EmbeddingCircleDistance, pathFilterPairwiseDistances, applySelect)
   live in PathSpace.wl. *)


(* ===================== Centrality ===================== *)

CentralElement[ distanceMatrix_List, n_ : 1 ] :=
  Module[ { scores, minScore, pool, selected, remaining },
    scores = Max /@ distanceMatrix;
    minScore = Min[ scores ];
    pool = Flatten @ Position[ scores, minScore ];
    If[ Length[ pool ] <= n, pool,
      selected = { First @ pool };
      remaining = Rest @ pool;
      Do[
        With[ { best = First @ MaximalBy[ remaining, idx |-> Min[ distanceMatrix[[ idx, selected ]] ] ] },
          AppendTo[ selected, best ];
          remaining = DeleteCases[ remaining, best ]
        ],
        { n - 1 }
      ];
      selected
    ]
  ]

PeripheralElement[ distanceMatrix_List, n_ : 1 ] :=
  Module[ { scores, maxScore, pool, selected, remaining },
    scores = Max /@ distanceMatrix;
    maxScore = Max[ scores ];
    pool = Flatten @ Position[ scores, maxScore ];
    If[ Length[ pool ] <= n, pool,
      selected = { First @ pool };
      remaining = Rest @ pool;
      Do[
        With[ { best = First @ MaximalBy[ remaining, idx |-> Max[ distanceMatrix[[ idx, selected ]] ] ] },
          AppendTo[ selected, best ];
          remaining = DeleteCases[ remaining, best ]
        ],
        { n - 1 }
      ];
      selected
    ]
  ]

(* ===================== Count semantics (internal) ===================== *)

(* Translate a count argument (Integer | UpTo[Integer] | All | Infinity) into
   a numeric upper bound (Integer or Infinity). *)

countLimit[ All ] = Infinity
countLimit[ Infinity ] = Infinity
countLimit[ UpTo[ n_Integer ] ] := n
countLimit[ n_Integer ] := n

takeUpTo[ list_, Infinity ] := list
takeUpTo[ list_, n_Integer ] := Take[ list, UpTo[ n ] ]

(* Enumerate every geodesic from u to v as a vertex sequence; the WL
   built-in idiom for "all shortest paths". *)
allGeodesics[ graph_Graph, u_, v_ ] :=
  With[ { d = GraphDistance[ graph, u, v ] },
    If[ d === Infinity, { }, FindPath[ graph, u, v, { d }, All ] ]
  ]


(* ===================== Stretched-out segments (internal) ===================== *)

(* A stretched-out geodesic from p1 to p2 is one along which every
   step v_i -> v_{i+1} chooses v_{i+1} from among the lex-max-distance
   neighbours of v_i, the lex tuple being (d(v_{i-1}, w), d(v_{i-2}, w),
   ..., d(p1, w)) -- recency first.  Built constructively (frontier BFS
   on geodesic-extension neighbours, lex-max ties at every step) so the
   pruning sub-option can act per step.  Returns vertex sequences in the
   same shape as FindSegment / FindPath. *)

stretchedOutSegments[ graph_Graph, p1_, p2_, prune_ ] :=
  Module[ { vidx, dmat, d, distP2, paths },
    If[ p1 === p2, Return[ { } ] ];
    If[ ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ], Return[ { } ] ];
    vidx = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    dmat = GraphDistanceMatrix[ graph ];
    d = dmat[[ vidx[ p1 ], vidx[ p2 ] ]];
    If[ d === Infinity, Return[ { } ] ];
    distP2 = dmat[[ All, vidx[ p2 ] ]];
    paths = { { p1 } };
    Do[
      paths = applyPruning[
        Flatten[
          ( path |-> With[
              { remaining = d - step,
                historyIdx = vidx /@ Reverse @ Most[ path ] },
              With[
                { progress = Select[ AdjacencyList[ graph, Last[ path ] ],
                    distP2[[ vidx[ # ] ]] === remaining & ] },
                ( Append[ path, # ] & ) /@ Which[
                  progress === { }, { },
                  historyIdx === { }, progress,
                  True, MaximalBy[ progress,
                    w |-> dmat[[ historyIdx, vidx[ w ] ]] ]
                ]
              ]
            ] ) /@ paths,
          1
        ],
        prune
      ];
      If[ paths === { }, Break[] ],
      { step, 1, d }
    ];
    paths
  ]

(* applyPruning trims a list of partial paths by either a beam width
   (integer cap, RandomSample if exceeded) or a Bernoulli keep
   probability (with a one-element floor so the bundle never dies by
   chance).  Caller must validate `prune` via pruningSpecQ first. *)

applyPruning[ paths_List, Infinity ] := paths
applyPruning[ paths_List, n_Integer /; n >= 1 ] :=
  If[ Length[ paths ] <= n, paths, RandomSample[ paths, n ] ]
applyPruning[ { }, p_?NumericQ /; 0 < p < 1 ] := { }
applyPruning[ paths_List, p_?NumericQ /; 0 < p < 1 ] :=
  With[ { kept = Select[ paths, RandomReal[ ] < p & ] },
    If[ kept === { }, RandomSample[ paths, 1 ], kept ]
  ]

pruningSpecQ[ Infinity ] := True
pruningSpecQ[ n_Integer ] /; n >= 1 := True
pruningSpecQ[ p_?NumericQ ] /; 0 < p < 1 := True
pruningSpecQ[ _ ] := False


(* ===================== Separating Sets (internal) ===================== *)

(* SeparatingSetQ tests that removing the vertex set vs from graph leaves a
   component containing center, that this component is contained in the
   closed ball of radius around center, and that all vertices outside the
   component lie strictly beyond radius.  The vertex set is unrestricted -
   typically a subset of the level surface { v : d(center, v) = radius }. *)

SeparatingSetQ[ graph_Graph, vs_List, center_, radius_ ] :=
  Module[ { rem, comps, centerComp },
    rem = VertexDelete[ graph, vs ];
    comps = ConnectedComponents[ rem ];
    centerComp = SelectFirst[ comps, MemberQ[ #, center ] & ];
    centerComp =!= Missing[ "NotFound" ] &&
    AllTrue[ centerComp, GraphDistance[ graph, center, # ] <= radius & ] &&
    AllTrue[ Complement[ VertexList[ rem ], centerComp ], GraphDistance[ graph, center, # ] > radius & ]
  ]

FindSeparatingCycles[ graph_Graph, cycles_List, center_, radius_ ] :=
  Select[ cycles, SeparatingSetQ[ graph, #, center, radius ] & ]

(* FindMinimalSeparatingSubgraphs enumerates subsets of levelSet that
   (a) induce a connected subgraph of graph, (b) separate center from the
   exterior of the closed radius-ball, and are minimal under set inclusion
   among such subsets. *)

FindMinimalSeparatingSubgraphs[ graph_Graph, levelSet_List, center_, radius_ ] :=
  Module[ { levelGraph, separating },
    levelGraph = Subgraph[ graph, levelSet ];
    separating = Select[ Rest @ Subsets[ levelSet ],
      subset |-> ConnectedGraphQ[ Subgraph[ levelGraph, subset ] ] &&
                 SeparatingSetQ[ graph, subset, center, radius ]
    ];
    Select[ separating,
      s |-> ! AnyTrue[ separating, t |-> Length[ t ] < Length[ s ] && SubsetQ[ s, t ] ]
    ]
  ]


(* FindPairSeparators enumerates inclusion-minimal subsets of `set` whose
   removal disconnects p1 from p2 in graph.  Reduces graph to an auxiliary
   graph on {p1, p2} \[Union] set in which every component of graph \\
   ({p1, p2} \[Union] set) contributes a clique on its boundary into those
   nodes (plus all direct graph edges within those nodes); minimal p1-p2
   separators within `set` coincide between graph and the auxiliary one,
   while the latter is much smaller for thin sets in bulky graphs.
   Subsets are tested in increasing size, skipping any superset of an
   already-found minimal separator. *)

FindPairSeparators[ graph_Graph, set_List, p1_, p2_ ] :=
  Module[ { aux, found = {} },
    aux = pairAuxiliaryGraph[ graph, set, p1, p2 ];
    Do[
      Do[
        If[ ! AnyTrue[ found, prev |-> SubsetQ[ T, prev ] ] &&
            GraphDistance[ VertexDelete[ aux, T ], p1, p2 ] === Infinity,
          AppendTo[ found, T ]
        ],
        { T, Subsets[ set, { k } ] }
      ],
      { k, 0, Length[ set ] }
    ];
    found
  ]

pairAuxiliaryGraph[ graph_Graph, set_List, p1_, p2_ ] :=
  Module[ { nodes, components, paired, direct },
    nodes = Union[ set, { p1, p2 } ];
    components = ConnectedComponents @ Subgraph[ graph,
      Complement[ VertexList[ graph ], nodes ] ];
    paired = Flatten[
      ( comp |-> UndirectedEdge @@@ Subsets[
          Intersection[ nodes, Union @@ ( AdjacencyList[ graph, # ] & /@ comp ) ],
          { 2 } ] ) /@ components, 1 ];
    direct = Cases[ EdgeList[ graph ],
      ( UndirectedEdge | DirectedEdge )[ u_, v_ ] /;
        MemberQ[ nodes, u ] && MemberQ[ nodes, v ] :> UndirectedEdge[ u, v ] ];
    Graph[ nodes, DeleteDuplicates[ Join[ paired, direct ] ] ]
  ]
