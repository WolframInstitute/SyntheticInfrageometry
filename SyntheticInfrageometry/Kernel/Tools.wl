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
PackageScope[stretchedOutPaths]
PackageScope[pulledPaths]
PackageScope[applyPruning]
PackageScope[pruningSpecQ]
PackageScope[lookbackSpecQ]
PackageScope[formanMethodSpecQ]


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


(* ===================== Stretched paths (internal) ===================== *)

(* Enumerate simple paths from p1 to p2 certified by a recency-lex
   distance rule with lookback K.  At each interior step v_i -> v_{i+1}
   the candidate w must be an unvisited neighbour of v_i and lie in
   MaximalBy[ candidates, w |-> ( d(v_{i-1}, w), d(v_{i-2}, w), ...,
   d(v_{i-K+1}, w) ) ].  The trivial first entry d(v_i, w) = 1 is
   omitted, so K = 1 yields an empty tuple (no filter) and produces
   every simple path; K = 2 enforces no triangle shortcut; K = All
   compares against every available predecessor and on most graphs
   collapses to geodesics.  Built constructively (frontier sweep with
   per-step filtering) so the pruning sub-option can act per step.
   Returns vertex sequences in the same shape as FindSegment / FindPath. *)

stretchedOutPaths[ graph_Graph, p1_, p2_, prune_, lookback_ ] :=
  Module[ { vidx, dmat, frontier, completed = { }, extended },
    If[ p1 === p2, Return[ { } ] ];
    If[ ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ], Return[ { } ] ];
    If[ GraphDistance[ graph, p1, p2 ] === Infinity, Return[ { } ] ];
    vidx = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    dmat = GraphDistanceMatrix[ graph ];
    frontier = { { p1 } };
    While[ frontier =!= { },
      extended = Flatten[
        ( path |-> With[
            { v = Last[ path ],
              historyIdx = With[ { rev = vidx /@ Reverse @ Most[ path ] },
                If[ lookback === All || lookback === Infinity, rev,
                  Take[ rev, UpTo[ lookback - 1 ] ] ] ] },
            With[
              { candidates = Select[ AdjacencyList[ graph, v ],
                  ! MemberQ[ path, # ] & ] },
              ( Append[ path, # ] & ) /@ Which[
                candidates === { }, { },
                historyIdx === { }, candidates,
                True, MaximalBy[ candidates,
                  w |-> dmat[[ historyIdx, vidx[ w ] ]] ]
              ]
            ]
          ] ) /@ frontier,
        1
      ];
      completed = Join[ completed, Select[ extended, Last[ # ] === p2 & ] ];
      frontier = applyPruning[ Select[ extended, Last[ # ] =!= p2 & ], prune ]
    ];
    completed
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

lookbackSpecQ[ All ] := True
lookbackSpecQ[ Infinity ] := True
lookbackSpecQ[ n_Integer ] /; n >= 1 := True
lookbackSpecQ[ _ ] := False


(* ===================== Pulled paths (internal) ===================== *)

(* Enumerate paths from p1 to p2 by a frontier sweep that, at each step
   v_i -> w, restricts the candidate set { w in N(v_i) \ path } to the
   minimisers of the Forman-Ricci curvature F(v_i, w) of the new edge.
   Mirrors stretchedOutPaths in shape; the per-step rule is MinimalBy on
   F instead of MaximalBy on the recency-lex distance tuple.  Ties are
   kept (frontier branches), so the procedure enumerates every walk
   whose every step is curvature-minimal in its candidate set.
   formanMethod is forwarded to FormanRicciCurvature ("Simple" or
   "Triangles").  Returns vertex sequences in the same shape as
   FindSegment / FindPath. *)

pulledPaths[ graph_Graph, p1_, p2_, formanMethod_ ] :=
  Module[ { fEdges, fSym, frontier, completed = { }, extended },
    If[ p1 === p2, Return[ { } ] ];
    If[ ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ], Return[ { } ] ];
    If[ GraphDistance[ graph, p1, p2 ] === Infinity, Return[ { } ] ];
    fEdges = FormanRicciCurvature[ graph, Method -> formanMethod ];
    fSym = Join[
      fEdges,
      AssociationThread[
        ( UndirectedEdge[ #[[ 2 ]], #[[ 1 ]] ] & ) /@ Keys[ fEdges ],
        Values[ fEdges ]
      ]
    ];
    frontier = { { p1 } };
    While[ frontier =!= { },
      extended = Flatten[
        ( path |-> With[
            { v = Last[ path ],
              candidates = Select[ AdjacencyList[ graph, Last[ path ] ],
                ! MemberQ[ path, # ] & ] },
            ( Append[ path, # ] & ) /@ If[ candidates === { }, { },
              MinimalBy[ candidates, w |-> fSym[ UndirectedEdge[ v, w ] ] ] ]
          ] ) /@ frontier,
        1
      ];
      completed = Join[ completed, Select[ extended, Last[ # ] === p2 & ] ];
      frontier  = Select[ extended, Last[ # ] =!= p2 & ]
    ];
    completed
  ]

formanMethodSpecQ[ "Simple" ] := True
formanMethodSpecQ[ "Triangles" ] := True
formanMethodSpecQ[ _ ] := False


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

(* FindSeparatingCycles filters a list of vertex cycles down to those that
   separate center from the exterior of the closed radius-ball.  Used by
   FindCircle (the cyclic vertex-sequence sibling of FindShell). *)

FindSeparatingCycles[ graph_Graph, cycles_List, center_, radius_ ] :=
  Select[ cycles, SeparatingSetQ[ graph, #, center, radius ] & ]

(* FindMinimalSeparatingSubgraphs enumerates subsets of levelSet that
   (a) induce a connected subgraph of graph, (b) separate center from the
   exterior of the closed radius-ball, and are minimal under set inclusion
   among such subsets.  Used by FindShell with Method -> "Separating". *)

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
