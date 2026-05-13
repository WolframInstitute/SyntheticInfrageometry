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
PackageScope[extendedOutPaths]
PackageScope[pulledPaths]
PackageScope[applyPruning]
PackageScope[parseCurvatureSpec]
PackageScope[infraSpread]
PackageScope[infraCap]
PackageScope[infraSpreadAndCartesian]
PackageScope[infraUnionSpread]
PackageScope[methodName]


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


(* ===================== Extended out-paths ===================== *)

(* Simple paths from p1 to p2 certified by a recency-lex distance rule with
   lookback K: at each step v_i -> w, the candidate w must lie in
   MaximalBy[unvisited neighbours, w |-> (d(v_{i-1},w), d(v_{i-2},w), ...,
   d(v_{i-K+1},w))].  K = 1 yields every simple path; K = 2 forbids triangle
   shortcuts; K = All compares against every predecessor and on most graphs
   collapses to geodesics. *)

extendedOutPaths[ graph_Graph, p1_, p2_, prune_, lookback_, countLimit_ ] :=
  Module[ { vidx, dmat, frontier, completed = { }, extended },
    If[ p1 === p2, Return[ { } ] ];
    If[ ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ], Return[ { } ] ];
    If[ GraphDistance[ graph, p1, p2 ] === Infinity, Return[ { } ] ];
    vidx = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    dmat = GraphDistanceMatrix[ graph ];
    frontier = { { p1 } };
    While[ frontier =!= { } && Length[ completed ] < countLimit,
      extended = Flatten[
        ( path |-> With[
            { v = Last[ path ],
              historyIdx = With[ { rev = vidx /@ Reverse @ Most[ path ] },
                If[ lookback === All || lookback === Infinity, rev,
                  Take[ rev, UpTo[ lookback - 1 ] ] ] ] },
            With[ { candidates = Select[ AdjacencyList[ graph, v ], ! MemberQ[ path, # ] & ] },
              ( Append[ path, # ] & ) /@ Which[
                candidates === { }, { },
                historyIdx === { }, candidates,
                True, MaximalBy[ candidates, w |-> dmat[[ historyIdx, vidx[ w ] ]] ]
              ]
            ]
          ] ) /@ frontier,
        1 ];
      completed = Join[ completed, Select[ extended, Last[ # ] === p2 & ] ];
      frontier  = applyPruning[ Select[ extended, Last[ # ] =!= p2 & ], prune ]
    ];
    Take[ completed, UpTo[ countLimit ] ]
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


(* ===================== Curvature-pulled paths ===================== *)

(* Paths from p1 to p2 obtained by a frontier sweep restricted at each step
   to MinimalBy[candidates, w |-> edgeKappa[v, w]].  Ties are kept (branches);
   dagNbrs = Automatic means full neighbourhood, otherwise the geodesic-DAG
   neighbour map restricts the candidate set to forward DAG edges. *)

pulledPaths[ graph_Graph, p1_, p2_, edgeKappa_, prune_, countLimit_, dagNbrs_ ] :=
  Module[ { frontier, completed = { }, extended },
    If[ p1 === p2, Return[ { } ] ];
    If[ ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ], Return[ { } ] ];
    If[ GraphDistance[ graph, p1, p2 ] === Infinity, Return[ { } ] ];
    frontier = { { p1 } };
    While[ frontier =!= { } && Length[ completed ] < countLimit,
      extended = Flatten[
        ( path |-> With[
            { v = Last[ path ],
              candidates = If[ dagNbrs === Automatic,
                Select[ AdjacencyList[ graph, Last[ path ] ], ! MemberQ[ path, # ] & ],
                Lookup[ dagNbrs, Key[ Last[ path ] ], { } ] ] },
            ( Append[ path, # ] & ) /@ If[ candidates === { }, { },
              MinimalBy[ candidates, w |-> edgeKappa[ v, w ] ] ]
          ] ) /@ frontier,
        1 ];
      completed = Join[ completed, Select[ extended, Last[ # ] === p2 & ] ];
      frontier  = applyPruning[ Select[ extended, Last[ # ] =!= p2 & ], prune ]
    ];
    Take[ completed, UpTo[ countLimit ] ]
  ]


(* parseCurvatureSpec normalises the "Curvature" sub-option of
   Method -> "CurvatureMinimizing" into a uniform Association consumed by
   buildEdgeKappa in InfraSegment.wl. *)

parseCurvatureSpec[ "Forman" ] :=
  <| "Head" -> "Forman", "Method" -> "Simple" |>

parseCurvatureSpec[ { "Forman", innerOpts___ } ] :=
  <| "Head" -> "Forman", "Method" -> Method /. { innerOpts } /. Method -> "Simple" |>

parseCurvatureSpec[ "Wolfram" ] :=
  <| "Head" -> "Wolfram", "Dimension" -> Automatic, "Radii" -> Automatic |>

parseCurvatureSpec[ { "Wolfram", innerOpts___ } ] :=
  <| "Head" -> "Wolfram",
     "Dimension" -> "Dimension" /. { innerOpts } /. "Dimension" -> Automatic,
     "Radii"     -> "Radii"     /. { innerOpts } /. "Radii"     -> Automatic |>

parseCurvatureSpec[ "Ollivier" ] :=
  <| "Head" -> "Ollivier" |>


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


(* Cycles in the given list that separate center from the exterior of
   the closed radius-ball.  Used by FindCircle. *)

FindSeparatingCycles[ graph_Graph, cycles_List, center_, radius_ ] :=
  Select[ cycles, SeparatingSetQ[ graph, #, center, radius ] & ]


(* Inclusion-minimal connected subsets of levelSet that separate center from
   the exterior of the closed radius-ball.  Used by FindShell with
   Method -> "Separating". *)

FindMinimalSeparatingSubgraphs[ graph_Graph, levelSet_List, center_, radius_ ] :=
  With[ { levelGraph = Subgraph[ graph, levelSet ] },
    With[ { separating = Select[ Rest @ Subsets[ levelSet ],
              subset |-> ConnectedGraphQ[ Subgraph[ levelGraph, subset ] ] &&
                         SeparatingSetQ[ graph, subset, center, radius ] ] },
      Select[ separating,
        s |-> ! AnyTrue[ separating, t |-> Length[ t ] < Length[ s ] && SubsetQ[ s, t ] ] ]
    ]
  ]


(* Inclusion-minimal subsets of `set` whose removal disconnects p1 from p2.
   The auxiliary graph contracts each non-`set` component of the complement
   to a clique on its boundary nodes, preserving p1-p2 separators within
   `set` while shrinking the search space.  Subsets tested in increasing
   size, skipping supersets of already-found minimal separators. *)

FindPairSeparators[ graph_Graph, set_List, p1_, p2_ ] :=
  Module[ { found = { },
            aux = pairAuxiliaryGraph[ graph, set, p1, p2 ] },
    Do[
      Do[
        If[ ! AnyTrue[ found, prev |-> SubsetQ[ T, prev ] ] &&
            GraphDistance[ VertexDelete[ aux, T ], p1, p2 ] === Infinity,
          AppendTo[ found, T ] ],
        { T, Subsets[ set, { k } ] } ],
      { k, 0, Length[ set ] } ];
    found
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

infraSpread[ ( InfraPoint | InfraSegment | InfraShell | InfraPlane | InfraCircle | InfraRay | InfraPolyline )[ reps_List ] ] := reps
infraSpread[ list_List ] /; AllTrue[ list,
    MatchQ[ ( InfraPoint | InfraSegment | InfraShell | InfraPlane | InfraCircle | InfraRay | InfraPolyline )[ { _ } ] ] ] :=
  First /@ list
infraSpread[ other_ ] := { other }


(* Collapse a wrapped entry to the union of its realisations, for set-
   conjunction Find* over a single _List argument (FindCommonLine,
   FindCommonPoint). *)

infraUnionSpread[ InfraPoint[ reps_List ] ] := DeleteDuplicates @ reps
infraUnionSpread[ ( InfraSegment | InfraShell | InfraPlane | InfraCircle | InfraRay )[ reps_List ] ] :=
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
