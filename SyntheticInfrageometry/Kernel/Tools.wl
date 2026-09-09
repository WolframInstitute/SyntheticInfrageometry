Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[dagGeodesics]
PackageScope[dagLayers]
PackageScope[SeparatingSetQ]
PackageScope[findAllMinimalAdmissible]
PackageScope[findGreedyMinimalAdmissible]
PackageScope[countLimit]
PackageScope[takeUpTo]
PackageScope[allGeodesics]
PackageScope[frontierSweep]
PackageScope[greedyFrontierSweep]
PackageScope[windowCandidateFn]
PackageScope[excludedSpecies]
PackageScope[applyPruning]
PackageScope[resolveMethod]
PackageScope[greedyBranch]
PackageScope[infraSpread]
PackageScope[spreadFind]
PackageScope[geodesicFind]
PackageScope[geodesicTake]
PackageScope[countTake]
PackageScope[loneBundle]
PackageScope[geodesicGraph]
PackageScope[geodesicCycleGraph]
PackageScope[vertexSet]
PackageScope[infraVertexSet]
PackageScope[infraVertexMultiset]
PackageScope[infraEdgeMultiset]
PackageScope[infraNumReps]
PackageScope[walkEdges]
PackageScope[cycleEdges]
PackageScope[setEdges]
PackageScope[pointQ]
PackageScope[multisetQ]
PackageScope[walkQ]
PackageScope[bundleQ]
PackageScope[chainedWalksQ]
PackageScope[inkClass]
PackageScope[walkGraphs]
PackageScope[walkGraph]
PackageScope[closedWalkGraph]
PackageScope[closedWalkQ]
PackageScope[positionSpelledQ]
PackageScope[walkSequence]
PackageScope[walkRealisations]
PackageScope[walkVertexSet]
PackageScope[toDensity]
PackageScope[linePointSet]
PackageScope[cycleToVertexSequence]
PackageScope[methodName]
PackageScope[methodOptions]
PackageScope[propertiesSubOpts]


(* ===================== Method-spec helper ===================== *)


methodName[ m_String ]          := m
methodName[ { m_String, ___ } ] := m


methodOptions[ _String ]                := { }
methodOptions[ { _String, opts___ } ]   := { opts }


(* ===================== The shape reader ===================== *)

(* THE SHAPE IS THE KIND -- nothing is wrapped.  A point is a vertex of the substrate carrying its label verbatim, a set / multiset / density is <| v -> m |> with a List the uniform case one Counts away, and a walk -- like every 1-d object -- is a Graph, a directed path being one walk and a DAG a bundle of them.
   The substrate is an argument because a vertex label may itself be a List ({i, j} on a tessellation), and then only the graph separates the point row from the multiset row.  walkQ takes it for uniformity: the general walk family lives on {i, v} position pairs, whose vertices are not substrate vertices, so there is nothing graph-relative to check. *)

pointQ[ graph_Graph, x_ ] := VertexQ[ graph, x ]

multisetQ[ graph_Graph, x_ ] := AssociationQ[ x ] || ( ListQ[ x ] && ! VertexQ[ graph, x ] )

walkQ[ _Graph, x_ ] := GraphQ[ x ]


(* THE INK TABLE KEYS ON SHAPE.  inkClass names the class the renderer inks an object in, read off the shape alone -- no head is consulted, because no head is left.  Two readings the shape cannot separate, both settled by the Spec: a vertex List is a SET, so a point family is written as its density (`InfraDensity` promotes, `Keys` demotes); and a chain of open walks is a POLYLINE, so a bundle of open geodesics that happens to chain end to end is misread as one -- it needs last(w_i) === first(w_{i+1}) at every i, which a same-endpoints bundle never satisfies *)

inkClass[ graph_Graph, x_ ] := Which[
  pointQ[ graph, x ],                                              "Point",
  AssociationQ[ x ],                                               "Density",
  GraphQ[ x ],                                                     "Walk",
  chainedWalksQ[ x ],                                              "Polyline",
  MatchQ[ x, { __Graph } ],                                        "Walk",
  MatchQ[ x, { { __Graph } .. } ] && AllTrue[ x, chainedWalksQ ],  "PolylineFamily",
  MatchQ[ x, { { __Graph } .. } ],                                 "Walk",
  ListQ[ x ] && SubsetQ[ VertexList @ graph, x ],                  "Set",
  MatchQ[ x, { __List } ],                                         "SetFamily",
  True,                                                            "Set" ]

(* a branching DAG stands for many walks with no single stroke; a path graph, a directed cycle and a position-spelled walk each stand for one *)
bundleQ[ w_Graph ] := ! closedWalkQ[ w ] && ! positionSpelledQ[ w ] && ! PathGraphQ[ w ]

(* consecutive legs of a polyline share their knot, and a leg is an open geodesic -- the closure of a polygon is the chain closing, not a leg *)
chainedWalksQ[ legs : { _Graph, __Graph } ] :=
  NoneTrue[ legs, closedWalkQ ] &&
  AllTrue[ Partition[ walkSequence /@ legs, 2, 1 ], Last @ First @ # === First @ Last @ # & ]
chainedWalksQ[ _ ] := False

walkGraphs[ w_Graph ]                 := { w }
walkGraphs[ ws : { __Graph } ]        := ws
walkGraphs[ ws : { { __Graph } .. } ] := Catenate @ ws


(* the two spellings of a walk graph.  The general walk family writes a walk on the POSITION PAIRS {i, v}: PathGraph marks it open, a directed cycle marks it closed (a constant loop is one vertex with a self-loop) -- a walk revisits, and only positions carry the singularity census.  The geodesic class writes its path graphs and DAGs on SUBSTRATE vertices, where nothing repeats.  Last /@ VertexList converts the first spelling to the second; nothing converts back.  A graph is read as position-spelled by its vertex labels alone, so a substrate whose vertices are themselves {i, v} pairs visited in position order is misread -- accepted, since the paclet never writes such a graph *)

walkGraph[ walk_List ] := PathGraph[ MapIndexed[ { First @ #2, #1 } &, walk ], DirectedEdges -> True ]

closedWalkGraph[ walk_List ] :=
  With[ { core = MapIndexed[ { First @ #2, #1 } &,
            If[ Length[ walk ] >= 2 && First @ walk === Last @ walk, Most @ walk, walk ] ] },
    Graph[ core, DirectedEdge @@@ Partition[ core, 2, 1, 1 ] ] ]

closedWalkQ[ w_Graph ] := ! LoopFreeGraphQ[ w ] || ! AcyclicGraphQ[ w ]

positionSpelledQ[ w_Graph ] :=
  AllTrue[ VertexList @ w, MatchQ[ { _Integer, _ } ] ] &&
  Sort[ First /@ VertexList @ w ] === Range @ VertexCount @ w

(* the vertex sequence of one walk graph, the cyclic core of a closed one; a substrate path or cycle is read by following its edges from an end, or from its first vertex when it has none *)
walkSequence[ w_Graph ] := Which[
  positionSpelledQ @ w, Last /@ SortBy[ VertexList @ w, First ],
  EdgeCount[ w ] == 0,  VertexList @ w,
  True,
    With[ { start = SelectFirst[ VertexList @ w,
              If[ DirectedGraphQ @ w, VertexInDegree[ w, # ] == 0, VertexDegree[ w, # ] == 1 ] &,
              First @ VertexList @ w ],
            nextOf = If[ DirectedGraphQ @ w,
              { u, prev } |-> First[ VertexOutComponent[ w, { u }, { 1 } ], None ],
              { u, prev } |-> First[ DeleteCases[ AdjacencyList[ w, u ], prev ], None ] ] },
      { seq = TakeWhile[
          First /@ NestList[ { nextOf @@ #, First @ # } &, { start, None }, VertexCount @ w ],
          # =!= None & ] },
      If[ closedWalkQ @ w, Most @ seq, seq ] ] ]

(* the walks a graph stands for, as vertex sequences: one for a path graph or a cycle (closed, first vertex repeated at the end), the source -> sink paths for a DAG *)
walkRealisations[ w_Graph ] := Which[
  closedWalkQ @ w,      { closeWalk @ walkSequence @ w },
  positionSpelledQ @ w, Map[ Last, dagGeodesics @ w, { 2 } ],
  DirectedGraphQ @ w,   dagGeodesics @ w,
  True,                 { walkSequence @ w } ]

walkVertexSet[ w_Graph ] :=
  Sort @ DeleteDuplicates @ If[ positionSpelledQ @ w, Last /@ VertexList @ w, VertexList @ w ]


(* ===================== The method ladder ===================== *)

(* Automatic resolves by the count: All asks for the whole class, hence the exhaustive pool, and any bounded or absent count for that many certified instances, which the deterministic lazy descent supplies exactly and reproducibly -- exponential enumeration and randomness are both opt-in *)

resolveMethod[ Automatic, All ]  = "Exhaustive";
resolveMethod[ Automatic, _ ]    = "Greedy";
resolveMethod[ spec_, _ ]        := spec


(* Identity explores every candidate in candidateFn order and backtracks ("Greedy"); RandomSample the same in uniformly random order ("RandomGreedy"), equally complete -- the shuffle moves which realisations are reached first, never which exist *)

greedyBranch[ "Greedy" ]        = Identity;
greedyBranch[ "RandomGreedy" ]  = RandomSample;


(* ===================== Cycle helper ===================== *)

(* FindCycle edge cycle -> open vertex sequence (wrap-around implicit). *)

cycleToVertexSequence[ cyc_List ] := First /@ cyc


(* ===================== Count semantics ===================== *)

(* a count argument as a numeric upper bound; Automatic is one instance -- asking without a count asks for a witness, not for the class *)

countLimit[ All ]               = Infinity
countLimit[ Infinity ]          = Infinity
countLimit[ Automatic ]         = 1
countLimit[ UpTo[ n_Integer ] ] := n
countLimit[ n_Integer ]         := n

takeUpTo[ list_, Infinity ]     := list
takeUpTo[ list_, n_Integer ]    := Take[ list, UpTo[ n ] ]


allGeodesics[ graph_Graph, u_, v_, count_ : All ] :=
  With[ { d = GraphDistance[ graph, u, v ] },
    If[ d === Infinity, { }, FindPath[ graph, u, v, { d }, count ] ]
  ]


(* a beam width (integer cap, random sampling if exceeded) or a Bernoulli keep probability, with a one-element floor so the bundle never dies by chance *)

applyPruning[ paths_List, Infinity ]                     := paths
applyPruning[ paths_List, n_Integer /; n >= 1 ]          :=
  If[ Length[ paths ] <= n, paths, RandomSample[ paths, n ] ]
applyPruning[ { }, p_?NumericQ /; 0 < p < 1 ]            := { }
applyPruning[ paths_List, p_?NumericQ /; 0 < p < 1 ]     :=
  With[ { kept = Select[ paths, RandomReal[ ] < p & ] },
    If[ kept === { }, RandomSample[ paths, 1 ], kept ] ]


(* ===================== Frontier sweep ===================== *)

(* BFS frontier from p1 to p2, applyPruning capping the live frontier per layer.  terminalQ = True ends a branch at its first arrival at p2; False emits every arrival and keeps extending through it, so the caller's candidateFn must bound the depth *)

frontierSweep[ graph_Graph, p1_, p2_, candidateFn_, prune_, count_, terminalQ_ : True ] :=
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
      frontier  = applyPruning[
        If[ terminalQ, Select[ extended, Last[ # ] =!= p2 & ], extended ], prune ]
    ];
    Take[ completed, UpTo[ count ] ]
  ]


(* lazy depth-first descent from p1 to p2: take the admissible candidates in candidateFn order, backtrack when a branch dead-ends, and stop after count completions accepted by acceptQ.  Complete, so a finite count is exact -- the certified-instance engine Automatic resolves to for a bounded count.
   maxSteps caps the depth; a walk-family caller with revisits allowed needs it to guarantee termination.  terminalQ = True ends a branch at its first arrival at p2, False keeps descending through it. *)

(* the closures are held in Module locals, never inlined into descend's RHS: a candidateFn built by windowCandidateFn is a Function[{g, walk}, ...], and substituting a pattern variable of the same name would rewrite the closure's own parameter list *)

greedyFrontierSweep[ graph_Graph, p1_, p2_, candidateFn_, acceptQ_, maxSteps_, count_,
    branch_ : Identity, terminalQ_ : True ] :=
  If[ p1 === p2 || ! VertexQ[ graph, p1 ] || ! VertexQ[ graph, p2 ] ||
      GraphDistance[ graph, p1, p2 ] === Infinity, { },
    Module[ { cap = countLimit @ count, acc = { }, descend,
              cands = candidateFn, keepQ = acceptQ, pick = branch, terminal = terminalQ },
      descend[ walk_ ] := (
        If[ Last @ walk === p2 && keepQ @ walk,
          AppendTo[ acc, walk ];
          If[ Length @ acc >= cap, Throw[ acc, greedyFrontierSweep ] ] ];
        If[ ( ! terminal || Last @ walk =!= p2 ) && Length[ walk ] - 1 < maxSteps,
          Scan[ descend[ Append[ walk, # ] ] &, pick @ cands[ graph, walk ] ] ]
      );
      Catch[ descend[ { p1 } ]; acc, greedyFrontierSweep ]
    ]
  ]


(* ===================== Window-rule machinery ===================== *)


propertiesSubOpts[ s_String ]              := { }
propertiesSubOpts[ { _String, opts___ } ]  := { opts }


(* every rule sees one thing -- the window: the last <= scale vertices of the walk with the candidate appended (the whole walk at scale Infinity).  Constraints are checked first and commute, the singularity exclusions reading the whole walk prefix since their violations are monotone under extension; selectors follow in list order, each refining the previous ties *)

windowCandidateFn[ graph_Graph, scale_, rules_List, fnSym_ ] :=
  With[ { species = windowRuleSpecies[ #, fnSym ] & /@ rules },
    { constraints = Pick[ rules, species, "Constraint" ],
      selectors   = windowSelector[ graph, scale, # ] & /@ Pick[ rules, species, "Selector" ] },
    { g, walk } |->
      Fold[ #2[ walk, #1 ] &,
        Select[ AdjacencyList[ g, Last @ walk ],
          w |-> AllTrue[ constraints, windowRuleQ[ g, scale, #, walk, w ] & ] ],
        selectors ]
  ]


(* fnSym is the calling head symbol, not fnSym::badproperty -- a MessageName passed as a bare argument evaluates to its template string, so Message would emit Message::name *)

windowRuleSpecies[ rule_, fnSym_ ] :=
  Switch[ rule,
    "Minimizing" | "Simple" | "Immersed" | "Generic" |
      ( "Exclude" -> ( "SelfIntersections" | "SelfTangencies" | "Cusps" | "TriplePoints" |
          { ( "SelfIntersections" | "SelfTangencies" | "Cusps" | "TriplePoints" ) .. } ) ),
                                          "Constraint",
    "Straightest",                        "Selector",
    { "Minimal", _ } | { "Maximal", _ },  "Selector",
    _String | { _String, ___ } | _Rule,
      ( Message[ MessageName[ fnSym, "badproperty" ], rule ]; Throw[ $Failed ] ),
    _,                                    "Constraint"
  ]


(* the species a rule list forbids: "Simple", "Immersed" and "Generic" name the standard exclusion sets, "Generic" adding endpoint freeness on the finished two-point walk *)
excludedSpecies[ rules_List ] := Union @@ Replace[ rules, {
  "Simple"   -> { "SelfIntersections" },
  "Immersed" -> { "Cusps" },
  "Generic"  -> { "Cusps", "SelfTangencies", "TriplePoints" },
  ( "Exclude" -> s_ ) :> Flatten @ { s },
  _ -> { } }, { 1 } ]


(* "Minimizing": the window is a shortest path. *)
windowRuleQ[ g_Graph, scale_, "Minimizing", walk_List, w_ ] :=
  With[ { win = walkWindow[ walk, scale ] },
    GraphDistance[ g, First @ win, w ] == Length[ win ] ]

(* each excluded species is refused exactly at the step that would create it, so per-step pruning is exact; endpoint freeness is not monotone and is the caller's finished-walk check *)
windowRuleQ[ g_Graph, scale_, rule : "Simple" | "Immersed" | "Generic" | ( "Exclude" -> _List ), walk_List, w_ ] :=
  AllTrue[ excludedSpecies @ { rule }, windowRuleQ[ g, scale, "Exclude" -> #, walk, w ] & ]

windowRuleQ[ _Graph, _, "Exclude" -> "SelfIntersections", walk_List, w_ ] := ! MemberQ[ walk, w ]

(* an apex can only form at the tip *)
windowRuleQ[ _Graph, _, "Exclude" -> "Cusps", walk_List, w_ ] :=
  Length[ walk ] < 2 || walk[[ -2 ]] =!= w

windowRuleQ[ _Graph, _, "Exclude" -> "TriplePoints", walk_List, w_ ] := Count[ walk, w ] <= 1

(* a repeated edge opens a repeated arc unless it is the mirror of a cusp, the stretch between the two traversals then being a palindrome *)
windowRuleQ[ _Graph, _, "Exclude" -> "SelfTangencies", walk_List, w_ ] :=
  NoneTrue[ Range[ Length[ walk ] - 1 ],
    p |-> ( walk[[ p ]] === Last[ walk ] && walk[[ p + 1 ]] === w ) ||
      ( walk[[ p ]] === w && walk[[ p + 1 ]] === Last[ walk ] && ! PalindromeQ[ walk[[ p + 1 ;; ]] ] ) ]

(* a bare predicate is a custom local law on the window *)
windowRuleQ[ _Graph, scale_, pred_, walk_List, w_ ] :=
  pred @ Append[ walkWindow[ walk, scale ], w ]


(* "Straightest": maximise the distance tuple from the candidate back along the window, nearest first -- the immediate predecessor sits at distance 1 for every candidate and carries no information *)
windowSelector[ graph_Graph, scale_, "Straightest" ] :=
  With[ { vidx = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ],
          dmat = GraphDistanceMatrix[ graph ] },
    { walk, candidates } |->
      With[ { historyIdx = vidx /@ Reverse @ Most @ walkWindow[ walk, scale ] },
        If[ candidates === { } || historyIdx === { }, candidates,
          MaximalBy[ candidates, w |-> dmat[[ historyIdx, vidx[ w ] ]] ] ]
      ]
  ]

windowSelector[ _Graph, scale_, { "Minimal", f_ } ] :=
  { walk, candidates } |->
    If[ candidates === { }, candidates,
      MinimalBy[ candidates, w |-> f @ Append[ walkWindow[ walk, scale ], w ] ] ]

windowSelector[ _Graph, scale_, { "Maximal", f_ } ] :=
  { walk, candidates } |->
    If[ candidates === { }, candidates,
      MaximalBy[ candidates, w |-> f @ Append[ walkWindow[ walk, scale ], w ] ] ]


(* The walk-side of the window: its last <= scale vertices. *)

walkWindow[ walk_List, Infinity ]          := walk
walkWindow[ walk_List, scale_Integer ]     := Take[ walk, -Min[ scale, Length[ walk ] ] ]


(* ===================== Separating sets ===================== *)

(* removing vs leaves a component containing center inside the closed ball B(center, radius), every vertex outside that component lying strictly beyond radius *)

SeparatingSetQ[ graph_Graph, vs_List, center_, radius_ ] :=
  With[ { rem = VertexDelete[ graph, vs ] },
    { centerComp = SelectFirst[ ConnectedComponents[ rem ], MemberQ[ #, center ] & ] },
    centerComp =!= Missing[ "NotFound" ] &&
    AllTrue[ centerComp, GraphDistance[ graph, center, # ] <= radius & ] &&
    AllTrue[ Complement[ VertexList[ rem ], centerComp ], GraphDistance[ graph, center, # ] > radius & ]
  ]


(* top-down peel toward inclusion-minimal admissible subsets: both helpers terminate when no admissible single-removal exists, so minimality is automatic at the leaves *)

(* lazy depth-first peel: remove one admissible vertex at a time, backtracking at each leaf, and stop after count distinct inclusion-minimal admissible subsets.  Complete, so a finite count is exact; DeleteCases preserves the order of set, so every state is already in canonical form and doubles as its own visited key -- without the visited set the same subset is re-descended once per peel order, and All ran minutes where the BFS takes a second *)

findGreedyMinimalAdmissible[ graph_Graph, set_List, admissible_, count_,
    branch_ : Identity ] :=
  If[ ! admissible[ set ], { },
    (* admissible and branch are held in Module locals, not inlined into descend's RHS: see the note on greedyFrontierSweep above *)
    Module[ { cap = countLimit @ count, acc = { }, seen = <||>, descend,
              admitQ = admissible, pick = branch },
      descend[ T_ ] :=
        If[ ! KeyExistsQ[ seen, T ],
          seen[ T ] = True;
          With[ { removable = Select[ T, w |-> admitQ[ DeleteCases[ T, w ] ] ] },
            If[ removable === { },
              AppendTo[ acc, T ];
              If[ Length @ acc >= cap, Throw[ acc, findGreedyMinimalAdmissible ] ],
              Scan[ descend[ DeleteCases[ T, # ] ] &, pick @ removable ] ] ] ];
      Catch[ descend[ set ]; acc, findGreedyMinimalAdmissible ]
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


(* ===================== The geodesic class ===================== *)

(* THE SHAPE IS THE KIND, so a construction returns its carrier bare.  An open geodesic is a directed path graph on the substrate vertices and a closed one a directed cycle on them; a Graph passes through, so a pool atom -- a geodesic DAG -- is already in shape.  The general walk family spells its walks on position pairs instead (walkGraph / closedWalkGraph above), since a walk revisits and a geodesic never does.  A set is the sorted, duplicate-free List, the shape Wolfram's own set algebra takes; the Association is reserved for densities, where multiplicity is real *)

geodesicGraph[ seq_List ] := PathGraph[ seq, DirectedEdges -> True ]
geodesicGraph[ w_Graph ]  := w

geodesicCycleGraph[ seq_List ] :=
  With[ { core = If[ Length[ seq ] >= 2 && First @ seq === Last @ seq, Most @ seq, seq ] },
    Graph[ core, DirectedEdge @@@ Partition[ core, 2, 1, 1 ] ] ]
geodesicCycleGraph[ w_Graph ] := w

vertexSet[ vs_List ] := Sort @ DeleteDuplicates @ vs


(* ===================== The count contract ===================== *)

(* count-less is ONE instance -- a witness, { } when there is none; a bounded count a List of them, a strict n failing when fewer exist; All the whole List.  Points are their own shape, so a point finder's instance is the bare vertex *)

countTake[ reps_List, Automatic ]         := First[ reps, { } ]
countTake[ reps_List, All ]               := reps
countTake[ reps_List, UpTo[ n_Integer ] ] := Take[ reps, UpTo[ n ] ]
countTake[ reps_List, n_Integer ]         := If[ Length @ reps < n, $Failed, Take[ reps, n ] ]

(* a geodesic bundle is the union of its walks, so a bundle of one carrier under All stands alone: a lone DAG is the bundle, a lone path graph its own bundle.  Closed families and set families have no acyclic union and stay Lists *)
loneBundle[ { one_Graph } ] := one
loneBundle[ other_ ]        := other


(* ===================== The realisation spread ===================== *)

(* the REALISATION spread, not the anchor rule: an Association spreads over its support, a walk graph into the vertex sequences it stands for, a list of graphs into all of theirs; anything else -- a bare vertex, and a walk or a set written as a vertex list -- is one realisation.  toDensity is where a List is read as a multiset *)

infraSpread[ fam_Association ]       := Keys @ fam
infraSpread[ w_Graph ]               := walkRealisations @ w
infraSpread[ ws : { __Graph } ]      := Catenate[ walkRealisations /@ ws ]
infraSpread[ { } ]                   := { }
infraSpread[ other_ ]                := { other }


(* spread each anchor, run the single-tuple core over the Cartesian product, put every realisation into its shape, union-deduplicate and apply the count contract.  shape is the per-realisation carrier: geodesicGraph, geodesicCycleGraph, vertexSet, walkGraph, closedWalkGraph, or Identity *)

spreadFind[ shape_, count_, core_, anchors__ ] :=
  With[ { results = core @@@ Tuples[ infraSpread /@ { anchors } ] },
    If[ MemberQ[ results, $Failed ], $Failed,
      countTake[ DeleteDuplicates[ shape /@ DeleteDuplicates @ Flatten[ results, 1 ] ], count ] ] ]


(* the geodesic class: open walks as substrate path graphs, pool atoms passing through, and a lone carrier under All standing alone *)

geodesicTake[ reps_List, count_ ] :=
  Replace[ countTake[ DeleteDuplicates[ geodesicGraph /@ DeleteDuplicates @ reps ], count ],
    l_List /; count === All :> loneBundle @ l ]

geodesicFind[ count_, core_, anchors__ ] :=
  Replace[ spreadFind[ geodesicGraph, count, core, anchors ],
    l_List /; count === All :> loneBundle @ l ]


(* all source -> sink directed paths, the one exponential step, materialised on demand; dagGeodesics[dag, limit] is the lazy form, a DFS stopping as soon as limit geodesics are collected *)

dagGeodesics[ dag_Graph ] := Which[
  VertexCount[ dag ] == 0, { },
  EdgeCount[ dag ] == 0,   List /@ VertexList[ dag ],
  True,
    With[ { srcs = Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ],
            snks = Select[ VertexList[ dag ], VertexOutDegree[ dag, # ] == 0 & ] },
      DeleteDuplicates @ Catenate @ Catenate @
        Table[ FindPath[ dag, s, t, Infinity, All ], { s, srcs }, { t, snks } ] ] ]

dagGeodesics[ dag_Graph, All | Infinity, _ : Identity ] := dagGeodesics[ dag ]
dagGeodesics[ dag_Graph, limit_, branch_ : Identity ] := Which[
  VertexCount[ dag ] == 0, { },
  EdgeCount[ dag ] == 0,   Take[ List /@ VertexList[ dag ], UpTo[ countLimit @ limit ] ],
  True,
    (* bounded DFS with early Throw -- a built-in cannot stop mid-enumeration; branch orders the candidates at every node (RandomSample is "RandomGreedy") *)
    Module[ { out = GroupBy[ List @@@ EdgeList[ dag ], First -> Last ],
              cap = countLimit @ limit, acc = { }, pick = branch, go },
      go[ path_ ] := With[ { nexts = Lookup[ out, Key @ Last @ path, { } ] },
        If[ nexts === { },
          ( AppendTo[ acc, path ]; If[ Length @ acc >= cap, Throw[ acc, dagGeodesics ] ] ),
          Scan[ go[ Append[ path, # ] ] &, pick @ nexts ] ] ];
      Catch[
        Scan[ go[ { # } ] &,
          pick @ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] ];
        acc, dagGeodesics ] ] ]


(* v -> d(source, v): the DAG is layer-aligned, so position i along every geodesic is layer i - 1 *)

dagLayers[ dag_Graph ] :=
  If[ VertexCount[ dag ] == 0, <||>,
    With[ { s = First @ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] },
      AssociationThread[ VertexList[ dag ], GraphDistance[ dag, s ] ] ] ]


(* ===================== Vertex sets of shapes ===================== *)

(* the support of any shape, without the graph: a density's keys, a walk graph's vertices (substrate-spelled through the position pairs), the union over a list of graphs or of vertex lists, a vertex list itself.  With the graph in hand, Keys @ toDensity[graph, x] is the anchor-rule reading and tells a list-labelled vertex from a set *)

infraVertexSet[ fam_Association ]  := Keys @ fam
infraVertexSet[ w_Graph ]          := walkVertexSet @ w
infraVertexSet[ ws : { __Graph } ] := Union @@ ( walkVertexSet /@ ws )
infraVertexSet[ { } ]              := { }
infraVertexSet[ sets : { __List } ] := Union @@ ( infraVertexSet /@ sets )
infraVertexSet[ list_List ]        := vertexSet @ list
infraVertexSet[ v_ ]               := { v }

infraVertexSet[ graph_Graph, x_ ]  := Keys @ toDensity[ graph, x ]


PackageScope[hullVertices]

hullVertices[ s_ ] := infraVertexSet[ s ]


(* ===================== Occupation of shapes ===================== *)

(* the raw occupation count c(v) = total appearances across realisations, the association InfraDensity publishes and InfraEqualQ compares.  For a density the multiset IS the object, for every other shape a lossy projection; a substrate DAG contributes its whole family's occupation by the Brandes DP, exactly as the enumerated family would *)

infraVertexMultiset[ fam_Association ]  := fam
infraVertexMultiset[ w_Graph ] := Which[
  closedWalkQ @ w,      Counts @ walkSequence @ w,
  positionSpelledQ @ w, Counts @ Catenate @ walkRealisations @ w,
  True,                 GeodesicOccupation @ w ]
infraVertexMultiset[ ws : { __Graph } ]  := Merge[ infraVertexMultiset /@ ws, Total ]
infraVertexMultiset[ { } ]               := <||>
infraVertexMultiset[ sets : { __List } ] := Merge[ Counts /@ sets, Total ]
infraVertexMultiset[ vs_List ]           := Counts @ vs


(* the raw count of appearances across realisations, keyed by sorted vertex pair; a set's edges are the induced subgraph's, hence the graph *)

infraEdgeMultiset[ _, _Association ] := <||>
infraEdgeMultiset[ g_, w_Graph ] := Which[
  closedWalkQ @ w,      Counts @ cycleEdges @ walkSequence @ w,
  positionSpelledQ @ w, Merge[ Counts[ walkEdges @ # ] & /@ walkRealisations @ w, Total ],
  True,                 KeyMap[ Sort[ List @@ # ] &, GeodesicEdgeOccupation[ w ] ] ]
infraEdgeMultiset[ g_, ws : { __Graph } ]  := Merge[ infraEdgeMultiset[ g, # ] & /@ ws, Total ]
infraEdgeMultiset[ _, { } ]                := <||>
infraEdgeMultiset[ g_, sets : { __List } ] := Merge[ Counts[ setEdges[ g, # ] ] & /@ sets, Total ]
infraEdgeMultiset[ g_, vs_List ]           := Counts @ setEdges[ g, vs ]


(* N = the number of realisations the marginal was summed over: a density's largest mass, a walk's 1, a DAG's geodesic count, a list's sum over its members.  A measure normalises by its HEAVIEST mass, not its total: the channel encodes RELATIVE mass within the object, so the modal vertex draws full and lighter ones fade *)

infraNumReps[ fam_Association ] := If[ Length @ fam === 0, 1, Max @ fam ]
infraNumReps[ w_Graph ] :=
  If[ closedWalkQ @ w || positionSpelledQ @ w, 1,
    With[ { occ = GeodesicOccupation[ w ] }, If[ Length @ occ === 0, 1, Max @ Values @ occ ] ] ]
infraNumReps[ ws : { __Graph } ]  := Max[ Total[ infraNumReps /@ ws ], 1 ]
infraNumReps[ { } ]               := 1
infraNumReps[ sets : { __List } ] := Length @ sets
infraNumReps[ _List ]             := 1


(* ===================== Instances, families, densities ===================== *)

(* A MULTISET is a finitely supported measure, <| atom -> weight |>, and carries no head: Counts, Merge, KeyMap, Total and KeySelect are its algebra, Keys its support.  A List is the multiset with uniform weight, one Counts away.  A DENSITY is the 0-d case, a multiset on bare vertices -- the marginal of anything to the vertex set, with respect to the counting measure.

   the ANCHOR RULE: every anchor argument of every construction is read through toDensity, so points, sets and objects all work in every construction under one coercion.  A vertex is the unit mass, a List its Counts, an Association itself, a graph its vertex occupation, a list of graphs or of vertex lists the sum over its members.  The branches are ordered rather than left to DownValue sorting, since a vertex label may itself be a List and only pointQ tells the two rows apart.

   keys sorted, so densities built by different routes compare SameQ; this was the one guarantee the old measure head carried *)

toDensity[ graph_Graph, x_ ] := Which[
  pointQ[ graph, x ],                                   <| x -> 1 |>,
  AssociationQ[ x ],                                    KeySort @ x,
  GraphQ[ x ],                                          KeySort @ infraVertexMultiset @ x,
  ListQ[ x ] && AllTrue[ x, VertexQ[ graph, # ] & ],    KeySort @ Counts @ x,
  MatchQ[ x, { ( _Graph | _List ) .. } ],               KeySort @ Merge[ toDensity[ graph, # ] & /@ x, Total ],
  ListQ[ x ],                                           KeySort @ Counts @ x,
  True,                                                 <| x -> 1 |> ]


(* ===================== InfraDensity ===================== *)

(* the marginal of any shape to the vertex set, <| v -> m |>, with respect to the counting measure: the anchor rule made public, and the ONE coercion in the API.  Counts promotes a List to a density, Keys demotes it back to the set, and Merge / KeyMap / Total / KeySelect are the rest of its algebra -- so no normalisation belongs here.  Vertex-only: the renderer computes its edge weights internally, off infraEdgeMultiset *)

InfraDensity[ graph_Graph, x_ ] := toDensity[ graph, x ]


(* the edges of one realisation: a vertex sequence read as a walk, as a closed walk, or as a set with its induced edges.  Keyed by sorted pair {a, b}, which the renderer remaps to UndirectedEdge *)

walkEdges[ seq_List ] :=
  If[ Length @ seq >= 2, Sort /@ Partition[ seq, 2, 1 ], { } ]

cycleEdges[ seq_List ] :=
  walkEdges @ If[ Length @ seq >= 2 && First @ seq === Last @ seq, seq, Append[ seq, First @ seq ] ]

setEdges[ None, _ ]           := { }
setEdges[ g_Graph, vs_List ]  := Sort /@ ( List @@@ EdgeList @ Subgraph[ g, vs ] )


(* the vertex set of a line-like shape: a walk graph, a bundle, or a bare vertex sequence *)

linePointSet[ w_Graph ]          := walkVertexSet[ w ]
linePointSet[ ws : { __Graph } ] := Union @@ ( walkVertexSet /@ ws )
linePointSet[ line_List ]        := line
