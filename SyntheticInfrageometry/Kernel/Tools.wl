Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[dagGeodesics]
PackageScope[dagLayers]
PackageScope[segReps]
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
PackageScope[infraCap]
PackageScope[spreadFind]
PackageScope[bundleTake]
PackageScope[$infraBundleHeads]
PackageScope[defineInfraBundleRules]
PackageScope[infraVertexMultiset]
PackageScope[infraEdgeMultiset]
PackageScope[atomVertexMasses]
PackageScope[atomEdgeMasses]
PackageScope[atomFamilySize]
PackageScope[infraRepType]
PackageScope[infraRepSeqs]
PackageScope[infraRepVerts]
PackageScope[infraRepEdges]
PackageScope[infraNumReps]
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


(* ===================== The method ladder ===================== *)

(* Automatic resolves by the count: All asks for the whole class, hence the exhaustive pool, and any bounded count for that many certified instances, which the lazy greedy descent supplies exactly -- so exponential enumeration is opt-in.
   The bounded branch descends in random order: the class is not canonically ordered, so an always-first witness would be an artefact of the enumeration, not of the geometry. *)

resolveMethod[ Automatic, All ]  = "Exhaustive";
resolveMethod[ Automatic, _ ]    = "RandomGreedy";
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

(* lazy depth-first peel: remove one admissible vertex at a time, backtracking at each leaf, and stop after count distinct inclusion-minimal admissible subsets.  Complete, so a finite count is exact; DeleteCases preserves the order of set, so every leaf is already in canonical form *)

findGreedyMinimalAdmissible[ graph_Graph, set_List, admissible_, count_,
    branch_ : Identity ] :=
  If[ ! admissible[ set ], { },
    (* admissible and branch are held in Module locals, not inlined into descend's RHS: see the note on greedyFrontierSweep above *)
    Module[ { cap = countLimit @ count, acc = { }, descend,
              admitQ = admissible, pick = branch },
      descend[ T_ ] :=
        With[ { removable = Select[ T, w |-> admitQ[ DeleteCases[ T, w ] ] ] },
          If[ removable === { },
            If[ ! MemberQ[ acc, T ],
              AppendTo[ acc, T ];
              If[ Length @ acc >= cap, Throw[ acc, findGreedyMinimalAdmissible ] ] ],
            Scan[ descend[ DeleteCases[ T, # ] ] &, pick @ removable ] ] ];
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


(* ===================== Multi-realisation wrapper helpers ===================== *)

(* a multi-object is a SET of realisations, canonical iff duplicate-free: realisations are alternative witnesses of one construction, all equally admissible, so no invariant distinguishes them and there is no mass channel here.  The vertex / edge measure of a bundle is a lossy PROJECTION off it (InfraMeasure), never stored state, and the one head whose projection is lossless is InfraPoint.
   A realisation slot holds either an explicit realisation or a compact atom (a geodesic-DAG Graph, InfraSegment only) standing for its whole family. *)

(* the single source of truth for the canonicalisation rules and measure dispatch *)
$infraBundleHeads = InfraSegment | InfraLine | InfraWalk | InfraLoop | InfraString |
  InfraShell | InfraBall | InfraEllipticShell | InfraPlane | InfraCircle | InfraEllipse |
  InfraPolygon | InfraTriangle | InfraRay | InfraPolyline;


(* the AllTrue[.., ListQ || GraphQ] realisation guard keeps scene-language forms (InfraShell[c, r]) inert; on graphs whose vertices are themselves lists a scene form is indistinguishable from a bundle *)

defineInfraBundleRules[ head_Symbol ] := (
  (* idempotency: re-wrapping a wrapper is the identity *)
  head[ inner_head ] := inner;
  head[ reps_List ] /; AnyTrue[ reps, MatchQ[ head[ _List ] ] ] :=
    head[ Flatten[ reps /. head[ xs_List ] :> xs, 1 ] ];
  head[ reps_List ] /; AllTrue[ reps, ListQ[ # ] || GraphQ[ # ] & ] && ! DuplicateFreeQ[ reps ] :=
    head[ DeleteDuplicates @ reps ];
  head[ reps : Except[ { __Graph }, _List ] ][ "Realizations" ] := reps;
  head[ reps : Except[ { __Graph }, _List ] ][ "First" ]        := First @ reps;
  head[ reps : Except[ { __Graph }, _List ] ][ "OccupationCount" ]    := infraVertexMultiset[ head[ reps ] ];
  head[ reps : Except[ { __Graph }, _List ] ][ "OccupationMeasure" ]  := InfraMeasure[ head[ reps ] ];
  head[ reps : Except[ { __Graph }, _List ] ][ "Measure" ]            := InfraMeasure[ head[ reps ] ];
  head[ reps : Except[ { __Graph }, _List ] ][ "ProbabilityMeasure" ] := InfraMeasure[ head[ reps ], Method -> "Probability" ];
)

Scan[ defineInfraBundleRules,
  List @@ $infraBundleHeads ]


(* a multi-realisation wrapper or a list of unary wrappers spreads into its bare realisations -- an InfraPoint over its support, since the measure is not an anchor property but is reconstructed at the projection; anything else becomes a singleton *)

infraSpread[ InfraPoint[ v_ ] ] := { v }
infraSpread[ list : { __InfraPoint } ] := #[[ 1 ]] & /@ list
With[ { heads = $infraBundleHeads },
  infraSpread[ heads[ reps_List ] ] := reps;
  infraSpread[ list_List ] /; AllTrue[ list, MatchQ[ heads[ { _ } ] ] ] :=
    #[[ 1, 1 ]] & /@ list
]
infraSpread[ InfraEffectivePoint[ m_Association ] ] := Keys @ m
infraSpread[ InfraSet[ vs_List ] ] := vs
infraSpread[ InfraSegment[ dag_Graph ] ] := dagGeodesics[ dag ]
infraSpread[ InfraSegment[ dags : { _Graph, __Graph } ] ] := Join @@ ( dagGeodesics /@ dags )
infraSpread[ InfraCircle[ dags : { __Graph } ] ] := Catenate[ dagGeodesics /@ dags ]
infraSpread[ InfraLine[ dags : { __Graph } ] ]   := Catenate[ dagGeodesics /@ dags ]
infraSpread[ other_ ] := { other }


(* support = the i-th vertices of the realisations long enough to have one, masses = their multiplicities: one of the projections at which a measure is constructed.  i may be negative *)

PackageScope[columnInfraPoint]

columnInfraPoint[ reps_List, i_Integer ] :=
  InfraEffectivePoint @ Counts[ ( #[[ i ]] & ) /@ Select[ reps, Length[ # ] >= Abs[ i ] & ] ]


(* all source -> sink directed paths, the one exponential step, materialised on demand; dagGeodesics[dag, limit] is the lazy form, a DFS stopping as soon as limit geodesics are collected *)

dagGeodesics[ dag_Graph ] := Which[
  VertexCount[ dag ] == 0, { },
  EdgeCount[ dag ] == 0,   List /@ VertexList[ dag ],
  True,
    With[ { srcs = Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ],
            snks = Select[ VertexList[ dag ], VertexOutDegree[ dag, # ] == 0 & ] },
      DeleteDuplicates @ Catenate @ Catenate @
        Table[ FindPath[ dag, s, t, Infinity, All ], { s, srcs }, { t, snks } ] ] ]

dagGeodesics[ dag_Graph, All | Infinity ] := dagGeodesics[ dag ]
dagGeodesics[ dag_Graph, limit_ ] := Which[
  VertexCount[ dag ] == 0, { },
  EdgeCount[ dag ] == 0,   Take[ List /@ VertexList[ dag ], UpTo[ countLimit @ limit ] ],
  True,
    (* bounded DFS with early Throw -- a built-in cannot stop mid-enumeration *)
    Module[ { out = GroupBy[ List @@@ EdgeList[ dag ], First -> Last ],
              cap = countLimit @ limit, acc = { }, go },
      go[ path_ ] := With[ { nexts = Lookup[ out, Key @ Last @ path, { } ] },
        If[ nexts === { },
          ( AppendTo[ acc, path ]; If[ Length @ acc >= cap, Throw[ acc, dagGeodesics ] ] ),
          Scan[ go[ Append[ path, # ] ] &, nexts ] ] ];
      Catch[
        Scan[ go[ { # } ] &,
          Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] ];
        acc, dagGeodesics ] ] ]


(* v -> d(source, v): the DAG is layer-aligned, so position i along every geodesic is layer i - 1 *)

dagLayers[ dag_Graph ] :=
  If[ VertexCount[ dag ] == 0, <||>,
    With[ { s = First @ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] },
      AssociationThread[ VertexList[ dag ], GraphDistance[ dag, s ] ] ] ]


segReps[ InfraSegment[ dag_Graph ] ]        := dagGeodesics[ dag ]
segReps[ InfraSegment[ reps_List, ___ ] ]   :=
  Catenate[ If[ GraphQ[ # ], dagGeodesics[ # ], { # } ] & /@ reps ]
segReps[ ws : { ___InfraSegment } ]         := Catenate[ segReps /@ ws ]


PackageScope[hullVertices]

hullVertices[ s_List ] /; AnyTrue[ s, StringStartsQ[ SymbolName @ Head @ #, "Infra" ] & ] :=
  infraVertexSet[ s ]
hullVertices[ s_List ] := s
hullVertices[ s_ ] := infraVertexSet[ s ]


(* $Failed is the mathematical "fewer than n exist" case; the count-less Automatic is soft, one witness if there is one *)

infraCap[ list_List, All ]                              := list
infraCap[ list_List, Automatic ]                        := Take[ list, UpTo[ 1 ] ]
infraCap[ list_List, UpTo[ n_Integer ] ]                := Take[ list, UpTo[ n ] ]
infraCap[ list_List, n_Integer ] /; n <= Length[ list ] := Take[ list, n ]
infraCap[ _List, _Integer ]                             := $Failed


(* spread each anchor, run the single-tuple core over the Cartesian product, union-deduplicate, and apply the count contract; returns ONE wrapper *)

spreadFind[ wrapHead_, count_, core_, anchors__ ] :=
  With[ { results = core @@@ Tuples[ infraSpread /@ { anchors } ] },
    If[ MemberQ[ results, $Failed ], $Failed,
      bundleTake[ wrapHead, DeleteDuplicates @ Flatten[ results, 1 ], count ] ] ]


(* the count contract on a realisation set: strict n fails on under-supply. *)

(* the point family is a plain List of atoms, not a wrapper: the FindClique / FindInstance shape *)
bundleTake[ InfraPoint, reps_, All ]               := InfraPoint /@ reps
bundleTake[ InfraPoint, reps_, Automatic ]         := InfraPoint /@ Take[ reps, UpTo @ 1 ]
bundleTake[ InfraPoint, reps_, UpTo[ n_Integer ] ] := InfraPoint /@ Take[ reps, UpTo @ n ]
bundleTake[ InfraPoint, reps_, n_Integer ]         :=
  If[ Length @ reps < n, $Failed, InfraPoint /@ Take[ reps, n ] ]

bundleTake[ head_, reps_, All ]               := head[ reps ]
bundleTake[ head_, reps_, Automatic ]         := head[ Take[ reps, UpTo @ 1 ] ]
bundleTake[ head_, reps_, UpTo[ n_Integer ] ] := head[ Take[ reps, UpTo @ n ] ]
bundleTake[ head_, reps_, n_Integer ]         :=
  If[ Length @ reps < n, $Failed, head[ Take[ reps, n ] ] ]


(* the raw occupation count c(v) = total appearances across realisations, the association InfraMeasure normalises and InfraEqualQ compares.  For an InfraPoint the measure IS the point, for every other head a lossy projection; a compact geodesic-DAG atom contributes its whole family's occupation by DP, exactly as the enumerated family would *)

infraVertexMultiset[ InfraEffectivePoint[ m_Association ] ] := m
infraVertexMultiset[ InfraPoint[ v_ ] ] := <| v -> 1 |>
infraVertexMultiset[ ( InfraObject | InfraSet )[ vs_List ] ] := Counts @ vs
infraVertexMultiset[ InfraSegment[ dag_Graph ] ]   := GeodesicOccupation[ dag ]
With[ { heads = $infraBundleHeads },
  infraVertexMultiset[ obj : ( head : heads )[ _List ] ] :=
    Merge[ atomVertexMasses[ infraRepType @ head ] /@ infraRepSeqs @ obj, Total ]
]


(* a Graph atom stands for its whole geodesic family (occupation by the Brandes DP), anything else for itself; familySize is that slot's contribution to the normalisation divisor *)

atomVertexMasses[ _ ][ dag_Graph ] := GeodesicOccupation[ dag ]
atomVertexMasses[ type_ ][ rep_ ]  := Counts[ infraRepVerts[ type, rep ] ]

atomEdgeMasses[ _ ][ _ ][ dag_Graph ] := KeyMap[ Sort[ List @@ # ] &, GeodesicEdgeOccupation[ dag ] ]
atomEdgeMasses[ g_ ][ type_ ][ rep_ ] := Counts[ infraRepEdges[ g, type, rep ] ]

atomFamilySize[ dag_Graph ] :=
  With[ { occ = GeodesicOccupation[ dag ] }, If[ Length @ occ === 0, 1, Max @ Values @ occ ] ]
atomFamilySize[ _ ] := 1


(* the raw count of appearances across realisations, keyed by sorted vertex pair; the graph is needed only for Sets-type realisations, whose edges are the induced subgraph's *)

infraEdgeMultiset[ _, InfraSegment[ dag_Graph ] ] :=
  KeyMap[ Sort[ List @@ # ] &, GeodesicEdgeOccupation[ dag ] ]

(* a circle-pool atom is an open DAG whose paths close: the wrap-around edge sink - source lies on every realisation of the atom and so carries its whole family *)
infraEdgeMultiset[ _, InfraCircle[ dags : { __Graph } ] ] :=
  Merge[
    Map[
      dag |-> With[
        { src = First @ Select[ VertexList @ dag, VertexInDegree[ dag, # ] == 0 & ],
          snk = First @ Select[ VertexList @ dag, VertexOutDegree[ dag, # ] == 0 & ] },
        Join[ KeyMap[ Sort[ List @@ # ] &, GeodesicEdgeOccupation[ dag ] ],
          <| Sort @ { snk, src } -> atomFamilySize[ dag ] |> ] ],
      dags ],
    Total ]
infraEdgeMultiset[ g_, obj_ ] :=
  Merge[ atomEdgeMasses[ g ][ infraRepType @ Head @ obj ] /@ infraRepSeqs @ obj, Total ]


(* ===================== Visit measure ===================== *)

(* c(v) = total appearances across realisations, normalised either as "Occupation" m(v) = c(v) / N -- the mean occupation per realisation, the opacity InfraSceneHighlight draws -- or as "Probability" p(v) = c(v) / Total[c].  A lossy view of the bundle: order and co-occurrence are discarded *)

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

(* the single source of truth shared with InfraSceneHighlight's repVerts / repEdges dispatch *)

infraRepType[ InfraPoint ]         = "Points";
infraRepType[ InfraEffectivePoint ]     = "Points";
infraRepType[ InfraSegment ]       = "Paths";
infraRepType[ InfraLine ]          = "Paths";
infraRepType[ InfraWalk ]          = "Paths";
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


infraRepSeqs[ ( InfraPolyline | InfraPolygon | InfraTriangle )[ reps_List ] ] := polylineToVertexSeqs @ reps
infraRepSeqs[ ( InfraObject | InfraSet )[ vs_List ] ]                        := { vs }
infraRepSeqs[ InfraPoint[ v_ ] ]                                             := { v }
infraRepSeqs[ InfraEffectivePoint[ m_Association ] ]                              := Keys @ m
infraRepSeqs[ head_[ reps_List, ___ ] ]                                      := reps

(* a point realisation is a bare vertex (wrapped to a singleton), path / cycle / set realisations are vertex lists; edges are sorted lists {a, b}, which InfraMeasure remaps to UndirectedEdge *)

infraRepVerts[ "Points", rep_ ] := { rep }
infraRepVerts[ _, rep_ ]        := rep

(* N = the number of realisations the marginal was summed over: a measured InfraPoint's total mass, a bundle's sum of per-slot family sizes (1 for an explicit realisation, the whole geodesic count for a compact DAG atom), 1 for a single set *)

infraNumReps[ InfraPoint[ _ ] ]                     := 1
(* a measure normalises by its HEAVIEST mass, not its total: the channel encodes RELATIVE mass within the object, so the modal vertex draws full and lighter ones fade.  Normalising by the total would render a uniform ball of n vertices at 1/n and make it vanish, and it is what separates ["Measure"] from ["ProbabilityMeasure"] *)
infraNumReps[ InfraEffectivePoint[ m_Association ] ]      := If[ Length @ m === 0, 1, Max @ m ]
infraNumReps[ ( InfraObject | InfraSet )[ _List ] ] := 1
infraNumReps[ InfraSegment[ dag_Graph ] ]           := atomFamilySize[ dag ]
infraNumReps[ head_[ reps_List, ___ ] ]             := Max[ Total[ atomFamilySize /@ reps ], 1 ]

(* Sets-type edges are the induced subgraph's, hence the graph dependency *)

infraRepEdges[ _, "Points", _ ]   := { }
infraRepEdges[ _, "PointSet", _ ] := { }
infraRepEdges[ _, "Paths", rep_ ] :=
  If[ Length @ rep >= 2, Sort /@ Partition[ rep, 2, 1 ], { } ]
infraRepEdges[ _, "Cycles", rep_ ] :=
  With[ { closed = If[ Length @ rep >= 2 && First @ rep === Last @ rep, rep, Append[ rep, First @ rep ] ] },
    If[ Length @ closed >= 2, Sort /@ Partition[ closed, 2, 1 ], { } ] ]
infraRepEdges[ g_, "Sets", rep_ ] :=
  Sort /@ ( List @@@ EdgeList @ Subgraph[ g, rep ] )


(* bare list = vertex sequence; line wrappers unwrap to the union of their realisations *)

(* a realisation slot may be a compact geodesic-DAG atom standing for its family *)
linePointSet[ ( InfraLine | InfraSegment | InfraWalk | InfraRay )[ reps_List ] ] :=
  Union @@ Replace[ reps, d_Graph :> VertexList[ d ], { 1 } ]
linePointSet[ InfraSegment[ dag_Graph ] ] := VertexList[ dag ]
linePointSet[ line_List ] := line
