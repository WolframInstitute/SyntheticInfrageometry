Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findSegmentCore]
PackageScope[extensionPool]


(* ===================== InfraSegment wrapper ===================== *)


InfraSegment[ reps : Except[ { __Graph }, _List ] ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* the distinct first / last vertices across realisations, deduplicated rather than a measure: every geodesic of one family shares its endpoints *)
InfraSegment[ reps : Except[ { __Graph }, _List ] ][ "Start" ] := InfraSet[ DeleteDuplicates[ First /@ reps ] ]
InfraSegment[ reps : Except[ { __Graph }, _List ] ][ "End" ]   := InfraSet[ DeleteDuplicates[ Last /@ reps ] ]

InfraSegment /: Part[ InfraSegment[ reps : Except[ { __Graph }, _List ] ], i_Integer ] := columnInfraPoint[ reps, i ]


(* ===== geodesic-DAG form: InfraSegment[dag_Graph] ===== *)

(* the whole geodesic family stored as the geodesic interval DAG: occupation comes from the Brandes count DP, never from enumerating the (possibly astronomically many) geodesics *)

InfraSegment[ dag_Graph ][ "Graph" ]              := dag
InfraSegment[ dag_Graph ][ "Vertices" ]           := VertexList[ dag ]
InfraSegment[ dag_Graph ][ "Length" ]             :=
  If[ VertexCount[ dag ] == 0, 0,
    Max @ GraphDistance[ dag, First @ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] ] ]
InfraSegment[ dag_Graph ][ "Multiplicity" ]       := infraNumReps[ InfraSegment[ dag ] ]
InfraSegment[ dag_Graph ][ "OccupationCount" ]    := GeodesicOccupation[ dag ]
InfraSegment[ dag_Graph ][ "OccupationMeasure" ]  := InfraMeasure[ InfraSegment[ dag ] ]
InfraSegment[ dag_Graph ][ "Measure" ]            := InfraMeasure[ InfraSegment[ dag ] ]
InfraSegment[ dag_Graph ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraSegment[ dag ], Method -> "Probability" ]
InfraSegment[ dag_Graph ][ "Realizations" ]               := dagGeodesics[ dag ]
(* lazy: the bounded DFS stops at spec geodesics, never enumerating the family *)
InfraSegment[ dag_Graph ][ "Realizations", spec_ ]        := infraCap[ dagGeodesics[ dag, spec ], spec ]
InfraSegment[ dag_Graph ][ "Paths" ]                      := dagGeodesics[ dag ]
(* column i = layer i - 1 (the DAG is layer-aligned), mass = geodesic occupation: exact, no enumeration *)
InfraSegment /: Part[ InfraSegment[ dag_Graph ], i_Integer ] :=
  With[ { layers = dagLayers[ dag ] },
    { len = Max[ 0, Values @ layers ] },
    { vs = Keys @ Select[ layers, # === If[ i > 0, i - 1, len + 1 + i ] & ] },
    InfraEffectivePoint @ KeyTake[ GeodesicOccupation[ dag ], vs ] ]

InfraSegment[ dag_Graph ][ "Start" ] := InfraSet[ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] ]
InfraSegment[ dag_Graph ][ "End" ]   := InfraSet[ Select[ VertexList[ dag ], VertexOutDegree[ dag, # ] == 0 & ] ]


(* ===================== FindInfraSegment ===================== *)

(* a geodesic sequence (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2).  No Properties axis: a rule narrowing the geodesic bundle is a local law at an infra-scale, hence a FindInfraGeodesic call *)

FindInfraSegment::badproperty = "Property `1` is not supported by FindInfraSegment; local rules on the geodesic bundle moved to FindInfraGeodesic[graph, p1, p2, scale].";
FindInfraSegment::badmethod   = "Method `1` is not supported by FindInfraSegment.";

Options[ FindInfraSegment ] = {
  Method -> Automatic
};

(* count = All with the exhaustive method gives the compact geodesic-DAG form, one GeodesicIntervalGraph atom per endpoint pair; any bounded count gives the enumerated paths, lazily via the DAG's bounded DFS.
   A multi-source / multi-sink union of geodesic intervals is not acyclic in general, so multi-endpoint families stay a set of per-pair atoms rather than one DAG *)

(* a lone atom collapses to the bare DAG form *)
InfraSegment[ { dag_Graph } ] := InfraSegment[ dag ]

(* ===== pool form: InfraSegment[{dag_Graph, ...}] ===== *)

(* one geodesic DAG per endpoint pair -- FindInfraSegment spread over wrapper anchors, or ExtendInfraSegment's admissible end pairs.  Count and occupation come from the per-atom DP, as for the InfraLine pool, and ["Length"] is one number per atom: a 20 x 20 grid edge has 9 x 10^9 lines through it, so nothing here is sized by the family.  Anything else enumerates *)

InfraSegment[ dags : { _Graph, __Graph } ][ "Graph" ]              := dags
InfraSegment[ dags : { _Graph, __Graph } ][ "Vertices" ]           := Union @@ ( VertexList /@ dags )
InfraSegment[ dags : { _Graph, __Graph } ][ "Length" ]             := ( Max @ Values @ dagLayers @ # & ) /@ dags
InfraSegment[ dags : { _Graph, __Graph } ][ "Multiplicity" ]       := infraNumReps @ InfraSegment @ dags
InfraSegment[ dags : { _Graph, __Graph } ][ "OccupationCount" ]    := infraVertexMultiset @ InfraSegment @ dags
InfraSegment[ dags : { _Graph, __Graph } ][ "OccupationMeasure" ]  := InfraMeasure @ InfraSegment @ dags
InfraSegment[ dags : { _Graph, __Graph } ][ "Measure" ]            := InfraMeasure @ InfraSegment @ dags
InfraSegment[ dags : { _Graph, __Graph } ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraSegment @ dags, Method -> "Probability" ]
InfraSegment[ dags : { _Graph, __Graph } ][ "Realizations" ]       := Catenate[ dagGeodesics /@ dags ]
InfraSegment[ dags : { _Graph, __Graph } ][ "Paths" ]              := Catenate[ dagGeodesics /@ dags ]
InfraSegment[ dags : { _Graph, __Graph } ][ "First" ]              := First @ dagGeodesics[ First @ dags, 1 ]
InfraSegment[ dags : { _Graph, __Graph } ][ "Start" ] :=
  InfraSet[ Union @@ Map[ dag |-> Select[ VertexList @ dag, VertexInDegree[ dag, # ] == 0 & ], dags ] ]
InfraSegment[ dags : { _Graph, __Graph } ][ "End" ]   :=
  InfraSet[ Union @@ Map[ dag |-> Select[ VertexList @ dag, VertexOutDegree[ dag, # ] == 0 & ], dags ] ]

(* lazy: atoms are consumed in order, each stopping at the residual budget *)
InfraSegment[ dags : { _Graph, __Graph } ][ "Realizations", spec_ ] :=
  infraCap[
    Fold[ { acc, dag } |-> If[ Length @ acc >= countLimit @ spec, acc,
        Join[ acc, dagGeodesics[ dag, countLimit @ spec - Length @ acc ] ] ],
      { }, dags ],
    spec ]

InfraSegment[ dags : { _Graph, __Graph } ][ args___ ] :=
  InfraSegment[ Join @@ ( dagGeodesics /@ dags ) ][ args ]

(* column i = layer i - 1 of each atom, mass = geodesic occupation: exact, no enumeration *)
InfraSegment /: Part[ InfraSegment[ dags : { _Graph, __Graph } ], i_Integer ] :=
  InfraEffectivePoint @ Merge[
    Map[ dag |-> With[ { layers = dagLayers[ dag ] },
        { len = Max[ 0, Values @ layers ] },
        KeyTake[ GeodesicOccupation[ dag ], Keys @ Select[ layers, # === If[ i > 0, i - 1, len + 1 + i ] & ] ] ],
      dags ],
    Total ]

FindInfraSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  If[ ! FreeQ[ { opts }, Properties ],
    Message[ FindInfraSegment::badproperty, Properties /. { opts } ]; $Failed,
    If[ count === All &&
        methodName[ resolveMethod[ OptionValue[ FindInfraSegment, { opts }, Method ], count ] ] === "Exhaustive",
      InfraSegment[ DeleteDuplicates[ GeodesicIntervalGraph[ graph, #[[ 1 ]], #[[ 2 ]] ] & /@
        Select[ Tuples[ infraSpread /@ { p1, p2 } ],
          #[[ 1 ]] =!= #[[ 2 ]] && VertexQ[ graph, #[[ 1 ]] ] && VertexQ[ graph, #[[ 2 ]] ] & ] ] ],
      spreadFind[ InfraSegment, count, findSegmentCore[ graph, ##, count, opts ] &, p1, p2 ]
    ]
  ]


findSegmentCore[ _Graph, p1_, p1_, ___ ] := { }

findSegmentCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic,
    opts : OptionsPattern[ FindInfraSegment ] ] :=
  With[ { methodSpec = resolveMethod[ OptionValue[ FindInfraSegment, { opts }, Method ], count ] },
    Switch[ methodName @ methodSpec,
      "Exhaustive",
        If[ countLimit @ count === 1,
          With[ { path = FindShortestPath[ graph, p1, p2 ] },
            If[ path === { }, { }, { path } ] ],
          With[ { d = GraphDistance[ graph, p1, p2 ] },
            If[ d === Infinity, { },
              FindPath[ graph, p1, p2, { d }, count /. UpTo[ k_ ] :> k ] ] ]
        ],
      (* the pool structure already IS the lazy descent: the DAG's bounded DFS
         stops at `count` geodesics, so the greedy branch is complete and exact *)
      "Greedy",
        dagGeodesics[ GeodesicIntervalGraph[ graph, p1, p2 ], count ],
      "RandomGreedy",
        With[ { dag = GeodesicIntervalGraph[ graph, p1, p2 ] },
          greedyFrontierSweep[ dag, p1, p2,
            { d, w } |-> DeleteCases[ VertexOutComponent[ d, { Last @ w }, 1 ], Last @ w ],
            True &, Infinity, count, RandomSample ] ],
      _,
        Message[ FindInfraSegment::badmethod, methodSpec ]; $Failed
    ]
  ]


(* ===================== ExtendInfraSegment ===================== *)

(* the geodesics containing a geodesic bundle from p1 to p2, extended past its ends by at most kspec edges per free side and inextensible within that budget: kspec Infinity gives the lines through the bundle (FindInfraLine), kspec 0 the bundle itself.  The seed is a walk, an InfraSegment DAG or any wrapper spreading to walks; the 6-ary form is Tarski A4 *)

ExtendInfraSegment::badproperty  = "Property `1` is not supported by ExtendInfraSegment; local rules on the extension moved to ExtendInfraGeodesic[graph, seed, scale, kspec].";
ExtendInfraSegment::badmethod    = "Method `1` is not supported by ExtendInfraSegment.";
ExtendInfraSegment::baddirection = "Direction `1` is not supported by ExtendInfraSegment.";

Options[ ExtendInfraSegment ] = {
  Properties  -> { },
  Method      -> Automatic,
  "Direction" -> "BothSides"
};

ExtendInfraSegment[ graph_Graph, seed_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  With[ { pools = extensionPool[ ExtendInfraSegment, graph, #, kspec, count, opts ] & /@
      Replace[ seed, {
        InfraSegment[ dag_Graph ]          :> { dag },
        InfraSegment[ dags : { __Graph } ] :> dags,
        other_ :> ( PathGraph[ #, DirectedEdges -> True ] & /@ infraSpread @ other ) } ] },
    If[ MemberQ[ pools, $Failed ], $Failed,
      bundleTake[ InfraSegment, DeleteDuplicates @ Catenate @ pools, count ] ] ]


(* Tarski A4: find x with B(a, b, x) and d(b, x) == d(c, d); the last vertex slot excludes rules so an optioned 3-argument call never lands here *)

ExtendInfraSegment[ graph_Graph, a_, b_, c_, d : Except[ _Rule | _RuleDelayed ],
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    { vs = If[ target === Infinity, { },
        Select[ VertexList[ graph ],
          x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target ] ] },
    bundleTake[ InfraPoint, vs, count ]
  ]


(* ===================== Extension pool ===================== *)

(* the pool of geodesics containing a bundle from p1 to p2 (a DAG with source p1 and sink p2) and extended by at most kmax edges per free side, read off the distance matrix.  The two extension graphs hold the candidate ends, layered by the distance from p1 resp. p2; a pair (s, e) is admissible iff jointly geodesic -- d(s, e) == d(s, p1) + d(p1, p2) + d(p2, e), whichever geodesics are used -- with the larger layer passing kspec (k: at most k, {k}: exactly k, {lo, hi}: in range) and each free side either at the budget or inextensible, and its atom is I(p1, s) reversed, the bundle, and I(p2, e), cut out of the extension graphs.  "Exhaustive" with All is the pool itself; every bounded count streams geodesics off the admissible pairs in candidate ("Greedy", "Exhaustive") or random ("RandomGreedy") order, so the class is the same under every Method.  head is the calling symbol, read for its options and messages *)

extensionPool[ _, _Graph, bundle_Graph, _, _, OptionsPattern[] ] /; VertexCount[ bundle ] == 0 := { }

extensionPool[ head_, graph_Graph, bundle_Graph, kspec_, count_, opts : OptionsPattern[] ] :=
  Catch @ With[ {
      properties = OptionValue[ head, { opts }, Properties ],
      methodHead = methodName @ resolveMethod[ OptionValue[ head, { opts }, Method ], count ],
      direction  = OptionValue[ head, { opts }, "Direction" ],
      kmax   = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
      stepsQ = Replace[ kspec, { Infinity :> ( True & ), { k_ } :> ( # == k & ),
                                 { lo_, hi_ } :> ( lo <= # <= hi & ), k_Integer :> ( # <= k & ) } ],
      p1 = First @ Select[ VertexList @ bundle, VertexInDegree[ bundle, # ] == 0 & ],
      p2 = First @ Select[ VertexList @ bundle, VertexOutDegree[ bundle, # ] == 0 & ],
      verts = VertexList @ graph },
    If[ properties =!= { }, Message[ MessageName[ head, "badproperty" ], properties ]; Throw[ $Failed ] ];
    If[ ! MatchQ[ direction, "Forward" | "Backward" | "BothSides" ],
      Message[ MessageName[ head, "baddirection" ], direction ]; Throw[ $Failed ] ];
    If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
      Message[ MessageName[ head, "badmethod" ], methodHead ]; Throw[ $Failed ] ];
    With[ { dm = GraphDistanceMatrix[ graph ], vidx = AssociationThread[ verts, Range @ Length @ verts ],
            leftExt  = GeodesicExtensionGraph[ graph, { p2, p1 } ],
            rightExt = GeodesicExtensionGraph[ graph, { p1, p2 } ] },
      { dist = dm[[ vidx @ #1, vidx @ #2 ]] & },
      { d = dist[ p1, p2 ],
        pairs = Tuples[ {
          If[ direction === "Forward",  { p1 }, Select[ VertexList @ leftExt,  dist[ p1, # ] <= kmax & ] ],
          If[ direction === "Backward", { p2 }, Select[ VertexList @ rightExt, dist[ p2, # ] <= kmax & ] ] } ] },
      { admissibleQ = { s, e } |-> dist[ s, e ] == dist[ s, p1 ] + d + dist[ p2, e ] &&
          stepsQ @ Max[ dist[ p1, s ], dist[ p2, e ] ] &&
          ( direction === "Forward"  || dist[ p1, s ] == kmax ||
            NoneTrue[ AdjacencyList[ graph, s ], dist[ #, e ] == dist[ s, e ] + 1 & ] ) &&
          ( direction === "Backward" || dist[ p2, e ] == kmax ||
            NoneTrue[ AdjacencyList[ graph, e ], dist[ s, # ] == dist[ s, e ] + 1 & ] ),
        atom = { s, e } |-> Graph @ Sort @ Join[
          EdgeList @ ReverseGraph @ Subgraph[ leftExt,
            Select[ VertexList @ leftExt, dist[ p1, # ] + dist[ #, s ] == dist[ p1, s ] & ] ],
          EdgeList @ bundle,
          EdgeList @ Subgraph[ rightExt,
            Select[ VertexList @ rightExt, dist[ p2, # ] + dist[ #, e ] == dist[ p2, e ] & ] ] ] },
      If[ methodHead === "Exhaustive" && count === All,
        atom @@@ Select[ pairs, admissibleQ @@ # & ],
        With[ { cap = countLimit @ count, branch = greedyBranch[ methodHead /. "Exhaustive" -> "Greedy" ] },
          Fold[ { acc, pair } |-> If[ Length @ acc >= cap || ! admissibleQ @@ pair, acc,
              Join[ acc, greedyFrontierSweep[ atom @@ pair, First @ pair, Last @ pair,
                { dag, walk } |-> DeleteCases[ VertexOutComponent[ dag, { Last @ walk }, 1 ], Last @ walk ],
                True &, Infinity, cap - Length @ acc, branch ] ] ],
            { }, branch @ pairs ] ] ]
    ]
  ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraSegment[ p1_, p2_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      segReps @ FindInfraSegment[ graph, p1, p2, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraSegment ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]


(* ===================== InfraWalkQ ===================== *)

(* consecutive vertices adjacent, revisits allowed: InfraWalkQ superset InfraSegmentQ superset InfraLineQ *)

InfraWalkQ[ graph_Graph, w : _InfraWalk | _InfraLoop | _InfraString ] :=
  AllTrue[ First @ w, InfraWalkQ[ graph, # ] & ]

InfraWalkQ[ graph_Graph, path_List ] /; Length[ path ] >= 2 :=
  AllTrue[ Partition[ path, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

InfraWalkQ[ _Graph, path_List ] /; Length[ path ] < 2 := False


(* ===================== InfraSegmentQ ===================== *)

(* consecutive vertices adjacent and the total edge count equal to d(v0, vk) *)

InfraSegmentQ[ graph_Graph, seg_InfraSegment ] :=
  AllTrue[ segReps @ seg, InfraSegmentQ[ graph, # ] & ]

InfraSegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

InfraSegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False


(* ===================== UniqueInfraSegmentQ ===================== *)

(* a geodetic graph: every vertex pair admits a unique geodesic *)

UniqueInfraSegmentQ[ graph_Graph, u_, v_ ] := GeodesicMultiplicity[ graph, u, v ] == 1

UniqueInfraSegmentQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> UniqueInfraSegmentQ[ graph, pair[[ 1 ]], pair[[ 2 ]] ] ]
