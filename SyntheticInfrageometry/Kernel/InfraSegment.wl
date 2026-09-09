Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findSegmentCore]
PackageScope[extensionPool]
PackageScope[seedBundles]


(* ===================== FindInfraSegment ===================== *)

(* a geodesic (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2), returned as a directed path graph on the substrate vertices.  The count-less call is one geodesic, a bounded count a List of them, and All the geodesic interval DAG: the bundle IS the union of its walks, so it is not a separate return type.  Anchors spreading to several endpoint pairs give one DAG per pair -- a multi-source / multi-sink union of intervals is not acyclic in general.  No Properties axis: a rule narrowing the geodesic bundle is a local law at an infra-scale, hence a FindInfraGeodesic call *)

FindInfraSegment::badproperty = "Property `1` is not supported by FindInfraSegment; local rules on the geodesic bundle moved to FindInfraGeodesic[graph, p1, p2, scale].";
FindInfraSegment::badmethod   = "Method `1` is not supported by FindInfraSegment.";

Options[ FindInfraSegment ] = {
  Method -> Automatic
};

(* count = All with the exhaustive method gives the DAG form, one GeodesicIntervalGraph atom per endpoint pair; any bounded count gives the enumerated paths, lazily via the DAG's bounded DFS.  The endpoints are point-shaped anchors, so each is read through the anchor rule: a vertex, a vertex list, a density or a walk all spread over their support *)

FindInfraSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  If[ ! FreeQ[ { opts }, Properties ],
    Message[ FindInfraSegment::badproperty, Properties /. { opts } ]; $Failed,
    If[ count === All &&
        methodName[ resolveMethod[ OptionValue[ FindInfraSegment, { opts }, Method ], count ] ] === "Exhaustive",
      loneBundle @ DeleteDuplicates[ GeodesicIntervalGraph[ graph, #[[ 1 ]], #[[ 2 ]] ] & /@
        Select[ Tuples[ Keys @ toDensity[ graph, # ] & /@ { p1, p2 } ],
          #[[ 1 ]] =!= #[[ 2 ]] && VertexQ[ graph, #[[ 1 ]] ] && VertexQ[ graph, #[[ 2 ]] ] & ] ],
      geodesicFind[ count, findSegmentCore[ graph, ##, count, opts ] &,
        toDensity[ graph, p1 ], toDensity[ graph, p2 ] ]
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

(* the geodesics containing a geodesic bundle from p1 to p2, extended past its ends by at most kspec edges per free side and inextensible within that budget: kspec Infinity gives the lines through the bundle (FindInfraLine), kspec 0 the bundle itself.  The seed is a walk, a geodesic DAG extended as one object, or anything spreading to walks; the 6-ary form is Tarski A4 *)

ExtendInfraSegment::badproperty  = "Property `1` is not supported by ExtendInfraSegment; local rules on the extension moved to ExtendInfraGeodesic[graph, seed, scale, kspec].";
ExtendInfraSegment::badmethod    = "Method `1` is not supported by ExtendInfraSegment.";
ExtendInfraSegment::baddirection = "Direction `1` is not supported by ExtendInfraSegment.";

Options[ ExtendInfraSegment ] = {
  Properties  -> { },
  Method      -> Automatic,
  "Direction" -> "BothSides"
};

ExtendInfraSegment[ graph_Graph, seed_,
    kspec : ( _Integer | UpTo[ _Integer ] | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  With[ { pools = extensionPool[ ExtendInfraSegment, graph, #, kspec, count, opts ] & /@ seedBundles @ seed },
    If[ MemberQ[ pools, $Failed ], $Failed, geodesicTake[ Catenate @ pools, count ] ] ]


(* the bundles a seed stands for, one DAG each: a substrate DAG or path graph is one bundle, a list of them several, and anything else -- a vertex list, a density, a position-spelled walk -- spreads to its walks *)

seedBundles[ dag_Graph ] /; ! positionSpelledQ[ dag ]                 := { dag }
seedBundles[ dags : { __Graph } ] /; NoneTrue[ dags, positionSpelledQ ] := dags
seedBundles[ other_ ]                                                  := geodesicGraph /@ infraSpread @ other


(* Tarski A4: find x with B(a, b, x) and d(b, x) == d(c, d); the last vertex slot excludes rules so an optioned 3-argument call never lands here *)

ExtendInfraSegment[ graph_Graph, a_, b_, c_, d : Except[ _Rule | _RuleDelayed ],
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    { vs = If[ target === Infinity, { },
        Select[ VertexList[ graph ],
          x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target ] ] },
    countTake[ vs, count ]
  ]


(* ===================== Extension pool ===================== *)

(* the pool of geodesics containing a bundle from p1 to p2 (a DAG with source p1 and sink p2) and extended by at most kmax edges per free side, read off the distance matrix.  The two extension graphs hold the candidate ends, layered by the distance from p1 resp. p2; a pair (s, e) is admissible iff jointly geodesic -- d(s, e) == d(s, p1) + d(p1, p2) + d(p2, e), whichever geodesics are used -- with the larger layer passing kspec (k or UpTo[k]: at most k, {k}: exactly k, {lo, hi}: in range) and each free side either at the budget or inextensible, and its atom is I(p1, s) reversed, the bundle, and I(p2, e), cut out of the extension graphs.  "Exhaustive" with All is the pool itself; every bounded count streams geodesics off the admissible pairs in candidate ("Greedy", "Exhaustive") or random ("RandomGreedy") order, so the class is the same under every Method.  head is the calling symbol, read for its options and messages *)

extensionPool[ _, _Graph, bundle_Graph, _, _, OptionsPattern[] ] /; VertexCount[ bundle ] == 0 := { }

extensionPool[ head_, graph_Graph, bundle_Graph, kspec_, count_, opts : OptionsPattern[] ] :=
  Catch @ With[ {
      properties = OptionValue[ head, { opts }, Properties ],
      methodHead = methodName @ resolveMethod[ OptionValue[ head, { opts }, Method ], count ],
      direction  = OptionValue[ head, { opts }, "Direction" ],
      kmax   = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k, UpTo[ k_ ] :> k } ],
      stepsQ = Replace[ kspec, { Infinity :> ( True & ), { k_ } :> ( # == k & ),
                                 { lo_, hi_ } :> ( lo <= # <= hi & ), UpTo[ k_ ] :> ( # <= k & ),
                                 k_Integer :> ( # <= k & ) } ],
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

(* InfraSegment survives only here, as the scene-language token; the scene engine binds the vertex sequences *)

dispatchConstruction[ graph_Graph, InfraSegment[ p1_, p2_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      infraSpread @ FindInfraSegment[ graph, p1, p2, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraSegment ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]


(* ===================== InfraWalkQ ===================== *)

(* consecutive vertices adjacent, revisits allowed: InfraWalkQ superset InfraSegmentQ superset InfraLineQ *)

InfraWalkQ[ graph_Graph, ws : { __Graph } ] := AllTrue[ ws, InfraWalkQ[ graph, # ] & ]

InfraWalkQ[ graph_Graph, w_Graph ] := AllTrue[ walkRealisations @ w, InfraWalkQ[ graph, # ] & ]

InfraWalkQ[ graph_Graph, path_List ] /; Length[ path ] >= 2 :=
  AllTrue[ Partition[ path, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

InfraWalkQ[ _Graph, path_List ] /; Length[ path ] < 2 := False


(* ===================== InfraSegmentQ ===================== *)

(* consecutive vertices adjacent and the total edge count equal to d(v0, vk); a graph -- one path or a DAG -- passes iff every walk it stands for does *)

InfraSegmentQ[ graph_Graph, ws : { __Graph } ] := AllTrue[ ws, InfraSegmentQ[ graph, # ] & ]

InfraSegmentQ[ graph_Graph, w_Graph ] := AllTrue[ walkRealisations @ w, InfraSegmentQ[ graph, # ] & ]

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
