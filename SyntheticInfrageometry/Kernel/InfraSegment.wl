Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findSegmentCore]
PackageScope[geodesicDAGBaseFn]


(* ===================== InfraSegment wrapper ===================== *)

InfraSegment[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraSegment[ _List ] ] ] :=
  InfraSegment[ Flatten[ reps /. InfraSegment[ xs_List ] :> xs, 1 ] ]

(* "Length" = list of edge counts, one per realisation: |path| - 1. *)
InfraSegment[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps


(* ===================== FindInfraSegment ===================== *)

(* A segment between p1 and p2: a geodesic vertex sequence
   (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2) and consecutive vi adjacent.
   Geodesic-ness is implicit -- Properties filters narrow the geodesic bundle
   further (e.g. {"EdgeMin", f} keeps geodesics MinimalBy f at each step). *)

FindInfraSegment::badproperty = "Property `1` is not supported by FindInfraSegment.";
FindInfraSegment::badmethod   = "Method `1` is not supported by FindInfraSegment.";

Options[ FindInfraSegment ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findSegmentCore[ graph, ##, count, opts ] &, p1, p2 ]


findSegmentCore[ _Graph, p1_, p1_, ___ ] := { }

findSegmentCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindInfraSegment ] ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraSegment, { opts }, Properties ],
      methodSpec = OptionValue[ FindInfraSegment, { opts }, Method ] /. Automatic -> "Exhaustive" },
    With[ { methodHead = methodName @ methodSpec,
            pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
            fastPathQ  = properties === { } },
      Switch[ methodHead,
        "Exhaustive",
          If[ fastPathQ,
            If[ count === 1,
              With[ { path = FindShortestPath[ graph, p1, p2 ] },
                If[ path === { }, { }, { path } ] ],
              With[ { d = GraphDistance[ graph, p1, p2 ] },
                If[ d === Infinity, { },
                  FindPath[ graph, p1, p2, { d }, count /. UpTo[ k_ ] :> k ] ] ]
            ],
            frontierSweep[ graph, p1, p2,
              makeCandidateFn[ graph, geodesicDAGBaseFn[ graph, p1, p2 ],
                properties, FindInfraSegment::badproperty ],
              pruning, countLimit @ count ]
          ],
        "Greedy",
          If[ fastPathQ,
            With[ { path = FindShortestPath[ graph, p1, p2 ] },
              If[ path === { }, { }, { path } ] ],
            greedyFrontierSweep[ graph, p1, p2,
              makeCandidateFn[ graph, geodesicDAGBaseFn[ graph, p1, p2 ],
                properties, FindInfraSegment::badproperty ] ]
          ],
        _,
          Message[ FindInfraSegment::badmethod, methodSpec ]; $Failed
      ]
    ]
  ]


(* Geodesic-DAG base candidate function: at vertex `Last @ path`, return its
   forward DAG neighbours under the precomputed geodesicDAGNeighbors map. *)

geodesicDAGBaseFn[ graph_Graph, p1_, p2_ ] :=
  With[ { dagNbrs = geodesicDAGNeighbors[ graph, p1, p2 ] },
    Function[ { g, path }, Lookup[ dagNbrs, Key @ Last @ path, { } ] ]
  ]


(* ===================== ExtendInfraSegment (Tarski A4) ===================== *)

(* Tarski axiom A4: find x with B(a, b, x) and d(b, x) == d(c, d).  The only
   surviving signature -- the 2-arg form (extend segment to maximal line) is
   subsumed by FindInfraLine[g, seg] and ExtendInfraPath[g, seg, All, ...]. *)

ExtendInfraSegment[ graph_Graph, a_, b_, c_, d_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    With[ { vs = If[ target === Infinity, { },
        Select[ VertexList[ graph ],
          x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target ] ] },
      With[ { capped = infraCap[ vs, count ] },
        If[ capped === $Failed, $Failed, InfraPoint[ { # } ] & /@ capped ]
      ]
    ]
  ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraSegment[ p1_, p2_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindInfraSegment[ graph, p1, p2, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraSegment ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]


(* ===================== InfraPathQ ===================== *)

(* A vertex sequence (v0, ..., vk) is a path iff consecutive vertices are
   adjacent and no vertex repeats.  InfraPathQ \supset InfraSegmentQ \supset InfraLineQ. *)

InfraPathQ[ graph_Graph, path_List ] /; Length[ path ] >= 2 :=
  DuplicateFreeQ[ path ] &&
  AllTrue[ Partition[ path, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

InfraPathQ[ _Graph, path_List ] /; Length[ path ] < 2 := False


(* ===================== InfraSegmentQ ===================== *)

(* A vertex sequence (v0, ..., vk) is a geodesic from v0 to vk iff consecutive
   vertices are adjacent and the total edge count equals d(v0, vk). *)

InfraSegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

InfraSegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False


(* ===================== UniqueInfraSegmentQ ===================== *)

(* UniqueInfraSegmentQ[g, u, v]: GeodesicMultiplicity[g, u, v] == 1.
   UniqueInfraSegmentQ[g]: every vertex pair admits a unique geodesic (geodetic graph). *)

UniqueInfraSegmentQ[ graph_Graph, u_, v_ ] := GeodesicMultiplicity[ graph, u, v ] == 1

UniqueInfraSegmentQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> UniqueInfraSegmentQ[ graph, pair[[ 1 ]], pair[[ 2 ]] ] ]
