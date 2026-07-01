Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findSegmentCore]
PackageScope[geodesicDAGBaseFn]


(* ===================== InfraSegment wrapper ===================== *)

InfraSegment[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraSegment[ _List ] ] ] :=
  InfraSegment[ Flatten[ reps /. InfraSegment[ xs_List ] :> xs, 1 ] ]

(* "Length" = list of edge counts, one per realisation: |path| - 1. *)
InfraSegment[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraSegment[ reps_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraSegment[ reps ] ]
InfraSegment[ reps_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraSegment[ reps ] ]
InfraSegment[ reps_List ][ "Measure" ] := InfraMeasure[ InfraSegment[ reps ] ]
InfraSegment[ reps_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraSegment[ reps ], Method -> "Probability" ]
(* seg[[i]] = weighted InfraPoint of the i-th position across realisations
   (mass = multiplicity).  First/Last and multi-index Part bypass this. *)
InfraSegment /: Part[ InfraSegment[ reps_List ], i_Integer ] := columnInfraPoint[ reps, i ]


(* ===== geodesic-DAG form: InfraSegment[dag_Graph] ===== *)

(* The whole geodesic family between two points, stored compactly as the
   geodesic interval DAG (GeodesicIntervalGraph).  Invariants are read straight
   off the DAG -- occupation by the Brandes count DP, never enumerating the
   (possibly astronomically many) geodesics.  ["Realizations"] is the bridge
   back to the explicit InfraSegment[{paths}] form.  Occupation accessors share
   the InfraMeasure association shape, so the renderer composes unchanged. *)

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
InfraSegment[ dag_Graph ][ "Realizations" ]               := InfraSegment[ dagGeodesics[ dag ] ]
InfraSegment[ dag_Graph ][ "Realizations", spec_ ]        := InfraSegment[ infraCap[ dagGeodesics[ dag ], spec ] ]
InfraSegment[ dag_Graph ][ "Paths" ]                      := dagGeodesics[ dag ]
InfraSegment /: Part[ InfraSegment[ dag_Graph ], i_Integer ] := columnInfraPoint[ dagGeodesics[ dag ], i ]


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

(* No extra conditions (empty Properties, Exhaustive, the whole family over plain
   vertex endpoints) -> the compact geodesic-DAG form.  An explicit finite count,
   any Properties filter, Greedy, or set/InfraPoint endpoints -> the enumerated
   path form (the calling triple over spreadFind, as before). *)

FindInfraSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : Automatic, opts : OptionsPattern[] ] :=
  If[ ( count === Automatic || count === All ) &&
      OptionValue[ FindInfraSegment, { opts }, Properties ] === { } &&
      methodName[ OptionValue[ FindInfraSegment, { opts }, Method ] /. Automatic -> "Exhaustive" ] === "Exhaustive" &&
      VertexQ[ graph, p1 ] && VertexQ[ graph, p2 ],
    InfraSegment[ GeodesicIntervalGraph[ graph, p1, p2 ] ],
    With[ { n = count /. Automatic -> 1 },
      spreadFind[ InfraSegment, n, findSegmentCore[ graph, ##, n, opts ] &, p1, p2 ] ]
  ]


findSegmentCore[ _Graph, p1_, p1_, ___ ] := { }

findSegmentCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindInfraSegment ] ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraSegment, { opts }, Properties ],
      methodSpec = OptionValue[ FindInfraSegment, { opts }, Method ] /. Automatic -> "Exhaustive" },
    If[ AnyTrue[ properties, MatchQ[ "ShortestPath" | { "ShortestPath", ___ } ] ],
      Message[ FindInfraSegment::badproperty, "ShortestPath" ]; Throw[ $Failed ] ];
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
                properties, FindInfraSegment ],
              pruning, countLimit @ count ]
          ],
        "Greedy",
          If[ fastPathQ,
            With[ { path = FindShortestPath[ graph, p1, p2 ] },
              If[ path === { }, { }, { path } ] ],
            greedyFrontierSweep[ graph, p1, p2,
              makeCandidateFn[ graph, geodesicDAGBaseFn[ graph, p1, p2 ],
                properties, FindInfraSegment ] ]
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
      segReps @ FindInfraSegment[ graph, p1, p2, All,
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
