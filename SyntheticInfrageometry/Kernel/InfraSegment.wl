Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findSegmentCore]
PackageScope[geodesicDAGBaseFn]


(* ===================== InfraSegment wrapper ===================== *)

(* set canonicalisation, ["Realizations"] / ["First"] and the occupation-measure
   accessors come from defineInfraBundleRules (Tools.wl).  The bundle carries no
   masses; a measure appears only where a projection creates an InfraPoint. *)

(* "Length" = list of edge counts, one per realisation: |path| - 1.  The walk-list
   patterns below exclude a DAG-atom list, which delegates to its expansion instead. *)
InfraSegment[ reps : Except[ { __Graph }, _List ] ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* source / sink InfraPoints: the distinct first / last vertices across
   realisations.  Deduplicated, not a measure -- every geodesic of one family
   shares its endpoints, so the multiplicity would only restate the family size.
   For the position-i occupation measure use seg[[i]] (and InfraPath, whose walks
   really can end anywhere, keeps its endpoint multiplicity). *)
InfraSegment[ reps : Except[ { __Graph }, _List ] ][ "Start" ] := InfraSet[ DeleteDuplicates[ First /@ reps ] ]
InfraSegment[ reps : Except[ { __Graph }, _List ] ][ "End" ]   := InfraSet[ DeleteDuplicates[ Last /@ reps ] ]

(* seg[[i]] = the InfraMesoPoint of the i-th position across realisations
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
InfraSegment[ dag_Graph ][ "Realizations" ]               := dagGeodesics[ dag ]
(* lazy: the bounded DFS stops at spec geodesics, never enumerating the family *)
InfraSegment[ dag_Graph ][ "Realizations", spec_ ]        := infraCap[ dagGeodesics[ dag, spec ], spec ]
InfraSegment[ dag_Graph ][ "Paths" ]                      := dagGeodesics[ dag ]
(* seg[[i]] read off the DAG: column i = layer i - 1 (the DAG is layer-aligned),
   mass = geodesic occupation -- exact, no enumeration.  ["Start"] / ["End"] are
   the first / last column, so a single-pair family gives a delta of mass = the
   family size. *)
InfraSegment /: Part[ InfraSegment[ dag_Graph ], i_Integer ] :=
  With[ { layers = dagLayers[ dag ] },
    { len = Max[ 0, Values @ layers ] },
    { vs = Keys @ Select[ layers, # === If[ i > 0, i - 1, len + 1 + i ] & ] },
    InfraMesoPoint @ KeyTake[ GeodesicOccupation[ dag ], vs ] ]

InfraSegment[ dag_Graph ][ "Start" ] := InfraSet[ Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] ]
InfraSegment[ dag_Graph ][ "End" ]   := InfraSet[ Select[ VertexList[ dag ], VertexOutDegree[ dag, # ] == 0 & ] ]


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

(* No extra conditions (empty Properties, Exhaustive, no explicit count) ->
   the compact geodesic-DAG form: one GeodesicIntervalGraph atom per endpoint
   pair, the atoms held as a set (a lone atom collapses to the bare
   InfraSegment[dag]).  An explicit count, any Properties filter, or Greedy ->
   the enumerated path form (the calling triple over spreadFind).  A multi-source
   / multi-sink union of geodesic intervals is not acyclic in general (opposite
   orientations from different sources), so multi-endpoint families stay a set
   of per-pair atoms rather than one DAG. *)

(* a lone atom collapses to the bare DAG form *)
InfraSegment[ { dag_Graph } ] := InfraSegment[ dag ]

(* a multi-pair family stays a set of DAG atoms; accessors read it as the union of
   their geodesics, so the atoms stay lazy until something actually asks for walks *)
InfraSegment[ dags : { _Graph, __Graph } ][ args___ ] :=
  InfraSegment[ Join @@ ( dagGeodesics /@ dags ) ][ args ]

FindInfraSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : Automatic, opts : OptionsPattern[] ] :=
  If[ count === Automatic &&
      OptionValue[ FindInfraSegment, { opts }, Properties ] === { } &&
      methodName[ OptionValue[ FindInfraSegment, { opts }, Method ] /. Automatic -> "Exhaustive" ] === "Exhaustive",
    InfraSegment[ DeleteDuplicates[ GeodesicIntervalGraph[ graph, #[[ 1 ]], #[[ 2 ]] ] & /@
      Select[ Tuples[ infraSpread /@ { p1, p2 } ],
        #[[ 1 ]] =!= #[[ 2 ]] && VertexQ[ graph, #[[ 1 ]] ] && VertexQ[ graph, #[[ 2 ]] ] & ] ] ],
    spreadFind[ InfraSegment, count /. Automatic -> All,
      findSegmentCore[ graph, ##, count /. Automatic -> All, opts ] &, p1, p2 ]
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
    { g, path } |-> Lookup[ dagNbrs, Key @ Last @ path, { } ]
  ]


(* ===================== ExtendInfraSegment (Tarski A4) ===================== *)

(* Tarski axiom A4: find x with B(a, b, x) and d(b, x) == d(c, d).  The only
   surviving signature -- the 2-arg form (extend segment to maximal line) is
   subsumed by FindInfraLine[g, seg] and ExtendInfraPath[g, seg, All, ...]. *)

ExtendInfraSegment[ graph_Graph, a_, b_, c_, d_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    { vs = If[ target === Infinity, { },
        Select[ VertexList[ graph ],
          x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target ] ] },
    bundleTake[ InfraPoint, vs, count ]
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
