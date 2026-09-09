Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== FindInfraRay ===================== *)

(* a ray from o through v: a geodesic o ... v ... e with d(o, e) == d(o, v) + d(v, e) and no neighbour of e one step farther from o -- the InfraRayQ class under every Method; the longest ones are SelectInfraWalk[graph, rays, All, "From" -> "MaxLength"].  The count-less call is one ray as a substrate path graph, a bounded count a List of them, All the pool: one DAG with source o, the o -> v geodesic bundle glued at v to the extension graph beyond v, whose o -> sink paths are exactly the rays -- a sink has no neighbour one step farther from o, which is InfraRayQ's far-end test.  "Exhaustive" with All is the pool itself, and every bounded count streams rays off it in candidate ("Greedy", "Exhaustive") or random ("RandomGreedy") order *)

FindInfraRay::badmethod = "Method `1` is not supported by FindInfraRay.";

Options[ FindInfraRay ] = { Method -> Automatic };

FindInfraRay[ graph_Graph, origin_, v_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  geodesicFind[ count,
    { o, w } |-> Catch @ With[ {
        methodHead = methodName @ resolveMethod[ OptionValue[ FindInfraRay, { opts }, Method ], count ] },
      If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
        Message[ FindInfraRay::badmethod, methodHead ]; Throw[ $Failed ] ];
      With[ { pool = Graph @ Sort @ Join[ EdgeList @ GeodesicIntervalGraph[ graph, o, w ],
                                          EdgeList @ GeodesicExtensionGraph[ graph, { o, w } ] ] },
        Which[
          EdgeCount @ pool == 0,                        { },
          methodHead === "Exhaustive" && count === All, { pool },
          True, dagGeodesics[ pool, count, greedyBranch[ methodHead /. "Exhaustive" -> "Greedy" ] ] ] ] ],
    toDensity[ graph, origin ], toDensity[ graph, v ] ]


(* ===================== InfraRayQ ===================== *)

(* a geodesic inextensible at its far end only: the origin is an endpoint by fiat, which is what distinguishes a ray from a line *)

InfraRayQ[ graph_Graph, ray_List ] /; Length[ ray ] >= 2 :=
  InfraSegmentQ[ graph, ray ] &&
  NoneTrue[ AdjacencyList[ graph, Last @ ray ],
    GraphDistance[ graph, First @ ray, # ] == Length[ ray ] & ]

InfraRayQ[ _Graph, ray_List ] /; Length[ ray ] < 2 := False

InfraRayQ[ graph_Graph, w_Graph ] := AllTrue[ walkRealisations @ w, InfraRayQ[ graph, # ] & ]

InfraRayQ[ graph_Graph, ws : { __Graph } ] := AllTrue[ ws, InfraRayQ[ graph, # ] & ]


(* ===================== PencilDirections / PencilCardinality ===================== *)

(* the pencil at O is the set of rays from O; a ray leaves O through exactly one neighbour, so the ray pools over the neighbours partition it, and the cardinality is their path count, read off the DP without enumeration *)

PencilDirections[ graph_Graph, origin_ ] :=
  Catenate[ infraSpread @ FindInfraRay[ graph, origin, #, All ] & /@ AdjacencyList[ graph, origin ] ]

PencilCardinality[ graph_Graph, origin_ ] :=
  Total[ Replace[ FindInfraRay[ graph, origin, #, All ],
      { { } -> 0, dag_Graph :> infraNumReps @ dag, dags_List :> Total[ infraNumReps /@ dags ] } ] & /@
    AdjacencyList[ graph, origin ] ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraRay[ origin_, v_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      infraSpread @ FindInfraRay[ graph, origin, v, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraRay ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { origin, v } |> ],
    extractBranches[ { opts } ] ]
