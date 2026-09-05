Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraRay wrapper ===================== *)


InfraRay[ reps : Except[ { __Graph }, _List ] ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps


(* ===== pool form: InfraRay[{dag_Graph, ...}] ===== *)

(* one DAG per anchor pair (o, v) with the single source o: the o -> v geodesic bundle glued at v to the extension graph beyond v, so the o -> sink paths are exactly the rays -- a sink has no neighbour one step farther from o, which is InfraRayQ's far-end test.  Rays to different sinks differ in length, so ["Length"] is one number per ray, read off the sink layers and the path counts, never by enumeration *)

InfraRay[ dags : { __Graph } ][ "Graph" ]              := dags
InfraRay[ dags : { __Graph } ][ "Vertices" ]           := Union @@ ( VertexList /@ dags )
InfraRay[ dags : { __Graph } ][ "Length" ]             :=
  Catenate @ Map[ dag |-> With[ { layers = dagLayers @ dag, occupation = GeodesicOccupation @ dag },
      Catenate @ Map[ sink |-> ConstantArray[ layers @ sink, occupation @ sink ],
        Select[ VertexList @ dag, VertexOutDegree[ dag, # ] == 0 & ] ] ],
    dags ]
InfraRay[ dags : { __Graph } ][ "Multiplicity" ]       := infraNumReps @ InfraRay @ dags
InfraRay[ dags : { __Graph } ][ "OccupationCount" ]    := infraVertexMultiset @ InfraRay @ dags
InfraRay[ dags : { __Graph } ][ "OccupationMeasure" ]  := InfraMeasure @ InfraRay @ dags
InfraRay[ dags : { __Graph } ][ "Measure" ]            := InfraMeasure @ InfraRay @ dags
InfraRay[ dags : { __Graph } ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraRay @ dags, Method -> "Probability" ]
InfraRay[ dags : { __Graph } ][ "Realizations" ]       := Catenate[ dagGeodesics /@ dags ]
InfraRay[ dags : { __Graph } ][ "First" ]              := First @ dagGeodesics[ First @ dags, 1 ]

(* lazy: atoms are consumed in order, each stopping at the residual budget *)
InfraRay[ dags : { __Graph } ][ "Realizations", spec_ ] :=
  infraCap[
    Fold[ { acc, dag } |-> If[ Length @ acc >= countLimit @ spec, acc,
        Join[ acc, dagGeodesics[ dag, countLimit @ spec - Length @ acc ] ] ],
      { }, dags ],
    spec ]


(* ===================== FindInfraRay ===================== *)

(* a ray from o through v: a geodesic o ... v ... e with d(o, e) == d(o, v) + d(v, e) and no neighbour of e one step farther from o -- the InfraRayQ class under every Method; the longest ones are SelectInfraWalk[graph, rays, All, "From" -> "MaxLength"].  The pool is one DAG with source o; "Exhaustive" with All is the pool itself, and every bounded count streams rays off it in candidate ("Greedy", "Exhaustive") or random ("RandomGreedy") order *)

FindInfraRay::badmethod = "Method `1` is not supported by FindInfraRay.";

Options[ FindInfraRay ] = { Method -> Automatic };

FindInfraRay[ graph_Graph, origin_, v_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraRay, count,
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
    origin, v ]


(* ===================== InfraRayQ ===================== *)

(* a geodesic inextensible at its far end only: the origin is an endpoint by fiat, which is what distinguishes a ray from a line *)

InfraRayQ[ graph_Graph, ray_List ] /; Length[ ray ] >= 2 :=
  InfraSegmentQ[ graph, ray ] &&
  NoneTrue[ AdjacencyList[ graph, Last @ ray ],
    GraphDistance[ graph, First @ ray, # ] == Length[ ray ] & ]

InfraRayQ[ _Graph, ray_List ] /; Length[ ray ] < 2 := False

InfraRayQ[ graph_Graph, rays_InfraRay ] :=
  AllTrue[ rays[ "Realizations" ], InfraRayQ[ graph, # ] & ]


(* ===================== PencilDirections / PencilCardinality ===================== *)

(* the pencil at O is the set of rays from O; a ray leaves O through exactly one neighbour, so the ray pools over the neighbours partition it, and the cardinality is their path count *)

PencilDirections[ graph_Graph, origin_ ] :=
  Catenate[ FindInfraRay[ graph, origin, #, All ][ "Realizations" ] & /@ AdjacencyList[ graph, origin ] ]

PencilCardinality[ graph_Graph, origin_ ] :=
  Total[ FindInfraRay[ graph, origin, #, All ][ "Multiplicity" ] & /@ AdjacencyList[ graph, origin ] ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraRay[ origin_, v_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      FindInfraRay[ graph, origin, v, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraRay ] ] ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { origin, v } |> ],
    extractBranches[ { opts } ] ]
