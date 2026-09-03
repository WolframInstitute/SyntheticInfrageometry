Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraTriangle wrapper ===================== *)


InfraTriangle[ reps_List ][ "Sides" ] := reps

InfraTriangle[ reps_List ][ "Length" ] :=
  Replace[ reps,
    { { }              -> 0,
      segs : { _InfraSegment .. } :> Total[ ( Length[ #[[ 1, 1 ]] ] - 1 ) & /@ segs ] },
    { 1 } ]

InfraTriangle[ reps_List ][ "Vertices" ] :=
  Map[ poly |-> ( InfraPoint /@ Most @ polylineToKnots[ poly ] ), reps ]
(* ===================== FindInfraTriangle ===================== *)


Options[ FindInfraTriangle ] = { Method -> "Exhaustive" };

FindInfraTriangle[ graph_Graph, vertices_List /; Length[ vertices ] === 3,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[ { core = findPolygonCore[ graph, vertices, count, opts ] },
    If[ core === $Failed, $Failed, InfraTriangle[ core ] ]
  ]


(* ===================== InfraTriangleQ ===================== *)

InfraTriangleQ[ graph_Graph, InfraTriangle[ reps_List ] ] :=
  AllTrue[ reps, InfraTriangleQ[ graph, # ] & ]

InfraTriangleQ[ graph_Graph, poly : { _InfraSegment, _InfraSegment, _InfraSegment } ] :=
  InfraPolygonQ[ graph, poly ]

InfraTriangleQ[ _Graph, _ ] := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraTriangle[ verts_List, opts___Rule ] ] :=
  capBranches[
    FindInfraTriangle[ graph, verts, All,
      Sequence @@ FilterRules[ { opts }, Options[ FindInfraTriangle ] ] ][ "Realizations" ],
    extractBranches[ { opts } ] ]
