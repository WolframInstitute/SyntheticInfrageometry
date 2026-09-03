Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraBall wrapper ===================== *)


InfraBall[ reps_List ][ "Volume" ] := Length /@ reps
(* ===================== FindInfraBall ===================== *)


FindInfraBall[ graph_Graph, c_, r_ ] :=
  InfraBall[ ( center |-> Select[ VertexList[ graph ], GraphDistance[ graph, center, # ] <= r & ] ) /@
    infraSpread[ c ] ]


(* ===================== InfraBallQ ===================== *)

(* vs is a closed ball iff some c in vs has { v : d(c, v) <= max_{w in vs} d(c, w) } == vs *)

InfraBallQ[ graph_Graph, b : _InfraBall | _InfraSet ] :=
  AllTrue[ If[ Head[ b ] === InfraSet, { First @ b }, First @ b ], InfraBallQ[ graph, # ] & ]

InfraBallQ[ graph_Graph, vs_List ] :=
  vs =!= { } &&
  AnyTrue[ vs, c |->
    With[ { r = Max @ ( GraphDistance[ graph, c, # ] & /@ vs ) },
      Sort @ Select[ VertexList[ graph ], GraphDistance[ graph, c, # ] <= r & ] === Sort @ vs
    ]
  ]


(* ===================== FindBallHull / BallHullQ ===================== *)

(* the intersection of all closed balls containing S: the smallest ball-convex (Mazur) superset *)

FindBallHull[ graph_Graph, s_ ] :=
  InfraSet @ Sort @ BallHull[ graph, hullVertices @ s ]

(* S is ball-convex: it equals its own ball hull (an intersection of balls). *)

BallHullQ[ graph_Graph, s_ ] :=
  With[ { vs = hullVertices @ s },
    Sort @ BallHull[ graph, vs ] === Union @ vs ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraBall[ center_, r_ ] ] :=
  applySelectOption[ graph,
    FindInfraBall[ graph, center, r ][ "Realizations" ],
    None, False, <| "Center" -> center, "Radius" -> r |> ]
