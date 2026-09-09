Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== FindInfraBall ===================== *)

(* the closed ball { v : d(c, v) <= r }, a sorted vertex list.  The centre goes through the anchor rule, and an anchor of several vertices weights one carrier rather than multiplying objects: the ball of a set is its closed r-neighbourhood, the union of the balls around its members *)

FindInfraBall[ graph_Graph, c_, r_ ] :=
  With[ { centers = Keys @ toDensity[ graph, c ] },
    vertexSet @ Select[ VertexList[ graph ],
      v |-> AnyTrue[ centers, GraphDistance[ graph, #, v ] <= r & ] ] ]


(* ===================== InfraBallQ ===================== *)

(* vs is a closed ball iff some c in vs has { v : d(c, v) <= max_{w in vs} d(c, w) } == vs; a family of sets passes iff each does *)

InfraBallQ[ graph_Graph, fam_Association ] := InfraBallQ[ graph, Keys @ fam ]

InfraBallQ[ graph_Graph, sets : { __List } ] /; ! AllTrue[ sets, VertexQ[ graph, # ] & ] :=
  AllTrue[ sets, InfraBallQ[ graph, # ] & ]

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
  vertexSet @ BallHull[ graph, hullVertices @ s ]

(* S is ball-convex: it equals its own ball hull (an intersection of balls). *)

BallHullQ[ graph_Graph, s_ ] :=
  With[ { vs = hullVertices @ s },
    Sort @ BallHull[ graph, vs ] === Union @ vs ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraBall[ center_, r_ ] ] :=
  applySelectOption[ graph,
    { FindInfraBall[ graph, center, r ] },
    None, False, <| "Center" -> center, "Radius" -> r |> ]
