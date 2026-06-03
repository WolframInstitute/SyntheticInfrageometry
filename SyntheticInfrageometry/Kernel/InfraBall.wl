Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraBall wrapper ===================== *)

(* InfraBall[{ball}] is the unary form; InfraBall[{ball1, ..., ballk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraBall[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraBall[ _List ] ] ] :=
  InfraBall[ Flatten[ reps /. InfraBall[ xs_List ] :> xs, 1 ] ]

(* "Volume" = vertex count per realisation. *)
InfraBall[ reps_List ][ "Volume" ] := Length /@ reps

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraBall[ reps_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraBall[ reps ] ]
InfraBall[ reps_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraBall[ reps ] ]
InfraBall[ reps_List ][ "Measure" ] := InfraMeasure[ InfraBall[ reps ] ]
InfraBall[ reps_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraBall[ reps ], Method -> "Probability" ]
(* ===================== FindInfraBall ===================== *)

(* The closed metric ball B_r(c) = { v : d(c, v) <= r }, returned as
   InfraBall[{ball}].  A multi-anchor center (InfraPoint wrapper or a list
   of unary InfraPoint wrappers) spreads into one realisation per center. *)

FindInfraBall[ graph_Graph, c_, r_ ] :=
  InfraBall[ ( center |-> Select[ VertexList[ graph ], GraphDistance[ graph, center, # ] <= r & ] ) /@ infraSpread[ c ] ]


(* ===================== InfraBallQ ===================== *)

(* vs is a closed metric ball iff there exists a center c in vs such that
   { v : d(c, v) <= max_{w in vs} d(c, w) } == vs. *)

InfraBallQ[ graph_Graph, vs_List ] :=
  vs =!= { } &&
  AnyTrue[ vs, c |->
    With[ { r = Max @ ( GraphDistance[ graph, c, # ] & /@ vs ) },
      Sort @ Select[ VertexList[ graph ], GraphDistance[ graph, c, # ] <= r & ] === Sort @ vs
    ]
  ]


(* ===================== FindBallHull / BallHullQ ===================== *)

(* The ball hull of S: the intersection of all closed balls containing S, the
   smallest ball-convex (Mazur) superset, returned as an InfraSet.  S is any
   Infra* object, a list of them, or a bare vertex list; the underlying vertex
   set is handed to BallHull (WolframInstitute`Infrageometry`).  Companion of
   the segment / line hulls (FindSegmentHull, FindLineHull) -- intersection of
   balls rather than closure under geodesics. *)

FindBallHull[ graph_Graph, s_ ] :=
  InfraSet @ Sort @ BallHull[ graph, hullVertices @ s ]

(* S is ball-convex: it equals its own ball hull (an intersection of balls). *)

BallHullQ[ graph_Graph, s_ ] :=
  With[ { vs = hullVertices @ s },
    Sort @ BallHull[ graph, vs ] === Union @ vs ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraBall[ center_, r_ ] ] :=
  applySelectOption[ graph,
    #[[ 1, 1 ]] & /@ FindInfraBall[ graph, center, r ],
    None, False, <| "Center" -> center, "Radius" -> r |> ]
