Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraBall wrapper ===================== *)

(* InfraBall[{ball}] is the unary form; InfraBall[{ball1, ..., ballk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraBall[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraBall[ _List ] ] ] :=
  InfraBall[ Flatten[ reps /. InfraBall[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraBall ===================== *)

(* The closed metric ball B_r(c) = { v : d(c, v) <= r }, returned as
   InfraBall[{ball}].  InfraPoint[{{c1}, ..., {ck}}] spreads over centers. *)

FindInfraBall[ graph_Graph, InfraPoint[ vs_List ], r_ ] :=
  InfraBall[ ( anchor |-> Select[ VertexList[ graph ], GraphDistance[ graph, First @ anchor, # ] <= r & ] ) /@ vs ]

FindInfraBall[ graph_Graph, c_, r_ ] :=
  InfraBall[ { Select[ VertexList[ graph ], GraphDistance[ graph, c, # ] <= r & ] } ]


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
