Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraLoop wrapper ===================== *)


InfraLoop[ reps_List ] /;
    AnyTrue[ reps, w |-> MatchQ[ w, _List ] && Length[ w ] >= 2 && First @ w =!= Last @ w ] :=
  InfraLoop[ closeWalk /@ reps ]

(* ===================== Scene-DSL constructor ===================== *)


dispatchConstruction[ graph_Graph, InfraLoop[ vs__ ] ] :=
  With[ { walk = closeWalk @ { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { walk },
      { } ]
  ]
