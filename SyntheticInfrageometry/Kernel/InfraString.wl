Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[canonicalString]


(* ===================== InfraString wrapper ===================== *)


InfraString[ reps_List ] /;
    AnyTrue[ reps, w |-> MatchQ[ w, _List ] && w =!= canonicalString[ w ] ] :=
  InfraString[ canonicalString /@ reps ]

(* canonical form: the lex-least cyclic rotation of Most[closeWalk[walk]] *)

canonicalString[ { } ]      := { }
canonicalString[ { v_ } ]   := { v }
canonicalString[ walk_List ] /; Length[ walk ] >= 2 :=
  With[ { core = If[ First @ walk === Last @ walk, Most @ walk, walk ] },
    First @ SortBy[ Table[ RotateLeft[ core, k ], { k, 0, Length[ core ] - 1 } ], Identity ]
  ]


(* ===================== Scene-DSL constructor ===================== *)


dispatchConstruction[ graph_Graph, InfraString[ vs__ ] ] :=
  With[ { walk = closeWalk @ { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { canonicalString @ walk },
      { } ]
  ]
