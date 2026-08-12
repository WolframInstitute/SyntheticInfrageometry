Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraLoop wrapper ===================== *)

(* InfraLoop[{walk}] is the unary form; InfraLoop[{w1, ..., wk}] is the
   multi-realisation form.  Each realisation is a closed vertex sequence
   with First === Last (open walks are auto-closed by appending First).
   Set canonicalisation and the shared accessors come from
   defineInfraBundleRules (Tools.wl). *)

InfraLoop[ reps_List ] /;
    AnyTrue[ reps, w |-> MatchQ[ w, _List ] && Length[ w ] >= 2 && First @ w =!= Last @ w ] :=
  InfraLoop[ closeWalk /@ reps ]

(* ===================== Scene-DSL constructor ===================== *)

(* InfraLoop[v1, v2, ..., vk] is the literal closed walk with the given
   vertices; auto-closes if First =!= Last; each consecutive pair must be
   a graph edge. *)

dispatchConstruction[ graph_Graph, InfraLoop[ vs__ ] ] :=
  With[ { walk = closeWalk @ { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { walk },
      { } ]
  ]
