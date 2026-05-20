Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[canonicalString]


(* ===================== InfraString wrapper ===================== *)

(* InfraString[{walk}] is the unary form; InfraString[{w1, ..., wk}] the
   multi-realisation form.  An InfraString realisation is a closed walk
   modulo cyclic rotation: stored as the lex-least cyclic rotation of
   Most[closeWalk[walk]] (the "core" of the closed walk, without the wrap-
   around vertex repetition).  Orientation is preserved -- no reversal
   quotient.  Equality of strings is SameQ on canonical forms. *)

InfraString[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraString[ _List ] ] ] :=
  InfraString[ Flatten[ reps /. InfraString[ xs_List ] :> xs, 1 ] ]

InfraString[ reps_List ] /;
    AnyTrue[ reps, w |-> MatchQ[ w, _List ] && w =!= canonicalString[ w ] ] :=
  InfraString[ DeleteDuplicates[ canonicalString /@ reps ] ]


(* Canonical form: lex-least cyclic rotation of Most[closeWalk[walk]].
   For a single-vertex degenerate string {v} the canonical form is {v}. *)

canonicalString[ { } ]      := { }
canonicalString[ { v_ } ]   := { v }
canonicalString[ walk_List ] /; Length[ walk ] >= 2 :=
  With[ { core = If[ First @ walk === Last @ walk, Most @ walk, walk ] },
    First @ SortBy[ Table[ RotateLeft[ core, k ], { k, 0, Length[ core ] - 1 } ], Identity ]
  ]


(* ===================== Scene-DSL constructor ===================== *)

(* InfraString[v1, v2, ..., vk] is the literal closed walk with the given
   vertices; auto-closes if First =!= Last; each consecutive pair must be
   a graph edge.  Stored as canonical rotation. *)

dispatchConstruction[ graph_Graph, InfraString[ vs__ ] ] :=
  With[ { walk = closeWalk @ { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { canonicalString @ walk },
      { } ]
  ]
