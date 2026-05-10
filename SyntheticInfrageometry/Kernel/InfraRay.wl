Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


(* ===================== InfraRay wrapper ===================== *)

InfraRay[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRay[ _List ] ] ] :=
  InfraRay[ Flatten[ reps /. InfraRay[ xs_List ] :> xs, 1 ] ]

InfraRay /: Part[ InfraRay[ reps_List ], i_Integer ] := InfraRay[ { reps[[ i ]] } ]
InfraRay /: Part[ InfraRay[ reps_List ], spec_ ]     := InfraRay[ reps[[ spec ]] ]

InfraRay[ reps_List ][ "Realizations" ] := reps
InfraRay[ reps_List ][ "Length" ]       := Length @ reps
InfraRay[ reps_List ][ "Expand" ]       := InfraRay[ { # } ] & /@ reps
InfraRay[ reps_List ][ "First" ]        := First @ reps


(* canonicalLine and allCanonicalLines are projective-incidence helpers
   shared with InfraPencil.wl and ProjectiveGeometry.wl. *)

canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine /@ FindLine[ graph, #[[ 1 ]], #[[ 2 ]], All ][ "Realizations" ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]


(* ===================== FindRay ===================== *)

(* A ray at base vertex origin in the direction of v is a maximal
   geodesic through origin containing v -- the same shape as FindLine's
   output, but framed projectively as "the line through origin in v's
   direction".  FindRay enumerates every such line and returns them as
   InfraRay[{ray1, ray2, ...}].  Multiple realisations belong to one
   direction class (the equivalence class of canonicalLine).            *)

findRayCore[ graph_Graph, origin_, v_ ] :=
  DeleteDuplicatesBy[ FindLine[ graph, origin, v, All ][ "Realizations" ], canonicalLine ]

FindRay[ graph_Graph, origin_, v_, All ] :=
  infraSpreadAndCartesian[ InfraRay, All, findRayCore[ graph, ##] &, origin, v ]

FindRay[ graph_Graph, origin_, v_, UpTo[ n_Integer ] ] :=
  With[ { result = FindRay[ graph, origin, v, All ] },
    If[ result === $Failed, $Failed,
      InfraRay[ Take[ result[ "Realizations" ], UpTo[ n ] ] ]
    ]
  ]

FindRay[ graph_Graph, origin_, v_, n_Integer : 1 ] :=
  With[ { result = FindRay[ graph, origin, v, UpTo[ n ] ] },
    If[ result === $Failed || result[ "Length" ] < n, $Failed, result ]
  ]
