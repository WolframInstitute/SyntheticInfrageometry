Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraRay wrapper ===================== *)

InfraRay[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRay[ _List ] ] ] :=
  InfraRay[ Flatten[ reps /. InfraRay[ xs_List ] :> xs, 1 ] ]

InfraRay /: Part[ InfraRay[ reps_List ], i_Integer ] := InfraRay[ { reps[[ i ]] } ]
InfraRay /: Part[ InfraRay[ reps_List ], spec_ ]     := InfraRay[ reps[[ spec ]] ]

InfraRay[ reps_List ][ "Realizations" ] := reps
InfraRay[ reps_List ][ "Length" ]       := Length @ reps
InfraRay[ reps_List ][ "Expand" ]       := InfraRay[ { # } ] & /@ reps
InfraRay[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindRay ===================== *)

(* A ray from origin in v's direction is a pointed half of a maximal
   geodesic line through origin containing v: the vertex sequence
   {origin, w_1, w_2, ..., w_k} with d(origin, w_i) = i and w_k an
   inextensible endpoint on the half containing v.  Multiple realisations
   come from the same direction class having multiple maximal-geodesic
   representatives in the graph (e.g. antipodes on an even cycle).      *)

findRayCore[ graph_Graph, origin_, v_ ] :=
  DeleteDuplicates @ Map[
    line |->
      With[ { oIdx = First[ FirstPosition[ line, origin, { 0 } ], 0 ],
              vIdx = First[ FirstPosition[ line, v, { 0 } ], 0 ] },
        Which[
          oIdx == 0 || vIdx == 0, Nothing,
          oIdx <= vIdx,           line[[ oIdx ;; -1 ]],
          True,                   Reverse[ line[[ 1 ;; oIdx ]] ]
        ]
      ],
    FindLine[ graph, origin, v, All ][ "Realizations" ]
  ]

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
