Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findRayCore]


(* ===================== InfraRay wrapper ===================== *)

(* InfraRay[{ray}] is the unary form; InfraRay[{ray1, ..., rayk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraRay[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRay[ _List ] ] ] :=
  InfraRay[ Flatten[ reps /. InfraRay[ xs_List ] :> xs, 1 ] ]


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
    #[[ 1, 1 ]] & /@ FindLine[ graph, origin, v, All ]
  ]

FindRay[ graph_Graph, origin_, v_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  infraSpreadAndCartesian[ InfraRay, count, findRayCore[ graph, ##] &, origin, v ]
