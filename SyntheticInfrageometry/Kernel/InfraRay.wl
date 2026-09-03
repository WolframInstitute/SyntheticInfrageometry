Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraRay wrapper ===================== *)


InfraRay[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps
(* ===================== FindInfraRay ===================== *)

(* {origin, w_1, ..., w_k} with d(origin, w_i) = i and w_k inextensible, on the half of a maximal geodesic through origin containing v *)

FindInfraRay[ graph_Graph, origin_, v_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  spreadFind[ InfraRay, count,
    { origin0, v0 } |-> DeleteDuplicates @ Map[
      line |->
        With[ { oIdx = First[ FirstPosition[ line, origin0, { 0 } ], 0 ],
                vIdx = First[ FirstPosition[ line, v0, { 0 } ], 0 ] },
          Which[
            oIdx == 0 || vIdx == 0, Nothing,
            oIdx <= vIdx,           line[[ oIdx ;; -1 ]],
            True,                   Reverse[ line[[ 1 ;; oIdx ]] ]
          ]
        ],
      FindInfraLine[ graph, origin0, v0, All ][ "Realizations" ]
    ], origin, v ]


(* ===================== InfraRayQ ===================== *)

(* a geodesic inextensible at its far end only: the origin is an endpoint by fiat, which is what distinguishes a ray from a line *)

InfraRayQ[ graph_Graph, ray_List ] /; Length[ ray ] >= 2 :=
  InfraSegmentQ[ graph, ray ] &&
  NoneTrue[ AdjacencyList[ graph, Last @ ray ],
    GraphDistance[ graph, First @ ray, # ] == Length[ ray ] & ]

InfraRayQ[ _Graph, ray_List ] /; Length[ ray ] < 2 := False

InfraRayQ[ graph_Graph, r_InfraRay ] :=
  AllTrue[ First @ r, InfraRayQ[ graph, # ] & ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraRay[ origin_, v_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      FindInfraRay[ graph, origin, v, All ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { origin, v } |> ],
    extractBranches[ { opts } ] ]
