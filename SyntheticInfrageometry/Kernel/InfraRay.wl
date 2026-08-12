Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraRay wrapper ===================== *)

(* InfraRay[{ray}] is the unary form; InfraRay[{ray1, ..., rayk}] is the
   multi-realisation form.  Set canonicalisation and the shared accessors
   come from defineInfraBundleRules (Tools.wl). *)

(* "Length" = list of edge counts, one per realisation: |ray| - 1. *)
InfraRay[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps
(* ===================== FindInfraRay ===================== *)

(* A ray from origin in v's direction is a pointed half of a maximal
   geodesic line through origin containing v: the vertex sequence
   {origin, w_1, w_2, ..., w_k} with d(origin, w_i) = i and w_k an
   inextensible endpoint on the half containing v.  Multiple realisations
   come from the same direction class having multiple maximal-geodesic
   representatives in the graph (e.g. antipodes on an even cycle).      *)

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


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraRay[ origin_, v_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      FindInfraRay[ graph, origin, v, All ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { origin, v } |> ],
    extractBranches[ { opts } ] ]
