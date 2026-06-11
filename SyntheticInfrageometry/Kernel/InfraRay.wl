Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraRay wrapper ===================== *)

(* InfraRay[{ray}] is the unary form; InfraRay[{ray1, ..., rayk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraRay[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRay[ _List ] ] ] :=
  InfraRay[ Flatten[ reps /. InfraRay[ xs_List ] :> xs, 1 ] ]

(* "Length" = list of edge counts, one per realisation: |ray| - 1. *)
InfraRay[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraRay[ reps_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraRay[ reps ] ]
InfraRay[ reps_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraRay[ reps ] ]
InfraRay[ reps_List ][ "Measure" ] := InfraMeasure[ InfraRay[ reps ] ]
InfraRay[ reps_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraRay[ reps ], Method -> "Probability" ]
(* ===================== FindInfraRay ===================== *)

(* A ray from origin in v's direction is a pointed half of a maximal
   geodesic line through origin containing v: the vertex sequence
   {origin, w_1, w_2, ..., w_k} with d(origin, w_i) = i and w_k an
   inextensible endpoint on the half containing v.  Multiple realisations
   come from the same direction class having multiple maximal-geodesic
   representatives in the graph (e.g. antipodes on an even cycle).      *)

FindInfraRay[ graph_Graph, origin_, v_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
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
      #[[ 1, 1 ]] & /@ FindInfraLine[ graph, origin0, v0, All ]
    ], origin, v ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraRay[ origin_, v_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindInfraRay[ graph, origin, v, All ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { origin, v } |> ],
    extractBranches[ { opts } ] ]
