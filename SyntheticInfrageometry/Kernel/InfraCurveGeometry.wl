Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== TurningAngles ===================== *)

(* kappa_i = Pi - InfraAngle[g, {v_{i-1}, v_i, v_{i+1}}]; a closed cycle includes the wrap-around triple *)

TurningAngles[ _Graph, { } ] := { }

(* a walk graph is read as its vertex sequence, closed when it is a cycle *)
TurningAngles[ graph_Graph, w_Graph ] := TurningAngles[ graph, First @ walkRealisations @ w ]

(* turning happens only at the knots of a polyline -- a List of geodesic legs -- since the interior of each leg is straight by construction *)
TurningAngles[ graph_Graph, legs : { __Graph } ] := TurningAngles[ graph, polylineToKnots @ legs ]

TurningAngles[ graph_Graph, path : { __ } ] /; ! MatchQ[ path, { __Graph } ] :=
  With[ { triples =
      If[ First[ path ] === Last[ path ] && Length[ path ] >= 3,
        Partition[ Most[ path ], 3, 1, { 1, 1 } ],
        Partition[ path, 3, 1 ]
      ]
    },
    Pi - ( InfraAngle[ graph, # ] & /@ triples )
  ]


(* ===================== TotalCurvature ===================== *)

(* K(c) = Sum_i kappa_i, exact rather than an approximation of Integral kappa ds, since the curve already is a polygon *)

TotalCurvature[ graph_Graph, path : ( { __ } | _Graph ) ] :=
  Total @ TurningAngles[ graph, path ]


(* ===================== TotalAbsoluteCurvature ===================== *)

(* Sum_i |kappa_i|; conjecturally >= 2 Pi for any closed cycle, the graph analogue of Fenchel's inequality *)

TotalAbsoluteCurvature[ graph_Graph, path : ( { __ } | _Graph ) ] :=
  Total @ Abs @ TurningAngles[ graph, path ]


(* ===================== TurningNumber ===================== *)

(* r(c) = K(c) / (2 Pi); Hopf forces r in {+1, -1} for smooth simple closed curves, on a graph it is generally real *)

TurningNumber[ graph_Graph, cycle : ( { __ } | _Graph ) ] :=
  TotalCurvature[ graph, cycle ] / ( 2 Pi )
