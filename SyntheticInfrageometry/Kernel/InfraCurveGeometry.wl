Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== TurningAngles ===================== *)

(* kappa_i = Pi - InfraAngle[g, {v_{i-1}, v_i, v_{i+1}}]; a closed cycle includes the wrap-around triple *)

TurningAngles[ _Graph, { } ] := { }

TurningAngles[ graph_Graph, path : { __ } ] :=
  With[ { triples =
      If[ First[ path ] === Last[ path ] && Length[ path ] >= 3,
        Partition[ Most[ path ], 3, 1, { 1, 1 } ],
        Partition[ path, 3, 1 ]
      ]
    },
    Pi - ( InfraAngle[ graph, # ] & /@ triples )
  ]

(* turning happens only at the knots: the interior of each geodesic leg is straight by construction *)

TurningAngles[ graph_Graph, InfraPolyline[ reps_List ] ] :=
  TurningAngles[ graph, # ] & /@ polylineToKnotVertices[ reps ]


(* ===================== TotalCurvature ===================== *)

(* K(c) = Sum_i kappa_i, exact rather than an approximation of Integral kappa ds, since the curve already is a polygon *)

TotalCurvature[ graph_Graph, path : { __ } ] :=
  Total @ TurningAngles[ graph, path ]


(* ===================== TotalAbsoluteCurvature ===================== *)

(* Sum_i |kappa_i|; conjecturally >= 2 Pi for any closed cycle, the graph analogue of Fenchel's inequality *)

TotalAbsoluteCurvature[ graph_Graph, path : { __ } ] :=
  Total @ Abs @ TurningAngles[ graph, path ]


(* ===================== TurningNumber ===================== *)

(* r(c) = K(c) / (2 Pi); Hopf forces r in {+1, -1} for smooth simple closed curves, on a graph it is generally real *)

TurningNumber[ graph_Graph, cycle : { __ } ] :=
  TotalCurvature[ graph, cycle ] / ( 2 Pi )
