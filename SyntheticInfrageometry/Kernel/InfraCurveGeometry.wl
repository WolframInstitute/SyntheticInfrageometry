Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== TurningAngles ===================== *)

(* Discrete curvature at an interior vertex v_i of a polygonal curve path =
   {v_1, ..., v_k} on a graph is the exterior angle Pi - InfraAngle[g, {v_{i-1},
   v_i, v_{i+1}}].  For closed cycles (First[path] == Last[path]) the wrap-
   around triple at v_1 is included so the result has length k - 1; for open
   paths the result has length k - 2. *)

TurningAngles[ graph_Graph, path : { __ } ] :=
  With[ { triples =
      If[ First[ path ] === Last[ path ] && Length[ path ] >= 3,
        Partition[ Most[ path ], 3, 1, { 1, 1 } ],
        Partition[ path, 3, 1 ]
      ]
    },
    Pi - ( InfraAngle[ graph, # ] & /@ triples )
  ]


(* ===================== TotalCurvature ===================== *)

(* The discrete total curvature K(c) = Sum_i kappa_i of a polygonal curve.  On
   graphs this is the exact analogue of the continuous Integral_0^L kappa(s) ds
   because the curve already is a polygon -- no approximation step. *)

TotalCurvature[ graph_Graph, path : { __ } ] :=
  Total @ TurningAngles[ graph, path ]


(* ===================== TotalAbsoluteCurvature ===================== *)

(* Sum_i |kappa_i|.  Always >= 0; conjecturally >= 2 Pi for any closed cycle
   (the graph analogue of Fenchel's inequality). *)

TotalAbsoluteCurvature[ graph_Graph, path : { __ } ] :=
  Total @ Abs @ TurningAngles[ graph, path ]


(* ===================== TurningNumber ===================== *)

(* r(c) = K(c) / (2 Pi).  In the smooth planar setting Hopf's theorem forces
   r(c) in {+1, -1} for simple closed curves; on graphs r(c) is generally a
   real number whose deviation from an integer measures how far the graph
   substrate is from a smooth surface. *)

TurningNumber[ graph_Graph, cycle : { __ } ] :=
  TotalCurvature[ graph, cycle ] / ( 2 Pi )
