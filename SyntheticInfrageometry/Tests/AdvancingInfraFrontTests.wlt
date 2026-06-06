BeginTestSection["AdvancingInfraFront"]

(* FindAdvancingInfraFront[g, o, steps] returns the foliation
   { InfraSet[S_0], ..., InfraSet[S_steps] } of an oriented wavefront from o. *)

(* The foliation has one set per step, plus the seed S_0. *)
VerificationTest[
  Length @ FindAdvancingInfraFront[ PathGraph @ Range @ 6, 1, 3 ],
  4,
  TestID -> "AdvancingFront-length-is-steps-plus-one"
]

(* S_0 is the source itself. *)
VerificationTest[
  First[ FindAdvancingInfraFront[ GridGraph[ { 4, 4 } ], 6, 3 ] ][ "Vertices" ],
  { 6 },
  TestID -> "AdvancingFront-S0-is-origin"
]

(* The default front is non-backtracking, so it never empties: on a triangle it
   circulates ({A},{B,C},{B,C},{A},...) rather than dying past the eccentricity
   as the metric sphere does. *)
VerificationTest[
  AllTrue[ FindAdvancingInfraFront[ CycleGraph[ 3 ], 1, 5 ], #[ "Vertices" ] =!= { } & ],
  True,
  TestID -> "AdvancingFront-default-never-empties-on-cycle"
]

(* "GeodesicSprayDepth" -> Infinity forbids transverse (d(o,c) == d(o,b)) steps;
   on a 1D substrate the surviving outward front is exactly the metric sphere
   foliation S_i = { v : d(o, v) == i }. *)
VerificationTest[
  With[ { g = PathGraph @ Range @ 7 },
    Sort /@ (#[ "Vertices" ] & /@ FindAdvancingInfraFront[ g, 1, 4, "GeodesicSprayDepth" -> Infinity ]) ===
      Table[ Select[ VertexList @ g, GraphDistance[ g, 1, # ] == i & ], { i, 0, 4 } ] ],
  True,
  TestID -> "AdvancingFront-spray-infinity-is-metric-spheres-on-path"
]

(* "SelfAvoidanceDepth" -> Infinity is a fully self-avoiding front: a triangle
   has no unvisited neighbour after two steps, so the front dies. *)
VerificationTest[
  Last[ FindAdvancingInfraFront[ CycleGraph[ 3 ], 1, 4, "SelfAvoidanceDepth" -> Infinity ] ][ "Vertices" ],
  { },
  TestID -> "AdvancingFront-self-avoiding-dies-on-triangle"
]

EndTestSection[]
