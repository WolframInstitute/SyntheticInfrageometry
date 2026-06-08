BeginTestSection["AdvancingInfraFront"]

(* FindAdvancingInfraFront[g, o, steps, k] returns the foliation
   { InfraSet[S_0], ..., InfraSet[S_steps] } of a bouncing wavefront from o:
   each step advances every front vertex one geodesic step outward from the
   k-th-predecessor shell S_{i-k}, keeping any vertex that cannot advance. *)

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

(* The defining property: the front never empties, on any finite graph and for
   any k -- because every front vertex either advances or is kept. This is what
   makes it infinite (eventually periodic), unlike the metric sphere which dies
   past the eccentricity. *)
VerificationTest[
  Min[ #[ "Length" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 60 ] ] >= 1,
  True,
  TestID -> "AdvancingFront-never-empties-path"
]
VerificationTest[
  Min[ #[ "Length" ] & /@ FindAdvancingInfraFront[ CycleGraph[ 8 ], 1, 60 ] ] >= 1,
  True,
  TestID -> "AdvancingFront-never-empties-cycle"
]

(* It bounces: from the centre of a path the wave runs out to the ends, reflects,
   and refocuses back at the origin -- so { 5 } recurs as a later front. The
   metric sphere never returns to the origin. *)
VerificationTest[
  MemberQ[ Rest[ #[ "Vertices" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 12 ] ], { 5 } ],
  True,
  TestID -> "AdvancingFront-refocuses-at-origin"
]

(* The size sequence is non-monotone -- it grows again after shrinking (a bounce),
   which a strictly-outward shell never does. *)
VerificationTest[
  With[ { sizes = #[ "Length" ] & /@ FindAdvancingInfraFront[ GridGraph[ { 7, 7 } ], 25, 20 ] },
    AnyTrue[ Differences @ sizes, Positive ] && AnyTrue[ Differences @ sizes, Negative ] ],
  True,
  TestID -> "AdvancingFront-size-is-non-monotone"
]

(* Locality + keep: S_{i+1} subset of S_i union N(S_i) at every step. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], front = FindAdvancingInfraFront[ GridGraph[ { 5, 5 } ], 13, 10 ] },
    AllTrue[ Partition[ #[ "Vertices" ] & /@ front, 2, 1 ],
      SubsetQ[ Union[ #[[ 1 ]], VertexList @ NeighborhoodGraph[ g, #[[ 1 ]] ] ], #[[ 2 ]] ] & ] ],
  True,
  TestID -> "AdvancingFront-local-with-keep"
]

(* k sets the dwell at each turning point: the longest run of consecutive equal
   fronts is k+1, so it strictly increases with k. *)
VerificationTest[
  Table[ Max[ Length /@ Split[ #[ "Vertices" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 30, k ] ] ], { k, 1, 3 } ],
  { 2, 3, 4 },
  TestID -> "AdvancingFront-k-controls-dwell"
]

(* Multi-source: an InfraPoint origin seeds S_0 with the whole vertex set. *)
VerificationTest[
  First[ FindAdvancingInfraFront[ CycleGraph[ 10 ], InfraPoint[ { 1, 6 } ], 4 ] ][ "Vertices" ],
  { 1, 6 },
  TestID -> "AdvancingFront-multi-source-seed"
]

EndTestSection[]
