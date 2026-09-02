BeginTestSection["AdvancingInfraFront"]

(* FindAdvancingInfraFront[g, o, steps] returns the foliation
   { InfraSet[S_0], ..., InfraSet[S_steps] } of a bouncing wavefront from o: each
   step moves every front vertex one geodesic step outward from the previous front
   S_{i-1}, reflecting back inward where there is no outward neighbour. The state is
   the pair (S_{i-1}, S_i), so it is a NestList on consecutive-front pairs. *)

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

(* The defining property: the front never empties, on any finite graph -- every
   front vertex moves to a neighbour (out or reflected back in). This makes it
   infinite (eventually periodic), unlike the metric sphere which dies past the
   eccentricity. *)
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

(* Immediate bounce, no dwell: no two consecutive fronts are equal, so the longest
   run of identical fronts is 1. This is the contrast with the old keep rule (run
   = k+1) -- the wave turns straight around at a turning point. *)
VerificationTest[
  Max[ Length /@ Split[ Sort /@ ( #[ "Vertices" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 30 ] ) ] ],
  1,
  TestID -> "AdvancingFront-immediate-bounce-no-dwell"
]

(* It bounces: from the centre of a path the wave runs out to the ends, reflects,
   and refocuses back at the origin -- so { 5 } recurs as a later front. The metric
   sphere never returns to the origin. *)
VerificationTest[
  MemberQ[ Rest[ #[ "Vertices" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 16 ] ], { 5 } ],
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

(* Locality: S_{i+1} subset of S_i union N(S_i) at every step (each vertex moves to
   a neighbour). *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], front = FindAdvancingInfraFront[ GridGraph[ { 5, 5 } ], 13, 10 ] },
    AllTrue[ Partition[ #[ "Vertices" ] & /@ front, 2, 1 ],
      SubsetQ[ Union[ #[[ 1 ]], VertexList @ NeighborhoodGraph[ g, #[[ 1 ]] ] ], #[[ 2 ]] ] & ] ],
  True,
  TestID -> "AdvancingFront-local-step"
]

(* Thin, no flooding: the front never grows past the largest metric shell of o --
   it stays a (reflected) metric sphere, not a spreading blob. *)
VerificationTest[
  With[ { g = GridGraph[ { 7, 7 } ] },
    Max[ #[ "Length" ] & /@ FindAdvancingInfraFront[ g, 25, 40 ] ] <=
      Max[ Length /@ GatherBy[ VertexList @ g, GraphDistance[ g, 25, # ] & ] ] ],
  True,
  TestID -> "AdvancingFront-thin-no-flooding"
]

(* Multi-source: an InfraPoint origin seeds S_0 with the whole vertex set. *)
VerificationTest[
  First[ FindAdvancingInfraFront[ CycleGraph[ 10 ], InfraSet[ { 1, 6 } ], 4 ] ][ "Vertices" ],
  { 1, 6 },
  TestID -> "AdvancingFront-multi-source-seed"
]

EndTestSection[]
