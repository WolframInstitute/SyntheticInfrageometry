BeginTestSection["InfraBall"]

(* ===== FindInfraBall: a ball is its sorted vertex list ===== *)

VerificationTest[
  FindInfraBall[PathGraph[Range[5]], 3, 1],
  {2, 3, 4},
  TestID -> "FindInfraBall-PathGraph-interior-r1"
]

VerificationTest[
  FindInfraBall[PathGraph[Range[5]], 1, 2],
  {1, 2, 3},
  TestID -> "FindInfraBall-PathGraph-endpoint-r2"
]

VerificationTest[
  FindInfraBall[CycleGraph[6], 1, 1],
  {1, 2, 6},
  TestID -> "FindInfraBall-CycleGraph6-r1"
]

VerificationTest[
  FindInfraBall[CompleteGraph[4], 1, 1],
  {1, 2, 3, 4},
  TestID -> "FindInfraBall-CompleteGraph4-r1"
]

VerificationTest[
  FindInfraBall[StarGraph[5], 1, 1],
  {1, 2, 3, 4, 5},
  TestID -> "FindInfraBall-StarGraph5-hub"
]

VerificationTest[
  FindInfraBall[StarGraph[5], 2, 1],
  {1, 2},
  TestID -> "FindInfraBall-StarGraph5-leaf"
]

VerificationTest[
  FindInfraBall[PathGraph[Range[5]], 3, 0],
  {3},
  TestID -> "FindInfraBall-r0-singleton"
]

(* an anchor of several vertices weights one carrier: the ball of a set is its closed r-neighbourhood, the union of the balls around its members *)
VerificationTest[
  FindInfraBall[PathGraph[Range[5]], <| 1 -> 1, 5 -> 1 |>, 1],
  {1, 2, 4, 5},
  TestID -> "FindInfraBall-multi-anchor"
]

VerificationTest[
  FindInfraBall[PathGraph[Range[5]], {1, 5}, 1],
  {1, 2, 4, 5},
  TestID -> "FindInfraBall-multi-anchor-vertex-list"
]

(* the ball of a walk is the tube around it *)
VerificationTest[
  FindInfraBall[GridGraph[{3, 3}], FindInfraSegment[GridGraph[{3, 3}], 1, 3], 1],
  {1, 2, 3, 4, 5, 6},
  TestID -> "FindInfraBall-walk-anchor-is-the-tube"
]

(* a ball is a legal HighlightGraph argument *)
VerificationTest[
  Head @ HighlightGraph[PathGraph[Range[5]], FindInfraBall[PathGraph[Range[5]], 3, 1]],
  Graph,
  TestID -> "FindInfraBall-is-a-HighlightGraph-argument"
]

(* ===== InfraBallQ ===== *)

VerificationTest[
  InfraBallQ[PathGraph[Range[5]], {2, 3, 4}],
  True,
  TestID -> "InfraBallQ-PathGraph-r1-ball-true"
]

VerificationTest[
  InfraBallQ[PathGraph[Range[5]], {1, 2}],
  True,
  TestID -> "InfraBallQ-PathGraph-endpoint-r1-true"
]

VerificationTest[
  InfraBallQ[PathGraph[Range[5]], {1, 5}],
  False,
  TestID -> "InfraBallQ-PathGraph-endpoints-only-false"
]

VerificationTest[
  InfraBallQ[CompleteGraph[4], {1, 2, 3, 4}],
  True,
  TestID -> "InfraBallQ-CompleteGraph4-full-true"
]

VerificationTest[
  InfraBallQ[CompleteGraph[4], {1, 2}],
  False,
  TestID -> "InfraBallQ-CompleteGraph4-half-false"
]

VerificationTest[
  InfraBallQ[StarGraph[5], {1, 2, 3, 4, 5}],
  True,
  TestID -> "InfraBallQ-StarGraph5-full-true"
]

VerificationTest[
  InfraBallQ[StarGraph[5], {1, 2}],
  True,
  TestID -> "InfraBallQ-StarGraph5-leaf-with-hub-true"
]

VerificationTest[
  InfraBallQ[PathGraph[Range[5]], {}],
  False,
  TestID -> "InfraBallQ-empty-false"
]

(* a family of sets passes iff every member does *)
VerificationTest[
  { InfraBallQ[PathGraph[Range[5]], {{2, 3, 4}, {1, 2}}], InfraBallQ[PathGraph[Range[5]], {{2, 3, 4}, {1, 5}}] },
  { True, False },
  TestID -> "InfraBallQ-family-is-the-conjunction"
]

(* ===== InfraDistance between balls ===== *)

VerificationTest[
  InfraDistance[PathGraph[Range[7]], FindInfraBall[PathGraph[Range[7]], 2, 1], FindInfraBall[PathGraph[Range[7]], 7, 1]],
  3,
  TestID -> "InfraDistance-ball-ball"
]

EndTestSection[]
