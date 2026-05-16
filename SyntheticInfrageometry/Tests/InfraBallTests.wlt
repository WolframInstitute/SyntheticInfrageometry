BeginTestSection["InfraBall"]

(* ===== FindInfraBall ===== *)

VerificationTest[
  FindInfraBall[PathGraph[Range[5]], 3, 1],
  InfraBall[{{2, 3, 4}}],
  TestID -> "FindInfraBall-PathGraph-interior-r1"
]

VerificationTest[
  Sort /@ First @ FindInfraBall[PathGraph[Range[5]], 1, 2],
  {{1, 2, 3}},
  TestID -> "FindInfraBall-PathGraph-endpoint-r2"
]

VerificationTest[
  Sort /@ First @ FindInfraBall[CycleGraph[6], 1, 1],
  {{1, 2, 6}},
  TestID -> "FindInfraBall-CycleGraph6-r1"
]

VerificationTest[
  Sort /@ First @ FindInfraBall[CompleteGraph[4], 1, 1],
  {{1, 2, 3, 4}},
  TestID -> "FindInfraBall-CompleteGraph4-r1"
]

VerificationTest[
  Sort /@ First @ FindInfraBall[StarGraph[5], 1, 1],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraBall-StarGraph5-hub"
]

VerificationTest[
  Sort /@ First @ FindInfraBall[StarGraph[5], 2, 1],
  {{1, 2}},
  TestID -> "FindInfraBall-StarGraph5-leaf"
]

VerificationTest[
  FindInfraBall[PathGraph[Range[5]], 3, 0],
  InfraBall[{{3}}],
  TestID -> "FindInfraBall-r0-singleton"
]

(* InfraBall accepts InfraPoint multi-anchor (spread over each center) *)
VerificationTest[
  Sort /@ First @ FindInfraBall[PathGraph[Range[5]], InfraPoint[{{1}, {5}}], 1],
  {{1, 2}, {4, 5}},
  TestID -> "FindInfraBall-multi-anchor"
]

(* ===== InfraBall wrapper auto-flatten ===== *)

VerificationTest[
  InfraBall[{InfraBall[{{1, 2}}], InfraBall[{{2, 3}}]}],
  InfraBall[{{1, 2}, {2, 3}}],
  TestID -> "InfraBall-auto-flatten-nested"
]

VerificationTest[
  InfraBall[{{1, 2, 3}}],
  InfraBall[{{1, 2, 3}}],
  TestID -> "InfraBall-unary-no-flatten"
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

(* ===== infraVertexSet for InfraBall ===== *)

VerificationTest[
  InfraDistance[PathGraph[Range[7]], InfraBall[{{2, 3}}], InfraBall[{{6, 7}}]],
  3,
  TestID -> "InfraDistance-InfraBall-InfraBall"
]

EndTestSection[]
