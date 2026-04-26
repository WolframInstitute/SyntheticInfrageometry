BeginTestSection["TropicalPredicates"]

(* ===== TropicalSegmentQ ===== *)

VerificationTest[
  TropicalSegmentQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 1, 5],
  True,
  TestID -> "TropicalSegmentQ-PathGraph-true"
]

VerificationTest[
  TropicalSegmentQ[PathGraph[Range[5]], {1, 2, 3}, 1, 5],
  False,
  TestID -> "TropicalSegmentQ-PathGraph-false-too-short"
]

VerificationTest[
  TropicalSegmentQ[CycleGraph[4], {1, 2, 3}, 1, 3],
  True,
  TestID -> "TropicalSegmentQ-CycleGraph4-one-side"
]

VerificationTest[
  TropicalSegmentQ[CycleGraph[4], {1, 3, 4}, 1, 3],
  True,
  TestID -> "TropicalSegmentQ-CycleGraph4-other-side"
]

VerificationTest[
  TropicalSegmentQ[CycleGraph[4], {1, 2, 3, 4}, 1, 3],
  False,
  TestID -> "TropicalSegmentQ-CycleGraph4-not-segment"
]

VerificationTest[
  TropicalSegmentQ[GridGraph[{3, 3}], {1, 2, 3, 6, 9}, 1, 9],
  True,
  TestID -> "TropicalSegmentQ-GridGraph3x3-corner-route"
]

(* ===== GeodesicallyConvexQ ===== *)

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {2, 3, 4}],
  True,
  TestID -> "GeodesicallyConvexQ-PathGraph-interior-true"
]

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {1, 5}],
  False,
  TestID -> "GeodesicallyConvexQ-PathGraph-endpoints-false"
]

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {3}],
  True,
  TestID -> "GeodesicallyConvexQ-singleton-true"
]

VerificationTest[
  GeodesicallyConvexQ[CycleGraph[4], {1, 3}],
  False,
  TestID -> "GeodesicallyConvexQ-CycleGraph4-antipodes-false"
]

VerificationTest[
  GeodesicallyConvexQ[CycleGraph[4], {1, 2, 3, 4}],
  True,
  TestID -> "GeodesicallyConvexQ-CycleGraph4-whole-true"
]

VerificationTest[
  GeodesicallyConvexQ[GridGraph[{3, 3}], {1, 2, 3}],
  True,
  TestID -> "GeodesicallyConvexQ-GridGraph3x3-row-true"
]

VerificationTest[
  GeodesicallyConvexQ[CompleteGraph[5], {1, 3, 5}],
  True,
  TestID -> "GeodesicallyConvexQ-CompleteGraph-arbitrary-true"
]

(* ===== UniqueTropicalSegmentQ ===== *)

VerificationTest[
  UniqueTropicalSegmentQ[PathGraph[Range[5]], 1, 5],
  True,
  TestID -> "UniqueTropicalSegmentQ-PathGraph-true"
]

VerificationTest[
  UniqueTropicalSegmentQ[CycleGraph[4], 1, 3],
  False,
  TestID -> "UniqueTropicalSegmentQ-CycleGraph4-antipodes-false"
]

VerificationTest[
  UniqueTropicalSegmentQ[GridGraph[{3, 3}], 1, 9],
  False,
  TestID -> "UniqueTropicalSegmentQ-GridGraph3x3-false"
]

VerificationTest[
  UniqueTropicalSegmentQ[GridGraph[{3, 3}], 1, 2],
  True,
  TestID -> "UniqueTropicalSegmentQ-GridGraph3x3-edge-true"
]

(* ===== TropicalT1Q ===== *)

VerificationTest[
  TropicalT1Q[PathGraph[Range[5]]],
  True,
  TestID -> "TropicalT1Q-PathGraph-true"
]

VerificationTest[
  TropicalT1Q[CycleGraph[6]],
  True,
  TestID -> "TropicalT1Q-CycleGraph6-true"
]

VerificationTest[
  TropicalT1Q[GridGraph[{3, 3}]],
  True,
  TestID -> "TropicalT1Q-GridGraph3x3-true"
]

VerificationTest[
  TropicalT1Q[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}]],
  False,
  TestID -> "TropicalT1Q-Disconnected-false"
]

EndTestSection[]
