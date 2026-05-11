BeginTestSection["MetricGeometry"]

(* ===== MetricInterval ===== *)

VerificationTest[
  Sort @ MetricInterval[PathGraph[Range[5]], 1, 5],
  {1, 2, 3, 4, 5},
  TestID -> "MetricInterval-PathGraph-full"
]

VerificationTest[
  Sort @ MetricInterval[PathGraph[Range[5]], 2, 4],
  {2, 3, 4},
  TestID -> "MetricInterval-PathGraph-interior"
]

VerificationTest[
  Sort @ MetricInterval[CycleGraph[4], 1, 3],
  {1, 2, 3, 4},
  TestID -> "MetricInterval-CycleGraph4-antipodes-fill"
]

VerificationTest[
  Sort @ MetricInterval[GridGraph[{3, 3}], 1, 9],
  Range[9],
  TestID -> "MetricInterval-GridGraph3x3-corners-fill"
]

VerificationTest[
  Sort @ MetricInterval[GridGraph[{3, 3}], 1, 3],
  {1, 2, 3},
  TestID -> "MetricInterval-GridGraph3x3-row"
]

VerificationTest[
  MetricInterval[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], 1, 3],
  {},
  TestID -> "MetricInterval-disconnected-empty"
]

(* ===== GeodesicCount ===== *)

VerificationTest[
  GeodesicCount[PathGraph[Range[5]], 1, 5],
  1,
  TestID -> "GeodesicCount-PathGraph-unique"
]

VerificationTest[
  GeodesicCount[CycleGraph[4], 1, 3],
  2,
  TestID -> "GeodesicCount-CycleGraph4-antipodes-two"
]

VerificationTest[
  GeodesicCount[GridGraph[{3, 3}], 1, 9],
  6,
  TestID -> "GeodesicCount-GridGraph3x3-six"
]

VerificationTest[
  GeodesicCount[CompleteGraph[5], 1, 2],
  1,
  TestID -> "GeodesicCount-CompleteGraph-edge"
]

VerificationTest[
  GeodesicCount[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], 1, 3],
  0,
  TestID -> "GeodesicCount-disconnected-zero"
]

VerificationTest[
  GeodesicCount[PathGraph[Range[5]], 3, 3],
  1,
  TestID -> "GeodesicCount-self-one"
]

VerificationTest[
  (* Sanity check: GeodesicCount agrees with explicit enumeration via FindPath *)
  With[{g = GridGraph[{3, 3}], d = GraphDistance[GridGraph[{3, 3}], 1, 9]},
    GeodesicCount[g, 1, 9] === Length[FindPath[g, 1, 9, {d}, All]]
  ],
  True,
  TestID -> "GeodesicCount-matches-enumeration"
]

(* ===== DistanceMultiplicityMatrix ===== *)

VerificationTest[
  With[{dm = DistanceMultiplicityMatrix[CycleGraph[4]]},
    {Dimensions[dm[[1]]], Dimensions[dm[[2]]]}
  ],
  {{4, 4}, {4, 4}},
  TestID -> "DistanceMultiplicityMatrix-shape"
]

VerificationTest[
  With[{dm = DistanceMultiplicityMatrix[CycleGraph[4]]},
    dm[[1]] === GraphDistanceMatrix[CycleGraph[4]]
  ],
  True,
  TestID -> "DistanceMultiplicityMatrix-D-matches-GraphDistanceMatrix"
]

VerificationTest[
  With[{dm = DistanceMultiplicityMatrix[CycleGraph[4]]},
    dm[[2, 1, 3]]
  ],
  2,
  TestID -> "DistanceMultiplicityMatrix-CycleGraph4-antipode-multiplicity-two"
]

VerificationTest[
  With[{dm = DistanceMultiplicityMatrix[PathGraph[Range[5]]]},
    AllTrue[Flatten[dm[[2]]], # == 1 &]
  ],
  True,
  TestID -> "DistanceMultiplicityMatrix-PathGraph-all-unique"
]

(* ===== MedianVertices ===== *)

VerificationTest[
  Sort @ MedianVertices[PathGraph[Range[5]], {1, 5}],
  Range[5],
  TestID -> "MedianVertices-PathGraph-pair-fills-interval"
]

VerificationTest[
  Sort @ MedianVertices[PathGraph[Range[5]], {1, 4}],
  {1, 2, 3, 4},
  TestID -> "MedianVertices-PathGraph-asymmetric-pair"
]

VerificationTest[
  Sort @ MedianVertices[GridGraph[{3, 3}], {1, 3, 7}],
  {1},
  TestID -> "MedianVertices-GridGraph3x3-triple-corner"
]

VerificationTest[
  Sort @ MedianVertices[CycleGraph[6], {1, 3, 5}],
  Sort @ {1, 3, 5},
  TestID -> "MedianVertices-CycleGraph6-balanced-triple"
]

(* ===== FindGeodesicConvexHull ===== *)

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {1, 5}],
  {1, 2, 3, 4, 5},
  TestID -> "FindGeodesicConvexHull-PathGraph-endpoints"
]

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {2, 4}],
  {2, 3, 4},
  TestID -> "FindGeodesicConvexHull-PathGraph-interior"
]

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {3}],
  {3},
  TestID -> "FindGeodesicConvexHull-singleton"
]

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {}],
  {},
  TestID -> "FindGeodesicConvexHull-empty"
]

VerificationTest[
  FindGeodesicConvexHull[CycleGraph[4], {1, 3}],
  {1, 2, 3, 4},
  TestID -> "FindGeodesicConvexHull-CycleGraph4-antipodes-fill"
]

VerificationTest[
  FindGeodesicConvexHull[GridGraph[{3, 3}], {1, 9}],
  Range[9],
  TestID -> "FindGeodesicConvexHull-GridGraph3x3-corners-fill"
]

VerificationTest[
  FindGeodesicConvexHull[GridGraph[{3, 3}], {1, 3}],
  {1, 2, 3},
  TestID -> "FindGeodesicConvexHull-GridGraph3x3-row-stays-row"
]

VerificationTest[
  FindGeodesicConvexHull[CompleteGraph[5], {1, 2}],
  {1, 2},
  TestID -> "FindGeodesicConvexHull-CompleteGraph-edge-stays-edge"
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

(* ===== GeodesicallyConvexQ, Method -> "Weak" ===== *)

VerificationTest[
  GeodesicallyConvexQ[CycleGraph[6], {1, 2, 3, 4}],
  False,
  TestID -> "GeodesicallyConvexQ-CycleGraph6-arc-Strong-false"
]

VerificationTest[
  GeodesicallyConvexQ[CycleGraph[6], {1, 2, 3, 4}, Method -> "Weak"],
  True,
  TestID -> "GeodesicallyConvexQ-CycleGraph6-arc-Weak-true"
]

VerificationTest[
  GeodesicallyConvexQ[CycleGraph[4], {1, 3}, Method -> "Weak"],
  False,
  TestID -> "GeodesicallyConvexQ-CycleGraph4-antipodes-Weak-false"
]

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {2, 3, 4}, Method -> "Weak"],
  True,
  TestID -> "GeodesicallyConvexQ-PathGraph-interior-Weak-true"
]

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {1, 5}, Method -> "Weak"],
  False,
  TestID -> "GeodesicallyConvexQ-PathGraph-endpoints-Weak-false"
]

VerificationTest[
  GeodesicallyConvexQ[GridGraph[{3, 3}], {1, 2, 3}, Method -> "Weak"],
  True,
  TestID -> "GeodesicallyConvexQ-GridGraph3x3-row-Weak-true"
]

VerificationTest[
  GeodesicallyConvexQ[GridGraph[{3, 3}], {1, 9}, Method -> "Weak"],
  False,
  TestID -> "GeodesicallyConvexQ-GridGraph3x3-corners-Weak-false"
]

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {3}, Method -> "Weak"],
  True,
  TestID -> "GeodesicallyConvexQ-singleton-Weak-true"
]

VerificationTest[
  GeodesicallyConvexQ[PathGraph[Range[5]], {}, Method -> "Weak"],
  True,
  TestID -> "GeodesicallyConvexQ-empty-Weak-true"
]

VerificationTest[
  GeodesicallyConvexQ[CycleGraph[6], {1, 2, 3, 4}, Method -> "Strong"] ===
    GeodesicallyConvexQ[CycleGraph[6], {1, 2, 3, 4}],
  True,
  TestID -> "GeodesicallyConvexQ-default-is-Strong"
]

EndTestSection[]
