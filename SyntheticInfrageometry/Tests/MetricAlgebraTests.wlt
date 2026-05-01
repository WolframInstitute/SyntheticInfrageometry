BeginTestSection["MetricAlgebra"]

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

(* ===== DistanceMatrixQ ===== *)

VerificationTest[
  DistanceMatrixQ[GraphDistanceMatrix[CycleGraph[6]]],
  True,
  TestID -> "DistanceMatrixQ-CycleGraph-true"
]

VerificationTest[
  DistanceMatrixQ[GraphDistanceMatrix[GridGraph[{3, 3}]]],
  True,
  TestID -> "DistanceMatrixQ-GridGraph-true"
]

VerificationTest[
  DistanceMatrixQ[{{0, 1, 5}, {1, 0, 1}, {5, 1, 0}}],
  False,
  TestID -> "DistanceMatrixQ-violates-triangle-inequality"
]

VerificationTest[
  DistanceMatrixQ[{{0, 1}, {1, 0}}],
  True,
  TestID -> "DistanceMatrixQ-K2-true"
]

VerificationTest[
  DistanceMatrixQ[{{1, 0}, {0, 1}}],
  False,
  TestID -> "DistanceMatrixQ-nonzero-diagonal-false"
]

VerificationTest[
  DistanceMatrixQ[{{0, 1}, {2, 0}}],
  False,
  TestID -> "DistanceMatrixQ-asymmetric-false"
]

VerificationTest[
  DistanceMatrixQ["not-a-matrix"],
  False,
  TestID -> "DistanceMatrixQ-non-matrix-false"
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

EndTestSection[]
