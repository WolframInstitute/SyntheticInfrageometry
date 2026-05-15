BeginTestSection["EuclideanPredicates"]

(* ===== InfraPathQ ===== *)

VerificationTest[
  InfraPathQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  True,
  TestID -> "InfraPathQ-simple-path"
]

VerificationTest[
  InfraPathQ[PathGraph[Range[5]], {1, 3}],
  False,
  TestID -> "InfraPathQ-non-edge"
]

VerificationTest[
  InfraPathQ[CycleGraph[5], {1, 2, 3, 4, 5, 1}],
  False,
  TestID -> "InfraPathQ-vertex-repeat"
]

VerificationTest[
  InfraPathQ[GridGraph[{3, 3}], {1, 2, 5, 4}],
  True,
  TestID -> "InfraPathQ-non-geodesic-simple"
]

VerificationTest[
  InfraSegmentQ[GridGraph[{3, 3}], {1, 2, 5, 4}],
  False,
  TestID -> "InfraPathQ-non-geodesic-InfraSegmentQ-false"
]

VerificationTest[
  InfraPathQ[PathGraph[Range[5]], {3}],
  False,
  TestID -> "InfraPathQ-single-vertex"
]

(* ===== InfraSegmentQ ===== *)

VerificationTest[
  InfraSegmentQ[PathGraph[Range[5]], {1, 2, 3}],
  True,
  TestID -> "InfraSegmentQ-valid-geodesic"
]

VerificationTest[
  InfraSegmentQ[PathGraph[Range[5]], {1, 3, 5}],
  False,
  TestID -> "InfraSegmentQ-non-adjacent-vertices"
]

VerificationTest[
  InfraSegmentQ[GridGraph[{3, 3}], {1, 4, 7}],
  True,
  TestID -> "InfraSegmentQ-GridGraph-geodesic"
]

VerificationTest[
  InfraSegmentQ[GridGraph[{3, 3}], {1, 4, 5, 2, 3}],
  False,
  TestID -> "InfraSegmentQ-not-shortest-path"
]

VerificationTest[
  InfraSegmentQ[PathGraph[Range[5]], {3}],
  False,
  TestID -> "InfraSegmentQ-single-vertex"
]

(* ===== InfraLineQ ===== *)

VerificationTest[
  InfraLineQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  True,
  TestID -> "InfraLineQ-maximal-geodesic"
]

VerificationTest[
  InfraLineQ[PathGraph[Range[5]], {2, 3, 4}],
  False,
  TestID -> "InfraLineQ-extendable-segment"
]

(* ===== InfraShellQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    InfraShellQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "InfraShellQ-valid-shell"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    InfraShellQ[g, {1, 4}]
  ],
  False,
  TestID -> "InfraShellQ-non-symmetric-pair-not-shell"
]

(* ===== FindInfraShellParameters ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    Length[FindInfraShellParameters[g, {2, 5, 10, 9, 8, 7}]] >= 1
  ],
  True,
  TestID -> "FindInfraShellParameters-finds-center"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{params = FindInfraShellParameters[g, {2, 5, 10, 9, 8, 7}]},
      AllTrue[params, MatchQ[{_, _Integer}]]
    ]
  ],
  True,
  TestID -> "FindInfraShellParameters-returns-pairs"
]

(* ===== InfraCircleQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    InfraCircleQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "InfraCircleQ-valid-cycle"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    InfraCircleQ[g, {2, 5, 10, 9, 8, 7, 2}]
  ],
  True,
  TestID -> "InfraCircleQ-accepts-closed-input"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    InfraCircleQ[g, {1, 2, 3}]
  ],
  False,
  TestID -> "InfraCircleQ-no-wrap-around-edge"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    InfraCircleQ[g, {1, 2}]
  ],
  False,
  TestID -> "InfraCircleQ-too-short"
]

(* ===== InfraParallelQ ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    InfraParallelQ[g, {1, 2, 3, 4}, {13, 14, 15, 16}]
  ],
  True,
  TestID -> "InfraParallelQ-GridGraph-parallel-rows"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    InfraParallelQ[g, {1, 2, 3, 4}, {1, 5, 9, 13}]
  ],
  False,
  TestID -> "InfraParallelQ-GridGraph-intersecting"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[GridGraph[{4, 4}]]},
    InfraParallelQ[d, {1, 2, 3, 4}, {13, 14, 15, 16}]
  ],
  True,
  TestID -> "InfraParallelQ-matrix-form"
]

(* ===== SeparatesQ ===== *)

VerificationTest[
  SeparatesQ[PathGraph[Range[5]], {3}, 1, 5],
  True,
  TestID -> "SeparatesQ-path-center"
]

VerificationTest[
  SeparatesQ[PathGraph[Range[5]], {2}, 4, 5],
  False,
  TestID -> "SeparatesQ-path-non-separating"
]

VerificationTest[
  SeparatesQ[PathGraph[Range[5]], {1}, 1, 5],
  False,
  TestID -> "SeparatesQ-endpoint-deletion-false"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    AllTrue[(#[[ 1, 1 ]] & /@ FindInfraBisectingHyperplane[g, 1, 5, All]), h |-> SeparatesQ[g, h, 1, 5]]
  ],
  True,
  TestID -> "SeparatesQ-bisecting-hyperplane-path"
]

(* ===== UniqueInfraSegmentQ ===== *)

VerificationTest[
  UniqueInfraSegmentQ[PathGraph[Range[5]], 1, 5],
  True,
  TestID -> "UniqueInfraSegmentQ-PathGraph-pair-true"
]

VerificationTest[
  UniqueInfraSegmentQ[CycleGraph[4], 1, 3],
  False,
  TestID -> "UniqueInfraSegmentQ-CycleGraph4-antipodes-false"
]

VerificationTest[
  UniqueInfraSegmentQ[PathGraph[Range[5]]],
  True,
  TestID -> "UniqueInfraSegmentQ-PathGraph-whole-true"
]

VerificationTest[
  UniqueInfraSegmentQ[CycleGraph[4]],
  False,
  TestID -> "UniqueInfraSegmentQ-CycleGraph4-whole-false"
]

VerificationTest[
  UniqueInfraSegmentQ[CompleteGraph[5]],
  True,
  TestID -> "UniqueInfraSegmentQ-CompleteGraph-whole-true"
]

EndTestSection[]
