BeginTestSection["EuclideanPredicates"]

(* ===== SegmentQ ===== *)

VerificationTest[
  SegmentQ[PathGraph[Range[5]], {1, 2, 3}],
  True,
  TestID -> "SegmentQ-valid-geodesic"
]

VerificationTest[
  SegmentQ[PathGraph[Range[5]], {1, 3, 5}],
  False,
  TestID -> "SegmentQ-non-adjacent-vertices"
]

VerificationTest[
  SegmentQ[GridGraph[{3, 3}], {1, 4, 7}],
  True,
  TestID -> "SegmentQ-GridGraph-geodesic"
]

VerificationTest[
  SegmentQ[GridGraph[{3, 3}], {1, 4, 5, 2, 3}],
  False,
  TestID -> "SegmentQ-not-shortest-path"
]

VerificationTest[
  SegmentQ[PathGraph[Range[5]], {3}],
  False,
  TestID -> "SegmentQ-single-vertex"
]

(* ===== LineQ ===== *)

VerificationTest[
  LineQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  True,
  TestID -> "LineQ-maximal-geodesic"
]

VerificationTest[
  LineQ[PathGraph[Range[5]], {2, 3, 4}],
  False,
  TestID -> "LineQ-extendable-segment"
]

(* ===== ShellQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    ShellQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "ShellQ-valid-shell"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    ShellQ[g, {1, 4}]
  ],
  False,
  TestID -> "ShellQ-non-symmetric-pair-not-shell"
]

(* ===== FindShellParameters ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    Length[FindShellParameters[g, {2, 5, 10, 9, 8, 7}]] >= 1
  ],
  True,
  TestID -> "FindShellParameters-finds-center"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{params = FindShellParameters[g, {2, 5, 10, 9, 8, 7}]},
      AllTrue[params, MatchQ[{_, _Integer}]]
    ]
  ],
  True,
  TestID -> "FindShellParameters-returns-pairs"
]

(* ===== CircleQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    CircleQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "CircleQ-valid-cycle"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    CircleQ[g, {2, 5, 10, 9, 8, 7, 2}]
  ],
  True,
  TestID -> "CircleQ-accepts-closed-input"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    CircleQ[g, {1, 2, 3}]
  ],
  False,
  TestID -> "CircleQ-no-wrap-around-edge"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    CircleQ[g, {1, 2}]
  ],
  False,
  TestID -> "CircleQ-too-short"
]

(* ===== ParallelQ ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    ParallelQ[g, {1, 2, 3, 4}, {13, 14, 15, 16}]
  ],
  True,
  TestID -> "ParallelQ-GridGraph-parallel-rows"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    ParallelQ[g, {1, 2, 3, 4}, {1, 5, 9, 13}]
  ],
  False,
  TestID -> "ParallelQ-GridGraph-intersecting"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[GridGraph[{4, 4}]]},
    ParallelQ[d, {1, 2, 3, 4}, {13, 14, 15, 16}]
  ],
  True,
  TestID -> "ParallelQ-matrix-form"
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
    AllTrue[FindBisectingHyperplane[g, 1, 5, All], h |-> SeparatesQ[g, h, 1, 5]]
  ],
  True,
  TestID -> "SeparatesQ-bisecting-hyperplane-path"
]

(* ===== UniqueSegmentQ ===== *)

VerificationTest[
  UniqueSegmentQ[PathGraph[Range[5]], 1, 5],
  True,
  TestID -> "UniqueSegmentQ-PathGraph-pair-true"
]

VerificationTest[
  UniqueSegmentQ[CycleGraph[4], 1, 3],
  False,
  TestID -> "UniqueSegmentQ-CycleGraph4-antipodes-false"
]

VerificationTest[
  UniqueSegmentQ[PathGraph[Range[5]]],
  True,
  TestID -> "UniqueSegmentQ-PathGraph-whole-true"
]

VerificationTest[
  UniqueSegmentQ[CycleGraph[4]],
  False,
  TestID -> "UniqueSegmentQ-CycleGraph4-whole-false"
]

VerificationTest[
  UniqueSegmentQ[CompleteGraph[5]],
  True,
  TestID -> "UniqueSegmentQ-CompleteGraph-whole-true"
]

EndTestSection[]
