BeginTestSection["Predicates"]

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

(* ===== CircleQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    CircleQ[g, {2, 5, 10, 9, 8, 7}, 1, 2]
  ],
  True,
  TestID -> "CircleQ-valid-circle"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    CircleQ[g, {2, 5, 10, 9, 8, 7}, 1, 3]
  ],
  False,
  TestID -> "CircleQ-wrong-radius"
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

(* ===== IntersectQ ===== *)

VerificationTest[
  IntersectQ[{1, 2, 3}, {3, 4, 5}],
  True,
  TestID -> "IntersectQ-overlapping"
]

VerificationTest[
  IntersectQ[{1, 2}, {3, 4}],
  False,
  TestID -> "IntersectQ-disjoint"
]

VerificationTest[
  IntersectQ[{}, {1, 2}],
  False,
  TestID -> "IntersectQ-empty-set"
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

(* ===== SegmentLineAngle ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SegmentLineAngle[g, {1, 2, 3}, {1, 2, 3, 4, 5}]
  ],
  0,
  TestID -> "SegmentLineAngle-segment-on-line"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SegmentLineAngle[g, 1, 3, {1, 2, 3}]
  ],
  0,
  TestID -> "SegmentLineAngle-endpoint-on-line"
]

EndTestSection[]
