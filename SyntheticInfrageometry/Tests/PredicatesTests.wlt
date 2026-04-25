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

(* ===== SphereQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    SphereQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "SphereQ-valid-cycle"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SphereQ[g, {1, 4}]
  ],
  False,
  TestID -> "SphereQ-non-symmetric-pair-not-sphere"
]

(* ===== FindSphereParameters ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    Length[FindSphereParameters[g, {2, 5, 10, 9, 8, 7}]] >= 1
  ],
  True,
  TestID -> "FindSphereParameters-finds-center"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{params = FindSphereParameters[g, {2, 5, 10, 9, 8, 7}]},
      AllTrue[params, MatchQ[{_, _Integer}]]
    ]
  ],
  True,
  TestID -> "FindSphereParameters-returns-pairs"
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

EndTestSection[]
