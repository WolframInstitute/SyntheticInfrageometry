BeginTestSection["Constructions"]

(* ===== MetricMidpoint ===== *)

VerificationTest[
  MetricMidpoint[{1, 2, 3, 4, 5}],
  3,
  TestID -> "MetricMidpoint-odd-length"
]

VerificationTest[
  MetricMidpoint[{1, 2, 3, 4}],
  2,
  TestID -> "MetricMidpoint-even-length"
]

VerificationTest[
  MetricMidpoint[{10, 20}],
  10,
  TestID -> "MetricMidpoint-two-elements"
]

(* ===== MetricPerpendicular ===== *)

VerificationTest[
  With[{g = CycleGraph[5]},
    MetricPerpendicular[g, {1, 2, 3, 4}, 5, All]
  ],
  {2},
  TestID -> "MetricPerpendicular-CycleGraph5"
]

VerificationTest[
  With[{g = CycleGraph[5]},
    With[{feet = MetricPerpendicular[g, {1, 2, 3, 4}, 5, All]},
      AllTrue[feet, MemberQ[{1, 2, 3, 4}, #] &]
    ]
  ],
  True,
  TestID -> "MetricPerpendicular-feet-on-line"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[CycleGraph[5]]},
    MetricPerpendicular[d, {1, 2, 3, 4}, 5, All]
  ],
  {2},
  TestID -> "MetricPerpendicular-matrix-form"
]

(* ===== CompleteEquilateralTriangle ===== *)

VerificationTest[
  With[{g = CycleGraph[6]},
    With[{tri = CompleteEquilateralTriangle[g, 1, 3, All]},
      AllTrue[tri, v |->
        GraphDistance[g, 1, v] == GraphDistance[g, 1, 3] &&
        GraphDistance[g, 3, v] == GraphDistance[g, 1, 3]
      ]
    ]
  ],
  True,
  TestID -> "CompleteEquilateralTriangle-equidistant"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Length[CompleteEquilateralTriangle[g, 1, 3, All]] >= 1
  ],
  True,
  TestID -> "CompleteEquilateralTriangle-exists"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[CycleGraph[6]]},
    With[{tri = CompleteEquilateralTriangle[d, 1, 3, All]},
      AllTrue[tri, v |-> d[[1, v]] == d[[1, 3]] && d[[3, v]] == d[[1, 3]]]
    ]
  ],
  True,
  TestID -> "CompleteEquilateralTriangle-matrix-form"
]

(* ===== MetricParallel ===== *)

VerificationTest[
  With[{g = CycleGraph[7]},
    ListQ @ MetricParallel[g, {1, 2, 3, 4, 5}, 7, All]
  ],
  True,
  TestID -> "MetricParallel-returns-list"
]

VerificationTest[
  With[{g = CycleGraph[7]},
    With[{parallels = MetricParallel[g, {1, 2, 3, 4, 5}, 7, All]},
      AllTrue[parallels, p |-> !IntersectQ[p, {1, 2, 3, 4, 5}]]
    ]
  ],
  True,
  TestID -> "MetricParallel-non-intersecting-if-found"
]

VerificationTest[
  ListQ @ MetricParallel[CycleGraph[5], {}, 1, All],
  True,
  TestID -> "MetricParallel-empty-line"
]

EndTestSection[]
