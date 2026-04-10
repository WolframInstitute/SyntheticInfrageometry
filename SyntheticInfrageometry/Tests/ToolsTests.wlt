BeginTestSection["Tools"]

(* ===== HausdorffDistance ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    HausdorffDistance[g, {1, 2}, {4, 5}]
  ],
  3,
  TestID -> "HausdorffDistance-PathGraph-disjoint-sets"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    HausdorffDistance[g, {1, 5}, {3}]
  ],
  2,
  TestID -> "HausdorffDistance-PathGraph-set-to-point"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    HausdorffDistance[g, {1, 2}, {1, 2}]
  ],
  0,
  TestID -> "HausdorffDistance-identical-sets"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    HausdorffDistance[g, {1, 2}, {4, 5}] == HausdorffDistance[g, {4, 5}, {1, 2}]
  ],
  True,
  TestID -> "HausdorffDistance-symmetry"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[PathGraph[Range[5]]]},
    HausdorffDistance[d, {1, 2}, {4, 5}]
  ],
  3,
  TestID -> "HausdorffDistance-matrix-form"
]

(* ===== FrechetDistance ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FrechetDistance[g, {1, 2, 3}, {1, 2, 3}]
  ],
  0,
  TestID -> "FrechetDistance-identical-paths"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FrechetDistance[g, {1, 2, 3}, {3, 4, 5}]
  ],
  2,
  TestID -> "FrechetDistance-shifted-paths"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FrechetDistance[g, {1, 2, 3}, {3, 4, 5}, Mean]
  ],
  2,
  TestID -> "FrechetDistance-with-Mean"
]

(* ===== MinimalSeparationDistance ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    MinimalSeparationDistance[g, {1, 2}, {4, 5}]
  ],
  2,
  TestID -> "MinimalSeparationDistance-PathGraph-disjoint"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    MinimalSeparationDistance[g, {1, 3}, {3, 5}]
  ],
  0,
  TestID -> "MinimalSeparationDistance-overlapping-sets"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[PathGraph[Range[5]]]},
    MinimalSeparationDistance[d, {1}, {5}]
  ],
  4,
  TestID -> "MinimalSeparationDistance-matrix-form"
]

(* ===== CentralElement ===== *)

VerificationTest[
  With[{d = GraphDistanceMatrix[PathGraph[Range[5]]]},
    CentralElement[d, 1]
  ],
  {3},
  TestID -> "CentralElement-PathGraph-center"
]

(* ===== PeripheralElement ===== *)

VerificationTest[
  With[{d = GraphDistanceMatrix[PathGraph[Range[5]]]},
    Sort @ PeripheralElement[d, 1]
  ],
  {1},
  TestID -> "PeripheralElement-PathGraph-endpoint"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[PathGraph[Range[5]]]},
    Sort @ PeripheralElement[d, 2]
  ],
  {1, 5},
  TestID -> "PeripheralElement-PathGraph-both-endpoints"
]

(* ===== applySelect ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{paths = FindPath[g, 1, 9, {4}, All]},
      With[{result = applySelect[g, paths, "LongestCircumference", <|"Cyclic" -> True|>]},
        AllTrue[result, Length[#] == Max[Length /@ paths] &]
      ]
    ]
  ],
  True,
  TestID -> "applySelect-LongestCircumference"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{paths = FindPath[g, 1, 9, {4}, All]},
      With[{result = applySelect[g, paths, "ShortestCircumference", <|"Cyclic" -> True|>]},
        AllTrue[result, Length[#] == Min[Length /@ paths] &]
      ]
    ]
  ],
  True,
  TestID -> "applySelect-ShortestCircumference"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{paths = FindPath[g, 1, 9, {4}, All]},
      applySelect[g, paths, None, <|"Cyclic" -> False|>] === paths
    ]
  ],
  True,
  TestID -> "applySelect-None-identity"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{paths = FindPath[g, 1, 9, {4}, All]},
      With[{result = applySelect[g, paths, "FrechetCentral", <|"Cyclic" -> False|>]},
        Length[result] >= 1 && SubsetQ[paths, result]
      ]
    ]
  ],
  True,
  TestID -> "applySelect-FrechetCentral"
]

(* ===== SegmentEndpoints ===== *)

VerificationTest[
  SegmentEndpoints[{1, 2, 3, 4, 5}],
  {1, 5},
  TestID -> "SegmentEndpoints-basic"
]

VerificationTest[
  SegmentEndpoints[{3, 7}],
  {3, 7},
  TestID -> "SegmentEndpoints-two-vertices"
]

EndTestSection[]
