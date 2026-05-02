BeginTestSection["PathSpace"]

(* ===== Sublist invariants ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    SubsetQ[paths, CentralPaths[g, paths]]
  ],
  True,
  TestID -> "CentralPaths-returns-sublist"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    SubsetQ[paths, PeripheralPaths[g, paths]]
  ],
  True,
  TestID -> "PeripheralPaths-returns-sublist"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    SubsetQ[paths, EmbeddingClosestPaths[g, paths, {1, 9}]]
  ],
  True,
  TestID -> "EmbeddingClosestPaths-returns-sublist"
]

(* ===== Operator form agrees with full form ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    CentralPaths[g, paths] === CentralPaths[g][paths]
  ],
  True,
  TestID -> "CentralPaths-operator-form-agrees"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    CentralPaths[g, paths, Method -> "Hausdorff"] ===
    CentralPaths[g, Method -> "Hausdorff"][paths]
  ],
  True,
  TestID -> "CentralPaths-operator-form-method-agrees"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    EmbeddingClosestPaths[g, paths, {1, 9}] === EmbeddingClosestPaths[g, {1, 9}][paths]
  ],
  True,
  TestID -> "EmbeddingClosestPaths-operator-form-agrees"
]

(* ===== Length-1 input is identity ===== *)

VerificationTest[
  CentralPaths[GridGraph[{3, 3}], {{1, 2, 3}}],
  {{1, 2, 3}},
  TestID -> "CentralPaths-singleton-identity"
]

VerificationTest[
  PeripheralPaths[GridGraph[{3, 3}], {{1, 2, 3}}, Method -> "Hausdorff"],
  {{1, 2, 3}},
  TestID -> "PeripheralPaths-singleton-identity"
]

VerificationTest[
  EmbeddingClosestPaths[GridGraph[{3, 3}], {{1, 2, 3}}, {1, 3}],
  {{1, 2, 3}},
  TestID -> "EmbeddingClosestPaths-singleton-identity"
]

VerificationTest[
  CentralPaths[GridGraph[{3, 3}], {}],
  {},
  TestID -> "CentralPaths-empty-identity"
]

VerificationTest[
  ShortestCircumferenceCycles[{}],
  {},
  TestID -> "ShortestCircumferenceCycles-empty-identity"
]

(* ===== Length filters ===== *)

VerificationTest[
  ShortestCircumferenceCycles[{{1, 2, 3, 4, 5}, {1, 2, 3}, {1, 2, 3, 4}}],
  {{1, 2, 3}},
  TestID -> "ShortestCircumferenceCycles-picks-min"
]

VerificationTest[
  LongestCircumferenceCycles[{{1, 2, 3, 4, 5}, {1, 2, 3}, {1, 2, 3, 4}}],
  {{1, 2, 3, 4, 5}},
  TestID -> "LongestCircumferenceCycles-picks-max"
]

VerificationTest[
  ShortestCircumferenceCycles[{{1, 2, 3}, {4, 5, 6}, {1, 2, 3, 4}}],
  {{1, 2, 3}, {4, 5, 6}},
  TestID -> "ShortestCircumferenceCycles-keeps-ties"
]

(* ===== Method options behave ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    Length[paths] > 1 &&
    AllTrue[{"Frechet", "Hausdorff", "MeanFrechet"},
      m |-> SubsetQ[paths, CentralPaths[g, paths, Method -> m]]]
  ],
  True,
  TestID -> "CentralPaths-all-methods-return-sublists"
]

(* ===== Cycle vs path distinction ===== *)

VerificationTest[
  With[{g = CycleGraph[6], cycles = FindCircle[CycleGraph[6], 1, {1, 2}, All]},
    SubsetQ[cycles, CentralCycles[g, cycles]]
  ],
  True,
  TestID -> "CentralCycles-returns-sublist"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], cycles = FindCircle[GridGraph[{4, 4}], 6, {1, 2}, All]},
    cycles =!= {} && SubsetQ[cycles, EmbeddingClosestCycles[g, cycles, {6, 1.5}]]
  ],
  True,
  TestID -> "EmbeddingClosestCycles-returns-sublist"
]

EndTestSection[]
