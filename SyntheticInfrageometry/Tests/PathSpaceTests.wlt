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

(* ===== GeodesicSubgraph ===== *)

VerificationTest[
  GraphQ @ GeodesicSubgraph[ PathGraph[ Range[ 5 ] ], { { 1, 5 } } ],
  True,
  TestID -> "GeodesicSubgraph-returns-graph"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    EdgeCount @ GeodesicSubgraph[ g, { { 1, 9 } }, "PathThickness" -> Infinity ] >
    EdgeCount @ GeodesicSubgraph[ g, { { 1, 9 } }, "PathThickness" -> 0 ]
  ],
  True,
  TestID -> "GeodesicSubgraph-thickness-grows"
]

VerificationTest[
  DirectedGraphQ @ GeodesicSubgraph[ CycleGraph[ 6 ], { { 1, 4 } }, "Directed" -> False ],
  False,
  TestID -> "GeodesicSubgraph-undirected"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    SubsetQ[ VertexList[ g ], VertexList @ GeodesicSubgraph[ g, { { 1, 9 }, { 3, 7 } } ] ]
  ],
  True,
  TestID -> "GeodesicSubgraph-multi-pair"
]

(* ===== PathSubgraph ===== *)

VerificationTest[
  VertexCount @ PathSubgraph[ PathGraph[ Range[ 6 ] ], 1, 5 ],
  5,
  TestID -> "PathSubgraph-default-is-geodesic"
]

VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    EdgeCount @ PathSubgraph[ g, 1, 3, UpTo[ 2 ] ] <
    EdgeCount @ PathSubgraph[ g, 1, 3, UpTo[ 4 ] ]
  ],
  True,
  TestID -> "PathSubgraph-length-cap-monotone"
]

VerificationTest[
  EdgeCount @ PathSubgraph[ CycleGraph[ 5 ], 1, 3, All ],
  5,
  TestID -> "PathSubgraph-all-on-2-connected"
]

VerificationTest[
  With[ { g = GraphDisjointUnion[ PathGraph[ { 1, 2 } ], PathGraph[ { 3, 4 } ] ] },
    EdgeCount @ PathSubgraph[ g, 1, 4 ]
  ],
  0,
  TestID -> "PathSubgraph-disconnected-empty"
]

VerificationTest[
  VertexList @ PathSubgraph[ PathGraph[ Range[ 4 ] ], 2, 2 ],
  { 2 },
  TestID -> "PathSubgraph-self-loop"
]

VerificationTest[
  DirectedGraphQ @ PathSubgraph[ CycleGraph[ 6 ], 1, 4, All, "Directed" -> False ],
  False,
  TestID -> "PathSubgraph-undirected"
]

VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    EdgeCount @ PathSubgraph[ g, 1, 4, 3 ] === EdgeCount @ PathSubgraph[ g, 1, 4, UpTo[ 3 ] ]
  ],
  True,
  TestID -> "PathSubgraph-integer-equals-UpTo"
]

EndTestSection[]
