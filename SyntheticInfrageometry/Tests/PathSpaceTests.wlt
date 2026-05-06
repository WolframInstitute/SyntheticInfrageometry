BeginTestSection["PathSpace"]

(* ===== Sublist invariants ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]["Realisations"]},
    SubsetQ[paths, SelectPaths[g, paths, "Central"]]
  ],
  True,
  TestID -> "SelectPaths-Central-returns-sublist"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]["Realisations"]},
    SubsetQ[paths, SelectPaths[g, paths, "Peripheral"]]
  ],
  True,
  TestID -> "SelectPaths-Peripheral-returns-sublist"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]["Realisations"]},
    SubsetQ[paths, EmbeddingClosestPaths[g, paths, {1, 9}]]
  ],
  True,
  TestID -> "EmbeddingClosestPaths-returns-sublist"
]

(* ===== Operator form agrees with full form ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    SelectPaths[g, paths, "Central"] === SelectPaths[g, "Central"][paths]
  ],
  True,
  TestID -> "SelectPaths-operator-form-agrees"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    SelectPaths[g, paths, "Central", Method -> "Hausdorff"] ===
    SelectPaths[g, "Central", Method -> "Hausdorff"][paths]
  ],
  True,
  TestID -> "SelectPaths-operator-form-method-agrees"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    EmbeddingClosestPaths[g, paths, {1, 9}] === EmbeddingClosestPaths[g, {1, 9}][paths]
  ],
  True,
  TestID -> "EmbeddingClosestPaths-operator-form-agrees"
]

(* ===== Wrapper passthrough ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], wrapped = FindSegment[GridGraph[{3, 3}], 1, 9, All]},
    Head @ SelectPaths[g, wrapped, "Central"]
  ],
  InfraSegment,
  TestID -> "SelectPaths-preserves-InfraSegment-wrapper"
]

VerificationTest[
  With[{g = CycleGraph[6], wrapped = FindCircle[CycleGraph[6], 1, {1, 2}, All]},
    Head @ SelectCycles[g, wrapped, "Central"]
  ],
  InfraCircle,
  TestID -> "SelectCycles-preserves-InfraCircle-wrapper"
]

(* ===== Chained criteria ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}], cycles = FindCircle[GridGraph[{4, 4}], 6, {1, 2}, All]["Realisations"]},
    SelectCycles[g, cycles, {"ShortestCircumference", "Central"}] ===
    SelectCycles[g, SelectCycles[g, cycles, "ShortestCircumference"], "Central"]
  ],
  True,
  TestID -> "SelectCycles-chained-criteria-fold"
]

(* ===== Length-1 input is identity ===== *)

VerificationTest[
  SelectPaths[GridGraph[{3, 3}], {{1, 2, 3}}, "Central"],
  {{1, 2, 3}},
  TestID -> "SelectPaths-Central-singleton-identity"
]

VerificationTest[
  SelectPaths[GridGraph[{3, 3}], {{1, 2, 3}}, "Peripheral", Method -> "Hausdorff"],
  {{1, 2, 3}},
  TestID -> "SelectPaths-Peripheral-singleton-identity"
]

VerificationTest[
  EmbeddingClosestPaths[GridGraph[{3, 3}], {{1, 2, 3}}, {1, 3}],
  {{1, 2, 3}},
  TestID -> "EmbeddingClosestPaths-singleton-identity"
]

VerificationTest[
  SelectPaths[GridGraph[{3, 3}], {}, "Central"],
  {},
  TestID -> "SelectPaths-Central-empty-identity"
]

VerificationTest[
  SelectCycles[GridGraph[{3, 3}], {}, "ShortestCircumference"],
  {},
  TestID -> "SelectCycles-ShortestCircumference-empty-identity"
]

(* ===== Length filters ===== *)

VerificationTest[
  SelectCycles[GridGraph[{3, 3}], {{1, 2, 3, 4, 5}, {1, 2, 3}, {1, 2, 3, 4}}, "ShortestCircumference"],
  {{1, 2, 3}},
  TestID -> "SelectCycles-ShortestCircumference-picks-min"
]

VerificationTest[
  SelectCycles[GridGraph[{3, 3}], {{1, 2, 3, 4, 5}, {1, 2, 3}, {1, 2, 3, 4}}, "LongestCircumference"],
  {{1, 2, 3, 4, 5}},
  TestID -> "SelectCycles-LongestCircumference-picks-max"
]

VerificationTest[
  SelectCycles[GridGraph[{3, 3}], {{1, 2, 3}, {4, 5, 6}, {1, 2, 3, 4}}, "ShortestCircumference"],
  {{1, 2, 3}, {4, 5, 6}},
  TestID -> "SelectCycles-ShortestCircumference-keeps-ties"
]

(* ===== Method options behave ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], paths = FindSegment[GridGraph[{3, 3}], 1, 9, All]["Realisations"]},
    Length[paths] > 1 &&
    AllTrue[{"Frechet", "Hausdorff", "MeanFrechet"},
      m |-> SubsetQ[paths, SelectPaths[g, paths, "Central", Method -> m]]]
  ],
  True,
  TestID -> "SelectPaths-Central-all-methods-return-sublists"
]

(* ===== Cycle vs path distinction ===== *)

VerificationTest[
  With[{g = CycleGraph[6], cycles = FindCircle[CycleGraph[6], 1, {1, 2}, All]},
    SubsetQ[cycles, SelectCycles[g, cycles, "Central"]]
  ],
  True,
  TestID -> "SelectCycles-Central-returns-sublist"
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

(* ===== "MostVisited" criterion ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realisations" ] },
    SubsetQ[ paths, SelectPaths[ g, paths, "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectPaths-MostVisited-returns-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realisations" ] },
    Length[ SelectPaths[ g, paths, "MostVisited" ] ] >= 1 &&
      Length[ SelectPaths[ g, paths, "MostVisited" ] ] <= Length[ paths ]
  ],
  True,
  TestID -> "SelectPaths-MostVisited-non-empty"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], wrapped = FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    Head @ SelectPaths[ g, wrapped, "MostVisited" ]
  ],
  InfraSegment,
  TestID -> "SelectPaths-MostVisited-preserves-wrapper"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], wrapped = FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    SelectPaths[ g, wrapped, "MostVisited" ] === SelectPaths[ g, "MostVisited" ][ wrapped ]
  ],
  True,
  TestID -> "SelectPaths-MostVisited-operator-form-agrees"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], cycles = FindCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ][ "Realisations" ] },
    SubsetQ[ cycles, SelectCycles[ g, cycles, "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectCycles-MostVisited-returns-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ SelectPaths[ g, InfraSegment[ { } ], "MostVisited" ]
  ],
  InfraSegment,
  TestID -> "SelectPaths-empty-wrapper-passthrough"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], reps = FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realisations" ] },
    SelectPaths[ g, InfraSegment[ { First @ reps } ], "MostVisited" ] === InfraSegment[ { First @ reps } ]
  ],
  True,
  TestID -> "SelectPaths-singleton-wrapper-passthrough"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], wrapped = FindCircle[ GridGraph[ { 3, 3 } ], 2, { 1, 2 }, All ] },
    Head @ SelectCycles[ g, wrapped, "MostVisited" ]
  ],
  InfraCircle,
  TestID -> "SelectCycles-MostVisited-preserves-wrapper"
]

VerificationTest[
  With[ { g = PathGraph[ Range @ 5 ], wrapped = FindSegment[ PathGraph[ Range @ 5 ], 1, 5, All ] },
    SelectPaths[ g, wrapped, "MostVisited" ] === wrapped
  ],
  True,
  TestID -> "SelectPaths-unique-segment-passthrough"
]

EndTestSection[]
