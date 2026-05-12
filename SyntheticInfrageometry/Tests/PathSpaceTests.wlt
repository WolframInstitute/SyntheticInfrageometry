BeginTestSection["PathSpace"]

(* ===== Sublist invariants under default n = All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectPath[ g, paths, All, "From" -> "Center" ] ]
  ],
  True,
  TestID -> "SelectPath-Center-pool-is-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectPath[ g, paths, All, "From" -> "Periphery" ] ]
  ],
  True,
  TestID -> "SelectPath-Periphery-pool-is-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, EmbeddingClosestPaths[ g, paths, { 1, 9 } ] ]
  ],
  True,
  TestID -> "EmbeddingClosestPaths-returns-sublist"
]

(* ===== Count contract: strict n, UpTo, All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectPath[ g, paths, 1, "From" -> "Center" ]
  ],
  1,
  TestID -> "SelectPath-strict-n-1"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectPath[ g, paths, UpTo[ 3 ], "From" -> "Center" ] <= 3
  ],
  True,
  TestID -> "SelectPath-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SelectPath[ g, paths, 99 ]
  ],
  $Failed,
  TestID -> "SelectPath-strict-overcount-fails"
]

VerificationTest[
  SelectPath[ GridGraph[ { 3, 3 } ], { }, 1 ],
  $Failed,
  TestID -> "SelectPath-empty-strict-fails"
]

VerificationTest[
  SelectPath[ GridGraph[ { 3, 3 } ], { }, All ],
  { },
  TestID -> "SelectPath-empty-All-empty"
]

(* ===== Default count = 1, matches FindPoint ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectPath[ g, paths ]
  ],
  1,
  TestID -> "SelectPath-default-n-is-1"
]

(* ===== Operator form ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = #[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    SubsetQ[ paths, SelectPath[ g, All, "From" -> "Center" ][ paths ] ]
  ],
  True,
  TestID -> "SelectPath-operator-form-runs"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SelectPath[ g, paths, All, "From" -> "Center", "Metric" -> "Hausdorff" ] ===
      ( SelectPath[ g, All, "From" -> "Center", "Metric" -> "Hausdorff" ][ paths ] )
  ],
  True,
  TestID -> "SelectPath-operator-form-options-agree"
]

(* ===== Wrapper passthrough ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], list = FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    MatchQ[ SelectPath[ g, list, All, "From" -> "Center" ], { InfraSegment[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectPath-preserves-unary-InfraSegment-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], list = FindCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ] },
    MatchQ[ SelectCycle[ g, list, All, "From" -> "Center" ], { InfraCircle[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectCycle-preserves-unary-InfraCircle-list"
]

(* ===== Length-1 / empty input ===== *)

VerificationTest[
  SelectPath[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, All, "From" -> "Center" ],
  { { 1, 2, 3 } },
  TestID -> "SelectPath-Center-singleton-identity"
]

VerificationTest[
  SelectPath[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, All, "From" -> "Periphery", "Metric" -> "Hausdorff" ],
  { { 1, 2, 3 } },
  TestID -> "SelectPath-Periphery-singleton-identity"
]

VerificationTest[
  EmbeddingClosestPaths[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, { 1, 3 } ],
  { { 1, 2, 3 } },
  TestID -> "EmbeddingClosestPaths-singleton-identity"
]

(* ===== SelectCycle length-based pool selectors ===== *)

VerificationTest[
  SelectCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "ShortestCircumference" ],
  { { 1, 2, 3 } },
  TestID -> "SelectCycle-ShortestCircumference-picks-min"
]

VerificationTest[
  SelectCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "LongestCircumference" ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "SelectCycle-LongestCircumference-picks-max"
]

VerificationTest[
  SelectCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 }, { 4, 5, 6 }, { 1, 2, 3, 4 } }, All,
    "From" -> "ShortestCircumference" ],
  { { 1, 2, 3 }, { 4, 5, 6 } },
  TestID -> "SelectCycle-ShortestCircumference-keeps-ties"
]

(* ===== Metric option carries through ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length[ paths ] > 1 &&
      AllTrue[ { "Hausdorff", "Frechet", "MeanFrechet" },
        m |-> SubsetQ[ paths, SelectPath[ g, paths, All, "From" -> "Center", "Metric" -> m ] ] ]
  ],
  True,
  TestID -> "SelectPath-Center-all-metrics-return-sublists"
]

(* ===== MostVisited pool ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectPath[ g, paths, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectPath-MostVisited-returns-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectPath[ g, paths, All, "From" -> "MostVisited" ] >= 1
  ],
  True,
  TestID -> "SelectPath-MostVisited-non-empty"
]

VerificationTest[
  With[ { g = PathGraph[ Range @ 5 ], wrapped = FindSegment[ PathGraph[ Range @ 5 ], 1, 5, All ] },
    SelectPath[ g, wrapped, All, "From" -> "MostVisited" ] === wrapped
  ],
  True,
  TestID -> "SelectPath-MostVisited-unique-segment-passthrough"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], cycles = (#[[ 1, 1 ]] & /@ FindCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ]) },
    SubsetQ[ cycles, SelectCycle[ g, cycles, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectCycle-MostVisited-returns-sublist"
]

(* ===== Distance constraint: Max k-clique in path-space ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ]) },
    Length @ SelectPath[ g, paths, 2, "Distance" -> "Max" ]
  ],
  2,
  TestID -> "SelectPath-Distance-Max-strict-2"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ]) },
    SubsetQ[ paths, SelectPath[ g, paths, UpTo[ 3 ], "Distance" -> "Max" ] ]
  ],
  True,
  TestID -> "SelectPath-Distance-Max-UpTo-3-sublist"
]

(* ===== "From" anchor -> spec ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ]) },
    With[ { ref = First @ paths,
            others = SelectPath[ g, paths, All, "From" -> ( First @ paths -> "Max" ) ] },
      SubsetQ[ paths, others ]
    ]
  ],
  True,
  TestID -> "SelectPath-From-anchor-Max-returns-sublist"
]

(* ===== Empty pool returns empty ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ SelectPath[ g, InfraSegment[ { } ], All ]
  ],
  InfraSegment,
  TestID -> "SelectPath-empty-wrapper-All-passthrough"
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
