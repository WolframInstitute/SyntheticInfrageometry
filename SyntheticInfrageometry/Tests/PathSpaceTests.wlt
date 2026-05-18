BeginTestSection["PathSpace"]

(* ===== Sublist invariants under default n = All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "Center" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Center-pool-is-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "Periphery" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Periphery-pool-is-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, EmbeddingClosest[ g, paths, { 1, 9 } ] ]
  ],
  True,
  TestID -> "EmbeddingClosest-returns-sublist"
]

(* ===== Count contract: strict n, UpTo, All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectInfraPath[ g, paths, 1, "From" -> "Center" ]
  ],
  1,
  TestID -> "SelectInfraPath-strict-n-1"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectInfraPath[ g, paths, UpTo[ 3 ], "From" -> "Center" ] <= 3
  ],
  True,
  TestID -> "SelectInfraPath-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SelectInfraPath[ g, paths, 99 ]
  ],
  $Failed,
  TestID -> "SelectInfraPath-strict-overcount-fails"
]

VerificationTest[
  SelectInfraPath[ GridGraph[ { 3, 3 } ], { }, 1 ],
  $Failed,
  TestID -> "SelectInfraPath-empty-strict-fails"
]

VerificationTest[
  SelectInfraPath[ GridGraph[ { 3, 3 } ], { }, All ],
  { },
  TestID -> "SelectInfraPath-empty-All-empty"
]

(* ===== Default count = 1, matches FindInfraPoint ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectInfraPath[ g, paths ]
  ],
  1,
  TestID -> "SelectInfraPath-default-n-is-1"
]

(* ===== Operator form ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = #[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    SubsetQ[ paths, SelectInfraPath[ g, All, "From" -> "Center" ][ paths ] ]
  ],
  True,
  TestID -> "SelectInfraPath-operator-form-runs"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SelectInfraPath[ g, paths, All, "From" -> "Center", "Metric" -> "Hausdorff" ] ===
      ( SelectInfraPath[ g, All, "From" -> "Center", "Metric" -> "Hausdorff" ][ paths ] )
  ],
  True,
  TestID -> "SelectInfraPath-operator-form-options-agree"
]

(* ===== Wrapper passthrough ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], list = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    MatchQ[ SelectInfraPath[ g, list, All, "From" -> "Center" ], { InfraSegment[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectInfraPath-preserves-unary-InfraSegment-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], list = FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ] },
    MatchQ[ SelectInfraCycle[ g, list, All, "From" -> "Center" ], { InfraCircle[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectInfraCycle-preserves-unary-InfraCircle-list"
]

(* ===== Length-1 / empty input ===== *)

VerificationTest[
  SelectInfraPath[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, All, "From" -> "Center" ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraPath-Center-singleton-identity"
]

VerificationTest[
  SelectInfraPath[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, All, "From" -> "Periphery", "Metric" -> "Hausdorff" ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraPath-Periphery-singleton-identity"
]

VerificationTest[
  EmbeddingClosest[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, { 1, 3 } ],
  { { 1, 2, 3 } },
  TestID -> "EmbeddingClosest-singleton-identity"
]

(* ===== SelectInfraCycle length-based pool selectors ===== *)

VerificationTest[
  SelectInfraCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "ShortestCircumference" ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraCycle-ShortestCircumference-picks-min"
]

VerificationTest[
  SelectInfraCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "LongestCircumference" ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "SelectInfraCycle-LongestCircumference-picks-max"
]

VerificationTest[
  SelectInfraCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 }, { 4, 5, 6 }, { 1, 2, 3, 4 } }, All,
    "From" -> "ShortestCircumference" ],
  { { 1, 2, 3 }, { 4, 5, 6 } },
  TestID -> "SelectInfraCycle-ShortestCircumference-keeps-ties"
]

(* ===== Metric option carries through ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length[ paths ] > 1 &&
      AllTrue[ { "Hausdorff", "Frechet", "MeanFrechet" },
        m |-> SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "Center", "Metric" -> m ] ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Center-all-metrics-return-sublists"
]

(* ===== MostVisited pool ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-returns-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectInfraPath[ g, paths, All, "From" -> "MostVisited" ] >= 1
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-non-empty"
]

VerificationTest[
  With[ { g = PathGraph[ Range @ 5 ], wrapped = FindInfraSegment[ PathGraph[ Range @ 5 ], 1, 5, All ] },
    SelectInfraPath[ g, wrapped, All, "From" -> "MostVisited" ] === wrapped
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-unique-segment-passthrough"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], cycles = (#[[ 1, 1 ]] & /@ FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ]) },
    SubsetQ[ cycles, SelectInfraCycle[ g, cycles, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectInfraCycle-MostVisited-returns-sublist"
]

(* ===== Distance constraint: Max k-clique in path-space ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ]) },
    Length @ SelectInfraPath[ g, paths, 2, "Distance" -> "Max" ]
  ],
  2,
  TestID -> "SelectInfraPath-Distance-Max-strict-2"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, UpTo[ 3 ], "Distance" -> "Max" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Distance-Max-UpTo-3-sublist"
]

(* ===== "From" anchor -> spec ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ]) },
    With[ { ref = First @ paths,
            others = SelectInfraPath[ g, paths, All, "From" -> ( First @ paths -> "Max" ) ] },
      SubsetQ[ paths, others ]
    ]
  ],
  True,
  TestID -> "SelectInfraPath-From-anchor-Max-returns-sublist"
]

(* ===== Empty pool returns empty ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ SelectInfraPath[ g, InfraSegment[ { } ], All ]
  ],
  InfraSegment,
  TestID -> "SelectInfraPath-empty-wrapper-All-passthrough"
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

(* ===== SelectInfraPath -- MinCurvature / MaxCurvature pool selectors ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    MemberQ[ paths, First @ SelectInfraPath[ g, paths, 1, "From" -> "MinCurvature" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-MinCurvature-returns-member-of-bundle"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          segment = InfraSegment @ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    Head @ SelectInfraPath[ g, segment, 1, "From" -> "MinCurvature" ]
  ],
  InfraSegment,
  TestID -> "SelectInfraPath-MinCurvature-wrapper-preserved"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Sort @ SelectInfraPath[ g, paths, All, "From" -> "MinCurvature" ] ===
      Sort @ SelectInfraPath[ g, paths, All, "From" -> { "MinCurvature", "FormanRicciCurvature" } ] ===
      Sort @ SelectInfraPath[ g, paths, All, "From" -> { "MinCurvature", "FormanRicciCurvature", "Mean" } ]
  ],
  True,
  TestID -> "SelectInfraPath-MinCurvature-bare-equals-full-spec"
]

VerificationTest[
  Module[ { g = GridGraph[ { 3, 3 } ], paths, kappaRaw, kappaSym, score, scores, picked },
    paths = #[[ 1, 1 ]] & /@ FindInfraSegment[ g, 1, 9, All ];
    kappaRaw = WolframInstitute`Infrageometry`FormanRicciCurvature[ g, "MaxCellDimension" -> 1 ];
    kappaSym = Join[ kappaRaw,
      AssociationThread[ Reverse /@ Keys[ kappaRaw ], Values[ kappaRaw ] ] ];
    score[ path_ ] := Mean[ kappaSym /@ ( UndirectedEdge @@@ Partition[ path, 2, 1 ] ) ];
    scores = score /@ paths;
    picked = First @ SelectInfraPath[ g, paths, 1, "From" -> "MinCurvature" ];
    score[ picked ] == Min[ scores ]
  ],
  True,
  TestID -> "SelectInfraPath-MinCurvature-score-equals-min"
]

VerificationTest[
  Module[ { g = GridGraph[ { 3, 3 } ], paths, kappaRaw, kappaSym, score, scores, picked },
    paths = #[[ 1, 1 ]] & /@ FindInfraSegment[ g, 1, 9, All ];
    kappaRaw = WolframInstitute`Infrageometry`FormanRicciCurvature[ g, "MaxCellDimension" -> 1 ];
    kappaSym = Join[ kappaRaw,
      AssociationThread[ Reverse /@ Keys[ kappaRaw ], Values[ kappaRaw ] ] ];
    score[ path_ ] := Mean[ kappaSym /@ ( UndirectedEdge @@@ Partition[ path, 2, 1 ] ) ];
    scores = score /@ paths;
    picked = First @ SelectInfraPath[ g, paths, 1, "From" -> { "MaxCurvature", "FormanRicciCurvature" } ];
    score[ picked ] == Max[ scores ]
  ],
  True,
  TestID -> "SelectInfraPath-MaxCurvature-score-equals-max"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Length @ SelectInfraPath[ g, paths, UpTo[ 3 ], "From" -> "MinCurvature" ] <= 3
  ],
  True,
  TestID -> "SelectInfraPath-MinCurvature-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "MinCurvature" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-MinCurvature-All-returns-subset"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          segment = InfraSegment @ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    Head @ ( SelectInfraPath[ g, 1, "From" -> "MinCurvature" ] @ segment )
  ],
  InfraSegment,
  TestID -> "SelectInfraPath-MinCurvature-operator-form-preserves-wrapper"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ]) },
    Sort @ SelectInfraPath[ g, paths, All, "From" -> { "MinCurvature", "FormanRicciCurvature", "Total" } ] ===
    Sort @ SelectInfraPath[ g, paths, All, "From" -> { "MinCurvature", "FormanRicciCurvature", "Mean"  } ]
  ],
  True,
  TestID -> "SelectInfraPath-MinCurvature-Total-equals-Mean-on-equal-length-bundle"
]


EndTestSection[]


BeginTestSection["SelectInfraPoint"]

VerificationTest[
  SubsetQ[ Range[ 5 ], #[[1, 1]]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "Center" ] ],
  True,
  TestID -> "SelectInfraPoint-Center-pool-is-sublist"
]

VerificationTest[
  #[[1, 1]]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "Center" ],
  { 3 },
  TestID -> "SelectInfraPoint-Center-on-PathGraph-picks-middle"
]

VerificationTest[
  Sort[ #[[1, 1]]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "Periphery" ] ],
  { 1, 5 },
  TestID -> "SelectInfraPoint-Periphery-on-PathGraph-picks-endpoints"
]

VerificationTest[
  Length @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], 1, "From" -> "Center" ],
  1,
  TestID -> "SelectInfraPoint-strict-n-1"
]

VerificationTest[
  Length @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], UpTo[ 3 ] ] <= 3,
  True,
  TestID -> "SelectInfraPoint-UpTo-soft"
]

VerificationTest[
  SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], 99 ],
  $Failed,
  TestID -> "SelectInfraPoint-strict-overcount-fails"
]

VerificationTest[
  SelectInfraPoint[ PathGraph[ Range[ 5 ] ], { }, 1 ],
  $Failed,
  TestID -> "SelectInfraPoint-empty-strict-fails"
]

VerificationTest[
  SelectInfraPoint[ PathGraph[ Range[ 5 ] ], { }, All ],
  { },
  TestID -> "SelectInfraPoint-empty-All-empty"
]

VerificationTest[
  Length @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ] ],
  1,
  TestID -> "SelectInfraPoint-default-n-is-1"
]

VerificationTest[
  Head @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], InfraPoint[ Range[ 5 ] ], All ],
  InfraPoint,
  TestID -> "SelectInfraPoint-preserves-InfraPoint-wrapper"
]

VerificationTest[
  Sort[ #[[1, 1]]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], All, "From" -> "Periphery" ][ Range[ 5 ] ] ],
  { 1, 5 },
  TestID -> "SelectInfraPoint-operator-form"
]

VerificationTest[
  Length @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], 2, "Distance" -> "Max" ],
  2,
  TestID -> "SelectInfraPoint-Distance-Max-strict-2"
]

VerificationTest[
  SubsetQ[ Range[ 5 ],
    #[[1, 1]]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> ( 3 -> 2 ) ] ],
  True,
  TestID -> "SelectInfraPoint-anchor-distance-pool-is-sublist"
]

EndTestSection[]
