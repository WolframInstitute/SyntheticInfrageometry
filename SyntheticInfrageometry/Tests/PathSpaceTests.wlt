BeginTestSection["PathSpace"]

(* ===== Sublist invariants under default n = All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "Center" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Center-pool-is-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "Periphery" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Periphery-pool-is-sublist"
]

(* the geodesic-DAG shortcut for "MostVisited" (longest additive-weight path
   under geodesic-occupation weights) selects the same set of most-visited
   geodesics as scoring the fully enumerated bundle *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    ( SelectInfraPath[ g, FindInfraSegment[ g, 1, 25 ], All, "From" -> "MostVisited" ]
        /. InfraSegment[ r_List ] :> Sort[ Sort /@ r ] )
    ===
    ( SelectInfraPath[ g, InfraSegment[ FindInfraSegment[ g, 1, 25 ][ "Realizations" ] ], All, "From" -> "MostVisited" ]
        /. InfraSegment[ r_List ] :> Sort[ Sort /@ r ] )
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-DAG-equals-enumeration"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, EmbeddingClosest[ g, paths, { 1, 9 } ] ]
  ],
  True,
  TestID -> "EmbeddingClosest-returns-sublist"
]

(* Arbitrary-curve reference: a bare list of >= 3 plane points picks the
   best-approximating bundle element (a sublist of the input). *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, EmbeddingClosest[ g, paths, { { 0, 0 }, { 1, 1 }, { 2, 2 } } ] ]
  ],
  True,
  TestID -> "EmbeddingClosest-curve-list-returns-sublist"
]

(* Curve reference preserves the wrapper head (Line curve, InfraPath bundle). *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], bare = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Head @ EmbeddingClosest[ g, InfraPath[ bare ], Line[ { { 0, 0 }, { 1, 1 }, { 2, 2 } } ] ]
  ],
  InfraPath,
  TestID -> "EmbeddingClosest-curve-preserves-wrapper"
]

(* FindEmbeddingClosestPath: generative snap of a curve to a walk.  Returns an
   InfraPath whose single realisation is a connected walk (consecutive vertices
   adjacent), tracing the curve under the embedding. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ { p = FindEmbeddingClosestPath[ g, Line[ GraphEmbedding[ g ][[ { 1, 13, 25 } ]] ] ] },
      MatchQ[ p, InfraPath[ { { __ } } ] ] &&
      AllTrue[ Partition[ p[[ 1, 1 ]], 2, 1 ], EdgeQ[ g, UndirectedEdge @@ # ] & ]
    ]
  ],
  True,
  TestID -> "FindEmbeddingClosestPath-traces-connected-walk"
]

(* ===== Count contract: strict n, UpTo, All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraPath[ g, paths, 1, "From" -> "Center" ]
  ],
  1,
  TestID -> "SelectInfraPath-strict-n-1"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraPath[ g, paths, UpTo[ 3 ], "From" -> "Center" ] <= 3
  ],
  True,
  TestID -> "SelectInfraPath-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
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
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraPath[ g, paths ]
  ],
  1,
  TestID -> "SelectInfraPath-default-n-is-1"
]

(* ===== Operator form ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ] },
    SubsetQ[ paths, SelectInfraPath[ g, All, "From" -> "Center" ][ paths ] ]
  ],
  True,
  TestID -> "SelectInfraPath-operator-form-runs"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SelectInfraPath[ g, paths, All, "From" -> "Center", "Metric" -> "Hausdorff" ] ===
      ( SelectInfraPath[ g, All, "From" -> "Center", "Metric" -> "Hausdorff" ][ paths ] )
  ],
  True,
  TestID -> "SelectInfraPath-operator-form-options-agree"
]

(* ===== Wrapper passthrough ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], list = InfraSegment[ { # } ] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ] },
    MatchQ[ SelectInfraPath[ g, list, All, "From" -> "Center" ], { InfraSegment[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectInfraPath-preserves-unary-InfraSegment-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], list = InfraCircle[ { # } ] & /@ FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ][ "Realizations" ] },
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
    "From" -> "MinLength" ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraCycle-MinLength-picks-min"
]

VerificationTest[
  SelectInfraCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "MaxLength" ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "SelectInfraCycle-MaxLength-picks-max"
]

VerificationTest[
  SelectInfraCycle[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 }, { 4, 5, 6 }, { 1, 2, 3, 4 } }, All,
    "From" -> "MinLength" ],
  { { 1, 2, 3 }, { 4, 5, 6 } },
  TestID -> "SelectInfraCycle-MinLength-keeps-ties"
]

(* ===== Metric option carries through ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length[ paths ] > 1 &&
      AllTrue[ { "Hausdorff", "Frechet", "MeanFrechet" },
        m |-> SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "Center", "Metric" -> m ] ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Center-all-metrics-return-sublists"
]

(* ===== MostVisited pool ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-returns-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraPath[ g, paths, All, "From" -> "MostVisited" ] >= 1
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-non-empty"
]

VerificationTest[
  With[ { g = PathGraph[ Range @ 5 ], wrapped = FindInfraSegment[ PathGraph[ Range @ 5 ], 1, 5, All ][ "Realizations" ] },
    SelectInfraPath[ g, wrapped, All, "From" -> "MostVisited" ] === wrapped
  ],
  True,
  TestID -> "SelectInfraPath-MostVisited-unique-segment-passthrough"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], cycles = FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ][ "Realizations" ] },
    SubsetQ[ cycles, SelectInfraCycle[ g, cycles, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectInfraCycle-MostVisited-returns-sublist"
]

(* ===== Bottleneck pool: max-min bundle occupation of vertices + edges ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    With[ { vC = Counts @ Catenate @ paths,
            eC = Counts @ Catenate @ ( Sort /@ Partition[ #, 2, 1 ] & /@ paths ) },
      With[ { scores = Min @ Join[ Lookup[ vC, #, 0 ], Lookup[ eC, Sort /@ Partition[ #, 2, 1 ], 0 ] ] & /@ paths },
        Sort @ SelectInfraPath[ g, paths, All, "From" -> "Bottleneck" ] ===
          Sort @ Pick[ paths, Thread[ scores == Max @ scores ] ]
      ]
    ]
  ],
  True,
  TestID -> "SelectInfraPath-Bottleneck-maximin-occupation"
]

(* ===== Distance constraint: Max k-clique in path-space ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ][ "Realizations" ]) },
    Length @ SelectInfraPath[ g, paths, 2, "Distance" -> "Max" ]
  ],
  2,
  TestID -> "SelectInfraPath-Distance-Max-strict-2"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraPath[ g, paths, UpTo[ 3 ], "Distance" -> "Max" ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Distance-Max-UpTo-3-sublist"
]

(* ===== "From" anchor -> spec ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ][ "Realizations" ]) },
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

(* ===== GeodesicSprayGraph ===== *)

VerificationTest[
  GraphQ @ GeodesicSprayGraph[ PathGraph[ Range[ 5 ] ], { { 1, 5 } } ],
  True,
  TestID -> "GeodesicSprayGraph-returns-graph"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    EdgeCount @ GeodesicSprayGraph[ g, { { 1, 9 } }, "PathThickness" -> Infinity ] >
    EdgeCount @ GeodesicSprayGraph[ g, { { 1, 9 } }, "PathThickness" -> 0 ]
  ],
  True,
  TestID -> "GeodesicSprayGraph-thickness-grows"
]

VerificationTest[
  DirectedGraphQ @ GeodesicSprayGraph[ CycleGraph[ 6 ], { { 1, 4 } }, "Directed" -> False ],
  False,
  TestID -> "GeodesicSprayGraph-undirected"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    SubsetQ[ VertexList[ g ], VertexList @ GeodesicSprayGraph[ g, { { 1, 9 }, { 3, 7 } } ] ]
  ],
  True,
  TestID -> "GeodesicSprayGraph-multi-pair"
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

(* ===== SelectInfraPath -- {"Min", scoreFn} / {"Max", scoreFn} selectors ===== *)

(* scoreFn is a user-supplied path-aggregated function; pool keeps positions
   where scoreFn is extremal.  degSumScore[path] is a synthetic test scorer
   (sum of edge-degree-sums along the walk). *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]),
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    MemberQ[ paths, First @ SelectInfraPath[ g, paths, 1, "From" -> { "Min", degSumScore } ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Min-returns-member-of-bundle"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          segment = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ],
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    Head @ SelectInfraPath[ g, segment, 1, "From" -> { "Min", degSumScore } ]
  ],
  InfraSegment,
  TestID -> "SelectInfraPath-Min-wrapper-preserved"
]

VerificationTest[
  Module[ { g = GridGraph[ { 3, 3 } ], paths, scoreFn, scores, picked },
    paths = FindInfraSegment[ g, 1, 9, All ][ "Realizations" ];
    scoreFn = path |-> Total[
      ( VertexDegree[ g, #[[ 1 ]] ] + VertexDegree[ g, #[[ 2 ]] ] & ) /@
        Partition[ path, 2, 1 ] ];
    scores = scoreFn /@ paths;
    picked = First @ SelectInfraPath[ g, paths, 1, "From" -> { "Min", scoreFn } ];
    scoreFn[ picked ] == Min[ scores ]
  ],
  True,
  TestID -> "SelectInfraPath-Min-score-equals-min"
]

VerificationTest[
  Module[ { g = GridGraph[ { 3, 3 } ], paths, scoreFn, scores, picked },
    paths = FindInfraSegment[ g, 1, 9, All ][ "Realizations" ];
    scoreFn = path |-> Total[
      ( VertexDegree[ g, #[[ 1 ]] ] + VertexDegree[ g, #[[ 2 ]] ] & ) /@
        Partition[ path, 2, 1 ] ];
    scores = scoreFn /@ paths;
    picked = First @ SelectInfraPath[ g, paths, 1, "From" -> { "Max", scoreFn } ];
    scoreFn[ picked ] == Max[ scores ]
  ],
  True,
  TestID -> "SelectInfraPath-Max-score-equals-max"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]),
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    Length @ SelectInfraPath[ g, paths, UpTo[ 3 ], "From" -> { "Min", degSumScore } ] <= 3
  ],
  True,
  TestID -> "SelectInfraPath-Min-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]),
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    SubsetQ[ paths, SelectInfraPath[ g, paths, All, "From" -> { "Min", degSumScore } ] ]
  ],
  True,
  TestID -> "SelectInfraPath-Min-All-returns-subset"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          segment = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ],
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    Head @ ( SelectInfraPath[ g, 1, "From" -> { "Min", degSumScore } ] @ segment )
  ],
  InfraSegment,
  TestID -> "SelectInfraPath-Min-operator-form-preserves-wrapper"
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


(* ===== FindForwardDeformation ===== *)

(* diamond-with-ear: geodesics 1-2-5 and 1-3-5; triangle {1,2,4} gives a LengthDelta 1 bump *)

VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    With[ { w = First[ FindForwardDeformation[ g, { 1, 2, 5 }, "LengthDelta" -> { 1 } ] /. InfraPath[ x_, ___ ] :> x ] },
      Length[ w ] - 1 == GraphDistance[ g, 1, 5 ] + 1 ]
  ],
  True,
  TestID -> "FindForwardDeformation-LengthDelta-equals-excess"
]

VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    With[ { w = First[ FindForwardDeformation[ g, { 1, 2, 5 }, "LengthDelta" ] /. InfraPath[ x_, ___ ] :> x ] },
      Length[ w ] - 1 == GraphDistance[ g, 1, 5 ] && w =!= { 1, 2, 5 } ]
  ],
  True,
  TestID -> "FindForwardDeformation-min-LengthDelta-is-shortest-alternative"
]

VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    With[ { w = First[ FindForwardDeformation[ g, { 1, 2, 5 }, "DeformationSize" ] /. InfraPath[ x_, ___ ] :> x ],
            dA = AssociationThread[ VertexList[ g ], GraphDistance[ g, 1 ] ] },
      First[ w ] == 1 && Last[ w ] == 5 &&
      AllTrue[ Partition[ w, 2, 1 ], dA[ #[[ 2 ]] ] - dA[ #[[ 1 ]] ] >= 0 & ]
    ]
  ],
  True,
  TestID -> "FindForwardDeformation-forward-monotone-endpoints"
]

(* triangulated grid: every level-k deformation has length n + k *)
VerificationTest[
  With[ { g = Graph[ Flatten[ {
       Table[ { i, j } <-> { i + 1, j }, { i, 2 }, { j, 3 } ],
       Table[ { i, j } <-> { i, j + 1 }, { i, 3 }, { j, 2 } ],
       Table[ { i, j } <-> { i + 1, j + 1 }, { i, 2 }, { j, 2 } ] } ] ] },
    With[ { n = GraphDistance[ g, { 1, 1 }, { 3, 3 } ],
            res = FindForwardDeformation[ g, { { 1, 1 }, { 3, 3 } }, "LengthDelta" -> 2, All ] /. InfraPath[ x_, ___ ] :> x },
      AllTrue[ Range[ 0, 2 ], k |-> AllTrue[ res[[ k + 1 ]], Length[ # ] - 1 == n + k & ] ]
    ]
  ],
  True,
  TestID -> "FindForwardDeformation-grid-length-equals-n-plus-k"
]

(* non-geodesic forward reference {1,4,2,5} (L = 3 > n = 2): deformations can shorten *)
VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    FindForwardDeformation[ g, { 1, 4, 2, 5 }, "LengthDelta" -> { -1 }, All ] /. InfraPath[ x_, ___ ] :> x
  ],
  { { 1, 2, 5 }, { 1, 3, 5 } },
  TestID -> "FindForwardDeformation-negative-delta-reaches-geodesics"
]

VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    FindForwardDeformation[ g, { 1, 4, 2, 5 }, "LengthDelta", All ] /. InfraPath[ x_, ___ ] :> x
  ],
  { { 1, 2, 5 }, { 1, 3, 5 } },
  TestID -> "FindForwardDeformation-min-delta-is-distance-minus-reference-length"
]

(* non-forward reference (2 -> 1 runs against the spray): deformations straighten it,
   DeformationSize measured against the given walk *)
VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    With[ { dA = AssociationThread[ VertexList[ g ], GraphDistance[ g, 1 ] ],
            groups = FindForwardDeformation[ g, { 1, 2, 1, 3, 5 }, "DeformationSize" -> { 1, 4 }, All ] /. InfraPath[ x_, ___ ] :> x },
      ( FindForwardDeformation[ g, { 1, 2, 1, 3, 5 }, "DeformationSize", All ] /. InfraPath[ x_, ___ ] :> x ) === { { 1, 3, 5 } } &&
      AllTrue[ Flatten[ groups, 1 ], w |-> AllTrue[ Partition[ w, 2, 1 ], dA[ #[[ 2 ]] ] - dA[ #[[ 1 ]] ] >= 0 & ] ]
    ]
  ],
  True,
  TestID -> "FindForwardDeformation-nonforward-base-straightened-forward"
]

(* spec order decides the driving axis: size-first groups by DeformationSize,
   delta-first groups by LengthDelta *)
VerificationTest[
  With[ { g = Graph[ { 1 <-> 2, 2 <-> 5, 1 <-> 3, 3 <-> 5, 1 <-> 4, 4 <-> 2 } ] },
    {
      FindForwardDeformation[ g, { 1, 2, 5 }, { "DeformationSize" -> { 1 }, "LengthDelta" -> 1 }, All ] /. InfraPath[ x_, ___ ] :> x,
      FindForwardDeformation[ g, { 1, 2, 5 }, { "LengthDelta" -> 1, "DeformationSize" -> { 1 } }, All ] /. InfraPath[ x_, ___ ] :> x,
      FindForwardDeformation[ g, { 1, 2, 5 }, { "DeformationSize" -> { 1, 2 }, "LengthDelta" -> 1 }, All ] /. InfraPath[ x_, ___ ] :> x
    }
  ],
  { { { 1, 4, 2, 5 } }, { { }, { { 1, 4, 2, 5 } } }, { { { 1, 4, 2, 5 } }, { { 1, 3, 5 } } } },
  TestID -> "FindForwardDeformation-spec-order-decides-driver"
]


(* the spray carries the base graph's embedding, so figures carved out of it
   stay aligned with the substrate *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { spray = GeodesicSprayGraph[ g, 1 ] },
      Sort @ Cases[ Options[ spray, VertexCoordinates ], _ -> vc_ :> Length @ vc ] =!= { } &&
        VertexList[ spray ] === VertexList[ g ] ]
  ],
  True,
  TestID -> "GeodesicSprayGraph-keeps-base-coordinates"
]

(* FindInfraCommonPoint accepts the compact geodesic-DAG segment form *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { s1 = FindInfraSegment[ g, 1, 9 ], s2 = FindInfraSegment[ g, 3, 7 ] },
      Sort @ FindInfraCommonPoint[ g, { s1, s2 } ][ "Support" ] ===
        Sort @ Intersection[
          Union @@ FindInfraSegment[ g, 1, 9, All ][ "Realizations" ],
          Union @@ FindInfraSegment[ g, 3, 7, All ][ "Realizations" ] ] ]
  ],
  True,
  TestID -> "FindInfraCommonPoint-accepts-DAG-segments"
]

EndTestSection[]
