BeginTestSection["WalkSpace"]

(* ===== Sublist invariants under default n = All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraWalk[ g, paths, All, "From" -> "Center" ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-Center-pool-is-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraWalk[ g, paths, All, "From" -> "Periphery" ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-Periphery-pool-is-sublist"
]

(* the geodesic-DAG shortcut for "MostVisited" (longest additive-weight path
   under geodesic-occupation weights) selects the same set of most-visited
   geodesics as scoring the fully enumerated bundle *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    ( SelectInfraWalk[ g, FindInfraSegment[ g, 1, 25 , All], All, "From" -> "MostVisited" ]
        /. InfraSegment[ r_List ] :> Sort[ Sort /@ r ] )
    ===
    ( SelectInfraWalk[ g, InfraSegment[ FindInfraSegment[ g, 1, 25 , All][ "Realizations" ] ], All, "From" -> "MostVisited" ]
        /. InfraSegment[ r_List ] :> Sort[ Sort /@ r ] )
  ],
  True,
  TestID -> "SelectInfraWalk-MostVisited-DAG-equals-enumeration"
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

(* Curve reference preserves the wrapper head (Line curve, InfraWalk bundle). *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], bare = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Head @ EmbeddingClosest[ g, InfraWalk[ bare ], Line[ { { 0, 0 }, { 1, 1 }, { 2, 2 } } ] ]
  ],
  InfraWalk,
  TestID -> "EmbeddingClosest-curve-preserves-wrapper"
]

(* FindEmbeddingClosestPath: generative snap of a curve to a walk.  Returns an
   InfraWalk whose single realisation is a connected walk (consecutive vertices
   adjacent), tracing the curve under the embedding. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ { p = FindEmbeddingClosestPath[ g, Line[ GraphEmbedding[ g ][[ { 1, 13, 25 } ]] ] ] },
      MatchQ[ p, InfraWalk[ { { __ } } ] ] &&
      AllTrue[ Partition[ p[[ 1, 1 ]], 2, 1 ], EdgeQ[ g, UndirectedEdge @@ # ] & ]
    ]
  ],
  True,
  TestID -> "FindEmbeddingClosestPath-traces-connected-walk"
]

(* ===== Count contract: strict n, UpTo, All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraWalk[ g, paths, 1, "From" -> "Center" ]
  ],
  1,
  TestID -> "SelectInfraWalk-strict-n-1"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraWalk[ g, paths, UpTo[ 3 ], "From" -> "Center" ] <= 3
  ],
  True,
  TestID -> "SelectInfraWalk-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SelectInfraWalk[ g, paths, 99 ]
  ],
  $Failed,
  TestID -> "SelectInfraWalk-strict-overcount-fails"
]

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { }, 1 ],
  $Failed,
  TestID -> "SelectInfraWalk-empty-strict-fails"
]

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { }, All ],
  { },
  TestID -> "SelectInfraWalk-empty-All-empty"
]

(* ===== Default count = 1, matches FindInfraPoint ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraWalk[ g, paths ]
  ],
  1,
  TestID -> "SelectInfraWalk-default-n-is-1"
]

(* ===== Operator form ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ] },
    SubsetQ[ paths, SelectInfraWalk[ g, All, "From" -> "Center" ][ paths ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-operator-form-runs"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SelectInfraWalk[ g, paths, All, "From" -> "Center", "Metric" -> "Hausdorff" ] ===
      ( SelectInfraWalk[ g, All, "From" -> "Center", "Metric" -> "Hausdorff" ][ paths ] )
  ],
  True,
  TestID -> "SelectInfraWalk-operator-form-options-agree"
]

(* ===== Wrapper passthrough ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], list = InfraSegment[ { # } ] & /@ FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ] },
    MatchQ[ SelectInfraWalk[ g, list, All, "From" -> "Center" ], { InfraSegment[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectInfraWalk-preserves-unary-InfraSegment-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], list = InfraCircle[ { # } ] & /@ FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ][ "Realizations" ] },
    MatchQ[ SelectInfraWalk[ g, list, All, "From" -> "Center" ], { InfraCircle[ { _ } ] .. } ]
  ],
  True,
  TestID -> "SelectInfraWalk-preserves-unary-InfraCircle-list"
]

(* ===== Length-1 / empty input ===== *)

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, All, "From" -> "Center" ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraWalk-Center-singleton-identity"
]

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, All, "From" -> "Periphery", "Metric" -> "Hausdorff" ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraWalk-Periphery-singleton-identity"
]

VerificationTest[
  EmbeddingClosest[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 } }, { 1, 3 } ],
  { { 1, 2, 3 } },
  TestID -> "EmbeddingClosest-singleton-identity"
]

(* ===== SelectInfraWalk length-based pool selectors ===== *)

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "MinLength", "Cyclic" -> True ],
  { { 1, 2, 3 } },
  TestID -> "SelectInfraWalk-MinLength-picks-min"
]

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { { 1, 2, 3, 4, 5 }, { 1, 2, 3 }, { 1, 2, 3, 4 } }, All,
    "From" -> "MaxLength", "Cyclic" -> True ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "SelectInfraWalk-MaxLength-picks-max"
]

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 3, 3 } ], { { 1, 2, 3 }, { 4, 5, 6 }, { 1, 2, 3, 4 } }, All,
    "From" -> "MinLength", "Cyclic" -> True ],
  { { 1, 2, 3 }, { 4, 5, 6 } },
  TestID -> "SelectInfraWalk-MinLength-keeps-ties"
]

(* ===== Metric option carries through ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length[ paths ] > 1 &&
      AllTrue[ { "Hausdorff", "Frechet", "MeanFrechet" },
        m |-> SubsetQ[ paths, SelectInfraWalk[ g, paths, All, "From" -> "Center", "Metric" -> m ] ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-Center-all-metrics-return-sublists"
]

(* ===== MostVisited pool ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraWalk[ g, paths, All, "From" -> "MostVisited" ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-MostVisited-returns-sublist"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    Length @ SelectInfraWalk[ g, paths, All, "From" -> "MostVisited" ] >= 1
  ],
  True,
  TestID -> "SelectInfraWalk-MostVisited-non-empty"
]

VerificationTest[
  With[ { g = PathGraph[ Range @ 5 ], wrapped = FindInfraSegment[ PathGraph[ Range @ 5 ], 1, 5, All ][ "Realizations" ] },
    SelectInfraWalk[ g, wrapped, All, "From" -> "MostVisited" ] === wrapped
  ],
  True,
  TestID -> "SelectInfraWalk-MostVisited-unique-segment-passthrough"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], cycles = FindInfraCircle[ GridGraph[ { 4, 4 } ], 6, { 1, 2 }, All ][ "Realizations" ] },
    SubsetQ[ cycles, SelectInfraWalk[ g, cycles, All, "From" -> "MostVisited", "Cyclic" -> True ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-MostVisited-returns-sublist"
]

(* ===== Bottleneck pool: max-min bundle occupation of vertices + edges ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]) },
    With[ { vC = Counts @ Catenate @ paths,
            eC = Counts @ Catenate @ ( Sort /@ Partition[ #, 2, 1 ] & /@ paths ) },
      With[ { scores = Min @ Join[ Lookup[ vC, #, 0 ], Lookup[ eC, Sort /@ Partition[ #, 2, 1 ], 0 ] ] & /@ paths },
        Sort @ SelectInfraWalk[ g, paths, All, "From" -> "Bottleneck" ] ===
          Sort @ Pick[ paths, Thread[ scores == Max @ scores ] ]
      ]
    ]
  ],
  True,
  TestID -> "SelectInfraWalk-Bottleneck-maximin-occupation"
]

(* ===== Distance constraint: Max k-clique in path-space ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ][ "Realizations" ]) },
    Length @ SelectInfraWalk[ g, paths, 2, "Distance" -> "Max" ]
  ],
  2,
  TestID -> "SelectInfraWalk-Distance-Max-strict-2"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ][ "Realizations" ]) },
    SubsetQ[ paths, SelectInfraWalk[ g, paths, UpTo[ 3 ], "Distance" -> "Max" ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-Distance-Max-UpTo-3-sublist"
]

(* ===== "From" anchor -> spec ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], paths = (FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All ][ "Realizations" ]) },
    With[ { ref = First @ paths,
            others = SelectInfraWalk[ g, paths, All, "From" -> ( First @ paths -> "Max" ) ] },
      SubsetQ[ paths, others ]
    ]
  ],
  True,
  TestID -> "SelectInfraWalk-From-anchor-Max-returns-sublist"
]

(* ===== Empty pool returns empty ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ SelectInfraWalk[ g, InfraSegment[ { } ], All ]
  ],
  InfraSegment,
  TestID -> "SelectInfraWalk-empty-wrapper-All-passthrough"
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

(* ===== SelectInfraWalk -- {"Min", scoreFn} / {"Max", scoreFn} selectors ===== *)

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
    MemberQ[ paths, First @ SelectInfraWalk[ g, paths, 1, "From" -> { "Min", degSumScore } ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-Min-returns-member-of-bundle"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          segment = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ],
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    Head @ SelectInfraWalk[ g, segment, 1, "From" -> { "Min", degSumScore } ]
  ],
  InfraSegment,
  TestID -> "SelectInfraWalk-Min-wrapper-preserved"
]

VerificationTest[
  Module[ { g = GridGraph[ { 3, 3 } ], paths, scoreFn, scores, picked },
    paths = FindInfraSegment[ g, 1, 9, All ][ "Realizations" ];
    scoreFn = path |-> Total[
      ( VertexDegree[ g, #[[ 1 ]] ] + VertexDegree[ g, #[[ 2 ]] ] & ) /@
        Partition[ path, 2, 1 ] ];
    scores = scoreFn /@ paths;
    picked = First @ SelectInfraWalk[ g, paths, 1, "From" -> { "Min", scoreFn } ];
    scoreFn[ picked ] == Min[ scores ]
  ],
  True,
  TestID -> "SelectInfraWalk-Min-score-equals-min"
]

VerificationTest[
  Module[ { g = GridGraph[ { 3, 3 } ], paths, scoreFn, scores, picked },
    paths = FindInfraSegment[ g, 1, 9, All ][ "Realizations" ];
    scoreFn = path |-> Total[
      ( VertexDegree[ g, #[[ 1 ]] ] + VertexDegree[ g, #[[ 2 ]] ] & ) /@
        Partition[ path, 2, 1 ] ];
    scores = scoreFn /@ paths;
    picked = First @ SelectInfraWalk[ g, paths, 1, "From" -> { "Max", scoreFn } ];
    scoreFn[ picked ] == Max[ scores ]
  ],
  True,
  TestID -> "SelectInfraWalk-Max-score-equals-max"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]),
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    Length @ SelectInfraWalk[ g, paths, UpTo[ 3 ], "From" -> { "Min", degSumScore } ] <= 3
  ],
  True,
  TestID -> "SelectInfraWalk-Min-UpTo-soft"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          paths = (FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ][ "Realizations" ]),
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    SubsetQ[ paths, SelectInfraWalk[ g, paths, All, "From" -> { "Min", degSumScore } ] ]
  ],
  True,
  TestID -> "SelectInfraWalk-Min-All-returns-subset"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ],
          segment = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ],
          degSumScore = path |-> Total[
            ( VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 1 ]] ] +
              VertexDegree[ GridGraph[ { 3, 3 } ], #[[ 2 ]] ] & ) /@
              Partition[ path, 2, 1 ] ] },
    Head @ ( SelectInfraWalk[ g, 1, "From" -> { "Min", degSumScore } ] @ segment )
  ],
  InfraSegment,
  TestID -> "SelectInfraWalk-Min-operator-form-preserves-wrapper"
]


EndTestSection[]


BeginTestSection["SelectInfraPoint"]
VerificationTest[
  SubsetQ[ Range[ 5 ], #["Vertex"]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "Center" ] ],
  True,
  TestID -> "SelectInfraPoint-Center-pool-is-sublist"
]

VerificationTest[
  #["Vertex"]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "Center" ],
  { 3 },
  TestID -> "SelectInfraPoint-Center-on-PathGraph-picks-middle"
]

VerificationTest[
  Sort[ #["Vertex"]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "Periphery" ] ],
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
  { Head @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], InfraSet[ Range[ 5 ] ], All ],
    Head @ First @ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], InfraSet[ Range[ 5 ] ], All ] },
  { List, InfraPoint },
  TestID -> "SelectInfraPoint-returns-atom-list"
]

VerificationTest[
  Sort[ #["Vertex"]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], All, "From" -> "Periphery" ][ Range[ 5 ] ] ],
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
    #["Vertex"]& /@ SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> ( 3 -> 2 ) ] ],
  True,
  TestID -> "SelectInfraPoint-anchor-distance-pool-is-sublist"
]


(* ===== InfraDeformationSize ===== *)

(* the number of reference edges a deformation replaces *)
VerificationTest[
  { InfraDeformationSize[ { 1, 2, 5 }, { 1, 3, 5 } ],
    InfraDeformationSize[ { 1, 2, 5 }, { 1, 4, 2, 5 } ],
    InfraDeformationSize[ { 1, 2, 5 }, { 1, 2, 4, 2, 5 } ] },
  { 2, 1, 0 },
  TestID -> "InfraDeformationSize-counts-replaced-reference-edges"
]

(* a deformation sharing a prefix and a suffix with the reference replaces only
   the edges between them *)
VerificationTest[
  With[ { ref = Range[ 6 ] },
    { InfraDeformationSize[ ref, { 1, 2, 3, 9, 5, 6 } ],
      InfraDeformationSize[ ref, { 1, 2, 8, 9, 5, 6 } ],
      InfraDeformationSize[ ref, ref ] }
  ],
  { 2, 3, 0 },
  TestID -> "InfraDeformationSize-is-the-replaced-window"
]

(* bundle form: one number per realisation *)
VerificationTest[
  InfraDeformationSize[ { 1, 2, 5 },
    InfraWalk[ { { 1, 3, 5 }, { 1, 4, 2, 5 }, { 1, 2, 4, 2, 5 } } ] ],
  { 2, 1, 0 },
  TestID -> "InfraDeformationSize-maps-over-a-bundle"
]

(* the invariant composes as a walk-space score: grouping and extremising by it
   happen at the call site, not through an option *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], ref = { 1, 2, 3, 6, 9 },
          ws = InfraWalk[ { { 1, 2, 3, 6, 9 }, { 1, 2, 5, 6, 9 }, { 1, 4, 7, 8, 9 } } ] },
    { InfraDeformationSize[ ref, ws ],
      With[ { picked = SelectInfraWalk[ g, ws, All,
                "From" -> { "Min", w |-> InfraDeformationSize[ ref, w ] } ] },
        Union[ InfraDeformationSize[ ref, picked ] ] ===
          { Min @ InfraDeformationSize[ ref, ws ] } ] }
  ],
  { { 0, 2, 4 }, True },
  TestID -> "InfraDeformationSize-drives-SelectInfraWalk-at-the-call-site"
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
    With[ { s1 = FindInfraSegment[ g, 1, 9 , All], s2 = FindInfraSegment[ g, 3, 7 , All] },
      Sort[ #[ "Vertex" ] & /@ FindInfraCommonPoint[ g, { s1, s2 } ] ] ===
        Sort @ Intersection[
          Union @@ FindInfraSegment[ g, 1, 9, All ][ "Realizations" ],
          Union @@ FindInfraSegment[ g, 3, 7, All ][ "Realizations" ] ] ]
  ],
  True,
  TestID -> "FindInfraCommonPoint-accepts-DAG-segments"
]

(* an InfraPoint atom is the natural single source of a spray; an atom list
   and an InfraSet are the multi-source forms *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { GeodesicSprayGraph[ g, InfraPoint[ 1 ] ] === GeodesicSprayGraph[ g, 1 ],
      GeodesicSprayGraph[ g, { InfraPoint[ 1 ], InfraPoint[ 4 ] } ] === GeodesicSprayGraph[ g, InfraSet[ { 1, 4 } ] ] } ],
  { True, True },
  TestID -> "GeodesicSprayGraph-accepts-point-atoms"
]
(* ===== "From" validation ===== *)

(* An unrecognised "From" selector is refused rather than silently read as the
   whole bundle.  "MinCurvature" / "MaxCurvature" were replaced by
   {"Min", scoreFn} / {"Max", scoreFn}, so they are the names a reader is most
   likely to copy from older prose. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    SelectInfraWalk[ g, FindInfraWalk[ g, 1, 13, 6, All ][ "Realizations" ], All,
      "From" -> "MinCurvature" ] ],
  $Failed,
  { SelectInfraWalk::badfrom },
  TestID -> "SelectInfraWalk-badfrom-retired-selector"
]

VerificationTest[
  SelectInfraWalk[ GridGraph[ { 5, 5 } ], { { 1, 2, 7, 6 } }, All, "From" -> "MaxCurvature" ],
  $Failed,
  { SelectInfraWalk::badfrom },
  TestID -> "SelectInfraWalk-badfrom-retired-selector"
]

VerificationTest[
  SelectInfraPoint[ PathGraph[ Range[ 5 ] ], Range[ 5 ], All, "From" -> "MinCurvature" ],
  $Failed,
  { SelectInfraPoint::badfrom },
  TestID -> "SelectInfraPoint-badfrom-retired-selector"
]

(* Refusing a legitimate selector would be worse than the silence it replaces:
   every admissible "From" shape must still produce a pool. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    { paths = FindInfraWalk[ g, 1, 13, 6, All ][ "Realizations" ] },
    FreeQ[
      SelectInfraWalk[ g, paths, All, "From" -> # ] & /@
        { All, "Center", "Periphery", "MostVisited", "Bottleneck", "MinLength", "MaxLength",
          First[ paths ] -> 2, { "Min", Length }, { "Max", Length } },
      $Failed ]
  ],
  True,
  TestID -> "SelectInfraWalk-From-vocabulary-not-refused"
]

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    FreeQ[
      SelectInfraPoint[ g, Range[ 25 ], All, "From" -> # ] & /@
        { All, "Random", "Center", "Periphery", 7, 3 -> 2, { 2, 3, 4 }, InfraSet[ { 2, 5, 7 } ] },
      $Failed ]
  ],
  True,
  TestID -> "SelectInfraPoint-From-vocabulary-not-refused"
]


EndTestSection[]
