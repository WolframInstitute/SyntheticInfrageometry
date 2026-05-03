BeginTestSection["EuclideanPostulates"]

(* ===== FindPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindPoint[g]},
      Length[pt] == 1 && SubsetQ[VertexList[g], pt]
    ]
  ],
  True,
  TestID -> "FindPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindPoint[g, 3]},
      Length[pts] == 3 && DuplicateFreeQ[pts] && SubsetQ[VertexList[g], pts]
    ]
  ],
  True,
  TestID -> "FindPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], FindPoint[g, 1, "From" -> "Center"]]
  ],
  True,
  TestID -> "FindPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], FindPoint[g, 1, "From" -> "Periphery"]]
  ],
  True,
  TestID -> "FindPoint-from-periphery"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = FindPoint[g, 2, "Distance" -> 4]},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindPoint[g, 1, "From" -> {2, 3, 4}]},
      Length[pt] == 1 && SubsetQ[{2, 3, 4}, pt]
    ]
  ],
  True,
  TestID -> "FindPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = FindPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4]},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindPoint-vertex-list-with-distance"
]

VerificationTest[
  FindPoint[PathGraph[Range[3]], 10],
  $Failed,
  TestID -> "FindPoint-exact-fails-when-too-few"
]

VerificationTest[
  With[{pts = FindPoint[PathGraph[Range[3]], UpTo[10]]},
    Length[pts] == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], pts]
  ],
  True,
  TestID -> "FindPoint-upto-returns-available"
]

VerificationTest[
  FindPoint[PathGraph[Range[3]], 3, "Distance" -> 5],
  $Failed,
  TestID -> "FindPoint-exact-fails-impossible-distance"
]

VerificationTest[
  With[{g = PathGraph[Range[7]]},
    With[{pt = FindPoint[g, 1, "From" -> 3 -> 2]},
      Length[pt] == 1 && GraphDistance[g, 3, First[pt]] == 2
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = FindPoint[g, UpTo[20], "From" -> 1 -> {2, 3}]},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = FindPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"]},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-max-distance"
]

(* ===== FindSegment ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = FindSegment[g, 1, 3]},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, All]},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = CentralPaths[g, FindSegment[g, 1, 9, All]]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-CentralPaths-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosestPaths[g, FindSegment[g, 1, 9, All], {1, 9}]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-EmbeddingClosestPaths"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  {},
  TestID -> "FindSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = CentralPaths[g, FindSegment[g, 1, 9, All], Method -> "Hausdorff"]},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-CentralPaths-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = PeripheralPaths[g, FindSegment[g, 1, 9, All]]},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-PeripheralPaths-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosestPaths[g, {1, 9}] @ CentralPaths[g] @ FindSegment[g, 1, 9, All]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, UpTo[2]]},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-upto-soft-cap"
]

(* ===== FindSegment Method -> "Extended" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "Extended"]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindSegment-Extended-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> "Extended"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-Extended-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> {"Extended", "Lookback" -> 1}] ===
      Sort @ FindPath[g, 1, 9, Infinity, All]
  ],
  True,
  TestID -> "FindSegment-Extended-K1-equals-FindPath"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Sort @ FindSegment[g, 1, 3, All, Method -> {"Extended", "Lookback" -> 1}],
      k2   = Sort @ FindSegment[g, 1, 3, All, Method -> {"Extended", "Lookback" -> 2}],
      kAll = Sort @ FindSegment[g, 1, 3, All, Method -> {"Extended", "Lookback" -> All}]
    },
      Length[k1] == 4 &&
      MemberQ[k1, {1, 2, 4, 3}] && MemberQ[k1, {1, 4, 2, 3}] &&
      k2 === Sort[{{1, 2, 3}, {1, 4, 3}}] &&
      kAll === k2
    ]
  ],
  True,
  TestID -> "FindSegment-Extended-K2-strict-between"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Length @ FindSegment[g, 1, 3, All, Method -> {"Extended", "Lookback" -> 1}],
      k2   = Length @ FindSegment[g, 1, 3, All, Method -> {"Extended", "Lookback" -> 2}],
      kAll = Length @ FindSegment[g, 1, 3, All, Method -> {"Extended", "Lookback" -> All}]
    },
      k1 >= k2 >= kAll
    ]
  ],
  True,
  TestID -> "FindSegment-Extended-K-monotone"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "Extended"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"Extended", "Lookback" -> 2}]
  ],
  True,
  TestID -> "FindSegment-Extended-Lookback-default-is-2"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length[FindSegment[g, 1, 16, All,
        Method -> {"Extended", "Pruning" -> 1}]] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-Extended-pruning-beam-1"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> "Bogus"],
    FindSegment::badmethod],
  $Failed,
  TestID -> "FindSegment-bad-method"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Extended", "Pruning" -> -1}],
    FindSegment::badpruning],
  $Failed,
  TestID -> "FindSegment-bad-pruning"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Extended", "Lookback" -> 0}],
    FindSegment::badlookback],
  $Failed,
  TestID -> "FindSegment-bad-lookback-zero"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Extended", "Lookback" -> -1}],
    FindSegment::badlookback],
  $Failed,
  TestID -> "FindSegment-bad-lookback-negative"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Extended", "Lookback" -> 1.5}],
    FindSegment::badlookback],
  $Failed,
  TestID -> "FindSegment-bad-lookback-fractional"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "Extended"],
  {},
  TestID -> "FindSegment-Extended-same-point-empty"
]


(* ===== FindSegment Method -> "Pulled" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "Pulled"]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindSegment-Pulled-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> "Pulled"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-Pulled-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> "Pulled"],
      walk |-> First[walk] === 1 && Last[walk] === 9 &&
        DuplicateFreeQ[walk] &&
        AllTrue[Partition[walk, 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-grid-walks-valid"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, All, Method -> {"Pulled", "Constraint" -> "Free"}]
  ],
  6,
  TestID -> "FindSegment-Pulled-Free-grid-bundle-size"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> "Pulled"],
      walk |-> Length[walk] - 1 >= GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-grid-walks-no-shorter-than-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "Pulled"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"Pulled", "FormanMethod" -> "Simple"}]
  ],
  True,
  TestID -> "FindSegment-Pulled-FormanMethod-default-is-Simple"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    Sort @ FindSegment[g, 1, 3, All, Method -> {"Pulled", "FormanMethod" -> "Simple", "Constraint" -> "Free"}] =!=
      Sort @ FindSegment[g, 1, 3, All, Method -> {"Pulled", "FormanMethod" -> "Triangles", "Constraint" -> "Free"}]
  ],
  True,
  TestID -> "FindSegment-Pulled-Free-FormanMethod-Triangles-differs-on-triangulated"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, UpTo[2], Method -> "Pulled"]
  ],
  2,
  TestID -> "FindSegment-Pulled-UpTo-truncates"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, 3, Method -> "Pulled"]
  ],
  3,
  TestID -> "FindSegment-Pulled-Count-exact"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    CentralPaths[g] @ FindSegment[g, 1, 9, All, Method -> "Pulled"]
  ],
  _List,
  SameTest -> MatchQ,
  TestID -> "FindSegment-Pulled-chains-with-CentralPaths"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Pulled", "FormanMethod" -> "Quadrilaterals"}],
    FindSegment::badforman],
  $Failed,
  TestID -> "FindSegment-Pulled-bad-FormanMethod"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length[FindSegment[g, 1, 16, All, Method -> {"Pulled", "Pruning" -> 1}]] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-pruning-beam-1"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Pulled", "Pruning" -> -1}],
    FindSegment::badpruning],
  $Failed,
  TestID -> "FindSegment-Pulled-bad-pruning"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    Length[FindSegment[g, 1, 36, 1, Method -> "Pulled"]]
  ],
  1,
  TestID -> "FindSegment-Pulled-count-1-terminates-early"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    Length[FindSegment[g, 1, 36, 1, Method -> "Extended"]]
  ],
  1,
  TestID -> "FindSegment-Extended-count-1-terminates-early"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "Pulled"],
  {},
  TestID -> "FindSegment-Pulled-same-point-empty"
]


(* ===== FindSegment Method -> "Pulled": Constraint -> "Geodesic" (default) ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> "Pulled"],
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SubsetQ[
      Sort @ FindSegment[g, 1, 9, All, Method -> "Shortest"],
      Sort @ FindSegment[g, 1, 9, All, Method -> "Pulled"]
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-Constraint-default-subset-of-geodesics"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    FindSegment[g, 1, 3, All, Method -> "Pulled"]
  ],
  {{1, 3}},
  TestID -> "FindSegment-Pulled-Constraint-default-target-adjacent"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Pulled", "Constraint" -> "Bogus"}],
    FindSegment::badconstraint],
  $Failed,
  TestID -> "FindSegment-Pulled-bad-Constraint"
]


(* ===== FindSegment Method -> "Pulled": CurvatureMethod -> "Wolfram" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> {"Pulled", "CurvatureMethod" -> "Wolfram"}]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindSegment-Pulled-Wolfram-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> {"Pulled", "CurvatureMethod" -> "Wolfram"}]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-Pulled-Wolfram-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> {"Pulled", "CurvatureMethod" -> "Wolfram"}],
      walk |-> First[walk] === 1 && Last[walk] === 9 &&
        DuplicateFreeQ[walk] &&
        AllTrue[Partition[walk, 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-Wolfram-grid-walks-valid"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> {"Pulled", "CurvatureMethod" -> "Wolfram"}],
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-Pulled-Wolfram-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, All,
      Method -> {"Pulled", "CurvatureMethod" -> "Wolfram", "Dimension" -> 2}]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindSegment-Pulled-Wolfram-Dimension-fixed-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, All,
      Method -> {"Pulled", "CurvatureMethod" -> "Wolfram",
                 "Dimension" -> 2, "Radii" -> {1, 2}}]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindSegment-Pulled-Wolfram-Radii-explicit-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "Pulled"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"Pulled", "CurvatureMethod" -> "Forman"}]
  ],
  True,
  TestID -> "FindSegment-Pulled-CurvatureMethod-default-is-Forman"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Pulled", "CurvatureMethod" -> "Bogus"}],
    FindSegment::badcurvature],
  $Failed,
  TestID -> "FindSegment-Pulled-bad-CurvatureMethod"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9,
      Method -> {"Pulled", "CurvatureMethod" -> "Wolfram", "Dimension" -> "two"}],
    FindSegment::baddim],
  $Failed,
  TestID -> "FindSegment-Pulled-Wolfram-bad-dimension"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9,
      Method -> {"Pulled", "CurvatureMethod" -> "Wolfram", "Radii" -> {3, 1}}],
    FindSegment::badradii],
  $Failed,
  TestID -> "FindSegment-Pulled-Wolfram-bad-radii"
]


(* ===== FindLine ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, 2, 4]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length[First @ FindLine[g, 2, 4]]
  ],
  5,
  TestID -> "FindLine-extends-to-full-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, 1, 5]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[CentralPaths[g, FindLine[g, 5, 6, All]], UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindLine-with-CentralPaths"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{exts = FindLine[g, 2, 4, UpTo[5]]},
      Length[exts] >= 1
    ]
  ],
  True,
  TestID -> "FindLine-upto-soft"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 3, All, "Maximality" -> "Extension"]
  ],
  {{1, 2, 3}},
  TestID -> "FindLine-Maximality-Extension-keeps-short-line"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 3, All, "Maximality" -> "Diameter"]
  ],
  {},
  TestID -> "FindLine-Maximality-Diameter-drops-short-line"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 5, All, "Maximality" -> "Diameter"]
  ],
  {{1, 2, 4, 5}},
  TestID -> "FindLine-Maximality-Diameter-keeps-diameter-line"
]

(* ===== FindShell ===== *)

(* Method -> "Metric" (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ First @ FindShell[g, 3, 2]
  ],
  {1, 5},
  TestID -> "FindShell-Metric-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindShell[g, 6, {1, 2}, All]},
      Length[result] == 1 &&
      AllTrue[First[result], v |-> 1 <= GraphDistance[g, 6, v] <= 2]
    ]
  ],
  True,
  TestID -> "FindShell-Metric-range-radius"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindShell[g, 1, 2, All]
  ],
  1,
  TestID -> "FindShell-Metric-single-result"
]

(* Method -> "Separating": minimal connected separators within the level surface. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindShell[g, 6, {1, 2}, All, Method -> "Separating"]},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]] &&
      AllTrue[shells, vs |-> ConnectedGraphQ[Subgraph[g, vs]]]
    ]
  ],
  True,
  TestID -> "FindShell-Separating-connected-within-range"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindShell[g, 6, {1, 2}, All, Method -> "Separating"]},
      AllTrue[shells, vs |-> AllTrue[shells,
        other |-> other === vs || ! (Length[other] < Length[vs] && SubsetQ[vs, other])
      ]]
    ]
  ],
  True,
  TestID -> "FindShell-Separating-minimal"
]

(* ===== FindCircle ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = Take[
        LongestCircumferenceCycles @ FindCircle[g, 6, {1, 2}, All],
        UpTo[1]]},
      Length[circles] >= 1 && AllTrue[circles, ListQ]
    ]
  ],
  True,
  TestID -> "FindCircle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{circles = FindCircle[g, 1, {1, 2}, All]},
      Length[circles] >= 1
    ]
  ],
  True,
  TestID -> "FindCircle-all-cycles"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = LongestCircumferenceCycles @ FindCircle[g, 6, {1, 2}, All]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-LongestCircumferenceCycles-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = ShortestCircumferenceCycles @ FindCircle[g, 6, {1, 2}, All]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-ShortestCircumferenceCycles"
]

(* ===== FindParallel ===== *)

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  {{5, 6, 7, 8}},
  TestID -> "FindParallel-GridGraph-row-from-row"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  {{5, 6, 7, 8}},
  TestID -> "FindParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  FindParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindParallel-self-on-line"
]

VerificationTest[
  FindParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  {},
  TestID -> "FindParallel-disconnected-empty"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  {{5, 6, 7, 8}},
  TestID -> "FindParallel-strict-1"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindParallel-strict-fails-when-too-few"
]

VerificationTest[
  FindParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  {},
  TestID -> "FindParallel-CycleGraph-no-parallel"
]

VerificationTest[
  ParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    First @ FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]],
  True,
  TestID -> "FindParallel-output-passes-ParallelQ"
]


(* ===== FindSegment Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindSegment[ g, 1, 16, All, Method -> "Embedding" ] ===
      Sort @ FindSegment[ g, 1, 16, All, Method -> "Shortest" ]
  ],
  True,
  TestID -> "FindSegment-Embedding-Geodesic-All-equals-Shortest-set"
]

VerificationTest[
  With[ { paths = FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, All, Method -> "Embedding" ] },
    Length[ paths ] >= 1 && AllTrue[ paths, First[ # ] === 1 && Last[ # ] === 16 & ]
  ],
  True,
  TestID -> "FindSegment-Embedding-paths-have-correct-endpoints"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], coords = GraphEmbedding[ GridGraph[ { 4, 4 } ] ] },
    FindSegment[ g, 1, 16, 1, Method -> { "Embedding", "Coordinates" -> coords } ] ===
      FindSegment[ g, 1, 16, 1, Method -> "Embedding" ]
  ],
  True,
  TestID -> "FindSegment-Embedding-explicit-coords-matches-Automatic"
]

VerificationTest[
  Length @ FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, 1, Method -> { "Embedding", "Pruning" -> 1 } ],
  1,
  TestID -> "FindSegment-Embedding-Pruning-beam-one"
]

VerificationTest[
  FindSegment[ PathGraph[ Range[ 5 ] ], 1, 5, 1, Method -> { "Embedding", "Constraint" -> "Free" } ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "FindSegment-Embedding-Free-PathGraph-unique-path"
]


(* ===== FindLine Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindLine[ g, 1, 16, All, Method -> "Embedding" ] === Sort @ FindLine[ g, 1, 16, All ]
  ],
  True,
  TestID -> "FindLine-Embedding-set-equals-default"
]


(* ===== FindShell Method -> "Embedding" ===== *)

VerificationTest[
  Length @ Flatten @ FindShell[ GridGraph[ { 4, 4 } ], 6, 1, All, Method -> "Embedding" ],
  Length @ Select[ VertexList[ GridGraph[ { 4, 4 } ] ],
    GraphDistance[ GridGraph[ { 4, 4 } ], 6, # ] == 1 & ],
  TestID -> "FindShell-Embedding-Geodesic-pool-equals-level-surface"
]

VerificationTest[
  Length @ FindShell[ GridGraph[ { 4, 4 } ], 6, 1, All, Method -> { "Embedding", "Constraint" -> "Free" } ],
  16,
  TestID -> "FindShell-Embedding-Free-pool-equals-all-vertices"
]


(* ===== FindCircle Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindCircle[ g, 6, { 1, 2 }, All, Method -> "Embedding" ] ===
      Sort @ FindCircle[ g, 6, { 1, 2 }, All ]
  ],
  True,
  TestID -> "FindCircle-Embedding-set-equals-default"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ FindCircle[ g, 6, { 1, 2 }, All, Method -> "Embedding" ] >= 1
  ],
  True,
  TestID -> "FindCircle-Embedding-non-empty-on-grid-with-cycles"
]


(* ===== FindParallel Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindParallel[ g, { 1, 2, 3, 4 }, 5, All, Method -> "Embedding" ] ===
      Sort @ FindParallel[ g, { 1, 2, 3, 4 }, 5, All ]
  ],
  True,
  TestID -> "FindParallel-Embedding-set-equals-default"
]

EndTestSection[]
