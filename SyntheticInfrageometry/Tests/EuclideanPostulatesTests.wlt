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

(* ===== FindSegment Method -> "Stretched" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "Stretched"]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindSegment-Stretched-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> "Stretched"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-Stretched-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> {"Stretched", "Lookback" -> 1}] ===
      Sort @ FindPath[g, 1, 9, Infinity, All]
  ],
  True,
  TestID -> "FindSegment-Stretched-K1-equals-FindPath"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SubsetQ[
      Sort @ FindSegment[g, 1, 9, All, Method -> {"Stretched", "Lookback" -> All}],
      Sort @ FindSegment[g, 1, 9, All, Method -> "Shortest"]
    ]
  ],
  True,
  TestID -> "FindSegment-Stretched-KAll-superset-geodesics"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Sort @ FindSegment[g, 1, 3, All, Method -> {"Stretched", "Lookback" -> 1}],
      k2   = Sort @ FindSegment[g, 1, 3, All, Method -> {"Stretched", "Lookback" -> 2}],
      kAll = Sort @ FindSegment[g, 1, 3, All, Method -> {"Stretched", "Lookback" -> All}]
    },
      Length[k1] == 4 &&
      MemberQ[k1, {1, 2, 4, 3}] && MemberQ[k1, {1, 4, 2, 3}] &&
      k2 === Sort[{{1, 2, 3}, {1, 4, 3}}] &&
      kAll === k2
    ]
  ],
  True,
  TestID -> "FindSegment-Stretched-K2-strict-between"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Length @ FindSegment[g, 1, 3, All, Method -> {"Stretched", "Lookback" -> 1}],
      k2   = Length @ FindSegment[g, 1, 3, All, Method -> {"Stretched", "Lookback" -> 2}],
      kAll = Length @ FindSegment[g, 1, 3, All, Method -> {"Stretched", "Lookback" -> All}]
    },
      k1 >= k2 >= kAll
    ]
  ],
  True,
  TestID -> "FindSegment-Stretched-K-monotone"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "Stretched"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"Stretched", "Lookback" -> 2}]
  ],
  True,
  TestID -> "FindSegment-Stretched-Lookback-default-is-2"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length[FindSegment[g, 1, 16, All,
        Method -> {"Stretched", "Pruning" -> 1}]] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-Stretched-pruning-beam-1"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> "Bogus"],
    FindSegment::badmethod],
  $Failed,
  TestID -> "FindSegment-bad-method"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Stretched", "Pruning" -> -1}],
    FindSegment::badpruning],
  $Failed,
  TestID -> "FindSegment-bad-pruning"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Stretched", "Lookback" -> 0}],
    FindSegment::badlookback],
  $Failed,
  TestID -> "FindSegment-bad-lookback-zero"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Stretched", "Lookback" -> -1}],
    FindSegment::badlookback],
  $Failed,
  TestID -> "FindSegment-bad-lookback-negative"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"Stretched", "Lookback" -> 1.5}],
    FindSegment::badlookback],
  $Failed,
  TestID -> "FindSegment-bad-lookback-fractional"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "Stretched"],
  {},
  TestID -> "FindSegment-Stretched-same-point-empty"
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

EndTestSection[]
