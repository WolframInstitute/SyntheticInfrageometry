BeginTestSection["EuclideanPostulates"]

(* ===== FindPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindPoint[g]},
      Length @ pt == 1 && SubsetQ[VertexList[g], (#[[ 1, 1 ]] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindPoint[g, 3]},
      Length @ pts == 3 && DuplicateFreeQ[(#[[ 1, 1 ]] & /@ pts)] && SubsetQ[VertexList[g], (#[[ 1, 1 ]] & /@ pts)]
    ]
  ],
  True,
  TestID -> "FindPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], (#[[ 1, 1 ]] & /@ FindPoint[g, 1, "From" -> "Center"])]
  ],
  True,
  TestID -> "FindPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], (#[[ 1, 1 ]] & /@ FindPoint[g, 1, "From" -> "Periphery"])]
  ],
  True,
  TestID -> "FindPoint-from-periphery"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindPoint[g, 2, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindPoint[g, 1, "From" -> {2, 3, 4}]},
      Length @ pt == 1 && SubsetQ[{2, 3, 4}, (#[[ 1, 1 ]] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4])},
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
    Length @ pts == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], (#[[ 1, 1 ]] & /@ pts)]
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
      Length @ pt == 1 && GraphDistance[g, 3, First @ First @ First @ pt] == 2
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindPoint[g, UpTo[20], "From" -> 1 -> {2, 3}])},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = (#[[ 1, 1 ]] & /@ FindPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"])},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-max-distance"
]

VerificationTest[
  MatchQ[ FindPoint[ PetersenGraph[] ], { InfraPoint[ { _ } ] .. } ],
  True,
  TestID -> "FindPoint-returns-list-of-unary-InfraPoint"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindPoint[g, UpTo[VertexCount[g]],
      "From" -> InfraPoint[{1, 16}] -> 3])},
      AllTrue[pts, GraphDistance[g, 1, #] == 3 && GraphDistance[g, 16, #] == 3 &]
    ]
  ],
  True,
  TestID -> "FindPoint-multi-anchor-intersection"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindPoint[g, UpTo[VertexCount[g]], "From" -> InfraPoint[{2, 5, 7}]])
  ],
  {2, 5, 7},
  TestID -> "FindPoint-multi-anchor-pool-no-distance"
]

(* ===== FindSegment ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 3])},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All])},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectPath[g, FindSegment[g, 1, 9, All], All, "From" -> "Center", "Metric" -> "Frechet"])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-SelectPath-Center-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ EmbeddingClosestPaths[g, FindSegment[g, 1, 9, All], {1, 9}])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-EmbeddingClosestPaths"
]

VerificationTest[
  InfraSegment @ FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  InfraSegment[{}],
  TestID -> "FindSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectPath[g, FindSegment[g, 1, 9, All], All, "From" -> "Center", "Metric" -> "Hausdorff"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-SelectPath-Center-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectPath[g, FindSegment[g, 1, 9, All], All, "From" -> "Periphery"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-SelectPath-Periphery"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosestPaths[g, {1, 9}] @ SelectPath[g, All, "From" -> "Center"] @
        (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, UpTo[2]])},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-upto-soft-cap"
]

(* ===== FindSegment Method -> "ShortestPathExtension" ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "ShortestPathExtension"]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-ShortestPathExtension-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 4, All, Method -> "ShortestPathExtension"])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-ShortestPathExtension-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]) ===
      Sort @ FindPath[g, 1, 9, Infinity, All]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-K1-equals-FindPath"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]),
      k2   = Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}]),
      kAll = Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> All}])
    },
      Length[k1] == 4 &&
      MemberQ[k1, {1, 2, 4, 3}] && MemberQ[k1, {1, 4, 2, 3}] &&
      k2 === Sort[{{1, 2, 3}, {1, 4, 3}}] &&
      kAll === k2
    ]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-K2-strict-between"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Length @ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}],
      k2   = Length @ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}],
      kAll = Length @ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> All}]
    },
      k1 >= k2 >= kAll
    ]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-K-monotone"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "ShortestPathExtension"]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}])
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-ShortestPathWindow-default-is-2"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ FindSegment[g, 1, 16, All,
        Method -> {"ShortestPathExtension", "Pruning" -> 1}] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-pruning-beam-1"
]

VerificationTest[
  InfraSegment @ FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "ShortestPathExtension"],
  InfraSegment[{}],
  TestID -> "FindSegment-ShortestPathExtension-same-point-empty"
]


(* ===== FindSegment Method -> "CurvatureMinimizing" ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "CurvatureMinimizing"]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-CurvatureMinimizing-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 4, All, Method -> "CurvatureMinimizing"])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-CurvatureMinimizing-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]),
      walk |-> First[walk] === 1 && Last[walk] === 9 &&
        DuplicateFreeQ[walk] &&
        AllTrue[Partition[walk, 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-grid-walks-valid"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Pool" -> "AllPaths"}]
  ],
  6,
  TestID -> "FindSegment-CurvatureMinimizing-AllPaths-grid-bundle-size"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]),
      walk |-> Length[walk] - 1 >= GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-grid-walks-no-shorter-than-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Simple"}}])
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Forman-Method-default-is-Simple"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 3, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Simple"}, "Pool" -> "AllPaths"}]) =!=
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 3, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Triangles"}, "Pool" -> "AllPaths"}])
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-AllPaths-Forman-Method-Triangles-differs-on-triangulated"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, UpTo[2], Method -> "CurvatureMinimizing"]
  ],
  2,
  TestID -> "FindSegment-CurvatureMinimizing-UpTo-truncates"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, 3, Method -> "CurvatureMinimizing"]
  ],
  3,
  TestID -> "FindSegment-CurvatureMinimizing-Count-exact"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SelectPath[g, All, "From" -> "Center"] @ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]
  ],
  { InfraSegment[ { _ } ] .. },
  SameTest -> MatchQ,
  TestID -> "FindSegment-CurvatureMinimizing-chains-with-SelectPath"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ FindSegment[g, 1, 16, All, Method -> {"CurvatureMinimizing", "Pruning" -> 1}] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-pruning-beam-1"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    Length @ FindSegment[g, 1, 36, 1, Method -> "CurvatureMinimizing"]
  ],
  1,
  TestID -> "FindSegment-CurvatureMinimizing-count-1-terminates-early"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    Length @ FindSegment[g, 1, 36, 1, Method -> "ShortestPathExtension"]
  ],
  1,
  TestID -> "FindSegment-ShortestPathExtension-count-1-terminates-early"
]

VerificationTest[
  InfraSegment @ FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "CurvatureMinimizing"],
  InfraSegment[{}],
  TestID -> "FindSegment-CurvatureMinimizing-same-point-empty"
]


(* ===== FindSegment Method -> "CurvatureMinimizing": Pool -> "ShortestPaths" (default) ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]),
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SubsetQ[
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "Shortest"]),
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"])
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Constraint-default-subset-of-geodesics"
]

VerificationTest[
  InfraSegment @ With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    FindSegment[g, 1, 3, All, Method -> "CurvatureMinimizing"]
  ],
  InfraSegment[{{1, 3}}],
  TestID -> "FindSegment-CurvatureMinimizing-Constraint-default-target-adjacent"
]

(* ===== FindSegment Method -> "CurvatureMinimizing": CurvatureMethod -> "Wolfram" ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 4, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]),
      walk |-> First[walk] === 1 && Last[walk] === 9 &&
        DuplicateFreeQ[walk] &&
        AllTrue[Partition[walk, 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-grid-walks-valid"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]),
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, All,
      Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram", "Dimension" -> 2}]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-Dimension-fixed-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindSegment[g, 1, 9, All,
      Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram",
                 "Dimension" -> 2, "Radii" -> {1, 2}}]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-Radii-explicit-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Forman"}])
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-CurvatureMethod-default-is-Forman"
]

(* ===== FindLine ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindLine[g, 2, 4]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ First @ First @ First @ FindLine[g, 2, 4]
  ],
  5,
  TestID -> "FindLine-extends-to-full-path"
]

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindLine[g, 1, 5]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[(#[[ 1, 1 ]] & /@ SelectPath[g, FindLine[g, 5, 6, All], All, "From" -> "Center"]), UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindLine-with-SelectPath-Center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindLine[g, 2, 4, UpTo[5]] >= 1
  ],
  True,
  TestID -> "FindLine-upto-soft"
]

VerificationTest[
  InfraSegment @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 3, All, "Maximality" -> "Extension"]
  ],
  InfraSegment[{{1, 2, 3}}],
  TestID -> "FindLine-Maximality-Extension-keeps-short-line"
]

VerificationTest[
  InfraSegment @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 3, All, "Maximality" -> "Diameter"]
  ],
  InfraSegment[{}],
  TestID -> "FindLine-Maximality-Diameter-drops-short-line"
]

VerificationTest[
  InfraSegment @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 5, All, "Maximality" -> "Diameter"]
  ],
  InfraSegment[{{1, 2, 4, 5}}],
  TestID -> "FindLine-Maximality-Diameter-keeps-diameter-line"
]

(* ===== FindShell ===== *)

(* Method -> "Metric" (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ First @ First @ First @ FindShell[g, 3, 2]
  ],
  {1, 5},
  TestID -> "FindShell-Metric-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindShell[g, 6, {1, 2}, All]},
      Length @ result == 1 &&
      AllTrue[First @ First @ First @ result, v |-> 1 <= GraphDistance[g, 6, v] <= 2]
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
    With[{shells = (#[[ 1, 1 ]] & /@ FindShell[g, 6, {1, 2}, All, Method -> "Separating"])},
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
    With[{shells = (#[[ 1, 1 ]] & /@ FindShell[g, 6, {1, 2}, All, Method -> "Separating"])},
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
        SelectCycle[g, (#[[ 1, 1 ]] & /@ FindCircle[g, 6, {1, 2}, All]), All, "From" -> "LongestCircumference"],
        UpTo[1]]},
      Length[circles] >= 1 && AllTrue[circles, ListQ]
    ]
  ],
  True,
  TestID -> "FindCircle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindCircle[g, 1, {1, 2}, All] >= 1
  ],
  True,
  TestID -> "FindCircle-all-cycles"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectCycle[g, (#[[ 1, 1 ]] & /@ FindCircle[g, 6, {1, 2}, All]), All, "From" -> "LongestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-SelectCycle-LongestCircumference-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectCycle[g, (#[[ 1, 1 ]] & /@ FindCircle[g, 6, {1, 2}, All]), All, "From" -> "ShortestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-SelectCycle-ShortestCircumference"
]

(* ===== FindParallel ===== *)

VerificationTest[
  InfraSegment @ FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-GridGraph-row-from-row"
]

VerificationTest[
  InfraSegment @ FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  InfraSegment @ FindParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindParallel-self-on-line"
]

VerificationTest[
  InfraSegment @ FindParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  InfraSegment[{}],
  TestID -> "FindParallel-disconnected-empty"
]

VerificationTest[
  InfraSegment @ FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-strict-1"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindParallel-strict-fails-when-too-few"
]

VerificationTest[
  InfraSegment @ FindParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  InfraSegment[{}],
  TestID -> "FindParallel-CycleGraph-no-parallel"
]

VerificationTest[
  ParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    First @ First @ First @ FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]],
  True,
  TestID -> "FindParallel-output-passes-ParallelQ"
]


(* ===== FindSegment Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindSegment[ g, 1, 16, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindSegment[ g, 1, 16, All, Method -> "Shortest" ])
  ],
  True,
  TestID -> "FindSegment-Embedding-Geodesic-All-equals-Shortest-set"
]

VerificationTest[
  With[ { paths = (#[[ 1, 1 ]] & /@ FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, All, Method -> "Embedding" ]) },
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
  InfraSegment @ FindSegment[ PathGraph[ Range[ 5 ] ], 1, 5, 1, Method -> { "Embedding", "Pool" -> "AllPaths" } ],
  InfraSegment[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindSegment-Embedding-AllPaths-PathGraph-unique-path"
]


(* ===== FindLine Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindLine[ g, 1, 16, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindLine[ g, 1, 16, All ])
  ],
  True,
  TestID -> "FindLine-Embedding-set-equals-default"
]


(* ===== FindShell Method -> "Embedding" ===== *)

VerificationTest[
  Length @ Flatten @ (#[[ 1, 1 ]] & /@ FindShell[ GridGraph[ { 4, 4 } ], 6, 1, All, Method -> "Embedding" ]),
  Length @ Select[ VertexList[ GridGraph[ { 4, 4 } ] ],
    GraphDistance[ GridGraph[ { 4, 4 } ], 6, # ] == 1 & ],
  TestID -> "FindShell-Embedding-Geodesic-pool-equals-level-surface"
]

VerificationTest[
  Length @ FindShell[ GridGraph[ { 4, 4 } ], 6, 1, All, Method -> { "Embedding", "Pool" -> "AllVertices" } ],
  16,
  TestID -> "FindShell-Embedding-AllPaths-pool-equals-all-vertices"
]


(* ===== FindCircle Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindCircle[ g, 6, { 1, 2 }, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindCircle[ g, 6, { 1, 2 }, All ])
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
    Sort @ (#[[ 1, 1 ]] & /@ FindParallel[ g, { 1, 2, 3, 4 }, 5, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindParallel[ g, { 1, 2, 3, 4 }, 5, All ])
  ],
  True,
  TestID -> "FindParallel-Embedding-set-equals-default"
]


(* ===== FindPoint All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindPoint[ g, All ]) === VertexList[ g ]
  ],
  True,
  TestID -> "FindPoint-All-returns-every-vertex"
]

VerificationTest[
  With[ { g = PetersenGraph[ ] },
    Length @ FindPoint[ g, All ] == VertexCount[ g ]
  ],
  True,
  TestID -> "FindPoint-All-length-equals-vertex-count"
]


(* ===== FindLine unified Method axis ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindLine[ g, 1, 16, All, Method -> "Shortest" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindLine[ g, 1, 16, All, Method -> Automatic ])
  ],
  True,
  TestID -> "FindLine-Shortest-equals-Automatic"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { lines = FindLine[ g, 1, 16, All, Method -> "ShortestPathExtension" ] },
      MatchQ[ lines, { InfraSegment[ { _ } ] .. } ] &&
        Length @ lines >= Length @ FindLine[ g, 1, 16, All, Method -> "Shortest" ]
    ]
  ],
  True,
  TestID -> "FindLine-ShortestPathExtension-superset-of-Shortest"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { lines = FindLine[ g, 1, 16, All, Method -> "CurvatureMinimizing" ] },
      MatchQ[ lines, { InfraSegment[ { _ } ] .. } ] && Length @ lines >= 1
    ]
  ],
  True,
  TestID -> "FindLine-CurvatureMinimizing-non-empty"
]


(* ===== ExtendSegment ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindSegment[ g, 1, 6, All ] },
      With[ { lines = (#[[ 1, 1 ]] & /@ ExtendSegment[ g, seg, All ]) },
        ListQ[ lines ] && AllTrue[ lines,
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
      ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-Shortest-contains-segment"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindSegment[ g, 1, 6, All ] },
      Sort @ (#[[ 1, 1 ]] & /@ ExtendSegment[ g, seg, All, Method -> "Shortest" ]) ===
        Sort @ Select[ (#[[ 1, 1 ]] & /@ FindLine[ g, 1, 6, All ]),
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-Shortest-matches-FindLine-filtered"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindSegment[ g, 1, 6, All ] },
      MatchQ[ ExtendSegment[ g, seg, All, Method -> "ShortestPathExtension" ], { __InfraSegment } ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-ShortestPathExtension-returns-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindSegment[ g, 1, 6, All ] },
      MatchQ[ ExtendSegment[ g, seg, All, Method -> "CurvatureMinimizing" ], { __InfraSegment } ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-CurvatureMinimizing-returns-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindSegment[ g, 1, 6, All ] },
      With[ { lines = ExtendSegment[ g, seg, 1, Method -> "Embedding" ] },
        Length @ lines == 1 &&
          MemberQ[ Partition[ First @ First @ First @ lines, Length @ seg, 1 ], seg ]
      ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-Embedding-contains-segment"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    InfraSegment @ ExtendSegment[ g, { 2, 3 }, 1, Method -> "Shortest" ] === InfraSegment[ { { 1, 2, 3, 4, 5 } } ]
  ],
  True,
  TestID -> "ExtendSegment-PathGraph-recovers-full-path"
]

VerificationTest[
  ExtendSegment[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 99, Method -> "Shortest" ],
  $Failed,
  TestID -> "ExtendSegment-strict-undersupply-Failed"
]

EndTestSection[]
