BeginTestSection["EuclideanPostulates"]

(* ===== FindInfraPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g]},
      Length @ pt == 1 && SubsetQ[VertexList[g], (#[[ 1, 1 ]] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindInfraPoint[g, 3]},
      Length @ pts == 3 && DuplicateFreeQ[(#[[ 1, 1 ]] & /@ pts)] && SubsetQ[VertexList[g], (#[[ 1, 1 ]] & /@ pts)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 1, "From" -> "Center"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 1, "From" -> "Periphery"])]
  ],
  True,
  TestID -> "FindInfraPoint-from-periphery"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 2, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindInfraPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindInfraPoint[g, 1, "From" -> {2, 3, 4}]},
      Length @ pt == 1 && SubsetQ[{2, 3, 4}, (#[[ 1, 1 ]] & /@ pt)]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4])},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindInfraPoint-vertex-list-with-distance"
]

VerificationTest[
  FindInfraPoint[PathGraph[Range[3]], 10],
  $Failed,
  TestID -> "FindInfraPoint-exact-fails-when-too-few"
]

VerificationTest[
  With[{pts = FindInfraPoint[PathGraph[Range[3]], UpTo[10]]},
    Length @ pts == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], (#[[ 1, 1 ]] & /@ pts)]
  ],
  True,
  TestID -> "FindInfraPoint-upto-returns-available"
]

VerificationTest[
  FindInfraPoint[PathGraph[Range[3]], 3, "Distance" -> 5],
  $Failed,
  TestID -> "FindInfraPoint-exact-fails-impossible-distance"
]

VerificationTest[
  With[{g = PathGraph[Range[7]]},
    With[{pt = FindInfraPoint[g, 1, "From" -> 3 -> 2]},
      Length @ pt == 1 && GraphDistance[g, 3, First @ First @ First @ pt] == 2
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[20], "From" -> 1 -> {2, 3}])},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"])},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-from-origin-max-distance"
]

VerificationTest[
  MatchQ[ FindInfraPoint[ PetersenGraph[] ], { InfraPoint[ { _ } ] .. } ],
  True,
  TestID -> "FindInfraPoint-returns-list-of-unary-InfraPoint"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[VertexCount[g]],
      "From" -> InfraPoint[{1, 16}] -> 3])},
      AllTrue[pts, GraphDistance[g, 1, #] == 3 && GraphDistance[g, 16, #] == 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraPoint-multi-anchor-intersection"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraPoint[g, UpTo[VertexCount[g]], "From" -> InfraPoint[{2, 5, 7}]])
  ],
  {2, 5, 7},
  TestID -> "FindInfraPoint-multi-anchor-pool-no-distance"
]

(* ===== FindInfraSegment ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindInfraSegment[g, 1, 5]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3])},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindInfraSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All])},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraSegment[g, 1, 9, All], All, "From" -> "Center", "Metric" -> "Frechet"])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraPath-Center-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ EmbeddingClosest[g, FindInfraSegment[g, 1, 9, All], {1, 9}])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-EmbeddingClosest"
]

VerificationTest[
  InfraSegment @ FindInfraSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  InfraSegment[{}],
  TestID -> "FindInfraSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraSegment[g, 1, 9, All], All, "From" -> "Center", "Metric" -> "Hausdorff"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraPath-Center-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraSegment[g, 1, 9, All], All, "From" -> "Periphery"])},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindInfraSegment-SelectInfraPath-Periphery"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosest[g, {1, 9}] @ SelectInfraPath[g, All, "From" -> "Center"] @
        (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All])},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, UpTo[2]])},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-upto-soft-cap"
]

(* ===== FindInfraSegment Method -> "ShortestPathExtension" ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindInfraSegment[g, 1, 5, All, Method -> "ShortestPathExtension"]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraSegment-ShortestPathExtension-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 4, All, Method -> "ShortestPathExtension"])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraSegment-ShortestPathExtension-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]) ===
      Sort @ FindPath[g, 1, 9, Infinity, All]
  ],
  True,
  TestID -> "FindInfraSegment-ShortestPathExtension-K1-equals-FindPath"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]),
      k2   = Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}]),
      kAll = Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> All}])
    },
      Length[k1] == 4 &&
      MemberQ[k1, {1, 2, 4, 3}] && MemberQ[k1, {1, 4, 2, 3}] &&
      k2 === Sort[{{1, 2, 3}, {1, 4, 3}}] &&
      kAll === k2
    ]
  ],
  True,
  TestID -> "FindInfraSegment-ShortestPathExtension-K2-strict-between"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Length @ FindInfraSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}],
      k2   = Length @ FindInfraSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}],
      kAll = Length @ FindInfraSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> All}]
    },
      k1 >= k2 >= kAll
    ]
  ],
  True,
  TestID -> "FindInfraSegment-ShortestPathExtension-K-monotone"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "ShortestPathExtension"]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}])
  ],
  True,
  TestID -> "FindInfraSegment-ShortestPathExtension-ShortestPathWindow-default-is-2"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ FindInfraSegment[g, 1, 16, All,
        Method -> {"ShortestPathExtension", "Pruning" -> 1}] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraSegment-ShortestPathExtension-pruning-beam-1"
]

VerificationTest[
  InfraSegment @ FindInfraSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "ShortestPathExtension"],
  InfraSegment[{}],
  TestID -> "FindInfraSegment-ShortestPathExtension-same-point-empty"
]


(* ===== FindInfraSegment Method -> "CurvatureMinimizing" ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindInfraSegment[g, 1, 5, All, Method -> "CurvatureMinimizing"]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraSegment-CurvatureMinimizing-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 4, All, Method -> "CurvatureMinimizing"])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraSegment-CurvatureMinimizing-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]),
      walk |-> First[walk] === 1 && Last[walk] === 9 &&
        DuplicateFreeQ[walk] &&
        AllTrue[Partition[walk, 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-grid-walks-valid"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Pool" -> "AllPaths"}]
  ],
  6,
  TestID -> "FindInfraSegment-CurvatureMinimizing-AllPaths-grid-bundle-size"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]),
      walk |-> Length[walk] - 1 >= GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-grid-walks-no-shorter-than-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"FormanRicciCurvature", Method -> "Simple"}}])
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Forman-Method-default-is-Simple"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"FormanRicciCurvature", Method -> "Simple"}, "Pool" -> "AllPaths"}]) =!=
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 3, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"FormanRicciCurvature", Method -> "Triangles"}, "Pool" -> "AllPaths"}])
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-AllPaths-Forman-Method-Triangles-differs-on-triangulated"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, UpTo[2], Method -> "CurvatureMinimizing"]
  ],
  2,
  TestID -> "FindInfraSegment-CurvatureMinimizing-UpTo-truncates"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, 3, Method -> "CurvatureMinimizing"]
  ],
  3,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Count-exact"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SelectInfraPath[g, All, "From" -> "Center"] @ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]
  ],
  { InfraSegment[ { _ } ] .. },
  SameTest -> MatchQ,
  TestID -> "FindInfraSegment-CurvatureMinimizing-chains-with-SelectInfraPath"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      Length @ FindInfraSegment[g, 1, 16, All, Method -> {"CurvatureMinimizing", "Pruning" -> 1}] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-pruning-beam-1"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    Length @ FindInfraSegment[g, 1, 36, 1, Method -> "CurvatureMinimizing"]
  ],
  1,
  TestID -> "FindInfraSegment-CurvatureMinimizing-count-1-terminates-early"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    Length @ FindInfraSegment[g, 1, 36, 1, Method -> "ShortestPathExtension"]
  ],
  1,
  TestID -> "FindInfraSegment-ShortestPathExtension-count-1-terminates-early"
]

VerificationTest[
  InfraSegment @ FindInfraSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "CurvatureMinimizing"],
  InfraSegment[{}],
  TestID -> "FindInfraSegment-CurvatureMinimizing-same-point-empty"
]


(* ===== FindInfraSegment Method -> "CurvatureMinimizing": Pool -> "ShortestPaths" (default) ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]),
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SubsetQ[
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "ShortestPath"]),
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"])
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Constraint-default-subset-of-geodesics"
]

VerificationTest[
  InfraSegment @ With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    FindInfraSegment[g, 1, 3, All, Method -> "CurvatureMinimizing"]
  ],
  InfraSegment[{{1, 3}}],
  TestID -> "FindInfraSegment-CurvatureMinimizing-Constraint-default-target-adjacent"
]

(* ===== FindInfraSegment Method -> "CurvatureMinimizing": CurvatureMethod -> "WolframRicciCurvature" ===== *)

VerificationTest[
  InfraSegment @ With[{g = PathGraph[Range[5]]},
    FindInfraSegment[g, 1, 5, All, Method -> {"CurvatureMinimizing", "Curvature" -> "WolframRicciCurvature"}]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraSegment-CurvatureMinimizing-Wolfram-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 4, All, Method -> {"CurvatureMinimizing", "Curvature" -> "WolframRicciCurvature"}])
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindInfraSegment-CurvatureMinimizing-Wolfram-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "WolframRicciCurvature"}]),
      walk |-> First[walk] === 1 && Last[walk] === 9 &&
        DuplicateFreeQ[walk] &&
        AllTrue[Partition[walk, 2, 1], EdgeQ[g, UndirectedEdge @@ #] &]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Wolfram-grid-walks-valid"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "WolframRicciCurvature"}]),
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Wolfram-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, All,
      Method -> {"CurvatureMinimizing", "Curvature" -> "WolframRicciCurvature", "Dimension" -> 2}]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Wolfram-Dimension-fixed-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Length @ FindInfraSegment[g, 1, 9, All,
      Method -> {"CurvatureMinimizing", "Curvature" -> "WolframRicciCurvature",
                 "Dimension" -> 2, "Radii" -> {1, 2}}]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindInfraSegment-CurvatureMinimizing-Wolfram-Radii-explicit-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "FormanRicciCurvature"}])
  ],
  True,
  TestID -> "FindInfraSegment-CurvatureMinimizing-CurvatureMethod-default-is-Forman"
]

(* ===== FindInfraLine ===== *)

VerificationTest[
  InfraLine @ With[{g = PathGraph[Range[5]]},
    FindInfraLine[g, 2, 4]
  ],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ First @ First @ First @ FindInfraLine[g, 2, 4]
  ],
  5,
  TestID -> "FindInfraLine-extends-to-full-path"
]

VerificationTest[
  InfraLine @ With[{g = PathGraph[Range[5]]},
    FindInfraLine[g, 1, 5]
  ],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[(#[[ 1, 1 ]] & /@ SelectInfraPath[g, FindInfraLine[g, 5, 6, All], All, "From" -> "Center"]), UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindInfraLine-with-SelectInfraPath-Center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindInfraLine[g, 2, 4, UpTo[5]] >= 1
  ],
  True,
  TestID -> "FindInfraLine-upto-soft"
]

VerificationTest[
  InfraLine @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 3, All, "Maximality" -> "Extension"]
  ],
  InfraLine[{{1, 2, 3}}],
  TestID -> "FindInfraLine-Maximality-Extension-keeps-short-line"
]

VerificationTest[
  InfraLine @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 3, All, "Maximality" -> "Diameter"]
  ],
  InfraLine[{}],
  TestID -> "FindInfraLine-Maximality-Diameter-drops-short-line"
]

VerificationTest[
  InfraLine @ With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindInfraLine[g, 1, 5, All, "Maximality" -> "Diameter"]
  ],
  InfraLine[{{1, 2, 4, 5}}],
  TestID -> "FindInfraLine-Maximality-Diameter-keeps-diameter-line"
]

(* ===== FindInfraShell ===== *)

(* Properties -> {} (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ First @ First @ First @ FindInfraShell[g, 3, 2]
  ],
  {1, 5},
  TestID -> "FindInfraShell-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindInfraShell[g, 6, {1, 2}, All]},
      Length @ result == 1 &&
      AllTrue[First @ First @ First @ result, v |-> 1 <= GraphDistance[g, 6, v] <= 2]
    ]
  ],
  True,
  TestID -> "FindInfraShell-range-radius"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindInfraShell[g, 1, 2, All]
  ],
  1,
  TestID -> "FindInfraShell-default-single-result"
]

(* Properties -> {"Separating", "Connected"}: minimal connected separators. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = (#[[ 1, 1 ]] & /@ FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}])},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]] &&
      AllTrue[shells, vs |-> ConnectedGraphQ[Subgraph[g, vs]]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Sep-Connected-within-range"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = (#[[ 1, 1 ]] & /@ FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating", "Connected"}])},
      AllTrue[shells, vs |-> AllTrue[shells,
        other |-> other === vs || ! (Length[other] < Length[vs] && SubsetQ[vs, other])
      ]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Sep-Connected-minimal"
]

(* Properties -> {"Separating"} alone (no connectedness requirement).
   Every returned vs is inside the level-set range; we don't re-test
   separation here because SeparatingSetQ is PackageScope and the
   admissibility predicate is enforced inside findShellCore. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = (#[[ 1, 1 ]] & /@ FindInfraShell[g, 6, {1, 2}, All, Properties -> {"Separating"}])},
      Length[shells] >= 1 &&
      AllTrue[shells, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindInfraShell-Separating-only-no-connected-requirement"
]

(* Method -> "Greedy" returns a single realisation. *)

VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, All,
    Properties -> {"Separating", "Connected"}, Method -> "Greedy"],
  1,
  TestID -> "FindInfraShell-Greedy-single-realisation"
]

VerificationTest[
  FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, 2,
    Properties -> {"Separating", "Connected"}, Method -> "Greedy"],
  $Failed,
  TestID -> "FindInfraShell-Greedy-count-gt-1-fails"
]

(* Method -> {"Exhaustive", "Pruning" -> n} respects branching cap. *)

VerificationTest[
  Length @ FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, All,
    Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}] >= 1,
  True,
  TestID -> "FindInfraShell-Pruning-bounded-runs"
]

(* Unknown property name raises ::badproperty. *)

VerificationTest[
  FindInfraShell[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"NonExistent"}],
  $Failed,
  {FindInfraShell::badproperty},
  TestID -> "FindInfraShell-badproperty-message"
]

(* ===== FindInfraCircle ===== *)

(* Default (Properties -> {}, Method -> "Exhaustive"): cycles sorted by length. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = #[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All]},
      Length[circles] >= 1 && AllTrue[circles, Length[#] >= 3 &]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindInfraCircle[g, 1, {1, 2}, All] >= 1
  ],
  True,
  TestID -> "FindInfraCircle-all-cycles"
]

(* Cycles are returned sorted by length ascending. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{lengths = Length /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All])},
      lengths === Sort[lengths]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-Exhaustive-sorted-by-length"
]

(* count = 1 default returns the shortest cycle. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shortest = First @ First @ First @ FindInfraCircle[g, 6, {1, 2}],
          allLengths = Length /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All])},
      Length[shortest] == Min[allLengths]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-default-returns-shortest"
]

(* Properties -> {"Separating"} restricts to separating cycles. *)

(* Properties -> {"Separating"} cycles are inside the level-set range;
   admissibility (SeparatingSetQ) is enforced inside findCircleCore. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = #[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}]},
      Length[circles] >= 1 &&
      AllTrue[circles, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindInfraCircle-Separating-all-in-level-set"
]

(* Method -> "Peel" produces the same set as "Exhaustive" on a small graph. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{exh  = Sort[Sort /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}])],
          peel = Sort[Sort /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All, Properties -> {"Separating"}, Method -> "Peel"])]},
      Length[exh] >= 1 && exh === peel
    ]
  ],
  True,
  TestID -> "FindInfraCircle-Peel-and-Exhaustive-agree"
]

(* Method -> "Greedy" returns one admissible cycle; count > 1 fails. *)

VerificationTest[
  Length @ FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 1,
    Properties -> {"Separating"}, Method -> "Greedy"],
  1,
  TestID -> "FindInfraCircle-Greedy-single-realisation"
]

VerificationTest[
  FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 2,
    Properties -> {"Separating"}, Method -> "Greedy"],
  $Failed,
  TestID -> "FindInfraCircle-Greedy-count-gt-1-fails"
]

(* Pruning caps the number of returned cycles. *)

VerificationTest[
  Length @ FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, All,
    Method -> {"Exhaustive", "Pruning" -> 2}] <= 2,
  True,
  TestID -> "FindInfraCircle-Pruning-caps-results"
]

(* Property "Connected" is not meaningful for cycles -> ::badproperty. *)

VerificationTest[
  FindInfraCircle[GridGraph[{4, 4}], 6, {1, 2}, 1, Properties -> {"Connected"}],
  $Failed,
  {FindInfraCircle::badproperty},
  TestID -> "FindInfraCircle-badproperty-Connected"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraCycle[g, (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All]), All, "From" -> "LongestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraCycle-LongestCircumference-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectInfraCycle[g, (#[[ 1, 1 ]] & /@ FindInfraCircle[g, 6, {1, 2}, All]), All, "From" -> "ShortestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindInfraCircle-SelectInfraCycle-ShortestCircumference"
]

(* ===== FindInfraParallel ===== *)

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-GridGraph-row-from-row"
]

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  InfraLine @ FindInfraParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraParallel-self-on-line"
]

VerificationTest[
  InfraLine @ FindInfraParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  InfraLine[{}],
  TestID -> "FindInfraParallel-disconnected-empty"
]

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-strict-1"
]

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindInfraParallel-strict-fails-when-too-few"
]

VerificationTest[
  InfraLine @ FindInfraParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  InfraLine[{}],
  TestID -> "FindInfraParallel-CycleGraph-no-parallel"
]

VerificationTest[
  InfraParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    First @ First @ First @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]],
  True,
  TestID -> "FindInfraParallel-output-passes-InfraParallelQ"
]


(* ===== FindInfraSegment Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[ g, 1, 16, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraSegment[ g, 1, 16, All, Method -> "ShortestPath" ])
  ],
  True,
  TestID -> "FindInfraSegment-Embedding-Geodesic-All-equals-Shortest-set"
]

VerificationTest[
  With[ { paths = (#[[ 1, 1 ]] & /@ FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, All, Method -> "Embedding" ]) },
    Length[ paths ] >= 1 && AllTrue[ paths, First[ # ] === 1 && Last[ # ] === 16 & ]
  ],
  True,
  TestID -> "FindInfraSegment-Embedding-paths-have-correct-endpoints"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], coords = GraphEmbedding[ GridGraph[ { 4, 4 } ] ] },
    FindInfraSegment[ g, 1, 16, 1, Method -> { "Embedding", "Coordinates" -> coords } ] ===
      FindInfraSegment[ g, 1, 16, 1, Method -> "Embedding" ]
  ],
  True,
  TestID -> "FindInfraSegment-Embedding-explicit-coords-matches-Automatic"
]

VerificationTest[
  Length @ FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16, 1, Method -> { "Embedding", "Pruning" -> 1 } ],
  1,
  TestID -> "FindInfraSegment-Embedding-Pruning-beam-one"
]

VerificationTest[
  InfraSegment @ FindInfraSegment[ PathGraph[ Range[ 5 ] ], 1, 5, 1, Method -> { "Embedding", "Pool" -> "AllPaths" } ],
  InfraSegment[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraSegment-Embedding-AllPaths-PathGraph-unique-path"
]


(* ===== FindInfraLine Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All ])
  ],
  True,
  TestID -> "FindInfraLine-Embedding-set-equals-default"
]


(* ===== EmbeddingClosest dispatch for InfraShell ===== *)

(* Sets ranked by directed Hausdorff distance to the Euclidean sphere of
   radius r centered at c. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ EmbeddingClosest[ g,
      InfraShell[ List /@ Select[ VertexList[ g ], GraphDistance[ g, 6, # ] == 1 & ] ],
      { 6, 1 } ]
  ],
  InfraShell,
  TestID -> "EmbeddingClosest-InfraShell-preserves-wrapper"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ First @ EmbeddingClosest[ g,
      InfraShell[ List /@ VertexList[ g ] ], { 6, 1 } ]
  ],
  16,
  TestID -> "EmbeddingClosest-InfraShell-pool-all-vertices"
]


(* ===== EmbeddingClosest dispatch for InfraCircle (pre-existing) ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Length @ First @ EmbeddingClosest[ g,
      InfraCircle[ #[[ 1, 1 ]] & /@ FindInfraCircle[ g, 6, { 1, 2 }, All, Properties -> { "Separating" } ] ],
      { 6, 1.5 } ] >= 1
  ],
  True,
  TestID -> "EmbeddingClosest-InfraCircle-on-Separating-set"
]


(* ===== FindInfraParallel Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraParallel[ g, { 1, 2, 3, 4 }, 5, All, Method -> "Embedding" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraParallel[ g, { 1, 2, 3, 4 }, 5, All ])
  ],
  True,
  TestID -> "FindInfraParallel-Embedding-set-equals-default"
]


(* ===== FindInfraPoint All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraPoint[ g, All ]) === VertexList[ g ]
  ],
  True,
  TestID -> "FindInfraPoint-All-returns-every-vertex"
]

VerificationTest[
  With[ { g = PetersenGraph[ ] },
    Length @ FindInfraPoint[ g, All ] == VertexCount[ g ]
  ],
  True,
  TestID -> "FindInfraPoint-All-length-equals-vertex-count"
]


(* ===== FindInfraLine unified Method axis ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> "ShortestPath" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 16, All, Method -> Automatic ])
  ],
  True,
  TestID -> "FindInfraLine-Shortest-equals-Automatic"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { lines = FindInfraLine[ g, 1, 16, All, Method -> "ShortestPathExtension" ] },
      MatchQ[ lines, { InfraLine[ { _ } ] .. } ] &&
        Length @ lines >= Length @ FindInfraLine[ g, 1, 16, All, Method -> "ShortestPath" ]
    ]
  ],
  True,
  TestID -> "FindInfraLine-ShortestPathExtension-superset-of-Shortest"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { lines = FindInfraLine[ g, 1, 16, All, Method -> "CurvatureMinimizing" ] },
      MatchQ[ lines, { InfraLine[ { _ } ] .. } ] && Length @ lines >= 1
    ]
  ],
  True,
  TestID -> "FindInfraLine-CurvatureMinimizing-non-empty"
]


(* ===== ExtendInfraSegment ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      With[ { lines = (#[[ 1, 1 ]] & /@ ExtendInfraSegment[ g, seg, All ]) },
        ListQ[ lines ] && AllTrue[ lines,
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
      ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-Shortest-contains-segment"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      Sort @ (#[[ 1, 1 ]] & /@ ExtendInfraSegment[ g, seg, All, Method -> "ShortestPath" ]) ===
        Sort @ Select[ (#[[ 1, 1 ]] & /@ FindInfraLine[ g, 1, 6, All ]),
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-Shortest-matches-FindInfraLine-filtered"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      MatchQ[ ExtendInfraSegment[ g, seg, All, Method -> "ShortestPathExtension" ], { __InfraLine } ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-ShortestPathExtension-returns-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      MatchQ[ ExtendInfraSegment[ g, seg, All, Method -> "CurvatureMinimizing" ], { __InfraLine } ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-CurvatureMinimizing-returns-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = First @ First @ First @ FindInfraSegment[ g, 1, 6, All ] },
      With[ { lines = ExtendInfraSegment[ g, seg, 1, Method -> "Embedding" ] },
        Length @ lines == 1 &&
          MemberQ[ Partition[ First @ First @ First @ lines, Length @ seg, 1 ], seg ]
      ]
    ]
  ],
  True,
  TestID -> "ExtendInfraSegment-Embedding-contains-segment"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    InfraLine @ ExtendInfraSegment[ g, { 2, 3 }, 1, Method -> "ShortestPath" ] === InfraLine[ { { 1, 2, 3, 4, 5 } } ]
  ],
  True,
  TestID -> "ExtendInfraSegment-PathGraph-recovers-full-path"
]

VerificationTest[
  ExtendInfraSegment[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 99, Method -> "ShortestPath" ],
  $Failed,
  TestID -> "ExtendInfraSegment-strict-undersupply-Failed"
]


(* FindInfraShell / FindInfraCircle: a bounded radius makes the answer depend only on
   the ball B(p, r + 1) / B(p, r + 2) around the centre.  The "Metric" and
   "Separating" recipes are graph-intrinsic; the "Embedding" recipe still
   uses the full graph for its spectral coordinates, so the local-vs-global
   cross-check is for "Metric" / "Separating". *)

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p = 45 },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraShell[ g, p, 2, All ]) ===
      Sort @ (#[[ 1, 1 ]] & /@ FindInfraShell[ NeighborhoodGraph[ g, p, 3 ], p, 2, All ])
  ],
  True,
  TestID -> "FindInfraShell-locality-Metric"
]

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p = 45 },
    Sort[ Sort /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[ g, p, { 1, 2 }, All ]) ] ===
      Sort[ Sort /@ (#[[ 1, 1 ]] & /@ FindInfraCircle[ NeighborhoodGraph[ g, p, 4 ], p, { 1, 2 }, All ]) ]
  ],
  True,
  TestID -> "FindInfraCircle-locality-Metric"
]

EndTestSection[]
