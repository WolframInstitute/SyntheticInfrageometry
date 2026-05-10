BeginTestSection["EuclideanPostulates"]

(* ===== FindPoint ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindPoint[g]},
      pt["Length"] == 1 && SubsetQ[VertexList[g], pt["Realizations"]]
    ]
  ],
  True,
  TestID -> "FindPoint-single-vertex"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pts = FindPoint[g, 3]},
      pts["Length"] == 3 && DuplicateFreeQ[pts["Realizations"]] && SubsetQ[VertexList[g], pts["Realizations"]]
    ]
  ],
  True,
  TestID -> "FindPoint-multiple-vertices"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphCenter[g], FindPoint[g, 1, "From" -> "Center"]["Realizations"]]
  ],
  True,
  TestID -> "FindPoint-from-center"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    SubsetQ[GraphPeriphery[g], FindPoint[g, 1, "From" -> "Periphery"]["Realizations"]]
  ],
  True,
  TestID -> "FindPoint-from-periphery"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = FindPoint[g, 2, "Distance" -> 4]["Realizations"]},
      Length[pts] == 2 && GraphDistance[g, pts[[1]], pts[[2]]] >= 4
    ]
  ],
  True,
  TestID -> "FindPoint-with-distance-constraint"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{pt = FindPoint[g, 1, "From" -> {2, 3, 4}]},
      pt["Length"] == 1 && SubsetQ[{2, 3, 4}, pt["Realizations"]]
    ]
  ],
  True,
  TestID -> "FindPoint-from-vertex-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{pts = FindPoint[g, 2, "From" -> {1, 2, 3, 4, 5}, "Distance" -> 4]["Realizations"]},
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
    pts["Length"] == 3 && SubsetQ[VertexList[PathGraph[Range[3]]], pts["Realizations"]]
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
      pt["Length"] == 1 && GraphDistance[g, 3, pt["First"]] == 2
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-exact-distance"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = FindPoint[g, UpTo[20], "From" -> 1 -> {2, 3}]["Realizations"]},
      AllTrue[pts, 2 <= GraphDistance[g, 1, #] <= 3 &]
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-distance-range"
]

VerificationTest[
  With[{g = CycleGraph[8]},
    With[{ecc = Max[GraphDistance[g, 1, #] & /@ VertexList[g]]},
      With[{pts = FindPoint[g, UpTo[VertexCount[g]], "From" -> 1 -> "Max"]["Realizations"]},
        AllTrue[pts, GraphDistance[g, 1, #] == ecc &]
      ]
    ]
  ],
  True,
  TestID -> "FindPoint-from-origin-max-distance"
]

VerificationTest[
  Head[FindPoint[PetersenGraph[]]] === InfraPoint,
  True,
  TestID -> "FindPoint-returns-InfraPoint-wrapper"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{pts = FindPoint[g, UpTo[VertexCount[g]],
      "From" -> InfraPoint[{1, 16}] -> 3]["Realizations"]},
      AllTrue[pts, GraphDistance[g, 1, #] == 3 && GraphDistance[g, 16, #] == 3 &]
    ]
  ],
  True,
  TestID -> "FindPoint-multi-anchor-intersection"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ FindPoint[g, UpTo[VertexCount[g]], "From" -> InfraPoint[{2, 5, 7}]]["Realizations"]
  ],
  {2, 5, 7},
  TestID -> "FindPoint-multi-anchor-pool-no-distance"
]

(* ===== FindSegment ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-unique-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{segs = FindSegment[g, 1, 3]["Realizations"]},
      Length[segs] == 1 && Length[First[segs]] == 3
    ]
  ],
  True,
  TestID -> "FindSegment-correct-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, All]["Realizations"]},
      AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-GridGraph-all-geodesics-same-length"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = SelectPaths[g, FindSegment[g, 1, 9, All], "Central"]["Realizations"]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-SelectPaths-Central-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = EmbeddingClosestPaths[g, FindSegment[g, 1, 9, All], {1, 9}]["Realizations"]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-EmbeddingClosestPaths"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1]],
  InfraSegment[{}],
  TestID -> "FindSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = SelectPaths[g, FindSegment[g, 1, 9, All], "Central", Method -> "Hausdorff"]["Realizations"]},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-SelectPaths-Central-Hausdorff"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = SelectPaths[g, FindSegment[g, 1, 9, All], "Peripheral"]["Realizations"]},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-SelectPaths-Peripheral-Frechet"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = (EmbeddingClosestPaths[g, {1, 9}] @ SelectPaths[g, "Central"] @ FindSegment[g, 1, 9, All])["Realizations"]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-chained-operator-form"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, UpTo[2]]["Realizations"]},
      Length[segs] <= 2 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-upto-soft-cap"
]

(* ===== FindSegment Method -> "ShortestPathExtension" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "ShortestPathExtension"]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-ShortestPathExtension-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> "ShortestPathExtension"]["Realizations"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-ShortestPathExtension-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]["Realizations"] ===
      Sort @ FindPath[g, 1, 9, Infinity, All]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-K1-equals-FindPath"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4}]},
    With[{
      k1   = Sort @ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]["Realizations"],
      k2   = Sort @ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}]["Realizations"],
      kAll = Sort @ FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> All}]["Realizations"]
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
      k1   = FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1}]["Length"],
      k2   = FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}]["Length"],
      kAll = FindSegment[g, 1, 3, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> All}]["Length"]
    },
      k1 >= k2 >= kAll
    ]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-K-monotone"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "ShortestPathExtension"]["Realizations"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 2}]["Realizations"]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-ShortestPathWindow-default-is-2"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      FindSegment[g, 1, 16, All,
        Method -> {"ShortestPathExtension", "Pruning" -> 1}]["Length"] == 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-ShortestPathExtension-pruning-beam-1"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> "Bogus"],
    FindSegment::badmethod],
  $Failed,
  TestID -> "FindSegment-bad-method"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"ShortestPathExtension", "Pruning" -> -1}],
    FindSegment::badpruning],
  $Failed,
  TestID -> "FindSegment-bad-pruning"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 0}],
    FindSegment::badwindow],
  $Failed,
  TestID -> "FindSegment-bad-lookback-zero"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> -1}],
    FindSegment::badwindow],
  $Failed,
  TestID -> "FindSegment-bad-lookback-negative"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"ShortestPathExtension", "ShortestPathWindow" -> 1.5}],
    FindSegment::badwindow],
  $Failed,
  TestID -> "FindSegment-bad-lookback-fractional"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "ShortestPathExtension"],
  InfraSegment[{}],
  TestID -> "FindSegment-ShortestPathExtension-same-point-empty"
]


(* ===== FindSegment Method -> "CurvatureMinimizing" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> "CurvatureMinimizing"]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-CurvatureMinimizing-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> "CurvatureMinimizing"]["Realizations"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-CurvatureMinimizing-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]["Realizations"],
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
    FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Pool" -> "AllPaths"}]["Length"]
  ],
  6,
  TestID -> "FindSegment-CurvatureMinimizing-AllPaths-grid-bundle-size"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]["Realizations"],
      walk |-> Length[walk] - 1 >= GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-grid-walks-no-shorter-than-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]["Realizations"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Simple"}}]["Realizations"]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Forman-Method-default-is-Simple"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    Sort @ FindSegment[g, 1, 3, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Simple"}, "Pool" -> "AllPaths"}]["Realizations"] =!=
      Sort @ FindSegment[g, 1, 3, All, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Triangles"}, "Pool" -> "AllPaths"}]["Realizations"]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-AllPaths-Forman-Method-Triangles-differs-on-triangulated"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    FindSegment[g, 1, 9, UpTo[2], Method -> "CurvatureMinimizing"]["Length"]
  ],
  2,
  TestID -> "FindSegment-CurvatureMinimizing-UpTo-truncates"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    FindSegment[g, 1, 9, 3, Method -> "CurvatureMinimizing"]["Length"]
  ],
  3,
  TestID -> "FindSegment-CurvatureMinimizing-Count-exact"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SelectPaths[g, "Central"] @ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]
  ],
  _InfraSegment,
  SameTest -> MatchQ,
  TestID -> "FindSegment-CurvatureMinimizing-chains-with-SelectPaths"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"CurvatureMinimizing", "Curvature" -> {"Forman", Method -> "Quadrilaterals"}}],
    FindSegment::badcurvature],
  $Failed,
  TestID -> "FindSegment-CurvatureMinimizing-bad-Forman-Method"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    BlockRandom[
      FindSegment[g, 1, 16, All, Method -> {"CurvatureMinimizing", "Pruning" -> 1}]["Length"] <= 1,
      RandomSeeding -> 42
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-pruning-beam-1"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"CurvatureMinimizing", "Pruning" -> -1}],
    FindSegment::badpruning],
  $Failed,
  TestID -> "FindSegment-CurvatureMinimizing-bad-pruning"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    FindSegment[g, 1, 36, 1, Method -> "CurvatureMinimizing"]["Length"]
  ],
  1,
  TestID -> "FindSegment-CurvatureMinimizing-count-1-terminates-early"
]

VerificationTest[
  With[{g = GridGraph[{6, 6}]},
    FindSegment[g, 1, 36, 1, Method -> "ShortestPathExtension"]["Length"]
  ],
  1,
  TestID -> "FindSegment-ShortestPathExtension-count-1-terminates-early"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1, UpTo[1], Method -> "CurvatureMinimizing"],
  InfraSegment[{}],
  TestID -> "FindSegment-CurvatureMinimizing-same-point-empty"
]


(* ===== FindSegment Method -> "CurvatureMinimizing": Pool -> "ShortestPaths" (default) ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]["Realizations"],
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    SubsetQ[
      Sort @ FindSegment[g, 1, 9, All, Method -> "Shortest"]["Realizations"],
      Sort @ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]["Realizations"]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Constraint-default-subset-of-geodesics"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4, 2 <-> 3, 2 <-> 4}]},
    FindSegment[g, 1, 3, All, Method -> "CurvatureMinimizing"]
  ],
  InfraSegment[{{1, 3}}],
  TestID -> "FindSegment-CurvatureMinimizing-Constraint-default-target-adjacent"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"CurvatureMinimizing", "Pool" -> "Bogus"}],
    FindSegment::badpool],
  $Failed,
  TestID -> "FindSegment-CurvatureMinimizing-bad-Constraint"
]


(* ===== FindSegment Method -> "CurvatureMinimizing": CurvatureMethod -> "Wolfram" ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-tree-unique-path"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    Sort @ FindSegment[g, 1, 4, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]["Realizations"]
  ],
  Sort[{{1, 2, 3, 4}, {1, 6, 5, 4}}],
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    AllTrue[
      FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]["Realizations"],
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
      FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram"}]["Realizations"],
      walk |-> Length[walk] - 1 === GraphDistance[g, 1, 9]
    ]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-Constraint-default-walks-are-geodesic"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    FindSegment[g, 1, 9, All,
      Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram", "Dimension" -> 2}]["Length"]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-Dimension-fixed-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    FindSegment[g, 1, 9, All,
      Method -> {"CurvatureMinimizing", "Curvature" -> "Wolfram",
                 "Dimension" -> 2, "Radii" -> {1, 2}}]["Length"]
  ],
  _Integer?Positive,
  SameTest -> MatchQ,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-Radii-explicit-runs"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    Sort @ FindSegment[g, 1, 9, All, Method -> "CurvatureMinimizing"]["Realizations"] ===
      Sort @ FindSegment[g, 1, 9, All, Method -> {"CurvatureMinimizing", "Curvature" -> "Forman"}]["Realizations"]
  ],
  True,
  TestID -> "FindSegment-CurvatureMinimizing-CurvatureMethod-default-is-Forman"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9, Method -> {"CurvatureMinimizing", "Curvature" -> "Bogus"}],
    FindSegment::badcurvature],
  $Failed,
  TestID -> "FindSegment-CurvatureMinimizing-bad-CurvatureMethod"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9,
      Method -> {"CurvatureMinimizing", "Curvature" -> {"Wolfram", "Dimension" -> "two"}}],
    FindSegment::badcurvature],
  $Failed,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-bad-dimension"
]

VerificationTest[
  Quiet[FindSegment[GridGraph[{3, 3}], 1, 9,
      Method -> {"CurvatureMinimizing", "Curvature" -> {"Wolfram", "Radii" -> {3, 1}}}],
    FindSegment::badcurvature],
  $Failed,
  TestID -> "FindSegment-CurvatureMinimizing-Wolfram-bad-radii"
]


(* ===== FindLine ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, 2, 4]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindLine-extends-from-points"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindLine[g, 2, 4]["First"]
  ],
  5,
  TestID -> "FindLine-extends-to-full-path"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, 1, 5]
  ],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindLine-already-maximal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = Take[SelectPaths[g, FindLine[g, 5, 6, All], "Central"]["Realizations"], UpTo[3]]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindLine-with-SelectPaths-Central"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, 2, 4, UpTo[5]]["Length"] >= 1
  ],
  True,
  TestID -> "FindLine-upto-soft"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 3, All, "Maximality" -> "Extension"]
  ],
  InfraSegment[{{1, 2, 3}}],
  TestID -> "FindLine-Maximality-Extension-keeps-short-line"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 3, All, "Maximality" -> "Diameter"]
  ],
  InfraSegment[{}],
  TestID -> "FindLine-Maximality-Diameter-drops-short-line"
]

VerificationTest[
  With[{g = Graph[{1 <-> 2, 2 <-> 3, 2 <-> 4, 4 <-> 5}]},
    FindLine[g, 1, 5, All, "Maximality" -> "Diameter"]
  ],
  InfraSegment[{{1, 2, 4, 5}}],
  TestID -> "FindLine-Maximality-Diameter-keeps-diameter-line"
]

(* ===== FindShell ===== *)

(* Method -> "Metric" (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ FindShell[g, 3, 2]["First"]
  ],
  {1, 5},
  TestID -> "FindShell-Metric-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindShell[g, 6, {1, 2}, All]},
      result["Length"] == 1 &&
      AllTrue[result["First"], v |-> 1 <= GraphDistance[g, 6, v] <= 2]
    ]
  ],
  True,
  TestID -> "FindShell-Metric-range-radius"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    FindShell[g, 1, 2, All]["Length"]
  ],
  1,
  TestID -> "FindShell-Metric-single-result"
]

(* Method -> "Separating": minimal connected separators within the level surface. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{shells = FindShell[g, 6, {1, 2}, All, Method -> "Separating"]["Realizations"]},
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
    With[{shells = FindShell[g, 6, {1, 2}, All, Method -> "Separating"]["Realizations"]},
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
        SelectCycles[g, FindCircle[g, 6, {1, 2}, All]["Realizations"], "LongestCircumference"],
        UpTo[1]]},
      Length[circles] >= 1 && AllTrue[circles, ListQ]
    ]
  ],
  True,
  TestID -> "FindCircle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    FindCircle[g, 1, {1, 2}, All]["Length"] >= 1
  ],
  True,
  TestID -> "FindCircle-all-cycles"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectCycles[g, FindCircle[g, 6, {1, 2}, All]["Realizations"], "LongestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-SelectCycles-LongestCircumference-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = SelectCycles[g, FindCircle[g, 6, {1, 2}, All]["Realizations"], "ShortestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-SelectCycles-ShortestCircumference"
]

(* ===== FindParallel ===== *)

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-GridGraph-row-from-row"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 6, All],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-GridGraph-row-interior-vertex"
]

VerificationTest[
  FindParallel[PathGraph[Range[5]], {1, 2, 3, 4, 5}, 3, All],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindParallel-self-on-line"
]

VerificationTest[
  FindParallel[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], {1, 2}, 3, All],
  InfraSegment[{}],
  TestID -> "FindParallel-disconnected-empty"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 1],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-strict-1"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, 2],
  $Failed,
  TestID -> "FindParallel-strict-fails-when-too-few"
]

VerificationTest[
  FindParallel[CycleGraph[8], {1, 2, 3}, 6, All],
  InfraSegment[{}],
  TestID -> "FindParallel-CycleGraph-no-parallel"
]

VerificationTest[
  ParallelQ[GridGraph[{4, 4}], {1, 2, 3, 4},
    FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5]["First"]],
  True,
  TestID -> "FindParallel-output-passes-ParallelQ"
]


(* ===== FindSegment Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindSegment[ g, 1, 16, All, Method -> "Embedding" ][ "Realizations" ] ===
      Sort @ FindSegment[ g, 1, 16, All, Method -> "Shortest" ][ "Realizations" ]
  ],
  True,
  TestID -> "FindSegment-Embedding-Geodesic-All-equals-Shortest-set"
]

VerificationTest[
  With[ { paths = FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, All, Method -> "Embedding" ][ "Realizations" ] },
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
  FindSegment[ GridGraph[ { 4, 4 } ], 1, 16, 1, Method -> { "Embedding", "Pruning" -> 1 } ][ "Length" ],
  1,
  TestID -> "FindSegment-Embedding-Pruning-beam-one"
]

VerificationTest[
  FindSegment[ PathGraph[ Range[ 5 ] ], 1, 5, 1, Method -> { "Embedding", "Pool" -> "AllPaths" } ],
  InfraSegment[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindSegment-Embedding-AllPaths-PathGraph-unique-path"
]


(* ===== FindLine Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindLine[ g, 1, 16, All, Method -> "Embedding" ][ "Realizations" ] ===
      Sort @ FindLine[ g, 1, 16, All ][ "Realizations" ]
  ],
  True,
  TestID -> "FindLine-Embedding-set-equals-default"
]


(* ===== FindShell Method -> "Embedding" ===== *)

VerificationTest[
  Length @ Flatten @ FindShell[ GridGraph[ { 4, 4 } ], 6, 1, All, Method -> "Embedding" ][ "Realizations" ],
  Length @ Select[ VertexList[ GridGraph[ { 4, 4 } ] ],
    GraphDistance[ GridGraph[ { 4, 4 } ], 6, # ] == 1 & ],
  TestID -> "FindShell-Embedding-Geodesic-pool-equals-level-surface"
]

VerificationTest[
  FindShell[ GridGraph[ { 4, 4 } ], 6, 1, All, Method -> { "Embedding", "Pool" -> "AllVertices" } ][ "Length" ],
  16,
  TestID -> "FindShell-Embedding-AllPaths-pool-equals-all-vertices"
]


(* ===== FindCircle Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindCircle[ g, 6, { 1, 2 }, All, Method -> "Embedding" ][ "Realizations" ] ===
      Sort @ FindCircle[ g, 6, { 1, 2 }, All ][ "Realizations" ]
  ],
  True,
  TestID -> "FindCircle-Embedding-set-equals-default"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    FindCircle[ g, 6, { 1, 2 }, All, Method -> "Embedding" ][ "Length" ] >= 1
  ],
  True,
  TestID -> "FindCircle-Embedding-non-empty-on-grid-with-cycles"
]


(* ===== FindParallel Method -> "Embedding" ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindParallel[ g, { 1, 2, 3, 4 }, 5, All, Method -> "Embedding" ][ "Realizations" ] ===
      Sort @ FindParallel[ g, { 1, 2, 3, 4 }, 5, All ][ "Realizations" ]
  ],
  True,
  TestID -> "FindParallel-Embedding-set-equals-default"
]


(* ===== FindPoint All ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ FindPoint[ g, All ][ "Realizations" ] === VertexList[ g ]
  ],
  True,
  TestID -> "FindPoint-All-returns-every-vertex"
]

VerificationTest[
  With[ { g = PetersenGraph[ ] },
    FindPoint[ g, All ][ "Length" ] == VertexCount[ g ]
  ],
  True,
  TestID -> "FindPoint-All-length-equals-vertex-count"
]


(* ===== FindLine unified Method axis ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Sort @ FindLine[ g, 1, 16, All, Method -> "Shortest" ][ "Realizations" ] ===
      Sort @ FindLine[ g, 1, 16, All, Method -> Automatic ][ "Realizations" ]
  ],
  True,
  TestID -> "FindLine-Shortest-equals-Automatic"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { lines = FindLine[ g, 1, 16, All, Method -> "ShortestPathExtension" ] },
      MatchQ[ lines, _InfraSegment ] &&
        lines[ "Length" ] >= FindLine[ g, 1, 16, All, Method -> "Shortest" ][ "Length" ]
    ]
  ],
  True,
  TestID -> "FindLine-ShortestPathExtension-superset-of-Shortest"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { lines = FindLine[ g, 1, 16, All, Method -> "CurvatureMinimizing" ] },
      MatchQ[ lines, _InfraSegment ] && lines[ "Length" ] >= 1
    ]
  ],
  True,
  TestID -> "FindLine-CurvatureMinimizing-non-empty"
]


(* ===== ExtendSegment ===== *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindSegment[ g, 1, 6, All ][ "First" ] },
      With[ { lines = ExtendSegment[ g, seg, All ][ "Realizations" ] },
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
    With[ { seg = FindSegment[ g, 1, 6, All ][ "First" ] },
      Sort @ ExtendSegment[ g, seg, All, Method -> "Shortest" ][ "Realizations" ] ===
        Sort @ Select[ FindLine[ g, 1, 6, All ][ "Realizations" ],
          lst |-> Length[ lst ] >= Length[ seg ] && MemberQ[ Partition[ lst, Length @ seg, 1 ], seg ] ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-Shortest-matches-FindLine-filtered"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindSegment[ g, 1, 6, All ][ "First" ] },
      MatchQ[ ExtendSegment[ g, seg, All, Method -> "ShortestPathExtension" ], _InfraSegment ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-ShortestPathExtension-returns-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindSegment[ g, 1, 6, All ][ "First" ] },
      MatchQ[ ExtendSegment[ g, seg, All, Method -> "CurvatureMinimizing" ], _InfraSegment ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-CurvatureMinimizing-returns-list"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { seg = FindSegment[ g, 1, 6, All ][ "First" ] },
      With[ { lines = ExtendSegment[ g, seg, 1, Method -> "Embedding" ] },
        lines[ "Length" ] == 1 &&
          MemberQ[ Partition[ lines[ "First" ], Length @ seg, 1 ], seg ]
      ]
    ]
  ],
  True,
  TestID -> "ExtendSegment-Embedding-contains-segment"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    ExtendSegment[ g, { 2, 3 }, 1, Method -> "Shortest" ] === InfraSegment[ { { 1, 2, 3, 4, 5 } } ]
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
