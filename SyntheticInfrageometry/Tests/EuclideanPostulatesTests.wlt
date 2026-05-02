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
  TestID -> "FindSegment-Stretched-unique-geodesic"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    With[{segs = FindSegment[g, 1, 4, All, Method -> "Stretched"]},
      Length[segs] == 2 && AllTrue[segs, Length[#] == 4 &]
    ]
  ],
  True,
  TestID -> "FindSegment-Stretched-cycle-symmetric"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{
      all = FindSegment[g, 1, 9, All],
      stretched = FindSegment[g, 1, 9, All, Method -> "Stretched"]
    },
      SubsetQ[all, stretched] &&
      AllTrue[stretched,
        path |-> AllTrue[
          Range[2, Length[path] - 1],
          i |-> With[{
            v = path[[i]],
            actual = path[[i + 1]],
            history = Reverse @ path[[1 ;; i - 1]]
          },
            With[{
              progress = Select[AdjacencyList[g, v],
                GraphDistance[g, #, 9] === Length[path] - i - 1 &]
            },
              MemberQ[
                MaximalBy[progress,
                  w |-> GraphDistance[g, #, w] & /@ history],
                actual
              ]
            ]
          ]
        ]
      ]
    ]
  ],
  True,
  TestID -> "FindSegment-Stretched-property-on-grid"
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

(* ===== FindSphere ===== *)

(* Method -> "Metric" (default): level surface { v : d(c, v) = r }. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ First @ FindSphere[g, 3, 2]
  ],
  {1, 5},
  TestID -> "FindSphere-Metric-default-equidistant"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{result = FindSphere[g, 6, {1, 2}, All]},
      Length[result] == 1 &&
      AllTrue[First[result], v |-> 1 <= GraphDistance[g, 6, v] <= 2]
    ]
  ],
  True,
  TestID -> "FindSphere-Metric-range-radius"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    Length @ FindSphere[g, 1, 2, All]
  ],
  1,
  TestID -> "FindSphere-Metric-single-result"
]

(* Method -> "SeparatingGraph": minimal connected separators within the level surface. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{spheres = FindSphere[g, 6, {1, 2}, All, Method -> "SeparatingGraph"]},
      Length[spheres] >= 1 &&
      AllTrue[spheres, vs |-> AllTrue[vs, v |-> 1 <= GraphDistance[g, 6, v] <= 2]] &&
      AllTrue[spheres, vs |-> ConnectedGraphQ[Subgraph[g, vs]]]
    ]
  ],
  True,
  TestID -> "FindSphere-SeparatingGraph-connected-within-range"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{spheres = FindSphere[g, 6, {1, 2}, All, Method -> "SeparatingGraph"]},
      AllTrue[spheres, vs |-> AllTrue[spheres,
        other |-> other === vs || ! (Length[other] < Length[vs] && SubsetQ[vs, other])
      ]]
    ]
  ],
  True,
  TestID -> "FindSphere-SeparatingGraph-minimal"
]

(* Method -> "SeparatingCycle": cycles in the level-surface subgraph that separate. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{spheres = Take[
        LongestCircumferenceCycles @ FindSphere[g, 6, {1, 2}, All, Method -> "SeparatingCycle"],
        UpTo[1]]},
      Length[spheres] >= 1 && AllTrue[spheres, ListQ]
    ]
  ],
  True,
  TestID -> "FindSphere-SeparatingCycle-returns-cycles"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{spheres = FindSphere[g, 1, {1, 2}, All, Method -> "SeparatingCycle"]},
      Length[spheres] >= 1
    ]
  ],
  True,
  TestID -> "FindSphere-SeparatingCycle-all-cycles"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{spheres = LongestCircumferenceCycles @
        FindSphere[g, 6, {1, 2}, All, Method -> "SeparatingCycle"]},
      Length[spheres] >= 1 && Length[Union[Length /@ spheres]] == 1
    ]
  ],
  True,
  TestID -> "FindSphere-SeparatingCycle-LongestCircumferenceCycles-uniform"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{spheres = ShortestCircumferenceCycles @
        FindSphere[g, 6, {1, 2}, All, Method -> "SeparatingCycle"]},
      Length[spheres] >= 1 && Length[Union[Length /@ spheres]] == 1
    ]
  ],
  True,
  TestID -> "FindSphere-SeparatingCycle-ShortestCircumferenceCycles"
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
