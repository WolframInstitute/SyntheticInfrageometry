BeginTestSection["Postulates"]

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

(* ===== FindSegment ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindSegment[g, 1, 5, 1]
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
    With[{segs = FindSegment[g, 1, 9, All, "Select" -> "FrechetCentral"]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-FrechetCentral"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, All, "Select" -> "EmbeddingClosest"]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-EmbeddingClosest"
]

VerificationTest[
  FindSegment[PathGraph[Range[5]], 1, 1],
  {},
  TestID -> "FindSegment-same-point-empty"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, All, "Select" -> "HausdorffCentral"]},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-HausdorffCentral"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, All, "Select" -> "FrechetPeripheral"]},
      Length[segs] >= 1
    ]
  ],
  True,
  TestID -> "FindSegment-FrechetPeripheral"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{segs = FindSegment[g, 1, 9, All,
        "Select" -> {"FrechetCentral", "EmbeddingClosest"}]},
      Length[segs] >= 1 && AllTrue[segs, Length[#] == 5 &]
    ]
  ],
  True,
  TestID -> "FindSegment-chained-select"
]

(* ===== FindLine ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{ext = FindLine[g, {2, 3, 4}]},
      Length[ext] >= 3
    ]
  ],
  True,
  TestID -> "FindLine-at-least-as-long"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, {1, 2, 3, 4, 5}]
  ],
  {1, 2, 3, 4, 5},
  TestID -> "FindLine-already-maximal"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length[FindLine[g, {2, 3}]]
  ],
  5,
  TestID -> "FindLine-extends-to-full-path"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{exts = FindLine[g, {5, 6}, 3, "Select" -> "FrechetCentral"]},
      Length[exts] >= 1 && AllTrue[exts, Length[#] > 2 &]
    ]
  ],
  True,
  TestID -> "FindLine-with-FrechetCentral"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, 2, 4]
  ],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindLine-from-points"
]

(* ===== FindCircle ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = FindCircle[g, 6, {1, 2}, 1, "Select" -> "LongestCircumference"]},
      Length[circles] >= 1 && AllTrue[circles, ListQ]
    ]
  ],
  True,
  TestID -> "FindCircle-returns-cycles"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = FindCircle[g, 6, {1, 2}, 1]},
      AllTrue[circles, c |-> AllTrue[c, v |-> GraphDistance[g, 6, v] <= 2]]
    ]
  ],
  True,
  TestID -> "FindCircle-within-radius"
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
    With[{circles = FindCircle[g, 6, {1, 2}, All, "Select" -> "LongestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-LongestCircumference-uniform"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{circles = FindCircle[g, 1, {1, 2}, 2, "Select" -> {"LongestCircumference", "FrechetCentral"}]},
      Length[circles] <= 2
    ]
  ],
  True,
  TestID -> "FindCircle-chained-select"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{circles = FindCircle[g, 6, {1, 2}, All, "Select" -> "ShortestCircumference"]},
      Length[circles] >= 1 && Length[Union[Length /@ circles]] == 1
    ]
  ],
  True,
  TestID -> "FindCircle-ShortestCircumference"
]

EndTestSection[]
