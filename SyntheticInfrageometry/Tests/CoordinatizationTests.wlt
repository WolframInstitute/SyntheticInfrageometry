BeginTestSection["Coordinatization"]

(* ===== Radar Basis ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    RadarBasisQ[g, First @ FindRadarBasis[g]]
  ],
  True,
  TestID -> "FindRadarBasis-path-returns-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length[First @ FindRadarBasis[g]] == 1
  ],
  True,
  TestID -> "RadarBasis-path-size-one"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    RadarBasisQ[g, First @ FindRadarBasis[g]]
  ],
  True,
  TestID -> "FindRadarBasis-cycle-returns-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1}},
    RadarCoordinates[g, 3, b] == {2}
  ],
  True,
  TestID -> "RadarCoordinates-path-distance-vector"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    Table[RadarCoordinates[g, v, b], {v, 1, 5}]
  ],
  {{0, 4}, {1, 3}, {2, 2}, {3, 1}, {4, 0}},
  TestID -> "RadarCoordinates-path-endpoints-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1}},
    DuplicateFreeQ[Table[RadarCoordinates[g, v, b], {v, VertexList[g]}]]
  ],
  True,
  TestID -> "RadarBasis-roundtrip-distinguishes"
]

(* ===== AxesCoordinates ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AxesCoordinates[g, {xAxis, yAxis}, 6]
  ],
  {1, 1},
  TestID -> "AxesCoordinates-grid-2axes-vertex6"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AxesCoordinates[g, {xAxis, yAxis}, 11]
  ],
  {2, 2},
  TestID -> "AxesCoordinates-grid-2axes-vertex11"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    Length @ AxesCoordinates[g, {xAxis, yAxis}]
  ],
  16,
  TestID -> "AxesCoordinates-grid-bulk-association"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AssociationQ @ AxesCoordinates[g, {xAxis, yAxis}]
  ],
  True,
  TestID -> "AxesCoordinates-bulk-is-association"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], axes = {{1, 2, 3, 4, 5}}},
    Table[AxesCoordinates[g, axes, v], {v, 1, 5}]
  ],
  {{0}, {1}, {2}, {3}, {4}},
  TestID -> "AxesCoordinates-single-axis-recovers-position"
]

VerificationTest[
  With[{g = PathGraph[Range[5]],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]},
    AxesCoordinates[g, {dag}, 4]
  ],
  {2},
  TestID -> "AxesCoordinates-dag-axis-outside-reach"
]

(* ===== FindOrthogonalAxes ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindOrthogonalAxes[g, 3, All]
  ],
  1,
  TestID -> "FindOrthogonalAxes-path-through-center-one-axis"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Length @ FindOrthogonalAxes[g, 1, All] >= 1
  ],
  True,
  TestID -> "FindOrthogonalAxes-grid-through-corner-some-axes"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalAxes[g, 3, All]},
      AllTrue[axes, MemberQ[#, 3] &]
    ]
  ],
  True,
  TestID -> "FindOrthogonalAxes-axes-pass-through-center"
]

VerificationTest[
  FindOrthogonalAxes[PathGraph[Range[5]], 3, 5],
  $Failed,
  TestID -> "FindOrthogonalAxes-strict-fail-too-many"
]

(* ===== Z-valued AxesCoordinates from a center ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    AxesCoordinates[g, 3, 3]
  ],
  {0},
  TestID -> "AxesCoordinates-center-maps-to-origin"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ Values @ AxesCoordinates[g, 3]
  ],
  Sort[{{-2}, {-1}, {0}, {1}, {2}}],
  TestID -> "AxesCoordinates-path-Z-coords"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    AssociationQ @ AxesCoordinates[g, 3]
  ],
  True,
  TestID -> "AxesCoordinates-center-bulk-association"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    AxesCoordinates[g, 1, 1]
  ],
  ConstantArray[0, Length @ FindOrthogonalAxes[GridGraph[{4, 4}], 1, All]],
  TestID -> "AxesCoordinates-grid-center-self-zero"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AxesCoordinates[g, {xAxis, yAxis}, 11, "Origin" -> 6]
  ],
  {1, 1},
  TestID -> "AxesCoordinates-explicit-Origin-signed"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AxesCoordinates[g, {xAxis, yAxis}, 1, "Origin" -> 6]
  ],
  {-1, -1},
  TestID -> "AxesCoordinates-explicit-Origin-negative"
]

EndTestSection[]
