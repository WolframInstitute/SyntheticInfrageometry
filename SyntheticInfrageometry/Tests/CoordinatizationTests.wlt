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

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    AssociationQ @ RadarCoordinates[g, b]
  ],
  True,
  TestID -> "RadarCoordinates-bulk-is-association"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    RadarCoordinates[g, b]
  ],
  Association[1 -> {0, 4}, 2 -> {1, 3}, 3 -> {2, 2}, 4 -> {3, 1}, 5 -> {4, 0}],
  TestID -> "RadarCoordinates-bulk-matches-vertex-form"
]

(* ===== OrthogonalCoordinates ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, {xAxis, yAxis}, 6]
  ],
  {1, 1},
  TestID -> "OrthogonalCoordinates-grid-2axes-vertex6"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, {xAxis, yAxis}, 11]
  ],
  {2, 2},
  TestID -> "OrthogonalCoordinates-grid-2axes-vertex11"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    Length @ OrthogonalCoordinates[g, {xAxis, yAxis}]
  ],
  16,
  TestID -> "OrthogonalCoordinates-grid-bulk-association"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AssociationQ @ OrthogonalCoordinates[g, {xAxis, yAxis}]
  ],
  True,
  TestID -> "OrthogonalCoordinates-bulk-is-association"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], axes = {{1, 2, 3, 4, 5}}},
    Table[OrthogonalCoordinates[g, axes, v], {v, 1, 5}]
  ],
  {{0}, {1}, {2}, {3}, {4}},
  TestID -> "OrthogonalCoordinates-single-axis-recovers-position"
]

VerificationTest[
  With[{g = PathGraph[Range[5]],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]},
    OrthogonalCoordinates[g, {dag}, 4]
  ],
  {2},
  TestID -> "OrthogonalCoordinates-dag-axis-outside-reach"
]

(* ===== Aggregation option (ties on a 4-cycle) ===== *)

(* On CycleGraph[4] with axis {1, 2, 3}: vertex 4 has d(4,1)=1, d(4,2)=2,
   d(4,3)=1 -- a tie at indices 0 and 2.  Aggregations should differ. *)

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, axes, 4, "Aggregation" -> "First"]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-aggregation-First"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, axes, 4, "Aggregation" -> "Last"]
  ],
  {2},
  TestID -> "OrthogonalCoordinates-aggregation-Last"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, axes, 4, "Aggregation" -> "Mean"]
  ],
  {1},
  TestID -> "OrthogonalCoordinates-aggregation-Mean"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, axes, 4, "Aggregation" -> "Median"]
  ],
  {1},
  TestID -> "OrthogonalCoordinates-aggregation-Median"
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

(* ===== Z-valued OrthogonalCoordinates from a center ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    OrthogonalCoordinates[g, 3, 3]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-center-maps-to-origin"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Sort @ Values @ OrthogonalCoordinates[g, 3]
  ],
  Sort[{{-2}, {-1}, {0}, {1}, {2}}],
  TestID -> "OrthogonalCoordinates-path-Z-coords"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    AssociationQ @ OrthogonalCoordinates[g, 3]
  ],
  True,
  TestID -> "OrthogonalCoordinates-center-bulk-association"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    OrthogonalCoordinates[g, 1, 1]
  ],
  ConstantArray[0, Length @ FindOrthogonalAxes[GridGraph[{4, 4}], 1, All]],
  TestID -> "OrthogonalCoordinates-grid-center-self-zero"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, {xAxis, yAxis}, 11, "Origin" -> 6]
  ],
  {1, 1},
  TestID -> "OrthogonalCoordinates-explicit-Origin-signed"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, {xAxis, yAxis}, 1, "Origin" -> 6]
  ],
  {-1, -1},
  TestID -> "OrthogonalCoordinates-explicit-Origin-negative"
]

(* ===== InfraPoint center ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    OrthogonalCoordinates[g, InfraPoint[{3}], 3] == OrthogonalCoordinates[g, 3, 3]
  ],
  True,
  TestID -> "OrthogonalCoordinates-InfraPoint-singleton-equals-vertex"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindOrthogonalAxes[g, InfraPoint[{3}], All] === FindOrthogonalAxes[g, 3, All]
  ],
  True,
  TestID -> "FindOrthogonalAxes-InfraPoint-singleton-equals-vertex"
]

(* On PathGraph[5] with InfraPoint[{2, 4}]: any longest geodesic passing
   through 2 OR 4 as interior point qualifies.  The whole path 1..5 is
   the only longest geodesic and it contains both. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalAxes[g, InfraPoint[{2, 4}], All]},
      AllTrue[axes, MemberQ[#, 2] || MemberQ[#, 4] &]
    ]
  ],
  True,
  TestID -> "FindOrthogonalAxes-InfraPoint-axes-contain-some-anchor"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{axes = FindOrthogonalAxes[g, InfraPoint[{6, 11}], All]},
      AllTrue[axes, MemberQ[#, 6] || MemberQ[#, 11] &]
    ]
  ],
  True,
  TestID -> "FindOrthogonalAxes-InfraPoint-grid-anchor-on-each-axis"
]

EndTestSection[]
