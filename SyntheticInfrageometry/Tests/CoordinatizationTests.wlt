BeginTestSection["Coordinatization"]

(* ===== Metric Basis ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    MetricBasisQ[g, First @ FindMetricBasis[g]]
  ],
  True,
  TestID -> "FindMetricBasis-path-returns-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length[First @ FindMetricBasis[g]] == 1
  ],
  True,
  TestID -> "MetricBasis-path-size-one"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    MetricBasisQ[g, First @ FindMetricBasis[g]]
  ],
  True,
  TestID -> "FindMetricBasis-cycle-returns-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1}},
    MetricCoordinates[g, 3, b] == {2}
  ],
  True,
  TestID -> "MetricCoordinates-path-distance-vector"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    Table[MetricCoordinates[g, v, b], {v, 1, 5}]
  ],
  {{0, 4}, {1, 3}, {2, 2}, {3, 1}, {4, 0}},
  TestID -> "MetricCoordinates-path-endpoints-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1}},
    DuplicateFreeQ[Table[MetricCoordinates[g, v, b], {v, VertexList[g]}]]
  ],
  True,
  TestID -> "MetricBasis-roundtrip-distinguishes"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    MetricBisector[g, 1, 5]
  ],
  {3},
  TestID -> "MetricBisector-path-midpoint"
]

VerificationTest[
  With[{g = PathGraph[Range[4]]},
    MetricBisector[g, 1, 4]
  ],
  {},
  TestID -> "MetricBisector-path-even-empty"
]

(* ===== LaminarLayers ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]], line = {1, 2, 3, 4, 5}},
    LaminarLayers[g, line]
  ],
  {{1}, {2}, {3}, {4}, {5}},
  TestID -> "LaminarLayers-line-singletons"
]

VerificationTest[
  With[{g = PathGraph[Range[5]],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]},
    LaminarLayers[g, dag]
  ],
  {{1}, {2}, {3}},
  TestID -> "LaminarLayers-simple-dag"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[1, 4],
                     DirectedEdge[2, 5], DirectedEdge[4, 5]}]},
    Sort /@ LaminarLayers[g, dag]
  ],
  {{1}, {2, 4}, {5}},
  TestID -> "LaminarLayers-dag-multiple-per-layer"
]

(* ===== Projection ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]], line = {1, 2, 3, 4, 5}},
    FindLineProjection[g, line, 3]
  ],
  {3},
  TestID -> "FindLineProjection-on-line"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    FindLineProjection[g, line, 4]
  ],
  {1},
  TestID -> "FindLineProjection-grid-below-first"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    FindLineProjection[g, line, 5]
  ],
  {2},
  TestID -> "FindLineProjection-grid-below-middle"
]

VerificationTest[
  With[{g = CycleGraph[6], line = {1, 2, 3}},
    Sort @ FindLineProjection[g, line, 5]
  ],
  {1, 3},
  TestID -> "FindLineProjection-cycle-ties-both-endpoints"
]

VerificationTest[
  With[{g = CycleGraph[6], line = {1, 2, 3}},
    FindLineProjection[g, line, 4]
  ],
  {3},
  TestID -> "FindLineProjection-cycle-adjacent-endpoint"
]

VerificationTest[
  With[{g = PathGraph[Range[5]],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]},
    FindDAGProjection[g, dag, 4]
  ],
  {3},
  TestID -> "FindDAGProjection-path-closest"
]

(* ===== LaminarCoordinates ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]], line = {1, 2, 3, 4, 5}},
    Table[LaminarCoordinates[g, line, v], {v, 1, 5}]
  ],
  {{0, 0}, {1, 0}, {2, 0}, {3, 0}, {4, 0}},
  TestID -> "LaminarCoordinates-on-line-zero-orthogonal"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    LaminarCoordinates[g, line, 4]
  ],
  {0, 1},
  TestID -> "LaminarCoordinates-grid-one-below-first"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    LaminarCoordinates[g, line, 5]
  ],
  {1, 1},
  TestID -> "LaminarCoordinates-grid-one-below-middle"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    LaminarCoordinates[g, line, 9]
  ],
  {2, 2},
  TestID -> "LaminarCoordinates-grid-far-corner"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    Length @ LaminarCoordinates[g, line]
  ],
  9,
  TestID -> "LaminarCoordinates-bulk-returns-association"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], line = {1, 2, 3}},
    AssociationQ @ LaminarCoordinates[g, line]
  ],
  True,
  TestID -> "LaminarCoordinates-bulk-is-association"
]

VerificationTest[
  With[{g = PathGraph[Range[5]],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]},
    LaminarCoordinates[g, dag, 4]
  ],
  {2, 1},
  TestID -> "LaminarCoordinates-dag-outside-reach"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[1, 4]}]},
    LaminarCoordinates[g, dag, 5]
  ],
  {1, 1},
  TestID -> "LaminarCoordinates-dag-ties-min-layer"
]

(* ===== Segment to Line extension (existing API) ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    FindLine[g, {2, 3, 4}] == {1, 2, 3, 4, 5}
  ],
  True,
  TestID -> "FindLine-extends-segment-to-full-path"
]

EndTestSection[]
