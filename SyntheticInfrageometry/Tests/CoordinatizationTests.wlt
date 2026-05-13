BeginTestSection["Coordinatization"]

(* Ensure the sister paclet is on $ContextPath for symbols (EffectiveResistance,
   etc.) referenced in tests below; the parent paclet imports it transitively
   but TestReport's parse may otherwise create them in Global` first. *)
Needs["WolframInstitute`Infrageometry`"]

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
    RadarCoordinates[g, b, 3] == {2}
  ],
  True,
  TestID -> "RadarCoordinates-path-distance-vector"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    Table[RadarCoordinates[g, b, v], {v, 1, 5}]
  ],
  {{0, 4}, {1, 3}, {2, 2}, {3, 1}, {4, 0}},
  TestID -> "RadarCoordinates-path-endpoints-basis"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1}},
    DuplicateFreeQ[Table[RadarCoordinates[g, b, v], {v, VertexList[g]}]]
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

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    Table[RadarCoordinates[g, b][v] === RadarCoordinates[g, b, v], {v, VertexList[g]}]
  ],
  {True, True, True, True, True},
  TestID -> "RadarCoordinates-operator-form-matches-direct"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    RadarCoordinates[g, b, InfraPoint[{3}]] === RadarCoordinates[g, b, 3]
  ],
  True,
  TestID -> "RadarCoordinates-InfraPoint-singleton-degenerates"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {1, 5}},
    RadarCoordinates[g, b, InfraPoint[{2, 4}]]
  ],
  {{1, 3}, {3, 1}},
  TestID -> "RadarCoordinates-InfraPoint-multi-returns-list"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], b = {InfraPoint[{1, 5}]}},
    RadarCoordinates[g, b, 3]
  ],
  {2},
  TestID -> "RadarCoordinates-InfraPoint-anchor-aggregation-Min"
]

(* ===== OrthogonalCoordinates ===== *)

(* Each test below picks the centre c so it sits at position 0 on every
   axis, making the signed coordinate coincide with the underlying layer
   index -- the simplest setup for exercising the projection / option
   semantics. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, 1, {xAxis, yAxis}, 6]
  ],
  {1, 1},
  TestID -> "OrthogonalCoordinates-grid-2axes-vertex6"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, 1, {xAxis, yAxis}, 11]
  ],
  {2, 2},
  TestID -> "OrthogonalCoordinates-grid-2axes-vertex11"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    Length @ OrthogonalCoordinates[g, 1, {xAxis, yAxis}]
  ],
  16,
  TestID -> "OrthogonalCoordinates-grid-bulk-association"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    AssociationQ @ OrthogonalCoordinates[g, 1, {xAxis, yAxis}]
  ],
  True,
  TestID -> "OrthogonalCoordinates-bulk-is-association"
]

VerificationTest[
  With[{g = PathGraph[Range[5]], axes = {{1, 2, 3, 4, 5}}},
    Table[OrthogonalCoordinates[g, 1, axes, v], {v, 1, 5}]
  ],
  {{0}, {1}, {2}, {3}, {4}},
  TestID -> "OrthogonalCoordinates-single-axis-recovers-position"
]

VerificationTest[
  With[{g = PathGraph[Range[5]],
        dag = Graph[{DirectedEdge[1, 2], DirectedEdge[2, 3]}]},
    OrthogonalCoordinates[g, 1, {dag}, 4]
  ],
  {2},
  TestID -> "OrthogonalCoordinates-dag-axis-outside-reach"
]

(* InfraSegment wrappers: first realisation drives projection. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = InfraSegment[{{1, 2, 3, 4}}],
        yAxis = InfraSegment[{{1, 5, 9, 13}}]},
    OrthogonalCoordinates[g, 1, {xAxis, yAxis}, 11]
  ],
  {2, 2},
  TestID -> "OrthogonalCoordinates-InfraSegment-axes"
]

(* ===== SelectCoordinate option (ties on a 4-cycle) ===== *)

(* On CycleGraph[4] with axis {1, 2, 3} and centre 1 (at position 0):
   vertex 4 has d(4,1)=1, d(4,2)=2, d(4,3)=1 -- a tie at indices 0 and 2.
   Different "SelectCoordinate" choices pick / aggregate / preserve the
   tied list. *)

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> First]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-First"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> Last]
  ],
  {2},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Last"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> Min]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Min"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> Max]
  ],
  {2},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Max"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> Mean]
  ],
  {1},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Mean"
]

VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> Median]
  ],
  {1},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Median"
]

(* All preserves the tied list as the per-axis coordinate. *)
VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> All]
  ],
  {{0, 2}},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-All"
]

(* User-supplied function works without an allow-list. *)
VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> (Quantile[#, 0.25] &)]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-userFunction"
]

(* "Centered" rule: shifted ix_v contains 0 (vertex 4 ties at positions 0
   and 2 on axis {1, 2, 3} anchored at 1, so shifted = {0, 2}) -> coord 0. *)
VerificationTest[
  With[{g = CycleGraph[4], axes = {{1, 2, 3}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> "Centered"]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Centered-contains-0"
]

(* "Centered" rule: shifted ix_v doesn't contain 0 -> Median fallback.
   On PathGraph[5] anchored at vertex 2 (position 1 of axis {1, 2, 3, 4, 5}),
   vertex 4 has unique closest position 3, so ix_v = {3} and shifted = {2}.
   Doesn't contain 0; Median[{2}] = 2. *)
VerificationTest[
  With[{g = PathGraph @ Range[5], axes = {{1, 2, 3, 4, 5}}},
    OrthogonalCoordinates[g, 2, axes, 4, "SelectCoordinate" -> "Centered"]
  ],
  {2},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Centered-fallback-Median"
]

(* "Centered" with anchor not at position 0: axis {3, 1, 2} on CycleGraph[4]
   anchored at vertex 1 (position 1).  For v = 4: distances are
   d(4, 3) = 1, d(4, 1) = 1, d(4, 2) = 2 -> ix_v = {0, 1}, shifted = {-1, 0}.
   Contains 0 -> coord 0. *)
VerificationTest[
  With[{g = CycleGraph[4], axes = {{3, 1, 2}}},
    OrthogonalCoordinates[g, 1, axes, 4, "SelectCoordinate" -> "Centered"]
  ],
  {0},
  TestID -> "OrthogonalCoordinates-SelectCoordinate-Centered-anchor-interior"
]

(* "Centered" coords are integers (Round[Median[...]] fallback).  Run on a
   reasonably diverse mesh and assert every per-vertex per-axis coord is an
   integer. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{axes = FindOrthogonalFrame[g, 6, All]},
      AllTrue[
        Catenate[OrthogonalCoordinates[g, 6, axes, #] & /@ VertexList[g]],
        IntegerQ
      ]
    ]
  ],
  True,
  TestID -> "OrthogonalCoordinates-Centered-integer-coords"
]

(* ===== FindOrthogonalFrame ===== *)

(* On PathGraph[5] at interior vertex 3 the line {1, 2, 3, 4, 5} is the
   unique frame: every axis is a full metric line through 3. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    Length @ FindOrthogonalFrame[g, 3, All]
  ],
  1,
  TestID -> "FindOrthogonalFrame-path-single-line"
]

(* Endpoint of a path: no antipodal pair exists, so there is no full
   metric line through vertex 1.  FindOrthogonalFrame returns $Failed. *)

VerificationTest[
  FindOrthogonalFrame[PathGraph[Range[5]], 1, All],
  $Failed,
  TestID -> "FindOrthogonalFrame-path-endpoint-no-line"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalFrame[g, 3, All]},
      AllTrue[axes, MemberQ[#[[ 1, 1 ]], 3] &]
    ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-axes-pass-through-center"
]

VerificationTest[
  FindOrthogonalFrame[PathGraph[Range[5]], 3, All, 100],
  $Failed,
  TestID -> "FindOrthogonalFrame-strict-fail-too-many"
]

(* "AxisCount" -> k prescribes the per-frame axis count.  On the 3x3 grid
   centred at 5 there is at least one 2-axis frame (the row + column). *)

VerificationTest[
  With[{frame = FindOrthogonalFrame[GridGraph[{3, 3}], 5, All, "AxisCount" -> 2]},
    Length[frame] === 2
  ],
  True,
  TestID -> "FindOrthogonalFrame-AxisCount-exact-2"
]

(* "AxisCount" -> k impossible: no 5-axis frame exists on the 3x3 grid. *)

VerificationTest[
  FindOrthogonalFrame[GridGraph[{3, 3}], 5, All, "AxisCount" -> 5],
  $Failed,
  TestID -> "FindOrthogonalFrame-AxisCount-exact-impossible"
]

(* "AxisCount" -> UpTo[k] is a soft cap. *)

VerificationTest[
  With[{frame = FindOrthogonalFrame[GridGraph[{3, 3}], 5, All, "AxisCount" -> UpTo[2]]},
    Length[frame] <= 2 && Length[frame] >= 1
  ],
  True,
  TestID -> "FindOrthogonalFrame-AxisCount-UpTo"
]

(* GeodesicGraph primitive *)

VerificationTest[
  Sort @ VertexList @ GeodesicGraph[GridGraph[{3, 3}], 5],
  Range[9],
  TestID -> "GeodesicGraph-3x3-vertices"
]

VerificationTest[
  Length @ EdgeList @ GeodesicGraph[PathGraph[Range[5]], 3],
  4,
  TestID -> "GeodesicGraph-path-edges-symmetric"
]

VerificationTest[
  Sort @ Select[VertexList[GeodesicGraph[GridGraph[{3, 3}], 5]],
    VertexOutDegree[GeodesicGraph[GridGraph[{3, 3}], 5], #] == 0 &],
  {1, 3, 7, 9},
  TestID -> "GeodesicGraph-3x3-sinks-are-corners"
]

VerificationTest[
  Length @ VertexList @ GeodesicGraph[GridGraph[{3, 3}], 5, "AxisLength" -> 1],
  5,
  TestID -> "GeodesicGraph-AxisLength-truncation"
]

(* ===== FindSpanningAxes (no-center form) ===== *)

VerificationTest[
  Length @ FindSpanningAxes[PathGraph[Range[5]], All] >= 1,
  True,
  TestID -> "FindSpanningAxes-path-some-axes"
]

VerificationTest[
  Length @ FindSpanningAxes[GridGraph[{4, 4}], UpTo[5]] <= 5,
  True,
  TestID -> "FindSpanningAxes-grid-UpTo-bound"
]

VerificationTest[
  FindSpanningAxes[PathGraph[Range[5]], 99],
  $Failed,
  TestID -> "FindSpanningAxes-strict-fail-too-many"
]

(* ===== Z-valued OrthogonalCoordinates from a center ===== *)

(* PathGraph[5] at 3 with the default frame from FindOrthogonalFrame:
   coords are signed displacements, the centre maps to {0, ..., 0}. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalFrame[g, 3, All]},
      OrthogonalCoordinates[g, 3, axes, 3]
    ]
  ],
  ConstantArray[0, Length @ FindOrthogonalFrame[PathGraph[Range[5]], 3, All]],
  TestID -> "OrthogonalCoordinates-center-maps-to-origin"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalFrame[g, 3, All]},
      AssociationQ @ OrthogonalCoordinates[g, 3, axes]
        && Length[OrthogonalCoordinates[g, 3, axes]] === 5
    ]
  ],
  True,
  TestID -> "OrthogonalCoordinates-path-bulk-association"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}], c = 6},
    With[{axes = FindOrthogonalFrame[g, c, All]},
      OrthogonalCoordinates[g, c, axes, c]
    ]
  ],
  ConstantArray[0, Length @ FindOrthogonalFrame[GridGraph[{4, 4}], 6, All]],
  TestID -> "OrthogonalCoordinates-grid-center-self-zero"
]

(* Centre is now positional: replaces the old "Origin" -> ... option. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, 6, {xAxis, yAxis}, 11]
  ],
  {1, 1},
  TestID -> "OrthogonalCoordinates-positional-center-signed"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}],
        xAxis = {1, 2, 3, 4}, yAxis = {1, 5, 9, 13}},
    OrthogonalCoordinates[g, 6, {xAxis, yAxis}, 1]
  ],
  {-1, -1},
  TestID -> "OrthogonalCoordinates-positional-center-negative"
]

(* ===== InfraPoint center ===== *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalFrame[g, 3, All]},
      OrthogonalCoordinates[g, InfraPoint[{3}], axes, 3] ==
        OrthogonalCoordinates[g, 3, axes, 3]
    ]
  ],
  True,
  TestID -> "OrthogonalCoordinates-InfraPoint-singleton-equals-vertex"
]

VerificationTest[
  Module[{g = PathGraph[Range[5]],
          canonicalize = Sort[First @ Sort[{#, Reverse @ #}] & /@ (#[[ 1, 1 ]] & /@ #)] &},
    canonicalize @ FindOrthogonalFrame[g, InfraPoint[{3}], All] ===
      canonicalize @ FindOrthogonalFrame[g, 3, All]
  ],
  True,
  TestID -> "FindOrthogonalFrame-InfraPoint-singleton-equals-vertex"
]

(* On PathGraph[5] with InfraPoint[{2, 4}]: any frame's axes pass through
   one of the listed vertices. *)

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    With[{axes = FindOrthogonalFrame[g, InfraPoint[{2, 4}], All]},
      AllTrue[axes, MemberQ[#[[ 1, 1 ]], 2] || MemberQ[#[[ 1, 1 ]], 4] &]
    ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-InfraPoint-axes-contain-some-anchor"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{axes = FindOrthogonalFrame[g, InfraPoint[{6, 11}], All]},
      AllTrue[axes, MemberQ[#[[ 1, 1 ]], 6] || MemberQ[#[[ 1, 1 ]], 11] &]
    ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-InfraPoint-grid-anchor-on-each-axis"
]

(* ===== Frame search semantics ===== *)

(* On a 3x3 grid centred at 5 the default frame admits at least 2 mutually
   perpendicular axes (with Method -> Automatic = "Exhaustive"). *)

VerificationTest[
  Length @ FindOrthogonalFrame[GridGraph[{3, 3}], 5, All] >= 2,
  True,
  TestID -> "FindOrthogonalFrame-3x3grid-centre-multi-axis"
]

(* Well-conditioned coords: every vertex on axis i has coordinate j == 0
   for every j != i (mutual perpendicularity at c). *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], c = 5},
    With[{axes = FindOrthogonalFrame[g, c, All]},
      AllTrue[Range @ Length @ axes,
        i |-> AllTrue[First @ First @ axes[[i]],
          v |-> With[{coords = OrthogonalCoordinates[g, c, axes, v]},
            AllTrue[Range @ Length @ coords, j |-> j == i || coords[[j]] == 0]
          ]
        ]
      ]
    ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-well-conditioned-coords"
]

(* On PathGraph[7] at interior 4: every axis is a metric line with 4
   interior; the longest is the whole path 1..7. *)

VerificationTest[
  With[{frame = FindOrthogonalFrame[PathGraph[Range[7]], 4, All]},
    Length[frame] === 1 && Length[First @ First @ frame[[1]]] === 7
  ],
  True,
  TestID -> "FindOrthogonalFrame-path-line-longest"
]

(* On a 3x3 grid at corner 1 the L-bent line through 1 ({3, 2, 1, 4, 7})
   is the unique metric line with 1 interior: corners 3 and 7 are
   strictly antipodal at 1 (d_g(3, 7) = 4 = depth(3) + depth(7)). *)

VerificationTest[
  Sort[First @ Sort[{#, Reverse @ #}] & /@ (#[[ 1, 1 ]] & /@ FindOrthogonalFrame[GridGraph[{3, 3}], 1, All])],
  Sort[First @ Sort[{#, Reverse @ #}] & /@ {{3, 2, 1, 4, 7}}],
  TestID -> "FindOrthogonalFrame-3x3grid-corner-L-bent-line"
]

(* BranchSampleSize is Exhaustive-only; under Greedy it is forced to All
   so the result is fully deterministic. *)

VerificationTest[
  FindOrthogonalFrame[GridGraph[{4, 4}], 6, All, Method -> "Greedy"] ===
    FindOrthogonalFrame[GridGraph[{4, 4}], 6, All, Method -> "Greedy", "BranchSampleSize" -> 1],
  True,
  TestID -> "FindOrthogonalFrame-Greedy-ignores-BranchSampleSize"
]

(* Determinism: same inputs always produce same output (no RandomPick). *)

VerificationTest[
  Module[{frame1 = FindOrthogonalFrame[GridGraph[{4, 4}], 6, All],
          frame2 = FindOrthogonalFrame[GridGraph[{4, 4}], 6, All]},
    frame1 === frame2
  ],
  True,
  TestID -> "FindOrthogonalFrame-deterministic"
]

(* Centre maps to the origin under any FindOrthogonalFrame frame. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{axes = FindOrthogonalFrame[g, 5, All]},
      OrthogonalCoordinates[g, 5, axes, 5] === ConstantArray[0, Length @ axes]
    ]
  ],
  True,
  TestID -> "OrthogonalCoordinates-3x3grid-centre-self-zero"
]


(* "SelectCoordinate" default is "Centered": omitting the option matches
   passing "Centered" explicitly. *)

VerificationTest[
  FindOrthogonalFrame[GridGraph[{4, 4}], 6, All, All] ===
    FindOrthogonalFrame[GridGraph[{4, 4}], 6, All, All, "SelectCoordinate" -> "Centered"],
  True,
  TestID -> "FindOrthogonalFrame-SelectCoordinate-default-is-Centered"
]


(* Unification: under "Centered", every vertex on axis i has coord 0 on
   every axis j != i.  This is by construction (perpendicularity test ==
   coord-is-0 test), but worth pinning. *)

VerificationTest[
  With[{g = GridGraph[{3, 3}], c = 5},
    With[{axes = FindOrthogonalFrame[g, c, All]},
      AllTrue[Range @ Length @ axes,
        i |-> AllTrue[First @ First @ axes[[i]],
          v |-> With[{coords = OrthogonalCoordinates[g, c, axes, v,
              "SelectCoordinate" -> "Centered"]},
            AllTrue[Range @ Length @ coords, j |-> j == i || coords[[j]] == 0]
          ]
        ]
      ]
    ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-Centered-unifies-perpendicularity-and-coords"
]


(* "SelectCoordinate" -> All is the strict-list-equality interpretation:
   c's and w's full tied projection lists must coincide.  Since the test is
   symmetric in c and w, the perpendicularity decision must be symmetric
   when restricted to a swap-invariant question. *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    FindOrthogonalFrame[g, 6, All, "SelectCoordinate" -> All] ===
      FindOrthogonalFrame[g, 6, All, "SelectCoordinate" -> All]
  ],
  True,
  TestID -> "FindOrthogonalFrame-SelectCoordinate-All-deterministic"
]


(* Different aggregation choices produce well-formed frames (no $Failed). *)

VerificationTest[
  AllTrue[
    {Min, Max, Median, Mean, All},
    sel |-> MatchQ[
      FindOrthogonalFrame[GridGraph[{3, 3}], 5, All, "SelectCoordinate" -> sel],
      {__WolframInstitute`SyntheticInfrageometry`InfraSegment}
    ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-SelectCoordinate-accepts-Min-Max-Median-Mean-All"
]


(* ===== ResistanceCoordinates =====
   Central claim: ||Phi(u) - Phi(v)||^2 == R(u, v) (Klein-Randic isometry). *)

isometryError[g_] :=
  With[{phi = Values @ ResistanceCoordinates[g]},
    Max[Abs[Outer[SquaredEuclideanDistance, phi, phi, 1] - EffectiveResistance[g]]]
  ]

VerificationTest[
  isometryError[PetersenGraph[]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-isometry-Petersen"
]

VerificationTest[
  isometryError[GridGraph[{3, 3}]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-isometry-Grid3x3"
]

VerificationTest[
  isometryError[CycleGraph[8]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-isometry-Cycle8"
]

VerificationTest[
  isometryError[CompleteGraph[5]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-isometry-K5"
]

VerificationTest[
  isometryError[Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 2 <-> 5}]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-isometry-tree"
]


(* Dimension = n - c for connected graphs. *)

VerificationTest[
  Length @ First @ Values @ ResistanceCoordinates[PetersenGraph[]],
  9,
  TestID -> "ResistanceCoordinates-dimension-Petersen"
]

VerificationTest[
  Length @ First @ Values @ ResistanceCoordinates[GridGraph[{4, 4}]],
  15,
  TestID -> "ResistanceCoordinates-dimension-Grid4x4"
]


(* Centeredness: rows sum to zero. *)

VerificationTest[
  With[{phi = Values @ ResistanceCoordinates[GridGraph[{3, 3}]]},
    Max[Abs[Total[phi]]] < 10^-9
  ],
  True,
  TestID -> "ResistanceCoordinates-centered"
]


(* "Origin" -> v sets v's coordinate to 0. *)

VerificationTest[
  Max[Abs @ ResistanceCoordinates[PetersenGraph[], "Origin" -> 1][1]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-Origin-zeroes-anchor"
]

VerificationTest[
  Max[Abs @ ResistanceCoordinates[CycleGraph[6], 3, "Origin" -> 3]] < 10^-10,
  True,
  TestID -> "ResistanceCoordinates-Origin-single-call"
]


(* "Dimension" cap. *)

VerificationTest[
  Length @ First @ Values @ ResistanceCoordinates[PetersenGraph[], "Dimension" -> 3],
  3,
  TestID -> "ResistanceCoordinates-Dimension-Integer"
]

VerificationTest[
  Length @ First @ Values @ ResistanceCoordinates[PetersenGraph[], "Dimension" -> UpTo[100]],
  9,
  TestID -> "ResistanceCoordinates-Dimension-UpTo-clipped"
]


(* InfraPoint query. *)

VerificationTest[
  Dimensions @ ResistanceCoordinates[PetersenGraph[], InfraPoint[{1, 2, 3}]],
  {3, 9},
  TestID -> "ResistanceCoordinates-InfraPoint-shape"
]

VerificationTest[
  ResistanceCoordinates[PetersenGraph[], InfraPoint[{1, 2, 3}]][[1]] ==
    ResistanceCoordinates[PetersenGraph[], 1],
  True,
  TestID -> "ResistanceCoordinates-InfraPoint-rows-match-singletons"
]

VerificationTest[
  ResistanceCoordinates[PetersenGraph[], InfraPoint[{1}]] ==
    ResistanceCoordinates[PetersenGraph[], 1],
  True,
  TestID -> "ResistanceCoordinates-InfraPoint-singleton-degenerates"
]


(* "Rescaling" -> "None" gives the smallest nonzero Laplacian eigenvectors. *)

VerificationTest[
  With[{
    phi = Values @ ResistanceCoordinates[CycleGraph[6], "Rescaling" -> "None"],
    es  = Eigensystem[N @ Normal @ KirchhoffMatrix[CycleGraph[6]]]
  },
    With[{vecs = Drop[es[[2, Ordering[es[[1]]]]], 1]},
      Max[Abs[phi - Transpose[vecs]]] < 10^-10
    ]
  ],
  True,
  TestID -> "ResistanceCoordinates-Rescaling-None"
]


(* Bounded axisLength makes the answer depend only on B(c, 2 axisLength). *)

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], c = 45 },
    FindOrthogonalFrame[ g, c, 2, All ] ===
      FindOrthogonalFrame[ NeighborhoodGraph[ g, c, 4 ], c, 2, All ]
  ],
  True,
  TestID -> "FindOrthogonalFrame-locality"
]


EndTestSection[]
