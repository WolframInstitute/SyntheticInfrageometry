BeginTestSection["EuclideanConstructions"]

(* ===== FindMidpoint ===== *)

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  InfraPoint[{3}],
  TestID -> "FindMidpoint-segment-odd-length"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}],
  InfraPoint[{2}],
  TestID -> "FindMidpoint-segment-even-length-lower-central"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], 1, 5],
  InfraPoint[{3}],
  TestID -> "FindMidpoint-endpoints-strict-1"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], d = GraphDistance[GridGraph[{3, 3}], 1, 9]},
    Sort @ FindMidpoint[g, 1, 9, All]["Realisations"] ===
      Sort @ DeleteDuplicates[
        #[[ Ceiling[ Length[#] / 2 ] ]] & /@ FindPath[g, 1, 9, {d}, All]
      ]
  ],
  True,
  TestID -> "FindMidpoint-all-matches-geodesic-midpoints"
]

VerificationTest[
  FindMidpoint[GridGraph[{3, 3}], 1, 9, UpTo[2]]["Length"] <= 2,
  True,
  TestID -> "FindMidpoint-upto-soft"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], 1, 5, 100],
  $Failed,
  TestID -> "FindMidpoint-strict-fails-when-too-few"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], 1, 5, Method -> "Spectral"],
  $Failed,
  {FindMidpoint::nyi},
  TestID -> "FindMidpoint-spectral-stub"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], 1, 5, Method -> "Walk"],
  $Failed,
  {FindMidpoint::badmethod},
  TestID -> "FindMidpoint-bad-method"
]

(* ===== FindPerpendicular ===== *)

VerificationTest[
  FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All],
  InfraPoint[{2}],
  TestID -> "FindPerpendicular-CycleGraph5"
]

VerificationTest[
  With[{feet = FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All]["Realisations"]},
    AllTrue[feet, MemberQ[{1, 2, 3, 4}, #] &]
  ],
  True,
  TestID -> "FindPerpendicular-feet-on-line"
]

VerificationTest[
  FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, 1],
  InfraPoint[{2}],
  TestID -> "FindPerpendicular-strict-1"
]

VerificationTest[
  FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, Method -> "Spectral"],
  $Failed,
  {FindPerpendicular::nyi},
  TestID -> "FindPerpendicular-spectral-stub"
]

(* ===== FindBisectingHyperplane ===== *)

(* The bisector of 1 and 5 in PathGraph[5] is just {3}; removing it
   disconnects 1 from 5 and is the only minimal hyperplane. *)
VerificationTest[
  FindBisectingHyperplane[PathGraph[Range[5]], 1, 5],
  InfraPlane[{{3}}],
  TestID -> "FindBisectingHyperplane-path-center"
]

VerificationTest[
  FindBisectingHyperplane[PathGraph[Range[5]], 1, 5, All],
  FindBisectingHyperplane[PathGraph[Range[5]], InfraPoint[{1}], InfraPoint[{5}], All],
  TestID -> "FindBisectingHyperplane-list-form-equiv"
]

(* On the 3x3 grid, the antidiagonal {3, 5, 7} is the bisector of 1 and 9;
   removing the entire antidiagonal is the only inclusion-minimal way to
   disconnect them. *)
VerificationTest[
  FindBisectingHyperplane[GridGraph[{3, 3}], 1, 9, All],
  InfraPlane[{{3, 5, 7}}],
  TestID -> "FindBisectingHyperplane-grid-antidiagonal"
]

(* PathGraph[6], 1 to 6 (odd distance): the strict bisector is empty,
   so no hyperplane exists in the {0, 0} window. *)
VerificationTest[
  FindBisectingHyperplane[PathGraph[Range[6]], 1, 6, All],
  InfraPlane[{}],
  TestID -> "FindBisectingHyperplane-odd-distance-empty"
]

(* Widening to {-1, 1} thickens the bisector to {3, 4}; on a path each of
   3 and 4 individually disconnects 1 from 6, giving two minimal
   hyperplanes within the thickened bisector. *)
VerificationTest[
  Sort @ FindBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, All]["Realisations"],
  {{3}, {4}},
  TestID -> "FindBisectingHyperplane-thickened-path"
]

(* CycleGraph[6], 1 to 4 (odd distance): the thickened {-1, 1} bisector
   is {2, 3, 5, 6}; cutting either arc requires one vertex from {2, 3}
   and one from {5, 6}, giving four minimal hyperplanes. *)
VerificationTest[
  Sort @ ( Sort /@ FindBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, All]["Realisations"] ),
  {{2, 5}, {2, 6}, {3, 5}, {3, 6}},
  TestID -> "FindBisectingHyperplane-cycle-thickened"
]

VerificationTest[
  FindBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, UpTo[2]]["Length"],
  2,
  TestID -> "FindBisectingHyperplane-upto-soft"
]

VerificationTest[
  FindBisectingHyperplane[PathGraph[Range[5]], 1, 5, 5],
  $Failed,
  TestID -> "FindBisectingHyperplane-strict-fails-when-too-few"
]

(* FindBisectingHyperplane returns InfraPlane (not InfraShell) so the two
   set-shaped wrappers stay distinct. *)
VerificationTest[
  Head @ FindBisectingHyperplane[PathGraph[Range[5]], 1, 5],
  InfraPlane,
  TestID -> "FindBisectingHyperplane-wraps-as-InfraPlane"
]

(* ===== CompleteEquilateralTriangle ===== *)

VerificationTest[
  Sort @ CompleteEquilateralTriangle[CycleGraph[6], 1, 3, All]["Realisations"],
  {5},
  TestID -> "CompleteEquilateralTriangle-cycle6"
]

VerificationTest[
  CompleteEquilateralTriangle[PathGraph[Range[5]], 1, 5, All],
  InfraPoint[{}],
  TestID -> "CompleteEquilateralTriangle-path-no-apex"
]

VerificationTest[
  CompleteEquilateralTriangle[CompleteGraph[4], 1, 2, 1],
  InfraPoint[{3}],
  TestID -> "CompleteEquilateralTriangle-K4-strict-1"
]

VerificationTest[
  CompleteEquilateralTriangle[CompleteGraph[4], 1, 2, Method -> "Spectral"],
  $Failed,
  {CompleteEquilateralTriangle::nyi},
  TestID -> "CompleteEquilateralTriangle-spectral-stub"
]

(* ===== InfraAngle ===== *)

VerificationTest[
  InfraAngle[CycleGraph[6], {1, 2, 3}],
  4,
  TestID -> "InfraAngle-cycle-local"
]

VerificationTest[
  InfraAngle[PathGraph[Range[5]], {1, 3, 5}],
  Infinity,
  TestID -> "InfraAngle-path-infinite"
]

VerificationTest[
  InfraAngle[CompleteGraph[4], {2, 1, 3}],
  1,
  TestID -> "InfraAngle-complete-graph"
]

VerificationTest[
  InfraAngle[CycleGraph[6], {1, 2, 3}] == InfraAngle[CycleGraph[6], {3, 2, 1}],
  True,
  TestID -> "InfraAngle-symmetric"
]

(* ===== SegmentLineAngle ===== *)

VerificationTest[
  SegmentLineAngle[PathGraph[Range[5]], 1, 3, {1, 2, 3, 4, 5}],
  0,
  TestID -> "SegmentLineAngle-segment-on-line"
]

VerificationTest[
  SegmentLineAngle[GridGraph[{3, 3}], 1, 9, {1, 2, 3}],
  2,
  TestID -> "SegmentLineAngle-grid-far-endpoint"
]

VerificationTest[
  SegmentLineAngle[GridGraph[{3, 3}], 5, 9, {1, 2, 3}],
  Infinity,
  TestID -> "SegmentLineAngle-near-endpoint-not-on-line"
]

VerificationTest[
  SegmentLineAngle[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 3, 4, 5}],
  0,
  TestID -> "SegmentLineAngle-segment-form"
]

VerificationTest[
  SegmentLineAngle[PathGraph[Range[5]], 1, 3, {1, 2, 3, 4, 5}, Method -> "Spectral"],
  $Failed,
  {SegmentLineAngle::nyi},
  TestID -> "SegmentLineAngle-spectral-stub"
]

VerificationTest[
  SegmentLineAngle[PathGraph[Range[5]], 1, 3, {1, 2, 3, 4, 5}, Method -> "Resistance"],
  $Failed,
  {SegmentLineAngle::badmethod},
  TestID -> "SegmentLineAngle-resistance-rejected"
]

(* ===== FindParallel: Method scaffolding ===== *)

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Metric"],
  InfraSegment[{{5, 6, 7, 8}}],
  TestID -> "FindParallel-explicit-metric"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Spectral"],
  $Failed,
  {FindParallel::badmethod},
  TestID -> "FindParallel-spectral-rejected"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Walk"],
  $Failed,
  {FindParallel::badmethod},
  TestID -> "FindParallel-bad-method"
]


(* ===== FindMidpoint Method -> "Embedding" ===== *)

VerificationTest[
  MemberQ[ FindMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, All, Method -> "Metric" ][ "Realisations" ],
           FindMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, 1, Method -> "Embedding" ][ "First" ] ],
  True,
  TestID -> "FindMidpoint-Embedding-Geodesic-in-metric-set"
]

VerificationTest[
  FindMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, All, Method -> { "Embedding", "Pool" -> "ShortestPaths" } ][ "Length" ],
  Length @ Select[ VertexList[ GridGraph[ { 5, 5 } ] ],
    GraphDistance[ GridGraph[ { 5, 5 } ], 1, # ] + GraphDistance[ GridGraph[ { 5, 5 } ], #, 25 ] ==
      GraphDistance[ GridGraph[ { 5, 5 } ], 1, 25 ] & ],
  TestID -> "FindMidpoint-Embedding-Geodesic-pool-equals-metric-interval"
]

VerificationTest[
  FindMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, All, Method -> { "Embedding", "Pool" -> "AllPaths" } ][ "Length" ],
  25,
  TestID -> "FindMidpoint-Embedding-AllPaths-pool-equals-all-vertices"
]


(* ===== FindPerpendicular Method -> "Embedding" ===== *)

VerificationTest[
  Sort @ FindPerpendicular[ GridGraph[ { 5, 5 } ], { 1, 2, 3, 4, 5 }, 13, All, Method -> "Embedding" ][ "Realisations" ],
  { 1, 2, 3, 4, 5 },
  TestID -> "FindPerpendicular-Embedding-pool-equals-line"
]

VerificationTest[
  FindPerpendicular[ GridGraph[ { 5, 5 } ], { 1, 2, 3, 4, 5 }, 13, All, Method -> "Embedding" ][ "First" ],
  3,
  TestID -> "FindPerpendicular-Embedding-closest-foot-is-projection"
]

EndTestSection[]
