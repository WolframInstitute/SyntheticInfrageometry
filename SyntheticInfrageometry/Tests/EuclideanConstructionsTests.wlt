BeginTestSection["EuclideanConstructions"]

(* ===== FindMidpoint ===== *)

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  3,
  TestID -> "FindMidpoint-segment-odd-length"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}],
  2,
  TestID -> "FindMidpoint-segment-even-length-lower-central"
]

VerificationTest[
  FindMidpoint[PathGraph[Range[5]], 1, 5],
  {3},
  TestID -> "FindMidpoint-endpoints-strict-1"
]

VerificationTest[
  Sort @ FindMidpoint[GridGraph[{3, 3}], 1, 9, All],
  Sort @ DeleteDuplicates[
    #[[ Ceiling[ Length[#] / 2 ] ]] & /@ FindSegment[GridGraph[{3, 3}], 1, 9, All]
  ],
  TestID -> "FindMidpoint-all-matches-segment-midpoints"
]

VerificationTest[
  Length @ FindMidpoint[GridGraph[{3, 3}], 1, 9, UpTo[2]] <= 2,
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
  {2},
  TestID -> "FindPerpendicular-CycleGraph5"
]

VerificationTest[
  With[{feet = FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All]},
    AllTrue[feet, MemberQ[{1, 2, 3, 4}, #] &]
  ],
  True,
  TestID -> "FindPerpendicular-feet-on-line"
]

VerificationTest[
  FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, 1],
  {2},
  TestID -> "FindPerpendicular-strict-1"
]

VerificationTest[
  FindPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, Method -> "Spectral"],
  $Failed,
  {FindPerpendicular::nyi},
  TestID -> "FindPerpendicular-spectral-stub"
]

(* ===== FindBisector ===== *)

VerificationTest[
  FindBisector[PathGraph[Range[5]], 1, 5],
  {3},
  TestID -> "FindBisector-path-center"
]

VerificationTest[
  Sort @ FindBisector[CycleGraph[6], 1, 3],
  Sort @ FindBisector[CycleGraph[6], {1, 3}],
  TestID -> "FindBisector-list-form-equiv"
]

VerificationTest[
  FindBisector[PathGraph[Range[5]], 1, 5, All],
  {3},
  TestID -> "FindBisector-all-matches-default"
]

VerificationTest[
  Length @ FindBisector[CycleGraph[6], 1, 3, UpTo[1]],
  1,
  TestID -> "FindBisector-upto-soft"
]

VerificationTest[
  FindBisector[PathGraph[Range[5]], 1, 5, Method -> "Resistance"],
  $Failed,
  {FindBisector::nyi},
  TestID -> "FindBisector-resistance-stub"
]

(* ===== CompleteEquilateralTriangle ===== *)

VerificationTest[
  Sort @ CompleteEquilateralTriangle[CycleGraph[6], 1, 3, All],
  {5},
  TestID -> "CompleteEquilateralTriangle-cycle6"
]

VerificationTest[
  CompleteEquilateralTriangle[PathGraph[Range[5]], 1, 5, All],
  {},
  TestID -> "CompleteEquilateralTriangle-path-no-apex"
]

VerificationTest[
  CompleteEquilateralTriangle[CompleteGraph[4], 1, 2, 1],
  {3},
  TestID -> "CompleteEquilateralTriangle-K4-strict-1"
]

VerificationTest[
  CompleteEquilateralTriangle[CompleteGraph[4], 1, 2, Method -> "Spectral"],
  $Failed,
  {CompleteEquilateralTriangle::nyi},
  TestID -> "CompleteEquilateralTriangle-spectral-stub"
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
  {{5, 6, 7, 8}},
  TestID -> "FindParallel-explicit-metric"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Spectral"],
  $Failed,
  {FindParallel::nyi},
  TestID -> "FindParallel-spectral-stub"
]

VerificationTest[
  FindParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Walk"],
  $Failed,
  {FindParallel::badmethod},
  TestID -> "FindParallel-bad-method"
]

EndTestSection[]
