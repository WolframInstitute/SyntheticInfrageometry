BeginTestSection["TropicalPostulates"]

(* ===== FindTropicalSegment ===== *)

VerificationTest[
  FindTropicalSegment[PathGraph[Range[5]], 1, 5, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindTropicalSegment-PathGraph-unique"
]

VerificationTest[
  FindTropicalSegment[PathGraph[Range[5]], 1, 5],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindTropicalSegment-PathGraph-default-1"
]

VerificationTest[
  Length @ FindTropicalSegment[CycleGraph[4], 1, 3, All],
  2,
  TestID -> "FindTropicalSegment-CycleGraph4-two-segments"
]

VerificationTest[
  Sort @ FindTropicalSegment[CycleGraph[4], 1, 3, All],
  {{1, 2, 3}, {1, 3, 4}},
  TestID -> "FindTropicalSegment-CycleGraph4-content"
]

VerificationTest[
  Length @ FindTropicalSegment[GridGraph[{3, 3}], 1, 9, All],
  6,
  TestID -> "FindTropicalSegment-GridGraph3x3-six"
]

VerificationTest[
  AllTrue[FindTropicalSegment[GridGraph[{3, 3}], 1, 9, All],
    set |-> Length[set] == 5 && SubsetQ[set, {1, 9}]],
  True,
  TestID -> "FindTropicalSegment-GridGraph3x3-shape"
]

VerificationTest[
  FindTropicalSegment[CycleGraph[4], 1, 3, 5],
  $Failed,
  TestID -> "FindTropicalSegment-strict-fails-when-too-few"
]

VerificationTest[
  FindTropicalSegment[CycleGraph[4], 1, 3, 2],
  FindTropicalSegment[CycleGraph[4], 1, 3, All],
  TestID -> "FindTropicalSegment-strict-2-matches-all"
]

VerificationTest[
  FindTropicalSegment[CycleGraph[4], 1, 3, UpTo[1]],
  Take[FindTropicalSegment[CycleGraph[4], 1, 3, All], 1],
  TestID -> "FindTropicalSegment-UpTo-1"
]

VerificationTest[
  FindTropicalSegment[CycleGraph[4], 1, 3, UpTo[5]],
  FindTropicalSegment[CycleGraph[4], 1, 3, All],
  TestID -> "FindTropicalSegment-UpTo-soft"
]

VerificationTest[
  FindTropicalSegment[PathGraph[Range[5]], 3, 3, All],
  {},
  TestID -> "FindTropicalSegment-self-empty"
]

(* ===== FindGeodesicConvexHull ===== *)

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {1, 5}],
  {1, 2, 3, 4, 5},
  TestID -> "FindGeodesicConvexHull-PathGraph-endpoints"
]

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {2, 4}],
  {2, 3, 4},
  TestID -> "FindGeodesicConvexHull-PathGraph-interior"
]

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {3}],
  {3},
  TestID -> "FindGeodesicConvexHull-singleton"
]

VerificationTest[
  FindGeodesicConvexHull[PathGraph[Range[5]], {}],
  {},
  TestID -> "FindGeodesicConvexHull-empty"
]

VerificationTest[
  FindGeodesicConvexHull[CycleGraph[4], {1, 3}],
  {1, 2, 3, 4},
  TestID -> "FindGeodesicConvexHull-CycleGraph4-antipodes-fill"
]

VerificationTest[
  FindGeodesicConvexHull[GridGraph[{3, 3}], {1, 9}],
  Range[9],
  TestID -> "FindGeodesicConvexHull-GridGraph3x3-corners-fill"
]

VerificationTest[
  FindGeodesicConvexHull[GridGraph[{3, 3}], {1, 3}],
  {1, 2, 3},
  TestID -> "FindGeodesicConvexHull-GridGraph3x3-row-stays-row"
]

VerificationTest[
  FindGeodesicConvexHull[CompleteGraph[5], {1, 2}],
  {1, 2},
  TestID -> "FindGeodesicConvexHull-CompleteGraph-edge-stays-edge"
]

EndTestSection[]
