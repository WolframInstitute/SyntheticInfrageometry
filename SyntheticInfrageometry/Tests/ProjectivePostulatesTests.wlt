BeginTestSection["ProjectivePostulates"]

(* ===== FindPencil ===== *)

VerificationTest[
  With[{p = FindPencil[PathGraph[Range[7]], 4]},
    AssociationQ[p] && Length[p] == 1 && First[Keys[p]] == {1, 2, 3, 4, 5, 6, 7}
  ],
  True,
  TestID -> "FindPencil-PathGraph-center-single-line"
]

VerificationTest[
  Length @ FindPencil[CycleGraph[6], 1],
  4,
  TestID -> "FindPencil-CycleGraph6-cardinality"
]

VerificationTest[
  With[{p = FindPencil[CycleGraph[6], 1]},
    AllTrue[Keys[p], Length[#] == 4 &] &&
    AllTrue[Keys[p], MemberQ[#, 1] &]
  ],
  True,
  TestID -> "FindPencil-CycleGraph6-all-keys-contain-O"
]

VerificationTest[
  With[{p = FindPencil[CycleGraph[6], 1]},
    Count[Values[p], v_ /; MemberQ[v, 4]]
  ],
  2,
  TestID -> "FindPencil-CycleGraph6-antipode-in-two-classes"
]

(* ===== PencilDirections, PencilCardinality ===== *)

VerificationTest[
  PencilCardinality[PathGraph[Range[7]], 4],
  1,
  TestID -> "PencilCardinality-PathGraph"
]

VerificationTest[
  PencilCardinality[CycleGraph[6], 1],
  4,
  TestID -> "PencilCardinality-CycleGraph6"
]

VerificationTest[
  PencilDirections[PathGraph[Range[7]], 4],
  {{1, 2, 3, 4, 5, 6, 7}},
  TestID -> "PencilDirections-PathGraph"
]

VerificationTest[
  Length @ PencilDirections[CycleGraph[6], 1],
  PencilCardinality[CycleGraph[6], 1],
  TestID -> "PencilDirections-Cardinality-agree"
]

(* ===== LineCount ===== *)

VerificationTest[
  LineCount[PathGraph[Range[5]]],
  1,
  TestID -> "LineCount-PathGraph-5"
]

VerificationTest[
  LineCount[CompleteGraph[4]],
  6,
  TestID -> "LineCount-CompleteGraph4-equals-edges"
]

VerificationTest[
  LineCount[PathGraph[Range[7]]],
  1,
  TestID -> "LineCount-PathGraph-7"
]

(* ===== FindCommonLine ===== *)

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindCommonLine-PathGraph-default-1"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindCommonLine-PathGraph-All"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}, UpTo[3]],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindCommonLine-PathGraph-UpTo-soft"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}, 2],
  $Failed,
  TestID -> "FindCommonLine-strict-fails-when-too-few"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 5, 3}],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindCommonLine-three-collinear-vertices"
]

VerificationTest[
  Length @ FindCommonLine[CycleGraph[6], {1, 4}, All],
  2,
  TestID -> "FindCommonLine-CycleGraph6-antipode-two-lines"
]

VerificationTest[
  FindCommonLine[GridGraph[{3, 3}], {1, 9, 5}, All] =!= {} &&
    AllTrue[FindCommonLine[GridGraph[{3, 3}], {1, 9, 5}, All],
      SubsetQ[#, {1, 9, 5}] &],
  True,
  TestID -> "FindCommonLine-GridGraph-diagonal"
]

(* ===== FindCommonPoint ===== *)

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, All],
  {2, 3},
  TestID -> "FindCommonPoint-overlap-two"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, All],
  {},
  TestID -> "FindCommonPoint-disjoint-empty"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, 1],
  {2},
  TestID -> "FindCommonPoint-strict-1"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, 1],
  $Failed,
  TestID -> "FindCommonPoint-strict-fails-when-empty"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, UpTo[5]],
  {2, 3},
  TestID -> "FindCommonPoint-UpTo-soft"
]

EndTestSection[]
