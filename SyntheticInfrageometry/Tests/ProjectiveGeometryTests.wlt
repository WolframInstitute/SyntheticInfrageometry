BeginTestSection["ProjectiveGeometry"]

geodesicGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
walkSequence  = WolframInstitute`SyntheticInfrageometry`PackageScope`walkSequence;
infraSpread   = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;

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

(* ===== FindInfraCommonLine ===== *)

(* count-less is the one line as a path graph; All is the lone bundle, here the
   same carrier since the path has exactly one line; a bounded count is a List *)

VerificationTest[
  walkSequence @ FindInfraCommonLine[PathGraph[Range[5]], {1, 3}],
  {1, 2, 3, 4, 5},
  TestID -> "FindInfraCommonLine-PathGraph-default-1"
]

VerificationTest[
  infraSpread @ FindInfraCommonLine[PathGraph[Range[5]], {1, 3}, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraCommonLine-PathGraph-All"
]

VerificationTest[
  { MatchQ[#, {_Graph}], walkSequence /@ # } & @
    FindInfraCommonLine[PathGraph[Range[5]], {1, 3}, UpTo[3]],
  { True, {{1, 2, 3, 4, 5}} },
  TestID -> "FindInfraCommonLine-PathGraph-UpTo-soft"
]

VerificationTest[
  FindInfraCommonLine[PathGraph[Range[5]], {1, 3}, 2],
  $Failed,
  TestID -> "FindInfraCommonLine-strict-fails-when-too-few"
]

VerificationTest[
  walkSequence @ FindInfraCommonLine[PathGraph[Range[5]], {1, 5, 3}],
  {1, 2, 3, 4, 5},
  TestID -> "FindInfraCommonLine-three-collinear-vertices"
]

VerificationTest[
  Length @ infraSpread @ FindInfraCommonLine[CycleGraph[6], {1, 4}, All],
  2,
  TestID -> "FindInfraCommonLine-CycleGraph6-antipode-two-lines"
]

VerificationTest[
  With[{result = infraSpread @ FindInfraCommonLine[GridGraph[{3, 3}], {1, 9, 5}, All]},
    Length @ result >= 1 && AllTrue[result, SubsetQ[#, {1, 9, 5}] &]
  ],
  True,
  TestID -> "FindInfraCommonLine-GridGraph-diagonal"
]

(* ===== FindInfraCommonLine multi-anchor (density entries) ===== *)

VerificationTest[
  infraSpread @ FindInfraCommonLine[PathGraph[Range[5]], {<| 1 -> 1, 3 -> 1 |>}, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraCommonLine-density-anchor"
]

VerificationTest[
  infraSpread @ FindInfraCommonLine[PathGraph[Range[5]], {<| 1 -> 1, 3 -> 1 |>, 5}, All],
  {{1, 2, 3, 4, 5}},
  TestID -> "FindInfraCommonLine-mixed-anchor"
]

(* ===== FindInfraCommonPoint ===== *)

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, All],
  { 2, 3 },
  TestID -> "FindInfraCommonPoint-overlap-two"
]

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, All],
  { },
  TestID -> "FindInfraCommonPoint-disjoint-empty"
]

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, 1],
  { 2 },
  TestID -> "FindInfraCommonPoint-strict-1"
]

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, 1],
  $Failed,
  TestID -> "FindInfraCommonPoint-strict-fails-when-empty"
]

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, UpTo[5]],
  { 2, 3 },
  TestID -> "FindInfraCommonPoint-UpTo-soft"
]

(* ===== FindInfraCommonPoint on walk graphs ===== *)

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], geodesicGraph /@ {{1, 2, 3}, {2, 3, 4}}, All],
  { 2, 3 },
  TestID -> "FindInfraCommonPoint-walk-graphs"
]

VerificationTest[
  Length[ FindInfraCommonPoint[CycleGraph[6],
    infraSpread @ FindInfraCommonLine[CycleGraph[6], {1, 4}, All], All] ],
  2,
  TestID -> "FindInfraCommonPoint-from-FindInfraCommonLine"
]

(* ===== SameDirectionQ ===== *)

(* the direction at O is oriented: a ray from O through v, not a line through both.
   1 and 7 lie on opposite sides of 4 on the path, so they are opposite directions,
   and no ray from 4 through 1 reaches 7. *)
VerificationTest[
  SameDirectionQ[PathGraph[Range[7]], 4, 1, 7],
  False,
  TestID -> "SameDirectionQ-PathGraph-opposite-sides"
]

VerificationTest[
  SameDirectionQ[PathGraph[Range[7]], 4, 1, 2],
  True,
  TestID -> "SameDirectionQ-PathGraph-same-side"
]

VerificationTest[
  SameDirectionQ[CycleGraph[6], 1, 3, 5],
  False,
  TestID -> "SameDirectionQ-Cycle6-no-common-ray"
]

(* 2 and 5 lie on one line through 1 but on its two sides; on rays they part *)
VerificationTest[
  SameDirectionQ[CycleGraph[6], 1, 2, 5],
  False,
  TestID -> "SameDirectionQ-Cycle6-two-sides-are-two-directions"
]

VerificationTest[
  SameDirectionQ[CycleGraph[6], 1, 2, 3],
  True,
  TestID -> "SameDirectionQ-Cycle6-same-side"
]

VerificationTest[
  SameDirectionQ[PathGraph[Range[5]], 3, 5, 5],
  True,
  TestID -> "SameDirectionQ-equal-vertex-trivial"
]

(* ===== CollinearQ ===== *)

VerificationTest[
  CollinearQ[PathGraph[Range[5]], {1, 2, 3}],
  True,
  TestID -> "CollinearQ-PathGraph-three-points"
]

VerificationTest[
  CollinearQ[PathGraph[Range[5]], {1, 3, 5}],
  True,
  TestID -> "CollinearQ-PathGraph-non-adjacent"
]

VerificationTest[
  CollinearQ[GridGraph[{3, 3}], {1, 2, 4}],
  True,
  TestID -> "CollinearQ-Grid-diagonal-line-exists"
]

VerificationTest[
  CollinearQ[CompleteGraph[4], {1, 2, 3}],
  False,
  TestID -> "CollinearQ-CompleteGraph-no-3-on-a-line"
]

VerificationTest[
  CollinearQ[PathGraph[Range[5]], {3}],
  True,
  TestID -> "CollinearQ-singleton-trivial"
]

(* ===== ConcurrentQ ===== *)

VerificationTest[
  ConcurrentQ[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}, {3, 4, 5}}],
  True,
  TestID -> "ConcurrentQ-three-overlapping"
]

VerificationTest[
  ConcurrentQ[PathGraph[Range[5]], {{1, 2}, {4, 5}}],
  False,
  TestID -> "ConcurrentQ-disjoint-lines"
]

VerificationTest[
  ConcurrentQ[PathGraph[Range[5]], {{1, 2, 3}}],
  True,
  TestID -> "ConcurrentQ-singleton-trivial"
]

(* ===== UniqueCollinearQ ===== *)

VerificationTest[
  UniqueCollinearQ[PathGraph[Range[5]], {1, 3}],
  True,
  TestID -> "UniqueCollinearQ-PathGraph"
]

VerificationTest[
  UniqueCollinearQ[CycleGraph[6], {1, 4}],
  False,
  TestID -> "UniqueCollinearQ-Cycle6-antipode-two-lines"
]

(* ===== UniqueConcurrentQ ===== *)

VerificationTest[
  UniqueConcurrentQ[PathGraph[Range[5]], {{1, 2, 3}, {3, 4, 5}}],
  True,
  TestID -> "UniqueConcurrentQ-PathGraph-meet-at-3"
]

VerificationTest[
  UniqueConcurrentQ[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}],
  False,
  TestID -> "UniqueConcurrentQ-two-shared-vertices"
]

VerificationTest[
  UniqueConcurrentQ[PathGraph[Range[5]], {{1, 2}, {4, 5}}],
  False,
  TestID -> "UniqueConcurrentQ-disjoint-lines"
]

VerificationTest[
  UniqueConcurrentQ[PathGraph[Range[5]], {{1, 2, 3}}],
  False,
  TestID -> "UniqueConcurrentQ-singleton-not-unique"
]

(* ===== Whitehead axioms ===== *)

VerificationTest[
  WhiteheadW1Q[PathGraph[Range[5]]],
  True,
  TestID -> "WhiteheadW1Q-PathGraph"
]

VerificationTest[
  WhiteheadW1Q[CompleteGraph[4]],
  False,
  TestID -> "WhiteheadW1Q-CompleteGraph-edges-have-2-vertices"
]

VerificationTest[
  WhiteheadW2Q[PathGraph[Range[5]]],
  True,
  TestID -> "WhiteheadW2Q-PathGraph-geodetic"
]

VerificationTest[
  WhiteheadW2Q[CycleGraph[4]],
  False,
  TestID -> "WhiteheadW2Q-Cycle4-antipode-two-geodesics"
]

VerificationTest[
  WhiteheadW2Q[CompleteGraph[5]],
  True,
  TestID -> "WhiteheadW2Q-CompleteGraph-trivially-geodetic"
]

VerificationTest[
  WhiteheadW3Q[PathGraph[Range[5]]],
  True,
  TestID -> "WhiteheadW3Q-PathGraph-trivial"
]

VerificationTest[
  ProjectivePlaneGraphQ[PathGraph[Range[5]]],
  False,
  TestID -> "ProjectivePlaneGraphQ-PathGraph-degenerate"
]

VerificationTest[
  ProjectivePlaneGraphQ[CompleteGraph[4]],
  False,
  TestID -> "ProjectivePlaneGraphQ-CompleteGraph-fails-W1"
]

EndTestSection[]
