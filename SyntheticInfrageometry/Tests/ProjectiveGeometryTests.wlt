BeginTestSection["ProjectiveGeometry"]

(* ===== Pencil direction summaries ===== *)

VerificationTest[
  PencilDirections[CycleGraph[6], 1],
  PencilDirections[CycleGraph[6], 1] // Identity,
  TestID -> "PencilDirections-stable"
]

VerificationTest[
  AllTrue[PencilDirections[CycleGraph[6], 1], MemberQ[#, 1] &],
  True,
  TestID -> "PencilDirections-CycleGraph6-all-contain-O"
]

VerificationTest[
  Count[PencilDirections[CycleGraph[6], 1], line_ /; MemberQ[line, 4]],
  2,
  TestID -> "PencilDirections-CycleGraph6-antipode-two-lines"
]

(* ===== FindRay ===== *)

VerificationTest[
  With[{r = FindRay[PathGraph[Range[7]], 4, 7, All]},
    Head[r] === InfraRay && r["Length"] == 1 &&
      MemberQ[r["First"], 4] && MemberQ[r["First"], 7]
  ],
  True,
  TestID -> "FindRay-PathGraph-toward-end"
]

VerificationTest[
  FindRay[CycleGraph[6], 1, 4, All]["Length"],
  2,
  TestID -> "FindRay-CycleGraph6-antipode-two-realisations"
]

VerificationTest[
  AllTrue[FindRay[CycleGraph[6], 1, 4, All]["Realizations"], MemberQ[#, 1] && MemberQ[#, 4] &],
  True,
  TestID -> "FindRay-CycleGraph6-rays-contain-O-and-v"
]

VerificationTest[
  AllTrue[FindRay[CycleGraph[6], 1, 4, All]["Realizations"], First[#] === 1 &],
  True,
  TestID -> "FindRay-CycleGraph6-first-vertex-is-origin"
]

VerificationTest[
  With[{r = FindRay[GridGraph[{3, 3}], 1, 9, 1]},
    Head[r] === InfraRay && r["Length"] == 1 &&
      MemberQ[r["First"], 1] && MemberQ[r["First"], 9]
  ],
  True,
  TestID -> "FindRay-GridGraph-strict-1"
]

VerificationTest[
  FindRay[CycleGraph[6], 1, 4, 5],
  $Failed,
  TestID -> "FindRay-strict-shortfall"
]

VerificationTest[
  FindRay[CycleGraph[6], 1, 4, UpTo[10]]["Length"],
  2,
  TestID -> "FindRay-UpTo-soft"
]

VerificationTest[
  Head @ FindRay[CycleGraph[6], InfraPoint[{1, 2}], 4, All],
  InfraRay,
  TestID -> "FindRay-multi-anchor-head"
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
  Length @ PencilDirections[PathGraph[Range[7]], 4],
  PencilCardinality[PathGraph[Range[7]], 4],
  TestID -> "PencilDirections-PathGraph-cardinality-agree"
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
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindCommonLine-PathGraph-default-1"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}, All],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindCommonLine-PathGraph-All"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}, UpTo[3]],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindCommonLine-PathGraph-UpTo-soft"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 3}, 2],
  $Failed,
  TestID -> "FindCommonLine-strict-fails-when-too-few"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {1, 5, 3}],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindCommonLine-three-collinear-vertices"
]

VerificationTest[
  FindCommonLine[CycleGraph[6], {1, 4}, All]["Length"],
  2,
  TestID -> "FindCommonLine-CycleGraph6-antipode-two-lines"
]

VerificationTest[
  With[{result = FindCommonLine[GridGraph[{3, 3}], {1, 9, 5}, All]},
    result["Length"] >= 1 && AllTrue[result["Realizations"], SubsetQ[#, {1, 9, 5}] &]
  ],
  True,
  TestID -> "FindCommonLine-GridGraph-diagonal"
]

(* ===== FindCommonLine multi-anchor (wrapped entries) ===== *)

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {InfraPoint[{1, 3}]}, All],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindCommonLine-InfraPoint-anchor"
]

VerificationTest[
  FindCommonLine[PathGraph[Range[5]], {InfraPoint[{1, 3}], 5}, All],
  InfraSegment[{{1, 2, 3, 4, 5}}],
  TestID -> "FindCommonLine-mixed-anchor"
]

(* ===== FindCommonPoint ===== *)

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, All],
  InfraPoint[{2, 3}],
  TestID -> "FindCommonPoint-overlap-two"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, All],
  InfraPoint[{}],
  TestID -> "FindCommonPoint-disjoint-empty"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, 1],
  InfraPoint[{2}],
  TestID -> "FindCommonPoint-strict-1"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, 1],
  $Failed,
  TestID -> "FindCommonPoint-strict-fails-when-empty"
]

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, UpTo[5]],
  InfraPoint[{2, 3}],
  TestID -> "FindCommonPoint-UpTo-soft"
]

(* ===== FindCommonPoint with wrapped lines ===== *)

VerificationTest[
  FindCommonPoint[PathGraph[Range[5]], {InfraSegment[{{1, 2, 3}}], InfraSegment[{{2, 3, 4}}]}, All],
  InfraPoint[{2, 3}],
  TestID -> "FindCommonPoint-InfraSegment-wrapped"
]

VerificationTest[
  FindCommonPoint[CycleGraph[6], FindCommonLine[CycleGraph[6], {1, 4}, All]["Realizations"], All]["Length"],
  2,
  TestID -> "FindCommonPoint-from-FindCommonLine"
]

(* ===== InfraRay wrapper boilerplate ===== *)

VerificationTest[
  InfraRay[{InfraRay[{{1, 2}}], InfraRay[{{1, 3}}]}],
  InfraRay[{{1, 2}, {1, 3}}],
  TestID -> "InfraRay-auto-flatten"
]

VerificationTest[
  InfraRay[{{1, 2}, {1, 3}}][[1]],
  InfraRay[{{1, 2}}],
  TestID -> "InfraRay-Part-wraps-singleton"
]

VerificationTest[
  InfraRay[{{1, 2}, {1, 3}}]["Length"],
  2,
  TestID -> "InfraRay-Length"
]

VerificationTest[
  InfraRay[{{1, 2}, {1, 3}}]["Realizations"],
  {{1, 2}, {1, 3}},
  TestID -> "InfraRay-Realizations"
]

VerificationTest[
  InfraRay[{{1, 2}, {1, 3}}]["Expand"],
  {InfraRay[{{1, 2}}], InfraRay[{{1, 3}}]},
  TestID -> "InfraRay-Expand"
]

(* ===== SameDirectionQ ===== *)

VerificationTest[
  SameDirectionQ[PathGraph[Range[7]], 4, 1, 7],
  True,
  TestID -> "SameDirectionQ-PathGraph-line-spans"
]

VerificationTest[
  SameDirectionQ[CycleGraph[6], 1, 3, 5],
  False,
  TestID -> "SameDirectionQ-Cycle6-no-common-line"
]

VerificationTest[
  SameDirectionQ[CycleGraph[6], 1, 2, 5],
  True,
  TestID -> "SameDirectionQ-Cycle6-line-through-O-with-both-sides"
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

(* ===== UniquePencilQ ===== *)

VerificationTest[
  UniquePencilQ[PathGraph[Range[7]], 4],
  True,
  TestID -> "UniquePencilQ-PathGraph"
]

VerificationTest[
  UniquePencilQ[CycleGraph[6], 1],
  False,
  TestID -> "UniquePencilQ-Cycle6-antipode-multivalued"
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
