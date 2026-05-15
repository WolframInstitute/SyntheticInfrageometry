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

(* ===== FindInfraRay ===== *)

VerificationTest[
  With[{r = FindInfraRay[PathGraph[Range[7]], 4, 7, All]},
    MatchQ[r, { InfraRay[{ _ }] .. }] && Length @ r == 1 &&
      MemberQ[r[[ 1, 1, 1 ]], 4] && MemberQ[r[[ 1, 1, 1 ]], 7]
  ],
  True,
  TestID -> "FindInfraRay-PathGraph-toward-end"
]

VerificationTest[
  Length @ FindInfraRay[CycleGraph[6], 1, 4, All],
  2,
  TestID -> "FindInfraRay-CycleGraph6-antipode-two-realisations"
]

VerificationTest[
  AllTrue[(#[[ 1, 1 ]] & /@ FindInfraRay[CycleGraph[6], 1, 4, All]), MemberQ[#, 1] && MemberQ[#, 4] &],
  True,
  TestID -> "FindInfraRay-CycleGraph6-rays-contain-O-and-v"
]

VerificationTest[
  AllTrue[(#[[ 1, 1 ]] & /@ FindInfraRay[CycleGraph[6], 1, 4, All]), First[#] === 1 &],
  True,
  TestID -> "FindInfraRay-CycleGraph6-first-vertex-is-origin"
]

VerificationTest[
  With[{r = FindInfraRay[GridGraph[{3, 3}], 1, 9, 1]},
    MatchQ[r, { InfraRay[{ _ }] .. }] && Length @ r == 1 &&
      MemberQ[r[[ 1, 1, 1 ]], 1] && MemberQ[r[[ 1, 1, 1 ]], 9]
  ],
  True,
  TestID -> "FindInfraRay-GridGraph-strict-1"
]

VerificationTest[
  FindInfraRay[CycleGraph[6], 1, 4, 5],
  $Failed,
  TestID -> "FindInfraRay-strict-shortfall"
]

VerificationTest[
  Length @ FindInfraRay[CycleGraph[6], 1, 4, UpTo[10]],
  2,
  TestID -> "FindInfraRay-UpTo-soft"
]

VerificationTest[
  MatchQ[ FindInfraRay[CycleGraph[6], InfraPoint[{1, 2}], 4, All], { InfraRay[{ _ }] .. } ],
  True,
  TestID -> "FindInfraRay-multi-anchor-shape"
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

(* ===== FindInfraCommonLine ===== *)

VerificationTest[
  InfraLine @ FindInfraCommonLine[PathGraph[Range[5]], {1, 3}],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraCommonLine-PathGraph-default-1"
]

VerificationTest[
  InfraLine @ FindInfraCommonLine[PathGraph[Range[5]], {1, 3}, All],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraCommonLine-PathGraph-All"
]

VerificationTest[
  InfraLine @ FindInfraCommonLine[PathGraph[Range[5]], {1, 3}, UpTo[3]],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraCommonLine-PathGraph-UpTo-soft"
]

VerificationTest[
  FindInfraCommonLine[PathGraph[Range[5]], {1, 3}, 2],
  $Failed,
  TestID -> "FindInfraCommonLine-strict-fails-when-too-few"
]

VerificationTest[
  InfraLine @ FindInfraCommonLine[PathGraph[Range[5]], {1, 5, 3}],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraCommonLine-three-collinear-vertices"
]

VerificationTest[
  Length @ FindInfraCommonLine[CycleGraph[6], {1, 4}, All],
  2,
  TestID -> "FindInfraCommonLine-CycleGraph6-antipode-two-lines"
]

VerificationTest[
  With[{result = FindInfraCommonLine[GridGraph[{3, 3}], {1, 9, 5}, All]},
    Length @ result >= 1 && AllTrue[(#[[ 1, 1 ]] & /@ result), SubsetQ[#, {1, 9, 5}] &]
  ],
  True,
  TestID -> "FindInfraCommonLine-GridGraph-diagonal"
]

(* ===== FindInfraCommonLine multi-anchor (wrapped entries) ===== *)

VerificationTest[
  InfraLine @ FindInfraCommonLine[PathGraph[Range[5]], {InfraPoint[{1, 3}]}, All],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraCommonLine-InfraPoint-anchor"
]

VerificationTest[
  InfraLine @ FindInfraCommonLine[PathGraph[Range[5]], {InfraPoint[{1, 3}], 5}, All],
  InfraLine[{{1, 2, 3, 4, 5}}],
  TestID -> "FindInfraCommonLine-mixed-anchor"
]

(* ===== FindInfraCommonPoint ===== *)

VerificationTest[
  InfraPoint @ FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, All],
  InfraPoint[{2, 3}],
  TestID -> "FindInfraCommonPoint-overlap-two"
]

VerificationTest[
  InfraPoint @ FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, All],
  InfraPoint[{}],
  TestID -> "FindInfraCommonPoint-disjoint-empty"
]

VerificationTest[
  InfraPoint @ FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, 1],
  InfraPoint[{2}],
  TestID -> "FindInfraCommonPoint-strict-1"
]

VerificationTest[
  FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2}, {3, 4}}, 1],
  $Failed,
  TestID -> "FindInfraCommonPoint-strict-fails-when-empty"
]

VerificationTest[
  InfraPoint @ FindInfraCommonPoint[PathGraph[Range[5]], {{1, 2, 3}, {2, 3, 4}}, UpTo[5]],
  InfraPoint[{2, 3}],
  TestID -> "FindInfraCommonPoint-UpTo-soft"
]

(* ===== FindInfraCommonPoint with wrapped lines ===== *)

VerificationTest[
  InfraPoint @ FindInfraCommonPoint[PathGraph[Range[5]], {InfraSegment[{{1, 2, 3}}], InfraSegment[{{2, 3, 4}}]}, All],
  InfraPoint[{2, 3}],
  TestID -> "FindInfraCommonPoint-InfraSegment-wrapped"
]

VerificationTest[
  Length @ FindInfraCommonPoint[CycleGraph[6], (#[[ 1, 1 ]] & /@ FindInfraCommonLine[CycleGraph[6], {1, 4}, All]), All],
  2,
  TestID -> "FindInfraCommonPoint-from-FindInfraCommonLine"
]

(* ===== InfraRay wrapper boilerplate ===== *)

VerificationTest[
  InfraRay[{InfraRay[{{1, 2}}], InfraRay[{{1, 3}}]}],
  InfraRay[{{1, 2}, {1, 3}}],
  TestID -> "InfraRay-auto-flatten"
]

VerificationTest[
  InfraRay[{{1, 2}, {1, 3}}][[1]],
  {{1, 2}, {1, 3}},
  TestID -> "InfraRay-Part-first-arg"
]

VerificationTest[
  Length @ First @ InfraRay[{{1, 2}, {1, 3}}],
  2,
  TestID -> "InfraRay-Length-of-inner"
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
