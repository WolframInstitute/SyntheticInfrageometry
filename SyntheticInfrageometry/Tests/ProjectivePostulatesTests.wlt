BeginTestSection["ProjectivePostulates"]

(* ===== FindPencil (returns InfraPencil of constituent InfraRays) ===== *)

VerificationTest[
  With[{p = FindPencil[PathGraph[Range[7]], 4]},
    Head[p] === InfraPencil && p["Length"] == 1 &&
      AllTrue[p["Realisations"], MatchQ[#, InfraRay[_List]] &]
  ],
  True,
  TestID -> "FindPencil-PathGraph-center-single-direction"
]

VerificationTest[
  FindPencil[CycleGraph[6], 1]["Length"],
  4,
  TestID -> "FindPencil-CycleGraph6-cardinality"
]

VerificationTest[
  With[{p = FindPencil[CycleGraph[6], 1]},
    AllTrue[p["Realisations"],
      ray |-> AllTrue[ray["Realisations"], MemberQ[#, 1] &]
    ]
  ],
  True,
  TestID -> "FindPencil-CycleGraph6-rays-contain-O"
]

VerificationTest[
  With[{p = FindPencil[CycleGraph[6], 1]},
    Count[p["Rays"], ray_ /; MemberQ[ray, 4]]
  ],
  2,
  TestID -> "FindPencil-CycleGraph6-antipode-on-two-direction-rays"
]

VerificationTest[
  With[{p = FindPencil[CycleGraph[6], 1]},
    Length[p["Rays"]] == 4 &&
    AllTrue[p["Rays"], MemberQ[#, 1] &]
  ],
  True,
  TestID -> "FindPencil-CycleGraph6-flat-rays"
]

(* ===== FindPencil multi-anchor ===== *)

VerificationTest[
  Head @ FindPencil[CycleGraph[6], InfraPoint[{1, 2}]],
  InfraPencil,
  TestID -> "FindPencil-multi-anchor-head"
]

VerificationTest[
  FindPencil[CycleGraph[6], InfraPoint[{1, 2}]]["Length"] >= 4,
  True,
  TestID -> "FindPencil-multi-anchor-rays-merged"
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
  AllTrue[FindRay[CycleGraph[6], 1, 4, All]["Realisations"], MemberQ[#, 1] && MemberQ[#, 4] &],
  True,
  TestID -> "FindRay-CycleGraph6-rays-contain-O-and-v"
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
    result["Length"] >= 1 && AllTrue[result["Realisations"], SubsetQ[#, {1, 9, 5}] &]
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
  FindCommonPoint[CycleGraph[6], FindCommonLine[CycleGraph[6], {1, 4}, All]["Realisations"], All]["Length"],
  2,
  TestID -> "FindCommonPoint-from-FindCommonLine"
]

(* ===== InfraPencil / InfraRay wrapper boilerplate ===== *)

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
  InfraRay[{{1, 2}, {1, 3}}]["Realisations"],
  {{1, 2}, {1, 3}},
  TestID -> "InfraRay-Realisations"
]

VerificationTest[
  InfraRay[{{1, 2}, {1, 3}}]["Expand"],
  {InfraRay[{{1, 2}}], InfraRay[{{1, 3}}]},
  TestID -> "InfraRay-Expand"
]

VerificationTest[
  InfraPencil[{InfraRay[{{1, 2}}], InfraRay[{{1, 3, 4}, {1, 5, 4}}]}]["Length"],
  2,
  TestID -> "InfraPencil-Length-counts-directions"
]

VerificationTest[
  InfraPencil[{InfraRay[{{1, 2}}], InfraRay[{{1, 3, 4}, {1, 5, 4}}]}]["Rays"],
  {{1, 2}, {1, 3, 4}, {1, 5, 4}},
  TestID -> "InfraPencil-Rays-flattens-constituents"
]

VerificationTest[
  InfraPencil[{InfraRay[{{1, 2}}], InfraRay[{{1, 3}}]}][[1]],
  InfraRay[{{1, 2}}],
  TestID -> "InfraPencil-Part-returns-constituent-Ray"
]

EndTestSection[]
