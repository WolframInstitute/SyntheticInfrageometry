BeginTestSection["EuclideanPredicates"]

(* ===== InfraPathQ ===== *)

VerificationTest[
  InfraPathQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  True,
  TestID -> "InfraPathQ-simple-path"
]

VerificationTest[
  InfraPathQ[PathGraph[Range[5]], {1, 3}],
  False,
  TestID -> "InfraPathQ-non-edge"
]

VerificationTest[
  InfraPathQ[CycleGraph[5], {1, 2, 3, 4, 5, 1}],
  False,
  TestID -> "InfraPathQ-vertex-repeat"
]

VerificationTest[
  InfraPathQ[GridGraph[{3, 3}], {1, 2, 5, 4}],
  True,
  TestID -> "InfraPathQ-non-geodesic-simple"
]

VerificationTest[
  InfraSegmentQ[GridGraph[{3, 3}], {1, 2, 5, 4}],
  False,
  TestID -> "InfraPathQ-non-geodesic-InfraSegmentQ-false"
]

VerificationTest[
  InfraPathQ[PathGraph[Range[5]], {3}],
  False,
  TestID -> "InfraPathQ-single-vertex"
]

(* ===== InfraSegmentQ ===== *)

VerificationTest[
  InfraSegmentQ[PathGraph[Range[5]], {1, 2, 3}],
  True,
  TestID -> "InfraSegmentQ-valid-geodesic"
]

VerificationTest[
  InfraSegmentQ[PathGraph[Range[5]], {1, 3, 5}],
  False,
  TestID -> "InfraSegmentQ-non-adjacent-vertices"
]

VerificationTest[
  InfraSegmentQ[GridGraph[{3, 3}], {1, 4, 7}],
  True,
  TestID -> "InfraSegmentQ-GridGraph-geodesic"
]

VerificationTest[
  InfraSegmentQ[GridGraph[{3, 3}], {1, 4, 5, 2, 3}],
  False,
  TestID -> "InfraSegmentQ-not-shortest-path"
]

VerificationTest[
  InfraSegmentQ[PathGraph[Range[5]], {3}],
  False,
  TestID -> "InfraSegmentQ-single-vertex"
]

(* ===== InfraLineQ ===== *)

VerificationTest[
  InfraLineQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  True,
  TestID -> "InfraLineQ-maximal-geodesic"
]

VerificationTest[
  InfraLineQ[PathGraph[Range[5]], {2, 3, 4}],
  False,
  TestID -> "InfraLineQ-extendable-segment"
]

(* ===== InfraShellQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    InfraShellQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "InfraShellQ-valid-shell"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    InfraShellQ[g, {1, 4}]
  ],
  False,
  TestID -> "InfraShellQ-non-symmetric-pair-not-shell"
]

(* ===== FindInfraShellParameters ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    Length[FindInfraShellParameters[g, {2, 5, 10, 9, 8, 7}]] >= 1
  ],
  True,
  TestID -> "FindInfraShellParameters-finds-center"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    With[{params = FindInfraShellParameters[g, {2, 5, 10, 9, 8, 7}]},
      AllTrue[params, MatchQ[{_, _Integer}]]
    ]
  ],
  True,
  TestID -> "FindInfraShellParameters-returns-pairs"
]

(* ===== InfraCircleQ ===== *)

VerificationTest[
  With[{g = PetersenGraph[]},
    InfraCircleQ[g, {2, 5, 10, 9, 8, 7}]
  ],
  True,
  TestID -> "InfraCircleQ-valid-cycle"
]

VerificationTest[
  With[{g = PetersenGraph[]},
    InfraCircleQ[g, {2, 5, 10, 9, 8, 7, 2}]
  ],
  True,
  TestID -> "InfraCircleQ-accepts-closed-input"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    InfraCircleQ[g, {1, 2, 3}]
  ],
  False,
  TestID -> "InfraCircleQ-no-wrap-around-edge"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    InfraCircleQ[g, {1, 2}]
  ],
  False,
  TestID -> "InfraCircleQ-too-short"
]

(* ===== InfraParallelQ ===== *)

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    InfraParallelQ[g, {1, 2, 3, 4}, {13, 14, 15, 16}]
  ],
  True,
  TestID -> "InfraParallelQ-GridGraph-parallel-rows"
]

VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    InfraParallelQ[g, {1, 2, 3, 4}, {1, 5, 9, 13}]
  ],
  False,
  TestID -> "InfraParallelQ-GridGraph-intersecting"
]

VerificationTest[
  With[{d = GraphDistanceMatrix[GridGraph[{4, 4}]]},
    InfraParallelQ[d, {1, 2, 3, 4}, {13, 14, 15, 16}]
  ],
  True,
  TestID -> "InfraParallelQ-matrix-form"
]

(* ===== InfraPerpendicularQ ===== *)

(* Empty intersection -> False regardless of method. *)
VerificationTest[
  InfraPerpendicularQ[PathGraph[Range[5]], {1, 2, 3}, {4, 5}],
  False,
  TestID -> "InfraPerpendicularQ-disjoint-False"
]

(* CycleGraph[5] line {1, 2, 3, 4} meets line {5, 1, 2} at {1, 2}; the foot
   of perpendicular from 5 onto {1, 2, 3, 4} is 2 (in the intersection), and
   feet from 3, 4 onto {5, 1, 2} lie at {5, 1}, so the Projection-with-Subset
   test fails on the symmetric leg.  Overlap (loosest) passes. *)
VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], {1, 2, 3, 4}, {5, 1, 2},
                      "Equality" -> "Overlap"],
  True,
  TestID -> "InfraPerpendicularQ-Projection-Overlap-cycle"
]

VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], {1, 2, 3, 4}, {5, 1, 2}],
  False,
  TestID -> "InfraPerpendicularQ-Projection-Subset-cycle-False"
]

(* Custom (3, 4, 5) Pythagorean graph: d(4, 1) = 3, d(8, 1) = 4, d(4, 8) = 5.
   Schoenberg's all-pairs strictness fails (only the (4, 8) endpoint pair is
   exactly right-angled), so the predicate returns False -- this is the strict
   synthetic notion and is correctly rare. *)
VerificationTest[
  With[{ g = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 1 <-> 5, 5 <-> 6, 6 <-> 7, 7 <-> 8,
                    4 <-> 9, 9 <-> 10, 10 <-> 11, 11 <-> 12, 12 <-> 8}] },
    InfraPerpendicularQ[g, {4, 3, 2, 1}, {1, 5, 6, 7, 8}, Method -> "Schoenberg"]
  ],
  False,
  TestID -> "InfraPerpendicularQ-Schoenberg-345-strict-False"
]

(* Schoenberg on a path is always negative (collinear), so the strict
   predicate is False there too. *)
VerificationTest[
  InfraPerpendicularQ[PathGraph[Range[7]], {1, 2, 3, 4}, {4, 5, 6, 7},
                      Method -> "Schoenberg"],
  False,
  TestID -> "InfraPerpendicularQ-Schoenberg-collinear-False"
]

(* Radius -> k localises to the k-neighborhood of the common vertex; with
   k = 1 the line restricted to the immediate neighborhood is too short for
   the isosceles-pair algorithm to find any feet (empty projection), so the
   "Overlap" equality fails -- the test correctly tightens with smaller k. *)
VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], {1, 2, 3, 4}, {5, 1, 2},
                      "Equality" -> "Overlap", "Radius" -> 1],
  False,
  TestID -> "InfraPerpendicularQ-Projection-radius-1-tightens"
]

(* InfraSegment / InfraLine wrappers accepted via lineSequence unwrap. *)
VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], InfraLine[{{1, 2, 3, 4}}], InfraSegment[{{5, 1, 2}}],
                      "Equality" -> "Overlap"],
  True,
  TestID -> "InfraPerpendicularQ-wrapper-input"
]

(* Empty intersection still False even with Method override. *)
VerificationTest[
  InfraPerpendicularQ[PathGraph[Range[6]], {1, 2}, {5, 6}, Method -> "Schoenberg"],
  False,
  TestID -> "InfraPerpendicularQ-empty-intersection-False"
]

(* Bad method name reports an error and returns $Failed. *)
VerificationTest[
  InfraPerpendicularQ[PathGraph[Range[5]], {1, 2, 3}, {3, 4, 5}, Method -> "BogusMethod"],
  $Failed,
  {InfraPerpendicularQ::badmethod},
  TestID -> "InfraPerpendicularQ-badmethod"
]

(* ===== SeparatesQ ===== *)

VerificationTest[
  SeparatesQ[PathGraph[Range[5]], {3}, 1, 5],
  True,
  TestID -> "SeparatesQ-path-center"
]

VerificationTest[
  SeparatesQ[PathGraph[Range[5]], {2}, 4, 5],
  False,
  TestID -> "SeparatesQ-path-non-separating"
]

VerificationTest[
  SeparatesQ[PathGraph[Range[5]], {1}, 1, 5],
  False,
  TestID -> "SeparatesQ-endpoint-deletion-false"
]

VerificationTest[
  With[{g = PathGraph[Range[5]]},
    AllTrue[(#[[ 1, 1 ]] & /@ FindInfraBisectingHyperplane[g, 1, 5, All]), h |-> SeparatesQ[g, h, 1, 5]]
  ],
  True,
  TestID -> "SeparatesQ-bisecting-hyperplane-path"
]

(* ===== UniqueInfraSegmentQ ===== *)

VerificationTest[
  UniqueInfraSegmentQ[PathGraph[Range[5]], 1, 5],
  True,
  TestID -> "UniqueInfraSegmentQ-PathGraph-pair-true"
]

VerificationTest[
  UniqueInfraSegmentQ[CycleGraph[4], 1, 3],
  False,
  TestID -> "UniqueInfraSegmentQ-CycleGraph4-antipodes-false"
]

VerificationTest[
  UniqueInfraSegmentQ[PathGraph[Range[5]]],
  True,
  TestID -> "UniqueInfraSegmentQ-PathGraph-whole-true"
]

VerificationTest[
  UniqueInfraSegmentQ[CycleGraph[4]],
  False,
  TestID -> "UniqueInfraSegmentQ-CycleGraph4-whole-false"
]

VerificationTest[
  UniqueInfraSegmentQ[CompleteGraph[5]],
  True,
  TestID -> "UniqueInfraSegmentQ-CompleteGraph-whole-true"
]

EndTestSection[]
