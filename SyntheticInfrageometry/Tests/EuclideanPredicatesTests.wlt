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
                      Method -> {"Projection", "Equality" -> "Overlap"}],
  True,
  TestID -> "InfraPerpendicularQ-Projection-Overlap-cycle"
]

VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], {1, 2, 3, 4}, {5, 1, 2}],
  False,
  TestID -> "InfraPerpendicularQ-Projection-Subset-cycle-False"
]

(* GridGraph: by 4-fold symmetry of the lattice at any interior vertex, the
   four corner-wedge angles at the centre are all equal (each ArcCos[-1] = Pi
   because graph distance between opposite-quadrant endpoints exceeds the
   Euclidean diagonal -- but all four are equal, which is what the test asks).  *)
VerificationTest[
  InfraPerpendicularQ[GridGraph[{5, 5}], {11, 12, 13, 14, 15}, {3, 8, 13, 18, 23},
                      Method -> "Alexandrov"],
  True,
  TestID -> "InfraPerpendicularQ-Alexandrov-symmetric-True"
]

(* Asymmetric: a "+" cross with an extra diagonal edge 1<->3 collapses the
   north-east corner angle without touching the other three, so the four
   corner angles are not equal and the test must return False. *)
VerificationTest[
  InfraPerpendicularQ[Graph[{0 <-> 1, 0 <-> 2, 0 <-> 3, 0 <-> 4, 1 <-> 3}],
                      {1, 0, 2}, {3, 0, 4}, Method -> {"Alexandrov", "Tolerance" -> 0.5}],
  False,
  TestID -> "InfraPerpendicularQ-Alexandrov-asymmetric-False"
]

(* When the common vertex sits at an endpoint of either line, one half is
   empty and there is no second direction representative on that side, so
   the angle test returns False. *)
VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], {1, 2, 3, 4}, {5, 1, 2}, Method -> "Alexandrov"],
  False,
  TestID -> "InfraPerpendicularQ-Alexandrov-empty-half-False"
]

(* Schoenberg has been removed from InfraPerpendicularQ's method dispatch
   (it carries the same geometric content as {"Alexandrov", "Curvature" -> 0});
   passing it now reports the standard badmethod error. *)
VerificationTest[
  InfraPerpendicularQ[PathGraph[Range[5]], {1, 2, 3}, {3, 4, 5}, Method -> "Schoenberg"],
  $Failed,
  {InfraPerpendicularQ::badmethod},
  TestID -> "InfraPerpendicularQ-Schoenberg-badmethod"
]

(* Coordinate method (ZeroTest -> "Mean" default): GridGraph by 4-fold
   symmetry sends every projection foot to the centre, so the mean signed
   coordinate is exactly 0 in both directions and the test passes. *)
VerificationTest[
  InfraPerpendicularQ[GridGraph[{5, 5}], {11, 12, 13, 14, 15}, {3, 8, 13, 18, 23},
                      Method -> "Coordinate"],
  True,
  TestID -> "InfraPerpendicularQ-Coordinate-Mean-balanced-True"
]

(* Coordinate Mean on the "+"-cross with diagonal edge 1<->3: vertex 3
   projects to {1, 0} (tie), vertex 4 projects to {0}, giving signed coords
   {-0.5, 0} with mean -0.25, so the test fails. *)
VerificationTest[
  InfraPerpendicularQ[Graph[{0 <-> 1, 0 <-> 2, 0 <-> 3, 0 <-> 4, 1 <-> 3}],
                      {1, 0, 2}, {3, 0, 4}, Method -> "Coordinate"],
  False,
  TestID -> "InfraPerpendicularQ-Coordinate-Mean-skewed-False"
]

(* Coordinate ZeroTest -> "Contains": the same "+"-cross has signed coords
   {-0.5, 0} in one direction (interval [-0.5, 0] contains 0 -> True) and
   {-1, 1} in the other (interval [-1, 1] contains 0 -> True), so the
   strictly weaker interval-containment test passes where Mean fails. *)
VerificationTest[
  InfraPerpendicularQ[Graph[{0 <-> 1, 0 <-> 2, 0 <-> 3, 0 <-> 4, 1 <-> 3}],
                      {1, 0, 2}, {3, 0, 4},
                      Method -> {"Coordinate", "ZeroTest" -> "Contains"}],
  True,
  TestID -> "InfraPerpendicularQ-Coordinate-Contains-True"
]

(* Coordinate with the nested ZeroTest spec {"Mean", "Tolerance" -> 0.3}
   relaxes the Mean test enough for the skewed "+"-cross to pass
   (|mean| = 0.25 <= 0.3). *)
VerificationTest[
  InfraPerpendicularQ[Graph[{0 <-> 1, 0 <-> 2, 0 <-> 3, 0 <-> 4, 1 <-> 3}],
                      {1, 0, 2}, {3, 0, 4},
                      Method -> {"Coordinate", "ZeroTest" -> {"Mean", "Tolerance" -> 0.3}}],
  True,
  TestID -> "InfraPerpendicularQ-Coordinate-Mean-nested-tolerance-True"
]

(* Coordinate with an unknown ZeroTest name reports badzerotest. *)
VerificationTest[
  InfraPerpendicularQ[GridGraph[{5, 5}], {11, 12, 13, 14, 15}, {3, 8, 13, 18, 23},
                      Method -> {"Coordinate", "ZeroTest" -> "Bogus"}],
  False,
  {InfraPerpendicularQ::badzerotest},
  TestID -> "InfraPerpendicularQ-Coordinate-badzerotest"
]

(* Radius -> k localises to the k-neighborhood of the common vertex; with
   k = 1 the line restricted to the immediate neighborhood is too short for
   the isosceles-pair algorithm to find any feet (empty projection), so the
   "Overlap" equality fails -- the test correctly tightens with smaller k. *)
VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], {1, 2, 3, 4}, {5, 1, 2},
                      Method -> {"Projection", "Equality" -> "Overlap"}, "Radius" -> 1],
  False,
  TestID -> "InfraPerpendicularQ-Projection-radius-1-tightens"
]

(* InfraSegment / InfraLine wrappers accepted via lineSequence unwrap. *)
VerificationTest[
  InfraPerpendicularQ[CycleGraph[5], InfraLine[{{1, 2, 3, 4}}], InfraSegment[{{5, 1, 2}}],
                      Method -> {"Projection", "Equality" -> "Overlap"}],
  True,
  TestID -> "InfraPerpendicularQ-wrapper-input"
]

(* Empty intersection still False even with Method override. *)
VerificationTest[
  InfraPerpendicularQ[PathGraph[Range[6]], {1, 2}, {5, 6}, Method -> "Alexandrov"],
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
