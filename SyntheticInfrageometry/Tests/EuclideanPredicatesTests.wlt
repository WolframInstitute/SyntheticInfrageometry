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

(* ===== FindInfraShellCenter (per-radius estimates, two methods) ===== *)

(* even chord d(1,7)=6: one estimate, exact midpoint 4 at radius 3 *)
VerificationTest[
  FindInfraShellCenter[PathGraph[Range[7]], {1, 7}],
  {{InfraMesoPoint[<|4 -> 1|>], 3}},
  TestID -> "FindInfraShellCenter-path-even-exact-midpoint"
]

VerificationTest[
  FindInfraShellCenter[CycleGraph[8], {1, 5}],
  {{InfraMesoPoint[<|3 -> 1, 7 -> 1|>], 2}},
  TestID -> "FindInfraShellCenter-cycle-two-antipodal-midpoints"
]

VerificationTest[
  MatchQ[FindInfraShellCenter[PetersenGraph[], {2, 5, 10, 9, 8, 7}], {{_InfraMesoPoint, _Integer} ..}],
  True,
  TestID -> "FindInfraShellCenter-returns-estimate-list"
]

VerificationTest[
  With[{g = GridGraph[{5, 5}], shell = Select[VertexList[GridGraph[{5, 5}]], GraphDistance[GridGraph[{5, 5}], 13, #] == 2 &]},
    MemberQ[FindInfraShellCenter[g, shell][[1, 1]]["Vertices"], 13]
  ],
  True,
  TestID -> "FindInfraShellCenter-blob-contains-true-center"
]

(* odd chord d(1,6)=5: Even parity drops it (no estimates) ... *)
VerificationTest[
  FindInfraShellCenter[PathGraph[Range[6]], {1, 6}, Method -> {"MaximalChordsBisectors", "Parity" -> "Even"}],
  {},
  TestID -> "FindInfraShellCenter-Even-drops-odd-chords"
]

(* ... Odd parity keeps it, the mesopoint splits across radii 2 and 3 *)
VerificationTest[
  FindInfraShellCenter[PathGraph[Range[6]], {1, 6}, Method -> {"MaximalChordsBisectors", "Parity" -> "Odd"}],
  {{InfraMesoPoint[<|3 -> 1|>], 2}, {InfraMesoPoint[<|4 -> 1|>], 3}},
  TestID -> "FindInfraShellCenter-Odd-splits-mesopoint-by-radius"
]

(* "Diameter" keeps only the globally longest chord {1,10} (odd, splits
   into 5@4 and 6@5); "PerVertex" also keeps vertex 4's chord {4,10}
   (even, midpoint 7@3). *)
VerificationTest[
  { FindInfraShellCenter[PathGraph[Range[10]], {1, 4, 10}, Method -> {"MaximalChordsBisectors", "Maximality" -> "Diameter"}],
    FindInfraShellCenter[PathGraph[Range[10]], {1, 4, 10}, Method -> {"MaximalChordsBisectors", "Maximality" -> "PerVertex"}] },
  { {{InfraMesoPoint[<|5 -> 1|>], 4}, {InfraMesoPoint[<|6 -> 1|>], 5}},
    {{InfraMesoPoint[<|7 -> 1|>], 3}, {InfraMesoPoint[<|5 -> 1|>], 4}, {InfraMesoPoint[<|6 -> 1|>], 5}} },
  TestID -> "FindInfraShellCenter-Maximality-Diameter-vs-PerVertex"
]

(* EquidistantPoints recovers the exact center / radius the bisector
   estimator misses on a small-diameter graph: Petersen's radius-2 sphere
   {2,5,10,9,8,7} is centered exactly at vertex 1. *)
VerificationTest[
  FindInfraShellCenter[PetersenGraph[], {2, 5, 10, 9, 8, 7}, Method -> "EquidistantPoints"],
  {{InfraMesoPoint[<|1 -> 1|>], 2}},
  TestID -> "FindInfraShellCenter-EquidistantPoints-Petersen-true-center"
]

VerificationTest[
  FindInfraShellCenter[CycleGraph[8], {1, 5}, Method -> "EquidistantPoints"],
  {{InfraMesoPoint[<|3 -> 1, 7 -> 1|>], 2}},
  TestID -> "FindInfraShellCenter-EquidistantPoints-cycle-two-centers"
]

VerificationTest[
  FindInfraShellCenter[GridGraph[{3, 3}], {1, 9}, Method -> "Bogus"],
  $Failed,
  {FindInfraShellCenter::badmethod},
  TestID -> "FindInfraShellCenter-bad-method"
]

VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    FindInfraShellCenter[g, FindInfraShell[g, 13, 2]] ===
      FindInfraShellCenter[g, Select[VertexList[g], GraphDistance[g, 13, #] == 2 &]]
  ],
  True,
  TestID -> "FindInfraShellCenter-accepts-InfraShell-wrapper"
]

VerificationTest[
  FindInfraShellCenter[GridGraph[{3, 3}], {1, 9}, Method -> {"MaximalChordsBisectors", "Distance" -> "Bogus"}],
  $Failed,
  {FindInfraShellCenter::baddistance},
  TestID -> "FindInfraShellCenter-bad-distance"
]

(* within the radius-2 estimate, the heaviest support vertex is the true center 13 *)
VerificationTest[
  With[{g = GridGraph[{5, 5}], shell = Select[VertexList[GridGraph[{5, 5}]], GraphDistance[GridGraph[{5, 5}], 13, #] == 2 &]},
    With[{center = FindInfraShellCenter[g, shell][[1, 1]]},
      MemberQ[Pick[center["Vertices"], center["Weights"], Max @ center["Weights"]], 13]
    ]
  ],
  True,
  TestID -> "FindInfraShellCenter-heaviest-support-is-true-center"
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
    AllTrue[FindInfraBisectingHyperplane[g, 1, 5, All]["Realizations"], h |-> SeparatesQ[g, h, 1, 5]]
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

(* ===== InfraPlaneQ ===== *)

(* The bisector FindInfraBisectingHyperplane returns must satisfy its own predicate. *)
VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    InfraPlaneQ[g, FindInfraBisectingHyperplane[g, 11, 15][[1, 1]], 11, 15]],
  True,
  TestID -> "InfraPlaneQ-roundtrip-grid-bisector"
]

(* Every vertex of a bisector is equidistant from the two foci. *)
VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    AllTrue[FindInfraBisectingHyperplane[g, 11, 15][[1, 1]],
      GraphDistance[g, 11, #] == GraphDistance[g, 15, #] &]],
  True,
  TestID -> "InfraPlaneQ-bisector-is-equidistant"
]

(* Dropping a vertex breaks separation, so the remainder is not a hyperplane. *)
VerificationTest[
  With[{g = GridGraph[{5, 5}]},
    InfraPlaneQ[g, Rest @ FindInfraBisectingHyperplane[g, 11, 15][[1, 1]], 11, 15]],
  False,
  TestID -> "InfraPlaneQ-punctured-bisector-false"
]

(* A set containing a focus never separates it from anything. *)
VerificationTest[
  InfraPlaneQ[GridGraph[{5, 5}], {11, 13}, 11, 15],
  False,
  TestID -> "InfraPlaneQ-set-containing-focus-false"
]

(* On an odd path the strict bisector is empty and cannot separate. *)
VerificationTest[
  InfraPlaneQ[PathGraph[Range[5]], {3}, 1, 5],
  True,
  TestID -> "InfraPlaneQ-path-midvertex-separates"
]

(* On a 4-cycle the two off-diagonal vertices are equidistant from the antipodal
   pair and separate it, so they form a hyperplane already at window 0; widening
   the slab cannot break that. *)
VerificationTest[
  {InfraPlaneQ[CycleGraph[4], {2, 4}, 1, 3],
   InfraPlaneQ[CycleGraph[4], {2, 4}, 1, 3, 1]},
  {True, True},
  TestID -> "InfraPlaneQ-cycle4-antipodal-band"
]

(* The window widens the slab: a vertex one step off the bisector is rejected
   at window 0 and accepted at window 1. *)
VerificationTest[
  With[{g = PathGraph[Range[5]]},
    {InfraPlaneQ[g, {2}, 1, 5], InfraPlaneQ[g, {2}, 1, 5, 3]}],
  {False, True},
  TestID -> "InfraPlaneQ-window-widens-slab"
]

(* The graph-free three-argument form is the inert scene assertion: it must
   stay unevaluated so InfraScene can hold it until bindings resolve. *)
VerificationTest[
  MatchQ[InfraPlaneQ[{3, 8, 13}, 11, 15], _InfraPlaneQ],
  True,
  TestID -> "InfraPlaneQ-scene-form-stays-inert"
]

EndTestSection[]
