BeginTestSection["Homotopy"]

walkGraph       = WolframInstitute`SyntheticInfrageometry`PackageScope`walkGraph;
closedWalkGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`closedWalkGraph;
walkSeq[ w_Graph ] := Last /@ VertexList[ w ]
walkSeqs[ ws_List ] := walkSeq /@ ws

(* ===== Wrapper auto-flatten ===== *)

VerificationTest[
  InfraHomotopy[{InfraHomotopy[{{{1, 2}, {1, 3, 2}}}], InfraHomotopy[{{{1}, {1, 2, 1}}}]}],
  InfraHomotopy[{{{1, 2}, {1, 3, 2}}, {{1}, {1, 2, 1}}}],
  TestID -> "InfraHomotopy-auto-flatten"
]

(* ===== Tree case: every two paths with same endpoints are homotopic ===== *)

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 3}],
  True,
  TestID -> "Tree-equal-paths-homotopic"
]

VerificationTest[
  NullHomotopicQ[PathGraph[Range[5]], {1, 2, 3, 2, 1}],
  True,
  TestID -> "Tree-backtrack-loop-null"
]

(* ===== A walk is read the same as a vertex list and as a path graph ===== *)

VerificationTest[
  { HomotopicQ[PathGraph[Range[5]], walkGraph @ {1, 2, 3, 2, 3}, {1, 2, 3}],
    HomotopicQ[PathGraph[Range[5]], {1, 2, 3, 2, 3}, walkGraph @ {1, 2, 3}] },
  { True, True },
  TestID -> "Walk-graph-and-vertex-list-agree"
]

(* ===== Triangle move ===== *)

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {3}],
  True,
  TestID -> "Triangle-move-with-NullHomotopicCycles3"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {}],
  False,
  TestID -> "Triangle-move-blocked-without-cycles"
]

VerificationTest[
  Length @ First @ FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {3}],
  1,
  TestID -> "Triangle-chain-singleton-wrapper"
]

(* ===== Backtrack reduction ===== *)

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], {1, 2, 3, 2, 3}, {1, 2, 3}],
  True,
  TestID -> "Backtrack-collapse"
]

(* the representative comes back in the shape the input had: a path graph *)
VerificationTest[
  With[{reps = FindInfraHomotopyRepresentative[PathGraph[Range[5]], walkGraph @ {1, 2, 3, 2, 3, 4}]},
    {MatchQ[reps, {_Graph}], PathGraphQ @ First @ reps, walkSeqs @ reps}],
  {True, True, {{1, 2, 3, 4}}},
  TestID -> "Representative-spur-collapse"
]

VerificationTest[
  walkSeqs @ FindInfraHomotopyRepresentative[CompleteGraph[3], {1, 2, 3}, "NullHomotopicCycles" -> {2, 3}],
  {{1, 3}},
  TestID -> "Representative-triangle-shortcut"
]

(* ===== Rectangle (4-cycle) ===== *)

VerificationTest[
  HomotopicQ[CycleGraph[4], {1, 2, 3}, {1, 4, 3}, "NullHomotopicCycles" -> {4}],
  True,
  TestID -> "Rectangle-move-with-Cycles4"
]

VerificationTest[
  HomotopicQ[CycleGraph[4], {1, 2, 3}, {1, 4, 3}, "NullHomotopicCycles" -> {3}],
  False,
  TestID -> "Rectangle-blocked-without-Cycles4"
]

(* ===== Non-contractible loop on a cycle ===== *)

VerificationTest[
  NullHomotopicQ[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}],
  False,
  TestID -> "C6-loop-not-null-default-cycles"
]

VerificationTest[
  NullHomotopicQ[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}, "NullHomotopicCycles" -> {6}],
  True,
  TestID -> "C6-loop-null-when-its-the-cycle"
]

(* a cycle graph is the closed walk itself *)
VerificationTest[
  { NullHomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}],
    NullHomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}, "NullHomotopicCycles" -> {6}] },
  { False, True },
  TestID -> "NullHomotopicQ-on-cycle-graph"
]

(* ===== Hole obstruction (3x3 grid) ===== *)

VerificationTest[
  NullHomotopicQ[GridGraph[{3, 3}], {1, 2, 3, 6, 9, 8, 7, 4, 1}, "NullHomotopicCycles" -> {4}],
  True,
  TestID -> "PlainGrid3x3-loop-null"
]

VerificationTest[
  NullHomotopicQ[VertexDelete[GridGraph[{3, 3}], 5],
    {1, 2, 3, 6, 9, 8, 7, 4, 1}, "NullHomotopicCycles" -> {4}],
  False,
  TestID -> "Grid3x3-with-hole-loop-not-null"
]

(* ===== Endpoints must match ===== *)

VerificationTest[
  HomotopicQ[CycleGraph[4], {1, 2, 3}, {2, 3, 4}],
  False,
  TestID -> "Different-endpoints-not-homotopic"
]

(* ===== Multi-realisation propagation: a list of walk graphs ===== *)

VerificationTest[
  Module[{grid23 = GridGraph[{2, 3}], paths},
    paths = walkGraph /@ FindInfraSegment[grid23, 1, 6, All]["Realizations"];
    Length @ FindInfraHomotopy[grid23, paths, paths, All, "NullHomotopicCycles" -> {3, 4}]["Realizations"]
  ],
  9,
  TestID -> "FindInfraHomotopy-cartesian-3x3-pairs"
]

VerificationTest[
  Module[{grid23 = GridGraph[{2, 3}], paths},
    paths = walkGraph /@ FindInfraSegment[grid23, 1, 6, All]["Realizations"];
    HomotopicQ[grid23, paths, paths, "NullHomotopicCycles" -> {3, 4}]
  ],
  True,
  TestID -> "HomotopicQ-multi-AllTrue-conjunction"
]

(* ===== Wrapper shape: Find* returns one InfraHomotopy carrying all chains ===== *)

VerificationTest[
  MatchQ[
    FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {3}],
    InfraHomotopy[{ _List }]
  ],
  True,
  TestID -> "Find-returns-list-of-unary-wrappers"
]

(* ===== Null-homotopy via the polymorphic FindInfraHomotopy: loop to constant loop ===== *)

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3],
    closedWalkGraph @ {1, 2, 3, 1}, closedWalkGraph @ {1},
    "NullHomotopicCycles" -> {3}]["Realizations"],
  1,
  TestID -> "Null-homotopy-triangle-loop"
]

(* ===== NullHomotopicCycles option parsing ===== *)

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2}, {1, 3, 2}, "NullHomotopicCycles" -> 3],
  True,
  TestID -> "NullHomotopicCycles-integer-shorthand"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2}, {1, 3, 2}],
  True,
  TestID -> "NullHomotopicCycles-default-is-{1,2,3}"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2}, {1, 3, 2}, "NullHomotopicCycles" -> {{1, 2, 3}}],
  True,
  TestID -> "NullHomotopicCycles-explicit-cycle-list"
]

(* ===== Consecutive-duplicate (length-1) reduction ===== *)

VerificationTest[
  walkSeqs @ FindInfraHomotopyRepresentative[PathGraph[Range[5]], {1, 2, 2, 3}],
  {{1, 2, 3}},
  TestID -> "Representative-consecutive-duplicate-default"
]

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 2, 3}],
  True,
  TestID -> "ConsecutiveDuplicate-homotopic-default"
]

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 2, 3}, "NullHomotopicCycles" -> {2, 3}],
  False,
  TestID -> "ConsecutiveDuplicate-blocked-without-1"
]

(* ===== FindInfraHomotopyRepresentative ===== *)

VerificationTest[
  walkSeqs @ FindInfraHomotopyRepresentative[CompleteGraph[3], {1, 2, 3}],
  {{1, 3}},
  TestID -> "Representative-K3-triangle-to-edge"
]

VerificationTest[
  walkSeqs @ FindInfraHomotopyRepresentative[PathGraph[Range[5]], {1, 2, 3, 2, 3}],
  {{1, 2, 3}},
  TestID -> "Representative-spur-reduction"
]

VerificationTest[
  Sort @ walkSeqs @ FindInfraHomotopyRepresentative[CycleGraph[4], {1, 2, 3}, All, "NullHomotopicCycles" -> {4}],
  Sort @ {{1, 2, 3}, {1, 4, 3}},
  TestID -> "Representative-C4-two-minimal-forms-with-4-cycle"
]

(* ===== FindInfraHomotopyRepresentativeHomotopy ===== *)

VerificationTest[
  FindInfraHomotopyRepresentativeHomotopy[CompleteGraph[3], {1, 2, 3}],
  InfraHomotopy[{{{1, 2, 3}, {1, 3}}}],
  TestID -> "RepresentativeHomotopy-K3-triangle-chain"
]

VerificationTest[
  With[{chain = First @ First @ FindInfraHomotopyRepresentativeHomotopy[PathGraph[Range[5]], walkGraph @ {1, 2, 3, 2, 3}]},
    {First[chain], Last[chain]}],
  {{1, 2, 3, 2, 3}, {1, 2, 3}},
  TestID -> "RepresentativeHomotopy-spur-endpoints"
]

(* ===== FindInfraHomotopy Method dispatch ===== *)

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, 1, Method -> "Exhaustive", "NullHomotopicCycles" -> {3}]["Realizations"],
  1,
  TestID -> "FindInfraHomotopy-Exhaustive-triangle"
]

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, 1, Method -> "Greedy", "NullHomotopicCycles" -> {3}]["Realizations"],
  1,
  TestID -> "FindInfraHomotopy-Greedy-triangle"
]

VerificationTest[
  FindInfraHomotopy[CycleGraph[4], {1, 2, 3}, {1, 4, 3}, 1, "NullHomotopicCycles" -> {}],
  $Failed,
  TestID -> "FindInfraHomotopy-disjoint-no-faces-fails"
]

(* ===== Move classification ===== *)

VerificationTest[
  HomotopyMoveType[{1, 2, 3}, {1, 3}],
  "Contract",
  TestID -> "HomotopyMoveType-Contract-triangle"
]

VerificationTest[
  HomotopyMoveType[{1, 3}, {1, 2, 3}],
  "Extend",
  TestID -> "HomotopyMoveType-Extend-triangle"
]

VerificationTest[
  HomotopyMoveType[{1, 2, 3}, {1, 4, 3}],
  "Lateral",
  TestID -> "HomotopyMoveType-Lateral-rectangle"
]

VerificationTest[
  HomotopyMoveTypes[FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {3}]["Realizations"][[1]]],
  {"Contract"},
  TestID -> "HomotopyMoveTypes-from-FindInfraHomotopy"
]

(* ===================== Free loop homotopy: a cycle graph under "FreeHomotopy" ===================== *)

VerificationTest[
  HomotopicQ[CompleteGraph[3], closedWalkGraph @ {1, 2, 3}, closedWalkGraph @ {2, 3, 1}, "FreeHomotopy" -> True],
  True,
  TestID -> "FreeLoop-triangle-rotation"
]

(* the same rotation is not a based homotopy: the base point moved *)
VerificationTest[
  HomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}, closedWalkGraph @ {3, 4, 5, 6, 1, 2},
    "NullHomotopicCycles" -> {}],
  False,
  TestID -> "BasedLoop-C6-rotation-not-based-homotopic"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}, closedWalkGraph @ {3, 4, 5, 6, 1, 2},
    "FreeHomotopy" -> True, "NullHomotopicCycles" -> {}],
  True,
  TestID -> "FreeLoop-C6-loop-rotation"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}, closedWalkGraph @ {1, 6, 5, 4, 3, 2},
    "FreeHomotopy" -> True, "NullHomotopicCycles" -> {}],
  False,
  TestID -> "FreeLoop-C6-orientation-matters"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}, closedWalkGraph @ {1, 6, 5, 4, 3, 2},
    "FreeHomotopy" -> True, "NullHomotopicCycles" -> {6}],
  True,
  TestID -> "FreeLoop-C6-orientation-trivial-when-contractible"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], closedWalkGraph @ {1, 2, 3}, closedWalkGraph @ {2}, "FreeHomotopy" -> True],
  True,
  TestID -> "FreeLoop-triangle-to-constant-at-rotated-base"
]

VerificationTest[
  HomotopicQ[
    Graph[{1 <-> 2, 2 <-> 3, 3 <-> 1, 4 <-> 5, 5 <-> 6, 6 <-> 4}],
    closedWalkGraph @ {1, 2, 3}, closedWalkGraph @ {4, 5, 6}, "FreeHomotopy" -> True],
  False,
  TestID -> "FreeLoop-disjoint-vertex-sets-false"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], closedWalkGraph @ {1, 2, 3, 4, 5, 6}, closedWalkGraph @ {1, 2, 3, 4, 5, 6},
    "FreeHomotopy" -> True, "NullHomotopicCycles" -> {}],
  True,
  TestID -> "FreeLoop-reflexive"
]

(* the representative of a closed walk is a cycle graph; the constant loop is one vertex with a self-loop *)
VerificationTest[
  With[{reps = FindInfraHomotopyRepresentative[CompleteGraph[3], closedWalkGraph @ {1, 2, 3}, "NullHomotopicCycles" -> {3}]},
    {MatchQ[reps, {__Graph}], AllTrue[reps, ! AcyclicGraphQ[#] || ! LoopFreeGraphQ[#] &], walkSeqs @ reps}],
  {True, True, {{1}}},
  TestID -> "Representative-closed-walk-is-cycle-graph"
]

(* ===================== Free path homotopy ===================== *)

VerificationTest[
  Sort @ walkSeqs @ FindInfraHomotopyRepresentative[PathGraph[Range[5]], {1, 2, 3, 4, 5}, "FreeHomotopy" -> True],
  {{1}, {2}, {3}, {4}, {5}},
  TestID -> "FreeHomotopy-path-collapse-to-vertex"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2}, {1, 3}, "FreeHomotopy" -> True, "NullHomotopicCycles" -> {3}],
  True,
  TestID -> "FreeHomotopy-different-endpoints-homotopic"
]

(* ===================== Open against closed is refused ===================== *)

VerificationTest[
  FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, closedWalkGraph @ {1, 2, 3}],
  $Failed,
  {FindInfraHomotopy::mismatch},
  TestID -> "Open-vs-closed-rejected"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], walkGraph @ {1, 2, 3, 1}, closedWalkGraph @ {1, 2, 3}],
  $Failed,
  {HomotopicQ::mismatch},
  TestID -> "Open-walk-returning-to-start-is-not-a-loop"
]

(* ===================== InfraCircle coercion: the circle is the free loop ===================== *)

VerificationTest[
  Sort @ walkSeqs @ FindInfraHomotopyRepresentative[CycleGraph[4], InfraCircle[{{1, 2, 3, 4}}], "NullHomotopicCycles" -> {4}],
  {{1}, {2}, {3}, {4}},
  TestID -> "InfraCircle-coerces-to-free-loop"
]

VerificationTest[
  NullHomotopicQ[CycleGraph[4], InfraCircle[{{1, 2, 3, 4}}], "NullHomotopicCycles" -> {4}],
  True,
  TestID -> "NullHomotopicQ-on-InfraCircle"
]

EndTestSection[]
