BeginTestSection["Homotopy"]

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

VerificationTest[
  ReducePath[PathGraph[Range[5]], {1, 2, 3, 2, 3, 4}],
  {1, 2, 3, 4},
  TestID -> "ReducePath-spur-collapse"
]

VerificationTest[
  ReducePath[CompleteGraph[3], {1, 2, 3}, "NullHomotopicCycles" -> {2, 3}],
  {1, 3},
  TestID -> "ReducePath-triangle-shortcut"
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

(* ===== Multi-realisation propagation ===== *)

VerificationTest[
  Module[{grid23 = GridGraph[{2, 3}], paths},
    paths = FindInfraSegment[grid23, 1, 6, All];
    Length @ FindInfraHomotopy[grid23,
      InfraSegment @ paths, InfraSegment @ paths, All,
      "NullHomotopicCycles" -> {3, 4}]
  ],
  9,
  TestID -> "FindInfraHomotopy-cartesian-3x3-pairs"
]

VerificationTest[
  Module[{grid23 = GridGraph[{2, 3}], paths},
    paths = FindInfraSegment[grid23, 1, 6, All];
    HomotopicQ[grid23,
      InfraSegment @ paths, InfraSegment @ paths,
      "NullHomotopicCycles" -> {3, 4}]
  ],
  True,
  TestID -> "HomotopicQ-multi-AllTrue-conjunction"
]

(* ===== Wrapper shape: Find* returns List of unary InfraHomotopy ===== *)

VerificationTest[
  MatchQ[
    FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {3}],
    { InfraHomotopy[{ _List }] }
  ],
  True,
  TestID -> "Find-returns-list-of-unary-wrappers"
]

(* ===== FindInfraNullHomotopy on a triangle ===== *)

VerificationTest[
  Length @ FindInfraNullHomotopy[CompleteGraph[3], {1, 2, 3, 1}, "NullHomotopicCycles" -> {3}],
  1,
  TestID -> "FindInfraNullHomotopy-triangle-loop"
]

(* ===== NullHomotopicCycles option parsing ===== *)

VerificationTest[
  HomotopicQ[CompleteGraph[3], {1, 2}, {1, 3, 2}, "NullHomotopicCycles" -> 3],
  True,
  TestID -> "NullHomotopicCycles-integer-shorthand"
]

VerificationTest[
  (* Default = {1, 2, 3} = consecutive-duplicate + 2-cycles + triangles *)
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
  ReducePath[PathGraph[Range[5]], {1, 2, 2, 3}],
  {1, 2, 3},
  TestID -> "ReducePath-consecutive-duplicate-default"
]

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 2, 3}],
  True,
  TestID -> "ConsecutiveDuplicate-homotopic-default"
]

VerificationTest[
  (* Without 1 in the option, consecutive duplicates do not reduce *)
  HomotopicQ[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 2, 3}, "NullHomotopicCycles" -> {2, 3}],
  False,
  TestID -> "ConsecutiveDuplicate-blocked-without-1"
]

(* ===== FindInfraMinimalForms ===== *)

VerificationTest[
  FindInfraMinimalForms[CompleteGraph[3], {1, 2, 3}],
  {{1, 3}},
  TestID -> "FindInfraMinimalForms-K3-triangle-to-edge"
]

VerificationTest[
  FindInfraMinimalForms[PathGraph[Range[5]], {1, 2, 3, 2, 3}],
  {{1, 2, 3}},
  TestID -> "FindInfraMinimalForms-spur-reduction"
]

VerificationTest[
  Sort @ FindInfraMinimalForms[CycleGraph[4], {1, 2, 3}, All, "NullHomotopicCycles" -> {4}],
  Sort @ {{1, 2, 3}, {1, 4, 3}},
  TestID -> "FindInfraMinimalForms-C4-two-minimal-forms-with-4-cycle"
]

(* ===== FindInfraReduction ===== *)

VerificationTest[
  FindInfraReduction[CompleteGraph[3], {1, 2, 3}],
  {{{1, 2, 3}, {1, 3}}},
  TestID -> "FindInfraReduction-K3-triangle-chain"
]

VerificationTest[
  With[{chain = First @ FindInfraReduction[PathGraph[Range[5]], {1, 2, 3, 2, 3}]},
    {First[chain], Last[chain]}],
  {{1, 2, 3, 2, 3}, {1, 2, 3}},
  TestID -> "FindInfraReduction-spur-endpoints"
]

(* ===== FindInfraHomotopy Method dispatch ===== *)

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, 1, Method -> "ViaMinimalForm"],
  1,
  TestID -> "FindInfraHomotopy-ViaMinimalForm-triangle"
]

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, 1, Method -> "MinimumMoves"],
  1,
  TestID -> "FindInfraHomotopy-MinimumMoves-triangle"
]

VerificationTest[
  (* Disjoint minimal-form sets on a 4-cycle with no cycles null-homotopic *)
  FindInfraHomotopy[CycleGraph[4], {1, 2, 3}, {1, 4, 3}, 1, "NullHomotopicCycles" -> {}],
  $Failed,
  TestID -> "FindInfraHomotopy-disjoint-minimal-forms-fails"
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
  (* Rectangle 2-2 swap: 1-2-3 and 1-4-3 have the same length on C4 *)
  HomotopyMoveType[{1, 2, 3}, {1, 4, 3}],
  "Lateral",
  TestID -> "HomotopyMoveType-Lateral-rectangle"
]

VerificationTest[
  HomotopyMoveTypes[First @ FindInfraHomotopy[CompleteGraph[3], {1, 2, 3}, {1, 3}, "NullHomotopicCycles" -> {3}]],
  {"Contract"},
  TestID -> "HomotopyMoveTypes-from-FindInfraHomotopy"
]

(* ===================== HomotopicLoopsQ ===================== *)

(* Same triangle, different base point: a rotation should make them
   freely homotopic *)
VerificationTest[
  HomotopicLoopsQ[CompleteGraph[3], {1, 2, 3, 1}, {2, 3, 1, 2}],
  True,
  TestID -> "HomotopicLoopsQ-triangle-rotation"
]

(* Same loop on C_6, rotated: True even with no faces *)
VerificationTest[
  HomotopicLoopsQ[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}, {3, 4, 5, 6, 1, 2, 3},
    "NullHomotopicCycles" -> {}],
  True,
  TestID -> "HomotopicLoopsQ-C6-loop-rotation"
]

(* Non-contractible loop vs its reversal on C_6: orientation differs,
   not freely homotopic when faces don't kill it *)
VerificationTest[
  HomotopicLoopsQ[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}, {1, 6, 5, 4, 3, 2, 1},
    "NullHomotopicCycles" -> {}],
  False,
  TestID -> "HomotopicLoopsQ-C6-orientation-matters"
]

(* When the hexagon itself is null-homotopic, pi_1 = 1 and any two loops
   based on shared vertices are freely homotopic *)
VerificationTest[
  HomotopicLoopsQ[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}, {1, 6, 5, 4, 3, 2, 1},
    "NullHomotopicCycles" -> {6}],
  True,
  TestID -> "HomotopicLoopsQ-C6-orientation-trivial-when-contractible"
]

(* Triangle loop is freely homotopic to constant loop at any of its vertices *)
VerificationTest[
  HomotopicLoopsQ[CompleteGraph[3], {1, 2, 3, 1}, {2}],
  True,
  TestID -> "HomotopicLoopsQ-triangle-to-constant-at-rotated-base"
]

(* Disjoint loops in a disconnected graph: not freely homotopic *)
VerificationTest[
  HomotopicLoopsQ[
    Graph[{1 <-> 2, 2 <-> 3, 3 <-> 1, 4 <-> 5, 5 <-> 6, 6 <-> 4}],
    {1, 2, 3, 1}, {4, 5, 6, 4}],
  False,
  TestID -> "HomotopicLoopsQ-disjoint-vertex-sets-false"
]

(* Self-equivalence *)
VerificationTest[
  HomotopicLoopsQ[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}, {1, 2, 3, 4, 5, 6, 1},
    "NullHomotopicCycles" -> {}],
  True,
  TestID -> "HomotopicLoopsQ-reflexive"
]

EndTestSection[]
