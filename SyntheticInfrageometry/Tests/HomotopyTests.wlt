BeginTestSection["Homotopy"]

(* ===== Wrapper auto-flatten ===== *)

VerificationTest[
  InfraHomotopy[{InfraHomotopy[{{{1, 2}, {1, 3, 2}}}], InfraHomotopy[{{{1}, {1, 2, 1}}}]}],
  InfraHomotopy[{{{1, 2}, {1, 3, 2}}, {{1}, {1, 2, 1}}}],
  TestID -> "InfraHomotopy-auto-flatten"
]

(* ===== Tree case: every two paths with same endpoints are homotopic ===== *)

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 2, 3}}]],
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
  HomotopicQ[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], "NullHomotopicCycles" -> {3}],
  True,
  TestID -> "Triangle-move-with-NullHomotopicCycles3"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], "NullHomotopicCycles" -> {}],
  False,
  TestID -> "Triangle-move-blocked-without-cycles"
]

VerificationTest[
  Length @ First @ FindInfraHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], "NullHomotopicCycles" -> {3}],
  1,
  TestID -> "Triangle-chain-singleton-wrapper"
]

(* ===== Backtrack reduction ===== *)

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], InfraPath[{{1, 2, 3, 2, 3}}], InfraPath[{{1, 2, 3}}]],
  True,
  TestID -> "Backtrack-collapse"
]

VerificationTest[
  FindInfraHomotopyRepresentative[PathGraph[Range[5]], InfraPath[{{1, 2, 3, 2, 3, 4}}]],
  {InfraPath[{{1, 2, 3, 4}}]},
  TestID -> "Representative-spur-collapse"
]

VerificationTest[
  FindInfraHomotopyRepresentative[CompleteGraph[3], InfraPath[{{1, 2, 3}}], "NullHomotopicCycles" -> {2, 3}],
  {InfraPath[{{1, 3}}]},
  TestID -> "Representative-triangle-shortcut"
]

(* ===== Rectangle (4-cycle) ===== *)

VerificationTest[
  HomotopicQ[CycleGraph[4], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 4, 3}}], "NullHomotopicCycles" -> {4}],
  True,
  TestID -> "Rectangle-move-with-Cycles4"
]

VerificationTest[
  HomotopicQ[CycleGraph[4], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 4, 3}}], "NullHomotopicCycles" -> {3}],
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
  HomotopicQ[CycleGraph[4], InfraPath[{{1, 2, 3}}], InfraPath[{{2, 3, 4}}]],
  False,
  TestID -> "Different-endpoints-not-homotopic"
]

(* ===== Multi-realisation propagation ===== *)

VerificationTest[
  Module[{grid23 = GridGraph[{2, 3}], paths},
    paths = #[[1, 1]] & /@ FindInfraSegment[grid23, 1, 6, All];
    Length @ FindInfraHomotopy[grid23,
      InfraPath[paths], InfraPath[paths], All,
      "NullHomotopicCycles" -> {3, 4}]
  ],
  9,
  TestID -> "FindInfraHomotopy-cartesian-3x3-pairs"
]

VerificationTest[
  Module[{grid23 = GridGraph[{2, 3}], paths},
    paths = #[[1, 1]] & /@ FindInfraSegment[grid23, 1, 6, All];
    HomotopicQ[grid23,
      InfraPath[paths], InfraPath[paths],
      "NullHomotopicCycles" -> {3, 4}]
  ],
  True,
  TestID -> "HomotopicQ-multi-AllTrue-conjunction"
]

(* ===== Wrapper shape: Find* returns List of unary InfraHomotopy ===== *)

VerificationTest[
  MatchQ[
    FindInfraHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], "NullHomotopicCycles" -> {3}],
    { InfraHomotopy[{ _List }] }
  ],
  True,
  TestID -> "Find-returns-list-of-unary-wrappers"
]

(* ===== Null-homotopy via the polymorphic FindInfraHomotopy ===== *)

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3],
    InfraLoop[{{1, 2, 3, 1}}], InfraLoop[{{1}}],
    "NullHomotopicCycles" -> {3}],
  1,
  TestID -> "Null-homotopy-triangle-loop"
]

(* ===== NullHomotopicCycles option parsing ===== *)

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraPath[{{1, 2}}], InfraPath[{{1, 3, 2}}], "NullHomotopicCycles" -> 3],
  True,
  TestID -> "NullHomotopicCycles-integer-shorthand"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraPath[{{1, 2}}], InfraPath[{{1, 3, 2}}]],
  True,
  TestID -> "NullHomotopicCycles-default-is-{1,2,3}"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraPath[{{1, 2}}], InfraPath[{{1, 3, 2}}], "NullHomotopicCycles" -> {{1, 2, 3}}],
  True,
  TestID -> "NullHomotopicCycles-explicit-cycle-list"
]

(* ===== Consecutive-duplicate (length-1) reduction ===== *)

VerificationTest[
  FindInfraHomotopyRepresentative[PathGraph[Range[5]], InfraPath[{{1, 2, 2, 3}}]],
  {InfraPath[{{1, 2, 3}}]},
  TestID -> "Representative-consecutive-duplicate-default"
]

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 2, 2, 3}}]],
  True,
  TestID -> "ConsecutiveDuplicate-homotopic-default"
]

VerificationTest[
  HomotopicQ[PathGraph[Range[5]], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 2, 2, 3}}], "NullHomotopicCycles" -> {2, 3}],
  False,
  TestID -> "ConsecutiveDuplicate-blocked-without-1"
]

(* ===== FindInfraHomotopyRepresentative ===== *)

VerificationTest[
  FindInfraHomotopyRepresentative[CompleteGraph[3], InfraPath[{{1, 2, 3}}]],
  {InfraPath[{{1, 3}}]},
  TestID -> "Representative-K3-triangle-to-edge"
]

VerificationTest[
  FindInfraHomotopyRepresentative[PathGraph[Range[5]], InfraPath[{{1, 2, 3, 2, 3}}]],
  {InfraPath[{{1, 2, 3}}]},
  TestID -> "Representative-spur-reduction"
]

VerificationTest[
  Sort @ FindInfraHomotopyRepresentative[CycleGraph[4], InfraPath[{{1, 2, 3}}], All, "NullHomotopicCycles" -> {4}],
  Sort @ {InfraPath[{{1, 2, 3}}], InfraPath[{{1, 4, 3}}]},
  TestID -> "Representative-C4-two-minimal-forms-with-4-cycle"
]

(* ===== FindInfraHomotopyRepresentativeHomotopy ===== *)

VerificationTest[
  FindInfraHomotopyRepresentativeHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}]],
  {InfraHomotopy[{{{1, 2, 3}, {1, 3}}}]},
  TestID -> "RepresentativeHomotopy-K3-triangle-chain"
]

VerificationTest[
  With[{chain = First @ First @ First @ FindInfraHomotopyRepresentativeHomotopy[PathGraph[Range[5]], InfraPath[{{1, 2, 3, 2, 3}}]]},
    {First[chain], Last[chain]}],
  {{1, 2, 3, 2, 3}, {1, 2, 3}},
  TestID -> "RepresentativeHomotopy-spur-endpoints"
]

(* ===== FindInfraHomotopy Method dispatch ===== *)

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], 1, Method -> "Exhaustive", "NullHomotopicCycles" -> {3}],
  1,
  TestID -> "FindInfraHomotopy-Exhaustive-triangle"
]

VerificationTest[
  Length @ FindInfraHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], 1, Method -> "Greedy", "NullHomotopicCycles" -> {3}],
  1,
  TestID -> "FindInfraHomotopy-Greedy-triangle"
]

VerificationTest[
  FindInfraHomotopy[CycleGraph[4], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 4, 3}}], 1, "NullHomotopicCycles" -> {}],
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
  HomotopyMoveTypes[First @ FindInfraHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraPath[{{1, 3}}], "NullHomotopicCycles" -> {3}]],
  {"Contract"},
  TestID -> "HomotopyMoveTypes-from-FindInfraHomotopy"
]

(* ===================== Free loop homotopy via InfraString ===================== *)

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraString[{{1, 2, 3, 1}}], InfraString[{{2, 3, 1, 2}}]],
  True,
  TestID -> "InfraString-triangle-rotation"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], InfraString[{{1, 2, 3, 4, 5, 6, 1}}], InfraString[{{3, 4, 5, 6, 1, 2, 3}}],
    "NullHomotopicCycles" -> {}],
  True,
  TestID -> "InfraString-C6-loop-rotation"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], InfraString[{{1, 2, 3, 4, 5, 6, 1}}], InfraString[{{1, 6, 5, 4, 3, 2, 1}}],
    "NullHomotopicCycles" -> {}],
  False,
  TestID -> "InfraString-C6-orientation-matters"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], InfraString[{{1, 2, 3, 4, 5, 6, 1}}], InfraString[{{1, 6, 5, 4, 3, 2, 1}}],
    "NullHomotopicCycles" -> {6}],
  True,
  TestID -> "InfraString-C6-orientation-trivial-when-contractible"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraString[{{1, 2, 3, 1}}], InfraString[{{2}}]],
  True,
  TestID -> "InfraString-triangle-to-constant-at-rotated-base"
]

VerificationTest[
  HomotopicQ[
    Graph[{1 <-> 2, 2 <-> 3, 3 <-> 1, 4 <-> 5, 5 <-> 6, 6 <-> 4}],
    InfraString[{{1, 2, 3, 1}}], InfraString[{{4, 5, 6, 4}}]],
  False,
  TestID -> "InfraString-disjoint-vertex-sets-false"
]

VerificationTest[
  HomotopicQ[CycleGraph[6], InfraString[{{1, 2, 3, 4, 5, 6, 1}}], InfraString[{{1, 2, 3, 4, 5, 6, 1}}],
    "NullHomotopicCycles" -> {}],
  True,
  TestID -> "InfraString-reflexive"
]

(* ===================== Free path homotopy ===================== *)

VerificationTest[
  FindInfraHomotopyRepresentative[PathGraph[Range[5]], InfraPath[{{1, 2, 3, 4, 5}}], "FreeHomotopy" -> True],
  {InfraPath[{{1}}]},
  TestID -> "FreeHomotopy-path-collapse-to-vertex"
]

VerificationTest[
  HomotopicQ[CompleteGraph[3], InfraPath[{{1, 2}}], InfraPath[{{1, 3}}], "FreeHomotopy" -> True, "NullHomotopicCycles" -> {3}],
  True,
  TestID -> "FreeHomotopy-different-endpoints-homotopic"
]

(* ===================== Bare-list rejection ===================== *)

VerificationTest[
  Quiet @ FindInfraHomotopyRepresentative[CompleteGraph[3], {1, 2, 3}],
  $Failed,
  TestID -> "Bare-list-rejected-FindInfraHomotopyRepresentative"
]

VerificationTest[
  Quiet @ FindInfraHomotopy[CompleteGraph[3], {1, 2}, {1, 3, 2}],
  $Failed,
  TestID -> "Bare-list-rejected-FindInfraHomotopy"
]

(* ===================== Mismatched wrapper heads ===================== *)

VerificationTest[
  Quiet @ FindInfraHomotopy[CompleteGraph[3], InfraPath[{{1, 2, 3}}], InfraLoop[{{1, 2, 3, 1}}]],
  $Failed,
  TestID -> "Mismatched-heads-rejected"
]

(* ===================== InfraCircle coercion ===================== *)

VerificationTest[
  FindInfraHomotopyRepresentative[CycleGraph[4], InfraCircle[{{1, 2, 3, 4}}], "NullHomotopicCycles" -> {4}],
  {InfraString[{{1}}]},
  TestID -> "InfraCircle-coerces-to-InfraString"
]

VerificationTest[
  NullHomotopicQ[CycleGraph[4], InfraCircle[{{1, 2, 3, 4}}], "NullHomotopicCycles" -> {4}],
  True,
  TestID -> "NullHomotopicQ-on-InfraCircle"
]

EndTestSection[]
