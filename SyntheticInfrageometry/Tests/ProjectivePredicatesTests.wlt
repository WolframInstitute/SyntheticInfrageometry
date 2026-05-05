BeginTestSection["ProjectivePredicates"]

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
