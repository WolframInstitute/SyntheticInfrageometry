BeginTestSection["Enumeration"]

(* ===== EnumerateGraphs: GraphData generator ===== *)

VerificationTest[
  Length @ EnumerateGraphs[4, ConnectedGraphQ],
  6,
  TestID -> "EnumerateGraphs-4-Connected-count"
]

VerificationTest[
  Length @ EnumerateGraphs[5, WhiteheadW1Q],
  11,
  TestID -> "EnumerateGraphs-5-W1-count"
]

VerificationTest[
  Length @ EnumerateGraphs[5, WhiteheadW2Q],
  3,
  TestID -> "EnumerateGraphs-5-W2-count"
]

VerificationTest[
  Length @ EnumerateGraphs[5, ProjectivePlaneGraphQ],
  0,
  TestID -> "EnumerateGraphs-5-ProjectivePlane-empty"
]

(* ===== Strict / soft / exhaustive triple ===== *)

VerificationTest[
  EnumerateGraphs[4, ConnectedGraphQ, All] === EnumerateGraphs[4, ConnectedGraphQ],
  True,
  TestID -> "EnumerateGraphs-default-equals-All"
]

VerificationTest[
  Length @ EnumerateGraphs[4, ConnectedGraphQ, 6],
  6,
  TestID -> "EnumerateGraphs-strict-exact-match"
]

VerificationTest[
  EnumerateGraphs[4, ConnectedGraphQ, 7],
  $Failed,
  TestID -> "EnumerateGraphs-strict-undershoot-fails"
]

VerificationTest[
  Length @ EnumerateGraphs[4, ConnectedGraphQ, UpTo[3]],
  3,
  TestID -> "EnumerateGraphs-UpTo-truncates"
]

VerificationTest[
  EnumerateGraphs[4, ConnectedGraphQ, UpTo[100]] === EnumerateGraphs[4, ConnectedGraphQ],
  True,
  TestID -> "EnumerateGraphs-UpTo-soft-no-fail"
]

(* ===== "From" generator override ===== *)

VerificationTest[
  Length @ EnumerateGraphs[Automatic, ConnectedGraphQ, "From" -> {GridGraph[{2, 2}], CycleGraph[4], PathGraph[Range[3]]}],
  3,
  TestID -> "EnumerateGraphs-From-list"
]

VerificationTest[
  Length @ EnumerateGraphs[Automatic, AcyclicGraphQ, "From" -> {GridGraph[{2, 2}], CycleGraph[4], PathGraph[Range[3]]}],
  1,
  TestID -> "EnumerateGraphs-From-list-filter"
]

EndTestSection[]
