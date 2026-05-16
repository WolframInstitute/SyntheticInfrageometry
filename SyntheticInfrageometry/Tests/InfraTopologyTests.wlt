BeginTestSection["InfraTopology"]

(* ===== BallClosure ===== *)

VerificationTest[
  Sort @ BallClosure[PathGraph[Range[5]], 1, #] & /@ Range[5],
  {{1, 2}, {2}, {3}, {4}, {4, 5}},
  TestID -> "BallClosure-PathGraph-r1-all-vertices"
]

VerificationTest[
  And @@ (Sort @ BallClosure[PathGraph[Range[5]], 0, #] === {#} & /@ Range[5]),
  True,
  TestID -> "BallClosure-PathGraph-r0-discrete"
]

VerificationTest[
  Union[Sort @ BallClosure[CycleGraph[6], 1, #] & /@ Range[6]],
  {{1}, {2}, {3}, {4}, {5}, {6}},
  TestID -> "BallClosure-CycleGraph6-r1-discrete"
]

VerificationTest[
  And @@ (Sort @ BallClosure[CompleteGraph[4], 1, #] === {1, 2, 3, 4} & /@ Range[4]),
  True,
  TestID -> "BallClosure-CompleteGraph4-r1-indiscrete"
]

VerificationTest[
  Sort @ BallClosure[StarGraph[5], 1, 1],
  {1},
  TestID -> "BallClosure-StarGraph5-hub"
]

VerificationTest[
  Sort @ BallClosure[StarGraph[5], 1, 2],
  {1, 2},
  TestID -> "BallClosure-StarGraph5-leaf"
]

VerificationTest[
  Sort @ BallClosure[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], 1, 1],
  {1, 2},
  TestID -> "BallClosure-disconnected-class"
]

VerificationTest[
  Sort @ BallClosure[PathGraph[Range[5]], 1, InfraPoint[{{1}}]],
  Sort @ BallClosure[PathGraph[Range[5]], 1, 1],
  TestID -> "BallClosure-accepts-InfraPoint"
]

(* ===== BallTopologyGraph ===== *)

VerificationTest[
  Sort @ VertexList @ BallTopologyGraph[PathGraph[Range[5]], 1],
  Range[5],
  TestID -> "BallTopologyGraph-PathGraph-VertexList"
]

(* Transitive reduction preserves reachability, so the in-component of p
   recovers cl(p). *)
VerificationTest[
  And @@ (Sort @ VertexInComponent[BallTopologyGraph[PathGraph[Range[5]], 1], #] ===
          Sort @ BallClosure[PathGraph[Range[5]], 1, #] & /@ Range[5]),
  True,
  TestID -> "BallTopologyGraph-PathGraph-r1-reachability-matches-closure"
]

VerificationTest[
  And @@ (Sort @ VertexInComponent[BallTopologyGraph[StarGraph[5], 1], #] ===
          Sort @ BallClosure[StarGraph[5], 1, #] & /@ Range[5]),
  True,
  TestID -> "BallTopologyGraph-StarGraph5-r1-reachability-matches-closure"
]

VerificationTest[
  And @@ (Sort @ VertexInComponent[BallTopologyGraph[GridGraph[{3, 3}], 1], #] ===
          Sort @ BallClosure[GridGraph[{3, 3}], 1, #] & /@ Range[9]),
  True,
  TestID -> "BallTopologyGraph-GridGraph3x3-r1-reachability-matches-closure"
]

VerificationTest[
  EdgeQ[BallTopologyGraph[PathGraph[Range[5]], 1], DirectedEdge[2, 1]],
  True,
  TestID -> "BallTopologyGraph-PathGraph-r1-edge-2to1"
]

VerificationTest[
  EdgeQ[BallTopologyGraph[PathGraph[Range[5]], 1], DirectedEdge[1, 2]],
  False,
  TestID -> "BallTopologyGraph-PathGraph-r1-no-edge-1to2"
]

VerificationTest[
  Length @ ConnectedComponents[BallTopologyGraph[CompleteGraph[4], 1]],
  1,
  TestID -> "BallTopologyGraph-CompleteGraph4-r1-one-weak-component"
]

(* ===== BallTopologyGraph "Reduced" -> False (unreduced preorder digraph) ===== *)

VerificationTest[
  IsomorphicGraphQ[
    TransitiveReductionGraph @ BallTopologyGraph[PathGraph[Range[5]], 1, "Reduced" -> False],
    BallTopologyGraph[PathGraph[Range[5]], 1]
  ],
  True,
  TestID -> "BallTopologyGraph-Reduced-False-PathGraph-idempotence"
]

VerificationTest[
  Length @ EdgeList @ BallTopologyGraph[CompleteGraph[4], 1, "Reduced" -> False],
  16,
  TestID -> "BallTopologyGraph-Reduced-False-CompleteGraph4-edges-n2"
]

VerificationTest[
  And @@ (EdgeQ[BallTopologyGraph[PathGraph[Range[5]], 1, "Reduced" -> False], DirectedEdge[#, #]] & /@ Range[5]),
  True,
  TestID -> "BallTopologyGraph-Reduced-False-PathGraph-self-loops"
]

(* The unreduced edge set is exactly the preorder relation: edge q -> p iff q in cl(p). *)
VerificationTest[
  Sort[List @@@ EdgeList @ BallTopologyGraph[StarGraph[5], 1, "Reduced" -> False]],
  Sort @ Flatten[Table[{q, p}, {p, 5}, {q, BallClosure[StarGraph[5], 1, p]}], 1],
  TestID -> "BallTopologyGraph-Reduced-False-StarGraph-matches-BallClosure"
]

(* ===== BallContinuousMapQ ===== *)

(* Identity is continuous on (g, r) -> (g, r) for any g, r. *)
VerificationTest[
  And @@ (BallContinuousMapQ[#, 1, #, 1, AssociationThread[VertexList @ #, VertexList @ #]] & /@
    {PathGraph[Range[5]], CycleGraph[6], GridGraph[{3, 3}], StarGraph[5], CompleteGraph[4]}),
  True,
  TestID -> "BallContinuousMapQ-identity-continuous"
]

(* Constant maps are continuous: every closure contains the constant. *)
VerificationTest[
  BallContinuousMapQ[
    GridGraph[{3, 3}], 1, StarGraph[5], 1,
    AssociationThread[Range[9], ConstantArray[1, 9]]
  ],
  True,
  TestID -> "BallContinuousMapQ-constant-continuous"
]

(* Discrete source (r = 0): the only constraint is from self-loops, trivially satisfied. *)
VerificationTest[
  BallContinuousMapQ[
    PathGraph[Range[5]], 0, PathGraph[Range[5]], 1,
    <| 1 -> 5, 2 -> 1, 3 -> 4, 4 -> 2, 5 -> 3 |>
  ],
  True,
  TestID -> "BallContinuousMapQ-discrete-source-any-map"
]

(* Indiscrete target (s >= diameter): every closure is the whole vertex set. *)
VerificationTest[
  BallContinuousMapQ[
    PathGraph[Range[5]], 1, PathGraph[Range[5]], GraphDiameter @ PathGraph[Range[5]],
    <| 1 -> 5, 2 -> 1, 3 -> 4, 4 -> 2, 5 -> 3 |>
  ],
  True,
  TestID -> "BallContinuousMapQ-indiscrete-target-any-map"
]

(* Counterexample: in PathGraph[Range[5]] at r = 1, cl(1) = {1, 2}, so the source
   has the preorder edge 2 -> 1.  With f(1) = 5, f(2) = 3, continuity requires
   3 in cl(5) = {4, 5} -- fails. *)
VerificationTest[
  BallContinuousMapQ[
    PathGraph[Range[5]], 1, PathGraph[Range[5]], 1,
    <| 1 -> 5, 2 -> 3, 3 -> 3, 4 -> 3, 5 -> 1 |>
  ],
  False,
  TestID -> "BallContinuousMapQ-path-broken-monotone"
]

(* Map as Association vs list-of-rules vs callable: same answer. *)
VerificationTest[
  {
    BallContinuousMapQ[StarGraph[5], 1, StarGraph[5], 1, <| 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 |>],
    BallContinuousMapQ[StarGraph[5], 1, StarGraph[5], 1, {1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5}],
    BallContinuousMapQ[StarGraph[5], 1, StarGraph[5], 1, Identity]
  },
  {True, True, True},
  TestID -> "BallContinuousMapQ-map-forms-agree"
]

(* Cross-validation: edge-iteration form (used internally) agrees with the literal
   closure-inclusion definition f(cl v) subset cl(f v) for every v. *)
VerificationTest[
  With[{g = StarGraph[5], h = PathGraph[Range[5]], f = <| 1 -> 3, 2 -> 2, 3 -> 2, 4 -> 4, 5 -> 4 |>},
    BallContinuousMapQ[g, 1, h, 1, f] ===
      AllTrue[VertexList @ g,
        v |-> SubsetQ[BallClosure[h, 1, f[v]], f /@ BallClosure[g, 1, v]]
      ]
  ],
  True,
  TestID -> "BallContinuousMapQ-edge-iter-agrees-with-closure-inclusion"
]

(* ===== Dual BallTopologyGraph ===== *)

VerificationTest[
  Sort @ EdgeList @ BallTopologyGraph[ PathGraph @ Range[5], 1, "Dual" -> True ],
  Sort @ EdgeList @ ReverseGraph @ BallTopologyGraph[ PathGraph @ Range[5], 1 ],
  TestID -> "BallTopologyGraph-Dual-equals-reverse"
]

VerificationTest[
  EdgeList @ BallTopologyGraph[ PathGraph @ Range[5], 0, "Dual" -> True ],
  {},
  TestID -> "BallTopologyGraph-Dual-discrete-empty"
]

VerificationTest[
  EdgeQ[ BallTopologyGraph[ PathGraph @ Range[5], 1, "Dual" -> True ], DirectedEdge[1, 2] ],
  True,
  TestID -> "BallTopologyGraph-Dual-PathGraph-edge-1to2"
]

VerificationTest[
  EdgeQ[ BallTopologyGraph[ PathGraph @ Range[5], 1, "Dual" -> True ], DirectedEdge[2, 1] ],
  False,
  TestID -> "BallTopologyGraph-Dual-PathGraph-no-edge-2to1"
]

(* Reachability in dual preorder matches dual closure. *)
VerificationTest[
  And @@ (Sort @ VertexInComponent[ BallTopologyGraph[ StarGraph[5], 1, "Dual" -> True ], # ] ===
          Sort @ BallClosure[ StarGraph[5], 1, #, "Dual" -> True ] & /@ Range[5]),
  True,
  TestID -> "BallTopologyGraph-Dual-reachability-matches-DualClosure"
]

(* ===== Dual BallClosure ===== *)

(* Hub's dual closure = all vertices (every ball fits inside V). *)
VerificationTest[
  Sort @ BallClosure[ StarGraph[5], 1, 1, "Dual" -> True ],
  {1, 2, 3, 4, 5},
  TestID -> "BallClosure-Dual-StarGraph-hub-all-vertices"
]

(* Leaf's dual closure = just itself. *)
VerificationTest[
  BallClosure[ StarGraph[5], 1, 2, "Dual" -> True ],
  {2},
  TestID -> "BallClosure-Dual-StarGraph-leaf-singleton"
]

(* PathGraph: dual closure of interior vertex includes vertices whose B_1 fits inside its B_1. *)
VerificationTest[
  Sort @ BallClosure[ PathGraph @ Range[5], 1, 2, "Dual" -> True ],
  {1, 2},
  TestID -> "BallClosure-Dual-PathGraph-vertex2"
]

(* InfraPoint wrapper forwards "Dual" option. *)
VerificationTest[
  Sort @ BallClosure[ StarGraph[5], 1, InfraPoint[{{1}}], "Dual" -> True ],
  Sort @ BallClosure[ StarGraph[5], 1, 1, "Dual" -> True ],
  TestID -> "BallClosure-Dual-InfraPoint-forwarded"
]

(* ===== Dual BallContinuousMapQ: dual-to-dual == primal-to-primal ===== *)

VerificationTest[
  BallContinuousMapQ[ PathGraph @ Range[5], 1, PathGraph @ Range[5], 1,
    AssociationThread[ Range[5], Range[5] ], "Dual" -> True ] ===
  BallContinuousMapQ[ PathGraph @ Range[5], 1, PathGraph @ Range[5], 1,
    AssociationThread[ Range[5], Range[5] ] ],
  True,
  TestID -> "BallContinuousMapQ-Dual-identity-same-as-primal"
]

VerificationTest[
  BallContinuousMapQ[ PathGraph @ Range[5], 1, PathGraph @ Range[5], 1,
    <| 1 -> 5, 2 -> 3, 3 -> 3, 4 -> 3, 5 -> 1 |>, "Dual" -> True ] ===
  BallContinuousMapQ[ PathGraph @ Range[5], 1, PathGraph @ Range[5], 1,
    <| 1 -> 5, 2 -> 3, 3 -> 3, 4 -> 3, 5 -> 1 |> ],
  True,
  TestID -> "BallContinuousMapQ-Dual-discontinuous-same-as-primal"
]

(* ===== InfraTopologicalSpace wrapper ===== *)

VerificationTest[
  InfraTopologicalSpace[ PathGraph @ Range[5], 1 ][ "Graph" ],
  PathGraph @ Range[5],
  TestID -> "InfraTopologicalSpace-Graph-accessor"
]

VerificationTest[
  IsomorphicGraphQ[
    InfraTopologicalSpace[ PathGraph @ Range[5], 1 ][ "Topology" ],
    BallTopologyGraph[ PathGraph @ Range[5], 1, "Reduced" -> False ]
  ],
  True,
  TestID -> "InfraTopologicalSpace-Topology-accessor"
]

(* "Dual" option threads through to BallTopologyGraph. *)
VerificationTest[
  IsomorphicGraphQ[
    InfraTopologicalSpace[ StarGraph[5], 1, "Dual" -> True ][ "Topology" ],
    BallTopologyGraph[ StarGraph[5], 1, "Reduced" -> False, "Dual" -> True ]
  ],
  True,
  TestID -> "InfraTopologicalSpace-Dual-option-threads"
]

(* Wrapper-based BallContinuousMapQ agrees with original signature. *)
VerificationTest[
  With[ { g = PathGraph @ Range[5], h = StarGraph[5],
          f = <| 1 -> 1, 2 -> 2, 3 -> 1, 4 -> 3, 5 -> 1 |> },
    BallContinuousMapQ[ g, 1, h, 1, f ] ===
    BallContinuousMapQ[
      InfraTopologicalSpace[ g, 1 ],
      InfraTopologicalSpace[ h, 1 ],
      f ]
  ],
  True,
  TestID -> "BallContinuousMapQ-wrapper-agrees-with-original"
]

VerificationTest[
  BallContinuousMapQ[
    InfraTopologicalSpace[ CycleGraph[6], 1 ],
    InfraTopologicalSpace[ CycleGraph[6], 1 ],
    AssociationThread[ Range[6], Range[6] ]
  ],
  True,
  TestID -> "BallContinuousMapQ-wrapper-identity-continuous"
]

(* Wrapper dispatch handles {__Rule} maps and callable maps too. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ StarGraph[5], 1 ] },
    {
      BallContinuousMapQ[ ts, ts, <| 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 |> ],
      BallContinuousMapQ[ ts, ts, { 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 } ],
      BallContinuousMapQ[ ts, ts, Identity ]
    }
  ],
  { True, True, True },
  TestID -> "BallContinuousMapQ-wrapper-map-forms-agree"
]

(* The known discontinuous counterexample is also caught via the wrapper. *)
VerificationTest[
  BallContinuousMapQ[
    InfraTopologicalSpace[ PathGraph @ Range[5], 1 ],
    InfraTopologicalSpace[ PathGraph @ Range[5], 1 ],
    <| 1 -> 5, 2 -> 3, 3 -> 3, 4 -> 3, 5 -> 1 |>
  ],
  False,
  TestID -> "BallContinuousMapQ-wrapper-broken-monotone"
]

EndTestSection[]
