BeginTestSection["InfraTopology"]

(* ===== InfraBallTopology ===== *)

VerificationTest[
  Sort @ VertexList @ InfraBallTopology[PathGraph[Range[5]], 1],
  Range[5],
  TestID -> "InfraBallTopology-PathGraph-VertexList"
]

VerificationTest[
  EdgeQ[InfraBallTopology[PathGraph[Range[5]], 1], DirectedEdge[2, 1]],
  True,
  TestID -> "InfraBallTopology-PathGraph-r1-edge-2to1"
]

VerificationTest[
  EdgeQ[InfraBallTopology[PathGraph[Range[5]], 1], DirectedEdge[1, 2]],
  False,
  TestID -> "InfraBallTopology-PathGraph-r1-no-edge-1to2"
]

VerificationTest[
  Length @ ConnectedComponents[InfraBallTopology[CompleteGraph[4], 1]],
  1,
  TestID -> "InfraBallTopology-CompleteGraph4-r1-one-weak-component"
]

(* ===== InfraBallTopology Dual ===== *)

VerificationTest[
  Sort @ EdgeList @ InfraBallTopology[ PathGraph @ Range[5], 1, "Dual" -> True ],
  Sort @ EdgeList @ ReverseGraph @ InfraBallTopology[ PathGraph @ Range[5], 1 ],
  TestID -> "InfraBallTopology-Dual-equals-reverse"
]

VerificationTest[
  EdgeList @ InfraBallTopology[ PathGraph @ Range[5], 0, "Dual" -> True ],
  {},
  TestID -> "InfraBallTopology-Dual-discrete-empty"
]

VerificationTest[
  EdgeQ[ InfraBallTopology[ PathGraph @ Range[5], 1, "Dual" -> True ], DirectedEdge[1, 2] ],
  True,
  TestID -> "InfraBallTopology-Dual-PathGraph-edge-1to2"
]

VerificationTest[
  EdgeQ[ InfraBallTopology[ PathGraph @ Range[5], 1, "Dual" -> True ], DirectedEdge[2, 1] ],
  False,
  TestID -> "InfraBallTopology-Dual-PathGraph-no-edge-2to1"
]

(* ===== InfraTopologicalSpace wrapper ===== *)

VerificationTest[
  InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ][ "Graph" ],
  PathGraph @ Range[5],
  TestID -> "InfraTopologicalSpace-Graph-accessor"
]

VerificationTest[
  IsomorphicGraphQ[
    InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ][ "Topology" ],
    InfraBallTopology[ PathGraph @ Range[5], 1 ]
  ],
  True,
  TestID -> "InfraTopologicalSpace-Topology-accessor"
]

(* Auto-reduce: passing the transitive closure is reduced back to Hasse. *)
VerificationTest[
  With[ { g = PathGraph @ Range[5] },
    IsomorphicGraphQ[
      InfraTopologicalSpace[ g,
        TransitiveClosureGraph @ InfraBallTopology[ g, 1 ] ][ "Topology" ],
      InfraBallTopology[ g, 1 ]
    ]
  ],
  True,
  TestID -> "InfraTopologicalSpace-auto-reduces-to-Hasse"
]

(* ===== InfraSet ===== *)

VerificationTest[
  InfraSet[ {1, 3, 5} ][ "Vertices" ],
  {1, 3, 5},
  TestID -> "InfraSet-Vertices-accessor"
]

VerificationTest[
  InfraSet[ {1, 3, 5} ][ "Length" ],
  3,
  TestID -> "InfraSet-Length-accessor"
]

VerificationTest[
  Head @ InfraSet[ {1, 2} ],
  InfraSet,
  TestID -> "InfraSet-Head"
]

(* ===== InfraSetClosure ===== *)

(* PathGraph r=1: cl(1)={1,2}, cl(2)={2}, cl(3)={3}, cl(4)={4}, cl(5)={4,5} *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ] },
    InfraSetClosure[ ts, InfraSet[{#}] ][ "Vertices" ] & /@ Range[5]
  ],
  {{1, 2}, {2}, {3}, {4}, {4, 5}},
  TestID -> "InfraSetClosure-PathGraph-r1-singletons"
]

VerificationTest[
  Head @ InfraSetClosure[
    InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ],
    InfraSet[ {1} ]
  ],
  InfraSet,
  TestID -> "InfraSetClosure-returns-InfraSet"
]

(* Hub of StarGraph[5]: cl({hub}) = {hub}. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ StarGraph[5], "Topology" -> {"Ball", 1} ] },
    InfraSetClosure[ ts, InfraSet[ {1} ] ][ "Vertices" ]
  ],
  {1},
  TestID -> "InfraSetClosure-StarGraph-hub"
]

(* Leaf: cl({leaf}) = {hub, leaf}. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ StarGraph[5], "Topology" -> {"Ball", 1} ] },
    Sort @ InfraSetClosure[ ts, InfraSet[ {2} ] ][ "Vertices" ]
  ],
  {1, 2},
  TestID -> "InfraSetClosure-StarGraph-leaf"
]

(* Set {1,5}: cl = cl(1) union cl(5) = {1,2} union {4,5} = {1,2,4,5}. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ] },
    InfraSetClosure[ ts, InfraSet[ {1, 5} ] ][ "Vertices" ]
  ],
  {1, 2, 4, 5},
  TestID -> "InfraSetClosure-PathGraph-two-endpoints"
]

(* Dual: cl of hub = all vertices; leaf stays singleton. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ StarGraph[5], InfraBallTopology[ StarGraph[5], 1, "Dual" -> True ] ] },
    Sort @ InfraSetClosure[ ts, InfraSet[ {1} ] ][ "Vertices" ]
  ],
  {1, 2, 3, 4, 5},
  TestID -> "InfraSetClosure-Dual-StarGraph-hub"
]

VerificationTest[
  With[ { ts = InfraTopologicalSpace[ StarGraph[5], InfraBallTopology[ StarGraph[5], 1, "Dual" -> True ] ] },
    InfraSetClosure[ ts, InfraSet[ {2} ] ][ "Vertices" ]
  ],
  {2},
  TestID -> "InfraSetClosure-Dual-StarGraph-leaf"
]

(* ===== InfraSetInterior ===== *)

(* PathGraph[7] r=1: int({2,3,4,5,6}) = {3,4,5}; endpoints 2,6 are boundary. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ] },
    InfraSetInterior[ ts, InfraSet[ {2, 3, 4, 5, 6} ] ][ "Vertices" ]
  ],
  {3, 4, 5},
  TestID -> "InfraSetInterior-PathGraph-r1-middle-strip"
]

VerificationTest[
  Head @ InfraSetInterior[
    InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ],
    InfraSet[ {1, 2, 3} ]
  ],
  InfraSet,
  TestID -> "InfraSetInterior-returns-InfraSet"
]

(* An open set: int = itself. {1,2,3} is open in PathGraph[7] r=1 since
   V\{1,2,3}={4,5,6,7} is closed (no exterior closure leaks in). *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ] },
    InfraSetInterior[ ts, InfraSet[ {1, 2, 3} ] ][ "Vertices" ]
  ],
  {1, 2, 3},
  TestID -> "InfraSetInterior-open-set-is-its-own-interior"
]

(* int(empty) = empty. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ] },
    InfraSetInterior[ ts, InfraSet[ {} ] ][ "Vertices" ]
  ],
  {},
  TestID -> "InfraSetInterior-empty-set"
]

(* int(V) = V. *)
VerificationTest[
  With[ { g = PathGraph @ Range[5],
          ts = InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ] },
    InfraSetInterior[ ts, InfraSet[ VertexList @ g ] ][ "Vertices" ]
  ],
  Range[5],
  TestID -> "InfraSetInterior-whole-graph"
]

(* ===== InfraSetBoundary ===== *)

(* PathGraph[7] r=1: bd({2,3,4,5,6}) = {2,6}. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ] },
    InfraSetBoundary[ ts, InfraSet[ {2, 3, 4, 5, 6} ] ][ "Vertices" ]
  ],
  {2, 6},
  TestID -> "InfraSetBoundary-PathGraph-r1-middle-strip"
]

VerificationTest[
  Head @ InfraSetBoundary[
    InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ],
    InfraSet[ {1, 2} ]
  ],
  InfraSet,
  TestID -> "InfraSetBoundary-returns-InfraSet"
]

(* Boundary of an open set is empty. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ] },
    InfraSetBoundary[ ts, InfraSet[ {1, 2, 3} ] ][ "Vertices" ]
  ],
  {},
  TestID -> "InfraSetBoundary-open-set-empty-boundary"
]

(* bd(S) subset cl(S) for any S. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ] },
    With[ { s = InfraSet[ {2, 3, 5} ] },
      SubsetQ[
        InfraSetClosure[ ts, s ][ "Vertices" ],
        InfraSetBoundary[ ts, s ][ "Vertices" ]
      ]
    ]
  ],
  True,
  TestID -> "InfraSetBoundary-subset-of-closure"
]

(* ===== Infra* wrapper dispatch ===== *)

(* InfraBall accepted directly: interior of B_2(center) on PathGraph[7]. *)
VerificationTest[
  With[ { g = PathGraph @ Range[7],
          ts = InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ] },
    InfraSetInterior[ ts, FindInfraBall[ g, 4, 2 ] ][ "Vertices" ]
  ],
  InfraSetInterior[
    InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ],
    InfraSet[ {2, 3, 4, 5, 6} ]
  ][ "Vertices" ],
  TestID -> "InfraSetInterior-accepts-InfraBall"
]

VerificationTest[
  Head @ InfraSetBoundary[
    InfraTopologicalSpace[ PathGraph @ Range[7], "Topology" -> {"Ball", 1} ],
    FindInfraBall[ PathGraph @ Range[7], 4, 2 ]
  ],
  InfraSet,
  TestID -> "InfraSetBoundary-accepts-InfraBall-returns-InfraSet"
]

(* ===== ContinuousMapQ ===== *)

(* Identity is continuous. *)
VerificationTest[
  And @@ (ContinuousMapQ[
      AssociationThread[ VertexList @ #, VertexList @ # ],
      InfraTopologicalSpace[ #, "Topology" -> {"Ball", 1} ],
      InfraTopologicalSpace[ #, "Topology" -> {"Ball", 1} ]
    ] & /@ {PathGraph[Range[5]], CycleGraph[6], GridGraph[{3, 3}], StarGraph[5], CompleteGraph[4]}),
  True,
  TestID -> "ContinuousMapQ-identity-continuous"
]

(* Constant maps are continuous. *)
VerificationTest[
  ContinuousMapQ[
    AssociationThread[ Range[9], ConstantArray[1, 9] ],
    InfraTopologicalSpace[ GridGraph[{3, 3}], "Topology" -> {"Ball", 1} ],
    InfraTopologicalSpace[ StarGraph[5], "Topology" -> {"Ball", 1} ]
  ],
  True,
  TestID -> "ContinuousMapQ-constant-continuous"
]

(* Discrete source (r = 0): Hasse is empty, every map is trivially continuous. *)
VerificationTest[
  ContinuousMapQ[
    <| 1 -> 5, 2 -> 1, 3 -> 4, 4 -> 2, 5 -> 3 |>,
    InfraTopologicalSpace[ PathGraph[Range[5]], "Topology" -> {"Ball", 0} ],
    InfraTopologicalSpace[ PathGraph[Range[5]], "Topology" -> {"Ball", 1} ]
  ],
  True,
  TestID -> "ContinuousMapQ-discrete-source-any-map"
]

(* Indiscrete target (s >= diameter): every map is continuous. *)
VerificationTest[
  ContinuousMapQ[
    <| 1 -> 5, 2 -> 1, 3 -> 4, 4 -> 2, 5 -> 3 |>,
    InfraTopologicalSpace[ PathGraph[Range[5]], "Topology" -> {"Ball", 1} ],
    InfraTopologicalSpace[ PathGraph[Range[5]], "Topology" -> {"Ball", GraphDiameter @ PathGraph[Range[5]]} ]
  ],
  True,
  TestID -> "ContinuousMapQ-indiscrete-target-any-map"
]

(* Counterexample: Hasse edge 2->1 requires f(2) reachable from f(1) in target;
   f(1)=5, f(2)=3 fails since 3 is not reachable from 5 in PathGraph r=1 Hasse. *)
VerificationTest[
  ContinuousMapQ[
    <| 1 -> 5, 2 -> 3, 3 -> 3, 4 -> 3, 5 -> 1 |>,
    InfraTopologicalSpace[ PathGraph[Range[5]], "Topology" -> {"Ball", 1} ],
    InfraTopologicalSpace[ PathGraph[Range[5]], "Topology" -> {"Ball", 1} ]
  ],
  False,
  TestID -> "ContinuousMapQ-path-broken-monotone"
]

(* Association, list-of-rules, and callable all agree. *)
VerificationTest[
  With[ { ts = InfraTopologicalSpace[ StarGraph[5], "Topology" -> {"Ball", 1} ] },
    {
      ContinuousMapQ[ <| 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 |>, ts, ts ],
      ContinuousMapQ[ { 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 }, ts, ts ],
      ContinuousMapQ[ Identity, ts, ts ]
    }
  ],
  {True, True, True},
  TestID -> "ContinuousMapQ-map-forms-agree"
]

(* Cross-validation: ContinuousMapQ agrees with the closure-inclusion definition. *)
VerificationTest[
  With[ { g = StarGraph[5], h = PathGraph[Range[5]],
          f = <| 1 -> 3, 2 -> 2, 3 -> 2, 4 -> 4, 5 -> 4 |>,
          tsg = InfraTopologicalSpace[ StarGraph[5], "Topology" -> {"Ball", 1} ],
          tsh = InfraTopologicalSpace[ PathGraph @ Range[5], "Topology" -> {"Ball", 1} ] },
    ContinuousMapQ[ f, tsg, tsh ] ===
    AllTrue[ VertexList @ g,
      v |-> SubsetQ[
        InfraSetClosure[ tsh, InfraSet[{ f[v] }] ][ "Vertices" ],
        f /@ InfraSetClosure[ tsg, InfraSet[{v}] ][ "Vertices" ]
      ]
    ]
  ],
  True,
  TestID -> "ContinuousMapQ-agrees-with-closure-inclusion"
]

EndTestSection[]
