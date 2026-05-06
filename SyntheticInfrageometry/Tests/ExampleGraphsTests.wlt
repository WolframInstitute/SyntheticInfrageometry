BeginTestSection["ExampleGraphs"]

(* ===== InfraExampleGraph keys ===== *)

VerificationTest[
  Length @ InfraExampleGraph[ ] >= 10,
  True,
  TestID -> "InfraExampleGraph-keys-non-empty"
]

VerificationTest[
  MemberQ[ InfraExampleGraph[ ], "Grid" ] &&
    MemberQ[ InfraExampleGraph[ ], "RectangleMesh" ] &&
    MemberQ[ InfraExampleGraph[ ], "DiskMesh" ] &&
    MemberQ[ InfraExampleGraph[ ], "SphereMesh" ],
  True,
  TestID -> "InfraExampleGraph-curvature-spectrum-keys"
]

(* ===== Lattice keys ===== *)

VerificationTest[
  Head @ InfraExampleGraph[ "Grid" ],
  Graph,
  TestID -> "InfraExampleGraph-Grid-default"
]

VerificationTest[
  VertexCount @ InfraExampleGraph[ "Grid", { 3, 3 } ],
  9,
  TestID -> "InfraExampleGraph-Grid-explicit"
]

VerificationTest[
  Head @ InfraExampleGraph[ "TriangularLattice" ],
  Graph,
  TestID -> "InfraExampleGraph-TriangularLattice-default"
]

VerificationTest[
  Head @ InfraExampleGraph[ "HexagonalLattice" ],
  Graph,
  TestID -> "InfraExampleGraph-HexagonalLattice-default"
]

(* ===== Mesh keys ===== *)

VerificationTest[
  Head @ InfraExampleGraph[ "RectangleMesh" ],
  Graph,
  TestID -> "InfraExampleGraph-RectangleMesh-default"
]

VerificationTest[
  With[ { coarse = InfraExampleGraph[ "RectangleMesh", { 3, 2 }, MaxCellMeasure -> 0.5 ],
          fine   = InfraExampleGraph[ "RectangleMesh", { 3, 2 }, MaxCellMeasure -> 0.05 ] },
    VertexCount[ coarse ] < VertexCount[ fine ]
  ],
  True,
  TestID -> "InfraExampleGraph-RectangleMesh-MaxCellMeasure-controls-resolution"
]

VerificationTest[
  Head @ InfraExampleGraph[ "DiskMesh" ],
  Graph,
  TestID -> "InfraExampleGraph-DiskMesh-default"
]

VerificationTest[
  Head @ InfraExampleGraph[ "SphereMesh", 1, MaxCellMeasure -> 0.5 ],
  Graph,
  TestID -> "InfraExampleGraph-SphereMesh-coarse"
]

(* ===== Tree, Cayley ===== *)

VerificationTest[
  ConnectedGraphQ @ InfraExampleGraph[ "RegularTree", { 3, 3 } ],
  True,
  TestID -> "InfraExampleGraph-RegularTree-connected"
]

VerificationTest[
  AcyclicGraphQ @ UndirectedGraph @ InfraExampleGraph[ "RegularTree", { 3, 3 } ],
  True,
  TestID -> "InfraExampleGraph-RegularTree-acyclic"
]

VerificationTest[
  Head @ InfraExampleGraph[ "Cayley" ],
  Graph,
  TestID -> "InfraExampleGraph-Cayley-default"
]

(* ===== Named gems ===== *)

VerificationTest[
  VertexCount @ InfraExampleGraph[ "Petersen" ],
  10,
  TestID -> "InfraExampleGraph-Petersen-vertex-count"
]

VerificationTest[
  VertexCount @ InfraExampleGraph[ "Heawood" ],
  14,
  TestID -> "InfraExampleGraph-Heawood-vertex-count"
]

VerificationTest[
  VertexCount @ InfraExampleGraph[ "MobiusKantor" ],
  16,
  TestID -> "InfraExampleGraph-MobiusKantor-vertex-count"
]

VerificationTest[
  Head @ InfraExampleGraph[ "Tutte" ],
  Graph,
  TestID -> "InfraExampleGraph-Tutte-default"
]

(* ===== Composes with paclet primitives ===== *)

VerificationTest[
  With[ { g = InfraExampleGraph[ "Grid", { 3, 3 } ] },
    Head @ FindSegment[ g, 1, 9, All ]
  ],
  InfraSegment,
  TestID -> "InfraExampleGraph-Grid-feeds-FindSegment"
]

VerificationTest[
  With[ { g = InfraExampleGraph[ "RegularTree", { 3, 2 } ] },
    AssociationQ @ FormanRicci[ g ]
  ],
  True,
  TestID -> "InfraExampleGraph-RegularTree-feeds-FormanRicci"
]

EndTestSection[]
