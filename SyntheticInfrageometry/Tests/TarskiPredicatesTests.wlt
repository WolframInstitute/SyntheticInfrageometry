BeginTestSection["TarskiPredicates"]

(* ===== BetweennessQ ===== *)

VerificationTest[
  BetweennessQ[PathGraph[Range[5]], 1, 3, 5],
  True,
  TestID -> "BetweennessQ-PathGraph-true"
]

VerificationTest[
  BetweennessQ[PathGraph[Range[5]], 1, 4, 3],
  False,
  TestID -> "BetweennessQ-PathGraph-false"
]

VerificationTest[
  BetweennessQ[CycleGraph[4], 1, 2, 3],
  True,
  TestID -> "BetweennessQ-CycleGraph4-via-2"
]

VerificationTest[
  BetweennessQ[CycleGraph[4], 1, 4, 3],
  True,
  TestID -> "BetweennessQ-CycleGraph4-via-4"
]

VerificationTest[
  BetweennessQ[Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}], 1, 2, 3],
  False,
  TestID -> "BetweennessQ-disconnected-false"
]

(* ===== EquidistanceQ ===== *)

VerificationTest[
  EquidistanceQ[PathGraph[Range[5]], 1, 2, 3, 4],
  True,
  TestID -> "EquidistanceQ-PathGraph-equal-edges"
]

VerificationTest[
  EquidistanceQ[PathGraph[Range[5]], 1, 3, 2, 5],
  False,
  TestID -> "EquidistanceQ-PathGraph-unequal"
]

VerificationTest[
  EquidistanceQ[CycleGraph[6], 1, 4, 2, 5],
  True,
  TestID -> "EquidistanceQ-CycleGraph6-antipodes-equal"
]

(* ===== TarskiStructure ===== *)

VerificationTest[
  Sort @ Keys @ TarskiStructure[PathGraph[Range[5]]],
  Sort @ {"Vertices", "VertexIndex", "Distances", "Betweenness", "Equidistance", "Diameter"},
  TestID -> "TarskiStructure-keys"
]

VerificationTest[
  TarskiStructure[PathGraph[Range[5]]]["Diameter"],
  4,
  TestID -> "TarskiStructure-PathGraph-diameter"
]

VerificationTest[
  TarskiStructure[CompleteGraph[4]]["Diameter"],
  1,
  TestID -> "TarskiStructure-CompleteGraph-diameter"
]

VerificationTest[
  Length @ TarskiStructure[PathGraph[Range[5]]]["Vertices"],
  5,
  TestID -> "TarskiStructure-vertex-count"
]

(* ===== TarskiBetweennessTensor ===== *)

VerificationTest[
  Dimensions @ TarskiBetweennessTensor[PathGraph[Range[5]]],
  {5, 5, 5},
  TestID -> "TarskiBetweennessTensor-PathGraph-shape"
]

VerificationTest[
  TarskiBetweennessTensor[PathGraph[Range[5]]][[1, 3, 5]],
  1,
  TestID -> "TarskiBetweennessTensor-PathGraph-true-entry"
]

VerificationTest[
  TarskiBetweennessTensor[PathGraph[Range[5]]][[1, 4, 3]],
  0,
  TestID -> "TarskiBetweennessTensor-PathGraph-false-entry"
]

(* The 3-tensor is sparse on path-like graphs *)
VerificationTest[
  Length @ TarskiBetweennessTensor[PathGraph[Range[5]]]["NonzeroPositions"] < 5^3,
  True,
  TestID -> "TarskiBetweennessTensor-PathGraph-sparse"
]

(* ===== TarskiEquidistanceClasses ===== *)

VerificationTest[
  (* Partition covers every unordered pair exactly once *)
  Sort @ Flatten[TarskiEquidistanceClasses[PathGraph[Range[5]]], 1] ===
    Sort @ Subsets[VertexList[PathGraph[Range[5]]], {2}],
  True,
  TestID -> "TarskiEquidistanceClasses-PathGraph-partition-covers"
]

VerificationTest[
  (* Each class has a single distance value *)
  AllTrue[
    TarskiEquidistanceClasses[PathGraph[Range[5]]],
    cls |-> Length @ DeleteDuplicates[
      GraphDistance[PathGraph[Range[5]], #[[1]], #[[2]]] & /@ cls
    ] === 1
  ],
  True,
  TestID -> "TarskiEquidistanceClasses-PathGraph-classes-single-distance"
]

(* ===== Always-True axioms ===== *)

VerificationTest[
  TarskiCongruenceReflexivityQ[PathGraph[Range[5]]],
  True,
  TestID -> "TarskiCongruenceReflexivityQ-PathGraph"
]

VerificationTest[
  TarskiCongruenceReflexivityQ[CycleGraph[6]],
  True,
  TestID -> "TarskiCongruenceReflexivityQ-CycleGraph"
]

VerificationTest[
  TarskiCongruenceTransitivityQ[GridGraph[{3, 3}]],
  True,
  TestID -> "TarskiCongruenceTransitivityQ-GridGraph"
]

VerificationTest[
  TarskiCongruenceIdentityQ[PetersenGraph[]],
  True,
  TestID -> "TarskiCongruenceIdentityQ-PetersenGraph"
]

VerificationTest[
  TarskiBetweennessIdentityQ[PathGraph[Range[5]]],
  True,
  TestID -> "TarskiBetweennessIdentityQ-PathGraph"
]

VerificationTest[
  TarskiBetweennessIdentityQ[CycleGraph[6]],
  True,
  TestID -> "TarskiBetweennessIdentityQ-CycleGraph"
]

(* ===== TarskiSegmentConstructionQ (always False) ===== *)

VerificationTest[
  TarskiSegmentConstructionQ[PathGraph[Range[5]]],
  False,
  TestID -> "TarskiSegmentConstructionQ-PathGraph-False"
]

VerificationTest[
  TarskiSegmentConstructionQ[CycleGraph[6]],
  False,
  TestID -> "TarskiSegmentConstructionQ-CycleGraph-False"
]

VerificationTest[
  TarskiSegmentConstructionQ[CompleteGraph[4]],
  False,
  TestID -> "TarskiSegmentConstructionQ-CompleteGraph-False"
]

(* ===== TarskiInnerPaschQ ===== *)

VerificationTest[
  TarskiInnerPaschQ[PathGraph[Range[5]]],
  True,
  TestID -> "TarskiInnerPaschQ-PathGraph-True"
]

VerificationTest[
  TarskiInnerPaschQ[CompleteGraph[4]],
  True,
  TestID -> "TarskiInnerPaschQ-CompleteGraph-True"
]

VerificationTest[
  TarskiInnerPaschQ[PetersenGraph[]],
  False,
  TestID -> "TarskiInnerPaschQ-PetersenGraph-False"
]

(* ===== TarskiLowerDimensionQ ===== *)

VerificationTest[
  TarskiLowerDimensionQ[PathGraph[Range[5]]],
  False,
  TestID -> "TarskiLowerDimensionQ-PathGraph-False"
]

VerificationTest[
  TarskiLowerDimensionQ[CycleGraph[6]],
  True,
  TestID -> "TarskiLowerDimensionQ-CycleGraph-True"
]

VerificationTest[
  TarskiLowerDimensionQ[GridGraph[{3, 3}]],
  True,
  TestID -> "TarskiLowerDimensionQ-GridGraph-True"
]

(* ===== TarskiContinuityQ (always False) ===== *)

VerificationTest[
  TarskiContinuityQ[PathGraph[Range[5]]],
  False,
  TestID -> "TarskiContinuityQ-PathGraph-False"
]

VerificationTest[
  TarskiContinuityQ[GridGraph[{3, 3}]],
  False,
  TestID -> "TarskiContinuityQ-GridGraph-False"
]

(* ===== TarskiEuclidAxiomQ (Indeterminate stub) ===== *)

VerificationTest[
  Quiet @ TarskiEuclidAxiomQ[PathGraph[Range[5]]],
  Indeterminate,
  TestID -> "TarskiEuclidAxiomQ-Indeterminate-stub"
]

(* ===== TarskiAxiomQ dashboard ===== *)

VerificationTest[
  Sort @ Keys @ Quiet @ TarskiAxiomQ[PathGraph[Range[5]]],
  Sort @ {"EquidistanceReflexivity", "EquidistanceTransitivity", "EquidistanceIdentity",
          "SegmentConstruction", "FiveSegments", "BetweennessIdentity",
          "InnerPasch", "LowerDimension", "UpperDimension", "Euclid", "Continuity"},
  TestID -> "TarskiAxiomQ-keys"
]

VerificationTest[
  Quiet[TarskiAxiomQ[CompleteGraph[4]]]["BetweennessIdentity"],
  True,
  TestID -> "TarskiAxiomQ-CompleteGraph-BetweennessIdentity"
]

VerificationTest[
  Quiet[TarskiAxiomQ[PathGraph[Range[5]]]]["Continuity"],
  False,
  TestID -> "TarskiAxiomQ-PathGraph-Continuity-False"
]

EndTestSection[]
