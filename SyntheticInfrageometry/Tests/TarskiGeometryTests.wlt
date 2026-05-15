BeginTestSection["TarskiGeometry"]

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

VerificationTest[
  Length @ TarskiBetweennessTensor[PathGraph[Range[5]]]["NonzeroPositions"] < 5^3,
  True,
  TestID -> "TarskiBetweennessTensor-PathGraph-sparse"
]

(* ===== TarskiEquidistanceClasses ===== *)

VerificationTest[
  Sort @ Flatten[TarskiEquidistanceClasses[PathGraph[Range[5]]], 1] ===
    Sort @ Subsets[VertexList[PathGraph[Range[5]]], {2}],
  True,
  TestID -> "TarskiEquidistanceClasses-PathGraph-partition-covers"
]

VerificationTest[
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

(* ===== ExtendInfraSegment (Tarski 5-vertex form, formerly FindTarskiSegmentExtension) ===== *)

VerificationTest[
  InfraPoint @ ExtendInfraSegment[PathGraph[Range[5]], 1, 2, 1, 2, All],
  InfraPoint[{3}],
  TestID -> "ExtendInfraSegment-Tarski-PathGraph-extends-by-one"
]

VerificationTest[
  InfraPoint @ ExtendInfraSegment[PathGraph[Range[5]], 1, 2, 1, 3, All],
  InfraPoint[{4}],
  TestID -> "ExtendInfraSegment-Tarski-PathGraph-extends-by-two"
]

VerificationTest[
  InfraPoint @ ExtendInfraSegment[PathGraph[Range[5]], 1, 2, 1, 5, All],
  InfraPoint[{}],
  TestID -> "ExtendInfraSegment-Tarski-PathGraph-no-room"
]

VerificationTest[
  ExtendInfraSegment[PathGraph[Range[5]], 1, 2, 1, 5],
  $Failed,
  TestID -> "ExtendInfraSegment-Tarski-PathGraph-strict-fails"
]

VerificationTest[
  InfraPoint @ ExtendInfraSegment[PathGraph[Range[5]], 1, 2, 1, 5, UpTo[1]],
  InfraPoint[{}],
  TestID -> "ExtendInfraSegment-Tarski-PathGraph-UpTo-empty-not-failed"
]

VerificationTest[
  Length @ ExtendInfraSegment[CycleGraph[6], 1, 2, 1, 2, All] >= 1,
  True,
  TestID -> "ExtendInfraSegment-Tarski-CycleGraph-has-extension"
]

(* ===== FindInfraMidpoint with Method -> "Tarski" (formerly FindTarskiMidpoint) ===== *)

VerificationTest[
  InfraPoint @ FindInfraMidpoint[PathGraph[Range[5]], 1, 5, All, Method -> "Tarski"],
  InfraPoint[{3}],
  TestID -> "FindInfraMidpoint-Tarski-PathGraph-even-distance-hit"
]

VerificationTest[
  InfraPoint @ FindInfraMidpoint[PathGraph[Range[5]], 1, 4, All, Method -> "Tarski"],
  InfraPoint[{}],
  TestID -> "FindInfraMidpoint-Tarski-PathGraph-odd-distance-miss"
]

VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], 1, 4, Method -> "Tarski"],
  $Failed,
  TestID -> "FindInfraMidpoint-Tarski-PathGraph-odd-distance-strict-fails"
]

VerificationTest[
  InfraPoint @ FindInfraMidpoint[PathGraph[Range[5]], 1, 4, UpTo[1], Method -> "Tarski"],
  InfraPoint[{}],
  TestID -> "FindInfraMidpoint-Tarski-PathGraph-odd-distance-UpTo-empty"
]

VerificationTest[
  Sort @ (#[[ 1, 1 ]] & /@ FindInfraMidpoint[CycleGraph[4], 1, 3, All, Method -> "Tarski"]),
  {2, 4},
  TestID -> "FindInfraMidpoint-Tarski-CycleGraph4-antipodes-two-midpoints"
]

VerificationTest[
  Length @ FindInfraMidpoint[CycleGraph[4], 1, 3, UpTo[1], Method -> "Tarski"],
  1,
  TestID -> "FindInfraMidpoint-Tarski-CycleGraph4-UpTo-caps"
]

(* ===== FindInfraReflection ===== *)

VerificationTest[
  InfraPoint @ FindInfraReflection[PathGraph[Range[5]], 1, 2, All],
  InfraPoint[{3}],
  TestID -> "FindInfraReflection-PathGraph-adjacent"
]

VerificationTest[
  InfraPoint @ FindInfraReflection[PathGraph[Range[5]], 1, 3, All],
  InfraPoint[{5}],
  TestID -> "FindInfraReflection-PathGraph-distance-two"
]

VerificationTest[
  InfraPoint @ FindInfraReflection[PathGraph[Range[5]], 1, 4, All],
  InfraPoint[{}],
  TestID -> "FindInfraReflection-PathGraph-no-room"
]

VerificationTest[
  FindInfraReflection[PathGraph[Range[5]], 1, 4],
  $Failed,
  TestID -> "FindInfraReflection-PathGraph-no-room-strict-fails"
]

VerificationTest[
  MemberQ[(#[[ 1, 1 ]] & /@ FindInfraReflection[CycleGraph[6], 1, 2, All]), 3],
  True,
  TestID -> "FindInfraReflection-CycleGraph6-includes-3"
]

VerificationTest[
  Length @ FindInfraReflection[HypercubeGraph[3], 1, 2, All] >= 2,
  True,
  TestID -> "FindInfraReflection-HypercubeGraph-multi-valued"
]

(* ===== FindTarskiCounterexample ===== *)

VerificationTest[
  FindTarskiCounterexample[PathGraph[Range[5]], TarskiCongruenceReflexivityQ, All],
  {},
  TestID -> "FindTarskiCounterexample-AlwaysTrue-empty"
]

VerificationTest[
  FindTarskiCounterexample[PathGraph[Range[5]], TarskiCongruenceReflexivityQ],
  $Failed,
  TestID -> "FindTarskiCounterexample-AlwaysTrue-strict-fails"
]

VerificationTest[
  Length @ FindTarskiCounterexample[PathGraph[Range[5]], TarskiSegmentConstructionQ, All] >= 1,
  True,
  TestID -> "FindTarskiCounterexample-SegmentConstruction-PathGraph-has-witness"
]

VerificationTest[
  Length @ FindTarskiCounterexample[PathGraph[Range[5]], TarskiSegmentConstructionQ, UpTo[3]] <= 3,
  True,
  TestID -> "FindTarskiCounterexample-SegmentConstruction-PathGraph-UpTo-respects-cap"
]

VerificationTest[
  Quiet @ FindTarskiCounterexample[PathGraph[Range[5]], TarskiContinuityQ, All],
  $Failed,
  TestID -> "FindTarskiCounterexample-Continuity-no-finite-witness"
]

VerificationTest[
  Length @ FindTarskiCounterexample[PetersenGraph[], TarskiInnerPaschQ, UpTo[1]] >= 1,
  True,
  TestID -> "FindTarskiCounterexample-InnerPasch-PetersenGraph-fails"
]


(* FindInfraReflection is local: depends only on B(a, 2 d(a, x)). *)

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], x = 23, a = 25 },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraReflection[ g, x, a, All ]) ===
      Sort @ (#[[ 1, 1 ]] & /@
        FindInfraReflection[ NeighborhoodGraph[ g, a, 2 GraphDistance[ g, a, x ] ], x, a, All ])
  ],
  True,
  TestID -> "FindInfraReflection-locality"
]

EndTestSection[]
