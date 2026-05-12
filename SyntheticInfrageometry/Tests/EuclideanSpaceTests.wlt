BeginTestSection["InfraEuclideanSpace"]

(* ===== InfraScalarProduct (Schoenberg) ===== *)

VerificationTest[
  InfraScalarProduct[PathGraph[Range[5]], 1, 3, 4],
  6,
  TestID -> "InfraScalarProduct-path-Schoenberg-formula"
]

VerificationTest[
  Table[
    InfraScalarProduct[PathGraph[Range[5]], 1, k, l] === (k - 1)(l - 1),
    {k, 1, 5}, {l, 1, 5}
  ] // Flatten // Apply[And],
  True,
  TestID -> "InfraScalarProduct-path-Schoenberg-identity"
]

VerificationTest[
  InfraScalarProduct[GridGraph[{4, 4}], 1, 2, 5],
  -1,
  TestID -> "InfraScalarProduct-grid-Schoenberg-not-orthogonal"
]

VerificationTest[
  InfraScalarProduct[PathGraph[Range[5]], 1, 3, 3],
  GraphDistance[PathGraph[Range[5]], 1, 3]^2,
  TestID -> "InfraScalarProduct-norm-equals-distance-squared"
]

(* ===== InfraScalarProduct (Parallelogram) ===== *)

VerificationTest[
  InfraScalarProduct[PathGraph[Range[10]], 5, 7, 6, Method -> "Parallelogram"],
  2,
  TestID -> "InfraScalarProduct-path-Parallelogram-matches-Schoenberg"
]

VerificationTest[
  Quiet @ InfraScalarProduct[CycleGraph[6], 1, 2, 3, Method -> "Parallelogram"],
  $Failed,
  TestID -> "InfraScalarProduct-cycle-Parallelogram-no-negation"
]

VerificationTest[
  InfraScalarProduct[PathGraph[Range[5]], 1, 2, 5, Method -> "Parallelogram"],
  $Failed,
  TestID -> "InfraScalarProduct-Parallelogram-no-realisation"
]

(* ===== FindInfraLinearCombination scaling: "Metric" ===== *)

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[10]], 1, {{2, 3}}],
  InfraPoint[{5}],
  TestID -> "FindInfraLinearCombination-scale-metric-integer"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[10]], 1, {{1.7, 3}}, All, "ScaleMethod" -> "Metric"],
  InfraPoint[{}],
  TestID -> "FindInfraLinearCombination-scale-metric-real-empty"
]

(* ===== FindInfraLinearCombination scaling: "Line" ===== *)

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[10]], 1, {{1.7, 3}}, "ScaleMethod" -> "Line"],
  InfraPoint[{4}],
  TestID -> "FindInfraLinearCombination-scale-line-real"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[10]], 1, {{2, 3}}, "ScaleMethod" -> "Line"],
  InfraPoint[{5}],
  TestID -> "FindInfraLinearCombination-scale-line-integer"
]

(* ===== FindInfraLinearCombination scaling: "Midpoint" ===== *)

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[9]], 1, {{1/2, 5}}, "ScaleMethod" -> "Midpoint"],
  InfraPoint[{3}],
  TestID -> "FindInfraLinearCombination-scale-midpoint-half"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[9]], 1, {{1/4, 5}}, "ScaleMethod" -> "Midpoint"],
  InfraPoint[{2}],
  TestID -> "FindInfraLinearCombination-scale-midpoint-quarter"
]

(* ===== FindInfraLinearCombination scaling: Automatic dispatch ===== *)

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[9]], 1, {{1/2, 5}}],
  InfraPoint[{3}],
  TestID -> "FindInfraLinearCombination-scale-auto-dyadic"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[10]], 1, {{2, 3}}],
  InfraPoint[{5}],
  TestID -> "FindInfraLinearCombination-scale-auto-integer"
]

(* ===== FindInfraLinearCombination sum: "Metric" ===== *)

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[GridGraph[{4, 4}], 1, {{1, 2}, {1, 5}}],
  InfraPoint[{6}],
  TestID -> "FindInfraLinearCombination-sum-metric-grid-parallelogram"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[CycleGraph[4], 1, {{1, 2}, {1, 4}}],
  InfraPoint[{3}],
  TestID -> "FindInfraLinearCombination-sum-metric-C4-antipode"
]

VerificationTest[
  Sort @ (#[[ 1, 1 ]] & /@ FindInfraLinearCombination[
    Graph[{1 <-> 2, 1 <-> 3, 2 <-> 4, 2 <-> 5, 3 <-> 6, 3 <-> 7}],
    1, {{1, 2}, {1, 3}}, All
  ]),
  {},
  TestID -> "FindInfraLinearCombination-sum-tree-no-parallelogram"
]

(* ===== FindInfraLinearCombination sum: "Parallel" ===== *)

VerificationTest[
  MemberQ[
    (#[[ 1, 1 ]] & /@ FindInfraLinearCombination[GridGraph[{4, 4}], 1, {{1, 2}, {1, 5}}, All, "SumMethod" -> "Parallel"]),
    6
  ],
  True,
  TestID -> "FindInfraLinearCombination-sum-parallel-grid-includes-corner"
]

(* ===== FindInfraLinearCombination composition / edge cases ===== *)

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[10]], 5, {{-1, 8}}],
  InfraPoint[{2}],
  TestID -> "FindInfraLinearCombination-reflection-by-minus-one"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[5]], 3, {}],
  InfraPoint[{3}],
  TestID -> "FindInfraLinearCombination-empty-terms"
]

VerificationTest[
  InfraPoint @ FindInfraLinearCombination[PathGraph[Range[5]], 3, {{0, 5}}],
  InfraPoint[{3}],
  TestID -> "FindInfraLinearCombination-zero-coefficient"
]

(* ===== InfraAngle (moved from EuclideanConstructions) ===== *)

VerificationTest[
  InfraAngle[CycleGraph[6], {1, 2, 3}],
  4,
  TestID -> "InfraAngle-cycle-local"
]

VerificationTest[
  InfraAngle[PathGraph[Range[5]], {1, 3, 5}],
  Infinity,
  TestID -> "InfraAngle-path-infinite"
]

VerificationTest[
  InfraAngle[CompleteGraph[4], {2, 1, 3}],
  1,
  TestID -> "InfraAngle-complete-graph"
]

VerificationTest[
  InfraAngle[CycleGraph[6], {1, 2, 3}] == InfraAngle[CycleGraph[6], {3, 2, 1}],
  True,
  TestID -> "InfraAngle-symmetric"
]

EndTestSection[]
