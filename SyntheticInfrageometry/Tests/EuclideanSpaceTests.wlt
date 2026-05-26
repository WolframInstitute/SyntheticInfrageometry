BeginTestSection["InfraEuclideanSpace"]

(* ===== InfraScalarProduct (Alexandrov, k = 0 default) ===== *)

VerificationTest[
  InfraScalarProduct[PathGraph[Range[5]], 1, 3, 4],
  6,
  TestID -> "InfraScalarProduct-path-Alexandrov-formula"
]

VerificationTest[
  Table[
    InfraScalarProduct[PathGraph[Range[5]], 1, k, l] === (k - 1)(l - 1),
    {k, 1, 5}, {l, 1, 5}
  ] // Flatten // Apply[And],
  True,
  TestID -> "InfraScalarProduct-path-Alexandrov-identity"
]

VerificationTest[
  InfraScalarProduct[GridGraph[{4, 4}], 1, 2, 5],
  -1,
  TestID -> "InfraScalarProduct-grid-Alexandrov-not-orthogonal"
]

VerificationTest[
  InfraScalarProduct[PathGraph[Range[5]], 1, 3, 3],
  GraphDistance[PathGraph[Range[5]], 1, 3]^2,
  TestID -> "InfraScalarProduct-norm-equals-distance-squared"
]

(* ===== InfraScalarProduct (Alexandrov, curvature k != 0) ===== *)

(* k = 0 default agrees with explicit "Curvature" -> 0. *)
VerificationTest[
  InfraScalarProduct[GridGraph[{4, 4}], 1, 2, 5,
    Method -> {"Alexandrov", "Curvature" -> 0}],
  -1,
  TestID -> "InfraScalarProduct-Alexandrov-curvature-zero-matches-default"
]

(* Generic relation: InfraAngle Alexandrov(k) = ArcCos[SP_k / (|u||v|)] for any k. *)
VerificationTest[
  With[ { g = CycleGraph[8], k = 1/4 },
    With[ { spk = InfraScalarProduct[g, 1, 3, 7,
                    Method -> {"Alexandrov", "Curvature" -> k}],
            angK = InfraAngle[g, {3, 1, 7},
                    Method -> {"Alexandrov", "Curvature" -> k}],
            a = GraphDistance[g, 1, 3], b = GraphDistance[g, 1, 7] },
      Simplify[ ArcCos[ spk / ( a b ) ] - angK ] === 0
    ] ],
  True,
  TestID -> "InfraScalarProduct-Alexandrov-curvature-recovers-angle"
]

(* Unknown method raises ::badmethod and returns $Failed. *)
VerificationTest[
  Quiet @ InfraScalarProduct[PathGraph[Range[5]], 1, 3, 4, Method -> "Schoenberg"],
  $Failed,
  TestID -> "InfraScalarProduct-Schoenberg-badmethod"
]

(* ===== InfraScalarProduct (Parallelogram) ===== *)

VerificationTest[
  InfraScalarProduct[PathGraph[Range[10]], 5, 7, 6, Method -> "Parallelogram"],
  2,
  TestID -> "InfraScalarProduct-path-Parallelogram-matches-Alexandrov"
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

(* Schoenberg is gone from InfraAngle.  The bare scalar product
   (d(p,q1)^2 + d(p,q2)^2 - d(q1,q2)^2) / 2 is recovered as
   InfraScalarProduct[g, p, q1, q2] (default Method -> "Alexandrov", k = 0). *)
VerificationTest[
  Quiet @ InfraAngle[PathGraph[Range[7]], {1, 4, 7}, Method -> "Schoenberg"],
  $Failed,
  TestID -> "InfraAngle-Schoenberg-badmethod"
]

VerificationTest[
  InfraScalarProduct[PathGraph[Range[7]], 4, 1, 7],
  -9,
  TestID -> "InfraScalarProduct-path-straight-recovers-Schoenberg-value"
]

(* (3, 4, 5) Pythagorean triple of graph distances:
   d(1, 4) = 3, d(1, 8) = 4, d(4, 8) = 5 -- Alexandrov SP = (9 + 16 - 25)/2 = 0. *)
VerificationTest[
  InfraScalarProduct[
    Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 1 <-> 5, 5 <-> 6, 6 <-> 7, 7 <-> 8,
           4 <-> 9, 9 <-> 10, 10 <-> 11, 11 <-> 12, 12 <-> 8}],
    1, 4, 8],
  0,
  TestID -> "InfraScalarProduct-Pythagorean-right-angle"
]

(* ArcCos[SP / (|u||v|)] recovers the InfraAngle Alexandrov(k = 0) value. *)
VerificationTest[
  With[ { g = CycleGraph[8] },
    ArcCos[
      InfraScalarProduct[g, 1, 3, 7] /
      ( GraphDistance[g, 1, 3] GraphDistance[g, 1, 7] )
    ] === InfraAngle[g, {3, 1, 7}, Method -> "Alexandrov"] ],
  True,
  TestID -> "InfraScalarProduct-recovers-Alexandrov-angle"
]

EndTestSection[]
