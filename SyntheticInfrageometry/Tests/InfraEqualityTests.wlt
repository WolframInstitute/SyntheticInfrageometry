BeginTestSection["InfraEquality"]

(* ===== points: the four Method branches ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], 4, 4 ],
  True,
  TestID -> "InfraEqualQ-Point-identical-default"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 3 -> 1, 4 -> 1 |>, <| 4 -> 1, 5 -> 1 |> ],
  False,
  TestID -> "InfraEqualQ-Point-half-overlap-Diffuse-False"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 3 -> 1, 4 -> 1, 5 -> 1 |>, <| 4 -> 1, 5 -> 1 |> ],
  True,
  TestID -> "InfraEqualQ-Point-majority-overlap-Diffuse-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 3 -> 1, 4 -> 1 |>, <| 4 -> 1, 5 -> 1 |>, Method -> "Overlap" ],
  True,
  TestID -> "InfraEqualQ-Point-half-overlap-Overlap-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 1 -> 1, 2 -> 1 |>, <| 6 -> 1, 7 -> 1 |>, Method -> "Overlap" ],
  False,
  TestID -> "InfraEqualQ-Point-disjoint-Overlap-False"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 3 -> 1, 4 -> 1, 5 -> 1 |>, <| 3 -> 1, 4 -> 1, 5 -> 1 |>, Method -> "Set" ],
  True,
  TestID -> "InfraEqualQ-Point-permuted-Set-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 3 -> 1, 4 -> 1, 5 -> 1 |>, <| 3 -> 1, 4 -> 1, 5 -> 1 |>, Method -> "Multiset" ],
  True,
  TestID -> "InfraEqualQ-Point-permuted-Multiset-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ],
    <| 3 -> 1, 4 -> 2 |>, <| 3 -> 1, 4 -> 1 |>, Method -> "Multiset" ],
  False,
  TestID -> "InfraEqualQ-effectivepoint-multiplicity-mismatch-Multiset-False"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], <| 3 -> 1, 4 -> 1 |>, <| 3 -> 1, 4 -> 1 |>, Method -> "Set" ],
  True,
  TestID -> "InfraEqualQ-repeated-vertex-collapses-to-the-same-multiset"
]

(* ===== Cross-head: heads must match ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 5 ] ], 1, InfraSegment[ { { 1 } } ] ],
  False,
  TestID -> "InfraEqualQ-head-mismatch-False"
]

(* ===== Bad method ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 5 ] ], 1, 1, Method -> "Nonsense" ],
  $Failed,
  { InfraEqualQ::badmethod },
  TestID -> "InfraEqualQ-bad-method-message"
]

(* ===== InfraSegment ===== *)

VerificationTest[
  InfraEqualQ[ GridGraph[ { 3, 3 } ],
    InfraSegment[ { { 1, 2, 5 }, { 1, 4, 5 } } ],
    InfraSegment[ { { 1, 4, 5 }, { 1, 2, 5 } } ],
    Method -> "Multiset" ],
  True,
  TestID -> "InfraEqualQ-Segment-permuted-realisations-Multiset"
]

VerificationTest[
  InfraEqualQ[ GridGraph[ { 3, 3 } ],
    FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9 , All],
    FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9 , All] ],
  True,
  TestID -> "InfraEqualQ-Segment-FindInfraSegment-self"
]

(* ===== InfraBall ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], FindInfraBall[ PathGraph[ Range[ 7 ] ], 4, 1 ], FindInfraBall[ PathGraph[ Range[ 7 ] ], 4, 1 ] ],
  True,
  TestID -> "InfraEqualQ-Ball-identical"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], FindInfraBall[ PathGraph[ Range[ 7 ] ], 4, 1 ], FindInfraBall[ PathGraph[ Range[ 7 ] ], 4, 2 ] ],
  True,
  TestID -> "InfraEqualQ-Ball-nested-Diffuse-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], FindInfraBall[ PathGraph[ Range[ 7 ] ], 4, 1 ], FindInfraBall[ PathGraph[ Range[ 7 ] ], 4, 2 ], Method -> "Set" ],
  False,
  TestID -> "InfraEqualQ-Ball-nested-Set-False"
]

(* ===== Boundary case |A cap B| == |A delta B| (strict inequality) ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraBall[ { { 2, 3, 4 } } ], InfraBall[ { { 3, 4, 5 } } ] ],
  False,
  TestID -> "InfraEqualQ-Ball-boundary-tie-Diffuse-False"
]

(* ===== InfraShell, multisets ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraShell[ { { 2, 4 } } ], InfraShell[ { { 2, 4 } } ] ],
  True,
  TestID -> "InfraEqualQ-Shell-identical"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 5 ] ], <| 1 -> 1, 2 -> 1, 3 -> 1 |>, <| 1 -> 1, 2 -> 1, 3 -> 1 |> ],
  True,
  TestID -> "InfraEqualQ-Object-identical"
]

(* ===== InfraCircle: open-cycle realisations are multiset-equal under rotation ===== *)

VerificationTest[
  InfraEqualQ[ CycleGraph[ 6 ],
    InfraCircle[ { { 1, 2, 3, 4, 5, 6 } } ],
    InfraCircle[ { { 2, 3, 4, 5, 6, 1 } } ],
    Method -> "Multiset" ],
  True,
  TestID -> "InfraEqualQ-Circle-rotation-Multiset"
]

(* ===== InfraPolyline ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ],
    InfraPolyline[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 4, 5 } } ] } } ],
    InfraPolyline[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 4, 5 } } ] } } ] ],
  True,
  TestID -> "InfraEqualQ-Polyline-identical"
]

(* ===== Lattice: Multiset => Set => Diffuse => Overlap ===== *)

VerificationTest[
  Module[ { g = PathGraph[ Range[ 7 ] ],
            check = { graph, a, b } |-> With[ {
              ov = InfraEqualQ[ graph, a, b, Method -> "Overlap" ],
              df = InfraEqualQ[ graph, a, b, Method -> "Diffuse" ],
              st = InfraEqualQ[ graph, a, b, Method -> "Set" ],
              ms = InfraEqualQ[ graph, a, b, Method -> "Multiset" ] },
              Implies[ ms, st ] && Implies[ st, df ] && Implies[ df, ov ] ] },
    AllTrue[ {
      check[ g, 1, 1 ],
      check[ g, <| 3 -> 1, 4 -> 1 |>, <| 4 -> 1, 5 -> 1 |> ],
      check[ g, <| 3 -> 1, 4 -> 1, 5 -> 1 |>, <| 4 -> 1, 5 -> 1 |> ],
      check[ g, <| 3 -> 1, 4 -> 1 |>, <| 3 -> 1, 4 -> 1 |> ],
      check[ g, InfraBall[ { { 2, 3, 4 } } ], InfraBall[ { { 3, 4, 5 } } ] ],
      check[ g, InfraShell[ { { 1, 5 }, { 2, 4 } } ], InfraShell[ { { 1, 5 } } ] ]
    }, # === True & ]
  ],
  True,
  TestID -> "InfraEqualQ-lattice-monotonicity"
]

EndTestSection[]
