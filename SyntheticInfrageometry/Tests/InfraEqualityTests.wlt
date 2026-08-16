BeginTestSection["InfraEquality"]

(* ===== InfraPoint: the four Method branches ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraPoint[4], InfraPoint[4] ],
  True,
  TestID -> "InfraEqualQ-Point-identical-default"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 3, 4 } ], InfraSet[ { 4, 5 } ] ],
  False,
  TestID -> "InfraEqualQ-Point-half-overlap-Diffuse-False"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 3, 4, 5 } ], InfraSet[ { 4, 5 } ] ],
  True,
  TestID -> "InfraEqualQ-Point-majority-overlap-Diffuse-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 3, 4 } ], InfraSet[ { 4, 5 } ], Method -> "Overlap" ],
  True,
  TestID -> "InfraEqualQ-Point-half-overlap-Overlap-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 1, 2 } ], InfraSet[ { 6, 7 } ], Method -> "Overlap" ],
  False,
  TestID -> "InfraEqualQ-Point-disjoint-Overlap-False"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 3, 4, 5 } ], InfraSet[ { 5, 4, 3 } ], Method -> "Set" ],
  True,
  TestID -> "InfraEqualQ-Point-permuted-Set-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 3, 4, 5 } ], InfraSet[ { 5, 4, 3 } ], Method -> "Multiset" ],
  True,
  TestID -> "InfraEqualQ-Point-permuted-Multiset-True"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ],
    InfraMesoPoint[ <| 3 -> 1, 4 -> 2 |> ], InfraMesoPoint[ <| 3 -> 1, 4 -> 1 |> ], Method -> "Multiset" ],
  False,
  TestID -> "InfraEqualQ-mesopoint-multiplicity-mismatch-Multiset-False"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraSet[ { 3, 4, 4 } ], InfraSet[ { 3, 4 } ], Method -> "Set" ],
  True,
  TestID -> "InfraEqualQ-Point-multiplicity-mismatch-Set-True"
]

(* ===== Cross-head: heads must match ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 5 ] ], InfraPoint[1], InfraSegment[ { { 1 } } ] ],
  False,
  TestID -> "InfraEqualQ-head-mismatch-False"
]

(* ===== Bad method ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 5 ] ], InfraPoint[1], InfraPoint[1], Method -> "Nonsense" ],
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
    FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9 ],
    FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9 ] ],
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

(* ===== InfraShell, InfraObject ===== *)

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 7 ] ], InfraShell[ { { 2, 4 } } ], InfraShell[ { { 2, 4 } } ] ],
  True,
  TestID -> "InfraEqualQ-Shell-identical"
]

VerificationTest[
  InfraEqualQ[ PathGraph[ Range[ 5 ] ], InfraObject[ { 1, 2, 3 } ], InfraObject[ { 1, 2, 3 } ] ],
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
      check[ g, InfraPoint[1], InfraPoint[1] ],
      check[ g, InfraSet[ { 3, 4 } ], InfraSet[ { 4, 5 } ] ],
      check[ g, InfraSet[ { 3, 4, 5 } ], InfraSet[ { 4, 5 } ] ],
      check[ g, InfraSet[ { 3, 4, 4 } ], InfraSet[ { 3, 4 } ] ],
      check[ g, InfraBall[ { { 2, 3, 4 } } ], InfraBall[ { { 3, 4, 5 } } ] ],
      check[ g, InfraShell[ { { 1, 5 }, { 2, 4 } } ], InfraShell[ { { 1, 5 } } ] ]
    }, # === True & ]
  ],
  True,
  TestID -> "InfraEqualQ-lattice-monotonicity"
]

EndTestSection[]
