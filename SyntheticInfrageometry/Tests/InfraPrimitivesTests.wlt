(* Wrapper-head behaviour: only auto-flatten survives.  String accessors and
   Part upvalue rules were removed -- wrappers are raw data, callers use
   First / Length / Part on the inner list directly. *)

VerificationTest[
  InfraPoint[ { InfraPoint[ { 1, 2 } ], InfraPoint[ { 3 } ], 4 } ],
  InfraPoint[ { 1, 2, 3, 4 } ],
  TestID -> "InfraPoint-auto-flatten"
]

VerificationTest[
  InfraSegment[ { InfraSegment[ { { 1, 2 }, { 1, 3 } } ], InfraSegment[ { { 2, 3 } } ] } ],
  InfraSegment[ { { 1, 2 }, { 1, 3 }, { 2, 3 } } ],
  TestID -> "InfraSegment-auto-flatten"
]

(* Round-trip: a List of unary wrappers wrapped under the same head collapses
   to the multi-realisation form (the canonical idiom for constructing multi
   from a Find* result). *)

VerificationTest[
  InfraPoint @ { InfraPoint[ { 1 } ], InfraPoint[ { 2 } ], InfraPoint[ { 3 } ] },
  InfraPoint[ { 1, 2, 3 } ],
  TestID -> "InfraPoint-unary-list-to-multi"
]

(* Default Part semantics: wrappers are raw data; Part returns inner elements. *)

VerificationTest[
  InfraPoint[ { 1, 2, 3 } ][[ 1 ]],
  { 1, 2, 3 },
  TestID -> "InfraPoint-Part-first-arg"
]

VerificationTest[
  First @ InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
  { { 1, 2, 3 }, { 1, 4, 3 } },
  TestID -> "InfraSegment-First-inner-list"
]

VerificationTest[
  Length @ First @ InfraPoint[ { 1, 2, 3 } ],
  3,
  TestID -> "InfraPoint-Length-of-inner"
]


(* ===================== FindInfraCycle ===================== *)

VerificationTest[
  Head @ First @ FindInfraCycle[ CycleGraph[ 4 ], 1 ],
  InfraCircle,
  TestID -> "FindInfraCycle-returns-InfraCircle"
]

VerificationTest[
  Length @ FindInfraCycle[ CycleGraph[ 4 ], All ],
  1,
  TestID -> "FindInfraCycle-CycleGraph4-one-cycle"
]

VerificationTest[
  FindInfraCycle[ TreeGraph[ { 1 -> 2, 2 -> 3 } ], 1 ],
  $Failed,
  TestID -> "FindInfraCycle-tree-no-cycles"
]

VerificationTest[
  Length @ First @ First @ First @ FindInfraCycle[ GridGraph[ { 3, 3 } ], { 4 }, 1 ],
  4,
  TestID -> "FindInfraCycle-length4-on-grid"
]

VerificationTest[
  NullHomotopicQ[ GridGraph[ { 3, 3 } ],
    First @ FindInfraCycle[ GridGraph[ { 3, 3 } ], 1 ],
    "NullHomotopicCycles" -> { 4 } ],
  True,
  TestID -> "FindInfraCycle-shortest-is-nullhomotopic-on-grid"
]


(* ===================== Length / Volume accessors =====================
   Line-like wrappers carry an integer "Length" (edge count per realisation,
   always returned as a list).  Set-like wrappers carry an integer "Volume"
   (vertex count per realisation).  Closed cycles count #vertices = #edges. *)

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 5, 3 } } ][ "Length" ],
  { 2, 3 },
  TestID -> "InfraSegment-Length-edge-count"
]

VerificationTest[
  InfraPath[ { { 1, 2, 3, 2, 1 } } ][ "Length" ],
  { 4 },
  TestID -> "InfraPath-Length-edge-count"
]

VerificationTest[
  InfraRay[ { { 1, 2, 3, 4 } } ][ "Length" ],
  { 3 },
  TestID -> "InfraRay-Length-edge-count"
]

VerificationTest[
  InfraLine[ { { 1, 2, 3, 4, 5 } } ][ "Length" ],
  { 4 },
  TestID -> "InfraLine-Length-edge-count"
]

VerificationTest[
  InfraCircle[ { { 1, 2, 3, 4, 5, 6 } } ][ "Length" ],
  { 6 },
  TestID -> "InfraCircle-Length-equals-vertex-count"
]

VerificationTest[
  InfraEllipse[ { { 1, 2, 3, 4 }, { 5, 6, 7, 8, 9 } } ][ "Length" ],
  { 4, 5 },
  TestID -> "InfraEllipse-Length-equals-vertex-count"
]

VerificationTest[
  InfraBall[ { { 1, 2, 3, 4, 5 } } ][ "Volume" ],
  { 5 },
  TestID -> "InfraBall-Volume-vertex-count"
]

VerificationTest[
  InfraShell[ { { 1, 2, 3 }, { 4, 5 } } ][ "Volume" ],
  { 3, 2 },
  TestID -> "InfraShell-Volume-vertex-count"
]

VerificationTest[
  InfraPlane[ { { 1, 2, 3, 4 } } ][ "Volume" ],
  { 4 },
  TestID -> "InfraPlane-Volume-vertex-count"
]

VerificationTest[
  InfraEllipticShell[ { { 1, 2, 3 } } ][ "Volume" ],
  { 3 },
  TestID -> "InfraEllipticShell-Volume-vertex-count"
]

VerificationTest[
  InfraObject[ { 1, 2, 3, 4 } ][ "Volume" ],
  { 4 },
  TestID -> "InfraObject-Volume-vertex-count-singleton"
]


(* ===================== InfraPolyline accessors ===================== *)

VerificationTest[
  FindInfraPolylineSubdivision[ GridGraph[ { 4, 4 } ],
    { 1, 2, 6, 5, 9, 13, 14, 15, 16 }, "MaxLength" -> 2 ][ "Length" ],
  { 8 },
  TestID -> "InfraPolyline-Length-sum-of-legs"
]

VerificationTest[
  FindInfraPolylineSubdivision[ GridGraph[ { 4, 4 } ],
    { 1, 2, 6, 5, 9, 13, 14, 15, 16 }, "MaxLength" -> 2 ][ "Knots" ],
  { { InfraPoint[ { 1 } ], InfraPoint[ { 6 } ], InfraPoint[ { 9 } ],
      InfraPoint[ { 14 } ], InfraPoint[ { 16 } ] } },
  TestID -> "InfraPolyline-Knots-as-InfraPoints"
]

VerificationTest[
  InfraPolyline[ { { } } ][ "Length" ],
  { 0 },
  TestID -> "InfraPolyline-Length-empty"
]

VerificationTest[
  InfraPolyline[ { { } } ][ "Knots" ],
  { { } },
  TestID -> "InfraPolyline-Knots-empty"
]
