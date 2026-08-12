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


(* ----- weighted InfraPoint (mass) ----- *)

VerificationTest[
  InfraPoint[ { a, a, b } ],
  InfraPoint[ { a, b }, { 2, 1 } ],
  TestID -> "InfraPoint-duplicate-merges-to-weighted"
]

VerificationTest[
  InfraPoint[ { a, b }, { 1, 1 } ],
  InfraPoint[ { a, b } ],
  TestID -> "InfraPoint-all-ones-collapses"
]

VerificationTest[
  { InfraPoint[ { a, b }, { 2, 1 } ][ "Support" ],
    InfraPoint[ { a, b }, { 2, 1 } ][ "Weights" ],
    InfraPoint[ { a, b }, { 2, 1 } ][ "Mass" ],
    InfraPoint[ { a, b } ][ "Weights" ] },
  { { a, b }, { 2, 1 }, 3, { 1, 1 } },
  TestID -> "InfraPoint-weight-accessors"
]

VerificationTest[
  InfraPoint[ { InfraPoint[ { a }, { 2 } ], InfraPoint[ { a, b } ] } ],
  InfraPoint[ { a, b }, { 3, 1 } ],
  TestID -> "InfraPoint-aggregation-sums-mass"
]


(* ----- synthetic-invariant accessors (delegate to Infrageometry over the support) ----- *)

(* one ball-volume row per support vertex: on a path B_r(end) = r + 1 *)
VerificationTest[
  InfraPoint[ { 1 } ][ "BallVolumes", PathGraph @ Range[ 7 ], { 0, 3 } ],
  { { 1, 2, 3, 4 } },
  TestID -> "InfraPoint-BallVolumes-accessor"
]

(* the accessor respects BallVolumes (counting) == Accumulate[ShellAreas] *)
VerificationTest[
  With[ { g = CycleGraph[ 10 ], p = InfraPoint[ { 1 } ] },
    First @ p[ "BallVolumes", g ] === Accumulate[ First @ p[ "ShellAreas", g ] ] ],
  True,
  TestID -> "InfraPoint-BallVolumes-accumulates-ShellAreas"
]

(* multi-support point: one row per support vertex *)
VerificationTest[
  Length @ InfraPoint[ { 1, 5 } ][ "ShellAreas", PathGraph @ Range[ 7 ], { 0, 2 } ],
  2,
  TestID -> "InfraPoint-ShellAreas-per-support-vertex"
]

(* dimension readout projects VolumeGrowthObservables["BallDimension"]: one numeric per support *)
VerificationTest[
  MatchQ[ InfraPoint[ { 25 } ][ "Dimension", GridGraph[ { 7, 7 } ] ], { _?NumericQ } ],
  True,
  TestID -> "InfraPoint-Dimension-accessor-numeric"
]

VerificationTest[
  { First @ InfraPoint[ { a, b }, { 2, 1 } ], First @ InfraPoint[ { a, b } ],
    InfraPoint[ { a, b }, { 2, 1 } ][ "Support" ] },
  { <| a -> 2, b -> 1 |>, { a, b }, { a, b } },
  TestID -> "InfraPoint-First-is-the-stored-argument"
]


(* ----- column projection: wrapper[[i]] ----- *)

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 1 ]],
  InfraPoint[ { 1 }, { 2 } ],
  TestID -> "InfraSegment-column-start-weighted"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ -1 ]],
  InfraPoint[ { 3 }, { 2 } ],
  TestID -> "InfraSegment-column-end-weighted"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 2 ]],
  InfraPoint[ { 2, 4 } ],
  TestID -> "InfraSegment-column-middle-spread"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 1, 1 ]],
  { 1, 2, 3 },
  TestID -> "InfraSegment-multi-index-first-path-preserved"
]

VerificationTest[
  InfraLine[ { { 1, 2, 3 }, { 1, 2, 5 } } ][[ 2 ]],
  InfraPoint[ { 2 }, { 2 } ],
  TestID -> "InfraLine-column-weighted"
]


(* ===================== FindInfraCycle ===================== *)

VerificationTest[
  Head @ FindInfraCycle[ CycleGraph[ 4 ], 1 ],
  InfraCircle,
  TestID -> "FindInfraCycle-returns-InfraCircle"
]

VerificationTest[
  Length @ FindInfraCycle[ CycleGraph[ 4 ], All ][ "Realizations" ],
  1,
  TestID -> "FindInfraCycle-CycleGraph4-one-cycle"
]

VerificationTest[
  FindInfraCycle[ TreeGraph[ { 1 -> 2, 2 -> 3 } ], 1 ],
  $Failed,
  TestID -> "FindInfraCycle-tree-no-cycles"
]

VerificationTest[
  Length @ First @ First @ FindInfraCycle[ GridGraph[ { 3, 3 } ], { 4 }, 1 ],
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

(* single-realisation InfraPoint endpoints collapse to the same geodesic DAG as
   bare vertices -- FindInfraPoint output composes into FindInfraSegment directly *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    FindInfraSegment[ g, InfraPoint[ { 1 } ], InfraPoint[ { 25 } ] ] === FindInfraSegment[ g, 1, 25 ] ],
  True,
  TestID -> "FindInfraSegment-InfraPoint-endpoints-give-DAG"
]

(* the DAG "Start" / "End" are the source / sink InfraPoints (in/out-degree-0) *)
VerificationTest[
  With[ { seg = FindInfraSegment[ GridGraph[ { 5, 5 } ], 1, 25 ] },
    { seg[ "Start" ], seg[ "End" ] } ],
  { InfraPoint[ { 1 } ], InfraPoint[ { 25 } ] },
  TestID -> "InfraSegment-DAG-Start-End-source-sink"
]

(* the enumerated form agrees: distinct first / last vertices across realisations *)
VerificationTest[
  { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "Start" ],
    InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "End" ] },
  { InfraPoint[ { 1 } ], InfraPoint[ { 3 } ] },
  TestID -> "InfraSegment-reps-Start-End"
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
