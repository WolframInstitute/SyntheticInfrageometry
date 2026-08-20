(* Wrapper-head behaviour: only auto-flatten survives.  String accessors and
   Part upvalue rules were removed -- wrappers are raw data, callers use
   First / Length / Part on the inner list directly. *)

VerificationTest[
  InfraSet[ { InfraSet[ { 1, 2 } ], InfraSet[ { 3, 4 } ] } ],
  InfraSet[ { 1, 2, 3, 4 } ],
  TestID -> "InfraSet-auto-flatten"
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
  InfraSet @ { InfraPoint[1], InfraPoint[2], InfraPoint[3] },
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "InfraSet-from-atom-list"
]


(* ----- weighted InfraPoint (mass) ----- *)

(* a set deduplicates; repetition becomes mass only in the measure layer *)
VerificationTest[
  { InfraSet[ { a, a, b } ], InfraMesoPoint[ { InfraPoint[a], InfraPoint[a], InfraPoint[b] } ] },
  { InfraSet[ { a, b } ], InfraMesoPoint[<|a -> 2, b -> 1|>] },
  TestID -> "set-dedups-measure-counts"
]

(* the all-ones measure STAYS a mesopoint -- layers never cross silently *)
VerificationTest[
  Head @ InfraMesoPoint[<|a -> 1, b -> 1|>],
  InfraMesoPoint,
  TestID -> "InfraMesoPoint-all-ones-stays-meso"
]

VerificationTest[
  { InfraMesoPoint[<|a -> 2, b -> 1|>][ "Support" ],
    InfraMesoPoint[<|a -> 2, b -> 1|>][ "Weights" ],
    InfraMesoPoint[<|a -> 2, b -> 1|>][ "Mass" ],
    InfraSet[ { a, b } ][ "Weights" ] },
  { InfraSet[ { a, b } ], { 2, 1 }, 3, { 1, 1 } },
  TestID -> "point-layer-weight-accessors"
]

(* repetition in an atom list is mass: the counting measure *)
VerificationTest[
  InfraMesoPoint[ { InfraPoint[a], InfraPoint[a], InfraPoint[b] } ],
  InfraMesoPoint[<|a -> 2, b -> 1|>],
  TestID -> "InfraMesoPoint-counts-atom-list"
]


(* ----- synthetic-invariant accessors (delegate to Infrageometry over the support) ----- *)

(* one ball-volume row per support vertex: on a path B_r(end) = r + 1 *)
(* the atom returns the bare per-radius row; the set returns one row per vertex *)
VerificationTest[
  { InfraPoint[1][ "BallVolumes", PathGraph @ Range[ 7 ], { 0, 3 } ],
    InfraSet[ { 1 } ][ "BallVolumes", PathGraph @ Range[ 7 ], { 0, 3 } ] },
  { { 1, 2, 3, 4 }, { { 1, 2, 3, 4 } } },
  TestID -> "point-layer-BallVolumes-accessor"
]

(* the accessor respects BallVolumes (counting) == Accumulate[ShellAreas] *)
VerificationTest[
  With[ { g = CycleGraph[ 10 ], p = InfraPoint[1] },
    p[ "BallVolumes", g ] === Accumulate[ p[ "ShellAreas", g ] ] ],
  True,
  TestID -> "InfraPoint-BallVolumes-accumulates-ShellAreas"
]

(* multi-support point: one row per support vertex *)
VerificationTest[
  Length @ InfraSet[ { 1, 5 } ][ "ShellAreas", PathGraph @ Range[ 7 ], { 0, 2 } ],
  2,
  TestID -> "InfraSet-ShellAreas-per-vertex"
]

(* dimension readout projects VolumeGrowthObservables["BallDimension"]: one numeric per support *)
VerificationTest[
  MatchQ[ InfraPoint[25][ "Dimension", GridGraph[ { 7, 7 } ] ], _?NumericQ ],
  True,
  TestID -> "InfraPoint-Dimension-accessor-numeric"
]

VerificationTest[
  { First @ InfraMesoPoint[<|a -> 2, b -> 1|>], First @ InfraSet[ { a, b } ],
    InfraMesoPoint[<|a -> 2, b -> 1|>][ "Support" ], InfraPoint[a][ "Vertex" ] },
  { <| a -> 2, b -> 1 |>, { a, b }, InfraSet[ { a, b } ], a },
  TestID -> "point-layer-First-is-the-stored-argument"
]


(* ----- column projection: wrapper[[i]] ----- *)

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 1 ]],
  InfraMesoPoint[ <| 1 -> 2 |> ],
  TestID -> "InfraSegment-column-start-weighted"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ -1 ]],
  InfraMesoPoint[ <| 3 -> 2 |> ],
  TestID -> "InfraSegment-column-end-weighted"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 2 ]],
  InfraMesoPoint[ <| 2 -> 1, 4 -> 1 |> ],
  TestID -> "InfraSegment-column-middle-spread"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 1, 1 ]],
  { 1, 2, 3 },
  TestID -> "InfraSegment-multi-index-first-path-preserved"
]

VerificationTest[
  InfraLine[ { { 1, 2, 3 }, { 1, 2, 5 } } ][[ 2 ]],
  InfraMesoPoint[ <| 2 -> 2 |> ],
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
    FindInfraSegment[ g, InfraPoint[1], InfraPoint[25] ] === FindInfraSegment[ g, 1, 25 ] ],
  True,
  TestID -> "FindInfraSegment-InfraPoint-endpoints-give-DAG"
]

(* the DAG "Start" / "End" are the source / sink InfraSets (in/out-degree-0) *)
VerificationTest[
  With[ { seg = FindInfraSegment[ GridGraph[ { 5, 5 } ], 1, 25 ] },
    { seg[ "Start" ], seg[ "End" ] } ],
  { InfraSet[ { 1 } ], InfraSet[ { 25 } ] },
  TestID -> "InfraSegment-DAG-Start-End-source-sink"
]

(* the enumerated form agrees: distinct first / last vertices across realisations *)
VerificationTest[
  { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "Start" ],
    InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "End" ] },
  { InfraSet[ { 1 } ], InfraSet[ { 3 } ] },
  TestID -> "InfraSegment-reps-Start-End"
]

VerificationTest[
  InfraWalk[ { { 1, 2, 3, 2, 1 } } ][ "Length" ],
  { 4 },
  TestID -> "InfraWalk-Length-edge-count"
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
  { { InfraPoint[1], InfraPoint[6], InfraPoint[9],
      InfraPoint[14], InfraPoint[16] } },
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
