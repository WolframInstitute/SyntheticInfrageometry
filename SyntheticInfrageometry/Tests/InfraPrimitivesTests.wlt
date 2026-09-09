toDensity = WolframInstitute`SyntheticInfrageometry`PackageScope`toDensity;

(* Wrapper-head behaviour: only auto-flatten survives.  String accessors and
   Part upvalue rules were removed -- wrappers are raw data, callers use
   First / Length / Part on the inner list directly.  Points and sets carry no
   head at all: the shape is the kind. *)

VerificationTest[
  InfraSegment[ { InfraSegment[ { { 1, 2 }, { 1, 3 } } ], InfraSegment[ { { 2, 3 } } ] } ],
  InfraSegment[ { { 1, 2 }, { 1, 3 }, { 2, 3 } } ],
  TestID -> "InfraSegment-auto-flatten"
]

(* ----- the multiset layer: <| atom -> weight |> ----- *)

(* a List is one Counts away from the multiset; repetition becomes mass *)
VerificationTest[
  { toDensity[ PathGraph @ Range[ 3 ], { 1, 1, 2 } ], Counts[ { a, a, b } ] },
  { <| 1 -> 2, 2 -> 1 |>, <| a -> 2, b -> 1 |> },
  TestID -> "list-reads-as-multiset-counts"
]

(* the all-ones density is still a density: nothing collapses it to its support, and
   Keys is the explicit step down, which drops the masses *)
VerificationTest[
  With[ { fam = <| a -> 1, b -> 1 |> },
    { Head @ fam, Keys @ fam } ],
  { Association, { a, b } },
  TestID -> "all-ones-density-stays-a-density"
]

(* the multiset algebra is the Association's own: Keys, Values, Total, Length *)
VerificationTest[
  With[ { fam = <| a -> 2, b -> 1 |> },
    { Keys @ fam, Values @ fam, Total @ fam, Length @ fam } ],
  { { a, b }, { 2, 1 }, 3, 2 },
  TestID -> "multiset-weight-algebra-is-the-association"
]


(* ----- synthetic invariants are read off the primitives, not off a wrapper ----- *)

(* on a path B_r(end) = r + 1; a multiset gives one row per support vertex *)
VerificationTest[
  { BallVolumes[ PathGraph @ Range[ 7 ], 1, { 0, 3 } ],
    BallVolumes[ PathGraph @ Range[ 7 ], Keys @ <| 1 -> 1 |>, { 0, 3 } ] },
  { { 1, 2, 3, 4 }, { { 1, 2, 3, 4 } } },
  TestID -> "point-layer-BallVolumes"
]

(* the tube of a pair thickens the metric interval, so it is never smaller than it *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    AllTrue[ TubeVolumes[ g, 13, 25, 1 ], # >= Length @ MetricInterval[ g, 13, 25 ] & ] ],
  True,
  TestID -> "point-layer-TubeVolumes"
]

(* the interval count at slack 0 counts the metric interval *)
VerificationTest[
  IntervalVolumes[ PathGraph @ Range[ 7 ], 1, 4, 0 ],
  4,
  TestID -> "point-layer-IntervalVolumes"
]

(* dimension readout projects VolumeGrowthObservables["BallDimension"] *)
VerificationTest[
  MatchQ[ VolumeGrowthObservables[ GridGraph[ { 7, 7 } ], 25 ][ "BallDimension" ], _?NumericQ ],
  True,
  TestID -> "point-layer-Dimension-numeric"
]

(* the k-th element of a multiset is read with the Association's own Part / Keys:
   [[k]] is the k-th MASS, Keys[[k]] the k-th vertex *)
VerificationTest[
  With[ { s = <| a -> 1, b -> 1, c -> 1 |> },
    { Keys[ s ][[ 2 ]], First @ Keys @ s, Keys @ s[[ ;; 2 ]] } ],
  { b, a, { a, b } },
  TestID -> "multiset-Part-through-Keys"
]


(* ----- column projection: wrapper[[i]] ----- *)

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 1 ]],
  <| 1 -> 2 |>,
  TestID -> "InfraSegment-column-start-weighted"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ -1 ]],
  <| 3 -> 2 |>,
  TestID -> "InfraSegment-column-end-weighted"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 2 ]],
  <| 2 -> 1, 4 -> 1 |>,
  TestID -> "InfraSegment-column-middle-spread"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][[ 1, 1 ]],
  { 1, 2, 3 },
  TestID -> "InfraSegment-multi-index-first-path-preserved"
]

VerificationTest[
  InfraLine[ { { 1, 2, 3 }, { 1, 2, 5 } } ][[ 2 ]],
  <| 2 -> 2 |>,
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

(* FindInfraPoint output is a bare vertex list, so it composes into FindInfraSegment
   directly -- with no unwrapping step *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], ends = { 1, 25 } },
    FindInfraSegment[ g, ends[[ 1 ]], ends[[ 2 ]], All ] === FindInfraSegment[ g, 1, 25, All ] ],
  True,
  TestID -> "FindInfraSegment-vertex-endpoints-give-DAG"
]

(* the DAG "Start" / "End" are the source / sink multisets (in/out-degree-0) *)
VerificationTest[
  With[ { seg = FindInfraSegment[ GridGraph[ { 5, 5 } ], 1, 25 , All] },
    { seg[ "Start" ], seg[ "End" ] } ],
  { <| 1 -> 1 |>, <| 25 -> 1 |> },
  TestID -> "InfraSegment-DAG-Start-End-source-sink"
]

(* the enumerated form agrees: distinct first / last vertices across realisations *)
VerificationTest[
  { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "Start" ],
    InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "End" ] },
  { <| 1 -> 1 |>, <| 3 -> 1 |> },
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

(* a set is one multiset, so its size is Length, not the per-realisation "Volume" of the bundle heads *)
VerificationTest[
  Length @ <| 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1 |>,
  4,
  TestID -> "multiset-Length-vertex-count"
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
  { { 1, 6, 9,
      14, 16 } },
  TestID -> "InfraPolyline-Knots-are-vertices"
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
