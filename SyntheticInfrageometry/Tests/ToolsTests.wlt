BeginTestSection["Tools"]

(* The distance metrics, centrality helpers, separating-cycle predicates, and
   path-selection routines in Tools.wl are now package-scope (internal). They
   are exercised indirectly through the public Find* functions and their
   "Select" option. Direct unit tests for them have been removed. *)

VerificationTest[
  True,
  True,
  TestID -> "Tools-placeholder"
]

(* ===================== InfraMeasure ===================== *)

(* set-like measure: every value is a frequency in (0, 1] *)
VerificationTest[
  AllTrue[ Values @ InfraMeasure[ InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ] ], 0 < # <= 1 & ],
  True,
  TestID -> "InfraMeasure-set-values-in-unit-interval"
]

(* the two vertices common to both realisations carry full measure 1 *)
VerificationTest[
  Lookup[ InfraMeasure[ InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ] ], { 2, 3 } ],
  { 1, 1 },
  TestID -> "InfraMeasure-set-common-vertices"
]

(* single realisation: every vertex visited exactly once per realisation maps to 1 *)
VerificationTest[
  InfraMeasure[ InfraSegment[ { { 1, 2, 3 } } ] ],
  <| 1 -> 1, 2 -> 1, 3 -> 1 |>,
  TestID -> "InfraMeasure-single-realisation-all-one"
]

(* raw counts: sum over vertices equals total membership across the bundle *)
VerificationTest[
  With[ { reps = { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 }, { 1, 2, 5, 8, 9 } } },
    Total @ Values @ InfraMeasure[ InfraSegment[ reps ], "Normalize" -> False ] ==
      Total[ Length /@ reps ] ],
  True,
  TestID -> "InfraMeasure-rawcount-sum-equals-membership"
]

(* normalisation divides raw counts by the number of realisations *)
VerificationTest[
  With[ { obj = InfraSegment[ { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 }, { 1, 2, 5, 8, 9 } } ] },
    InfraMeasure[ obj ] == InfraMeasure[ obj, "Normalize" -> False ] / 3 ],
  True,
  TestID -> "InfraMeasure-normalise-is-rawcount-over-numreps"
]

(* empty bundle yields the empty measure *)
VerificationTest[
  InfraMeasure[ InfraSegment[ { } ] ],
  <||>,
  TestID -> "InfraMeasure-empty-bundle"
]

(* edge measure: keys are sorted UndirectedEdges of the path's steps *)
VerificationTest[
  Keys @ InfraMeasure[ PathGraph @ Range[ 4 ], InfraSegment[ { { 1, 2, 3, 4 } } ], "On" -> "Edges" ],
  { UndirectedEdge[ 1, 2 ], UndirectedEdge[ 2, 3 ], UndirectedEdge[ 3, 4 ] },
  TestID -> "InfraMeasure-edge-keys-undirected"
]

(* "Both" returns the two marginals keyed by name *)
VerificationTest[
  Keys @ InfraMeasure[ PathGraph @ Range[ 4 ], InfraSegment[ { { 1, 2, 3, 4 } } ], "On" -> "Both" ],
  { "Vertices", "Edges" },
  TestID -> "InfraMeasure-both-shape"
]

(* weighted InfraPoint: measure is the weight-normalised probability distribution *)
VerificationTest[
  Total @ Values @ InfraMeasure[ InfraPoint[ { 1, 2 }, { 3, 1 } ] ],
  1,
  TestID -> "InfraMeasure-weighted-point-sums-to-one"
]

(* the ["Measure"] accessor delegates to the engine, across all wrapper shapes *)
VerificationTest[
  AllTrue[
    { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
      InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ],
      InfraCircle[ { { 1, 2, 3 } } ],
      InfraPoint[ { 1, 2 }, { 3, 1 } ],
      InfraSet[ { 1, 2, 3 } ] },
    w |-> w[ "Measure" ] === InfraMeasure[ w ] ],
  True,
  TestID -> "InfraMeasure-accessor-agrees-with-engine"
]

(* the accessor is the normalized vertex measure: set-like values in (0,1], single realisation all 1 *)
VerificationTest[
  { AllTrue[ Values @ InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ][ "Measure" ], 0 < # <= 1 & ],
    InfraSegment[ { { 1, 2, 3 } } ][ "Measure" ] },
  { True, <| 1 -> 1, 2 -> 1, 3 -> 1 |> },
  TestID -> "InfraMeasure-accessor-invariants"
]

EndTestSection[]
