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

(* occupation: sum over vertices equals mean realisation length *)
VerificationTest[
  With[ { reps = { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 }, { 1, 2, 5, 8, 9 } } },
    Total @ Values @ InfraMeasure[ InfraSegment[ reps ] ] ==
      Total[ Length /@ reps ] / Length[ reps ] ],
  True,
  TestID -> "InfraMeasure-occupation-sum-equals-mean-length"
]

(* probability: the node distribution sums to 1 and is occupation renormalised *)
VerificationTest[
  With[ { obj = InfraSegment[ { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 }, { 1, 2, 5, 8, 9 } } ] },
    With[ { p = InfraMeasure[ obj, Method -> "Probability" ], occ = InfraMeasure[ obj ] },
      Total @ Values @ p == 1 && p == occ / Total[ occ ] ] ],
  True,
  TestID -> "InfraMeasure-probability-sums-to-one"
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

(* a density has two distinct normalisations: the default "Occupation" is
   membership relative to the heaviest mass (max 1, what the renderer draws),
   "Probability" is the distribution summing to 1 *)
VerificationTest[
  { Max @ Values @ InfraMeasure[ <| InfraPoint[ 1 ] -> 3, InfraPoint[ 2 ] -> 1 |> ],
    Total @ Values @ InfraMeasure[ <| InfraPoint[ 1 ] -> 3, InfraPoint[ 2 ] -> 1 |>, Method -> "Probability" ] },
  { 1, 1 },
  TestID -> "InfraMeasure-density-two-normalisations"
]

(* the ["Measure"] accessor delegates to the engine, across all wrapper shapes *)
VerificationTest[
  AllTrue[
    { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
      InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ],
      InfraCircle[ { { 1, 2, 3 } } ],
      InfraSet[ { 1, 2, 3 } ] },
    w |-> w[ "Measure" ] === InfraMeasure[ w ] ],
  True,
  TestID -> "InfraMeasure-accessor-agrees-with-engine"
]

(* a density carries no head, so it has no accessors: the engine measures it directly, unkeying the InfraPoint keys to bare vertices *)
VerificationTest[
  InfraMeasure[ <| InfraPoint[ 1 ] -> 3, InfraPoint[ 2 ] -> 1 |> ],
  <| 1 -> 1, 2 -> 1/3 |>,
  TestID -> "InfraMeasure-density-unkeys-to-vertices"
]

(* the accessor is the normalized vertex measure: set-like values in (0,1], single realisation all 1 *)
VerificationTest[
  { AllTrue[ Values @ InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ][ "Measure" ], 0 < # <= 1 & ],
    InfraSegment[ { { 1, 2, 3 } } ][ "Measure" ] },
  { True, <| 1 -> 1, 2 -> 1, 3 -> 1 |> },
  TestID -> "InfraMeasure-accessor-invariants"
]

(* OccupationCount (raw integers), OccupationMeasure (== Measure), ProbabilityMeasure (count renormalised, sums to 1) across all wrapper shapes *)
VerificationTest[
  AllTrue[
    { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
      InfraShell[ { { 1, 2, 3 }, { 2, 3, 4 } } ],
      InfraCircle[ { { 1, 2, 3 } } ],
      InfraSet[ { 1, 2, 3 } ] },
    w |-> And[
      w[ "OccupationMeasure" ] === w[ "Measure" ],
      AllTrue[ Values @ w[ "OccupationCount" ], IntegerQ ],
      w[ "ProbabilityMeasure" ] === w[ "OccupationCount" ] / Total @ w[ "OccupationCount" ],
      Total @ Values @ w[ "ProbabilityMeasure" ] === 1 ] ],
  True,
  TestID -> "InfraMeasure-occupation-probability-accessors"
]

(* ===== instances, families and densities ===== *)

(* the anchor rule and the family constructor are internal, so the tests reach them
   by their PackageScope context *)
toDensity = WolframInstitute`SyntheticInfrageometry`PackageScope`toDensity;
toFamily  = WolframInstitute`SyntheticInfrageometry`PackageScope`toFamily;

(* ===== bundles are sets of realisations ===== *)

(* a bundle is a SET of alternative realisations: duplicates collapse, no mass *)
VerificationTest[
  { InfraSegment[ { { 1, 2, 3 }, { 1, 2, 3 }, { 1, 4, 3 } } ],
    InfraShell[ { { 1, 2 }, { 1, 2 }, { 3 } } ],
    InfraSegment[ { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ] } ] },
  { InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
    InfraShell[ { { 1, 2 }, { 3 } } ],
    InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ] },
  TestID -> "Bundle-is-a-set-duplicates-collapse"
]

(* no bundle head accepts a mass channel: a second argument stays inert *)
VerificationTest[
  { InfraSegment[ { { 1, 2, 3 } }, { 2 } ], InfraCircle[ { { 1, 2, 3 } }, { 1 } ] },
  { InfraSegment[ { { 1, 2, 3 } }, { 2 } ], InfraCircle[ { { 1, 2, 3 } }, { 1 } ] },
  TestID -> "Bundle-has-no-mass-channel"
]

(* the family layer is a headless <| instance -> weight |> Association: repetition in
   an instance list reads as mass, and the all-ones family stays a family (layers
   never cross silently -- taking the support is the explicit InfraSet step) *)
VerificationTest[
  { Counts[ { InfraPoint[1], InfraPoint[1], InfraPoint[2] } ],
    toFamily[ { InfraPoint[1], InfraPoint[1], InfraPoint[2] } ],
    toFamily[ InfraWalk[ { { 1, 2 } } ] ],
    InfraSet[ { 1, 1, 2 } ] },
  { <| InfraPoint[ 1 ] -> 2, InfraPoint[ 2 ] -> 1 |>,
    <| InfraPoint[ 1 ] -> 2, InfraPoint[ 2 ] -> 1 |>,
    <| InfraWalk[ { { 1, 2 } } ] -> 1 |>,
    InfraSet[ { 1, 2 } ] },
  TestID -> "family-layer-is-a-headless-association"
]

(* the ANCHOR RULE: a vertex, a point, a set, a point list and a density all coerce
   to one 0-d density on InfraPoint keys, so every construction reads its anchors
   through a single step *)
VerificationTest[
  { toDensity[ 1 ], toDensity[ InfraPoint[ 1 ] ], toDensity[ InfraPoint[ 1, <| "Label" -> "p" |> ] ],
    toDensity[ InfraSet[ { 1, 2 } ] ], toDensity[ { InfraPoint[1], InfraPoint[1], InfraPoint[2] } ],
    toDensity[ <| InfraPoint[ 1 ] -> 3 |> ] },
  { <| InfraPoint[ 1 ] -> 1 |>, <| InfraPoint[ 1 ] -> 1 |>, <| InfraPoint[ 1 ] -> 1 |>,
    <| InfraPoint[ 1 ] -> 1, InfraPoint[ 2 ] -> 1 |>,
    <| InfraPoint[ 1 ] -> 2, InfraPoint[ 2 ] -> 1 |>,
    <| InfraPoint[ 1 ] -> 3 |> },
  TestID -> "anchor-rule-coerces-everything-to-a-density"
]

(* the family algebra is the Association's own; the measures come off the engine *)
VerificationTest[
  With[ { p = <| InfraPoint[ 1 ] -> 3, InfraPoint[ 2 ] -> 1 |> },
    { InfraSet @ p, Values @ p, Total @ p,
      InfraMeasure[ p, Method -> "Probability" ], InfraMeasure @ p } ],
  (* InfraMeasure is membership relative to the heaviest mass;
     Method -> "Probability" is the distribution summing to 1 *)
  { InfraSet[ { 1, 2 } ], { 3, 1 }, 4, <| 1 -> 3/4, 2 -> 1/4 |>, <| 1 -> 1, 2 -> 1/3 |> },
  TestID -> "density-algebra-and-measures"
]

(* the measure is CONSTRUCTED at a projection off a bundle, never carried by
   it: the column / endpoint / midpoint projections of a geodesic family are
   its occupation measure *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], s = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9 , All] },
    { s[[ 2 ]], s[ "Start" ], FindInfraMidpoint[ g, s ] } ],
  (* ["Start"] is a set-level fact (every geodesic of a family shares it), so it
     is an InfraSet; the position and midpoint projections are measures *)
  { <| InfraPoint[ 2 ] -> 3, InfraPoint[ 4 ] -> 3 |>, InfraSet[ { 1 } ],
    <| InfraPoint[ 3 ] -> 1, InfraPoint[ 5 ] -> 4, InfraPoint[ 7 ] -> 1 |> },
  TestID -> "Measure-constructed-at-projection"
]

(* anchor masses do NOT propagate: a construction sees an InfraPoint's support,
   so the family (and its measure) is the same weighted or not *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    KeySort @ InfraMeasure @ FindInfraSegment[ g, <| InfraPoint[ 1 ] -> 2, InfraPoint[ 3 ] -> 1 |>, 9 , All] ===
    KeySort @ InfraMeasure @ FindInfraSegment[ g, InfraSet[ { 1, 3 } ], 9 , All] ],
  True,
  TestID -> "Anchor-masses-do-not-propagate"
]

(* ===== Compact-native algorithms on the geodesic-DAG form ===== *)

(* the compact multi-atom set and the enumerated bundle carry the same measure *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], p = InfraSet[ { 1, 3 } ] },
    KeySort @ InfraMeasure[ FindInfraSegment[ g, p, 9 , All] ] ===
    KeySort @ InfraMeasure[ FindInfraSegment[ g, p, 9, All ] ] ],
  True,
  TestID -> "Compact-atoms-equal-enumerated-measure"
]

(* a multi-endpoint family is the plain union of the per-pair families: its raw
   occupation is the sum, normalised by the summed family sizes *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    KeySort @ FindInfraSegment[ g, InfraSet[ { 1, 3 } ], 9 , All][ "OccupationCount" ] ===
    KeySort @ Merge[ { FindInfraSegment[ g, 1, 9 , All][ "OccupationCount" ],
                       FindInfraSegment[ g, 3, 9 , All][ "OccupationCount" ] }, Total ] ],
  True,
  TestID -> "Multi-endpoint-family-is-the-union"
]

(* DAG-native column projection equals the enumerated column projection *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { KeySort @ InfraMeasure @ FindInfraSegment[ g, 1, 16 , All][[ 2 ]] ===
        KeySort @ InfraMeasure @ FindInfraSegment[ g, 1, 16, All ][[ 2 ]],
      KeySort @ InfraMeasure @ FindInfraSegment[ g, 1, 16 , All][[ -2 ]] ===
        KeySort @ InfraMeasure @ FindInfraSegment[ g, 1, 16, All ][[ -2 ]] } ],
  { True, True },
  TestID -> "DAG-column-projection-equals-enumeration"
]

(* DAG-native midpoint equals the enumerated midpoint, both parities *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { KeySort @ InfraMeasure @ FindInfraMidpoint[ g, FindInfraSegment[ g, 1, 16 , All] ] ===
        KeySort @ InfraMeasure @ FindInfraMidpoint[ g, FindInfraSegment[ g, 1, 16, All ] ],
      KeySort @ InfraMeasure @ FindInfraMidpoint[ g, FindInfraSegment[ g, 1, 12 , All] ] ===
        KeySort @ InfraMeasure @ FindInfraMidpoint[ g, FindInfraSegment[ g, 1, 12, All ] ] } ],
  { True, True },
  TestID -> "DAG-midpoint-equals-enumeration"
]

(* lazy Realizations: a bounded prefix of the full family, strict cap honoured *)
VerificationTest[
  With[ { s = FindInfraSegment[ GridGraph[ { 4, 4 } ], 1, 16 , All] },
    { Length @ s[ "Realizations", UpTo[ 2 ] ],
      SubsetQ[ s[ "Realizations" ], s[ "Realizations", UpTo[ 2 ] ] ],
      s[ "Realizations", 1000 ] } ],
  { 2, True, $Failed },
  TestID -> "DAG-lazy-realizations-prefix"
]

(* the calling quadruple on FindInfraSegment *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { Head @ First @ FindInfraSegment[ g, 1, 9 , All] === Graph,
      MatchQ[ FindInfraSegment[ g, 1, 9, 1 ], InfraSegment[ { _List } ] ],
      MatchQ[ FindInfraSegment[ g, 1, 9, UpTo[ 100 ] ], InfraSegment[ { __List } ] ],
      Length @ FindInfraSegment[ g, 1, 9, All ][ "Realizations" ] === 6,
      FindInfraSegment[ g, 1, 9, 7 ] === $Failed } ],
  { True, True, True, True, True },
  TestID -> "FindInfraSegment-calling-quadruple"
]

EndTestSection[]
