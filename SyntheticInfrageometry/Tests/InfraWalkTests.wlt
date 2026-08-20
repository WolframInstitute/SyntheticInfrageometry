BeginTestSection["InfraWalk"]

(* ===================== InfraWalk wrapper ===================== *)

VerificationTest[
  InfraWalk[ { InfraWalk[ { { 1, 2, 3 } } ], InfraWalk[ { { 1, 3 } } ] } ],
  InfraWalk[ { { 1, 2, 3 }, { 1, 3 } } ],
  TestID -> "InfraWalk-auto-flatten"
]

VerificationTest[
  InfraWalk[ { { 1, 2, 3 } } ],
  InfraWalk[ { { 1, 2, 3 } } ],
  TestID -> "InfraWalk-singleton-unchanged"
]

(* ===================== FindInfraWalk ===================== *)

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5 ],
  InfraWalk[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraWalk-default-single-path"
]

VerificationTest[
  Length @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ][ "Realizations" ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
  TestID -> "FindInfraWalk-All-matches-Wolfram-FindPath"
]

VerificationTest[
  MatchQ[
    FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
    InfraWalk[ { __List } ] ],
  True,
  TestID -> "FindInfraWalk-output-shape"
]

VerificationTest[
  AllTrue[
    FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ][ "Realizations" ],
    p |-> InfraWalkQ[ GridGraph[ { 3, 3 } ], p ] ],
  True,
  TestID -> "FindInfraWalk-all-paths-pass-InfraWalkQ"
]

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, { 4 }, 1 ],
  InfraWalk[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraWalk-exact-length-spec"
]

(* honest Properties -> {} also counts non-simple walks in the range, so this
   compares like for like against FindPath's simple-paths-only count *)
VerificationTest[
  Length @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All,
    Properties -> { "Simple" } ][ "Realizations" ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  TestID -> "FindInfraWalk-range-length-spec"
]

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, UpTo[ 10 ] ],
  InfraWalk[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraWalk-UpTo-no-failure"
]

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, 7 ],
  $Failed,
  TestID -> "FindInfraWalk-strict-shortfall-Failed"
]

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 3, 3 ],
  InfraWalk[ { } ],
  TestID -> "FindInfraWalk-degenerate-same-endpoints"
]

(* ===================== Multi-anchor spread ===================== *)

VerificationTest[
  Sort[ #[[ { 1, -1 } ]] & /@
    FindInfraWalk[ PathGraph[ Range[ 5 ] ], InfraSet[ { 1, 2 } ], 5, Infinity, All ][ "Realizations" ] ],
  Sort[ { { 1, 5 }, { 2, 5 } } ],
  TestID -> "FindInfraWalk-multi-source-spread"
]

(* ===================== InfraGeodesicQ: the scale ladder ===================== *)

(* The ladder is exact at both ends: scale 1 is the walk class, scale Infinity
   the segment class. *)

VerificationTest[
  With[ { g = CycleGraph[ 6 ], w = { 1, 2, 3, 4, 5, 6, 1 } },
    { InfraGeodesicQ[ g, w, 1 ], InfraWalkQ[ g, w ],
      InfraGeodesicQ[ g, { 1, 2, 3, 4 }, Infinity ], InfraSegmentQ[ g, { 1, 2, 3, 4 } ] }
  ],
  { True, True, True, True },
  TestID -> "InfraGeodesicQ-ladder-ends"
]

(* A walk that winds all the way around C6 is minimizing at every scale <= 3
   (each window is a shortest path) and fails beyond it. *)
VerificationTest[
  InfraGeodesicQ[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6, 1 }, # ] & /@ { 1, 2, 3, 4, Infinity },
  { True, True, True, False, False },
  TestID -> "InfraGeodesicQ-winding-walk-scale-threshold"
]

(* Doubling back fails already at scale 2: d(1, 1) == 0, not 2. *)
VerificationTest[
  { InfraGeodesicQ[ PathGraph[ Range[ 5 ] ], { 1, 2, 1 }, 1 ],
    InfraGeodesicQ[ PathGraph[ Range[ 5 ] ], { 1, 2, 1 }, 2 ] },
  { True, False },
  TestID -> "InfraGeodesicQ-backtrack-fails-at-scale-2"
]

(* A non-walk (non-adjacent consecutive vertices) is no geodesic at any scale. *)
VerificationTest[
  InfraGeodesicQ[ PathGraph[ Range[ 5 ] ], { 1, 3, 5 }, Infinity ],
  False,
  TestID -> "InfraGeodesicQ-non-walk-rejected"
]


(* ===================== FindInfraGeodesic ===================== *)

(* Scale Infinity with the default "Minimizing" rule is exactly the segment class. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All ][ "Realizations" ] ===
      Sort @ FindInfraSegment[ g, 1, 9, All ][ "Realizations" ]
  ],
  True,
  TestID -> "FindInfraGeodesic-scale-Infinity-is-the-segment-class"
]

(* Finder and predicate agree: every realisation is a geodesic at the scale asked for. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { walks = FindInfraGeodesic[ g, 1, 16, 2, Infinity, All,
              Properties -> { "Simple", "Minimizing" } ][ "Realizations" ] },
      walks =!= { } && AllTrue[ walks, w |-> InfraGeodesicQ[ g, w, 2 ] ]
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-realisations-pass-InfraGeodesicQ"
]

(* Scale-2 minimizing simple walks on C6 are the two geodesics: a shorter local
   window cannot be met by winding the long way round. *)
VerificationTest[
  Sort @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 2, Infinity, All,
    Properties -> { "Simple", "Minimizing" } ][ "Realizations" ],
  Sort[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  TestID -> "FindInfraGeodesic-scale-2-cycle-geodesics"
]

(* "Straightest" is a selector, so it can only refine: its output is a subset of
   the class it selects from. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    SubsetQ[
      Sort @ FindInfraSegment[ g, 1, 9, All ][ "Realizations" ],
      Sort @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All,
        Properties -> { "Minimizing", "Straightest" } ][ "Realizations" ] ]
  ],
  True,
  TestID -> "FindInfraGeodesic-selector-refines-the-class"
]

(* A constant score discriminates nothing, so the selector is vacuous. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All,
      Properties -> { "Minimizing", { "Maximal", 1 & } } ][ "Realizations" ] ===
      Sort @ FindInfraSegment[ g, 1, 9, All ][ "Realizations" ]
  ],
  True,
  TestID -> "FindInfraGeodesic-constant-selector-is-vacuous"
]

(* Minimising the degree sum keeps the two boundary geodesics of the grid: the
   interior vertex 5 costs more than any boundary step. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All,
      Properties -> { "Minimizing",
        { "Minimal", w |-> VertexDegree[ g, w[[ -2 ]] ] + VertexDegree[ g, w[[ -1 ]] ] } } ][ "Realizations" ]
  ],
  Sort[ { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 } } ],
  TestID -> "FindInfraGeodesic-Minimal-degree-sum-hugs-the-boundary"
]

(* At scale 1 the window is an edge -- the last vertex and the candidate -- so an
   edge function ports as w |-> f @@ w.  A law that only accepts two-vertex
   windows keeps the whole walk class exactly when that is so. *)
VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    Sort @ FindInfraGeodesic[ g, 1, 4, 1, { 3 }, All,
      Properties -> { w |-> Length[ w ] == 2 } ][ "Realizations" ] ===
      Sort @ FindInfraWalk[ g, 1, 4, { 3 }, All ][ "Realizations" ]
  ],
  True,
  TestID -> "FindInfraGeodesic-scale-1-window-is-an-edge"
]

(* A bare predicate is a custom local law; True keeps the whole class. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ FindInfraGeodesic[ g, 1, 9, 1, { 4 }, All, Properties -> { "Simple", True & } ][ "Realizations" ] ===
      Sort @ FindInfraWalk[ g, 1, 9, { 4 }, All, Properties -> { "Simple" } ][ "Realizations" ]
  ],
  True,
  TestID -> "FindInfraGeodesic-bare-predicate-True-keeps-the-class"
]

(* "Straightest" on the square with a chord: the pull-apart walks are the two
   two-step walks, not the chord route. *)
VerificationTest[
  Sort @ FindInfraGeodesic[ Graph[ { 1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4 } ], 1, 3, 2, Infinity, All,
    Properties -> { "Simple", "Straightest" } ][ "Realizations" ],
  Sort[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
  TestID -> "FindInfraGeodesic-Straightest-pull-apart"
]

(* kspec bounds the sweep depth, and an exact-length spec is honoured. *)
VerificationTest[
  With[ { walks = FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 1, { 5 }, All ][ "Realizations" ] },
    walks =!= { } && DeleteDuplicates[ Length[ # ] - 1 & /@ walks ] === { 5 }
  ],
  True,
  TestID -> "FindInfraGeodesic-kspec-bounds-the-sweep"
]

(* With revisits allowed a local rule alone leaves an infinite class -- refused,
   not silently truncated. *)
VerificationTest[
  FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 2, Infinity, All ],
  $Failed,
  { FindInfraGeodesic::unbounded },
  TestID -> "FindInfraGeodesic-unbounded-class-refused"
]

VerificationTest[
  FindInfraGeodesic[ GridGraph[ { 3, 3 } ], 1, 9, 2, 4, 1, Properties -> { "Bogus" } ],
  $Failed,
  { FindInfraGeodesic::badproperty },
  TestID -> "FindInfraGeodesic-badproperty-message"
]

VerificationTest[
  FindInfraGeodesic[ GridGraph[ { 3, 3 } ], 1, 9, 2, 4, 1, Method -> "Unknown" ],
  $Failed,
  { FindInfraGeodesic::badmethod },
  TestID -> "FindInfraGeodesic-badmethod-message"
]

(* Greedy is deterministic; GreedyRandomPick varies with the ambient seed. *)
VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ],
          f = w |-> VertexDegree[ GridGraph[ { 6, 6 } ], w[[ -2 ]] ] +
                    VertexDegree[ GridGraph[ { 6, 6 } ], w[[ -1 ]] ] },
    FindInfraGeodesic[ g, 1, 36, Infinity, Infinity, 1,
      Properties -> { "Minimizing", { "Minimal", f } }, Method -> "Greedy" ] ===
      FindInfraGeodesic[ g, 1, 36, Infinity, Infinity, 1,
        Properties -> { "Minimizing", { "Minimal", f } }, Method -> "Greedy" ]
  ],
  True,
  TestID -> "FindInfraGeodesic-Greedy-deterministic"
]

VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ],
          f = w |-> VertexDegree[ GridGraph[ { 6, 6 } ], w[[ -2 ]] ] +
                    VertexDegree[ GridGraph[ { 6, 6 } ], w[[ -1 ]] ] },
    Length @ DeleteDuplicates @ Table[
      BlockRandom[
        FindInfraGeodesic[ g, 1, 36, Infinity, Infinity, 1,
          Properties -> { "Minimizing", { "Minimal", f } }, Method -> "GreedyRandomPick" ][ "First" ],
        RandomSeeding -> s ],
      { s, 1, 8 } ]
  ],
  _Integer?( # > 1 & ),
  SameTest -> MatchQ,
  TestID -> "FindInfraGeodesic-GreedyRandomPick-varies-across-seeds"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    BlockRandom[
      Length @ FindInfraGeodesic[ g, 1, 16, Infinity, Infinity, All,
        Properties -> { "Minimizing", "Straightest" },
        Method -> { "Exhaustive", "Pruning" -> 1 } ][ "Realizations" ],
      RandomSeeding -> 42 ]
  ],
  1,
  TestID -> "FindInfraGeodesic-Pruning-beam-1"
]


(* ===================== ExtendInfraWalk ===================== *)

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Direction" -> "Forward", "Length" -> 1,
    Properties -> {"Simple", "Minimizing"} ],
  InfraWalk[ { { 3, 4, 5 } } ],
  TestID -> "ExtendInfraWalk-PathGraph-forward"
]

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Direction" -> "Backward", "Length" -> 2,
    Properties -> {"Simple", "Minimizing"} ],
  InfraWalk[ { { 1, 2, 3, 4 } } ],
  TestID -> "ExtendInfraWalk-PathGraph-backward"
]

VerificationTest[
  Sort @ ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3 }, All,
      Properties -> {"Simple", "Minimizing"} ][ "Realizations" ],
  Sort[ { { 1, 2, 3, 4, 5 }, { 5, 4, 3, 2, 1 } } ],
  TestID -> "ExtendInfraWalk-PathGraph-both-automatic"
]

VerificationTest[
  Sort @ ExtendInfraWalk[ CycleGraph[ 6 ], FindInfraSegment[ CycleGraph[ 6 ], 1, 4 ], All,
      "Length" -> 0, Properties -> {"Simple", "Minimizing"} ][ "Realizations" ],
  Sort[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  TestID -> "ExtendInfraWalk-DAG-segment-spread"
]

VerificationTest[
  Sort @ ExtendInfraWalk[ CycleGraph[ 6 ], { 1 }, All,
      "Direction" -> "Forward", "Length" -> 2,
      Properties -> {"Simple", "Straightest"} ][ "Realizations" ],
  Sort[ { { 1, 2, 3 }, { 1, 6, 5 } } ],
  TestID -> "ExtendInfraWalk-CycleGraph-LongestPath-sum"
]

VerificationTest[
  AllTrue[
    ExtendInfraWalk[ GridGraph[ { 3, 3 } ], { 1 }, All,
      "Direction" -> "Forward", "Length" -> 3, Properties -> { "Simple" } ][ "Realizations" ],
    p |-> InfraWalkQ[ GridGraph[ { 3, 3 } ], p ] ],
  True,
  TestID -> "ExtendInfraWalk-all-extensions-pass-InfraWalkQ"
]

VerificationTest[
  MatchQ[
    ExtendInfraWalk[ GridGraph[ { 3, 3 } ], { 1 }, All,
      "Direction" -> "BothSides", "Length" -> 2 ],
    InfraWalk[ { __List } ] ],
  True,
  TestID -> "ExtendInfraWalk-output-shape"
]

VerificationTest[
  Length @
    ExtendInfraWalk[ PathGraph[ Range[ 7 ] ], { 4, 5 }, 1,
      "Direction" -> "Forward", "Length" -> 2,
      Properties -> {"Simple", "Minimizing"} ][ "Realizations" ][[ 1 ]],
  4,
  TestID -> "ExtendInfraWalk-Length-truncation"
]

(* Multi-realisation input: each realisation is extended *)
VerificationTest[
  Sort @ ExtendInfraWalk[ PathGraph[ Range[ 7 ] ],
      InfraWalk[ { { 3 }, { 5 } } ], All,
      "Direction" -> "Forward", "Length" -> 1,
      Properties -> {"Simple", "Minimizing"} ][ "Realizations" ],
  Sort[ { { 3, 2 }, { 3, 4 }, { 5, 4 }, { 5, 6 } } ],
  TestID -> "ExtendInfraWalk-multi-realisation"
]

(* Dead-end freeze: forward extension of the right endpoint freezes *)
VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 4, 5 }, 1,
    "Direction" -> "Forward", "Length" -> 5,
    Properties -> {"Simple", "Minimizing"} ],
  InfraWalk[ { { 4, 5 } } ],
  TestID -> "ExtendInfraWalk-dead-end-freeze"
]

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 1,
    Properties -> {"Simple", "Minimizing"} ],
  InfraWalk[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "ExtendInfraWalk-BothSides-extends-segment-to-line"
]

(* Per-step symmetric stepping: from oriented seed {3, 4} on PathGraph[5]
   with Length -> 1, "BothSides" grows by exactly +1 edge on each side:
   {2, 3, 4, 5}.  The old asymmetric Cartesian would also have produced
   length-1 single-side walks ({3, 4, 5}, {2, 3, 4}). *)
VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Length" -> 1, "Direction" -> "BothSides",
    Properties -> {"Simple", "Minimizing"} ],
  InfraWalk[ { { 2, 3, 4, 5 } } ],
  TestID -> "ExtendInfraWalk-BothSides-symmetric-one-step"
]

(* Asymmetric tail: from {4, 5} on PathGraph[5] with "BothSides", forward
   freezes immediately (5 has no Simple+ShortestPath neighbor past it);
   backward keeps growing one edge per step until it reaches vertex 1.
   Outer-step budget Length -> 5 covers the full extension. *)
VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 4, 5 }, 1,
    "Length" -> 5, "Direction" -> "BothSides",
    Properties -> {"Simple", "Minimizing"} ],
  InfraWalk[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "ExtendInfraWalk-BothSides-asymmetric-tail"
]

(* Unknown direction -> $Failed with ::baddirection *)
VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3 }, 1,
    "Direction" -> "Sideways", Properties -> {"Simple"} ],
  $Failed,
  {ExtendInfraWalk::baddirection},
  TestID -> "ExtendInfraWalk-baddirection"
]

(* count > available: $Failed *)
VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3 }, 99,
    Properties -> {"Simple", "Minimizing"} ],
  $Failed,
  TestID -> "ExtendInfraWalk-strict-shortfall-Failed"
]


(* ===================== InfraWalk scene-DSL constructor ===================== *)

(* Bare-vertex chain on P5: 1-2-3 is a valid walk. *)
VerificationTest[
  With[{
    scene = InfraScene[ { path }, { path == InfraWalk[ 1, 2, 3 ] } ],
    g = PathGraph[ Range[ 5 ] ]
  },
    With[{ instances = FindInfraScene[ scene, g ] },
      Length[ instances ] == 1 && instances[[ 1 ]][[ 1 ]][ path ] === { 1, 2, 3 }
    ]
  ],
  True,
  TestID -> "InfraWalk-scene-DSL-bare-chain"
]

(* No edge between 1 and 3 on P5 => empty result. *)
VerificationTest[
  With[{
    scene = InfraScene[ { path }, { path == InfraWalk[ 1, 3 ] } ],
    g = PathGraph[ Range[ 5 ] ]
  },
    FindInfraScene[ scene, g ]
  ],
  { },
  TestID -> "InfraWalk-scene-DSL-no-edge-empty"
]

(* Non-simple chain 1-2-1 on P3 is kept (no DuplicateFreeQ filter). *)
VerificationTest[
  With[{
    scene = InfraScene[ { path }, { path == InfraWalk[ 1, 2, 1 ] } ],
    g = PathGraph[ Range[ 3 ] ]
  },
    With[{ instances = FindInfraScene[ scene, g ] },
      Length[ instances ] == 1 && instances[[ 1 ]][[ 1 ]][ path ] === { 1, 2, 1 }
    ]
  ],
  True,
  TestID -> "InfraWalk-scene-DSL-non-simple-kept"
]

(* endpoint accessors keep multiplicity: the end-vertex multiset is the
   occupation measure of where the walks terminate. *)
VerificationTest[
  With[ { reps = { { 1, 2, 4 }, { 5, 6, 4 }, { 7, 8, 9 } } },
    KeySort @ InfraWalk[ reps ][ "End" ][ "OccupationCount" ] === KeySort @ Counts[ Last /@ reps ] &&
    KeySort @ InfraWalk[ reps ][ "Start" ][ "OccupationCount" ] === KeySort @ Counts[ First /@ reps ]
  ],
  True,
  TestID -> "InfraWalk-endpoint-accessors-keep-multiplicity"
]

(* kspec bounds the walk-space sweep itself: with revisits allowed the walk space
   is infinite past kmax, so the enumeration must terminate. *)
VerificationTest[
  With[ { walks = FindInfraWalk[ CycleGraph[ 6 ], 1, 4, { 5 }, All ][ "Realizations" ] },
    walks =!= { } && AllTrue[ walks,
      w |-> Length[ w ] - 1 == 5 && First[ w ] == 1 && Last[ w ] == 4 &&
        AllTrue[ Partition[ w, 2, 1 ], Apply[ EdgeQ[ CycleGraph[ 6 ], UndirectedEdge[ #1, #2 ] ] & ] ] ]
  ],
  True,
  TestID -> "FindInfraWalk-property-sweep-depth-bounded"
]

(* one length-unconstrained walk is the canonical witness: a geodesic *)
VerificationTest[
  FindInfraWalk[ GridGraph[ { 4, 4 } ], 1, 16, Infinity, 1 ][ "Length" ],
  { GraphDistance[ GridGraph[ { 4, 4 } ], 1, 16 ] },
  TestID -> "FindInfraWalk-Infinity-count1-geodesic"
]

(* bare k means at most k on both the fast path and the property path *)
VerificationTest[
  Sort @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 4, All,
    Properties -> { "Simple" } ][ "Realizations" ],
  Sort @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ][ "Realizations" ],
  TestID -> "FindInfraWalk-bare-k-at-most-both-paths"
]

(* a lower length bound must not be starved by the early-stop count *)
VerificationTest[
  Length @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 6 }, 2,
    Properties -> { "Simple" } ][ "Realizations" ],
  2,
  TestID -> "FindInfraWalk-exact-length-strict-count"
]

(* Method -> "Greedy": lazy DFS, one instance; unconstrained length falls back
   to the canonical geodesic witness (safe against an unbounded descent). *)
VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, 1, Method -> "Greedy" ],
  InfraWalk[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraWalk-Greedy-unconstrained-canonical-witness"
]

(* Greedy on a bounded kspec, no backtracking: picking the first (lowest-index)
   candidate at each step happens to always point toward vertex 1 on a
   PathGraph, so descending from the high end succeeds. *)
VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 5, 1, 4, 1, Method -> "Greedy" ],
  InfraWalk[ { { 5, 4, 3, 2, 1 } } ],
  TestID -> "FindInfraWalk-Greedy-bounded-succeeds"
]

EndTestSection[]
