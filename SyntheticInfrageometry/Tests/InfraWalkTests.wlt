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

(* ===================== ExtendInfraWalk ===================== *)

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Direction" -> "Forward", "Length" -> 1,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraWalk[ { { 3, 4, 5 } } ],
  TestID -> "ExtendInfraWalk-PathGraph-forward"
]

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Direction" -> "Backward", "Length" -> 2,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraWalk[ { { 1, 2, 3, 4 } } ],
  TestID -> "ExtendInfraWalk-PathGraph-backward"
]

VerificationTest[
  Sort @ ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3 }, All,
      Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ],
  Sort[ { { 1, 2, 3, 4, 5 }, { 5, 4, 3, 2, 1 } } ],
  TestID -> "ExtendInfraWalk-PathGraph-both-automatic"
]

VerificationTest[
  Sort @ ExtendInfraWalk[ CycleGraph[ 6 ], FindInfraSegment[ CycleGraph[ 6 ], 1, 4 ], All,
      "Length" -> 0, Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ],
  Sort[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  TestID -> "ExtendInfraWalk-DAG-segment-spread"
]

VerificationTest[
  Sort @ ExtendInfraWalk[ CycleGraph[ 6 ], { 1 }, All,
      "Direction" -> "Forward", "Length" -> 2,
      Properties -> {"Simple", {"LongestPath", "Aggregation" -> "Sum"}} ][ "Realizations" ],
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
      Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ][[ 1 ]],
  4,
  TestID -> "ExtendInfraWalk-Length-truncation"
]

(* Multi-realisation input: each realisation is extended *)
VerificationTest[
  Sort @ ExtendInfraWalk[ PathGraph[ Range[ 7 ] ],
      InfraWalk[ { { 3 }, { 5 } } ], All,
      "Direction" -> "Forward", "Length" -> 1,
      Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ],
  Sort[ { { 3, 2 }, { 3, 4 }, { 5, 4 }, { 5, 6 } } ],
  TestID -> "ExtendInfraWalk-multi-realisation"
]

(* Dead-end freeze: forward extension of the right endpoint freezes *)
VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 4, 5 }, 1,
    "Direction" -> "Forward", "Length" -> 5,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraWalk[ { { 4, 5 } } ],
  TestID -> "ExtendInfraWalk-dead-end-freeze"
]

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 1,
    Properties -> {"Simple", "ShortestPath"} ],
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
    Properties -> {"Simple", "ShortestPath"} ],
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
    Properties -> {"Simple", "ShortestPath"} ],
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
    Properties -> {"Simple", "ShortestPath"} ],
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
   is infinite past kmax, so a property-path enumeration must terminate. *)
VerificationTest[
  With[ { walks = FindInfraWalk[ CycleGraph[ 6 ], 1, 4, { 5 }, All,
      Properties -> { { "EdgeMax", 1 & } } ][ "Realizations" ] },
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
