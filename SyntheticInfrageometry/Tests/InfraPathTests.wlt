BeginTestSection["InfraPath"]

(* ===================== InfraPath wrapper ===================== *)

VerificationTest[
  InfraPath[ { InfraPath[ { { 1, 2, 3 } } ], InfraPath[ { { 1, 3 } } ] } ],
  InfraPath[ { { 1, 2, 3 }, { 1, 3 } } ],
  TestID -> "InfraPath-auto-flatten"
]

VerificationTest[
  InfraPath[ { { 1, 2, 3 } } ],
  InfraPath[ { { 1, 2, 3 } } ],
  TestID -> "InfraPath-singleton-unchanged"
]

(* ===================== FindInfraPath ===================== *)

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5 ],
  InfraPath[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraPath-default-single-path"
]

VerificationTest[
  Length @ FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ][ "Realizations" ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
  TestID -> "FindInfraPath-All-matches-Wolfram-FindPath"
]

VerificationTest[
  MatchQ[
    FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
    InfraPath[ { __List } ] ],
  True,
  TestID -> "FindInfraPath-output-shape"
]

VerificationTest[
  AllTrue[
    FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ][ "Realizations" ],
    p |-> InfraPathQ[ GridGraph[ { 3, 3 } ], p ] ],
  True,
  TestID -> "FindInfraPath-all-paths-pass-InfraPathQ"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5, { 4 }, 1 ],
  InfraPath[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraPath-exact-length-spec"
]

VerificationTest[
  Length @ FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ][ "Realizations" ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  TestID -> "FindInfraPath-range-length-spec"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, UpTo[ 10 ] ],
  InfraPath[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "FindInfraPath-UpTo-no-failure"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, 7 ],
  $Failed,
  TestID -> "FindInfraPath-strict-shortfall-Failed"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 3, 3 ],
  InfraPath[ { } ],
  TestID -> "FindInfraPath-degenerate-same-endpoints"
]

(* ===================== Multi-anchor spread ===================== *)

VerificationTest[
  Sort[ #[[ { 1, -1 } ]] & /@
    FindInfraPath[ PathGraph[ Range[ 5 ] ], InfraPoint[ { 1, 2 } ], 5, Infinity, All ][ "Realizations" ] ],
  Sort[ { { 1, 5 }, { 2, 5 } } ],
  TestID -> "FindInfraPath-multi-source-spread"
]

(* ===================== ExtendInfraPath ===================== *)

VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Direction" -> "Forward", "Length" -> 1,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraPath[ { { 3, 4, 5 } } ],
  TestID -> "ExtendInfraPath-PathGraph-forward"
]

VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Direction" -> "Backward", "Length" -> 2,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraPath[ { { 1, 2, 3, 4 } } ],
  TestID -> "ExtendInfraPath-PathGraph-backward"
]

VerificationTest[
  Sort @ ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 3 }, All,
      Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ],
  Sort[ { { 1, 2, 3, 4, 5 }, { 5, 4, 3, 2, 1 } } ],
  TestID -> "ExtendInfraPath-PathGraph-both-automatic"
]

VerificationTest[
  Sort @ ExtendInfraPath[ CycleGraph[ 6 ], { 1 }, All,
      "Direction" -> "Forward", "Length" -> 2,
      Properties -> {"Simple", {"LongestPath", "Aggregation" -> "Sum"}} ][ "Realizations" ],
  Sort[ { { 1, 2, 3 }, { 1, 6, 5 } } ],
  TestID -> "ExtendInfraPath-CycleGraph-LongestPath-sum"
]

VerificationTest[
  AllTrue[
    ExtendInfraPath[ GridGraph[ { 3, 3 } ], { 1 }, All,
      "Direction" -> "Forward", "Length" -> 3, Properties -> { "Simple" } ][ "Realizations" ],
    p |-> InfraPathQ[ GridGraph[ { 3, 3 } ], p ] ],
  True,
  TestID -> "ExtendInfraPath-all-extensions-pass-InfraPathQ"
]

VerificationTest[
  MatchQ[
    ExtendInfraPath[ GridGraph[ { 3, 3 } ], { 1 }, All,
      "Direction" -> "BothSides", "Length" -> 2 ],
    InfraPath[ { __List } ] ],
  True,
  TestID -> "ExtendInfraPath-output-shape"
]

VerificationTest[
  Length @
    ExtendInfraPath[ PathGraph[ Range[ 7 ] ], { 4, 5 }, 1,
      "Direction" -> "Forward", "Length" -> 2,
      Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ][[ 1 ]],
  4,
  TestID -> "ExtendInfraPath-Length-truncation"
]

(* Multi-realisation input: each realisation is extended *)
VerificationTest[
  Sort @ ExtendInfraPath[ PathGraph[ Range[ 7 ] ],
      InfraPath[ { { 3 }, { 5 } } ], All,
      "Direction" -> "Forward", "Length" -> 1,
      Properties -> {"Simple", "ShortestPath"} ][ "Realizations" ],
  Sort[ { { 3, 2 }, { 3, 4 }, { 5, 4 }, { 5, 6 } } ],
  TestID -> "ExtendInfraPath-multi-realisation"
]

(* Dead-end freeze: forward extension of the right endpoint freezes *)
VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 4, 5 }, 1,
    "Direction" -> "Forward", "Length" -> 5,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraPath[ { { 4, 5 } } ],
  TestID -> "ExtendInfraPath-dead-end-freeze"
]

VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 2, 3 }, 1,
    Properties -> {"Simple", "ShortestPath"} ],
  InfraPath[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "ExtendInfraPath-BothSides-extends-segment-to-line"
]

(* Per-step symmetric stepping: from oriented seed {3, 4} on PathGraph[5]
   with Length -> 1, "BothSides" grows by exactly +1 edge on each side:
   {2, 3, 4, 5}.  The old asymmetric Cartesian would also have produced
   length-1 single-side walks ({3, 4, 5}, {2, 3, 4}). *)
VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 3, 4 }, 1,
    "Length" -> 1, "Direction" -> "BothSides",
    Properties -> {"Simple", "ShortestPath"} ],
  InfraPath[ { { 2, 3, 4, 5 } } ],
  TestID -> "ExtendInfraPath-BothSides-symmetric-one-step"
]

(* Asymmetric tail: from {4, 5} on PathGraph[5] with "BothSides", forward
   freezes immediately (5 has no Simple+ShortestPath neighbor past it);
   backward keeps growing one edge per step until it reaches vertex 1.
   Outer-step budget Length -> 5 covers the full extension. *)
VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 4, 5 }, 1,
    "Length" -> 5, "Direction" -> "BothSides",
    Properties -> {"Simple", "ShortestPath"} ],
  InfraPath[ { { 1, 2, 3, 4, 5 } } ],
  TestID -> "ExtendInfraPath-BothSides-asymmetric-tail"
]

(* Unknown direction -> $Failed with ::baddirection *)
VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 3 }, 1,
    "Direction" -> "Sideways", Properties -> {"Simple"} ],
  $Failed,
  {ExtendInfraPath::baddirection},
  TestID -> "ExtendInfraPath-baddirection"
]

(* count > available: $Failed *)
VerificationTest[
  ExtendInfraPath[ PathGraph[ Range[ 5 ] ], { 3 }, 99,
    Properties -> {"Simple", "ShortestPath"} ],
  $Failed,
  TestID -> "ExtendInfraPath-strict-shortfall-Failed"
]


(* ===================== InfraPath scene-DSL constructor ===================== *)

(* Bare-vertex chain on P5: 1-2-3 is a valid walk. *)
VerificationTest[
  With[{
    scene = InfraScene[ { path }, { path == InfraPath[ 1, 2, 3 ] } ],
    g = PathGraph[ Range[ 5 ] ]
  },
    With[{ instances = FindInfraScene[ scene, g ] },
      Length[ instances ] == 1 && instances[[ 1 ]][[ 1 ]][ path ] === { 1, 2, 3 }
    ]
  ],
  True,
  TestID -> "InfraPath-scene-DSL-bare-chain"
]

(* No edge between 1 and 3 on P5 => empty result. *)
VerificationTest[
  With[{
    scene = InfraScene[ { path }, { path == InfraPath[ 1, 3 ] } ],
    g = PathGraph[ Range[ 5 ] ]
  },
    FindInfraScene[ scene, g ]
  ],
  { },
  TestID -> "InfraPath-scene-DSL-no-edge-empty"
]

(* Non-simple chain 1-2-1 on P3 is kept (no DuplicateFreeQ filter). *)
VerificationTest[
  With[{
    scene = InfraScene[ { path }, { path == InfraPath[ 1, 2, 1 ] } ],
    g = PathGraph[ Range[ 3 ] ]
  },
    With[{ instances = FindInfraScene[ scene, g ] },
      Length[ instances ] == 1 && instances[[ 1 ]][[ 1 ]][ path ] === { 1, 2, 1 }
    ]
  ],
  True,
  TestID -> "InfraPath-scene-DSL-non-simple-kept"
]

(* endpoint accessors keep multiplicity: the end-vertex multiset is the
   occupation measure of where the walks terminate. *)
VerificationTest[
  With[ { reps = { { 1, 2, 4 }, { 5, 6, 4 }, { 7, 8, 9 } } },
    KeySort @ InfraPath[ reps ][ "End" ][ "OccupationCount" ] === KeySort @ Counts[ Last /@ reps ] &&
    KeySort @ InfraPath[ reps ][ "Start" ][ "OccupationCount" ] === KeySort @ Counts[ First /@ reps ]
  ],
  True,
  TestID -> "InfraPath-endpoint-accessors-keep-multiplicity"
]

EndTestSection[]
