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

(* at these lengths the generic default coincides with the simple paths (a
   crossing on this grid needs length >= 8), so the counts compare like for
   like *)
VerificationTest[
  Length @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ][ "Realizations" ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  TestID -> "FindInfraWalk-range-length-spec"
]

(* "Crossings" counts arrivals at an already-visited vertex, exactly: on
   CycleGraph[4] every non-backtracking walk is a pure rotation, so the
   length-6 walks 1 -> 3 have exactly 3 crossings -- the 1-crossing class is
   empty, the 3-crossing class is the two rotations, and the generic default
   is also empty at this length (winding is a tangency) *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { FindInfraWalk[ g, 1, 3, { 6 }, All ][ "Realizations" ],
      FindInfraWalk[ g, 1, 3, { 6 }, All, "Crossings" -> 1 ][ "Realizations" ],
      Sort @ FindInfraWalk[ g, 1, 3, { 6 }, All, "Crossings" -> 3 ][ "Realizations" ] } ],
  { { }, { }, Sort[ { { 1, 2, 3, 4, 1, 2, 3 }, { 1, 4, 3, 2, 1, 4, 3 } } ] },
  TestID -> "FindInfraWalk-crossings-count-is-exact"
]

(* a 1-crossing walk self-intersects exactly once, without backtracking; drawn
   at random under the default Automatic, so the draw is seeded *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { w = BlockRandom[
        First @ FindInfraWalk[ g, 1, 9, 8, "Crossings" -> 1 ][ "Realizations" ],
        RandomSeeding -> 7 ] },
    { First[ w ] === 1 && Last[ w ] === 9 && Length[ w ] - 1 <= 8,
      Length[ w ] - Length[ DeleteDuplicates[ w ] ] === 1,
      AllTrue[ Partition[ w, 3, 1 ], #[[ 1 ]] =!= #[[ 3 ]] & ] } ],
  { True, True, True },
  TestID -> "FindInfraWalk-crossing-walk-is-a-nonbacktracking-revisit"
]

(* Method -> Automatic on a bounded count is the certified lazy descent: the
   instances are genuine, distinct members of the exhaustive class, exactly as
   many as asked *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { class = FindInfraWalk[ g, 1, 9, { 6 }, All, Method -> "Exhaustive" ][ "Realizations" ] },
    { got = FindInfraWalk[ g, 1, 9, { 6 }, 3 ][ "Realizations" ] },
    Length[ got ] === 3 && DuplicateFreeQ[ got ] && SubsetQ[ class, got ] ],
  True,
  TestID -> "FindInfraWalk-Automatic-greedy-members-of-class"
]

(* the default class is InfraGenericQ's: on the 3-by-3 grid at length <= 8 it
   strictly exceeds the simple paths (the degree-4 centre supports an isolated
   crossing), and it is exactly the census filter over the bare walk class *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { reps = FindInfraWalk[ g, 1, 9, 8, All ][ "Realizations" ] },
    AllTrue[ reps, InfraGenericQ[ g, # ] & ] &&
    AnyTrue[ reps, ! DuplicateFreeQ[ # ] & ] &&
    Sort @ reps ===
      Sort @ Select[
        FindInfraGeodesic[ g, 1, 9, 1, 8, All, Properties -> { } ][ "Realizations" ],
        InfraGenericQ[ g, # ] & ] ],
  True,
  TestID -> "FindInfraWalk-default-class-is-generic-immersed"
]

(* "Immersed" alone leaves an infinite class -- winding a long cycle never
   cusps -- so an unbounded kspec is refused *)
VerificationTest[
  FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, Infinity, 1, Properties -> { "Immersed" } ],
  $Failed,
  { FindInfraWalk::unbounded },
  TestID -> "FindInfraWalk-immersed-unbounded-refused"
]

(* Properties -> {} is the bare walk class: backtracking walks are members,
   and a walk may pass through the endpoint and return (non-terminal sweep) *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { reps = FindInfraWalk[ g, 1, 9, { 6 }, All, Properties -> { } ][ "Realizations" ] },
    AnyTrue[ reps, ! InfraImmersedQ[ g, # ] & ] &&
    AnyTrue[ reps, Count[ #, 9 ] >= 2 & ] ],
  True,
  TestID -> "FindInfraWalk-empty-properties-bare-walk-class"
]

(* an unbounded crossing class is refused: crossings exist at every excess
   length, and a geodesic witness would be simple, hence outside the class *)
VerificationTest[
  FindInfraWalk[ GridGraph[ { 4, 4 } ], 1, 16, Infinity, 1, "Crossings" -> 1 ],
  $Failed,
  { FindInfraWalk::unbounded },
  TestID -> "FindInfraWalk-unbounded-crossing-class-refused"
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
      Sort @ FindInfraWalk[ g, 1, 9, { 4 }, All ][ "Realizations" ]
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

(* "Generic" bounds the class by itself -- multiplicity <= 2 forces termination
   -- so kspec Infinity is accepted; on the 6-cycle the generic walks 1 -> 4
   are exactly the two geodesics (anything longer repeats an edge or returns
   to an endpoint). *)
VerificationTest[
  Sort @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 1, Infinity, All,
    Properties -> { "Generic" } ][ "Realizations" ],
  Sort @ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } },
  TestID -> "FindInfraGeodesic-Generic-bounds-the-class"
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

(* Greedy is deterministic; RandomGreedy varies with the ambient seed. *)
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
          Properties -> { "Minimizing", { "Minimal", f } }, Method -> "RandomGreedy" ][ "First" ],
        RandomSeeding -> s ],
      { s, 1, 8 } ]
  ],
  _Integer?( # > 1 & ),
  SameTest -> MatchQ,
  TestID -> "FindInfraGeodesic-RandomGreedy-varies-across-seeds"
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
  Sort @ ExtendInfraWalk[ CycleGraph[ 6 ], FindInfraSegment[ CycleGraph[ 6 ], 1, 4 , All], All,
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

(* kspec bounds the walk-space sweep itself: the crossing space is infinite past
   kmax, so the enumeration must terminate. *)
VerificationTest[
  With[ { walks = FindInfraWalk[ CycleGraph[ 4 ], 1, 3, { 6 }, All, "Crossings" -> 3 ][ "Realizations" ] },
    walks =!= { } && AllTrue[ walks,
      w |-> Length[ w ] - 1 == 6 && First[ w ] == 1 && Last[ w ] == 3 &&
        AllTrue[ Partition[ w, 2, 1 ], Apply[ EdgeQ[ CycleGraph[ 4 ], UndirectedEdge[ #1, #2 ] ] & ] ] ]
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

(* bare k means at most k on the frontier sweep too: same class as the explicit
   {0, k} range *)
VerificationTest[
  Sort @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 8, All,
    "Crossings" -> 1 ][ "Realizations" ],
  Sort @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 0, 8 }, All,
    "Crossings" -> 1 ][ "Realizations" ],
  TestID -> "FindInfraWalk-bare-k-at-most-both-paths"
]

(* a lower length bound must not be starved by the early-stop count *)
VerificationTest[
  Length @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 8 }, 2,
    "Crossings" -> 1, Method -> "Exhaustive" ][ "Realizations" ],
  2,
  TestID -> "FindInfraWalk-exact-length-strict-count"
]

(* Method -> "Greedy": lazy DFS, one instance; the generic default bounds the
   unconstrained descent by itself. *)
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


(* ===================== The method ladder ===================== *)

(* The lazy descent is COMPLETE, so every finite count is exact: on CycleGraph[4]
   there are exactly 2 three-crossing walks 1 -> 3 of length 6 (the two
   rotations), and asking for k of them under the deterministic "Greedy" returns
   k distinct genuine ones for every k <= 2.  Asking for 3 is the honest $Failed. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { whole = FindInfraWalk[ g, 1, 3, { 6 }, All, "Crossings" -> 3 ][ "Realizations" ] },
    { AllTrue[ Range[ 1, Length @ whole ],
        k |-> With[ { got = FindInfraWalk[ g, 1, 3, { 6 }, k, "Crossings" -> 3,
              Method -> "Greedy" ][ "Realizations" ] },
          Length[ got ] === k && DuplicateFreeQ[ got ] && SubsetQ[ whole, got ] &&
          AllTrue[ got, w |-> InfraWalkQ[ g, w ] && Length[ w ] - 1 === 6 ] ] ],
      FindInfraWalk[ g, 1, 3, { 6 }, Length[ whole ] + 1, "Crossings" -> 3,
        Method -> "Greedy" ] } ],
  { True, $Failed },
  TestID -> "FindInfraWalk-Greedy-finite-count-is-exact"
]

(* Count-coupling: the count-less call is one instance -- a genuine member of the
   class, and the class itself is bigger.  This is what makes exponential
   enumeration opt-in. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { one = FindInfraWalk[ g, 1, 3, { 6 }, "Crossings" -> 3 ][ "Realizations" ] },
    { Length @ one === 1,
      InfraWalkQ[ g, First @ one ] &&
        Length[ First @ one ] - Length[ DeleteDuplicates @ First @ one ] === 3,
      Length @ FindInfraWalk[ g, 1, 3, { 6 }, All, "Crossings" -> 3 ][ "Realizations" ] > 1 } ],
  { True, True, True },
  TestID -> "FindInfraWalk-countless-is-one-instance"
]

(* A randomized descent cannot backtrack, so it can fall short of a strict count;
   the shortfall is loud ($Failed plus a message) rather than a silent under-supply. *)
VerificationTest[
  FindInfraWalk[ CycleGraph[ 4 ], 1, 3, { 6 }, 99, "Crossings" -> 3, Method -> "RandomGreedy" ],
  $Failed,
  { FindInfraWalk::shortfall },
  TestID -> "FindInfraWalk-RandomGreedy-strict-shortfall-fails-loudly"
]

(* All the same walks, whichever engine enumerates them. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    Sort @ FindInfraWalk[ g, 1, 3, { 6 }, All, "Crossings" -> 3, Method -> "Greedy" ][ "Realizations" ] ===
      Sort @ FindInfraWalk[ g, 1, 3, { 6 }, All, "Crossings" -> 3, Method -> "Exhaustive" ][ "Realizations" ] ],
  True,
  TestID -> "FindInfraWalk-Greedy-All-agrees-with-Exhaustive"
]

(* The kinked sampler: each splice pays exactly one crossing, retraces no edge,
   and respects the "LoopLength" floor -- the loop span at every doubled vertex
   is at least lmin. *)
VerificationTest[
  SeedRandom[ 7 ];
  With[ { g = GridGraph[ { 6, 6 } ] },
    { w = First @ FindInfraWalk[ g, 1, 36, { 10, 26 }, 1, "Crossings" -> 1,
        "LoopLength" -> { 8, 12 } ][ "Realizations" ] },
    { Length[ w ] - Length[ DeleteDuplicates @ w ],
      Min[ ( Last[ # ] - First[ # ] & ) @ Flatten @ Position[ w, # ] & /@
          Select[ DeleteDuplicates @ w, Count[ w, # ] > 1 & ] ] >= 8,
      Max[ Values @ Counts[ Sort /@ Partition[ w, 2, 1 ] ] ] === 1,
      AllTrue[ Partition[ w, 3, 1 ], #[[ 1 ]] =!= #[[ 3 ]] & ],
      10 <= Length[ w ] - 1 <= 26 } ],
  { 1, True, True, True, True },
  TestID -> "FindInfraWalk-LoopLength-floor-and-exact-crossing"
]

(* Automatic LoopLength: the default bounded-count draw stays in the exact class. *)
VerificationTest[
  SeedRandom[ 3 ];
  With[ { g = GridGraph[ { 6, 6 } ] },
    { w = First @ FindInfraWalk[ g, 1, 36, { 10, 24 }, 1, "Crossings" -> 2 ][ "Realizations" ] },
    { Length[ w ] - Length[ DeleteDuplicates @ w ],
      AllTrue[ Partition[ w, 3, 1 ], #[[ 1 ]] =!= #[[ 3 ]] & ],
      10 <= Length[ w ] - 1 <= 24 } ],
  { 2, True, True },
  TestID -> "FindInfraWalk-Automatic-LoopLength-exact-class"
]

(* ===================== WalkSingularities ===================== *)

(* a simple path is free of every singularity *)
VerificationTest[
  WalkSingularities[ { 1, 2, 5, 8, 9 } ],
  <| "Cusp" -> { }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
     "EndpointIncidence" -> { } |>,
  TestID -> "WalkSingularities-simple-path-empty"
]

(* the retraced arc around an apex belongs to the cusp, however deep *)
VerificationTest[
  WalkSingularities[ { 6, 5, 4, 3, 2, 3, 4, 7 } ],
  <| "Cusp" -> { 5 }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
     "EndpointIncidence" -> { } |>,
  TestID -> "WalkSingularities-deep-cusp-one-name"
]

(* a walk revisiting a tree edge: the fold is one cusp, nothing else *)
VerificationTest[
  WalkSingularities[ { 2, 1, 3, 1, 4 } ],
  <| "Cusp" -> { 3 }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
     "EndpointIncidence" -> { } |>,
  TestID -> "WalkSingularities-tree-fold-is-a-cusp"
]

(* an arc re-run in the same direction: both ranges ascend *)
VerificationTest[
  WalkSingularities[ { 1, 2, 5, 6, 3, 2, 5, 8 } ][ "Tangency" ],
  { { { 2, 3 }, { 6, 7 } } },
  TestID -> "WalkSingularities-direct-tangency"
]

(* an arc retraced in reverse: the second range descends *)
VerificationTest[
  WalkSingularities[ { 3, 2, 5, 8, 7, 4, 5, 2, 1 } ][ "Tangency" ],
  { { { 2, 3 }, { 8, 7 } } },
  TestID -> "WalkSingularities-inverse-tangency"
]

(* an isolated double visit is the crossing, the generic double point *)
VerificationTest[
  WalkSingularities[ { 2, 5, 4, 7, 8, 5, 6 } ],
  <| "Cusp" -> { }, "Tangency" -> { }, "Crossing" -> { { 2, 6 } }, "TriplePoint" -> { },
     "EndpointIncidence" -> { } |>,
  TestID -> "WalkSingularities-crossing"
]

VerificationTest[
  WalkSingularities[ { 0, 1, 2, 3, 1, 4, 5, 1, 6 } ][ "TriplePoint" ],
  { { 2, 5, 8 } },
  TestID -> "WalkSingularities-triple-point"
]

(* an endpoint of multiplicity >= 2 is an incidence, not a crossing *)
VerificationTest[
  WalkSingularities[ { 7, 4, 5, 2, 1, 4 } ],
  <| "Cusp" -> { }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
     "EndpointIncidence" -> { { 2, 6 } } |>,
  TestID -> "WalkSingularities-endpoint-incidence"
]

(* the same closed sequence: incident endpoints as an open list, clean as a loop *)
VerificationTest[
  { WalkSingularities[ { 1, 2, 3, 4, 5, 6, 1 } ][ "EndpointIncidence" ],
    WalkSingularities[ InfraLoop[ { { 1, 2, 3, 4, 5, 6, 1 } } ] ] },
  { { { 1, 7 } },
    { <| "Cusp" -> { }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
         "Cover" -> 1 |> } },
  TestID -> "WalkSingularities-loop-closes-endpoint-incidence"
]

(* wraparound applies on closed heads: the doubled edge is a two-cusp fold *)
VerificationTest[
  WalkSingularities[ InfraLoop[ { { 1, 2, 1 } } ] ],
  { <| "Cusp" -> { 1, 2 }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
       "Cover" -> 1 |> },
  TestID -> "WalkSingularities-loop-wraparound-cusps"
]

(* a doubled interval folds at both ends: two cusps, nothing else *)
VerificationTest[
  WalkSingularities[ InfraLoop[ { { 1, 2, 5, 2, 1 } } ] ][[ 1, "Cusp" ]],
  { 1, 3 },
  TestID -> "WalkSingularities-doubled-interval-two-cusps"
]

(* a periodic core is the multiply covered loop *)
VerificationTest[
  WalkSingularities[ InfraLoop[ { { 1, 2, 3, 1, 2, 3, 1 } } ] ],
  { <| "Cusp" -> { }, "Tangency" -> { }, "Crossing" -> { }, "TriplePoint" -> { },
       "Cover" -> 2 |> },
  TestID -> "WalkSingularities-multiply-covered-loop"
]

(* figure-eight based at the cut vertex of the bowtie: one crossing *)
VerificationTest[
  WalkSingularities[ InfraLoop[ { { 1, 2, 3, 4, 5, 3, 1 } } ] ],
  { <| "Cusp" -> { }, "Tangency" -> { }, "Crossing" -> { { 3, 6 } }, "TriplePoint" -> { },
       "Cover" -> 1 |> },
  TestID -> "WalkSingularities-figure-eight-crossing"
]

(* ===================== InfraImmersedQ / InfraGenericQ ===================== *)

(* hierarchy walk > immersed > generic on one crossing walk *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], w = { 2, 5, 4, 7, 8, 5, 6 } },
    { InfraWalkQ[ g, w ], InfraImmersedQ[ g, w ], InfraGenericQ[ g, w ] } ],
  { True, True, True },
  TestID -> "InfraGenericQ-crossing-is-generic"
]

(* a self-tangency is immersed but not generic *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], w = { 3, 2, 5, 8, 7, 4, 5, 2, 1 } },
    { InfraImmersedQ[ g, w ], InfraGenericQ[ g, w ] } ],
  { True, False },
  TestID -> "InfraGenericQ-tangency-not-generic"
]

(* a cusp breaks immersion *)
VerificationTest[
  InfraImmersedQ[ Graph[ { 1 <-> 2, 1 <-> 3, 1 <-> 4 } ], { 2, 1, 3, 1, 4 } ],
  False,
  TestID -> "InfraImmersedQ-cusp-not-immersed"
]

(* wrapper overloads AllTrue over realisations *)
VerificationTest[
  { InfraImmersedQ[ GridGraph[ { 3, 3 } ], InfraWalk[ { { 1, 2, 5 }, { 1, 2, 1 } } ] ],
    InfraImmersedQ[ GridGraph[ { 3, 3 } ], InfraWalk[ { { 1, 2, 5 }, { 2, 5, 8 } } ] ] },
  { False, True },
  TestID -> "InfraImmersedQ-wrapper-AllTrue"
]

(* a self-crossing walk on the square torus grid is generic *)
VerificationTest[
  With[ { g = Graph[ Flatten @ Table[ UndirectedEdge[ { i, j }, # ] & /@
        { { Mod[ i + 1, 4 ], j }, { i, Mod[ j + 1, 4 ] } }, { i, 0, 3 }, { j, 0, 3 } ] ],
      w = { { 1, 2 }, { 2, 2 }, { 2, 1 }, { 3, 1 }, { 3, 2 }, { 2, 2 }, { 2, 3 } } },
    { WalkSingularities[ w ][ "Crossing" ], InfraGenericQ[ g, w ] } ],
  { { { 2, 6 } }, True },
  TestID -> "InfraGenericQ-torus-grid-crossing"
]

(* open endpoint coincidence is an incidence, the closed heads are clean *)
VerificationTest[
  { InfraGenericQ[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6, 1 } ],
    InfraGenericQ[ CycleGraph[ 6 ], InfraLoop[ { { 1, 2, 3, 4, 5, 6, 1 } } ] ],
    InfraGenericQ[ CycleGraph[ 6 ], InfraString[ { { 1, 2, 3, 4, 5, 6 } } ] ] },
  { False, True, True },
  TestID -> "InfraGenericQ-open-vs-closed-endpoint"
]

(* the closing step of a string must be a graph edge *)
VerificationTest[
  InfraGenericQ[ CycleGraph[ 6 ], InfraString[ { { 1, 2, 3 } } ] ],
  False,
  TestID -> "InfraGenericQ-string-wrap-edge-checked"
]

(* a multiply covered loop is not generic *)
VerificationTest[
  InfraGenericQ[ Graph[ { 1 <-> 2, 2 <-> 3, 3 <-> 1, 3 <-> 4, 4 <-> 5, 5 <-> 3 } ],
    InfraLoop[ { { 1, 2, 3, 1, 2, 3, 1 } } ] ],
  False,
  TestID -> "InfraGenericQ-multiple-cover-not-generic"
]

(* the default FindInfraWalk class (simple paths) is generic *)
VerificationTest[
  AllTrue[ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ][ "Realizations" ],
    w |-> InfraGenericQ[ GridGraph[ { 3, 3 } ], w ] ],
  True,
  TestID -> "InfraGenericQ-simple-paths-are-generic"
]

EndTestSection[]
