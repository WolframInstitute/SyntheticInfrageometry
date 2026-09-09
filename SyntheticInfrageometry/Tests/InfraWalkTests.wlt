BeginTestSection["InfraWalk"]

walkGraph       = WolframInstitute`SyntheticInfrageometry`PackageScope`walkGraph;
closedWalkGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`closedWalkGraph;
infraSpread      = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;
walkSeq[ w_Graph ] := Last /@ VertexList[ w ]
(* count-less returns ONE walk graph and a bounded count a List of them, so the
   sequence reader takes either shape *)
walkSeqs[ w_Graph ] := { walkSeq @ w }
walkSeqs[ ws_List ] := walkSeq /@ ws

(* ===================== The walk shape ===================== *)

(* a walk is a directed path graph on position pairs {i, v}: its edge count is
   the length, Last /@ VertexList its vertex sequence, and every walk of the
   class comes back as one such graph *)
VerificationTest[
  With[ { ws = FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, UpTo[ 4 ], All ] },
    { MatchQ[ ws, { __Graph } ],
      AllTrue[ ws, PathGraphQ[ # ] && DirectedGraphQ[ # ] && AcyclicGraphQ[ # ] & ],
      AllTrue[ ws, First /@ VertexList[ # ] === Range @ VertexCount @ # & ],
      Union[ EdgeCount /@ ws ],
      Sort @ walkSeqs @ ws === Sort @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ] } ],
  { True, True, True, { 4 }, True },
  TestID -> "walk-is-a-path-graph-on-position-pairs"
]

(* a walk that revisits is still a path graph, since positions never repeat *)
VerificationTest[
  With[ { w = walkGraph @ { 1, 2, 1, 2, 3 } },
    { PathGraphQ @ w, VertexCount @ w, EdgeCount @ w, walkSeq @ w } ],
  { True, 5, 4, { 1, 2, 1, 2, 3 } },
  TestID -> "walk-graph-revisits-keep-positions"
]

(* a closed walk is a directed cycle on its core; the constant loop is one
   vertex with a self-loop *)
VerificationTest[
  With[ { c = closedWalkGraph @ { 1, 2, 3, 1 }, k = closedWalkGraph @ { 7 } },
    { AcyclicGraphQ @ c, VertexCount @ c, EdgeCount @ c, walkSeq @ c, LoopFreeGraphQ @ k, walkSeq @ k } ],
  { False, 3, 3, { 1, 2, 3 }, False, { 7 } },
  TestID -> "closed-walk-is-a-cycle-graph"
]

(* the bundle of two walks is their GraphUnion, a DAG layered by position *)
VerificationTest[
  With[ { u = GraphUnion[ walkGraph @ { 1, 2, 3 }, walkGraph @ { 1, 4, 3 } ] },
    { AcyclicGraphQ @ u, VertexCount @ u, EdgeCount @ u } ],
  { True, 4, 4 },
  TestID -> "walk-bundle-is-GraphUnion"
]

(* the predicates read a walk graph and a vertex list alike *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], w = { 2, 5, 4, 7, 8, 5, 6 } },
    { InfraWalkQ[ g, walkGraph @ w ],
      InfraGenericQ[ g, walkGraph @ w ] === InfraGenericQ[ g, w ],
      WalkSingularities[ walkGraph @ w ] === WalkSingularities[ w ],
      InfraGeodesicQ[ g, walkGraph @ { 1, 2, 3 } ] } ],
  { True, True, True, True },
  TestID -> "walk-graph-reads-as-its-vertex-list"
]

(* a bare integer after p1 is the endpoint; the budget is UpTo[k] *)
VerificationTest[
  { walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 2, All ],
    walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, UpTo[ 2 ], All ] },
  { { { 1, 2 } }, { { 1, 2, 3 } } },
  TestID -> "FindInfraWalk-endpoint-versus-budget"
]

(* ===================== FindInfraWalk ===================== *)

(* the pointed form is the primitive -- growth from a seed until stuck; on
   the path graph the one maximal generic walk from 1 is the full path *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1 ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "FindInfraWalk-pointed-default-witness"
]

(* growth stops at the budget: kspec counts edges *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, UpTo[ 2 ], All ],
  { { 1, 2, 3 } },
  TestID -> "FindInfraWalk-pointed-budget"
]

(* from an interior seed the maximal generic walks run both ways *)
VerificationTest[
  Sort @ walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 3, Infinity, All ],
  Sort[ { { 3, 2, 1 }, { 3, 4, 5 } } ],
  TestID -> "FindInfraWalk-pointed-maximal-both-ways"
]

VerificationTest[
  Sort @ walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], <| 1 -> 1, 2 -> 1 |>, UpTo[ 3 ], All ],
  Sort[ { { 1, 2, 3, 4 }, { 2, 1 }, { 2, 3, 4, 5 } } ],
  TestID -> "FindInfraWalk-pointed-multi-source-spread"
]

(* the two-point form is sugar -- the walks ending at p2; a target matching
   the kspec grammar is written p2, since the pointed reading
   wins the positional tie *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5 ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "FindInfraWalk-two-point-wrapped-target"
]

VerificationTest[
  Length @ walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, UpTo[ 4 ], All ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
  TestID -> "FindInfraWalk-All-matches-Wolfram-FindPath"
]

VerificationTest[
  MatchQ[
    FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, UpTo[ 4 ], All ],
    { __Graph } ],
  True,
  TestID -> "FindInfraWalk-output-shape"
]

VerificationTest[
  AllTrue[
    walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, UpTo[ 4 ], All ],
    p |-> InfraWalkQ[ GridGraph[ { 3, 3 } ], p ] ],
  True,
  TestID -> "FindInfraWalk-all-paths-pass-InfraWalkQ"
]

VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, { 4 }, 1 ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "FindInfraWalk-exact-length-spec"
]

(* the simple default is FindPath's class exactly, at every length *)
VerificationTest[
  Length @ walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  TestID -> "FindInfraWalk-range-length-spec"
]

(* the crossing count is an invariant, never a class option: the exhaustive
   spelling filters the immersed class -- on CycleGraph[4] every non-
   backtracking walk is a pure rotation, so the length-6 walks 1 -> 3 have
   exactly 3 arrivals at a visited vertex, no walk has exactly 1, and the
   simple default is empty at this length (a simple walk on C4 has at most
   3 edges) *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { reps = walkSeqs @ FindInfraWalk[ g, 1, 3, { 6 }, All, Properties -> { "Immersed" } ] },
    { walkSeqs @ FindInfraWalk[ g, 1, 3, { 6 }, All ],
      Select[ reps, Length[ # ] - Length[ DeleteDuplicates @ # ] === 1 & ],
      Sort @ Select[ reps, Length[ # ] - Length[ DeleteDuplicates @ # ] === 3 & ] } ],
  { { }, { }, Sort[ { { 1, 2, 3, 4, 1, 2, 3 }, { 1, 4, 3, 2, 1, 4, 3 } } ] },
  TestID -> "FindInfraWalk-crossing-count-exhaustive-spelling"
]

(* the pointed random walk stopped at its first self-intersection: the tip is
   the one doubled vertex -- drawn under the ambient seed.  The simple default
   cannot self-intersect, so the generic class is asked for explicitly *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { w = BlockRandom[
        First @ walkSeqs @ FindInfraWalk[ g, 1, UpTo[ 20 ], Properties -> { "Generic" },
          Method -> "RandomGreedy", "StoppingCondition" -> 1 ],
        RandomSeeding -> 7 ] },
    { First[ w ] === 1, Count[ w, Last @ w ] === 2,
      Length[ w ] - Length[ DeleteDuplicates[ w ] ] === 1 } ],
  { True, True, True },
  TestID -> "FindInfraWalk-random-walk-stops-at-first-crossing"
]

(* Method -> Automatic on a bounded count is the certified lazy descent: the
   instances are genuine, distinct members of the exhaustive class, exactly as
   many as asked *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { class = walkSeqs @ FindInfraWalk[ g, 1, 9, { 6 }, All, Method -> "Exhaustive" ] },
    { got = walkSeqs @ FindInfraWalk[ g, 1, 9, { 6 }, 3 ] },
    Length[ got ] === 3 && DuplicateFreeQ[ got ] && SubsetQ[ class, got ] ],
  True,
  TestID -> "FindInfraWalk-Automatic-greedy-members-of-class"
]

(* the default class is the simple paths -- the census filter DuplicateFreeQ
   over the bare walk class -- and "Generic" is the opt-in widening: on the
   3-by-3 grid at length <= 8 it strictly exceeds the simple paths (the
   degree-4 centre supports an isolated crossing) and is exactly
   InfraGenericQ's filter over the bare class *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { bare    = walkSeqs @ FindInfraWalk[ g, 1, 9, UpTo[ 8 ], All, Properties -> { } ],
      simple  = walkSeqs @ FindInfraWalk[ g, 1, 9, UpTo[ 8 ], All ],
      generic = walkSeqs @ FindInfraWalk[ g, 1, 9, UpTo[ 8 ], All, Properties -> { "Generic" } ] },
    { Sort @ simple === Sort @ Select[ bare, DuplicateFreeQ ],
      SubsetQ[ generic, simple ] && AnyTrue[ generic, ! DuplicateFreeQ[ # ] & ],
      Sort @ generic === Sort @ Select[ bare, InfraGenericQ[ g, # ] & ] } ],
  { True, True, True },
  TestID -> "FindInfraWalk-default-class-is-simple-Generic-opt-in"
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
    { reps = walkSeqs @ FindInfraWalk[ g, 1, 9, { 6 }, All, Properties -> { } ] },
    AnyTrue[ reps, ! InfraImmersedQ[ g, # ] & ] &&
    AnyTrue[ reps, Count[ #, 9 ] >= 2 & ] ],
  True,
  TestID -> "FindInfraWalk-empty-properties-bare-walk-class"
]

(* the pointed form owns the refusal too: "Immersed" alone leaves an
   infinite class, and a stopping condition cannot bound it (it may never
   fire) *)
VerificationTest[
  FindInfraWalk[ GridGraph[ { 4, 4 } ], 1, Infinity, 1, Properties -> { "Immersed" } ],
  $Failed,
  { FindInfraWalk::unbounded },
  TestID -> "FindInfraWalk-pointed-unbounded-refused"
]

VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, UpTo[ 10 ] ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "FindInfraWalk-UpTo-no-failure"
]

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, 7 ],
  $Failed,
  TestID -> "FindInfraWalk-strict-shortfall-Failed"
]

VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 3, <| 3 -> 1 |> ],
  { },
  TestID -> "FindInfraWalk-degenerate-same-endpoints"
]

(* ===================== Multi-anchor spread ===================== *)

VerificationTest[
  Sort[ #[[ { 1, -1 } ]] & /@
    walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], <| 1 -> 1, 2 -> 1 |>, 5, Infinity, All ] ],
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
    Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All ] ===
      Sort @ infraSpread @ FindInfraSegment[ g, 1, 9, All ]
  ],
  True,
  TestID -> "FindInfraGeodesic-scale-Infinity-is-the-segment-class"
]

(* Finder and predicate agree: every realisation is a geodesic at the scale asked for. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { walks = walkSeqs @ FindInfraGeodesic[ g, 1, 16, 2, Infinity, All,
              Properties -> { "Simple", "Minimizing" } ] },
      walks =!= { } && AllTrue[ walks, w |-> InfraGeodesicQ[ g, w, 2 ] ]
    ]
  ],
  True,
  TestID -> "FindInfraGeodesic-realisations-pass-InfraGeodesicQ"
]

(* Scale-2 minimizing simple walks on C6 are the two geodesics: a shorter local
   window cannot be met by winding the long way round. *)
VerificationTest[
  Sort @ walkSeqs @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 2, Infinity, All,
    Properties -> { "Simple", "Minimizing" } ],
  Sort[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  TestID -> "FindInfraGeodesic-scale-2-cycle-geodesics"
]

(* "Straightest" is a selector, so it can only refine: its output is a subset of
   the class it selects from. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    SubsetQ[
      Sort @ infraSpread @ FindInfraSegment[ g, 1, 9, All ],
      Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All,
        Properties -> { "Minimizing", "Straightest" } ] ]
  ],
  True,
  TestID -> "FindInfraGeodesic-selector-refines-the-class"
]

(* A constant score discriminates nothing, so the selector is vacuous. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All,
      Properties -> { "Minimizing", { "Maximal", 1 & } } ] ===
      Sort @ infraSpread @ FindInfraSegment[ g, 1, 9, All ]
  ],
  True,
  TestID -> "FindInfraGeodesic-constant-selector-is-vacuous"
]

(* Minimising the degree sum keeps the two boundary geodesics of the grid: the
   interior vertex 5 costs more than any boundary step. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, Infinity, Infinity, All,
      Properties -> { "Minimizing",
        { "Minimal", w |-> VertexDegree[ g, w[[ -2 ]] ] + VertexDegree[ g, w[[ -1 ]] ] } } ]
  ],
  Sort[ { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 } } ],
  TestID -> "FindInfraGeodesic-Minimal-degree-sum-hugs-the-boundary"
]

(* At scale 1 the window is an edge -- the last vertex and the candidate -- so an
   edge function ports as w |-> f @@ w.  A law that only accepts two-vertex
   windows keeps the whole walk class exactly when that is so. *)
VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 4, 1, { 3 }, All,
      Properties -> { w |-> Length[ w ] == 2 } ] ===
      Sort @ walkSeqs @ FindInfraWalk[ g, 1, 4, { 3 }, All ]
  ],
  True,
  TestID -> "FindInfraGeodesic-scale-1-window-is-an-edge"
]

(* A bare predicate is a custom local law; True keeps the whole class. *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, 1, { 4 }, All, Properties -> { "Simple", True & } ] ===
      Sort @ walkSeqs @ FindInfraWalk[ g, 1, 9, { 4 }, All ]
  ],
  True,
  TestID -> "FindInfraGeodesic-bare-predicate-True-keeps-the-class"
]

(* "Straightest" on the square with a chord: the pull-apart walks are the two
   two-step walks, not the chord route. *)
VerificationTest[
  Sort @ walkSeqs @ FindInfraGeodesic[ Graph[ { 1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 2 <-> 4 } ], 1, 3, 2, Infinity, All,
    Properties -> { "Simple", "Straightest" } ],
  Sort[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
  TestID -> "FindInfraGeodesic-Straightest-pull-apart"
]

(* kspec bounds the sweep depth, and an exact-length spec is honoured. *)
VerificationTest[
  With[ { walks = walkSeqs @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 1, { 5 }, All ] },
    walks =!= { } && DeleteDuplicates[ Length[ # ] - 1 & /@ walks ] === { 5 }
  ],
  True,
  TestID -> "FindInfraGeodesic-kspec-bounds-the-sweep"
]

(* With revisits allowed a local rule alone leaves an infinite class -- refused,
   not silently truncated.  The wrapper's messages are FindInfraWalk's. *)
VerificationTest[
  FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 2, Infinity, All ],
  $Failed,
  { FindInfraWalk::unbounded },
  TestID -> "FindInfraGeodesic-unbounded-class-refused"
]

(* "Generic" bounds the class by itself -- multiplicity <= 2 forces termination
   -- so kspec Infinity is accepted; on the 6-cycle the generic walks 1 -> 4
   are exactly the two geodesics (anything longer repeats an edge or returns
   to an endpoint). *)
VerificationTest[
  Sort @ walkSeqs @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 1, Infinity, All,
    Properties -> { "Generic" } ],
  Sort @ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } },
  TestID -> "FindInfraGeodesic-Generic-bounds-the-class"
]

(* the pointed geodesic: the maximal minimizing walks from 1 are the two
   geodesics to the antipode *)
VerificationTest[
  Sort @ walkSeqs @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, Infinity, Infinity, All ],
  Sort[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  TestID -> "FindInfraGeodesic-pointed-maximal-minimizing"
]

(* at scale 1 every step minimizes, so the budget is the only stop: all 2^3
   walks of length 3 from the seed *)
VerificationTest[
  Length @ walkSeqs @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 1, { 3 }, All ],
  8,
  TestID -> "FindInfraGeodesic-pointed-scale-1-budget-class"
]

(* the two-point sweep is non-terminal under a finite-scale rule: a walk may
   pass through p2 and return to end there *)
VerificationTest[
  AnyTrue[ walkSeqs @ FindInfraGeodesic[ CycleGraph[ 6 ], 1, 4, 1, { 5 }, All ],
    Count[ #, 4 ] >= 2 & ],
  True,
  TestID -> "FindInfraGeodesic-two-point-non-terminal"
]

VerificationTest[
  FindInfraGeodesic[ GridGraph[ { 3, 3 } ], 1, 9, 2, UpTo[ 4 ], 1, Properties -> { "Bogus" } ],
  $Failed,
  { FindInfraWalk::badproperty },
  TestID -> "FindInfraGeodesic-badproperty-message"
]

VerificationTest[
  FindInfraGeodesic[ GridGraph[ { 3, 3 } ], 1, 9, 2, UpTo[ 4 ], 1, Method -> "Unknown" ],
  $Failed,
  { FindInfraWalk::badmethod },
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
        First @ walkSeqs @ FindInfraGeodesic[ g, 1, 36, Infinity, Infinity, 1,
          Properties -> { "Minimizing", { "Minimal", f } }, Method -> "RandomGreedy" ],
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
      Length @ walkSeqs @ FindInfraGeodesic[ g, 1, 16, Infinity, Infinity, All,
        Properties -> { "Minimizing", "Straightest" },
        Method -> { "Exhaustive", "Pruning" -> 1 } ],
      RandomSeeding -> 42 ]
  ],
  1,
  TestID -> "FindInfraGeodesic-Pruning-beam-1"
]


(* ===================== FindInfraGeodesic is FindInfraWalk at "InfraScale" ===================== *)

(* the geodesic finder is the general finder at the positional scale with
   "Minimizing" added to the rules; its Properties -> { } is the "Minimizing"
   class, and the bare class at a finite scale is FindInfraWalk's
   Properties -> { } *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], c = CycleGraph[ 6 ] },
    { Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, 2, UpTo[ 6 ], All, Properties -> { "Simple" } ] ===
        Sort @ walkSeqs @ FindInfraWalk[ g, 1, 9, UpTo[ 6 ], All, "InfraScale" -> 2,
          Properties -> { "Minimizing", "Simple" } ],
      Sort @ walkSeqs @ FindInfraGeodesic[ c, 1, 3, { 6 }, All, Properties -> { "Immersed" } ] ===
        Sort @ walkSeqs @ FindInfraWalk[ c, 1, { 6 }, All, "InfraScale" -> 3,
          Properties -> { "Minimizing", "Immersed" } ],
      Sort @ walkSeqs @ FindInfraGeodesic[ g, 1, 9, 1, { 6 }, All ] ===
        Sort @ walkSeqs @ FindInfraWalk[ g, 1, 9, { 6 }, All, Properties -> { } ] } ],
  { True, True, True },
  TestID -> "FindInfraGeodesic-is-FindInfraWalk-with-Minimizing-at-InfraScale"
]

(* "InfraScale" is the horizon of the local rule: the walk winding once round
   C6 is minimizing in every window of 3 vertices with the next one and fails
   at 4, so the finder emits it at scale 3 and not at 4 -- exactly
   InfraGeodesicQ's threshold *)
VerificationTest[
  With[ { g = CycleGraph[ 6 ], winding = { 1, 2, 3, 4, 5, 6, 1 } },
    { MemberQ[ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, "InfraScale" -> #,
          Properties -> { "Minimizing", "Immersed" } ], winding ],
      InfraGeodesicQ[ g, winding, # ] } & /@ { 2, 3, 4, Infinity } ],
  { { True, True }, { True, True }, { False, False }, { False, False } },
  TestID -> "FindInfraWalk-InfraScale-window-semantics"
]


(* ===================== ExtendInfraWalk / ExtendInfraGeodesic ===================== *)

(* the geodesic extender is the general extender at the positional scale with
   "Minimizing" added to the rules *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Sort @ walkSeqs @ ExtendInfraGeodesic[ g, { 4, 5 }, 2, UpTo[ 3 ], All, Properties -> { "Simple" } ] ===
      Sort @ walkSeqs @ ExtendInfraWalk[ g, { 4, 5 }, UpTo[ 3 ], All, "InfraScale" -> 2,
        Properties -> { "Minimizing", "Simple" } ] ],
  True,
  TestID -> "ExtendInfraGeodesic-is-ExtendInfraWalk-with-Minimizing-at-InfraScale"
]

(* under the default class the extension is bounded by itself: a simple walk
   cannot revisit, so kspec Infinity is legal and the two-sided extension of a
   middle edge is the whole path *)
VerificationTest[
  walkSeqs @ ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 2, 3 } ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "ExtendInfraWalk-default-simple-bounds-the-class"
]

(* the pointed finder and the one-sided extender of the seed {p1} are the same
   computation, under the shared default class *)
VerificationTest[
  ExtendInfraWalk[ CycleGraph[ 6 ], { 1 }, UpTo[ 4 ], All, "Direction" -> "Forward" ],
  FindInfraWalk[ CycleGraph[ 6 ], 1, UpTo[ 4 ], All ],
  TestID -> "ExtendInfraWalk-seed-point-equals-pointed-finder"
]

VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 3, 4 }, Infinity, UpTo[ 1 ], 1,
    "Direction" -> "Forward", Properties -> { "Simple", "Minimizing" } ],
  { { 3, 4, 5 } },
  TestID -> "ExtendInfraGeodesic-PathGraph-forward"
]

VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 3, 4 }, Infinity, UpTo[ 2 ], 1,
    "Direction" -> "Backward", Properties -> { "Simple", "Minimizing" } ],
  { { 1, 2, 3, 4 } },
  TestID -> "ExtendInfraGeodesic-PathGraph-backward"
]

VerificationTest[
  Sort @ walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 3 }, Infinity, Infinity, All,
      Properties -> { "Simple", "Minimizing" } ],
  Sort[ { { 1, 2, 3, 4, 5 }, { 5, 4, 3, 2, 1 } } ],
  TestID -> "ExtendInfraGeodesic-PathGraph-both-unbudgeted"
]

VerificationTest[
  Sort @ walkSeqs @ ExtendInfraGeodesic[ CycleGraph[ 6 ], FindInfraSegment[ CycleGraph[ 6 ], 1, 4, All ],
      Infinity, UpTo[ 0 ], All, Properties -> { "Simple", "Minimizing" } ],
  Sort[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
  TestID -> "ExtendInfraGeodesic-DAG-segment-spread"
]

VerificationTest[
  Sort @ walkSeqs @ ExtendInfraGeodesic[ CycleGraph[ 6 ], { 1 }, Infinity, UpTo[ 2 ], All,
      "Direction" -> "Forward", Properties -> { "Simple", "Straightest" } ],
  Sort[ { { 1, 2, 3 }, { 1, 6, 5 } } ],
  TestID -> "ExtendInfraGeodesic-CycleGraph-Straightest-forward"
]

VerificationTest[
  AllTrue[
    walkSeqs @ ExtendInfraGeodesic[ GridGraph[ { 3, 3 } ], { 1 }, Infinity, UpTo[ 3 ], All,
      "Direction" -> "Forward", Properties -> { "Simple" } ],
    p |-> InfraWalkQ[ GridGraph[ { 3, 3 } ], p ] ],
  True,
  TestID -> "ExtendInfraGeodesic-all-extensions-pass-InfraWalkQ"
]

VerificationTest[
  MatchQ[
    ExtendInfraGeodesic[ GridGraph[ { 3, 3 } ], { 1 }, Infinity, UpTo[ 2 ], All ],
    { __Graph } ],
  True,
  TestID -> "ExtendInfraGeodesic-output-shape"
]

VerificationTest[
  Length @
    ( walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 7 ] ], { 4, 5 }, Infinity, UpTo[ 2 ], 1,
      "Direction" -> "Forward",
      Properties -> { "Simple", "Minimizing" } ] )[[ 1 ]],
  4,
  TestID -> "ExtendInfraGeodesic-budget-truncation"
]

(* Multi-realisation input: each realisation is extended, each with its own
   relative budget *)
VerificationTest[
  Sort @ walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 7 ] ],
      walkGraph /@ { { 3 }, { 5 } }, Infinity, UpTo[ 1 ], All,
      "Direction" -> "Forward", Properties -> { "Simple", "Minimizing" } ],
  Sort[ { { 3, 2 }, { 3, 4 }, { 5, 4 }, { 5, 6 } } ],
  TestID -> "ExtendInfraGeodesic-multi-realisation"
]

(* Dead-end freeze: forward extension of the right endpoint freezes *)
VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 4, 5 }, Infinity, UpTo[ 5 ], 1,
    "Direction" -> "Forward", Properties -> { "Simple", "Minimizing" } ],
  { { 4, 5 } },
  TestID -> "ExtendInfraGeodesic-dead-end-freeze"
]

VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 2, 3 }, Infinity, Infinity, 1,
    Properties -> { "Simple", "Minimizing" } ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "ExtendInfraGeodesic-BothSides-extends-segment-to-line"
]

(* on "BothSides" the budget counts edges added per growing side -- outer
   steps -- so kspec 1 buys one symmetric step *)
VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 3, 4 }, Infinity, UpTo[ 1 ], 1,
    Properties -> { "Simple", "Minimizing" } ],
  { { 2, 3, 4, 5 } },
  TestID -> "ExtendInfraGeodesic-BothSides-symmetric-one-step"
]

(* a frozen side stops paying: at kspec 2 the live side keeps growing one
   edge per step, four edges added in total *)
VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 3, 4 }, Infinity, UpTo[ 2 ], 1,
    Properties -> { "Simple", "Minimizing" } ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "ExtendInfraGeodesic-BothSides-budget-per-side"
]

(* Asymmetric tail: forward freezes immediately, backward keeps growing one
   edge per step until it reaches vertex 1 *)
VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 4, 5 }, Infinity, UpTo[ 5 ], 1,
    Properties -> { "Simple", "Minimizing" } ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "ExtendInfraGeodesic-BothSides-asymmetric-tail"
]

(* the two-sided Cartesian is re-checked as a whole geodesic: without the
   joined "Minimizing" filter C6 emits {5, 6, 1, 2, 3}, with d(5, 3) = 2 *)
VerificationTest[
  AllTrue[
    walkSeqs @ ExtendInfraGeodesic[ CycleGraph[ 6 ], { 1 }, Infinity, Infinity, All ],
    w |-> InfraGeodesicQ[ CycleGraph[ 6 ], w ] ],
  True,
  TestID -> "ExtendInfraGeodesic-BothSides-joined-Minimizing-filter"
]

(* the two sides move independently: in C4 no joint step survives the joined
   "Minimizing" filter -- {4, 1, 2, 3} has d(4, 3) = 1 -- and the maximal
   geodesics through the edge {1, 2} are reached one side at a time *)
VerificationTest[
  Sort @ walkSeqs @ ExtendInfraGeodesic[ CycleGraph[ 4 ], { 1, 2 }, Infinity, Infinity, All ],
  Sort[ { { 1, 2, 3 }, { 4, 1, 2 } } ],
  TestID -> "ExtendInfraGeodesic-BothSides-single-side-move"
]

(* the two L-shaped lines carrying the top row of a 4x4 grid: each needs the
   row extended on one side only *)
VerificationTest[
  Sort @ walkSeqs @ ExtendInfraGeodesic[ GridGraph[ { 4, 4 } ], { 1, 2, 3, 4 }, Infinity, Infinity,
    All ],
  Sort[ { { 1, 2, 3, 4, 8, 12, 16 }, { 13, 9, 5, 1, 2, 3, 4 } } ],
  TestID -> "ExtendInfraGeodesic-BothSides-grid-row-L-lines"
]

(* at scale Infinity the extension class is the geodesic-extension class the
   distance matrix defines: the walk engine and the segment pool agree, here
   on a torus where the two ends interact *)
VerificationTest[
  With[ { g = TorusGraph[ { 4, 5 } ], unoriented = w |-> Sort[ { w, Reverse @ w } ] },
    Sort[ unoriented /@ walkSeqs @ ExtendInfraGeodesic[ g, { 1, 2 }, Infinity, Infinity, All ] ] ===
    Sort[ unoriented /@ infraSpread @ ExtendInfraSegment[ g, { 1, 2 }, Infinity, All ] ] ],
  True,
  TestID -> "ExtendInfraGeodesic-BothSides-agrees-with-segment-pool"
]

(* an unbudgeted extension is maximal: re-extending a returned line returns it *)
VerificationTest[
  AllTrue[
    walkSeqs @ ExtendInfraGeodesic[ CycleGraph[ 6 ], { 1, 2 }, Infinity, Infinity, All ],
    w |-> walkSeqs @ ExtendInfraGeodesic[ CycleGraph[ 6 ], w, Infinity, Infinity, All ] === { w } ],
  True,
  TestID -> "ExtendInfraGeodesic-BothSides-extension-is-maximal"
]

(* kspec bounds the edges added per side, not the moves taken: at kspec 1 the
   only walk both capped and maximal is the symmetric one, the one-sided
   {3, 4, 5} and {4, 5, 6} still having room to grow *)
VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 7 ] ], { 4, 5 }, Infinity, UpTo[ 1 ], All,
    Properties -> { "Simple", "Minimizing" } ],
  { { 3, 4, 5, 6 } },
  TestID -> "ExtendInfraGeodesic-BothSides-budget-caps-each-side"
]

(* exact kspec on both sides: a budget the graph cannot pay returns nothing *)
VerificationTest[
  walkSeqs @ ExtendInfraWalk[ PathGraph[ Range[ 6 ] ], { 3, 4 }, { 10 }, All ],
  { },
  TestID -> "ExtendInfraWalk-BothSides-exact-kspec-unreachable"
]

(* exact relative kspec: the branch frozen after one added edge fails {2} *)
VerificationTest[
  walkSeqs @ ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 4 }, Infinity, { 2 }, All,
    "Direction" -> "Forward", Properties -> { "Simple", "Minimizing" } ],
  { { 4, 3, 2 } },
  TestID -> "ExtendInfraGeodesic-exact-kspec-drops-short-freeze"
]

(* a stopping condition on a class that may self-intersect: the winding walk
   stops at its first return, "Delay" grants further edges *)
VerificationTest[
  walkSeqs @ ExtendInfraWalk[ CycleGraph[ 6 ], { 1, 2 }, UpTo[ 20 ], 1,
    "Direction" -> "Forward", Properties -> { "Immersed" },
    "StoppingCondition" -> 1 ],
  { { 1, 2, 3, 4, 5, 6, 1 } },
  TestID -> "ExtendInfraWalk-stopping-condition-forward"
]

VerificationTest[
  walkSeqs @ ExtendInfraWalk[ CycleGraph[ 6 ], { 1, 2 }, UpTo[ 20 ], 1,
    "Direction" -> "Forward", Properties -> { "Immersed" },
    "StoppingCondition" -> { 1, "Delay" -> 2 } ],
  { { 1, 2, 3, 4, 5, 6, 1, 2, 3 } },
  TestID -> "ExtendInfraWalk-stopping-condition-delay"
]

(* events replay over the seed: a deadline that already passed inside the
   seed returns it unextended *)
VerificationTest[
  walkSeqs @ ExtendInfraWalk[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6, 1 }, UpTo[ 10 ], 1,
    "Direction" -> "Forward", Properties -> { "Immersed" },
    "StoppingCondition" -> 1 ],
  { { 1, 2, 3, 4, 5, 6, 1 } },
  TestID -> "ExtendInfraWalk-events-replay-over-seed"
]

(* a two-ended walk has no single tip for the event clock *)
VerificationTest[
  ExtendInfraWalk[ CycleGraph[ 6 ], { 1, 2 }, UpTo[ 10 ], 1,
    Properties -> { "Immersed" }, "StoppingCondition" -> 1 ],
  $Failed,
  { ExtendInfraWalk::eventsided },
  TestID -> "ExtendInfraWalk-eventsided"
]

(* under the simple default no arrival at a visited vertex can happen: the
   condition warns and the walk runs to its budget *)
VerificationTest[
  Sort @ walkSeqs @ ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3 }, UpTo[ 5 ], All,
      "Direction" -> "Forward", "StoppingCondition" -> 1 ],
  Sort[ { { 3, 2, 1 }, { 3, 4, 5 } } ],
  { ExtendInfraWalk::deadevent },
  TestID -> "ExtendInfraWalk-deadevent-warns"
]

(* "Minimizing" at a finite scale does not bound the class; the wrapper's
   messages are ExtendInfraWalk's *)
VerificationTest[
  ExtendInfraGeodesic[ CycleGraph[ 6 ], { 1 }, 2 ],
  $Failed,
  { ExtendInfraWalk::unbounded },
  TestID -> "ExtendInfraGeodesic-unbounded-finite-scale"
]

VerificationTest[
  ExtendInfraWalk[ PathGraph[ Range[ 5 ] ], { 3 }, UpTo[ 1 ], 1, "Direction" -> "Sideways" ],
  $Failed,
  { ExtendInfraWalk::baddirection },
  TestID -> "ExtendInfraWalk-baddirection"
]

VerificationTest[
  ExtendInfraGeodesic[ PathGraph[ Range[ 5 ] ], { 3 }, Infinity, Infinity, 99,
    Properties -> { "Simple", "Minimizing" } ],
  $Failed,
  TestID -> "ExtendInfraGeodesic-strict-shortfall-Failed"
]

VerificationTest[
  BlockRandom[
    With[ { r = ExtendInfraGeodesic[ GridGraph[ { 3, 3 } ], { 1 }, Infinity, UpTo[ 4 ], 1,
        "Direction" -> "Forward", Properties -> { "Simple" },
        Method -> "RandomGreedy" ] },
      MatchQ[ r, { _Graph } ] && VertexCount[ First @ r ] == 5 ],
    RandomSeeding -> 7 ],
  True,
  TestID -> "ExtendInfraGeodesic-RandomGreedy-forward-trajectory"
]

VerificationTest[
  BlockRandom[
    MatchQ[
      ExtendInfraGeodesic[ GridGraph[ { 3, 3 } ], { 5 }, Infinity, UpTo[ 4 ], 1,
        Properties -> { "Simple" }, Method -> "RandomGreedy" ],
      { _Graph } ],
    RandomSeeding -> 7 ],
  True,
  TestID -> "ExtendInfraGeodesic-RandomGreedy-BothSides-trajectory"
]


(* ===================== InfraWalk scene-DSL constructor ===================== *)

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

(* the graph carries what the accessors used to: the length is the edge count,
   the ends are read off the vertex sequence, and the end multiset is Counts *)
VerificationTest[
  With[ { reps = { { 1, 2, 4 }, { 5, 6, 4 }, { 7, 8, 9 } } },
    { ws = walkGraph /@ reps },
    { EdgeCount /@ ws, walkSeqs @ ws === reps,
      KeySort @ Counts[ Last /@ walkSeqs @ ws ] }
  ],
  { { 2, 2, 2 }, True, <| 4 -> 2, 9 -> 1 |> },
  TestID -> "walk-graph-length-ends-and-end-multiset"
]

(* kspec bounds the walk-space sweep itself: past kmax the bare walk class is
   infinite, so the enumeration must terminate. *)
VerificationTest[
  With[ { walks = walkSeqs @ FindInfraWalk[ CycleGraph[ 4 ], 1, 3, { 6 }, All, Properties -> { } ] },
    walks =!= { } && AllTrue[ walks,
      w |-> Length[ w ] - 1 == 6 && First[ w ] == 1 && Last[ w ] == 3 &&
        AllTrue[ Partition[ w, 2, 1 ], Apply[ EdgeQ[ CycleGraph[ 4 ], UndirectedEdge[ #1, #2 ] ] & ] ] ]
  ],
  True,
  TestID -> "FindInfraWalk-property-sweep-depth-bounded"
]

(* one length-unconstrained walk is the canonical witness: a geodesic *)
VerificationTest[
  EdgeCount /@ FindInfraWalk[ GridGraph[ { 4, 4 } ], 1, 16, Infinity, 1 ],
  { GraphDistance[ GridGraph[ { 4, 4 } ], 1, 16 ] },
  TestID -> "FindInfraWalk-Infinity-count1-geodesic"
]

(* bare k means at most k on the frontier sweep too: same class as the explicit
   {0, k} range *)
VerificationTest[
  Sort @ walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, UpTo[ 6 ], All,
    Properties -> { } ],
  Sort @ walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 0, 6 }, All,
    Properties -> { } ],
  TestID -> "FindInfraWalk-bare-k-at-most-both-paths"
]

(* a lower length bound must not be starved by the early-stop count *)
VerificationTest[
  Length @ walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, { 8 }, 2,
    Properties -> { }, Method -> "Exhaustive" ],
  2,
  TestID -> "FindInfraWalk-exact-length-strict-count"
]

(* Method -> "Greedy": lazy DFS, one instance; the generic default bounds the
   unconstrained descent by itself. *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, 1, Method -> "Greedy" ],
  { { 1, 2, 3, 4, 5 } },
  TestID -> "FindInfraWalk-Greedy-unconstrained-canonical-witness"
]

(* Greedy on a bounded kspec, no backtracking: picking the first (lowest-index)
   candidate at each step happens to always point toward vertex 1 on a
   PathGraph, so descending from the high end succeeds. *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 5, 1, UpTo[ 4 ], 1, Method -> "Greedy" ],
  { { 5, 4, 3, 2, 1 } },
  TestID -> "FindInfraWalk-Greedy-bounded-succeeds"
]


(* ===================== The method ladder ===================== *)

(* The lazy descent is COMPLETE, so every finite count is exact: from vertex 1
   on CycleGraph[4] there are exactly 2 immersed walks of length 6 (the two
   rotations), and asking for k of them under the deterministic "Greedy"
   returns k distinct genuine ones for every k <= 2.  Asking for 3 is the
   honest $Failed. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { whole = walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { "Immersed" } ] },
    { AllTrue[ Range[ 1, Length @ whole ],
        k |-> With[ { got = walkSeqs @ FindInfraWalk[ g, 1, { 6 }, k, Properties -> { "Immersed" },
              Method -> "Greedy" ] },
          Length[ got ] === k && DuplicateFreeQ[ got ] && SubsetQ[ whole, got ] &&
          AllTrue[ got, w |-> InfraWalkQ[ g, w ] && Length[ w ] - 1 === 6 ] ] ],
      FindInfraWalk[ g, 1, { 6 }, Length[ whole ] + 1, Properties -> { "Immersed" },
        Method -> "Greedy" ] } ],
  { True, $Failed },
  TestID -> "FindInfraWalk-Greedy-finite-count-is-exact"
]

(* Count-coupling: the count-less call is one instance -- a genuine member of the
   class, and the class itself is bigger.  This is what makes exponential
   enumeration opt-in. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { one = walkSeqs @ FindInfraWalk[ g, 1, { 6 }, Properties -> { "Immersed" } ] },
    { Length @ one === 1,
      InfraWalkQ[ g, First @ one ] && Length[ First @ one ] - 1 === 6,
      Length @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { "Immersed" } ] > 1 } ],
  { True, True, True },
  TestID -> "FindInfraWalk-countless-is-one-instance"
]

(* All the same walks, whichever engine enumerates them. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { "Immersed" },
        Method -> "Greedy" ] ===
      Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { "Immersed" },
        Method -> "Exhaustive" ] ],
  True,
  TestID -> "FindInfraWalk-Greedy-All-agrees-with-Exhaustive"
]

(* Randomising the branch order moves which walks come out first, never which
   exist: the random descent is as complete as the deterministic one, so a
   strict count is still exact and All still recovers the class. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    { class = Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { "Immersed" },
        Method -> "Greedy" ] },
    { Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { "Immersed" },
          Method -> "RandomGreedy" ] === class,
      Union @ Table[
        Length @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, 2, Properties -> { "Immersed" } ],
        { 20 } ],
      SubsetQ[ class,
        walkSeqs @ FindInfraWalk[ g, 1, { 6 }, 2, Properties -> { "Immersed" } ] ] } ],
  { True, { 2 }, True },
  TestID -> "FindInfraWalk-RandomGreedy-is-complete"
]

(* Method -> Automatic on a bounded count is the deterministic descent: the
   default witness is reproducible without a seed and is the explicit "Greedy" one *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { FindInfraWalk[ g, 1, 16, { 6 } ] === FindInfraWalk[ g, 1, 16, { 6 } ],
      FindInfraWalk[ g, 1, 16, { 6 } ] ===
        FindInfraWalk[ g, 1, 16, { 6 }, Method -> "Greedy" ] } ],
  { True, True },
  TestID -> "FindInfraWalk-Automatic-is-deterministic-Greedy"
]

(* "RandomGreedy" draws the witness from the ambient random state -- SeedRandom
   reproduces it, and the seeds disagree *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { BlockRandom[ walkSeqs @ FindInfraWalk[ g, 1, 16, { 6 }, Method -> "RandomGreedy" ],
        RandomSeeding -> 3 ] ===
      BlockRandom[ walkSeqs @ FindInfraWalk[ g, 1, 16, { 6 }, Method -> "RandomGreedy" ],
        RandomSeeding -> 3 ],
      Length @ Union @ Table[
        First @ walkSeqs @ FindInfraWalk[ g, 1, 16, { 6 }, Method -> "RandomGreedy" ],
        { 30 } ] > 1 } ],
  { True, True },
  TestID -> "FindInfraWalk-RandomGreedy-witness-is-ambient-seeded"
]

(* the count-less two-point default is the canonical witness, a geodesic, and is
   what "Greedy" means on this signature *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { w = First @ walkSeqs @ FindInfraWalk[ g, 1, <| 16 -> 1 |> ] },
    { Length[ w ] - 1 == GraphDistance[ g, 1, 16 ],
      FindInfraWalk[ g, 1, <| 16 -> 1 |> ] === FindInfraWalk[ g, 1, <| 16 -> 1 |>, Method -> "Greedy" ] } ],
  { True, True },
  TestID -> "FindInfraWalk-two-point-countless-default-is-Greedy-geodesic"
]


(* ===================== Stopping conditions ===================== *)

(* the deadline arithmetic is exact: on the single edge the walk can only
   bounce, so its first arrival at a visited vertex is the third vertex, and
   "Delay" grants that many further edges *)
VerificationTest[
  { walkSeqs @ FindInfraWalk[ PathGraph[ { 1, 2 } ], 1, UpTo[ 9 ], Properties -> { },
      "StoppingCondition" -> 1 ],
    walkSeqs @ FindInfraWalk[ PathGraph[ { 1, 2 } ], 1, UpTo[ 9 ], Properties -> { },
      "StoppingCondition" -> { 1, "Delay" -> 2 } ] },
  { { { 1, 2, 1 } }, { { 1, 2, 1, 2, 1 } } },
  TestID -> "FindInfraWalk-stopping-delay-arithmetic"
]

(* n counts arrivals at visited vertices, whichever vertex: the bounce's
   second arrival is its fourth vertex *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ { 1, 2 } ], 1, UpTo[ 9 ], Properties -> { },
    "StoppingCondition" -> 2 ],
  { { 1, 2, 1, 2 } },
  TestID -> "FindInfraWalk-stopping-count-second-arrival"
]

(* the second firing, on the generic class: two isolated double points *)
VerificationTest[
  With[ { g = GridGraph[ { 6, 6 } ] },
    { w = BlockRandom[
        First @ walkSeqs @ FindInfraWalk[ g, 1, UpTo[ 200 ], Properties -> { "Generic" },
          Method -> "RandomGreedy", "StoppingCondition" -> 2 ],
        RandomSeeding -> 1 ] },
    Length[ w ] - Length[ DeleteDuplicates @ w ] ],
  2,
  TestID -> "FindInfraWalk-stopping-count-second-firing"
]

(* a condition awaiting a self-intersection the constraints exclude warns and
   runs to the budget *)
VerificationTest[
  walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, Properties -> { "Simple" },
    "StoppingCondition" -> 1 ],
  { { 1, 2, 3, 4, 5 } },
  { FindInfraWalk::deadevent },
  TestID -> "FindInfraWalk-dead-event-warns"
]

VerificationTest[
  FindInfraWalk[ PathGraph[ Range[ 5 ] ], 1, UpTo[ 4 ], "StoppingCondition" -> "Crossing" ],
  $Failed,
  { FindInfraWalk::badevent },
  TestID -> "FindInfraWalk-badevent-message"
]

(* the endpoint is one stopping condition among many: a predicate event cuts
   the two-point search too -- a walk touching 5 stops there and never
   reaches 9 *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { reps = walkSeqs @ FindInfraWalk[ g, 1, 9, UpTo[ 8 ], All,
        "StoppingCondition" -> ( MemberQ[ #, 5 ] & ) ] },
    reps =!= { } && AllTrue[ reps, FreeQ[ #, 5 ] & ] ],
  True,
  TestID -> "FindInfraWalk-two-point-predicate-event"
]

(* ===================== WalkSingularities ===================== *)

(* a simple path is free of every singularity *)
VerificationTest[
  WalkSingularities[ { 1, 2, 5, 8, 9 } ],
  <| "SelfIntersections" -> { }, "SelfTangencies" -> { }, "Cusps" -> { } |>,
  TestID -> "WalkSingularities-simple-path-empty"
]

(* the retraced arc around an apex is one cusp block, however deep; its
   coincidences are self-intersections, not a repeated arc *)
VerificationTest[
  WalkSingularities[ { 6, 5, 4, 3, 2, 3, 4, 7 } ],
  <| "SelfIntersections" -> { { 3, 7 }, { 4, 6 } }, "SelfTangencies" -> { },
     "Cusps" -> { { 3, 4, 5, 6, 7 } } |>,
  TestID -> "WalkSingularities-deep-cusp-one-block"
]

(* a walk revisiting a tree edge: the fold is one cusp, nothing else *)
VerificationTest[
  WalkSingularities[ { 2, 1, 3, 1, 4 } ],
  <| "SelfIntersections" -> { { 2, 4 } }, "SelfTangencies" -> { }, "Cusps" -> { { 2, 3, 4 } } |>,
  TestID -> "WalkSingularities-tree-fold-is-a-cusp"
]

(* an arc re-run in the same direction: both intervals ascend *)
VerificationTest[
  WalkSingularities[ { 1, 2, 5, 6, 3, 2, 5, 8 } ][ "SelfTangencies" ],
  { { { 2, 3 }, { 6, 7 } } },
  TestID -> "WalkSingularities-direct-tangency"
]

(* an arc retraced in reverse: the second interval descends *)
VerificationTest[
  WalkSingularities[ { 3, 2, 5, 8, 7, 4, 5, 2, 1 } ][ "SelfTangencies" ],
  { { { 2, 3 }, { 8, 7 } } },
  TestID -> "WalkSingularities-inverse-tangency"
]

(* an isolated double visit is a self-intersection and nothing else *)
VerificationTest[
  WalkSingularities[ { 2, 5, 4, 7, 8, 5, 6 } ],
  <| "SelfIntersections" -> { { 2, 6 } }, "SelfTangencies" -> { }, "Cusps" -> { } |>,
  TestID -> "WalkSingularities-double-visit"
]

(* a triple point is a self-intersection group of three positions *)
VerificationTest[
  WalkSingularities[ { 0, 1, 2, 3, 1, 4, 5, 1, 6 } ],
  <| "SelfIntersections" -> { { 2, 5, 8 } }, "SelfTangencies" -> { }, "Cusps" -> { } |>,
  TestID -> "WalkSingularities-triple-point"
]

(* winding twice round a loop repeats the arc, the intervals overlapping at
   one parameter *)
VerificationTest[
  WalkSingularities[ { 1, 2, 3, 1, 2, 3, 1 } ],
  <| "SelfIntersections" -> { { 1, 4, 7 }, { 2, 5 }, { 3, 6 } },
     "SelfTangencies" -> { { { 1, 4 }, { 4, 7 } } }, "Cusps" -> { } |>,
  TestID -> "WalkSingularities-open-winding"
]

(* an arc traversed three times is one group of three intervals *)
VerificationTest[
  WalkSingularities[ { 0, 1, 2, 5, 1, 2, 6, 1, 2, 7 } ][ "SelfTangencies" ],
  { { { 2, 3 }, { 5, 6 }, { 8, 9 } } },
  TestID -> "WalkSingularities-arc-traversed-thrice"
]

(* bouncing on one edge: three overlapping cusp blocks, and the fold arc
   re-run in the same direction *)
VerificationTest[
  WalkSingularities[ { 1, 2, 1, 2, 1 } ],
  <| "SelfIntersections" -> { { 1, 3, 5 }, { 2, 4 } }, "SelfTangencies" -> { { { 1, 3 }, { 3, 5 } } },
     "Cusps" -> { { 1, 2, 3 }, { 1, 2, 3, 4, 5 }, { 3, 4, 5 } } |>,
  TestID -> "WalkSingularities-bounce"
]

(* the same closed sequence: coincident endpoints as an open list, clean as a loop *)
VerificationTest[
  { WalkSingularities[ { 1, 2, 3, 4, 5, 6, 1 } ][ "SelfIntersections" ],
    WalkSingularities[ closedWalkGraph /@ { { 1, 2, 3, 4, 5, 6, 1 } } ] },
  { { { 1, 7 } }, { <| "SelfIntersections" -> { }, "SelfTangencies" -> { }, "Cusps" -> { } |> } },
  TestID -> "WalkSingularities-loop-closes-endpoint-coincidence"
]

(* wraparound on closed heads: the doubled edge folds at both vertices, each
   block cut where it would wrap onto itself *)
VerificationTest[
  WalkSingularities[ closedWalkGraph /@ { { 1, 2, 1 } } ][[ 1, "Cusps" ]],
  { { 1 }, { 2 } },
  TestID -> "WalkSingularities-loop-wraparound-cusps"
]

(* a doubled interval folds at both ends: two cusp blocks, positions in
   cyclic order *)
VerificationTest[
  WalkSingularities[ closedWalkGraph /@ { { 1, 2, 5, 2, 1 } } ][[ 1 ]],
  <| "SelfIntersections" -> { { 2, 4 } }, "SelfTangencies" -> { }, "Cusps" -> { { 4, 1, 2 }, { 2, 3, 4 } } |>,
  TestID -> "WalkSingularities-doubled-interval-two-cusps"
]

(* a periodic core is the multiply covered loop: one repeated arc tiling the cycle *)
VerificationTest[
  WalkSingularities[ closedWalkGraph /@ { { 1, 2, 3, 1, 2, 3, 1 } } ][[ 1 ]],
  <| "SelfIntersections" -> { { 1, 4 }, { 2, 5 }, { 3, 6 } },
     "SelfTangencies" -> { { { 1, 3 }, { 4, 6 } } }, "Cusps" -> { } |>,
  TestID -> "WalkSingularities-multiply-covered-loop"
]

(* figure-eight based at the cut vertex of the bowtie: one double visit *)
VerificationTest[
  WalkSingularities[ closedWalkGraph /@ { { 1, 2, 3, 4, 5, 3, 1 } } ][[ 1 ]],
  <| "SelfIntersections" -> { { 3, 6 } }, "SelfTangencies" -> { }, "Cusps" -> { } |>,
  TestID -> "WalkSingularities-figure-eight"
]

(* a repeated arc across the wrap of a closed core is read in lifted
   positions, mod m *)
VerificationTest[
  WalkSingularities[ closedWalkGraph /@ { { 3, 7, 1, 2, 3, 4, 5, 6, 2, 3 } } ][[ 1 ]],
  <| "SelfIntersections" -> { { 1, 5 }, { 4, 9 } }, "SelfTangencies" -> { { { 4, 5 }, { 9, 10 } } },
     "Cusps" -> { } |>,
  TestID -> "WalkSingularities-loop-wrapped-arc"
]

(* an arc retraced in reverse on a closed core *)
VerificationTest[
  WalkSingularities[ closedWalkGraph /@ { { 5, 2, 3, 6, 7, 8, 9, 3, 2, 4, 5 } } ][[ 1 ]],
  <| "SelfIntersections" -> { { 2, 9 }, { 3, 8 } }, "SelfTangencies" -> { { { 2, 3 }, { 9, 8 } } },
     "Cusps" -> { } |>,
  TestID -> "WalkSingularities-loop-inverse-tangency"
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

VerificationTest[
  { InfraImmersedQ[ GridGraph[ { 3, 3 } ], walkGraph /@ { { 1, 2, 5 }, { 1, 2, 1 } } ],
    InfraImmersedQ[ GridGraph[ { 3, 3 } ], walkGraph /@ { { 1, 2, 5 }, { 2, 5, 8 } } ] },
  { False, True },
  TestID -> "InfraImmersedQ-wrapper-AllTrue"
]

(* a self-crossing walk on the square torus grid is generic *)
VerificationTest[
  With[ { g = Graph[ Flatten @ Table[ UndirectedEdge[ { i, j }, # ] & /@
        { { Mod[ i + 1, 4 ], j }, { i, Mod[ j + 1, 4 ] } }, { i, 0, 3 }, { j, 0, 3 } ] ],
      w = { { 1, 2 }, { 2, 2 }, { 2, 1 }, { 3, 1 }, { 3, 2 }, { 2, 2 }, { 2, 3 } } },
    { WalkSingularities[ w ][ "SelfIntersections" ], InfraGenericQ[ g, w ] } ],
  { { { 2, 6 } }, True },
  TestID -> "InfraGenericQ-torus-grid-crossing"
]

(* open endpoint coincidence is an incidence, the closed heads are clean *)
VerificationTest[
  { InfraGenericQ[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6, 1 } ],
    InfraGenericQ[ CycleGraph[ 6 ], closedWalkGraph /@ { { 1, 2, 3, 4, 5, 6, 1 } } ],
    InfraGenericQ[ CycleGraph[ 6 ], closedWalkGraph /@ { { 1, 2, 3, 4, 5, 6 } } ] },
  { False, True, True },
  TestID -> "InfraGenericQ-open-vs-closed-endpoint"
]

(* the closing step of a string must be a graph edge *)
VerificationTest[
  InfraGenericQ[ CycleGraph[ 6 ], closedWalkGraph /@ { { 1, 2, 3 } } ],
  False,
  TestID -> "InfraGenericQ-string-wrap-edge-checked"
]

(* a multiply covered loop is not generic *)
VerificationTest[
  InfraGenericQ[ Graph[ { 1 <-> 2, 2 <-> 3, 3 <-> 1, 3 <-> 4, 4 <-> 5, 5 <-> 3 } ],
    closedWalkGraph /@ { { 1, 2, 3, 1, 2, 3, 1 } } ],
  False,
  TestID -> "InfraGenericQ-multiple-cover-not-generic"
]

(* the default FindInfraWalk class (simple paths) is generic *)
VerificationTest[
  AllTrue[ walkSeqs @ FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, 9, UpTo[ 4 ], All ],
    w |-> InfraGenericQ[ GridGraph[ { 3, 3 } ], w ] ],
  True,
  TestID -> "InfraGenericQ-simple-paths-are-generic"
]

(* ===================== Species exclusion ===================== *)

(* each exclusion is exactly the census filter over the bare walk class *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { bare = walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All, Properties -> { } ] },
    AllTrue[ { "Cusps", "SelfTangencies", "TriplePoints", "SelfIntersections" },
      species |->
        Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 6 }, All,
            Properties -> { "Exclude" -> species } ] ===
          Sort @ Select[ bare, Switch[ species,
            "SelfIntersections", DuplicateFreeQ[ # ],
            "TriplePoints", Max[ Counts @ # ] <= 2,
            _, WalkSingularities[ # ][ species ] === { } ] & ] ] ],
  True,
  TestID -> "Exclude-per-species-equals-census-filter"
]

(* the named classes are exclusion sets: the default "Simple" excludes
   self-intersections, "Immersed" cusps, and "Generic" cusps, self-tangencies
   and triple points *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 7 }, All ] ===
        Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 7 }, All,
          Properties -> { "Exclude" -> "SelfIntersections" } ],
      Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 5 }, All, Properties -> { "Immersed" } ] ===
        Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 5 }, All,
          Properties -> { "Exclude" -> "Cusps" } ],
      Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 7 }, All, Properties -> { "Generic" } ] ===
        Sort @ walkSeqs @ FindInfraWalk[ g, 1, { 7 }, All,
          Properties -> { "Exclude" -> { "Cusps", "SelfTangencies", "TriplePoints" } } ] } ],
  { True, True, True },
  TestID -> "Exclude-named-classes-are-exclusion-sets"
]

(* excluding third visits bounds the class by itself, so an unbounded budget
   is accepted *)
VerificationTest[
  With[ { reps = walkSeqs @ FindInfraWalk[ PathGraph[ Range[ 4 ] ], 1, Infinity, All,
      Properties -> { "Exclude" -> "TriplePoints" } ] },
    reps =!= { } && AllTrue[ reps, Max[ Counts @ # ] <= 2 & ] ],
  True,
  TestID -> "Exclude-triple-points-bounds-the-class"
]

VerificationTest[
  FindInfraWalk[ GridGraph[ { 3, 3 } ], 1, UpTo[ 4 ], Properties -> { "Exclude" -> "Bogus" } ],
  $Failed,
  { FindInfraWalk::badproperty },
  TestID -> "Exclude-unknown-species-refused"
]

(* ===================== InfraWalkCrossingQ ===================== *)

(* straight through the centre of the 3-by-3 grid twice, once along each
   axis: the passes separate each other on the shell {1, 2}, a transverse
   crossing at scale 1; the ambient point, its multiset and the position pair
   all name it *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], w = { 1, 4, 5, 6, 9, 8, 5, 2, 3 } },
    { InfraWalkCrossingQ[ g, w, 5, 1 ], InfraWalkCrossingQ[ g, w, 5, 1 ],
      InfraWalkCrossingQ[ g, w, { 3, 7 }, 1 ] } ],
  { True, True, True },
  TestID -> "InfraWalkCrossingQ-straight-crossing-is-transverse"
]

(* two corner turns at the centre kiss on the shell: a double visit the
   census cannot tell from a crossing, and the sphere can *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], w = { 1, 2, 5, 4, 7, 8, 5, 6, 9 } },
    { InfraGenericQ[ g, w ], InfraWalkCrossingQ[ g, w, 5, 1 ] } ],
  { True, False },
  TestID -> "InfraWalkCrossingQ-corner-kiss-is-not-a-crossing"
]

(* a pass has to leave the shell for its radial arc to cut it: a walk ending
   on the sphere resolves nothing *)
VerificationTest[
  InfraWalkCrossingQ[ GridGraph[ { 3, 3 } ], { 4, 5, 6, 9, 8, 5, 2 }, 5, 1 ],
  False,
  TestID -> "InfraWalkCrossingQ-truncated-pass-unresolved"
]

(* a point visited three times is no crossing *)
VerificationTest[
  InfraWalkCrossingQ[ GridGraph[ { 3, 3 } ], { 2, 5, 4, 1, 2, 5, 8, 7, 4, 5, 6 }, 5, 1 ],
  False,
  TestID -> "InfraWalkCrossingQ-triple-point-is-no-crossing"
]

(* the wrapper answers for all its realisations *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { InfraWalkCrossingQ[ g,
        walkGraph /@ { { 1, 4, 5, 6, 9, 8, 5, 2, 3 }, { 3, 6, 5, 4, 1, 2, 5, 8, 7 } }, 5, 1 ],
      InfraWalkCrossingQ[ g,
        walkGraph /@ { { 1, 4, 5, 6, 9, 8, 5, 2, 3 }, { 1, 2, 5, 4, 7, 8, 5, 6, 9 } }, 5, 1 ] } ],
  { True, False },
  TestID -> "InfraWalkCrossingQ-wrapper-AllTrue"
]

(* on the square torus grid the walk turns W -> S and E -> N at {2, 2}:
   generic, not a crossing; list-valued labels take the position pair *)
VerificationTest[
  With[ { g = Graph[ Flatten @ Table[ UndirectedEdge[ { i, j }, # ] & /@
        { { Mod[ i + 1, 4 ], j }, { i, Mod[ j + 1, 4 ] } }, { i, 0, 3 }, { j, 0, 3 } ] ],
      w = { { 0, 2 }, { 1, 2 }, { 2, 2 }, { 2, 1 }, { 3, 1 }, { 3, 2 }, { 2, 2 }, { 2, 3 }, { 2, 0 } } },
    { InfraGenericQ[ g, w ], InfraWalkCrossingQ[ g, w, { 3, 7 }, 1 ] } ],
  { True, False },
  TestID -> "InfraWalkCrossingQ-torus-corner-kiss"
]

(* on a larger grid the straight crossing holds at scales 1 and 2 and is
   unresolved at scale 3, where the passes never leave the ball; two corner
   turns are no crossing at any scale *)
VerificationTest[
  With[ { g = GridGraph[ { 7, 7 } ],
      straight = { 4, 11, 18, 25, 32, 39, 46, 47, 48, 41, 34, 27, 26, 25, 24, 23, 22 },
      corners = { 4, 11, 18, 25, 24, 23, 22, 29, 36, 43, 44, 45, 46, 39, 32, 25, 26, 27, 28 } },
    { InfraWalkCrossingQ[ g, straight, 25, # ] & /@ { 1, 2, 3 },
      InfraWalkCrossingQ[ g, corners, 25, # ] & /@ { 1, 2 } } ],
  { { True, True, False }, { False, False } },
  TestID -> "InfraWalkCrossingQ-grid-scales"
]

(* in three dimensions the shell minus two radial cuts stays connected: no
   double point is a crossing *)
VerificationTest[
  InfraWalkCrossingQ[ GridGraph[ { 5, 5, 5 } ],
    { 61, 62, 63, 64, 65, 90, 115, 114, 113, 88, 63, 38, 13 }, 63, 1 ],
  False,
  TestID -> "InfraWalkCrossingQ-three-dimensions-no-crossing"
]

(* on a triangulated patch, where the exact sphere is itself a cycle, the
   straight crossing and a diagonal one are transverse at scales 1 and 2 *)
VerificationTest[
  With[ { g = EdgeAdd[ GridGraph[ { 7, 7 } ],
        UndirectedEdge[ #, # + 8 ] & /@ Select[ Range[ 42 ], Mod[ #, 7 ] != 0 & ] ],
      straight = { 4, 11, 18, 25, 32, 39, 46, 47, 48, 41, 34, 27, 26, 25, 24, 23, 22 },
      diagonal = { 4, 11, 18, 25, 32, 39, 46, 47, 48, 41, 33, 25, 17, 9, 1 } },
    { InfraWalkCrossingQ[ g, straight, 25, # ] & /@ { 1, 2 },
      InfraWalkCrossingQ[ g, diagonal, 25, # ] & /@ { 1, 2 } } ],
  { { True, True }, { True, True } },
  TestID -> "InfraWalkCrossingQ-triangulated-patch"
]

(* a figure eight through the centre of the 5-by-5 grid, both passes
   straight: a crossing on the closed heads too *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    { InfraWalkCrossingQ[ g, closedWalkGraph /@ { { 13, 14, 19, 18, 13, 8, 7, 12, 13 } }, 13, 1 ],
      InfraWalkCrossingQ[ g, closedWalkGraph /@ { { 13, 14, 19, 18, 13, 8, 7, 12 } }, 13, 1 ] } ],
  { True, True },
  TestID -> "InfraWalkCrossingQ-figure-eight-loop"
]

EndTestSection[]
