Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[allNeighboursBaseFn]


(* ===================== InfraWalk wrapper ===================== *)

(* set canonicalisation, ["Realizations"] / ["First"] and the occupation-measure
   accessors come from defineInfraBundleRules (Tools.wl). *)

(* InfraWalk[p1, p2, ..., pk] with InfraPoint args (or singleton lists thereof,
   as returned by FindInfraPoint) builds the wrapper from the Cartesian product
   of point realisations: each tuple is one walk. *)
InfraWalk[ args : ( _InfraPoint | { _InfraPoint } ) .. ] :=
  InfraWalk[ Tuples @ Map[ Replace[ #, { x_ } :> x ][[ 1 ]]&, { args } ] ]

(* "Length" = list of edge counts, one per realisation: |walk| - 1. *)
InfraWalk[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* endpoint InfraPoints, multiplicity kept: the end-vertex multiset is the
   occupation measure of where the walks terminate (unlike InfraSegment, whose
   endpoints are deduplicated geodesic ends). *)
InfraWalk[ reps_List ][ "Start" ] := columnInfraPoint[ reps, 1 ]
InfraWalk[ reps_List ][ "End" ]   := columnInfraPoint[ reps, -1 ]
(* ===================== FindInfraWalk ===================== *)

(* A walk from p1 to p2.  "Simple" -> True (default) gives embedded curves --
   simple paths, exactly FindPath's class, so Method -> Automatic delegates to
   FindPath whatever the count (it is already lazy for bounded counts).
   "Simple" -> False gives immersed curves with a crossing: non-backtracking
   walks revisiting at least one vertex (a bounce a,b,a is a cusp, not a
   crossing, and non-backtracking excludes it).  There Method -> Automatic
   draws crossing walks at random for a bounded count ("RandomGreedy",
   ambient-seeded lasso draws: a random loop vertex, a short cycle through it,
   geodesic legs -- a deterministic first-completion would hug the lowest-index
   cycle) and enumerates exhaustively under All; kspec must be finite
   (::unbounded), since crossings exist at every excess length.  Local
   geodesic rules live on FindInfraGeodesic, and the unrestricted
   revisits-allowed walk class is FindInfraGeodesic at scale 1. *)

FindInfraWalk::properties  = "FindInfraWalk no longer takes Properties; use \"Simple\" -> True (default) | False. Local geodesic rules live on FindInfraGeodesic.";
FindInfraWalk::badmethod   = "Method `1` is not supported by FindInfraWalk.";
FindInfraWalk::shortfall   = "\"RandomGreedy\" drew `1` distinct walks of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";
FindInfraWalk::unbounded   = "with \"Simple\" -> False the self-intersecting walk class is infinite without a length bound; give kspec a finite bound.";

Options[ FindInfraWalk ] = {
  "Simple" -> True,
  Method   -> Automatic
};

FindInfraWalk[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    { q1, q2 } |-> If[ q1 === q2, { },
      Catch @ With[ {
          simpleQ   = TrueQ @ OptionValue[ FindInfraWalk,
            FilterRules[ { opts }, Options @ FindInfraWalk ], "Simple" ],
          methodOpt = OptionValue[ FindInfraWalk,
            FilterRules[ { opts }, Options @ FindInfraWalk ], Method ] },
        { methodSpec = resolveMethod[
            If[ methodOpt === Automatic,
              Which[ simpleQ, "Exhaustive",
                countLimit[ count ] === Infinity, "Exhaustive",
                True, "RandomGreedy" ],
              methodOpt ], count ] },
        { methodHead = methodName @ methodSpec,
          pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax       = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          acceptQ    = If[ simpleQ, walkLengthAdmissibleQ[ kspec ],
            With[ { lengthQ = walkLengthAdmissibleQ[ kspec ] },
              w |-> lengthQ[ w ] && ! DuplicateFreeQ[ w ] ] ] },
        { candidateFn = If[ simpleQ,
            { g, path } |-> Select[ allNeighboursBaseFn[ g, path ], ! MemberQ[ path, # ] & ],
            { g, path } |-> If[ Length[ path ] < 2, allNeighboursBaseFn[ g, path ],
              DeleteCases[ allNeighboursBaseFn[ g, path ], path[[ -2 ]] ] ] ] },
        If[ ! FreeQ[ { opts }, Properties ],
          Message[ FindInfraWalk::properties ]; Throw[ $Failed ] ];
        (* a crossing walk of every excess length exists once one does, so the class
           has no canonical finite witness -- refuse, as FindInfraGeodesic does *)
        If[ ! simpleQ && kspec === Infinity,
          Message[ FindInfraWalk::unbounded ]; Throw[ $Failed ] ];
        Switch[ methodHead,
          "Exhaustive",
            If[ simpleQ,
              If[ kspec === Infinity && countLimit[ count ] === 1,
                (* one length-unconstrained simple path: FindPath's DFS can wander for
                   minutes on mesh-like graphs, and a geodesic is simple -- the canonical
                   witness *)
                With[ { path = FindShortestPath[ graph, q1, q2 ] },
                  If[ path === { }, { }, { path } ] ],
                Replace[
                  FindPath[ graph, q1, q2, kspec, count /. { UpTo[ n_ ] :> n, Automatic -> 1 } ],
                  Except[ _List ] -> { } ] ],
              (* kspec bounds the sweep depth, not just a post-filter: the walk space is
                 infinite past kmax.  Early stop on count is never sound here -- the BFS
                 completes short walks first, and a short completion can be duplicate-free
                 and fill the quota while failing the crossing filter.  A crossing walk
                 may pass through q2, so the sweep is non-terminal. *)
              Select[
                frontierSweep[ graph, q1, q2,
                  { g, path } |-> If[ Length[ path ] - 1 >= kmax, { },
                    candidateFn[ g, path ] ],
                  pruning, Infinity, False ],
                acceptQ ]
            ],
          "Greedy",
            greedyFrontierSweep[ graph, q1, q2, candidateFn, acceptQ, kmax, count,
              Identity, simpleQ ],
          "RandomGreedy",
            If[ simpleQ,
              randomDraws[
                { } |-> greedyFrontierSweep[ graph, q1, q2, candidateFn, acceptQ, kmax, 1,
                  randomBranch, True ],
                count, FindInfraWalk ],
              (* a blind random descent almost never ends at a distant q2, so a crossing
                 draw is assembled as a lasso: a random loop vertex v, one short cycle
                 through it, geodesic legs q1 -> v -> q2, oriented to avoid a backtrack at
                 the joints.  Any crossing walk contains a loop vertex v with
                 d(q1,v) + |C| + d(v,q2) <= its length, so no admissible v proves the
                 class empty. *)
              With[ {
                  dist1 = AssociationThread[ VertexList @ graph, GraphDistance[ graph, q1 ] ],
                  dist2 = AssociationThread[ VertexList @ graph, GraphDistance[ graph, q2 ] ],
                  kmin  = Replace[ kspec, { { lo_, _ } :> lo, { k_Integer } :> k, _ -> 0 } ] },
                { anchors = Select[ VertexList @ graph, dist1[ # ] + dist2[ # ] + 3 <= kmax & ] },
                If[ anchors === { }, { },
                  randomDraws[
                    { } |-> With[ { v = RandomChoice @ anchors },
                      { cycles = FindCycle[ { graph, v },
                          { Max[ 3, kmin - dist1[ v ] - dist2[ v ] ],
                            kmax - dist1[ v ] - dist2[ v ] }, 1 ] },
                      If[ cycles === { }, { },
                        With[ { seq = cycleToVertexSequence @ First @ cycles },
                          { loop = Append[
                              RotateLeft[ seq, First @ FirstPosition[ seq, v ] - 1 ], v ] },
                          { legs = { FindShortestPath[ graph, q1, v ],
                              FindShortestPath[ graph, v, q2 ] } },
                          { lassos = Select[
                              ( Join[ legs[[ 1 ]], Rest @ #, Rest @ legs[[ 2 ]] ] & ) /@
                                { loop, Reverse @ loop },
                              w |-> acceptQ[ w ] &&
                                AllTrue[ Partition[ w, 3, 1 ], #[[ 1 ]] =!= #[[ 3 ]] & ] ] },
                          If[ lassos === { }, { }, { RandomChoice @ lassos } ] ] ] ],
                    count, FindInfraWalk ] ] ] ],
          _,
            Message[ FindInfraWalk::badmethod, methodSpec ]; $Failed
        ]
      ]
    ], p1, p2 ]


(* Path-family base candidate function: every adjacent vertex of Last @ path,
   without simplicity filtering -- "Simple" Property handles that opt-in. *)

allNeighboursBaseFn[ g_Graph, path_List ] := AdjacencyList[ g, Last @ path ]


(* walkLengthAdmissibleQ[kspec]: predicate on a vertex sequence checking it
   has length compatible with kspec.  Path length = number of edges = Length - 1.
   Bare k means at most k (the FindPath convention); {k} means exactly k. *)

walkLengthAdmissibleQ[ Infinity ]                 := True &
walkLengthAdmissibleQ[ k_Integer ]                := Length[ # ] - 1 <= k &
walkLengthAdmissibleQ[ { k_Integer } ]            := Length[ # ] - 1 == k &
walkLengthAdmissibleQ[ { kmin_Integer, kmax_Integer } ] :=
  kmin <= Length[ # ] - 1 <= kmax &


(* ===================== FindInfraGeodesic ===================== *)

(* A geodesic at infra-scale r from p1 to p2: a walk in which every window --
   the last r vertices together with the next one -- satisfies the local rule.
   The class degenerates at both ends of the scale ladder: under "Minimizing",
   r = 1 asks only for adjacency (any walk), r = Infinity for a segment.
   Returns InfraWalk; InfraGeodesicQ[graph, walk, r] carries the class.

   Rules (Properties): "Minimizing" (window is a shortest path), "Simple" (no
   revisits) and a bare predicate on the window are constraints; "Straightest"
   (step away from the window) and {"Minimal", f} / {"Maximal", f} on f[window]
   are selectors refining the surviving ties.

   Candidates are local: every neighbour of the walk's last vertex, filtered by
   the rules alone.  p2 never enters a window, so a selector may steer the walk
   away from p2 and no realisation survives -- that is the honest answer for an
   observer whose horizon is r.  The target-aware geodesic DAG is
   FindInfraSegment's optimisation of the constraint-only case r = Infinity. *)

FindInfraGeodesic::badproperty = "Property `1` is not supported by FindInfraGeodesic; the rules are \"Minimizing\", \"Straightest\", \"Simple\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window.";
FindInfraGeodesic::badmethod   = "Method `1` is not supported by FindInfraGeodesic.";
FindInfraGeodesic::unbounded   = "The geodesic class at scale `1` is infinite without a length bound: give a finite kspec, add the \"Simple\" rule, or ask at scale Infinity.";
FindInfraGeodesic::shortfall   = "\"RandomGreedy\" drew `1` distinct geodesics of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";

Options[ FindInfraGeodesic ] = {
  Properties -> { "Minimizing" },
  Method     -> Automatic
};

FindInfraGeodesic[ graph_Graph, p1_, p2_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    { q1, q2 } |-> If[ q1 === q2, { },
      Catch @ With[ {
          rules      = OptionValue[ FindInfraGeodesic, { opts }, Properties ],
          methodSpec = resolveMethod[ OptionValue[ FindInfraGeodesic, { opts }, Method ], count ] },
        { methodHead = methodName @ methodSpec,
          pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax       = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          acceptQ    = walkLengthAdmissibleQ[ kspec ],
          candidateFn = windowCandidateFn[ graph, scale, rules, FindInfraGeodesic ] },
        (* with revisits allowed a local rule alone leaves an infinite class
           (a walk can wind around a long cycle forever and stay minimizing at
           every finite scale); only simplicity or a global minimizing rule
           bounds it *)
        If[ kmax === Infinity && ! MemberQ[ rules, "Simple" ] &&
            ! ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ),
          Message[ FindInfraGeodesic::unbounded, scale ]; Throw[ $Failed ] ];
        Switch[ methodHead,
          "Exhaustive",
            Select[
              frontierSweep[ graph, q1, q2,
                { g, walk } |-> If[ Length[ walk ] - 1 >= kmax, { },
                  candidateFn[ g, walk ] ],
                pruning,
                If[ MatchQ[ kspec, { _Integer } | { _Integer, _Integer } ],
                  Infinity, countLimit @ count ] ],
              acceptQ ],
          "Greedy",
            greedyFrontierSweep[ graph, q1, q2, candidateFn, acceptQ, kmax, count ],
          "RandomGreedy",
            randomDraws[
              { } |-> greedyFrontierSweep[ graph, q1, q2, candidateFn, acceptQ, kmax, 1, randomBranch ],
              count, FindInfraGeodesic ],
          _,
            Message[ FindInfraGeodesic::badmethod, methodSpec ]; $Failed
        ]
      ]
    ], p1, p2 ]


(* ===================== InfraGeodesicQ ===================== *)

(* A walk is a geodesic at infra-scale r iff it is a walk and every window --
   r consecutive vertices with the next one -- is a shortest path.  The ladder is
   exact at both ends: r = 1 is InfraWalkQ, r = Infinity is InfraSegmentQ. *)

InfraGeodesicQ[ graph_Graph, w : _InfraWalk | _InfraLoop | _InfraString,
    scale : ( _Integer | Infinity ) : Infinity ] :=
  AllTrue[ First @ w, InfraGeodesicQ[ graph, #, scale ] & ]

InfraGeodesicQ[ graph_Graph, walk_List,
    scale : ( _Integer | Infinity ) : Infinity ] /; Length[ walk ] >= 2 :=
  InfraWalkQ[ graph, walk ] &&
  AllTrue[ Range[ 2, Length[ walk ] ],
    i |-> With[ { j = If[ scale === Infinity, 1, Max[ 1, i - scale ] ] },
      GraphDistance[ graph, walk[[ j ]], walk[[ i ]] ] == i - j ] ]

InfraGeodesicQ[ _Graph, walk_List, ___ ] /; Length[ walk ] < 2 := False


(* ===================== ExtendInfraWalk ===================== *)

(* ExtendInfraWalk[g, walk, n] extends a walk by per-step rules until
   inextensible ("Length" -> Automatic) or for the requested step budget.
   Properties is FindInfraGeodesic's rule vocabulary, read at the infra-scale
   "InfraScale" (Infinity by default, so "Minimizing" extends a segment along a
   global geodesic); default Properties -> {} allows non-simple extensions. *)

ExtendInfraWalk::badproperty  = "Property `1` is not supported by ExtendInfraWalk; the rules are \"Minimizing\", \"Straightest\", \"Simple\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window.";
ExtendInfraWalk::badmethod    = "Method `1` is not supported by ExtendInfraWalk.";
ExtendInfraWalk::baddirection = "Direction `1` is not supported by ExtendInfraWalk.";

Options[ ExtendInfraWalk ] = {
  Properties   -> { },
  Method       -> "Exhaustive",
  "InfraScale" -> Infinity,
  "Length"     -> Automatic,
  "Direction"  -> "BothSides"
};

ExtendInfraWalk[ graph_Graph, path_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    walk0 |-> If[ Length[ walk0 ] < 1, { walk0 },
      Catch @ With[ {
          properties = OptionValue[ ExtendInfraWalk, { opts }, Properties ],
          methodSpec = OptionValue[ ExtendInfraWalk, { opts }, Method ] /. Automatic -> "Exhaustive",
          scale      = OptionValue[ ExtendInfraWalk, { opts }, "InfraScale" ],
          direction  = OptionValue[ ExtendInfraWalk, { opts }, "Direction" ],
          length     = OptionValue[ ExtendInfraWalk, { opts }, "Length" ] },
        { methodHead = methodName @ methodSpec,
          pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        If[ methodHead =!= "Exhaustive",
          Message[ ExtendInfraWalk::badmethod, methodSpec ]; Throw[ $Failed ] ];
        With[ { candidateFn = windowCandidateFn[ graph, scale, properties, ExtendInfraWalk ],
                simpleQ     = MemberQ[ properties, "Simple" ] },
          Switch[ direction,
            "Forward",   extendOneSide[ graph, walk0, candidateFn, length, pruning ],
            "Backward",  Reverse /@ extendOneSide[ graph, Reverse @ walk0,
                           candidateFn, length, pruning ],
            "BothSides", extendBothSidesSymmetric[ graph, walk0, candidateFn,
                           length, pruning, simpleQ ],
            _, Message[ ExtendInfraWalk::baddirection, direction ]; Throw[ $Failed ]
          ]
        ]
      ]
    ], path ]


(* One-side frontier BFS over walk space.  Walks with no admissible next
   vertex freeze; walks at step budget exit alive. *)

extendOneSide[ graph_Graph, seed_List, candidateFn_, length_, pruning_ ] :=
  Module[ { live = { seed }, dead = { }, steps = 0,
            maxSteps = length /. Automatic -> Infinity },
    While[ live =!= { } && steps < maxSteps,
      With[ { pairs = ( p |-> { p, candidateFn[ graph, p ] } ) /@ live },
        dead = Join[ dead, Cases[ pairs, { p_, { } } :> p ] ];
        live = applyPruning[
          Flatten[ Cases[ pairs,
            { p_, nexts : { __ } } :> ( Append[ p, # ] & /@ nexts ) ], 1 ],
          pruning ]
      ];
      steps++
    ];
    DeleteDuplicates @ Join[ dead, live ]
  ]


(* Per-step symmetric BFS: each outer step grows the walk by at most one
   vertex on each side.  A side freezes when its candidateFn returns {}; the
   other side keeps growing one edge per step until it freezes too.  "Length"
   counts outer steps. *)

extendBothSidesSymmetric[ graph_Graph, seed_List, candidateFn_, length_, pruning_, simpleQ_ ] :=
  Module[ { live = { seed }, dead = { }, steps = 0,
            maxSteps = length /. Automatic -> Infinity },
    While[ live =!= { } && steps < maxSteps,
      With[ { pairs = ( w |-> { w, stepBothSides[ graph, w, candidateFn ] } ) /@ live },
        dead = Join[ dead, Cases[ pairs, { w_, { } } :> w ] ];
        live = applyPruning[
          Flatten[ Cases[ pairs, { _, nexts : { __ } } :> nexts ], 1 ],
          pruning ];
        If[ simpleQ, live = Select[ live, DuplicateFreeQ ] ]
      ];
      steps++
    ];
    DeleteDuplicates @ Join[ dead, live ]
  ]


stepBothSides[ graph_Graph, walk_List, candidateFn_ ] :=
  With[ { backCands = candidateFn[ graph, Reverse @ walk ],
          fwdCands  = candidateFn[ graph, walk ] },
    Which[
      backCands === { } && fwdCands === { }, { },
      backCands === { },                     Append[ walk, # ] & /@ fwdCands,
      fwdCands  === { },                     Prepend[ walk, # ] & /@ backCands,
      True, Flatten[ Outer[ Prepend[ Append[ walk, #2 ], #1 ] &,
              backCands, fwdCands, 1 ], 1 ]
    ]
  ]


(* ===================== ConcatenateInfraWalk ===================== *)

(* path concatenation: all pairs (walk1, walk2) with Last[walk1] === First[walk2] *)

ConcatenateInfraWalk[ path1_, path2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  spreadFind[ InfraWalk, count,
    { walk1, walk2 } |->
      If[ Last[ walk1 ] === First[ walk2 ], { Join[ walk1, Rest @ walk2 ] }, { } ],
    path1, path2 ]


(* ===================== Scene-DSL constructor ===================== *)

(* InfraWalk[v1, v2, ..., vk] inside a scene is the literal walk with the
   given vertices, valid iff each consecutive pair is a graph edge.  Non-
   simple chains kept (no DuplicateFreeQ filter). *)

dispatchConstruction[ graph_Graph, InfraWalk[ vs__ ] ] :=
  With[ { walk = { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { walk },
      { } ]
  ]
