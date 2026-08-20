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

(* A walk from p1 to p2 (not necessarily simple, not necessarily geodesic).
   kspec restricts walk length; Properties impose per-step constraints:
   "Simple" (no revisits), "ShortestPath"+Window, "LongestPath"+Window,
   "EdgeMin"|"EdgeMax" with user-supplied f[a, b].  Properties -> {} (default)
   is honestly walk-valued -- the frontier sweep, no simplicity assumed;
   Properties -> {"Simple"} alone takes the FindPath fast path, since that is
   exactly what FindPath already computes.  Method is "Exhaustive" (default,
   BFS) with optional Pruning, or "Greedy" (DFS, one instance). *)

FindInfraWalk::badproperty = "Property `1` is not supported by FindInfraWalk.";
FindInfraWalk::badmethod   = "Method `1` is not supported by FindInfraWalk.";

Options[ FindInfraWalk ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraWalk[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    { q1, q2 } |-> If[ q1 === q2, { },
      Catch @ With[ {
          properties = OptionValue[ FindInfraWalk, { opts }, Properties ],
          methodSpec = OptionValue[ FindInfraWalk, { opts }, Method ] /. Automatic -> "Exhaustive" },
        { methodHead = methodName @ methodSpec,
          pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          fastPathQ  = properties === { "Simple" } },
        Switch[ methodHead,
          "Exhaustive",
            If[ fastPathQ,
              If[ kspec === Infinity && countLimit[ count ] === 1,
                (* one length-unconstrained simple path: FindPath's DFS can wander for
                   minutes on mesh-like graphs, and a geodesic is simple -- the canonical
                   witness *)
                With[ { path = FindShortestPath[ graph, q1, q2 ] },
                  If[ path === { }, { }, { path } ] ],
                Replace[
                  FindPath[ graph, q1, q2, kspec, count /. UpTo[ n_ ] :> n ],
                  Except[ _List ] -> { } ] ],
              If[ properties === { } && kspec === Infinity,
                (* Properties -> {} is the truly unconstrained case: with revisits
                   allowed and no length bound, the walk space is an infinite class
                   (arbitrarily long walks always exist) -- the geodesic is the
                   canonical bounded witness, exactly as the fast path's own count-1
                   shortcut already treats it.  A non-trivial property (e.g.
                   "ShortestPath", "Simple") can itself bound the class even at
                   kspec = Infinity, so this shortcut must not preempt those. *)
                With[ { path = FindShortestPath[ graph, q1, q2 ] },
                  If[ path === { }, { }, { path } ] ],
                (* kspec bounds the sweep depth, not just a post-filter: with revisits
                   allowed the walk space is infinite past kmax.  Early stop on count is
                   sound only when kspec has no lower bound -- the BFS completes short
                   walks first, and those would fill the quota yet fail the filter. *)
                With[ { kmax = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
                        candidateFn = makeCandidateFn[ graph, allNeighboursBaseFn,
                          properties, FindInfraWalk ] },
                  Select[
                    frontierSweep[ graph, q1, q2,
                      { g, path } |-> If[ Length[ path ] - 1 >= kmax, { },
                        candidateFn[ g, path ] ],
                      pruning,
                      If[ MatchQ[ kspec, { _Integer } | { _Integer, _Integer } ],
                        Infinity, countLimit @ count ] ],
                    walkLengthAdmissibleQ[ kspec ] ] ]
              ]
            ],
          "Greedy",
            If[ properties === { } && kspec === Infinity,
              (* unbounded greedy descent can cycle forever once revisits are honest
                 and nothing bounds the class; a geodesic is a walk, so it is the
                 canonical bounded witness (mirrors the "Exhaustive" honest branch) *)
              With[ { path = FindShortestPath[ graph, q1, q2 ] },
                If[ path === { }, { }, { path } ] ],
              With[ { kmax = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
                      candidateFn = makeCandidateFn[ graph, allNeighboursBaseFn,
                        properties, FindInfraWalk ] },
                Select[
                  greedyFrontierSweep[ graph, q1, q2, candidateFn, First, kmax ],
                  walkLengthAdmissibleQ[ kspec ] ] ]
            ],
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


(* ===================== ExtendInfraWalk ===================== *)

(* ExtendInfraWalk[g, walk, n] extends a walk by per-step rules until
   inextensible ("Length" -> Automatic) or for the requested step budget.
   Same Properties as FindInfraWalk; default Properties -> {} allows non-simple
   extensions. *)

ExtendInfraWalk::badproperty  = "Property `1` is not supported by ExtendInfraWalk.";
ExtendInfraWalk::badmethod    = "Method `1` is not supported by ExtendInfraWalk.";
ExtendInfraWalk::baddirection = "Direction `1` is not supported by ExtendInfraWalk.";

Options[ ExtendInfraWalk ] = {
  Properties  -> { },
  Method      -> "Exhaustive",
  "Length"    -> Automatic,
  "Direction" -> "BothSides"
};

ExtendInfraWalk[ graph_Graph, path_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    walk0 |-> If[ Length[ walk0 ] < 1, { walk0 },
      Catch @ With[ {
          properties = OptionValue[ ExtendInfraWalk, { opts }, Properties ],
          methodSpec = OptionValue[ ExtendInfraWalk, { opts }, Method ] /. Automatic -> "Exhaustive",
          direction  = OptionValue[ ExtendInfraWalk, { opts }, "Direction" ],
          length     = OptionValue[ ExtendInfraWalk, { opts }, "Length" ] },
        { methodHead = methodName @ methodSpec,
          pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        If[ methodHead =!= "Exhaustive",
          Message[ ExtendInfraWalk::badmethod, methodSpec ]; Throw[ $Failed ] ];
        With[ { candidateFn = makeCandidateFn[ graph, allNeighboursBaseFn,
                                properties, ExtendInfraWalk ],
                simpleQ     = MemberQ[ properties, "Simple" | { "Simple" } ] },
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
