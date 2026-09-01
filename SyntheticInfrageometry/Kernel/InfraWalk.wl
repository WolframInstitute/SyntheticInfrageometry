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

(* A walk from p1 to p2, non-backtracking with a prescribed number of
   self-crossings.  Option "Crossings" -> c counts the arrivals at an
   already-visited vertex: c = 0 (default) is exactly the simple paths --
   embedded curves, FindPath's class, so Method -> Automatic delegates to
   FindPath whatever the count -- and c >= 1 the immersed curves crossing
   themselves exactly c times (a bounce a,b,a is a cusp, not a crossing, and
   non-backtracking excludes it; winding pays one crossing per revisited
   vertex, so the C4 walk 1,2,3,4,1,2,3 has three).  For c >= 1, Automatic
   draws kinked walks at random for a bounded count ("RandomGreedy",
   ambient-seeded: a uniform random simple backbone, then c loop splices, each
   loop drawn uniformly in the graph punctured by the walk so the count is
   exact, with germs twisted by the link rotation so the crossing is
   transversal where the substrate carries one; option "LoopLength" ->
   Automatic | {lmin, lmax} confines each loop's length -- the floor keeps
   loops off the girth.  Under Automatic the lasso fallback covers walks the
   splices cannot reach, e.g. windings) and enumerates exhaustively under All;
   kspec must be finite (::unbounded).  Local geodesic rules live on
   FindInfraGeodesic, and the unrestricted revisits-allowed walk class is
   FindInfraGeodesic at scale 1. *)

FindInfraWalk::properties  = "FindInfraWalk no longer takes Properties; use \"Crossings\" -> 0 (default, simple paths) | c. Local geodesic rules live on FindInfraGeodesic.";
FindInfraWalk::simple      = "FindInfraWalk now counts self-crossings; use \"Crossings\" -> 0 (default, simple paths) | c.";
FindInfraWalk::badmethod   = "Method `1` is not supported by FindInfraWalk.";
FindInfraWalk::shortfall   = "\"RandomGreedy\" drew `1` distinct walks of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";
FindInfraWalk::unbounded   = "with \"Crossings\" -> c >= 1 the class is infinite without a length bound; give kspec a finite bound.";

Options[ FindInfraWalk ] = {
  "Crossings"  -> 0,
  "LoopLength" -> Automatic,
  Method       -> Automatic
};

FindInfraWalk[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    { q1, q2 } |-> If[ q1 === q2, { },
      Catch @ With[ {
          crossings = OptionValue[ FindInfraWalk,
            FilterRules[ { opts }, Options @ FindInfraWalk ], "Crossings" ],
          loopSpec = OptionValue[ FindInfraWalk,
            FilterRules[ { opts }, Options @ FindInfraWalk ], "LoopLength" ],
          methodOpt = OptionValue[ FindInfraWalk,
            FilterRules[ { opts }, Options @ FindInfraWalk ], Method ] },
        { embeddedQ = crossings === 0 },
        { methodSpec = resolveMethod[
            If[ methodOpt === Automatic,
              Which[ embeddedQ, "Exhaustive",
                countLimit[ count ] === Infinity, "Exhaustive",
                True, "RandomGreedy" ],
              methodOpt ], count ] },
        { methodHead = methodName @ methodSpec,
          pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax       = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          lengthQ    = walkLengthAdmissibleQ[ kspec ] },
        { acceptQ = w |-> lengthQ[ w ] && walkCrossings[ w ] === crossings,
          (* non-backtracking steps, and a step onto a visited vertex only while the
             crossing budget lasts: excess is monotone, so the descent never leaves
             the exact class; at c = 0 this is precisely the simple-path filter *)
          candidateFn = { g, path } |-> With[
            { nb = If[ Length[ path ] < 2, allNeighboursBaseFn[ g, path ],
                DeleteCases[ allNeighboursBaseFn[ g, path ], path[[ -2 ]] ] ] },
            If[ walkCrossings[ path ] < crossings, nb,
              Select[ nb, ! MemberQ[ path, # ] & ] ] ] },
        If[ ! FreeQ[ { opts }, Properties ],
          Message[ FindInfraWalk::properties ]; Throw[ $Failed ] ];
        If[ ! FreeQ[ { opts }, "Simple" ],
          Message[ FindInfraWalk::simple ]; Throw[ $Failed ] ];
        (* a crossing walk of every excess length exists once one does, so the class
           has no canonical finite witness -- refuse, as FindInfraGeodesic does *)
        If[ ! embeddedQ && kspec === Infinity,
          Message[ FindInfraWalk::unbounded ]; Throw[ $Failed ] ];
        Switch[ methodHead,
          "Exhaustive",
            If[ embeddedQ,
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
                 completes short walks first, and a short completion can fill the quota
                 while still under the crossing budget.  A crossing walk may pass through
                 q2, so the sweep is non-terminal. *)
              Select[
                frontierSweep[ graph, q1, q2,
                  { g, path } |-> If[ Length[ path ] - 1 >= kmax, { },
                    candidateFn[ g, path ] ],
                  pruning, Infinity, False ],
                acceptQ ]
            ],
          "Greedy",
            greedyFrontierSweep[ graph, q1, q2, candidateFn, acceptQ, kmax, count,
              Identity, embeddedQ ],
          "RandomGreedy",
            If[ embeddedQ,
              randomDraws[
                { } |-> greedyFrontierSweep[ graph, q1, q2, candidateFn, acceptQ, kmax, 1,
                  randomBranch, True ],
                count, FindInfraWalk ],
              (* a blind random descent almost never ends at a distant q2, so a draw is
                 assembled: a uniform random simple backbone in the metric ellipse
                 { v : d(q1,v) + d(v,q2) <= d + slack }, then c kink splices -- each
                 loop drawn uniformly in the graph punctured by the walk, so the splice
                 pays exactly one crossing, with germs twisted by the link rotation
                 where the substrate carries one, so the crossing is transversal.
                 "LoopLength" confines each loop's length; under Automatic a failed
                 kinked draw falls back to the lasso splicer, whose overlapping loops
                 reach the winding walks the punctured splices cannot.  Any crossing
                 walk contains a loop vertex v with d(q1,v) + |C| + d(v,q2) <= its
                 length, so no admissible v proves the class empty. *)
              With[ {
                  dist1 = AssociationThread[ VertexList @ graph, GraphDistance[ graph, q1 ] ],
                  dist2 = AssociationThread[ VertexList @ graph, GraphDistance[ graph, q2 ] ],
                  kmin  = Replace[ kspec, { { lo_, _ } :> lo, { k_Integer } :> k, _ -> 0 } ] },
                { anchors = Select[ VertexList @ graph, dist1[ # ] + dist2[ # ] + 3 <= kmax & ] },
                If[ anchors === { }, { },
                  With[ { d = dist1[ q2 ] },
                    { loopWin = Replace[ loopSpec, Automatic :>
                        With[ { share = Floor[ ( kmax - d ) / crossings ] },
                          { Max[ 3, Ceiling[ share / 2 ] ], Max[ 3, share ] } ] ] },
                    { slack = Max[ 0, Min[ 4, kmax - d - crossings * loopWin[[ 1 ]] ] ] },
                    { workGraph = ellipseSubgraph[ graph, dist1, dist2,
                        d + slack + loopWin[[ 2 ]] + 4 ] },
                    { backboneSampler = nbWalkSampler[
                        ellipseSubgraph[ workGraph, dist1, dist2, d + slack ],
                        q1, q2, { d, d + slack } ] },
                    randomDraws[
                      { } |-> With[ { w = kinkedWalkDraw[ workGraph, backboneSampler,
                            crossings, loopWin ] },
                        Which[
                          ListQ @ w && acceptQ @ w, { w },
                          loopSpec =!= Automatic, { },
                          True,
                            Module[ { walk, splices = 0, u, loop, pos, spliced },
                              walk = With[ { v = RandomChoice @ anchors },
                                { loops = sprayLoopDraw[ graph, v,
                                    If[ crossings === 1, Max[ 3, kmin - dist1[ v ] - dist2[ v ] ], 3 ],
                                    kmax - dist1[ v ] - dist2[ v ] ] },
                                If[ loops === { }, $Failed,
                                  With[ { good = Select[
                                      ( Join[ FindShortestPath[ graph, q1, v ], Rest @ #,
                                          Rest @ FindShortestPath[ graph, v, q2 ] ] & ) /@
                                        { First @ loops, Reverse @ First @ loops },
                                      nonBacktrackingQ ] },
                                    If[ good === { }, $Failed, RandomChoice @ good ] ] ] ];
                              While[ walk =!= $Failed && walkCrossings[ walk ] < crossings &&
                                  Length[ walk ] + 2 <= kmax && splices < 8,
                                splices++;
                                u = RandomChoice @ DeleteDuplicates @ walk;
                                loop = sprayLoopDraw[ graph, u, 3, kmax - Length[ walk ] + 1 ];
                                If[ loop =!= { },
                                  pos = RandomChoice @ Flatten @ Position[ walk, u, { 1 }, Heads -> False ];
                                  spliced = Select[
                                    ( Join[ walk[[ ;; pos ]], Rest @ #, walk[[ pos + 1 ;; ]] ] & ) /@
                                      { First @ loop, Reverse @ First @ loop },
                                    nonBacktrackingQ ];
                                  If[ spliced =!= { }, walk = RandomChoice @ spliced ] ] ];
                              If[ walk =!= $Failed && acceptQ[ walk ], { walk }, { } ] ] ] ],
                      count, FindInfraWalk ] ] ] ] ],
          _,
            Message[ FindInfraWalk::badmethod, methodSpec ]; $Failed
        ]
      ]
    ], p1, p2 ]


(* Path-family base candidate function: every adjacent vertex of Last @ path;
   backtracking and the crossing budget are filtered by the caller. *)

allNeighboursBaseFn[ g_Graph, path_List ] := AdjacencyList[ g, Last @ path ]


(* crossings of a walk: arrivals at an already-visited vertex *)
walkCrossings[ w_List ] := Length[ w ] - Length[ DeleteDuplicates[ w ] ]

(* immersed steps only: no a,b,a cusp anywhere *)
nonBacktrackingQ[ w_List ] := AllTrue[ Partition[ w, 3, 1 ], #[[ 1 ]] =!= #[[ 3 ]] & ]

(* one random loop at u of length within [lo, hi]: a geodesic spoke out to a
   random vertex at rank r (r uniform up to hi/2), then a shortest return
   computed with the spoke's interior deleted, so the return must round the
   corridor and the loop is a genuine simple cycle through u -- fat at every
   radius, never a glued-geodesic sliver, and with no survivorship bias toward
   short loops.  { } when the return misses the length window -- the caller
   retries. *)
sprayLoopDraw[ graph_Graph, u_, lo_Integer, hi_Integer ] :=
  If[ hi < 3, { },
    With[ { distU = AssociationThread[ VertexList @ graph, GraphDistance[ graph, u ] ] },
      { r = RandomInteger[ { 1, Max[ 1, Floor[ hi / 2 ] ] } ] },
      { sphere = Select[ VertexList @ graph, distU[ # ] === r & ] },
      If[ sphere === { }, { },
        With[ { spoke = FindShortestPath[ graph, u, RandomChoice @ sphere ] },
          { cut = If[ Length[ spoke ] == 2, EdgeDelete[ graph, UndirectedEdge @@ spoke ],
              VertexDelete[ graph, spoke[[ 2 ;; -2 ]] ] ] },
          { return = Quiet @ FindShortestPath[ cut, Last @ spoke, u ] },
          If[ return === { } ||
              ! ( Max[ 3, lo ] <= Length[ spoke ] + Length[ return ] - 2 <= hi ), { },
            { Join[ spoke, Rest @ return ] } ] ] ] ] ]


(* walks q1 -> q2 of length <= budget live exactly in the metric ellipse *)
ellipseSubgraph[ graph_, dist1_, dist2_, budget_ ] :=
  Subgraph[ graph, Select[ VertexList @ graph, dist1[ # ] + dist2[ # ] <= budget & ] ]


(* backward walk counts on directed edges (the non-backtracking transfer
   matrix), consumed by nbWalkDraw: one draw is exactly uniform over the
   non-backtracking walks a -> b with length in the window.  $Failed when the
   class is empty. *)
nbWalkSampler[ graph_, a_, b_, { lmin_, lmax_ } ] := Module[
  { de, idx, sIdx, transfer, seedVec, counts, starts, totals },
  If[ lmax < 1 || ! MemberQ[ VertexList @ graph, a ] || ! MemberQ[ VertexList @ graph, b ],
    Return[ $Failed, Module ] ];
  de = Join[ #, Reverse /@ # ] & @ ( List @@@ EdgeList @ graph );
  If[ de === { }, Return[ $Failed, Module ] ];
  idx = AssociationThread[ de, Range @ Length @ de ];
  sIdx = Table[
    idx[ { edge[[ 2 ]], # } ] & /@ DeleteCases[ AdjacencyList[ graph, edge[[ 2 ]] ], edge[[ 1 ]] ],
    { edge, de } ];
  transfer = SparseArray[
    Flatten @ Table[ { i, j } -> 1, { i, Length @ de }, { j, sIdx[[ i ]] } ],
    { Length @ de, Length @ de } ];
  seedVec = SparseArray[ Table[ If[ de[[ i, 2 ]] === b, 1, 0 ], { i, Length @ de } ] ];
  counts = NestList[ transfer . # &, seedVec, lmax - 1 ];
  starts = Flatten @ Position[ de, { a, _ } ];
  totals = Table[ Total @ counts[[ l, starts ]], { l, Max[ 1, lmin ], lmax } ];
  If[ starts === { } || Total @ totals == 0, $Failed,
    <| "Edges" -> de, "Successors" -> sIdx, "Counts" -> counts, "Starts" -> starts,
       "Totals" -> totals, "Window" -> { Max[ 1, lmin ], lmax } |> ] ]

nbWalkDraw[ s_ ] := Module[ { len, e, walk },
  len = RandomChoice[ s[ "Totals" ] -> Range[ s[ "Window" ][[ 1 ]], s[ "Window" ][[ 2 ]] ] ];
  e = RandomChoice[ Normal @ s[ "Counts" ][[ len, s[ "Starts" ] ]] -> s[ "Starts" ] ];
  walk = s[ "Edges" ][[ e ]];
  Do[
    e = RandomChoice[
      Normal @ s[ "Counts" ][[ len - step, s[ "Successors" ][[ e ]] ]] -> s[ "Successors" ][[ e ]] ];
    walk = Append[ walk, s[ "Edges" ][[ e, 2 ]] ], { step, 1, len - 1 } ];
  walk ]


(* germ pair of a kink at u with walk arms a1, a2: when the link of u is a
   cycle (a rotation), both germs in one arc cut by the arms in the twisted
   cyclic order (a1, n2, n1, a2) -- the pairing that interleaves the branches,
   so the crossing is transversal; without a rotation, any two distinct
   punctured neighbours. *)
kinkGermPair[ ball_, punct_, u_, a1_, a2_ ] := With[
  { lk = With[ { sub = Subgraph[ ball, AdjacencyList[ ball, u ] ] },
      If[ ConnectedGraphQ[ sub ] && Union[ VertexDegree[ sub ] ] === { 2 },
        First @ FindCycle[ sub, { VertexCount[ sub ] }, 1 ] /. UndirectedEdge[ x_, _ ] :> x,
        $Failed ] ] },
  If[ lk === $Failed,
    With[ { cands = Select[ AdjacencyList[ ball, u ], MemberQ[ VertexList @ punct, # ] & ] },
      If[ Length @ cands < 2, $Failed, RandomSample[ cands, 2 ] ] ],
    With[ { rot = RotateLeft[ lk, First @ FirstPosition[ lk, a1 ] - 1 ] },
      { k = First @ FirstPosition[ rot, a2 ] },
      { arcs = Select[
          Select[ #, MemberQ[ VertexList @ punct, # ] & ] & /@
            { rot[[ 2 ;; k - 1 ]], Reverse @ rot[[ k + 1 ;; ]] },
          Length[ # ] >= 2 & ] },
      If[ arcs === { }, $Failed,
        With[ { arc = RandomChoice @ arcs },
          { ij = Sort @ RandomSample[ Range @ Length @ arc, 2 ] },
          { arc[[ ij[[ 2 ]] ]], arc[[ ij[[ 1 ]] ]] } ] ] ] ] ]


(* one kink: splice a loop at walk[[p]], drawn uniformly in the ball punctured
   by the walk -- the splice pays exactly one crossing and retraces no edge --
   with loop length in the window *)
kinkSplice[ graph_, walk_, p_, { lmin_, lmax_ } ] := Module[
  { u = walk[[ p ]], ball, punct, germs, s, path },
  ball = NeighborhoodGraph[ graph, u, Ceiling[ lmax / 2 ] + 1 ];
  punct = VertexDelete[ ball,
    Intersection[ VertexList @ ball, DeleteCases[ DeleteDuplicates @ walk, u ] ] ];
  germs = kinkGermPair[ ball, punct, u, walk[[ p - 1 ]], walk[[ p + 1 ]] ];
  If[ germs === $Failed, Return[ $Failed, Module ] ];
  s = nbWalkSampler[ VertexDelete[ punct, { u } ], germs[[ 1 ]], germs[[ 2 ]],
    { Max[ 1, lmin - 2 ], lmax - 2 } ];
  If[ s === $Failed, Return[ $Failed, Module ] ];
  path = SelectFirst[ Table[ nbWalkDraw @ s, 40 ], DuplicateFreeQ ];
  If[ MissingQ @ path, $Failed, Join[ walk[[ ;; p ]], path, walk[[ p ;; ]] ] ] ]


(* one kinked-walk draw: a uniform random simple backbone off the prepared
   sampler, then c kink splices at random positions *)
kinkedWalkDraw[ workGraph_, backboneSampler_, crossings_, loopWin_ ] := Module[
  { walk, try },
  If[ backboneSampler === $Failed, Return[ $Failed, Module ] ];
  walk = SelectFirst[ Table[ nbWalkDraw @ backboneSampler, 40 ], DuplicateFreeQ ];
  If[ MissingQ @ walk || Length @ walk < 3, Return[ $Failed, Module ] ];
  Do[
    try = SelectFirst[
      Table[ Quiet @ kinkSplice[ workGraph, walk,
          RandomInteger[ { 2, Length[ walk ] - 1 } ], loopWin ], 20 ],
      ListQ, $Failed ];
    If[ try === $Failed, Return[ $Failed, Module ], walk = try ],
    crossings ];
  walk ]


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


(* ===================== WalkSingularities ===================== *)

(* Singularity census of a walk read as a parametrized curve -- an invariant
   of the vertex sequence alone, no substrate structure enters.  Around every
   coincidence v_i === v_j lies a maximal matching block, same-direction or
   reversed; the species is the shape of that block.  "Cusp": apexes i with
   v_{i-1} === v_{i+1}; the mirrored arc around an apex belongs to the cusp,
   however deep the retrace (a,b,c,d,c,b,a is one cusp at d).  "Tangency"
   (self-tangency): maximal repeated blocks of length >= 2 with disjoint
   parameter ranges, as the range pair {{i1, i2}, {j1, j2}} -- the second
   range ascends when the arc is re-run in the same direction (direct) and
   descends when it is retraced in reverse (inverse).  "Crossing": isolated
   double visits (multiplicity 2, both passes interior, no block extension)
   -- the transverse double point, the one multiple point a generic curve
   has.  "TriplePoint": all parameters of vertices of multiplicity >= 3.
   Open walks add "EndpointIncidence" (v_1 or v_n of multiplicity >= 2);
   closed walks (InfraLoop / InfraString, parameters on the cyclic core)
   instead add "Cover" -> q: a core with minimal period p < m is the q = m/p
   fold cover of its period loop, whose census is returned. *)

WalkSingularities[ w_InfraWalk ] := openCensus /@ First @ w

WalkSingularities[ w : _InfraLoop | _InfraString ] := closedCensus /@ First @ w

WalkSingularities[ walk_List ] := openCensus @ walk


openCensus[ walk_List ] := With[
  { m = Length @ walk, index = PositionIndex @ walk },
  <|
    "Cusp" -> Select[ Range[ 2, m - 1 ], i |-> walk[[ i - 1 ]] === walk[[ i + 1 ]] ],
    "Tangency" -> Join[ directBlocks @ walk, inverseBlocks @ walk ],
    "Crossing" -> Select[ Values @ index,
      ps |-> Length[ ps ] == 2 && 1 < First[ ps ] && Last[ ps ] < m &&
        isolatedPairQ[ walk, ps ] ],
    "TriplePoint" -> Select[ Values @ index, ps |-> Length[ ps ] >= 3 ],
    "EndpointIncidence" -> If[ m < 2, { },
      Select[ Lookup[ index, DeleteDuplicates @ { First @ walk, Last @ walk } ],
        ps |-> Length[ ps ] >= 2 ] ]
  |> ]


closedCensus[ rep_List ] := With[
  { core = Most @ closeWalk @ rep },
  { period = SelectFirst[ Divisors @ Length @ core,
      d |-> core === RotateLeft[ core, d ] ] },
  Append[ cyclicCensus @ core[[ ;; period ]], "Cover" -> Length[ core ] / period ] ]


cyclicCensus[ core_List ] := With[
  { m = Length @ core, index = PositionIndex @ core },
  <|
    "Cusp" -> Select[ Range @ m,
      i |-> core[[ Mod[ i - 2, m ] + 1 ]] === core[[ Mod[ i, m ] + 1 ]] ],
    "Tangency" -> Join[ cyclicDirectBlocks @ core, cyclicInverseBlocks @ core ],
    "Crossing" -> Select[ Values @ index,
      ps |-> Length[ ps ] == 2 && cyclicIsolatedPairQ[ core, ps ] ],
    "TriplePoint" -> Select[ Values @ index, ps |-> Length[ ps ] >= 3 ]
  |> ]


(* an arc traversed twice in the same direction, v_{i+t} === v_{i+d+t}; the
   two ranges are disjoint iff the run is no longer than the shift d *)
directBlocks[ walk_List ] := With[ { m = Length @ walk },
  Catenate @ Table[
    { { First @ #, Last @ # }, { First @ # + d, Last @ # + d } } & /@
      Select[ maximalRuns @ Select[ Range[ m - d ], i |-> walk[[ i ]] === walk[[ i + d ]] ],
        run |-> 2 <= Length[ run ] <= d ],
    { d, 2, m - 1 } ] ]


(* an arc retraced in reverse, v_i === v_{s-i}; a run reaching the apex s/2
   is the mirror of a cusp, not a tangency *)
inverseBlocks[ walk_List ] := With[ { m = Length @ walk },
  Catenate @ Table[
    { { First @ #, Last @ # }, { s - First @ #, s - Last @ # } } & /@
      Select[ maximalRuns @ Select[ Range[ Max[ 1, s - m ], Floor[ ( s - 1 ) / 2 ] ],
          i |-> walk[[ i ]] === walk[[ s - i ]] ],
        run |-> Length[ run ] >= 2 && ! ( EvenQ[ s ] && Last[ run ] == s / 2 - 1 ) ],
    { s, 3, 2 m - 1 } ] ]


cyclicDirectBlocks[ core_List ] := With[
  { m = Length @ core },
  { cyc = i |-> Mod[ i - 1, m ] + 1 },
  DeleteDuplicatesBy[
    Catenate @ Table[
      { { First @ #, Last @ # }, { cyc[ First @ # + d ], cyc[ Last @ # + d ] } } & /@
        Select[ cyclicRuns[ Select[ Range @ m, i |-> core[[ i ]] === core[[ cyc[ i + d ] ]] ], m ],
          run |-> 2 <= Length[ run ] <= d ],
      { d, 2, Floor[ m / 2 ] } ],
    entry |-> Sort[ Sort /@ entry ] ] ]


cyclicInverseBlocks[ core_List ] := With[
  { m = Length @ core },
  { cyc = i |-> Mod[ i - 1, m ] + 1 },
  { sigma = { s, i } |-> cyc[ s - i ] },
  DeleteDuplicatesBy[
    Catenate @ Table[
      { { First @ #, Last @ # }, { sigma[ s, First @ # ], sigma[ s, Last @ # ] } } & /@
        Select[ cyclicRuns[
            Select[ Range @ m,
              i |-> sigma[ s, i ] =!= i && core[[ i ]] === core[[ sigma[ s, i ] ]] ],
            m ],
          run |-> Length[ run ] >= 2 &&
            NoneTrue[ run,
              i |-> sigma[ s, i ] === cyc[ i + 2 ] || sigma[ s, i ] === cyc[ i - 2 ] ] ],
      { s, 0, m - 1 } ],
    entry |-> Sort[ Sort /@ entry ] ] ]


maximalRuns[ set_List ] := Split[ Sort @ set, #2 == #1 + 1 & ]


cyclicRuns[ set_List, m_ ] := With[
  { runs = maximalRuns @ set },
  If[ Length[ runs ] >= 2 && First[ First @ runs ] == 1 && Last[ Last @ runs ] == m,
    Prepend[ runs[[ 2 ;; -2 ]], Join[ Last @ runs, First @ runs ] ],
    runs ] ]


(* no extension of the coincidence in any direction: the two visits share no
   preceding or following vertex *)
isolatedPairQ[ walk_List, { i_, j_ } ] := With[
  { match = { p, q } |-> 1 <= p <= Length @ walk && 1 <= q <= Length @ walk &&
      walk[[ p ]] === walk[[ q ]] },
  ! ( match[ i - 1, j - 1 ] || match[ i + 1, j + 1 ] ||
      match[ i - 1, j + 1 ] || match[ i + 1, j - 1 ] ) ]


cyclicIsolatedPairQ[ core_List, { i_, j_ } ] := With[
  { m = Length @ core },
  { match = { p, q } |-> core[[ Mod[ p - 1, m ] + 1 ]] === core[[ Mod[ q - 1, m ] + 1 ]] },
  ! ( match[ i - 1, j - 1 ] || match[ i + 1, j + 1 ] ||
      match[ i - 1, j + 1 ] || match[ i + 1, j - 1 ] ) ]


(* ===================== InfraImmersedQ / InfraGenericQ ===================== *)

(* immersed walk: a walk with no cusp; the hierarchy walk > immersed > generic
   never tightens InfraWalkQ *)

InfraImmersedQ[ graph_Graph, w_InfraWalk ] := AllTrue[ First @ w, InfraImmersedQ[ graph, # ] & ]

InfraImmersedQ[ graph_Graph, w : _InfraLoop | _InfraString ] :=
  AllTrue[ First @ w,
    InfraWalkQ[ graph, closeWalk @ # ] && closedCensus[ # ][ "Cusp" ] === { } & ]

InfraImmersedQ[ graph_Graph, walk_List ] :=
  InfraWalkQ[ graph, walk ] && openCensus[ walk ][ "Cusp" ] === { }


(* generic walk: immersed and in general position -- multiple points are only
   isolated crossings; no tangencies, no triple points, endpoints off the
   curve on open walks, singly covered on closed walks *)

InfraGenericQ[ graph_Graph, w_InfraWalk ] := AllTrue[ First @ w, InfraGenericQ[ graph, # ] & ]

InfraGenericQ[ graph_Graph, w : _InfraLoop | _InfraString ] :=
  AllTrue[ First @ w,
    InfraWalkQ[ graph, closeWalk @ # ] &&
      With[ { c = closedCensus @ # },
        Lookup[ c, { "Cusp", "Tangency", "TriplePoint" } ] === { { }, { }, { } } &&
          c[ "Cover" ] === 1 ] & ]

InfraGenericQ[ graph_Graph, walk_List ] :=
  InfraWalkQ[ graph, walk ] &&
  Lookup[ openCensus @ walk, { "Cusp", "Tangency", "TriplePoint", "EndpointIncidence" } ] ===
    { { }, { }, { }, { } }


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
