Package["WolframInstitute`SyntheticInfrageometry`"]


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

(* The pointed form FindInfraWalk[g, p1, kspec, count] grows walks from p1 --
   the primitive construction: growth from a seed under the Properties rules
   (FindInfraGeodesic's vocabulary read on the whole walk, scale-Infinity
   windows) until a stopping condition fires, the length budget kspec is
   spent, or no admissible step remains.  The default class {"Generic"} is
   the generic immersed walks, InfraGenericQ's class read per step: no cusps,
   no tangencies, no triple points, every multiple point an isolated
   transverse crossing; a stopped walk's tip may still sit on a crossing --
   endpoint freeness is the census condition InfraGenericQ adds on the
   finished curve.  Method -> "RandomGreedy" on the pointed form is the
   random walk: one uniform admissible step at a time.  "Generic", "Simple"
   and whole-walk "Minimizing" bound the class by themselves, so kspec
   Infinity is legal under the default; without a bounding rule it is
   refused (::unbounded) -- a stopping condition cannot bound, since it may
   never fire.

   The two-point form FindInfraWalk[g, p1, p2, kspec, count] is sugar over
   the same class: the endpoint is one stopping condition among many -- the
   walks ending at p2, with endpoint freeness at both ends under "Generic".
   The pointed reading wins a positional tie, so a target that matches the
   kspec grammar (a bare integer, {k}, {lo, hi}) is written InfraPoint[p2].

   "StoppingCondition" takes event -> action rules; a bare event means
   -> "Stop".  Events are the singularity names ("Crossing", "Tangency",
   "TriplePoint", "Cusp": the step creates one), the constraint rules
   ("Minimizing", "Simple", "Immersed", "Generic": the step violates one),
   or a predicate on the walk so far.  Actions are "Stop" and {"Stop",
   "Delay" -> k, "Count" -> c}: stop k steps after the c-th firing.
   Entries race and the earliest deadline wins; a deadline only tightens
   the budget, and events fire between steps, so there is no event
   location. *)

FindInfraWalk::badproperty = "Property `1` is not supported by FindInfraWalk; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the walk.";
FindInfraWalk::badmethod   = "Method `1` is not supported by FindInfraWalk.";
FindInfraWalk::shortfall   = "\"RandomGreedy\" drew `1` distinct walks of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";
FindInfraWalk::unbounded   = "the walk class is infinite without a length bound; give kspec a finite bound or a bounding rule (\"Simple\", \"Generic\", \"Minimizing\").";
FindInfraWalk::badevent    = "Stopping condition `1` is not supported; entries are event or event -> \"Stop\" | {\"Stop\", \"Delay\" -> k, \"Count\" -> c}, with event a singularity name (\"Crossing\", \"Tangency\", \"TriplePoint\", \"Cusp\"), a constraint rule, or a predicate on the walk so far.";
FindInfraWalk::deadevent   = "the event `1` can never fire under the Properties constraints; the walk runs to its budget.";

Options[ FindInfraWalk ] = {
  Properties          -> { "Generic" },
  "StoppingCondition" -> { },
  Method              -> Automatic
};

FindInfraWalk[ graph_Graph, p1_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    q1 |-> Catch @ With[ {
        rules  = OptionValue[ FindInfraWalk, { opts }, Properties ],
        events = parseStoppingEvents[
          OptionValue[ FindInfraWalk, { opts }, "StoppingCondition" ], FindInfraWalk ],
        methodSpec = resolveMethod[ OptionValue[ FindInfraWalk, { opts }, Method ], count ] },
      { methodHead  = methodName @ methodSpec,
        pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
        kmax        = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
        lengthQ     = walkLengthAdmissibleQ[ kspec ],
        candidateFn = windowCandidateFn[ graph, Infinity, rules, FindInfraWalk ],
        deadlineFn  = stoppingDeadlineFn[ graph, Infinity, events ] },
      warnDeadEvents[ events, rules, Infinity, FindInfraWalk ];
      If[ kmax === Infinity && ! MemberQ[ rules, "Simple" | "Generic" | "Minimizing" ],
        Message[ FindInfraWalk::unbounded ]; Throw[ $Failed ] ];
      Switch[ methodHead,
        "Exhaustive",
          pointedFrontierSweep[ graph, q1, candidateFn, lengthQ, deadlineFn, kmax,
            pruning, countLimit @ count ],
        "Greedy",
          pointedGreedySweep[ graph, q1, candidateFn, lengthQ, deadlineFn, kmax, count ],
        "RandomGreedy",
          randomDraws[
            { } |-> pointedGreedySweep[ graph, q1, candidateFn, lengthQ, deadlineFn, kmax,
              1, randomBranch ],
            count, FindInfraWalk ],
        _,
          Message[ FindInfraWalk::badmethod, methodSpec ]; $Failed
      ] ], p1 ]

FindInfraWalk[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    { q1, q2 } |-> If[ q1 === q2, { },
      Catch @ With[ {
          rules  = OptionValue[ FindInfraWalk, { opts }, Properties ],
          events = parseStoppingEvents[
            OptionValue[ FindInfraWalk, { opts }, "StoppingCondition" ], FindInfraWalk ],
          methodOpt = OptionValue[ FindInfraWalk, { opts }, Method ] },
        { methodSpec = resolveMethod[ methodOpt, count ] },
        { methodHead  = methodName @ methodSpec,
          pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax        = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          lengthQ     = walkLengthAdmissibleQ[ kspec ],
          candidateFn = windowCandidateFn[ graph, Infinity, rules, FindInfraWalk ],
          (* a walk in a class forbidding a second visit to its endpoint
             ("Simple", "Generic", or whole-walk "Minimizing", whose distance
             from the start strictly increases) ends at its first arrival, so
             the sweeps may stop branches there; at whole-walk scale these are
             also exactly the rules that bound the class without a length
             bound *)
          terminal    = MemberQ[ rules, "Simple" | "Generic" | "Minimizing" ] },
        { acceptQ = If[ MemberQ[ rules, "Generic" ],
            (* endpoint freeness is the one census condition the moving tip
               cannot prune; checked on the finished walk *)
            w |-> lengthQ[ w ] && Count[ w, First @ w ] === 1 && Count[ w, Last @ w ] === 1,
            lengthQ ],
          stepFn = If[ events === { }, candidateFn,
            With[ { deadlineFn = stoppingDeadlineFn[ graph, Infinity, events ] },
              { g, walk } |-> If[ Length[ walk ] - 1 >= deadlineFn @ walk, { },
                candidateFn[ g, walk ] ] ] ] },
        warnDeadEvents[ events, rules, Infinity, FindInfraWalk ];
        If[ kmax === Infinity && ! terminal,
          Message[ FindInfraWalk::unbounded ]; Throw[ $Failed ] ];
        If[ methodOpt === Automatic && kspec === Infinity && countLimit[ count ] === 1 &&
            events === { } &&
            SubsetQ[ { "Minimizing", "Simple", "Immersed", "Generic" }, rules ],
          (* a geodesic is simple, hence immersed, generic and minimizing --
             the canonical count-less witness, without FindPath's wandering
             DFS *)
          With[ { path = FindShortestPath[ graph, q1, q2 ] },
            If[ path === { }, { }, { path } ] ],
          Switch[ methodHead,
            "Exhaustive",
              Select[
                frontierSweep[ graph, q1, q2,
                  { g, walk } |-> If[ Length[ walk ] - 1 >= kmax, { }, stepFn[ g, walk ] ],
                  pruning,
                  (* early stop on count is unsound when a completion can
                     still be rejected: by an exact / range length, or by
                     the endpoint check *)
                  If[ MatchQ[ kspec, { _Integer } | { _Integer, _Integer } ] ||
                      MemberQ[ rules, "Generic" ],
                    Infinity, countLimit @ count ],
                  terminal ],
                acceptQ ],
            "Greedy",
              greedyFrontierSweep[ graph, q1, q2, stepFn, acceptQ, kmax, count,
                Identity, terminal ],
            "RandomGreedy",
              randomDraws[
                { } |-> greedyFrontierSweep[ graph, q1, q2, stepFn, acceptQ, kmax, 1,
                  randomBranch, terminal ],
                count, FindInfraWalk ],
            _,
              Message[ FindInfraWalk::badmethod, methodSpec ]; $Failed
          ] ] ] ], p1, p2 ]


(* ===================== Pointed growth engines ===================== *)

(* Lazy depth-first growth from p1: descend by candidateFn and emit a walk
   when its budget -- kmax tightened per branch by the stopping-condition
   deadline -- is spent or no admissible step remains; acceptQ (the kspec
   window) filters the emissions, and a finite count is exact because the
   descent is complete.  branch = Identity backtracks ("Greedy"); branch =
   randomBranch follows one uniform step per node and cannot backtrack --
   a single draw of the random walk ("RandomGreedy", via randomDraws). *)

(* the closures are held in Module locals, never inlined into descend's RHS:
   see the note on greedyFrontierSweep (Tools.wl) *)

pointedGreedySweep[ graph_Graph, p1_, candidateFn_, acceptQ_, deadlineFn_, kmax_,
    count_, branch_ : Identity ] :=
  If[ ! VertexQ[ graph, p1 ], { },
    Module[ { cap = countLimit @ count, acc = { }, descend, emit,
              cands = candidateFn, keepQ = acceptQ, dlFn = deadlineFn, pick = branch },
      emit[ walk_ ] := If[ keepQ @ walk,
        AppendTo[ acc, walk ];
        If[ Length @ acc >= cap, Throw[ acc, pointedGreedySweep ] ] ];
      descend[ walk_ ] :=
        If[ Length[ walk ] - 1 >= Min[ kmax, dlFn @ walk ],
          emit[ walk ],
          With[ { nexts = pick @ cands[ graph, walk ] },
            If[ nexts === { }, emit[ walk ],
              Scan[ descend[ Append[ walk, # ] ] &, nexts ] ] ] ];
      Catch[ descend[ { p1 } ]; acc, pointedGreedySweep ]
    ] ]


(* BFS sibling for the exhaustive pointed sweep: applyPruning caps the live
   frontier per layer, and emissions pass acceptQ before they count, so an
   early stop on the count is exact. *)

pointedFrontierSweep[ graph_Graph, p1_, candidateFn_, acceptQ_, deadlineFn_, kmax_,
    prune_, count_ ] :=
  If[ ! VertexQ[ graph, p1 ], { },
    Module[ { frontier = { { p1 } }, completed = { }, moves },
      While[ frontier =!= { } && Length[ completed ] < count,
        moves = ( walk |-> { walk,
            If[ Length[ walk ] - 1 >= Min[ kmax, deadlineFn @ walk ], { },
              candidateFn[ graph, walk ] ] } ) /@ frontier;
        completed = Join[ completed,
          Select[ Cases[ moves, { w_, { } } :> w ], acceptQ ] ];
        frontier = applyPruning[
          Flatten[ Cases[ moves, { w_, nexts : { __ } } :> ( Append[ w, # ] & /@ nexts ) ], 1 ],
          prune ]
      ];
      Take[ completed, UpTo[ count ] ]
    ] ]


(* ===================== Stopping conditions ===================== *)

(* "StoppingCondition" entries normalised to {event, delay, count} triples:
   a bare event is event -> "Stop", and {"Stop", "Delay" -> k, "Count" -> c}
   stops k steps after the c-th firing. *)

parseStoppingEvents[ spec_, fnSym_ ] :=
  parseStoppingEntry[ #, fnSym ] & /@ Replace[ spec, entry : Except[ _List ] :> { entry } ]

parseStoppingEntry[ event_ -> "Stop", fnSym_ ] :=
  { checkStoppingEvent[ event, fnSym ], 0, 1 }

parseStoppingEntry[ event_ -> { "Stop", acts___Rule }, fnSym_ ] :=
  { checkStoppingEvent[ event, fnSym ],
    Lookup[ { acts }, "Delay", 0 ], Lookup[ { acts }, "Count", 1 ] }

parseStoppingEntry[ event : Except[ _Rule ], fnSym_ ] :=
  { checkStoppingEvent[ event, fnSym ], 0, 1 }

parseStoppingEntry[ entry_, fnSym_ ] :=
  ( Message[ MessageName[ fnSym, "badevent" ], entry ]; Throw[ $Failed ] )


$stoppingEventNames = "Minimizing" | "Simple" | "Immersed" | "Generic" |
  "Crossing" | "Tangency" | "TriplePoint" | "Cusp";

checkStoppingEvent[ event_String, fnSym_ ] :=
  If[ MatchQ[ event, $stoppingEventNames ], event,
    Message[ MessageName[ fnSym, "badevent" ], event ]; Throw[ $Failed ] ]

checkStoppingEvent[ event_, _ ] := event


(* a dead event -- awaiting a singularity the constraints exclude -- can
   never fire: warn rather than silently run to the budget *)

warnDeadEvents[ { }, _, _, _ ] := Null

warnDeadEvents[ entries_List, rules_List, scale_, fnSym_ ] :=
  With[ { dead = deadEventNames[ rules, scale ] },
    Scan[
      If[ StringQ @ First @ # && MemberQ[ dead, First @ # ],
        Message[ MessageName[ fnSym, "deadevent" ], First @ # ] ] &,
      entries ] ]


(* the event names each constraint prunes before they can fire.  Whole-walk
   "Minimizing" forbids any revisit (the distance from the start strictly
   increases), so at scale Infinity it excludes every singularity. *)

deadEventNames[ rules_List, scale_ ] := Union @@ Replace[ rules, {
  "Simple" -> { "Simple", "Generic", "Immersed", "Crossing", "Tangency", "TriplePoint", "Cusp" },
  "Generic" -> { "Generic", "Immersed", "Tangency", "TriplePoint", "Cusp" },
  "Immersed" -> { "Immersed", "Cusp" },
  "Minimizing" :> If[ scale === Infinity,
    { "Minimizing", "Simple", "Generic", "Immersed", "Crossing", "Tangency", "TriplePoint", "Cusp" },
    { "Minimizing" } ],
  _ -> { } }, { 1 } ]


(* One incremental detector serves constraints (forbid the step) and events
   (await it): each event is read at the walk's tip.  "Crossing" is an
   arrival at any visited vertex along a fresh edge -- the isolated double
   visit of the census, read at the moment it appears; a later pass along
   the same arc upgrades it, and the upgrade is a "Tangency" firing.
   "Tangency" is a retraced edge off a cusp apex, so the mirrored arc of a
   deep cusp fires it too -- WalkSingularities on the finished walk stays
   the authority.  A rule name fires when the step violates the rule; a
   predicate fires when it turns True on the walk so far. *)

stepEventQ[ g_, scale_, rule : "Minimizing" | "Simple" | "Immersed" | "Generic", walk_ ] :=
  ! windowRuleQ[ g, scale, rule, Most @ walk, Last @ walk ]

stepEventQ[ _, _, "Cusp", walk_ ] :=
  Length[ walk ] >= 3 && walk[[ -3 ]] === walk[[ -1 ]]

stepEventQ[ _, _, "Tangency", walk_ ] :=
  repeatedTipEdgeQ[ walk ] && ! ( Length[ walk ] >= 3 && walk[[ -3 ]] === walk[[ -1 ]] )

stepEventQ[ _, _, "Crossing", walk_ ] :=
  Count[ walk, Last @ walk ] == 2 && ! repeatedTipEdgeQ[ walk ]

stepEventQ[ _, _, "TriplePoint", walk_ ] :=
  Count[ walk, Last @ walk ] >= 3

stepEventQ[ _, _, pred_, walk_ ] := TrueQ[ pred @ walk ]


repeatedTipEdgeQ[ walk_ ] := Length[ walk ] >= 3 &&
  MemberQ[ Sort /@ Partition[ Most @ walk, 2, 1 ], Sort @ walk[[ -2 ;; ]] ]


(* The per-branch event state -- remaining fire counts and the armed deadline
   in edges -- is a function of the walk prefix: memoised per prefix, so each
   frontier walk owns its own and a shared prefix is stepped once.  The
   closure is rebuilt per anchor tuple, so the memo table dies with it. *)

stoppingDeadlineFn[ _, _, { } ] := Infinity &

stoppingDeadlineFn[ graph_, scale_, entries_List ] :=
  Module[ { state },
    state[ walk_ ] := state[ walk ] =
      If[ Length[ walk ] < 2, { entries[[ All, 3 ]], Infinity },
        With[ { prev = state @ Most @ walk },
          { fired = MapThread[
              { rem, entry } |-> Boole[ rem > 0 &&
                stepEventQ[ graph, scale, First @ entry, walk ] ],
              { First @ prev, entries } ] },
          { rem = First @ prev - fired },
          { rem,
            Min @ Prepend[
              MapThread[
                If[ #1 === 1 && #2 === 0, Length[ walk ] - 1 + #3[[ 2 ]], Infinity ] &,
                { fired, rem, entries } ],
              Last @ prev ] } ] ];
    walk |-> Last @ state @ walk ]


(* crossings of a walk: arrivals at an already-visited vertex -- an
   invariant (and a stopping condition), never a class option *)
walkCrossings[ w_List ] := Length[ w ] - Length[ DeleteDuplicates[ w ] ]


(* backward walk counts on directed edges (the non-backtracking transfer
   matrix), consumed by nbWalkDraw: one draw is exactly uniform over the
   non-backtracking walks from a with length in the window -- ending at b,
   or ending anywhere (b = All, the pointed sampler).  $Failed when the
   class is empty. *)
nbWalkSampler[ graph_, a_, window : { _, _ } ] := nbWalkSampler[ graph, a, All, window ]

nbWalkSampler[ graph_, a_, b_, { lmin_, lmax_ } ] := Module[
  { de, idx, sIdx, transfer, seedVec, counts, starts, totals },
  If[ lmax < 1 || ! MemberQ[ VertexList @ graph, a ] ||
      ( b =!= All && ! MemberQ[ VertexList @ graph, b ] ),
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
  seedVec = SparseArray[ Table[ If[ b === All || de[[ i, 2 ]] === b, 1, 0 ], { i, Length @ de } ] ];
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


(* walkLengthAdmissibleQ[kspec]: predicate on a vertex sequence checking it
   has length compatible with kspec.  Path length = number of edges = Length - 1.
   Bare k means at most k (the FindPath convention); {k} means exactly k. *)

walkLengthAdmissibleQ[ Infinity ]                 := True &
walkLengthAdmissibleQ[ k_Integer ]                := Length[ # ] - 1 <= k &
walkLengthAdmissibleQ[ { k_Integer } ]            := Length[ # ] - 1 == k &
walkLengthAdmissibleQ[ { kmin_Integer, kmax_Integer } ] :=
  kmin <= Length[ # ] - 1 <= kmax &


(* ===================== FindInfraGeodesic ===================== *)

(* A geodesic at infra-scale r: a walk in which every window -- the last r
   vertices together with the next one -- satisfies the local rule.  The
   class degenerates at both ends of the scale ladder: under "Minimizing",
   r = 1 asks only for adjacency (any walk), r = Infinity for a segment.
   Returns InfraWalk; InfraGeodesicQ[graph, walk, r] carries the class.

   The pointed form FindInfraGeodesic[g, p1, scale, kspec, count] grows the
   class from p1 until a stopping condition fires, the budget kspec is
   spent, or no admissible step remains -- FindInfraWalk's reshape at a
   finite horizon, with the same "StoppingCondition" grammar.  The two-point
   form FindInfraGeodesic[g, p1, p2, scale, kspec, count] keeps the walks
   ending at p2; the pointed reading wins a positional tie, so a target
   matching the scale grammar (a bare integer) is written InfraPoint[p2].

   Rules (Properties): "Minimizing" (window is a shortest path), "Simple" (no
   revisits), "Immersed" (no cusps), "Generic" (general position -- read on
   the whole walk prefix, see windowRuleQ) and a bare predicate on the window
   are constraints; "Straightest" (step away from the window) and
   {"Minimal", f} / {"Maximal", f} on f[window] are selectors refining the
   surviving ties.

   Candidates are local: every neighbour of the walk's last vertex, filtered by
   the rules alone.  p2 never enters a window, so a selector may steer the walk
   away from p2 and no realisation survives -- that is the honest answer for an
   observer whose horizon is r.  The target-aware geodesic DAG is
   FindInfraSegment's optimisation of the constraint-only case r = Infinity. *)

FindInfraGeodesic::badproperty = "Property `1` is not supported by FindInfraGeodesic; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window.";
FindInfraGeodesic::badmethod   = "Method `1` is not supported by FindInfraGeodesic.";
FindInfraGeodesic::unbounded   = "The geodesic class at scale `1` is infinite without a length bound: give a finite kspec, add \"Simple\" or \"Generic\", or ask \"Minimizing\" at scale Infinity.";
FindInfraGeodesic::shortfall   = "\"RandomGreedy\" drew `1` distinct geodesics of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";
FindInfraGeodesic::badevent    = "Stopping condition `1` is not supported; entries are event or event -> \"Stop\" | {\"Stop\", \"Delay\" -> k, \"Count\" -> c}, with event a singularity name (\"Crossing\", \"Tangency\", \"TriplePoint\", \"Cusp\"), a constraint rule, or a predicate on the walk so far.";
FindInfraGeodesic::deadevent   = "the event `1` can never fire under the Properties constraints; the walk runs to its budget.";

Options[ FindInfraGeodesic ] = {
  Properties          -> { "Minimizing" },
  "StoppingCondition" -> { },
  Method              -> Automatic
};

FindInfraGeodesic[ graph_Graph, p1_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    q1 |-> Catch @ With[ {
        rules  = OptionValue[ FindInfraGeodesic, { opts }, Properties ],
        events = parseStoppingEvents[
          OptionValue[ FindInfraGeodesic, { opts }, "StoppingCondition" ], FindInfraGeodesic ],
        methodSpec = resolveMethod[ OptionValue[ FindInfraGeodesic, { opts }, Method ], count ] },
      { methodHead  = methodName @ methodSpec,
        pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
        kmax        = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
        lengthQ     = walkLengthAdmissibleQ[ kspec ],
        candidateFn = windowCandidateFn[ graph, scale, rules, FindInfraGeodesic ],
        deadlineFn  = stoppingDeadlineFn[ graph, scale, events ] },
      warnDeadEvents[ events, rules, scale, FindInfraGeodesic ];
      If[ kmax === Infinity && ! MemberQ[ rules, "Simple" | "Generic" ] &&
          ! ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ),
        Message[ FindInfraGeodesic::unbounded, scale ]; Throw[ $Failed ] ];
      Switch[ methodHead,
        "Exhaustive",
          pointedFrontierSweep[ graph, q1, candidateFn, lengthQ, deadlineFn, kmax,
            pruning, countLimit @ count ],
        "Greedy",
          pointedGreedySweep[ graph, q1, candidateFn, lengthQ, deadlineFn, kmax, count ],
        "RandomGreedy",
          randomDraws[
            { } |-> pointedGreedySweep[ graph, q1, candidateFn, lengthQ, deadlineFn, kmax,
              1, randomBranch ],
            count, FindInfraGeodesic ],
        _,
          Message[ FindInfraGeodesic::badmethod, methodSpec ]; $Failed
      ] ], p1 ]

FindInfraGeodesic[ graph_Graph, p1_, p2_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    { q1, q2 } |-> If[ q1 === q2, { },
      Catch @ With[ {
          rules  = OptionValue[ FindInfraGeodesic, { opts }, Properties ],
          events = parseStoppingEvents[
            OptionValue[ FindInfraGeodesic, { opts }, "StoppingCondition" ], FindInfraGeodesic ],
          methodSpec = resolveMethod[ OptionValue[ FindInfraGeodesic, { opts }, Method ], count ] },
        { methodHead  = methodName @ methodSpec,
          pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax        = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          lengthQ     = walkLengthAdmissibleQ[ kspec ],
          candidateFn = windowCandidateFn[ graph, scale, rules, FindInfraGeodesic ],
          (* the sweeps may stop a branch at its first arrival at p2 exactly
             when no accepted walk revisits its endpoint: under "Simple" or
             "Generic", or under "Minimizing" at scale Infinity, where the
             distance from the start strictly increases; a finite-scale rule
             lets a walk pass through p2 and return *)
          terminal    = MemberQ[ rules, "Simple" | "Generic" ] ||
            ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ) },
        { acceptQ = If[ MemberQ[ rules, "Generic" ],
            (* endpoint freeness is the one census condition the moving tip
               cannot prune; checked on the finished walk *)
            w |-> lengthQ[ w ] && Count[ w, First @ w ] === 1 && Count[ w, Last @ w ] === 1,
            lengthQ ],
          stepFn = If[ events === { }, candidateFn,
            With[ { deadlineFn = stoppingDeadlineFn[ graph, scale, events ] },
              { g, walk } |-> If[ Length[ walk ] - 1 >= deadlineFn @ walk, { },
                candidateFn[ g, walk ] ] ] ] },
        warnDeadEvents[ events, rules, scale, FindInfraGeodesic ];
        (* with revisits allowed a local rule alone leaves an infinite class
           (a walk can wind around a long cycle forever and stay minimizing at
           every finite scale); only simplicity, genericity (multiplicity
           <= 2), or a global minimizing rule bounds it *)
        If[ kmax === Infinity && ! terminal,
          Message[ FindInfraGeodesic::unbounded, scale ]; Throw[ $Failed ] ];
        Switch[ methodHead,
          "Exhaustive",
            Select[
              frontierSweep[ graph, q1, q2,
                { g, walk } |-> If[ Length[ walk ] - 1 >= kmax, { },
                  stepFn[ g, walk ] ],
                pruning,
                (* early stop on count is unsound when a completion can still
                   be rejected: by an exact / range length, or by the endpoint
                   check *)
                If[ MatchQ[ kspec, { _Integer } | { _Integer, _Integer } ] ||
                    MemberQ[ rules, "Generic" ],
                  Infinity, countLimit @ count ],
                terminal ],
              acceptQ ],
          "Greedy",
            greedyFrontierSweep[ graph, q1, q2, stepFn, acceptQ, kmax, count,
              Identity, terminal ],
          "RandomGreedy",
            randomDraws[
              { } |-> greedyFrontierSweep[ graph, q1, q2, stepFn, acceptQ, kmax, 1,
                randomBranch, terminal ],
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

ExtendInfraWalk::badproperty  = "Property `1` is not supported by ExtendInfraWalk; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window.";
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
                (* the per-step Cartesian of a two-sided step can violate a
                   whole-walk constraint each side admits alone; filter the
                   joined walk on its monotone conditions *)
                stepFilter  = Which[
                  MemberQ[ properties, "Simple" ], DuplicateFreeQ,
                  MemberQ[ properties, "Generic" ],
                    w |-> Max[ Counts @ w ] <= 2 &&
                      DuplicateFreeQ[ Sort /@ Partition[ w, 2, 1 ] ],
                  True, True & ] },
          Switch[ direction,
            "Forward",   extendOneSide[ graph, walk0, candidateFn, length, pruning ],
            "Backward",  Reverse /@ extendOneSide[ graph, Reverse @ walk0,
                           candidateFn, length, pruning ],
            "BothSides", extendBothSidesSymmetric[ graph, walk0, candidateFn,
                           length, pruning, stepFilter ],
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
   counts outer steps.  stepFilter re-checks each joined two-sided step
   against the whole-walk constraints ("Simple", "Generic"). *)

extendBothSidesSymmetric[ graph_Graph, seed_List, candidateFn_, length_, pruning_, stepFilter_ ] :=
  Module[ { live = { seed }, dead = { }, steps = 0,
            maxSteps = length /. Automatic -> Infinity },
    While[ live =!= { } && steps < maxSteps,
      With[ { pairs = ( w |-> { w, stepBothSides[ graph, w, candidateFn ] } ) /@ live },
        dead = Join[ dead, Cases[ pairs, { w_, { } } :> w ] ];
        live = Select[
          applyPruning[
            Flatten[ Cases[ pairs, { _, nexts : { __ } } :> nexts ], 1 ],
            pruning ],
          stepFilter ]
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
