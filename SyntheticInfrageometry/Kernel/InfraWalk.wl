Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraWalk wrapper ===================== *)


InfraWalk[ args : ( _InfraPoint | { _InfraPoint } ) .. ] :=
  InfraWalk[ Tuples @ Map[ Replace[ #, { x_ } :> x ][[ 1 ]]&, { args } ] ]

InfraWalk[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* multiplicity kept: the end-vertex multiset is the occupation measure of where the walks terminate, unlike InfraSegment's deduplicated geodesic ends *)
InfraWalk[ reps_List ][ "Start" ] := columnInfraPoint[ reps, 1 ]
InfraWalk[ reps_List ][ "End" ]   := columnInfraPoint[ reps, -1 ]
(* ===================== FindInfraWalk ===================== *)

(* growth from a seed under the Properties rules until a stopping condition fires, the length budget kspec is spent, or no admissible step remains.  The default class {"Generic"} is InfraGenericQ's read per step: no cusps, no tangencies, no triple points, every multiple point an isolated transverse crossing -- endpoint freeness is the census condition added on the finished curve.
   "Generic", "Simple" and whole-walk "Minimizing" bound the class by themselves, so kspec Infinity is legal under the default; without a bounding rule it is refused, since a stopping condition may never fire. *)

FindInfraWalk::badproperty = "Property `1` is not supported by FindInfraWalk; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the walk.";
FindInfraWalk::badmethod   = "Method `1` is not supported by FindInfraWalk.";
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
          pointedFrontierSweep[ graph, { q1 }, candidateFn, lengthQ, deadlineFn, kmax,
            pruning, countLimit @ count ],
        "Greedy" | "RandomGreedy",
          pointedGreedySweep[ graph, { q1 }, candidateFn, lengthQ, deadlineFn, kmax, count,
            greedyBranch @ methodHead ],
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
          (* a class forbidding a second visit to its endpoint ("Simple", "Generic", whole-walk "Minimizing") ends at its first arrival, so the sweeps may stop branches there; these are also exactly the rules that bound the class without a length bound *)
          terminal    = MemberQ[ rules, "Simple" | "Generic" | "Minimizing" ] },
        { acceptQ = If[ MemberQ[ rules, "Generic" ],
            (* endpoint freeness is the one census condition the moving tip cannot prune; checked on the finished walk *)
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
          (* a geodesic is simple, hence immersed, generic and minimizing: the canonical count-less witness *)
          With[ { path = FindShortestPath[ graph, q1, q2 ] },
            If[ path === { }, { }, { path } ] ],
          Switch[ methodHead,
            "Exhaustive",
              Select[
                frontierSweep[ graph, q1, q2,
                  { g, walk } |-> If[ Length[ walk ] - 1 >= kmax, { }, stepFn[ g, walk ] ],
                  pruning,
                  (* an early stop on count is unsound when a completion can still be rejected, by an exact / range length or by the endpoint check *)
                  If[ MatchQ[ kspec, { _Integer } | { _Integer, _Integer } ] ||
                      MemberQ[ rules, "Generic" ],
                    Infinity, countLimit @ count ],
                  terminal ],
                acceptQ ],
            "Greedy" | "RandomGreedy",
              greedyFrontierSweep[ graph, q1, q2, stepFn, acceptQ, kmax, count,
                greedyBranch @ methodHead, terminal ],
            _,
              Message[ FindInfraWalk::badmethod, methodSpec ]; $Failed
          ] ] ] ], p1, p2 ]


(* ===================== Pointed growth engines ===================== *)

(* lazy depth-first growth from a seed walk: descend by candidateFn and emit when the budget is spent or no admissible step remains, acceptQ filtering the emissions; the descent is complete, so a finite count is exact.  branch = Identity backtracks in candidateFn order, RandomSample in shuffled order *)

(* the closures are held in Module locals, never inlined into descend's RHS: see greedyFrontierSweep (Tools.wl) *)

pointedGreedySweep[ graph_Graph, seed_List, candidateFn_, acceptQ_, deadlineFn_, kmax_,
    count_, branch_ : Identity ] :=
  If[ seed === { } || ! AllTrue[ seed, VertexQ[ graph, # ] & ], { },
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
      Catch[ descend[ seed ]; acc, pointedGreedySweep ]
    ] ]


(* BFS sibling: applyPruning caps the live frontier per layer, and emissions pass acceptQ before they count, so an early stop on the count is exact *)

pointedFrontierSweep[ graph_Graph, seed_List, candidateFn_, acceptQ_, deadlineFn_, kmax_,
    prune_, count_ ] :=
  If[ seed === { } || ! AllTrue[ seed, VertexQ[ graph, # ] & ], { },
    Module[ { frontier = { seed }, completed = { }, moves },
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

(* entries normalised to {event, delay, count} triples: a bare event is event -> "Stop" *)

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


(* a dead event -- awaiting a singularity the constraints exclude -- can never fire: warn rather than run silently to the budget *)

warnDeadEvents[ { }, _, _, _ ] := Null

warnDeadEvents[ entries_List, rules_List, scale_, fnSym_ ] :=
  With[ { dead = deadEventNames[ rules, scale ] },
    Scan[
      If[ StringQ @ First @ # && MemberQ[ dead, First @ # ],
        Message[ MessageName[ fnSym, "deadevent" ], First @ # ] ] &,
      entries ] ]


(* whole-walk "Minimizing" forbids any revisit, so at scale Infinity it excludes every singularity *)

deadEventNames[ rules_List, scale_ ] := Union @@ Replace[ rules, {
  "Simple" -> { "Simple", "Generic", "Immersed", "Crossing", "Tangency", "TriplePoint", "Cusp" },
  "Generic" -> { "Generic", "Immersed", "Tangency", "TriplePoint", "Cusp" },
  "Immersed" -> { "Immersed", "Cusp" },
  "Minimizing" :> If[ scale === Infinity,
    { "Minimizing", "Simple", "Generic", "Immersed", "Crossing", "Tangency", "TriplePoint", "Cusp" },
    { "Minimizing" } ],
  _ -> { } }, { 1 } ]


(* one incremental detector serves constraints and events, each read at the walk's tip: "Crossing" is an arrival at a visited vertex along a fresh edge, and a later pass along the same arc upgrades it, the upgrade firing "Tangency".  WalkSingularities on the finished walk stays the authority *)

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


(* the per-branch event state is a function of the walk prefix: memoised per prefix, so a shared prefix is stepped once; the closure is rebuilt per anchor tuple, so the memo table dies with it *)

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


(* crossings are arrivals at an already-visited vertex: an invariant, never a class option *)
walkCrossings[ w_List ] := Length[ w ] - Length[ DeleteDuplicates[ w ] ]


(* backward walk counts on directed edges, the non-backtracking transfer matrix: one draw is exactly uniform over the non-backtracking walks from a with length in the window *)
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


(* path length = number of edges = Length - 1; a bare k means at most k (the FindPath convention), {k} exactly k *)

walkLengthAdmissibleQ[ Infinity ]                 := True &
walkLengthAdmissibleQ[ k_Integer ]                := Length[ # ] - 1 <= k &
walkLengthAdmissibleQ[ { k_Integer } ]            := Length[ # ] - 1 == k &
walkLengthAdmissibleQ[ { kmin_Integer, kmax_Integer } ] :=
  kmin <= Length[ # ] - 1 <= kmax &


(* ===================== FindInfraGeodesic ===================== *)

(* a geodesic at infra-scale r: a walk in which every window -- the last r vertices together with the next one -- satisfies the local rule.  The class degenerates at both ends of the ladder: under "Minimizing", r = 1 asks only for adjacency, r = Infinity for a segment.
   Candidates are local, so p2 never enters a window: a selector may steer the walk away from p2 and leave no realisation, which is the honest answer for an observer whose horizon is r.  FindInfraSegment's geodesic DAG is the target-aware optimisation of the constraint-only case r = Infinity. *)

FindInfraGeodesic::badproperty = "Property `1` is not supported by FindInfraGeodesic; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window.";
FindInfraGeodesic::badmethod   = "Method `1` is not supported by FindInfraGeodesic.";
FindInfraGeodesic::unbounded   = "The geodesic class at scale `1` is infinite without a length bound: give a finite kspec, add \"Simple\" or \"Generic\", or ask \"Minimizing\" at scale Infinity.";
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
          pointedFrontierSweep[ graph, { q1 }, candidateFn, lengthQ, deadlineFn, kmax,
            pruning, countLimit @ count ],
        "Greedy" | "RandomGreedy",
          pointedGreedySweep[ graph, { q1 }, candidateFn, lengthQ, deadlineFn, kmax, count,
            greedyBranch @ methodHead ],
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
          (* a branch may stop at its first arrival at p2 exactly when no accepted walk revisits its endpoint; a finite-scale rule lets a walk pass through p2 and return *)
          terminal    = MemberQ[ rules, "Simple" | "Generic" ] ||
            ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ) },
        { acceptQ = If[ MemberQ[ rules, "Generic" ],
            (* endpoint freeness is the one census condition the moving tip cannot prune; checked on the finished walk *)
            w |-> lengthQ[ w ] && Count[ w, First @ w ] === 1 && Count[ w, Last @ w ] === 1,
            lengthQ ],
          stepFn = If[ events === { }, candidateFn,
            With[ { deadlineFn = stoppingDeadlineFn[ graph, scale, events ] },
              { g, walk } |-> If[ Length[ walk ] - 1 >= deadlineFn @ walk, { },
                candidateFn[ g, walk ] ] ] ] },
        warnDeadEvents[ events, rules, scale, FindInfraGeodesic ];
        (* with revisits allowed a local rule alone leaves an infinite class -- a walk can wind a long cycle forever and stay minimizing at every finite scale -- so only simplicity, genericity or a global minimizing rule bounds it *)
        If[ kmax === Infinity && ! terminal,
          Message[ FindInfraGeodesic::unbounded, scale ]; Throw[ $Failed ] ];
        Switch[ methodHead,
          "Exhaustive",
            Select[
              frontierSweep[ graph, q1, q2,
                { g, walk } |-> If[ Length[ walk ] - 1 >= kmax, { },
                  stepFn[ g, walk ] ],
                pruning,
                (* an early stop on count is unsound when a completion can still be rejected, by an exact / range length or by the endpoint check *)
                If[ MatchQ[ kspec, { _Integer } | { _Integer, _Integer } ] ||
                    MemberQ[ rules, "Generic" ],
                  Infinity, countLimit @ count ],
                terminal ],
              acceptQ ],
          "Greedy" | "RandomGreedy",
            greedyFrontierSweep[ graph, q1, q2, stepFn, acceptQ, kmax, count,
              greedyBranch @ methodHead, terminal ],
          _,
            Message[ FindInfraGeodesic::badmethod, methodSpec ]; $Failed
        ]
      ]
    ], p1, p2 ]


(* ===================== InfraGeodesicQ ===================== *)

(* every window of r consecutive vertices with the next one is a shortest path; the ladder is exact at both ends: r = 1 is InfraWalkQ, r = Infinity is InfraSegmentQ *)

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

(* an invariant of the vertex sequence alone, no substrate structure entering.  Around every coincidence v_i === v_j lies a maximal matching block, same-direction or reversed, and the species is the shape of that block.
   "Cusp": apexes i with v_{i-1} === v_{i+1}, the whole mirrored arc belonging to the cusp however deep the retrace.  "Tangency": maximal repeated blocks of length >= 2 with disjoint parameter ranges, the second range ascending when the arc is re-run in the same direction and descending when retraced.  "Crossing": isolated double visits with both passes interior -- the transverse double point.  "TriplePoint": the parameters of vertices of multiplicity >= 3.
   Open walks add "EndpointIncidence"; a closed core of minimal period p < m is the q = m/p fold cover of its period loop, whose census is returned as "Cover" -> q. *)

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


(* generic walk: immersed and in general position -- multiple points only isolated crossings, endpoints off the curve *)

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


(* ===================== ExtendInfraGeodesic ===================== *)

(* continues a seed walk under the geodesic rules at infra-scale scale: Find seeds with points and owns the two-point sugar, Extend seeds with walks and owns "Direction".  kspec is the extension budget, in added edges per growing side, and is mandatory-finite whenever the class is infinite.
   "BothSides" adds at most one edge per side per step and re-checks the joined step against the monotone whole-walk constraints its sides cannot see alone.  Stopping conditions replay over the seed, so a deadline may already sit inside it and the seed come back unextended; a two-ended walk has no single tip for the event clock, so they require "Forward" or "Backward". *)

ExtendInfraGeodesic::badproperty  = "Property `1` is not supported by ExtendInfraGeodesic; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window.";
ExtendInfraGeodesic::badmethod    = "Method `1` is not supported by ExtendInfraGeodesic.";
ExtendInfraGeodesic::baddirection = "Direction `1` is not supported by ExtendInfraGeodesic.";
ExtendInfraGeodesic::badevent     = "Stopping condition `1` is not supported; entries are event or event -> \"Stop\" | {\"Stop\", \"Delay\" -> k, \"Count\" -> c}, with event a singularity name (\"Crossing\", \"Tangency\", \"TriplePoint\", \"Cusp\"), a constraint rule, or a predicate on the walk so far.";
ExtendInfraGeodesic::deadevent    = "the event `1` can never fire under the Properties constraints; the walk runs to its budget.";
ExtendInfraGeodesic::eventsided   = "stopping conditions read the walk at a single growing tip; extend with \"Direction\" -> \"Forward\" or \"Backward\".";
ExtendInfraGeodesic::unbounded    = "The extension class at scale `1` is infinite without a length bound: give a finite kspec, add \"Simple\" or \"Generic\", or ask \"Minimizing\" at scale Infinity.";

Options[ ExtendInfraGeodesic ] = {
  Properties          -> { "Minimizing" },
  "StoppingCondition" -> { },
  Method              -> Automatic,
  "Direction"         -> "BothSides"
};

ExtendInfraGeodesic[ graph_Graph, seed_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    walk0 |-> If[ walk0 === { } || ! AllTrue[ walk0, VertexQ[ graph, # ] & ], { },
      Catch @ With[ {
          rules  = OptionValue[ ExtendInfraGeodesic, { opts }, Properties ],
          events = parseStoppingEvents[
            OptionValue[ ExtendInfraGeodesic, { opts }, "StoppingCondition" ], ExtendInfraGeodesic ],
          direction  = OptionValue[ ExtendInfraGeodesic, { opts }, "Direction" ],
          methodSpec = resolveMethod[ OptionValue[ ExtendInfraGeodesic, { opts }, Method ], count ],
          absSpec = Replace[ kspec, {
            { lo_, hi_ } :> { lo, hi } + Length[ walk0 ] - 1,
            { k_ }       :> { k + Length[ walk0 ] - 1 },
            k_Integer    :> k + Length[ walk0 ] - 1 } ] },
        { methodHead  = methodName @ methodSpec,
          pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax        = Replace[ absSpec, { { _, hi_ } :> hi, { k_ } :> k } ],
          lengthQ     = walkLengthAdmissibleQ[ absSpec ],
          (* "BothSides" reads kspec per growing side: the budget counts outer steps, each live side paying one edge per step *)
          stepsMax    = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          stepsQ      = Replace[ kspec, {
            Infinity     :> ( True & ),
            { k_ }       :> ( # == k & ),
            { lo_, hi_ } :> ( lo <= # <= hi & ),
            k_Integer    :> ( # <= k & ) } ],
          candidateFn = windowCandidateFn[ graph, scale, rules, ExtendInfraGeodesic ],
          deadlineFn  = stoppingDeadlineFn[ graph, scale, events ],
          (* a two-sided step can violate a whole-walk constraint each side admits alone; re-check the joined walk on the constraints monotone under extension -- a walk is a geodesic iff every sub-walk is, so a failed joined check never heals *)
          stepChecks = Join[
            If[ MemberQ[ rules, "Simple" ], { DuplicateFreeQ }, { } ],
            If[ MemberQ[ rules, "Generic" ],
              { w |-> Max[ Counts @ w ] <= 2 &&
                  DuplicateFreeQ[ Sort /@ Partition[ w, 2, 1 ] ] }, { } ],
            If[ scale === Infinity && MemberQ[ rules, "Minimizing" ],
              { w |-> GraphDistance[ graph, First @ w, Last @ w ] == Length[ w ] - 1 }, { } ] ] },
        { stepFilter = If[ stepChecks === { }, True &,
            w |-> AllTrue[ stepChecks, #[ w ] & ] ] },
        If[ events =!= { } && direction === "BothSides",
          Message[ ExtendInfraGeodesic::eventsided ]; Throw[ $Failed ] ];
        warnDeadEvents[ events, rules, scale, ExtendInfraGeodesic ];
        If[ kmax === Infinity && ! MemberQ[ rules, "Simple" | "Generic" ] &&
            ! ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ),
          Message[ ExtendInfraGeodesic::unbounded, scale ]; Throw[ $Failed ] ];
        If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
          Message[ ExtendInfraGeodesic::badmethod, methodSpec ]; Throw[ $Failed ] ];
        Switch[ direction,
          "Forward",
            Switch[ methodHead,
              "Exhaustive",
                pointedFrontierSweep[ graph, walk0, candidateFn, lengthQ, deadlineFn, kmax,
                  pruning, countLimit @ count ],
              "Greedy" | "RandomGreedy",
                pointedGreedySweep[ graph, walk0, candidateFn, lengthQ, deadlineFn, kmax,
                  count, greedyBranch @ methodHead ] ],
          "Backward",
            Reverse /@ Switch[ methodHead,
              "Exhaustive",
                pointedFrontierSweep[ graph, Reverse @ walk0, candidateFn, lengthQ,
                  deadlineFn, kmax, pruning, countLimit @ count ],
              "Greedy" | "RandomGreedy",
                pointedGreedySweep[ graph, Reverse @ walk0, candidateFn, lengthQ, deadlineFn,
                  kmax, count, greedyBranch @ methodHead ] ],
          "BothSides",
            Switch[ methodHead,
              "Exhaustive",
                extendBothFrontierSweep[ graph, walk0, candidateFn, stepsQ, stepsMax,
                  pruning, countLimit @ count, stepFilter ],
              "Greedy" | "RandomGreedy",
                extendBothGreedySweep[ graph, walk0, candidateFn, stepsQ, stepsMax, count,
                  greedyBranch @ methodHead, stepFilter ] ],
          _, Message[ ExtendInfraGeodesic::baddirection, direction ]; Throw[ $Failed ]
        ] ] ], seed ]


(* ===================== Two-sided growth engines ===================== *)

(* two-sided siblings of the pointed engines: each outer step grows the walk by at most one edge per side, a side freezing when its candidateFn returns { }, and kmax counts the outer steps.  The BFS honours "Pruning" and its early count stop is exact; the DFS backtracks, in candidateFn order (branch = Identity) or shuffled (RandomSample) *)

extendBothFrontierSweep[ graph_Graph, seed_List, candidateFn_, stepsQ_, kmax_,
    prune_, count_, stepFilter_ ] :=
  Module[ { frontier = { seed }, completed = { }, steps = 0, moves },
    While[ frontier =!= { } && Length[ completed ] < count,
      moves = ( walk |-> { walk,
          If[ steps >= kmax, { },
            Select[ stepBothSides[ graph, walk, candidateFn, Identity ], stepFilter ] ] } ) /@
        frontier;
      completed = Join[ completed,
        If[ stepsQ[ steps ], Cases[ moves, { w_, { } } :> w ], { } ] ];
      frontier = applyPruning[
        DeleteDuplicates @ Flatten[ Cases[ moves, { _, nexts : { __ } } :> nexts ], 1 ],
        prune ];
      steps++
    ];
    Take[ completed, UpTo[ count ] ]
  ]


(* the closures are held in Module locals, never inlined into descend's RHS: see greedyFrontierSweep (Tools.wl) *)

extendBothGreedySweep[ graph_Graph, seed_List, candidateFn_, stepsQ_, kmax_,
    count_, branch_, stepFilter_ ] :=
  Module[ { cap = countLimit @ count, acc = { }, descend, emit,
            cands = candidateFn, keepQ = stepsQ, filterQ = stepFilter, pick = branch },
    emit[ walk_, steps_ ] := If[ keepQ @ steps,
      AppendTo[ acc, walk ];
      If[ Length @ acc >= cap, Throw[ acc, extendBothGreedySweep ] ] ];
    descend[ walk_, steps_ ] :=
      If[ steps >= kmax,
        emit[ walk, steps ],
        With[ { nexts = Select[ stepBothSides[ graph, walk, cands, pick ], filterQ ] },
          If[ nexts === { }, emit[ walk, steps ],
            Scan[ descend[ #, steps + 1 ] &, nexts ] ] ] ];
    Catch[ descend[ seed, 0 ]; acc, extendBothGreedySweep ]
  ]


stepBothSides[ graph_Graph, walk_List, candidateFn_, branch_ ] :=
  With[ { backCands = branch @ candidateFn[ graph, Reverse @ walk ],
          fwdCands  = branch @ candidateFn[ graph, walk ] },
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


dispatchConstruction[ graph_Graph, InfraWalk[ vs__ ] ] :=
  With[ { walk = { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { walk },
      { } ]
  ]
