Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraWalk wrapper ===================== *)


InfraWalk[ args : ( _InfraPoint | { _InfraPoint } ) .. ] :=
  InfraWalk[ Tuples @ Map[ Replace[ #, { x_ } :> x ][[ 1 ]]&, { args } ] ]

InfraWalk[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

(* multiplicity kept: the end-vertex multiset is the occupation measure of where the walks terminate, unlike InfraSegment's deduplicated geodesic ends *)
InfraWalk[ reps_List ][ "Start" ] := columnInfraPoint[ reps, 1 ]
InfraWalk[ reps_List ][ "End" ]   := columnInfraPoint[ reps, -1 ]


(* ===================== FindInfraWalk ===================== *)

(* growth from a seed under the Properties rules until a stopping condition fires, the length budget kspec is spent, or no admissible step remains.  Every rule reads the window -- the last <= "InfraScale" vertices with the candidate, the whole walk at the default scale Infinity.  The default class {"Simple"} is the simple paths; "Generic" (InfraGenericQ's read per step, endpoint freeness added on the finished curve), "Immersed" and the bare class {} are opt-in.
   A rule excluding self-intersections, triple points or self-tangencies bounds the class by itself, as does "Minimizing" at scale Infinity, so kspec Infinity is legal under the default; without a bounding rule it is refused, since a stopping condition may never fire. *)

FindInfraWalk::badproperty = "Property `1` is not a walk rule; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Exclude\" -> species, \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window; species are \"SelfIntersections\", \"SelfTangencies\", \"Cusps\", \"TriplePoints\".";
FindInfraWalk::badmethod   = "Method `1` is not supported.";
FindInfraWalk::unbounded   = "the walk class at scale `1` is infinite without a length bound: give a finite kspec, add \"Simple\", \"Generic\" or an \"Exclude\" of \"SelfIntersections\", \"TriplePoints\" or \"SelfTangencies\", or ask \"Minimizing\" at scale Infinity.";
FindInfraWalk::badevent    = "Stopping condition `1` is not supported; give n (stop at the n-th arrival at a visited vertex), a predicate on the walk so far, or {spec, \"Delay\" -> k}.";
FindInfraWalk::deadevent   = "the stopping condition awaits a self-intersection the Properties constraints exclude; the walk runs to its budget.";

Options[ FindInfraWalk ] = {
  "InfraScale"        -> Infinity,
  Properties          -> { "Simple" },
  "StoppingCondition" -> None,
  Method              -> Automatic
};

FindInfraWalk[ graph_Graph, p1_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    q1 |-> Catch @ With[ {
        scale  = OptionValue[ FindInfraWalk, { opts }, "InfraScale" ],
        rules  = OptionValue[ FindInfraWalk, { opts }, Properties ],
        events = parseStoppingCondition[
          OptionValue[ FindInfraWalk, { opts }, "StoppingCondition" ], FindInfraWalk ],
        methodSpec = resolveMethod[ OptionValue[ FindInfraWalk, { opts }, Method ], count ] },
      { methodHead  = methodName @ methodSpec,
        pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
        kmax        = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
        lengthQ     = walkLengthAdmissibleQ[ kspec ],
        candidateFn = windowCandidateFn[ graph, scale, rules, FindInfraWalk ],
        deadlineFn  = stoppingDeadlineFn @ events },
      warnDeadEvents[ events, rules, scale, FindInfraWalk ];
      If[ kmax === Infinity && ! boundedClassQ[ rules, scale ],
        Message[ FindInfraWalk::unbounded, scale ]; Throw[ $Failed ] ];
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
          scale  = OptionValue[ FindInfraWalk, { opts }, "InfraScale" ],
          rules  = OptionValue[ FindInfraWalk, { opts }, Properties ],
          events = parseStoppingCondition[
            OptionValue[ FindInfraWalk, { opts }, "StoppingCondition" ], FindInfraWalk ],
          methodOpt = OptionValue[ FindInfraWalk, { opts }, Method ] },
        { methodSpec = resolveMethod[ methodOpt, count ] },
        { methodHead  = methodName @ methodSpec,
          pruning     = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
          kmax        = Replace[ kspec, { { _, hi_ } :> hi, { k_ } :> k } ],
          lengthQ     = walkLengthAdmissibleQ[ kspec ],
          candidateFn = windowCandidateFn[ graph, scale, rules, FindInfraWalk ],
          deadlineFn  = stoppingDeadlineFn @ events,
          (* a branch may stop at its first arrival at p2 exactly when no accepted walk revisits its endpoint -- no self-intersections, "Generic", or "Minimizing" on the whole walk; a finite-scale rule lets a walk pass through p2 and return *)
          terminal    = MemberQ[ excludedSpecies @ rules, "SelfIntersections" ] ||
            MemberQ[ rules, "Generic" ] ||
            ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ) },
        { acceptQ = If[ MemberQ[ rules, "Generic" ],
            (* endpoint freeness is the one census condition the moving tip cannot prune; checked on the finished walk *)
            w |-> lengthQ[ w ] && Count[ w, First @ w ] === 1 && Count[ w, Last @ w ] === 1,
            lengthQ ],
          stepFn = If[ events === { }, candidateFn,
            { g, walk } |-> If[ Length[ walk ] - 1 >= deadlineFn @ walk, { },
              candidateFn[ g, walk ] ] ] },
        warnDeadEvents[ events, rules, scale, FindInfraWalk ];
        (* with revisits allowed a local rule alone leaves an infinite class -- a walk can wind a long cycle forever and stay minimizing at every finite scale -- so only an exclusion capping the length or whole-walk "Minimizing" bounds it *)
        If[ kmax === Infinity && ! boundedClassQ[ rules, scale ],
          Message[ FindInfraWalk::unbounded, scale ]; Throw[ $Failed ] ];
        If[ methodOpt === Automatic && kspec === Infinity && countLimit[ count ] === 1 &&
            events === { } &&
            AllTrue[ rules,
              MatchQ[ #, "Minimizing" | "Simple" | "Immersed" | "Generic" | ( "Exclude" -> _ ) ] & ],
          (* a geodesic is simple, hence immersed, generic, and minimizing in every window at every scale: the canonical count-less witness *)
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

(* the condition normalised to one {event, delay, count} entry: n stops at the n-th arrival at a visited vertex, a predicate at its first True, {spec, "Delay" -> k} that many edges later *)

parseStoppingCondition[ None, _ ] := { }

parseStoppingCondition[ n_Integer /; n >= 1, _ ] := { { "SelfIntersection", 0, n } }

parseStoppingCondition[ { spec_, "Delay" -> k_Integer /; k >= 0 }, fnSym_ ] :=
  MapAt[ k &, parseStoppingCondition[ spec, fnSym ], { 1, 2 } ]

parseStoppingCondition[ pred : Except[ None | _Integer | _List | _String | _Rule ], _ ] :=
  { { pred, 0, 1 } }

parseStoppingCondition[ spec_, fnSym_ ] :=
  ( Message[ MessageName[ fnSym, "badevent" ], spec ]; Throw[ $Failed ] )


(* the event can never fire once self-intersections are excluded: warn rather than run silently to the budget *)

warnDeadEvents[ entries_List, rules_List, scale_, fnSym_ ] :=
  If[ MatchQ[ entries, { { "SelfIntersection", _, _ } } ] &&
      ( MemberQ[ excludedSpecies @ rules, "SelfIntersections" ] ||
        ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ) ),
    Message[ MessageName[ fnSym, "deadevent" ] ] ]


(* the class is finite once no vertex may be revisited or visited a third time (length <= 2 |V|), or no arc repeated (each edge at most once per direction), or the whole walk minimizes *)

boundedClassQ[ rules_List, scale_ ] :=
  ( scale === Infinity && MemberQ[ rules, "Minimizing" ] ) ||
  IntersectingQ[ excludedSpecies @ rules, { "SelfIntersections", "TriplePoints", "SelfTangencies" } ]


(* the event is read at the tip: an arrival at an already visited vertex, however it arrives *)

stepEventQ[ "SelfIntersection", walk_ ] := Count[ walk, Last @ walk ] >= 2

stepEventQ[ pred_, walk_ ] := TrueQ[ pred @ walk ]


(* the per-branch event state is a function of the walk prefix: memoised per prefix, so a shared prefix is stepped once; the closure is rebuilt per anchor tuple, so the memo table dies with it *)

stoppingDeadlineFn[ { } ] := Infinity &

stoppingDeadlineFn[ entries_List ] :=
  Module[ { state },
    state[ walk_ ] := state[ walk ] =
      If[ Length[ walk ] < 2, { entries[[ All, 3 ]], Infinity },
        With[ { prev = state @ Most @ walk },
          { fired = MapThread[
              { rem, entry } |-> Boole[ rem > 0 && stepEventQ[ First @ entry, walk ] ],
              { First @ prev, entries } ] },
          { rem = First @ prev - fired },
          { rem,
            Min @ Prepend[
              MapThread[
                If[ #1 === 1 && #2 === 0, Length[ walk ] - 1 + #3[[ 2 ]], Infinity ] &,
                { fired, rem, entries } ],
              Last @ prev ] } ] ];
    walk |-> Last @ state @ walk ]


(* path length = number of edges = Length - 1; a bare k means at most k (the FindPath convention), {k} exactly k *)

walkLengthAdmissibleQ[ Infinity ]                 := True &
walkLengthAdmissibleQ[ k_Integer ]                := Length[ # ] - 1 <= k &
walkLengthAdmissibleQ[ { k_Integer } ]            := Length[ # ] - 1 == k &
walkLengthAdmissibleQ[ { kmin_Integer, kmax_Integer } ] :=
  kmin <= Length[ # ] - 1 <= kmax &


(* ===================== FindInfraGeodesic ===================== *)

(* a geodesic at infra-scale r: a walk in which every window -- the last r vertices together with the next one -- is a shortest path, any further rule holding on the window too.  The class degenerates at both ends of the ladder: r = 1 asks only for adjacency, r = Infinity for a segment.  The wrapper is FindInfraWalk at "InfraScale" -> r with "Minimizing" always among the rules -- the name promises the rule -- so the bare class at a finite scale is FindInfraWalk with Properties -> { }.
   Candidates are local, so p2 never enters a window: a selector may steer the walk away from p2 and leave no realisation, which is the honest answer for an observer whose horizon is r.  FindInfraSegment's geodesic DAG is the target-aware optimisation of the constraint-only case r = Infinity. *)

Options[ FindInfraGeodesic ] = {
  Properties          -> { },
  "StoppingCondition" -> None,
  Method              -> Automatic
};

FindInfraGeodesic[ graph_Graph, p1_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  FindInfraWalk[ graph, p1, kspec, count, "InfraScale" -> scale,
    Properties -> DeleteDuplicates @ Prepend[ OptionValue[ FindInfraGeodesic, { opts }, Properties ], "Minimizing" ],
    Sequence @@ FilterRules[ { opts }, Except[ Properties ] ] ]

FindInfraGeodesic[ graph_Graph, p1_, p2_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  FindInfraWalk[ graph, p1, p2, kspec, count, "InfraScale" -> scale,
    Properties -> DeleteDuplicates @ Prepend[ OptionValue[ FindInfraGeodesic, { opts }, Properties ], "Minimizing" ],
    Sequence @@ FilterRules[ { opts }, Except[ Properties ] ] ]


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

(* an invariant of the vertex sequence: the coincidences v_i === v_j, the maximal repeated arcs, and the mirrored blocks v_{i-t} === v_{i+t} around an apex.  A closed head is read on its cyclic core Most @ closeWalk @ rep, an interval lifted past m being read mod m *)

WalkSingularities[ w_InfraWalk ] := openCensus /@ First @ w

WalkSingularities[ w : _InfraLoop | _InfraString ] := cyclicCensus[ Most @ closeWalk @ # ] & /@ First @ w

WalkSingularities[ walk_List ] := openCensus @ walk


(* direct blocks v_i === v_{i+d} and inverse blocks v_i === v_{s-i} on maximal runs of two or more positions; an inverse run reaching the apex s/2 is the mirror of a cusp, not a repeated arc *)

openCensus[ walk_List ] := With[
  { m = Length @ walk },
  { traversals = Join[
      Catenate @ Table[
        { #, # + d } & /@
          Select[ maximalRuns @ Select[ Range[ m - d ], i |-> walk[[ i ]] === walk[[ i + d ]] ],
            run |-> Length[ run ] >= 2 ],
        { d, 2, m - 2 } ],
      Catenate @ Table[
        { #, Reverse[ s - # ] } & /@
          Select[ maximalRuns @ Select[ Range[ Max[ 1, s - m ], Floor[ ( s - 1 ) / 2 ] ],
              i |-> walk[[ i ]] === walk[[ s - i ]] ],
            run |-> Length[ run ] >= 2 && ! ( EvenQ[ s ] && Last[ run ] == s / 2 - 1 ) ],
        { s, 3, 2 m - 1 } ] ] },
  <|
    "SelfIntersections" -> Select[ Values @ PositionIndex @ walk, Length[ # ] >= 2 & ],
    "SelfTangencies" -> arcGroups[ walk, Catenate @ traversals ],
    "Cusps" -> ( i |-> With[
        { k = LengthWhile[ Range @ Min[ i - 1, m - i ], t |-> walk[[ i - t ]] === walk[[ i + t ]] ] },
        Range[ i - k, i + k ] ] ) /@
      Select[ Range[ 2, m - 1 ], i |-> walk[[ i - 1 ]] === walk[[ i + 1 ]] ]
  |> ]


(* a core of minimal period p < m is the m/p fold cover of its period loop: one repeated arc tiling the cycle; a cyclic inverse run touching an apex of the reflection i -> s - i is a cusp *)

cyclicCensus[ core_List ] := With[
  { m = Length @ core },
  { cyc = i |-> Mod[ i - 1, m ] + 1,
    period = SelectFirst[ Divisors @ m, d |-> core === RotateLeft[ core, d ] ] },
  { traversals = Join[
      If[ period < m, { Partition[ Range @ m, period ] }, { } ],
      Catenate @ Table[
        { First[ # ] + Range[ 0, Length[ # ] - 1 ], cyc[ First[ # ] + d ] + Range[ 0, Length[ # ] - 1 ] } & /@
          Select[ cyclicRuns[ Select[ Range @ m, i |-> core[[ i ]] === core[[ cyc[ i + d ] ]] ], m ],
            run |-> 2 <= Length[ run ] < m ],
        { d, 2, Floor[ m / 2 ] } ],
      Catenate @ Table[
        { First[ # ] + Range[ 0, Length[ # ] - 1 ], cyc[ s - Last[ # ] ] + Range[ 0, Length[ # ] - 1 ] } & /@
          Select[ cyclicRuns[
              Select[ Range @ m, i |-> cyc[ s - i ] =!= i && core[[ i ]] === core[[ cyc[ s - i ] ]] ], m ],
            run |-> Length[ run ] >= 2 &&
              NoneTrue[ run, i |-> cyc[ s - i ] === cyc[ i + 2 ] || cyc[ s - i ] === cyc[ i - 2 ] ] ],
        { s, 0, m - 1 } ] ] },
  <|
    "SelfIntersections" -> Select[ Values @ PositionIndex @ core, Length[ # ] >= 2 & ],
    "SelfTangencies" -> arcGroups[ core, Catenate @ traversals ],
    "Cusps" -> ( i |-> With[
        { k = LengthWhile[ Range @ Floor[ ( m - 1 ) / 2 ],
            t |-> core[[ cyc[ i - t ] ]] === core[[ cyc[ i + t ] ]] ] },
        cyc /@ Range[ i - k, i + k ] ] ) /@
      Select[ Range @ m, i |-> core[[ cyc[ i - 1 ] ]] === core[[ cyc[ i + 1 ] ]] ]
  |> ]


(* the traversals of one arc form a group of oriented intervals, the first ascending and each later one descending when it runs the arc backwards *)

arcGroups[ core_List, traversals_List ] := With[
  { m = Length @ core },
  { keyed = ( pos |-> With[ { arc = core[[ Mod[ pos - 1, m ] + 1 ]] },
        { key = First @ Sort @ { arc, Reverse @ arc } },
        key -> { pos, arc === key } ] ) /@ DeleteDuplicates @ traversals },
  Values @ GroupBy[ keyed, First -> Last,
    ps |-> With[ { sorted = SortBy[ DeleteDuplicates @ ps, First @ First @ # & ] },
      { flip = ! Last @ First @ sorted },
      ( { pos, direct } |-> If[ Xor[ direct, flip ],
          { First @ pos, Last @ pos }, { Last @ pos, First @ pos } ] ) @@@ sorted ] ] ]


maximalRuns[ set_List ] := Split[ Sort @ set, #2 == #1 + 1 & ]


cyclicRuns[ set_List, m_ ] := With[
  { runs = maximalRuns @ set },
  If[ Length[ runs ] >= 2 && First[ First @ runs ] == 1 && Last[ Last @ runs ] == m,
    Prepend[ runs[[ 2 ;; -2 ]], Join[ Last @ runs, First @ runs ] ],
    runs ] ]


(* ===================== InfraImmersedQ / InfraGenericQ ===================== *)

(* immersed walk: a walk with no cusp *)

InfraImmersedQ[ graph_Graph, w_InfraWalk ] := AllTrue[ First @ w, InfraImmersedQ[ graph, # ] & ]

InfraImmersedQ[ graph_Graph, w : _InfraLoop | _InfraString ] :=
  AllTrue[ First @ w, rep |-> InfraWalkQ[ graph, closeWalk @ rep ] &&
    cyclicCensus[ Most @ closeWalk @ rep ][ "Cusps" ] === { } ]

InfraImmersedQ[ graph_Graph, walk_List ] :=
  InfraWalkQ[ graph, walk ] && openCensus[ walk ][ "Cusps" ] === { }


(* generic walk: immersed and in general position -- no repeated arc, every self-intersection a double point, the endpoints of an open walk off the curve *)

InfraGenericQ[ graph_Graph, w_InfraWalk ] := AllTrue[ First @ w, InfraGenericQ[ graph, # ] & ]

InfraGenericQ[ graph_Graph, w : _InfraLoop | _InfraString ] :=
  AllTrue[ First @ w, rep |-> InfraWalkQ[ graph, closeWalk @ rep ] &&
    With[ { c = cyclicCensus[ Most @ closeWalk @ rep ] },
      c[ "Cusps" ] === { } && c[ "SelfTangencies" ] === { } &&
      AllTrue[ c[ "SelfIntersections" ], Length[ # ] == 2 & ] ] ]

InfraGenericQ[ graph_Graph, walk_List ] :=
  InfraWalkQ[ graph, walk ] &&
  With[ { c = openCensus @ walk },
    c[ "Cusps" ] === { } && c[ "SelfTangencies" ] === { } &&
    AllTrue[ c[ "SelfIntersections" ], Length[ # ] == 2 && FreeQ[ #, 1 | Length @ walk ] & ] ]


(* ===================== InfraWalkCrossingQ ===================== *)

(* a double visit of v is a crossing at scale r when each pass through B(v, r-1), continued along its radial arcs through the shell {r, r+1}, separates the other pass's exits on that shell.  Two 0-spheres link only in S^1: on a surface-like substrate this is the interleaving of the two germ pairs, and where the shell stays connected after a radial cut nothing is a crossing *)

InfraWalkCrossingQ[ graph_Graph, w_InfraWalk, at_, r_Integer ] :=
  AllTrue[ First @ w, InfraWalkCrossingQ[ graph, #, at, r ] & ]

InfraWalkCrossingQ[ graph_Graph, w : _InfraLoop | _InfraString, at_, r_Integer ] :=
  AllTrue[ First @ w, rep |-> walkCrossingQ[ graph, Most @ closeWalk @ rep, True, at, r ] ]

InfraWalkCrossingQ[ graph_Graph, walk_List, at_, r_Integer ] :=
  walkCrossingQ[ graph, walk, False, at, r ]


(* the ambient point names the double visit; a vertex visited once or more than twice is no crossing.  A vertex label that is itself a pair of integers is written InfraPoint[v], the position pair winning the tie *)

walkCrossingQ[ graph_, core_List, closedQ_, at : Except[ { _Integer, _Integer } ], r_ ] :=
  With[ { ps = Select[ Range @ Length @ core, core[[ # ]] === Replace[ at, InfraPoint[ v_ ] :> v ] & ] },
    Length[ ps ] == 2 && walkCrossingQ[ graph, core, closedQ, ps, r ] ]

walkCrossingQ[ graph_, core_List, closedQ_, { i_Integer, j_Integer }, r_ ] := With[
  { m = Length @ core, v = core[[ i ]] },
  { localG = NeighborhoodGraph[ graph, { v }, r + 1 ] },
  { d = AssociationThread[ VertexList @ localG, GraphDistance[ localG, v ] ] },
  { at = t |-> core[[ Mod[ t - 1, m ] + 1 ]],
    dist = t |-> If[ closedQ || 1 <= t <= m,
      Lookup[ d, Key @ core[[ Mod[ t - 1, m ] + 1 ]], Infinity ], Missing[ ] ] },
  (* the excursion through B(v, r-1) around a visit, and the radial arc out of an exit: the walk while it sits on the sphere, then its first step onto the outer ring -- Missing when it turns back or ends first *)
  { excursion = p |-> {
      p - LengthWhile[ Range[ p - 1, p - m, -1 ], t |-> TrueQ[ dist[ t ] <= r - 1 ] ],
      p + LengthWhile[ Range[ p + 1, p + m ], t |-> TrueQ[ dist[ t ] <= r - 1 ] ] },
    radial = { start, step } |-> With[
      { ps = NestWhileList[ # + step &, start, dist[ # ] === r &, 1, m ] },
      If[ dist[ Last @ ps ] === r + 1, at /@ ps, Missing[ ] ] ] },
  { ei = excursion @ i, ej = excursion @ j },
  { exitsI = { First[ ei ] - 1, Last[ ei ] + 1 }, exitsJ = { First[ ej ] - 1, Last[ ej ] + 1 } },
  { band = Subgraph[ localG, Select[ VertexList @ localG, r <= d[ # ] <= r + 1 & ] ],
    cutI = { radial[ First @ exitsI, -1 ], radial[ Last @ exitsI, 1 ] },
    cutJ = { radial[ First @ exitsJ, -1 ], radial[ Last @ exitsJ, 1 ] } },
  core[[ j ]] === v &&
  ! IntersectingQ[ Mod[ Range @@ ei - 1, m ] + 1, Mod[ Range @@ ej - 1, m ] + 1 ] &&
  AllTrue[ Join[ exitsI, exitsJ ], dist[ # ] === r & ] &&
  FreeQ[ { cutI, cutJ }, _Missing ] &&
  SeparatesQ[ band, DeleteDuplicates[ Join @@ cutJ ], at @ First @ exitsI, at @ Last @ exitsI ] &&
  SeparatesQ[ band, DeleteDuplicates[ Join @@ cutI ], at @ First @ exitsJ, at @ Last @ exitsJ ] ]


(* ===================== ExtendInfraWalk ===================== *)

(* continues a seed walk under the Properties rules, each read on the window of the last <= "InfraScale" vertices: Find seeds with points and owns the two-point sugar, Extend seeds with walks and owns "Direction".  kspec is the extension budget, in added edges per growing side, and is mandatory-finite whenever the class is infinite.
   "BothSides" adds at most one edge per side per step and re-checks the joined step against the monotone whole-walk constraints its sides cannot see alone.  Stopping conditions replay over the seed, so a deadline may already sit inside it and the seed come back unextended; a two-ended walk has no single tip for the event clock, so they require "Forward" or "Backward". *)

ExtendInfraWalk::badproperty  = "Property `1` is not a walk rule; the rules are \"Minimizing\", \"Simple\", \"Immersed\", \"Generic\", \"Exclude\" -> species, \"Straightest\", {\"Minimal\", f}, {\"Maximal\", f}, or a predicate on the window; species are \"SelfIntersections\", \"SelfTangencies\", \"Cusps\", \"TriplePoints\".";
ExtendInfraWalk::badmethod    = "Method `1` is not supported.";
ExtendInfraWalk::baddirection = "Direction `1` is not supported; give \"Forward\", \"Backward\" or \"BothSides\".";
ExtendInfraWalk::badevent     = "Stopping condition `1` is not supported; give n (stop at the n-th arrival at a visited vertex), a predicate on the walk so far, or {spec, \"Delay\" -> k}.";
ExtendInfraWalk::deadevent    = "the stopping condition awaits a self-intersection the Properties constraints exclude; the walk runs to its budget.";
ExtendInfraWalk::eventsided   = "stopping conditions read the walk at a single growing tip; extend with \"Direction\" -> \"Forward\" or \"Backward\".";
ExtendInfraWalk::unbounded    = "the extension class at scale `1` is infinite without a length bound: give a finite kspec, add \"Simple\", \"Generic\" or an \"Exclude\" of \"SelfIntersections\", \"TriplePoints\" or \"SelfTangencies\", or ask \"Minimizing\" at scale Infinity.";

Options[ ExtendInfraWalk ] = {
  "InfraScale"        -> Infinity,
  Properties          -> { "Simple" },
  "StoppingCondition" -> None,
  Method              -> Automatic,
  "Direction"         -> "BothSides"
};

ExtendInfraWalk[ graph_Graph, seed_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraWalk, count,
    walk0 |-> If[ walk0 === { } || ! AllTrue[ walk0, VertexQ[ graph, # ] & ], { },
      Catch @ With[ {
          scale  = OptionValue[ ExtendInfraWalk, { opts }, "InfraScale" ],
          rules  = OptionValue[ ExtendInfraWalk, { opts }, Properties ],
          events = parseStoppingCondition[
            OptionValue[ ExtendInfraWalk, { opts }, "StoppingCondition" ], ExtendInfraWalk ],
          direction  = OptionValue[ ExtendInfraWalk, { opts }, "Direction" ],
          methodSpec = resolveMethod[ OptionValue[ ExtendInfraWalk, { opts }, Method ], count ],
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
          candidateFn = windowCandidateFn[ graph, scale, rules, ExtendInfraWalk ],
          deadlineFn  = stoppingDeadlineFn @ events,
          (* a two-sided step can violate a whole-walk constraint each side admits alone; re-check the joined walk on the constraints monotone under extension -- a walk is a geodesic iff every sub-walk is, so a failed joined check never heals *)
          stepChecks = Join[
            Replace[ excludedSpecies @ rules, {
              "SelfIntersections" -> DuplicateFreeQ,
              "TriplePoints"      -> ( w |-> Max[ Counts @ w ] <= 2 ),
              "Cusps"             -> ( w |-> openCensus[ w ][ "Cusps" ] === { } ),
              "SelfTangencies"    -> ( w |-> openCensus[ w ][ "SelfTangencies" ] === { } ) }, { 1 } ],
            If[ scale === Infinity && MemberQ[ rules, "Minimizing" ],
              { w |-> GraphDistance[ graph, First @ w, Last @ w ] == Length[ w ] - 1 }, { } ] ] },
        { stepFilter = If[ stepChecks === { }, True &,
            w |-> AllTrue[ stepChecks, #[ w ] & ] ] },
        If[ events =!= { } && direction === "BothSides",
          Message[ ExtendInfraWalk::eventsided ]; Throw[ $Failed ] ];
        warnDeadEvents[ events, rules, scale, ExtendInfraWalk ];
        If[ kmax === Infinity && ! boundedClassQ[ rules, scale ],
          Message[ ExtendInfraWalk::unbounded, scale ]; Throw[ $Failed ] ];
        If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
          Message[ ExtendInfraWalk::badmethod, methodSpec ]; Throw[ $Failed ] ];
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
          _, Message[ ExtendInfraWalk::baddirection, direction ]; Throw[ $Failed ]
        ] ] ], seed ]


(* ===================== ExtendInfraGeodesic ===================== *)

(* continues a seed walk as a geodesic at infra-scale scale: ExtendInfraWalk at "InfraScale" -> scale with "Minimizing" always among the rules *)

Options[ ExtendInfraGeodesic ] = {
  Properties          -> { },
  "StoppingCondition" -> None,
  Method              -> Automatic,
  "Direction"         -> "BothSides"
};

ExtendInfraGeodesic[ graph_Graph, seed_,
    scale : ( _Integer | Infinity ),
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  ExtendInfraWalk[ graph, seed, kspec, count, "InfraScale" -> scale,
    Properties -> DeleteDuplicates @ Prepend[ OptionValue[ ExtendInfraGeodesic, { opts }, Properties ], "Minimizing" ],
    Sequence @@ FilterRules[ { opts }, Except[ Properties ] ] ]


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
