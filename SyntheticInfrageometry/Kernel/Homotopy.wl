Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findHomotopyCore]
PackageScope[resolveFaces]
PackageScope[fundamentalCycles]
PackageScope[faceMoves]
PackageScope[applyMove]
PackageScope[walkSpaceBFS]
PackageScope[loopRotations]


(* ===================== InfraHomotopy wrapper ===================== *)

(* Each realisation is the chain {p_0, ..., p_k} of intermediate walks produced
   by k elementary moves from p_0 to p_k. *)

InfraHomotopy[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraHomotopy[ _List ] ] ] :=
  InfraHomotopy[ Flatten[ reps /. InfraHomotopy[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraHomotopy ===================== *)

(* A homotopy from p1 to p2: a finite chain of elementary moves, each
   rewriting one arc of a null-homotopic cycle as the complementary arc.
   Method "MinimumMoves" (default) does shortest-path BFS in walk-space and
   returns one minimum-move chain.  Method "ViaMinimalForm" reduces both
   endpoints to a common minimal form and returns one chain per common
   minimum (potentially several per pair). *)

Options[ FindInfraHomotopy ] = {
  Method                -> "MinimumMoves",
  "NullHomotopicCycles" -> { 1, 2, 3 },
  "MaxLength"           -> Automatic,
  "MaxMoves"            -> Infinity
};

FindInfraHomotopy[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  infraSpreadAndCartesian[ InfraHomotopy, count,
    findHomotopyCore[ graph, ##, opts ] &, p1, p2 ]


findHomotopyCore[ graph_Graph, p1_List, p2_List, opts : OptionsPattern[ FindInfraHomotopy ] ] :=
  Module[ {
    method = OptionValue[ FindInfraHomotopy, { opts }, Method ],
    rules  = resolveFaces[ graph, OptionValue[ FindInfraHomotopy, { opts }, "NullHomotopicCycles" ] ],
    maxMoves = OptionValue[ FindInfraHomotopy, { opts }, "MaxMoves" ],
    maxLen
  },
    maxLen = Replace[ OptionValue[ FindInfraHomotopy, { opts }, "MaxLength" ],
      Automatic :> autoMaxLength[ { p1, p2 }, rules ] ];
    Switch[ method,
      "ViaMinimalForm", findHomotopyViaMinimalForm[ graph, p1, p2, rules, maxLen, maxMoves ],
      "MinimumMoves",   findHomotopyMinimumMoves[ graph, p1, p2, rules, maxLen, maxMoves ]
    ]
  ]


findHomotopyViaMinimalForm[ graph_Graph, p1_List, p2_List, rules_Association, maxLen_, maxMoves_ ] :=
  Module[ { parent1, parent2, m1, m2, common },
    If[ First[ p1 ] =!= First[ p2 ] || Last[ p1 ] =!= Last[ p2 ], Return[ { } ] ];
    If[ p1 === p2, Return[ { { p1 } } ] ];
    { parent1 } = walkSpaceBFS[ graph, p1, rules, maxLen, maxMoves, ( False & ) ][[ { 1 } ]];
    { parent2 } = walkSpaceBFS[ graph, p2, rules, maxLen, maxMoves, ( False & ) ][[ { 1 } ]];
    m1 = minimalReached[ parent1 ];
    m2 = minimalReached[ parent2 ];
    common = Intersection[ m1, m2 ];
    Map[
      m |-> Join[ reconstructChain[ parent1, m ], Reverse @ Most @ reconstructChain[ parent2, m ] ],
      common
    ]
  ]


findHomotopyMinimumMoves[ graph_Graph, p1_List, p2_List, rules_Association, maxLen_, maxMoves_ ] :=
  Module[ { parent, found },
    If[ First[ p1 ] =!= First[ p2 ] || Last[ p1 ] =!= Last[ p2 ], Return[ { } ] ];
    If[ p1 === p2, Return[ { { p1 } } ] ];
    { parent, found } = walkSpaceBFS[ graph, p1, rules, maxLen, maxMoves,
      ( #1 === p2 & ) ][[ { 1, 2 } ]];
    If[ found === $NotFound, { }, { reconstructChain[ parent, p2 ] } ]
  ]


(* ===================== FindInfraMinimalForms ===================== *)

(* All length-shortest walks in the homotopy class of path.  Bidirectional
   BFS bounded by "MaxLength" (Automatic = Length[p] + 2 * maxCycleLength). *)

Options[ FindInfraMinimalForms ] = {
  "NullHomotopicCycles" -> { 1, 2, 3 },
  "MaxLength"           -> Automatic,
  "MaxMoves"            -> Infinity
};

FindInfraMinimalForms[ graph_Graph, path_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  Module[ {
    rules    = resolveFaces[ graph, OptionValue[ FindInfraMinimalForms, { opts }, "NullHomotopicCycles" ] ],
    maxMoves = OptionValue[ FindInfraMinimalForms, { opts }, "MaxMoves" ],
    maxLen, parent
  },
    maxLen = Replace[ OptionValue[ FindInfraMinimalForms, { opts }, "MaxLength" ],
      Automatic :> autoMaxLength[ { path }, rules ] ];
    { parent } = walkSpaceBFS[ graph, path, rules, maxLen, maxMoves,
      ( False & ) ][[ { 1 } ]];
    takeUpTo[ minimalReached[ parent ], countLimit[ count ] ]
  ]


(* ===================== FindInfraReduction ===================== *)

(* Chain(s) reducing path to a minimal form.  Each chain is a list of walks
   {p, p', ..., m} ending at m in Min(p).  May go up before coming down. *)

Options[ FindInfraReduction ] = Options[ FindInfraMinimalForms ];

FindInfraReduction[ graph_Graph, path_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  Module[ {
    rules    = resolveFaces[ graph, OptionValue[ FindInfraReduction, { opts }, "NullHomotopicCycles" ] ],
    maxMoves = OptionValue[ FindInfraReduction, { opts }, "MaxMoves" ],
    maxLen, parent
  },
    maxLen = Replace[ OptionValue[ FindInfraReduction, { opts }, "MaxLength" ],
      Automatic :> autoMaxLength[ { path }, rules ] ];
    { parent } = walkSpaceBFS[ graph, path, rules, maxLen, maxMoves,
      ( False & ) ][[ { 1 } ]];
    takeUpTo[ reconstructChain[ parent, # ] & /@ minimalReached[ parent ], countLimit[ count ] ]
  ]


(* ===================== ReducePath ===================== *)

(* One minimal form of path.  Convenience wrapper around FindInfraMinimalForms. *)

Options[ ReducePath ] = Options[ FindInfraMinimalForms ];

ReducePath[ graph_Graph, path_List, opts : OptionsPattern[ ] ] :=
  First @ FindInfraMinimalForms[ graph, path, 1, opts ]


(* ===================== HomotopicQ ===================== *)

(* p1 ~ p2 iff their minimal-form sets intersect.  Implemented via short-circuit
   BFS from p1 with stopWhen = (q === p2) for performance -- equivalent yes/no
   answer without exhausting both minimal-form sets. *)

Options[ HomotopicQ ] = Options[ FindInfraHomotopy ];

HomotopicQ[ graph_Graph, p1_List, p2_List, opts : OptionsPattern[ ] ] :=
  Module[ {
    rules    = resolveFaces[ graph, OptionValue[ HomotopicQ, { opts }, "NullHomotopicCycles" ] ],
    maxMoves = OptionValue[ HomotopicQ, { opts }, "MaxMoves" ],
    maxLen, found
  },
    If[ First[ p1 ] =!= First[ p2 ] || Last[ p1 ] =!= Last[ p2 ], Return[ False ] ];
    If[ p1 === p2, Return[ True ] ];
    maxLen = Replace[ OptionValue[ HomotopicQ, { opts }, "MaxLength" ],
      Automatic :> autoMaxLength[ { p1, p2 }, rules ] ];
    { found } = walkSpaceBFS[ graph, p1, rules, maxLen, maxMoves,
      ( #1 === p2 & ) ][[ { 2 } ]];
    found =!= $NotFound
  ]

HomotopicQ[ graph_Graph, p1_, p2_, opts : OptionsPattern[ ] ] :=
  AllTrue[ Tuples[ infraSpread /@ { p1, p2 } ],
    pair |-> HomotopicQ[ graph, pair[[ 1 ]], pair[[ 2 ]], opts ] ]


(* ===================== NullHomotopicQ / FindInfraNullHomotopy ===================== *)

(* A closed walk c is null-homotopic iff it admits a chain to the constant walk
   (First[c]).  Open input is auto-closed by appending First[c]. *)

Options[ NullHomotopicQ ]   = Options[ FindInfraHomotopy ];
Options[ FindInfraNullHomotopy ] = Options[ FindInfraHomotopy ];

NullHomotopicQ[ graph_Graph, cycle_List, opts : OptionsPattern[ ] ] :=
  With[ { closed = closeWalk[ cycle ] },
    HomotopicQ[ graph, closed, { First[ closed ] }, opts ]
  ]

NullHomotopicQ[ graph_Graph, InfraCircle[ reps_List ], opts : OptionsPattern[ ] ] :=
  AllTrue[ reps, NullHomotopicQ[ graph, #, opts ] & ]


FindInfraNullHomotopy[ graph_Graph, cycle_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  With[ { closed = closeWalk[ cycle ] },
    FindInfraHomotopy[ graph, closed, { First[ closed ] }, count, opts ]
  ]

FindInfraNullHomotopy[ graph_Graph, InfraCircle[ reps_List ],
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  With[ { result = FindInfraNullHomotopy[ graph, #, count, opts ] & /@ reps },
    If[ MemberQ[ result, $Failed ], $Failed,
      With[ { capped = infraCap[
            DeleteDuplicates @ Flatten[ result /. InfraHomotopy[ rs_List ] :> rs, 1 ],
            count ] },
        If[ capped === $Failed, $Failed, InfraHomotopy[ { # } ] & /@ capped ]
      ]
    ]
  ]


(* ===================== HomotopicLoopsQ ===================== *)

(* Free loop homotopy: equivalence generated by path-homotopy moves and cyclic
   rotation of the loop.  Two loops with disjoint vertex sets return False
   (the strict through-loops definition can't bridge them). *)

Options[ HomotopicLoopsQ ] = Options[ FindInfraHomotopy ];

HomotopicLoopsQ[ graph_Graph, loop1_List, loop2_List, opts : OptionsPattern[ ] ] :=
  AnyTrue[
    Select[
      Tuples[ { loopRotations[ closeWalk @ loop1 ], loopRotations[ closeWalk @ loop2 ] } ],
      First[ #[[ 1 ]] ] === First[ #[[ 2 ]] ] & ],
    pair |-> HomotopicQ[ graph, pair[[ 1 ]], pair[[ 2 ]], opts ]
  ]

HomotopicLoopsQ[ graph_Graph, InfraCircle[ reps_List ], loop2_, opts : OptionsPattern[ ] ] :=
  AllTrue[ reps, HomotopicLoopsQ[ graph, #, loop2, opts ] & ]

HomotopicLoopsQ[ graph_Graph, loop1_, InfraCircle[ reps_List ], opts : OptionsPattern[ ] ] :=
  AllTrue[ reps, HomotopicLoopsQ[ graph, loop1, #, opts ] & ]


(* ===================== Move classification ===================== *)

(* Elementary move replaces one arc by another of the same cycle, so it
   changes walk length by |newArc| - |oldArc|: Contract / Extend / Lateral. *)

HomotopyMoveType[ walk1_List, walk2_List ] :=
  Which[
    Length[ walk2 ] < Length[ walk1 ], "Contract",
    Length[ walk2 ] > Length[ walk1 ], "Extend",
    True,                              "Lateral"
  ]

HomotopyMoveTypes[ chain_List ] /; AllTrue[ chain, MatchQ[ _List ] ] :=
  MapThread[ HomotopyMoveType, { Most @ chain, Rest @ chain } ]

HomotopyMoveTypes[ InfraHomotopy[ { chain_List } ] ] := HomotopyMoveTypes[ chain ]

HomotopyMoveTypes[ InfraHomotopy[ reps_List ] ] := HomotopyMoveTypes /@ reps


(* ===================== Helpers ===================== *)

closeWalk[ cycle_List ] :=
  If[ First[ cycle ] === Last[ cycle ], cycle, Append[ cycle, First[ cycle ] ] ]


(* All cyclic rotations of a closed walk, returned as closed walks. *)

loopRotations[ c_List ] /; Length[ c ] <= 1 := { c }

loopRotations[ c_List ] :=
  With[ { core = Most @ c },
    DeleteDuplicates @ Table[
      With[ { shifted = RotateLeft[ core, k ] }, Append[ shifted, First[ shifted ] ] ],
      { k, 0, Length[ core ] - 1 } ]
  ]


(* resolveFaces[g, spec] -> <|"Dup", "Spur", "Cycles"|>.  Length-1 and length-2
   set the corresponding flags (handled by position-local generation); length
   k >= 3 are enumerated via FindCycle[g, {k}, All].  The fundamental basis
   FindFundamentalCycles is generally not equivalent: partial bases give
   non-trivial pi_1 quotients, so we keep the "all cycles of length k" reading. *)

resolveFaces[ graph_Graph, n_Integer ] /; n >= 1 := resolveFaces[ graph, Range[ 1, n ] ]

resolveFaces[ graph_Graph, lengths_List ] /; AllTrue[ lengths, IntegerQ[ # ] && # >= 1 & ] :=
  <|
    "Dup"    -> MemberQ[ lengths, 1 ],
    "Spur"   -> MemberQ[ lengths, 2 ],
    "Cycles" -> Catenate[ cyclesOfLength[ graph, # ] & /@ Select[ lengths, # >= 3 & ] ]
  |>

resolveFaces[ graph_Graph, cycles : { __List } ] :=
  <|
    "Dup"    -> AnyTrue[ cycles, Length[ # ] == 1 & ],
    "Spur"   -> AnyTrue[ cycles, Length[ # ] == 2 & ],
    "Cycles" -> Select[ cycles, Length[ # ] >= 3 & ]
  |>

resolveFaces[ _Graph, { } ] := <| "Dup" -> False, "Spur" -> False, "Cycles" -> { } |>


(* Vertex sequences of all cycles of length k in graph (length k >= 3). *)

cyclesOfLength[ graph_Graph, k_Integer ] := First /@ # & /@ FindCycle[ graph, { k }, All ]


(* Vertex sequences of the fundamental cycle basis: cyclomatic-number cycles
   spanning the cycle space. Available for users who pass this explicitly via
   "NullHomotopicCycles" -> fundamentalCycles[g]. *)

fundamentalCycles[ graph_Graph ] := First /@ # & /@ FindFundamentalCycles[ graph ]


(* Conservative default walk-length cap: max input length plus twice the
   longest declared cycle, generous enough to admit "go up then down" chains. *)

autoMaxLength[ walks_List, rules_Association ] :=
  Max[ Length /@ walks ] + 2 * maxCycleLengthOf[ rules ]

maxCycleLengthOf[ rules_Association ] :=
  With[ { cs = rules[ "Cycles" ] },
    Max[ 3, If[ cs === { }, 0, Max[ Length /@ cs ] ] ]
  ]


(* faceMoves[face]: ordered (oldArc, newArc) pairs.  For closed walk
   (f_1, ..., f_k), each cut (s, L) yields oldArc = L edges from slot s+1,
   newArc = the complementary k-L edges (reversed). *)

faceMoves[ face_List ] /; Length[ face ] < 2 := { }

faceMoves[ face_List ] :=
  With[ { k = Length[ face ],
          doubled    = Join[ face, face ],
          revDoubled = Join[ Reverse @ face, Reverse @ face ] },
    DeleteDuplicates @ Select[
      Join[
        Flatten[ Table[
          { doubled[[ s + 1 ;; s + L + 1 ]],
            Reverse @ doubled[[ s + L + 1 ;; s + k + 1 ]] },
          { s, 0, k - 1 }, { L, 0, k } ], 1 ],
        Flatten[ Table[
          { revDoubled[[ s + 1 ;; s + L + 1 ]],
            Reverse @ revDoubled[[ s + L + 1 ;; s + k + 1 ]] },
          { s, 0, k - 1 }, { L, 0, k } ], 1 ]
      ],
      #[[ 1 ]] =!= #[[ 2 ]] &
    ]
  ]


(* Slide oldArc through path as a contiguous sub-walk; each match yields one
   rewritten path with oldArc replaced by newArc. *)

applyMove[ path_List, { oldArc_List, newArc_List } ] :=
  With[ { arcLen = Length[ oldArc ], pathLen = Length[ path ] },
    If[ arcLen > pathLen, { },
      Cases[
        Table[
          If[ path[[ i ;; i + arcLen - 1 ]] === oldArc,
            Join[ path[[ ;; i - 1 ]], newArc, path[[ i + arcLen ;; ]] ],
            Nothing ],
          { i, 1, pathLen - arcLen + 1 } ],
        _List ]
    ]
  ]


(* Length-1 cycle moves: a-a-a ... <-> a ...  Position-local; no graph
   neighbours needed (the walk format itself supplies the duplication). *)

consecutiveDupMoves[ path_List ] :=
  With[ { n = Length[ path ] },
    Join[
      Table[ Insert[ path, path[[ i ]], i + 1 ], { i, n } ],
      Cases[ Range[ n - 1 ],
        i_ /; path[[ i ]] === path[[ i + 1 ]] :> Drop[ path, { i + 1 } ] ]
    ]
  ]


(* Length-2 cycle moves: ... a b a ... <-> ... a ...  Position-local using
   precomputed AdjacencyList values for the insertion direction. *)

spurMovesAt[ path_List, vN_Association ] :=
  With[ { n = Length[ path ] },
    Join[
      Flatten[ Table[
        With[ { a = path[[ i ]] },
          ( Join[ path[[ ;; i ]], { #, a }, path[[ i + 1 ;; ]] ] & ) /@
            DeleteCases[ vN[ a ], a ] ],
        { i, n } ], 1 ],
      Cases[ Range[ n - 2 ],
        i_ /; path[[ i ]] === path[[ i + 2 ]] :> Drop[ path, { i + 1, i + 2 } ] ]
    ]
  ]


(* All elementary neighbours of path: 1-cycle (if Dup), 2-cycle (if Spur),
   and the k >= 3 cycle moves from rules["Cycles"].  Deduplicated. *)

elementaryMoves[ path_List, vN_Association, rules_Association, cycleMoves_List ] :=
  DeleteDuplicates @ Join[
    If[ rules[ "Dup" ],  consecutiveDupMoves[ path ], { } ],
    If[ rules[ "Spur" ], spurMovesAt[ path, vN ],     { } ],
    Catenate[ applyMove[ path, # ] & /@ cycleMoves ]
  ]


(* Bidirectional BFS in walk-space from start.  At each new walk q', call
   stopWhen[q', parent]; first True halts and returns {parent, q', layer}.
   Otherwise exhausts the bounded walk-space.  Walks longer than maxLen are
   skipped; BFS stops after maxMoves layers. *)

walkSpaceBFS[ graph_Graph, start_List, rules_Association, maxLen_, maxMoves_, stopWhen_ ] :=
  Module[ {
    vN = AssociationMap[ AdjacencyList[ graph, # ] &, VertexList[ graph ] ],
    cycleMoves,
    parent = <| start -> None |>,
    frontier = { start },
    nextFrontier,
    found = $NotFound,
    layer = 0
  },
    cycleMoves = If[ rules[ "Cycles" ] === { }, { },
      Join @@ ( faceMoves /@ rules[ "Cycles" ] ) ];
    While[ found === $NotFound && frontier =!= { } && layer < maxMoves,
      nextFrontier = { };
      Scan[
        p |->
          Scan[
            q |->
              If[ ! KeyExistsQ[ parent, q ] && Length[ q ] <= maxLen,
                AssociateTo[ parent, q -> p ];
                AppendTo[ nextFrontier, q ];
                If[ found === $NotFound && stopWhen[ q, parent ], found = q ]
              ],
            elementaryMoves[ p, vN, rules, cycleMoves ]
          ],
        frontier
      ];
      frontier = nextFrontier;
      layer++
    ];
    { parent, found, layer }
  ]


(* Length-shortest walks among the visited keys of a BFS parent association. *)

minimalReached[ parent_Association ] :=
  With[ { walks = Keys[ parent ] },
    With[ { minLen = Min[ Length /@ walks ] },
      Select[ walks, Length[ # ] == minLen & ]
    ]
  ]


(* Walk parent backwards from target to the start (parent[start] = None). *)

reconstructChain[ parent_Association, target_List ] :=
  Module[ { chain = { target }, current = target },
    While[ parent[ current ] =!= None,
      current = parent[ current ];
      PrependTo[ chain, current ] ];
    chain
  ]
