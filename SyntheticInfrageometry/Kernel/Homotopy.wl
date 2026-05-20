Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[walkSpaceBFS]
PackageScope[walkSpaceGreedyDFS]
PackageScope[hausdorffMove]
PackageScope[closeWalk]
PackageScope[loopRotations]
PackageScope[faceMoves]
PackageScope[applyMove]
PackageScope[resolveFaces]
PackageScope[fundamentalCycles]
PackageScope[walkModeFor]


(* ===================== InfraHomotopy wrapper ===================== *)

(* Each realisation is the chain {p_0, ..., p_k} of intermediate walks
   produced by k elementary moves.  The walks share the wrapper head of
   the input to the homotopy finder (InfraPath / InfraLoop / InfraString). *)

InfraHomotopy[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraHomotopy[ _List ] ] ] :=
  InfraHomotopy[ Flatten[ reps /. InfraHomotopy[ xs_List ] :> xs, 1 ] ]


(* ===================== Shared options ===================== *)

$infraHomotopyOptions = {
  Method                -> "Exhaustive",
  "FreeHomotopy"        -> False,
  "NullHomotopicCycles" -> { 1, 2, 3 },
  "MaxLength"           -> Automatic,
  "MaxMoves"            -> Infinity
};


(* ===================== Walk-mode dispatch ===================== *)

(* walkModeFor[head, freeHomotopy] -> { addSlides, canonicalize }.

   Path + fixed    : { False, False } -- endpoints fixed; no slides.
   Path + free     : { True,  False } -- endpoints can slide along edges; no canon.
   Loop + fixed    : { False, False } -- base point fixed; no slides; closed walks.
   Loop + free     : { False, True  } -- equivalence quotients by rotation; canon.
   String          : { False, True  } -- canonical form is rotation-quotiented. *)

walkModeFor[ InfraPath,   freeHom_ ] := { freeHom === True, False }
walkModeFor[ InfraLoop,   freeHom_ ] := { False, freeHom === True }
walkModeFor[ InfraString, _ ]        := { False, True }
walkModeFor[ InfraCircle, _ ]        := { False, True }


(* representativeHeadFor maps an input head to the output wrapper head of
   the homotopy operations.  InfraCircle coerces to InfraString because
   the geometric circle has no preferred base point. *)

representativeHeadFor[ InfraPath ]   := InfraPath
representativeHeadFor[ InfraLoop ]   := InfraLoop
representativeHeadFor[ InfraString ] := InfraString
representativeHeadFor[ InfraCircle ] := InfraString
representativeHeadFor[ _ ]           := $Failed


(* Coerce a single realisation to the canonical form for its target mode. *)

coerceRealisation[ InfraPath,   walk_List ] := walk
coerceRealisation[ InfraLoop,   walk_List ] := closeWalk @ walk
coerceRealisation[ InfraString, walk_List ] := canonicalString @ walk
coerceRealisation[ InfraCircle, walk_List ] := canonicalString @ closeWalk @ walk


(* ===================== FindInfraHomotopyRepresentative ===================== *)

(* The length-shortest walk in the homotopy class of obj. Polymorphic on
   Head[obj] (InfraPath / InfraLoop / InfraString / InfraCircle).  Output
   wrapper head matches the input (InfraCircle -> InfraString). *)

FindInfraHomotopyRepresentative::wrap = "First argument must be wrapped in InfraPath, InfraLoop, InfraString, or InfraCircle, not `1`.";

Options[ FindInfraHomotopyRepresentative ] = $infraHomotopyOptions;

FindInfraHomotopyRepresentative[ graph_Graph, obj_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { inHead = Head[ obj ], outHead = representativeHeadFor @ Head[ obj ] },
    If[ outHead === $Failed,
      Message[ FindInfraHomotopyRepresentative::wrap, inHead ]; $Failed,
      infraSpreadAndCartesian[ outHead, count,
        representativeCore[ graph, ##, inHead, opts ] &, obj ]
    ]
  ]


representativeCore[ graph_Graph, walk_List, inHead_, opts___ ] :=
  With[ { parent = First @ runWalkBFS[ graph, walk, inHead, ( False & ), opts ] },
    minimalReached @ parent
  ]


(* ===================== FindInfraHomotopyRepresentativeHomotopy ===================== *)

(* One reduction chain {obj, ..., m} of elementary moves ending at a
   representative m.  Each chain's intermediate walks share the input
   wrapper head; the chain itself is wrapped in InfraHomotopy. *)

FindInfraHomotopyRepresentativeHomotopy::wrap =
  "First argument must be wrapped in InfraPath, InfraLoop, InfraString, or InfraCircle, not `1`.";

Options[ FindInfraHomotopyRepresentativeHomotopy ] = $infraHomotopyOptions;

FindInfraHomotopyRepresentativeHomotopy[ graph_Graph, obj_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { inHead = Head[ obj ], outHead = representativeHeadFor @ Head[ obj ] },
    If[ outHead === $Failed,
      Message[ FindInfraHomotopyRepresentativeHomotopy::wrap, inHead ]; $Failed,
      infraSpreadAndCartesian[ InfraHomotopy, count,
        reductionCore[ graph, ##, inHead, opts ] &, obj ]
    ]
  ]


reductionCore[ graph_Graph, walk_List, inHead_, opts___ ] :=
  With[ { parent = First @ runWalkBFS[ graph, walk, inHead, ( False & ), opts ] },
    reconstructChain[ parent, # ] & /@ minimalReached[ parent ]
  ]


(* ===================== FindInfraHomotopy ===================== *)

(* Chain of elementary moves from one walk to another.  a and b must
   share a wrapper head (InfraCircle coerces to InfraString). *)

FindInfraHomotopy::wrap     = "First two object arguments must be wrapped in InfraPath, InfraLoop, InfraString, or InfraCircle, not `1` and `2`.";
FindInfraHomotopy::mismatch = "Endpoint wrapper heads must match: got `1` and `2`.";
FindInfraHomotopy::badmethod = "Method `1` is not supported by FindInfraHomotopy.";

Options[ FindInfraHomotopy ] = $infraHomotopyOptions;

FindInfraHomotopy[ graph_Graph, a_, b_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  Module[ { aHead = Head[ a ], bHead = Head[ b ], inHead },
    inHead = unifyHomotopyHeads[ aHead, bHead ];
    If[ inHead === $Failed,
      If[ representativeHeadFor[ aHead ] === $Failed || representativeHeadFor[ bHead ] === $Failed,
        Message[ FindInfraHomotopy::wrap, aHead, bHead ],
        Message[ FindInfraHomotopy::mismatch, aHead, bHead ] ];
      $Failed,
      infraSpreadAndCartesian[ InfraHomotopy, count,
        homotopyCore[ graph, ##, inHead, opts ] &, a, b ]
    ]
  ]


(* Unify endpoint heads: both must coerce to the same mode head.
   InfraCircle and InfraString coerce together to InfraString.  InfraPath
   and InfraLoop are distinct (paths have endpoints, loops are closed). *)

unifyHomotopyHeads[ InfraPath,   InfraPath ]   := InfraPath
unifyHomotopyHeads[ InfraLoop,   InfraLoop ]   := InfraLoop
unifyHomotopyHeads[ InfraString, InfraString ] := InfraString
unifyHomotopyHeads[ InfraString, InfraCircle ] := InfraString
unifyHomotopyHeads[ InfraCircle, InfraString ] := InfraString
unifyHomotopyHeads[ InfraCircle, InfraCircle ] := InfraString
unifyHomotopyHeads[ _, _ ]                     := $Failed


homotopyCore[ graph_Graph, walkA_List, walkB_List, inHead_, opts___ ] :=
  Module[ { freeHom, modeInfo, canonicalize, slides, startW, targetW, methodSpec, methodHead, pruning,
            rules, maxMoves, maxLen },
    freeHom      = OptionValue[ FindInfraHomotopy, { opts }, "FreeHomotopy" ];
    modeInfo     = walkModeFor[ inHead, freeHom ];
    slides       = modeInfo[[ 1 ]];
    canonicalize = modeInfo[[ 2 ]];
    startW       = If[ canonicalize, canonicalString @ walkA, walkA ];
    targetW      = If[ canonicalize, canonicalString @ walkB, walkB ];
    rules        = resolveFaces[ graph, OptionValue[ FindInfraHomotopy, { opts }, "NullHomotopicCycles" ] ];
    maxMoves     = OptionValue[ FindInfraHomotopy, { opts }, "MaxMoves" ];
    maxLen       = OptionValue[ FindInfraHomotopy, { opts }, "MaxLength" ] /.
                     Automatic :> autoMaxLength[ { startW, targetW }, rules ];
    methodSpec   = OptionValue[ FindInfraHomotopy, { opts }, Method ] /. Automatic -> "Exhaustive";
    methodHead   = methodName @ methodSpec;
    pruning      = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity;
    If[ startW === targetW, Return[ { { startW } } ] ];
    If[ inHead === InfraPath && First[ walkA ] =!= First[ walkB ] && ! freeHom, Return[ { } ] ];
    If[ inHead === InfraPath && Last[ walkA ]  =!= Last[ walkB ]  && ! freeHom, Return[ { } ] ];
    If[ inHead === InfraLoop && First[ walkA ] =!= First[ walkB ] && ! freeHom, Return[ { } ] ];
    Switch[ methodHead,
      "Exhaustive",
        With[ { result = walkSpaceBFS[ graph, startW, rules, maxLen, maxMoves,
                  ( #1 === targetW & ), slides, canonicalize ] },
          With[ { parent = result[[ 1 ]], found = result[[ 2 ]] },
            If[ found === $NotFound, { }, { reconstructChain[ parent, targetW ] } ]
          ]
        ],
      "Greedy",
        With[ { chain = walkSpaceGreedyDFS[ graph, startW, targetW,
                  ( hausdorffMove[ graph, #, targetW ] & ),
                  rules, maxLen, maxMoves, slides, canonicalize ] },
          If[ chain === $Failed || Last[ chain ] =!= targetW, { }, { chain } ]
        ],
      _,
        Message[ FindInfraHomotopy::badmethod, methodSpec ]; $Failed
    ]
  ]


(* ===================== HomotopicQ ===================== *)

(* Predicate: are two walks in the same homotopy class?  Wrapped inputs
   spread Cartesian-AllTrue.  Internally short-circuits via the
   exhaustive BFS with a target-equality stop predicate. *)

HomotopicQ::wrap     = FindInfraHomotopy::wrap;
HomotopicQ::mismatch = FindInfraHomotopy::mismatch;

Options[ HomotopicQ ] = $infraHomotopyOptions;

HomotopicQ[ graph_Graph, a_, b_, opts : OptionsPattern[] ] :=
  Module[ { aHead = Head[ a ], bHead = Head[ b ], inHead },
    inHead = unifyHomotopyHeads[ aHead, bHead ];
    If[ inHead === $Failed,
      If[ representativeHeadFor[ aHead ] === $Failed || representativeHeadFor[ bHead ] === $Failed,
        Message[ HomotopicQ::wrap, aHead, bHead ],
        Message[ HomotopicQ::mismatch, aHead, bHead ] ];
      $Failed,
      AllTrue[ Tuples[ { infraSpread @ a, infraSpread @ b } ],
        pair |-> homotopicQCore[ graph, pair[[ 1 ]], pair[[ 2 ]], inHead, opts ] ]
    ]
  ]


homotopicQCore[ graph_Graph, walkA_List, walkB_List, inHead_, opts___ ] :=
  Module[ { freeHom, modeInfo, canonicalize, slides, startW, targetW, rules, maxMoves, maxLen, result },
    freeHom      = OptionValue[ HomotopicQ, { opts }, "FreeHomotopy" ];
    modeInfo     = walkModeFor[ inHead, freeHom ];
    slides       = modeInfo[[ 1 ]];
    canonicalize = modeInfo[[ 2 ]];
    startW       = If[ canonicalize, canonicalString @ walkA, walkA ];
    targetW      = If[ canonicalize, canonicalString @ walkB, walkB ];
    If[ startW === targetW, Return[ True ] ];
    If[ inHead === InfraPath && First[ walkA ] =!= First[ walkB ] && ! freeHom, Return[ False ] ];
    If[ inHead === InfraPath && Last[ walkA ]  =!= Last[ walkB ]  && ! freeHom, Return[ False ] ];
    If[ inHead === InfraLoop && First[ walkA ] =!= First[ walkB ] && ! freeHom, Return[ False ] ];
    rules        = resolveFaces[ graph, OptionValue[ HomotopicQ, { opts }, "NullHomotopicCycles" ] ];
    maxMoves     = OptionValue[ HomotopicQ, { opts }, "MaxMoves" ];
    maxLen       = OptionValue[ HomotopicQ, { opts }, "MaxLength" ] /.
                     Automatic :> autoMaxLength[ { startW, targetW }, rules ];
    result       = walkSpaceBFS[ graph, startW, rules, maxLen, maxMoves,
                     ( #1 === targetW & ), slides, canonicalize ];
    result[[ 2 ]] =!= $NotFound
  ]


(* ===================== NullHomotopicQ ===================== *)

(* A closed walk is null-homotopic iff it is homotopic (with whatever
   equivalence its wrapper head encodes) to a constant walk.  Open input
   is auto-closed.  Accepts InfraLoop / InfraCircle / InfraString
   wrappers (Cartesian-AllTrue conjunction) and bare closed walks. *)

NullHomotopicQ::wrap = "Argument must be wrapped in InfraLoop, InfraString, or InfraCircle, or a bare closed walk.";

Options[ NullHomotopicQ ] = $infraHomotopyOptions;

NullHomotopicQ[ graph_Graph, cycle_List, opts : OptionsPattern[] ] :=
  With[ { closed = closeWalk @ cycle },
    HomotopicQ[ graph, InfraLoop[ { closed } ], InfraLoop[ { { First @ closed } } ], opts ]
  ]

NullHomotopicQ[ graph_Graph, InfraLoop[ reps_List ], opts : OptionsPattern[] ] :=
  AllTrue[ reps, NullHomotopicQ[ graph, #, opts ] & ]

NullHomotopicQ[ graph_Graph, InfraString[ reps_List ], opts : OptionsPattern[] ] :=
  AllTrue[ reps,
    HomotopicQ[ graph, InfraString[ { # } ], InfraString[ { { First @ # } } ], opts ] & ]

NullHomotopicQ[ graph_Graph, InfraCircle[ reps_List ], opts : OptionsPattern[] ] :=
  AllTrue[ reps,
    HomotopicQ[ graph, InfraString[ { canonicalString @ closeWalk @ # } ],
      InfraString[ { { First @ # } } ], opts ] & ]

NullHomotopicQ[ _Graph, _, OptionsPattern[] ] :=
  ( Message[ NullHomotopicQ::wrap ]; $Failed )


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


(* ===================== Walk-space search engines ===================== *)

(* runWalkBFS -- shared BFS driver for the unary-input finders
   (Representative, RepresentativeHomotopy).  Sets up modes, runs to the
   walk-space cap, and returns the walkSpaceBFS result tuple. *)

runWalkBFS[ graph_Graph, walk_List, inHead_, stopWhen_, opts___ ] :=
  Module[ { freeHom, modeInfo, canonicalize, slides, startW, rules, maxMoves, maxLen },
    freeHom      = OptionValue[ FindInfraHomotopyRepresentative, { opts }, "FreeHomotopy" ];
    modeInfo     = walkModeFor[ inHead, freeHom ];
    slides       = modeInfo[[ 1 ]];
    canonicalize = modeInfo[[ 2 ]];
    startW       = If[ canonicalize, canonicalString @ walk, walk ];
    rules        = resolveFaces[ graph,
                     OptionValue[ FindInfraHomotopyRepresentative, { opts }, "NullHomotopicCycles" ] ];
    maxMoves     = OptionValue[ FindInfraHomotopyRepresentative, { opts }, "MaxMoves" ];
    maxLen       = OptionValue[ FindInfraHomotopyRepresentative, { opts }, "MaxLength" ] /.
                     Automatic :> autoMaxLength[ { startW }, rules ];
    walkSpaceBFS[ graph, startW, rules, maxLen, maxMoves, stopWhen, slides, canonicalize ]
  ]


(* walkSpaceBFS -- bidirectional BFS in walk-space.  start is one walk
   (already canonicalised if canonicalize == True); rules supplies the
   face set; maxLen caps stored-walk length; maxMoves caps BFS depth.
   stopWhen[q, parent] short-circuits when True; otherwise the search
   exhausts the bounded walk-space.  addSlides toggles the endpoint-
   slide moves (free path homotopy).  canonicalize toggles per-state
   reduction to canonicalString (free loop / string homotopy). *)

walkSpaceBFS[ graph_Graph, start_List, rules_Association, maxLen_, maxMoves_, stopWhen_,
    addSlides : ( True | False ) : False, canonicalize : ( True | False ) : False ] :=
  Module[ {
    vN = AssociationMap[ AdjacencyList[ graph, # ] &, VertexList[ graph ] ],
    cycleMoves,
    canon       = If[ canonicalize, canonicalString, Identity ],
    neighboursOf,
    parent,
    frontier,
    nextFrontier,
    found = $NotFound,
    layer = 0
  },
    cycleMoves = If[ rules[ "Cycles" ] === { }, { },
      Join @@ ( faceMoves /@ rules[ "Cycles" ] ) ];
    neighboursOf = If[ canonicalize,
      p |-> Catenate[ elementaryMoves[ #, vN, rules, cycleMoves, addSlides ] & /@ loopRotations[ closeWalk @ p ] ],
      p |-> elementaryMoves[ p, vN, rules, cycleMoves, addSlides ] ];
    parent   = <| start -> None |>;
    frontier = { start };
    While[ found === $NotFound && frontier =!= { } && layer < maxMoves,
      nextFrontier = { };
      Scan[
        p |->
          Scan[
            qRaw |->
              With[ { q = canon @ qRaw },
                If[ ! KeyExistsQ[ parent, q ] && Length[ q ] <= maxLen,
                  AssociateTo[ parent, q -> p ];
                  AppendTo[ nextFrontier, q ];
                  If[ found === $NotFound && stopWhen[ q, parent ], found = q ]
                ]
              ],
            neighboursOf[ p ]
          ],
        frontier
      ];
      frontier = nextFrontier;
      layer++
    ];
    { parent, found, layer }
  ]


(* walkSpaceGreedyDFS -- DFS picking at each step the neighbour with
   smallest scoreFn[q] strictly less than scoreFn[current].  No
   backtracking, no random tie-break; ties resolved by walk length then
   OrderedQ.  Returns the chain {start, q1, ..., qk} where qk is the
   first walk at which no improving move exists.  If a target is given
   (=!= None) and the search hits it, returns the chain ending at
   target; otherwise returns the local-minimum chain.  $Failed only
   for empty start (degenerate). *)

walkSpaceGreedyDFS[ graph_Graph, start_List, target_, scoreFn_, rules_Association,
    maxLen_, maxMoves_, addSlides : ( True | False ) : False, canonicalize : ( True | False ) : False ] :=
  Module[ {
    vN = AssociationMap[ AdjacencyList[ graph, # ] &, VertexList[ graph ] ],
    cycleMoves,
    canon  = If[ canonicalize, canonicalString, Identity ],
    neighboursOf,
    chain, current, currentScore, neighbours, bestNeighbour, bestScore, steps = 0,
    visited
  },
    cycleMoves = If[ rules[ "Cycles" ] === { }, { },
      Join @@ ( faceMoves /@ rules[ "Cycles" ] ) ];
    neighboursOf = If[ canonicalize,
      p |-> Catenate[ elementaryMoves[ #, vN, rules, cycleMoves, addSlides ] & /@ loopRotations[ closeWalk @ p ] ],
      p |-> elementaryMoves[ p, vN, rules, cycleMoves, addSlides ] ];
    current = canon @ start;
    chain   = { current };
    visited = <| current -> True |>;
    While[ steps < maxMoves && current =!= target,
      neighbours = Select[
        DeleteDuplicates[ canon /@ neighboursOf[ current ] ],
        ! KeyExistsQ[ visited, # ] && Length[ # ] <= maxLen & ];
      If[ neighbours === { }, Break[ ] ];
      currentScore  = scoreFn[ current ];
      bestNeighbour = First @ SortBy[ neighbours, { scoreFn, Length, Identity } ];
      bestScore     = scoreFn[ bestNeighbour ];
      If[ bestScore >= currentScore && bestNeighbour =!= target, Break[ ] ];
      AppendTo[ chain, bestNeighbour ];
      AssociateTo[ visited, bestNeighbour -> True ];
      current = bestNeighbour;
      steps++
    ];
    chain
  ]


(* hausdorffMove -- symmetric Hausdorff distance between the vertex sets
   of two walks under the graph metric.  Used as the greedy score
   function in FindInfraHomotopy. *)

hausdorffMove[ graph_Graph, walkA_List, walkB_List ] :=
  With[ { setA = DeleteDuplicates @ walkA, setB = DeleteDuplicates @ walkB },
    With[ { dMat = Outer[ GraphDistance[ graph, #1, #2 ] &, setA, setB ] },
      Max[ Min /@ dMat, Min /@ Transpose @ dMat ]
    ]
  ]


(* ===================== Move set ===================== *)

(* All elementary neighbours of path: 1-cycle (if Dup), 2-cycle (if Spur),
   k >= 3 cycle moves from rules["Cycles"], and (if addSlides) endpoint-
   slide moves for free path homotopy. *)

elementaryMoves[ path_List, vN_Association, rules_Association, cycleMoves_List,
    addSlides : ( True | False ) : False ] :=
  DeleteDuplicates @ Join[
    If[ rules[ "Dup" ],  consecutiveDupMoves[ path ], { } ],
    If[ rules[ "Spur" ], spurMovesAt[ path, vN ],     { } ],
    Catenate[ applyMove[ path, # ] & /@ cycleMoves ],
    If[ addSlides, endpointSlideMoves[ path, vN ], { } ]
  ]


(* Endpoint slide for free path homotopy: extend or retract at either end
   by one vertex.  Never produces an empty walk (retracts only when
   Length >= 2). *)

endpointSlideMoves[ path_List, vN_Association ] /; Length[ path ] === 0 := { }

endpointSlideMoves[ path_List, vN_Association ] :=
  With[ { firstV = First @ path, lastV = Last @ path },
    Join[
      ( Append[ path, # ]  & ) /@ vN[ lastV ],
      ( Prepend[ path, # ] & ) /@ vN[ firstV ],
      If[ Length[ path ] >= 2, { Most @ path, Rest @ path }, { } ]
    ]
  ]


(* Length-1 cycle moves: a-a-a ... <-> a ... *)

consecutiveDupMoves[ path_List ] :=
  With[ { n = Length[ path ] },
    Join[
      Table[ Insert[ path, path[[ i ]], i + 1 ], { i, n } ],
      Cases[ Range[ n - 1 ],
        i_ /; path[[ i ]] === path[[ i + 1 ]] :> Drop[ path, { i + 1 } ] ]
    ]
  ]


(* Length-2 cycle moves: ... a b a ... <-> ... a ... *)

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


(* ===================== Cycle / face resolution ===================== *)

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


cyclesOfLength[ graph_Graph, k_Integer ] := First /@ # & /@ FindCycle[ graph, { k }, All ]


fundamentalCycles[ graph_Graph ] := First /@ # & /@ FindFundamentalCycles[ graph ]


autoMaxLength[ walks_List, rules_Association ] :=
  Max[ Length /@ walks ] + 2 * maxCycleLengthOf[ rules ]

maxCycleLengthOf[ rules_Association ] :=
  With[ { cs = rules[ "Cycles" ] },
    Max[ 3, If[ cs === { }, 0, Max[ Length /@ cs ] ] ]
  ]


(* ===================== Helpers ===================== *)

closeWalk[ cycle_List ] :=
  If[ First[ cycle ] === Last[ cycle ], cycle, Append[ cycle, First[ cycle ] ] ]


loopRotations[ c_List ] /; Length[ c ] <= 1 := { c }

loopRotations[ c_List ] :=
  With[ { core = Most @ c },
    DeleteDuplicates @ Table[
      With[ { shifted = RotateLeft[ core, k ] }, Append[ shifted, First[ shifted ] ] ],
      { k, 0, Length[ core ] - 1 } ]
  ]


minimalReached[ parent_Association ] :=
  With[ { walks = Keys[ parent ] },
    With[ { minLen = Min[ Length /@ walks ] },
      Select[ walks, Length[ # ] == minLen & ]
    ]
  ]


reconstructChain[ parent_Association, target_List ] :=
  Module[ { chain = { target }, current = target },
    While[ parent[ current ] =!= None,
      current = parent[ current ];
      PrependTo[ chain, current ] ];
    chain
  ]
