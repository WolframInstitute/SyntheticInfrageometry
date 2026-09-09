Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[walkSpaceBFS]
PackageScope[walkSpaceGreedyDFS]
PackageScope[hausdorffMove]
PackageScope[closeWalk]
PackageScope[canonicalString]
PackageScope[loopRotations]
PackageScope[faceMoves]
PackageScope[applyMove]
PackageScope[resolveFaces]
PackageScope[walkModeFor]


(* ===================== InfraHomotopy wrapper ===================== *)


InfraHomotopy[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraHomotopy[ _List ] ] ] :=
  InfraHomotopy[ Flatten[ reps /. InfraHomotopy[ xs_List ] :> xs, 1 ] ]

InfraHomotopy[ inner_InfraHomotopy ]                    := inner
InfraHomotopy[ reps_List ][ "Realizations" ]   := reps
InfraHomotopy[ reps_List ][ "First" ]          := First @ reps
InfraHomotopy[ reps_List ][ "Weights" ]                 := ConstantArray[ 1, Length @ reps ]
InfraHomotopy[ reps_List, ws_List ][ "Weights" ]        := ws
InfraHomotopy[ reps_List ][ "Mass" ]                    := Length @ reps
InfraHomotopy[ reps_List, ws_List ][ "Mass" ]           := Total @ ws


(* ===================== Shared options ===================== *)

$infraHomotopyOptions = {
  Method                -> "Exhaustive",
  "FreeHomotopy"        -> False,
  "NullHomotopicCycles" -> { 1, 2, 3 },
  "MaxLength"           -> Automatic,
  "MaxMoves"            -> Infinity
};


(* ===================== Walk-mode dispatch ===================== *)

(* the homotopy class is read off the shape and one option.  An open walk -- a vertex list or a path graph -- is a path with its endpoints fixed, slid by "FreeHomotopy"; a closed walk -- a cycle graph, a circle included -- is a loop with its base point fixed, quotiented by rotation into the free loop by "FreeHomotopy".  Both arguments of a two-walk question must be open or both closed *)

(* { addSlides, canonicalize } *)
walkModeFor[ closedQ_, freeHom_ ] := { ! closedQ && TrueQ @ freeHom, closedQ && TrueQ @ freeHom }

closedWalkArgQ[ w_Graph ]              := closedWalkQ @ w
closedWalkArgQ[ ws : { __Graph } ]     := closedWalkQ @ First @ ws
closedWalkArgQ[ _ ]                    := False

freeWalkArgQ[ _, freeHom_ ]             := TrueQ @ freeHom

(* the internal spelling of a realisation: a closed walk carries its base point repeated at the end, a free loop its lex-least rotation; the result takes the shape the input had *)
coerceRealisation[ closedQ_, canonicalize_, walk_List ] :=
  Which[ canonicalize, canonicalString @ walk, closedQ, closeWalk @ walk, True, walk ]

walkShape[ closedQ_ ] := If[ closedQ, closedWalkGraph, walkGraph ]


(* ===================== FindInfraHomotopyRepresentative ===================== *)


Options[ FindInfraHomotopyRepresentative ] = $infraHomotopyOptions;

FindInfraHomotopyRepresentative[ graph_Graph, obj_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[ { closedQ = closedWalkArgQ @ obj,
          freeHom = freeWalkArgQ[ obj, OptionValue[ FindInfraHomotopyRepresentative, { opts }, "FreeHomotopy" ] ] },
    spreadFind[ walkShape @ closedQ, count,
      walk |-> minimalReached @ First @ runWalkBFS[ graph, walk, closedQ, freeHom, ( False & ), opts ],
      obj ] ]


(* ===================== FindInfraHomotopyRepresentativeHomotopy ===================== *)


Options[ FindInfraHomotopyRepresentativeHomotopy ] = $infraHomotopyOptions;

FindInfraHomotopyRepresentativeHomotopy[ graph_Graph, obj_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[ { closedQ = closedWalkArgQ @ obj,
          freeHom = freeWalkArgQ[ obj, OptionValue[ FindInfraHomotopyRepresentativeHomotopy, { opts }, "FreeHomotopy" ] ] },
    Replace[ spreadFind[ Identity, count,
        walk |-> With[ { parent = First @ runWalkBFS[ graph, walk, closedQ, freeHom, ( False & ), opts ] },
          reconstructChain[ parent, # ] & /@ minimalReached[ parent ] ],
        obj ],
      chains_List :> InfraHomotopy[ chains ] ] ]


(* ===================== FindInfraHomotopy ===================== *)


FindInfraHomotopy::mismatch  = "The first walk is `1` and the second `2`; both must be open or both closed.";
FindInfraHomotopy::badmethod = "Method `1` is not supported by FindInfraHomotopy.";

Options[ FindInfraHomotopy ] = $infraHomotopyOptions;

FindInfraHomotopy[ graph_Graph, a_, b_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[ { closedQ = closedWalkArgQ @ a,
          freeHom = freeWalkArgQ[ a, OptionValue[ FindInfraHomotopy, { opts }, "FreeHomotopy" ] ] ||
                    freeWalkArgQ[ b, False ] },
    If[ closedQ =!= closedWalkArgQ @ b,
      Message[ FindInfraHomotopy::mismatch, openOrClosed @ closedQ, openOrClosed @ ! closedQ ]; $Failed,
      Replace[ spreadFind[ Identity, count,
          homotopyCore[ graph, ##, closedQ, freeHom, opts ] &, a, b ],
        chains_List :> InfraHomotopy[ chains ] ] ] ]

openOrClosed[ True ]  = "closed";
openOrClosed[ False ] = "open";


homotopyCore[ graph_Graph, walkA_List, walkB_List, closedQ_, freeHom_, opts___ ] :=
  Module[ { modeInfo, canonicalize, slides, startW, targetW, methodSpec, methodHead, pruning,
            rules, maxMoves, maxLen },
    modeInfo     = walkModeFor[ closedQ, freeHom ];
    slides       = modeInfo[[ 1 ]];
    canonicalize = modeInfo[[ 2 ]];
    startW       = coerceRealisation[ closedQ, canonicalize, walkA ];
    targetW      = coerceRealisation[ closedQ, canonicalize, walkB ];
    rules        = resolveFaces[ graph, OptionValue[ FindInfraHomotopy, { opts }, "NullHomotopicCycles" ] ];
    maxMoves     = OptionValue[ FindInfraHomotopy, { opts }, "MaxMoves" ];
    maxLen       = OptionValue[ FindInfraHomotopy, { opts }, "MaxLength" ] /.
                     Automatic :> autoMaxLength[ { startW, targetW }, rules ];
    methodSpec   = OptionValue[ FindInfraHomotopy, { opts }, Method ] /. Automatic -> "Exhaustive";
    methodHead   = methodName @ methodSpec;
    pruning      = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity;
    If[ startW === targetW, Return[ { { startW } } ] ];
    If[ ! freeHom && First[ startW ] =!= First[ targetW ], Return[ { } ] ];
    If[ ! closedQ && ! freeHom && Last[ startW ] =!= Last[ targetW ], Return[ { } ] ];
    Switch[ methodHead,
      "Exhaustive",
        With[ { result = walkSpaceBFS[ graph, startW, rules, maxLen, maxMoves,
                  ( #1 === targetW & ), slides, canonicalize ] },
          { parent = result[[ 1 ]], found = result[[ 2 ]] },
          If[ found === $NotFound, { }, { reconstructChain[ parent, targetW ] } ]
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


HomotopicQ::mismatch = FindInfraHomotopy::mismatch;

Options[ HomotopicQ ] = $infraHomotopyOptions;

HomotopicQ[ graph_Graph, a_, b_, opts : OptionsPattern[] ] :=
  With[ { closedQ = closedWalkArgQ @ a,
          freeHom = freeWalkArgQ[ a, OptionValue[ HomotopicQ, { opts }, "FreeHomotopy" ] ] ||
                    freeWalkArgQ[ b, False ] },
    If[ closedQ =!= closedWalkArgQ @ b,
      Message[ HomotopicQ::mismatch, openOrClosed @ closedQ, openOrClosed @ ! closedQ ]; $Failed,
      AllTrue[ Tuples[ { infraSpread @ a, infraSpread @ b } ],
        pair |-> homotopicQCore[ graph, pair[[ 1 ]], pair[[ 2 ]], closedQ, freeHom, opts ] ] ] ]


homotopicQCore[ graph_Graph, walkA_List, walkB_List, closedQ_, freeHom_, opts___ ] :=
  Module[ { modeInfo, canonicalize, slides, startW, targetW, rules, maxMoves, maxLen, result },
    modeInfo     = walkModeFor[ closedQ, freeHom ];
    slides       = modeInfo[[ 1 ]];
    canonicalize = modeInfo[[ 2 ]];
    startW       = coerceRealisation[ closedQ, canonicalize, walkA ];
    targetW      = coerceRealisation[ closedQ, canonicalize, walkB ];
    If[ startW === targetW, Return[ True ] ];
    If[ ! freeHom && First[ startW ] =!= First[ targetW ], Return[ False ] ];
    If[ ! closedQ && ! freeHom && Last[ startW ] =!= Last[ targetW ], Return[ False ] ];
    rules        = resolveFaces[ graph, OptionValue[ HomotopicQ, { opts }, "NullHomotopicCycles" ] ];
    maxMoves     = OptionValue[ HomotopicQ, { opts }, "MaxMoves" ];
    maxLen       = OptionValue[ HomotopicQ, { opts }, "MaxLength" ] /.
                     Automatic :> autoMaxLength[ { startW, targetW }, rules ];
    result       = walkSpaceBFS[ graph, startW, rules, maxLen, maxMoves,
                     ( #1 === targetW & ), slides, canonicalize ];
    result[[ 2 ]] =!= $NotFound
  ]


(* ===================== NullHomotopicQ ===================== *)

(* a closed walk is null-homotopic iff it is homotopic, as a based loop, to the constant walk at its base point; a vertex list or an open walk graph is read as closed *)

Options[ NullHomotopicQ ] = $infraHomotopyOptions;

NullHomotopicQ[ graph_Graph, cycle_List, opts : OptionsPattern[] ] :=
  With[ { closed = closeWalk @ cycle },
    HomotopicQ[ graph, closedWalkGraph @ closed, closedWalkGraph @ { First @ closed }, opts ] ]

NullHomotopicQ[ graph_Graph, ws : { __Graph }, opts : OptionsPattern[] ] :=
  AllTrue[ ws, NullHomotopicQ[ graph, #, opts ] & ]

NullHomotopicQ[ graph_Graph, w_Graph, opts : OptionsPattern[] ] :=
  AllTrue[ walkRealisations @ w, NullHomotopicQ[ graph, #, opts ] & ]


(* ===================== Move classification ===================== *)

(* an elementary move replaces one arc of a cycle by the complementary one, so it changes walk length by |newArc| - |oldArc| *)

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


runWalkBFS[ graph_Graph, walk_List, closedQ_, freeHom_, stopWhen_, opts___ ] :=
  Module[ { modeInfo, canonicalize, slides, startW, rules, maxMoves, maxLen },
    modeInfo     = walkModeFor[ closedQ, freeHom ];
    slides       = modeInfo[[ 1 ]];
    canonicalize = modeInfo[[ 2 ]];
    startW       = coerceRealisation[ closedQ, canonicalize, walk ];
    rules        = resolveFaces[ graph,
                     OptionValue[ FindInfraHomotopyRepresentative, { opts }, "NullHomotopicCycles" ] ];
    maxMoves     = OptionValue[ FindInfraHomotopyRepresentative, { opts }, "MaxMoves" ];
    maxLen       = OptionValue[ FindInfraHomotopyRepresentative, { opts }, "MaxLength" ] /.
                     Automatic :> autoMaxLength[ { startW }, rules ];
    walkSpaceBFS[ graph, startW, rules, maxLen, maxMoves, stopWhen, slides, canonicalize ]
  ]


(* bidirectional BFS in walk-space; stopWhen short-circuits, otherwise the bounded walk-space is exhausted *)

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


(* DFS to the neighbour of smallest score strictly below the current one, without backtracking: it ends at the first walk admitting no improving move, or at target if the search hits it *)

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


(* symmetric Hausdorff distance between the vertex sets of two walks, the greedy score in FindInfraHomotopy *)

hausdorffMove[ graph_Graph, walkA_List, walkB_List ] :=
  With[ { setA = DeleteDuplicates @ walkA, setB = DeleteDuplicates @ walkB },
    { dMat = Outer[ GraphDistance[ graph, #1, #2 ] &, setA, setB ] },
    Max[ Min /@ dMat, Min /@ Transpose @ dMat ]
  ]


(* ===================== Move set ===================== *)


elementaryMoves[ path_List, vN_Association, rules_Association, cycleMoves_List,
    addSlides : ( True | False ) : False ] :=
  DeleteDuplicates @ Join[
    If[ rules[ "Dup" ],  consecutiveDupMoves[ path ], { } ],
    If[ rules[ "Spur" ], spurMovesAt[ path, vN ],     { } ],
    Catenate[ applyMove[ path, # ] & /@ cycleMoves ],
    If[ addSlides, endpointSlideMoves[ path, vN ], { } ]
  ]


(* endpoint slide: extend or retract at either end by one vertex, never to an empty walk *)

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


(* for a closed face (f_1, ..., f_k), each cut (s, L) gives oldArc = L edges from slot s+1 and newArc = the complementary k-L edges, reversed *)

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


autoMaxLength[ walks_List, rules_Association ] :=
  Max[ Length /@ walks ] + 2 * maxCycleLengthOf[ rules ]

maxCycleLengthOf[ rules_Association ] :=
  With[ { cs = rules[ "Cycles" ] },
    Max[ 3, If[ cs === { }, 0, Max[ Length /@ cs ] ] ]
  ]


(* ===================== Helpers ===================== *)

closeWalk[ cycle_List ] :=
  If[ First[ cycle ] === Last[ cycle ], cycle, Append[ cycle, First[ cycle ] ] ]


(* the free loop's canonical form: the lex-least cyclic rotation of the core Most @ closeWalk @ walk *)

canonicalString[ { } ]      := { }
canonicalString[ { v_ } ]   := { v }
canonicalString[ walk_List ] /; Length[ walk ] >= 2 :=
  With[ { core = If[ First @ walk === Last @ walk, Most @ walk, walk ] },
    First @ SortBy[ Table[ RotateLeft[ core, k ], { k, 0, Length[ core ] - 1 } ], Identity ]
  ]


loopRotations[ c_List ] /; Length[ c ] <= 1 := { c }

loopRotations[ c_List ] :=
  With[ { core = Most @ c },
    DeleteDuplicates @ Table[
      With[ { shifted = RotateLeft[ core, k ] }, Append[ shifted, First[ shifted ] ] ],
      { k, 0, Length[ core ] - 1 } ]
  ]


minimalReached[ parent_Association ] :=
  With[ { walks = Keys[ parent ] },
    { minLen = Min[ Length /@ walks ] },
    Select[ walks, Length[ # ] == minLen & ]
  ]


reconstructChain[ parent_Association, target_List ] :=
  Module[ { chain = { target }, current = target },
    While[ parent[ current ] =!= None,
      current = parent[ current ];
      PrependTo[ chain, current ] ];
    chain
  ]
