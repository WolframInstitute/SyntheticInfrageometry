Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findRegularPolygonCore]
PackageScope[matchPolygonSlot]
PackageScope[kDiagonals]


(* ===================== InfraPolygon wrapper ===================== *)

(* InfraPolygon[{cycle}] is the unary form; InfraPolygon[{cycle1, ..., cyclek}]
   is the multi-realisation form.  Auto-flatten on nested wrappers. *)

InfraPolygon[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPolygon[ _List ] ] ] :=
  InfraPolygon[ Flatten[ reps /. InfraPolygon[ xs_List ] :> xs, 1 ] ]

(* "Length" = vertex count per realisation (= edge count, wrap-around implicit). *)
InfraPolygon[ reps_List ][ "Length" ] := Length /@ reps


(* ===================== FindInfraRegularPolygon ===================== *)

(* A regular n-gon w.r.t. metric tuple As is a length-n cyclic vertex
   sequence v_1, ..., v_n with d(v_i, v_{i+k mod n}) satisfying As[[k]] for
   every i and every k = 1..Length[As].  Slot grammar:
     A_Integer                   exact value
     {lo_Integer, hi_Integer}    constant across i, value in [lo, hi]
     Automatic                   constant across i, any value
   With As[[1]] = Automatic the candidate space is the full set of simple
   n-cycles of g (slow on large graphs); otherwise candidates come from
   FindCycle on the distance-As[[1]] subgraph. *)

FindInfraRegularPolygon::badproperty = "Property `1` is not supported by FindInfraRegularPolygon.";
FindInfraRegularPolygon::badmethod   = "Method `1` is not supported by FindInfraRegularPolygon.";
FindInfraRegularPolygon::badcount    = "Diagonal tuple `1` has length exceeding Floor[n/2] for n = `2`.";
FindInfraRegularPolygon::badslot     = "Slot entry `1` is not Integer | {lo, hi} | Automatic.";
FindInfraRegularPolygon::badfrom     = "\"From\" specification `1` is not All | v | v -> r_Integer.";

Options[ FindInfraRegularPolygon ] = {
  Properties -> { },
  Method     -> "Exhaustive",
  "From"     -> All
};

FindInfraRegularPolygon[ graph_Graph, As_List, n_Integer /; n >= 3,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { core = findRegularPolygonCore[ graph, As, n, opts ] },
    If[ core === $Failed, $Failed,
      With[ { capped = infraCap[ core, count ] },
        If[ capped === $Failed, $Failed, InfraPolygon[ { # } ] & /@ capped ]
      ]
    ]
  ]


findRegularPolygonCore[ graph_Graph, As_List, n_Integer, opts : OptionsPattern[ FindInfraRegularPolygon ] ] :=
  Module[ { properties, methodSpec, dm, idx, vs, candidates, pruning, methodHead,
            fromSpec, anchor, radius, workGraph, workVs, workDm },
    properties = OptionValue[ FindInfraRegularPolygon, { opts }, Properties ];
    methodSpec = OptionValue[ FindInfraRegularPolygon, { opts }, Method ];
    fromSpec   = OptionValue[ FindInfraRegularPolygon, { opts }, "From" ];
    Catch[
      If[ properties =!= { },
        Message[ FindInfraRegularPolygon::badproperty, First @ properties ]; Throw[ $Failed ] ];
      If[ Length[ As ] < 1 || Length[ As ] > Floor[ n / 2 ],
        Message[ FindInfraRegularPolygon::badcount, As, n ]; Throw[ $Failed ] ];
      validateSlots @ As;
      methodHead = methodName @ methodSpec;
      If[ methodHead =!= "Exhaustive",
        Message[ FindInfraRegularPolygon::badmethod, methodSpec ]; Throw[ $Failed ] ];
      { anchor, radius } = parseFromSpec @ fromSpec;
      pruning = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity;
      dm  = GraphDistanceMatrix @ graph;
      vs  = VertexList @ graph;
      idx = AssociationThread[ vs -> Range @ Length @ vs ];
      workGraph = If[ anchor === None || radius === All, graph,
        NeighborhoodGraph[ graph, anchor, radius ] ];
      workVs = VertexList @ workGraph;
      workDm = If[ workGraph === graph, dm, dm[[ idx /@ workVs, idx /@ workVs ]] ];
      candidates = cycleToVertexSequence /@
        FindCycle[ candidateSourceGraph[ workGraph, First @ As, workDm, workVs ], { n }, All ];
      candidates = applyPruning[ candidates, pruning ];
      If[ anchor =!= None && radius === All,
        candidates = Select[ candidates, anchorContainedQ[ #, anchor ] & ] ];
      DeleteDuplicates @ Select[ candidates,
        cyc |-> AllTrue[ Range @ Length @ As,
          matchPolygonSlot[ #, As[[ # ]], cyc, dm, idx ] & ] ]
    ]
  ]


(* "From" parser: returns {anchor, radius} pair.
     All                        -> {None,  All}    (no localization)
     v                          -> {v,     All}    (cycles containing v)
     v -> r                     -> {v,     r}      (cycles within N_r(v))
     InfraPoint[{v}]            -> {v,     All}    (unwrap unary wrapper)
     InfraPoint[{v1,...,vk}]    -> {{...}, All}    (multi-anchor: union semantics)
     {InfraPoint[{v1}],...}     -> {{...}, All}    (list of unaries, e.g. FindInfraPoint output)
   Anchor in the localization (-> r) case is forwarded directly to
   NeighborhoodGraph (which accepts vertex or vertex-list); anchor in the
   membership case is dispatched by anchorContainedQ (MemberQ vs IntersectingQ). *)

parseFromSpec[ All ]                              := { None, All }
parseFromSpec[ ( anchor_ -> r_Integer ) ] /; r >= 0 :=
  { normalizeAnchor @ anchor, r }
parseFromSpec[ anchor_ ] /; ! MatchQ[ anchor, _Rule ] :=
  { normalizeAnchor @ anchor, All }
parseFromSpec[ bad_ ] :=
  ( Message[ FindInfraRegularPolygon::badfrom, bad ]; Throw[ $Failed ] )


normalizeAnchor[ InfraPoint[ { v_ } ] ]                              := v
normalizeAnchor[ InfraPoint[ vs_List ] ]                             := vs
normalizeAnchor[ list_List ] /; AllTrue[ list, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  list[[ All, 1, 1 ]]
normalizeAnchor[ v_ ]                                                := v


anchorContainedQ[ cyc_List, anchor_List ] := IntersectingQ[ cyc, anchor ]
anchorContainedQ[ cyc_List, anchor_ ]     := MemberQ[ cyc, anchor ]


validateSlots[ As_List ] :=
  Scan[
    Replace[ #,
      { _Integer | { _Integer, _Integer } | Automatic :> Null,
        bad_ :> ( Message[ FindInfraRegularPolygon::badslot, bad ]; Throw[ $Failed ] ) } ] &,
    As ]


(* Candidate-cycle source: integer/range slot 1 -> distance subgraph;
   Automatic -> g itself.  Self-loops removed via diagonal mask. *)

candidateSourceGraph[ graph_Graph, Automatic, _, _ ] := graph

candidateSourceGraph[ _Graph, a_Integer, dm_, vs_List ] :=
  AdjacencyGraph[ vs, distanceMask[ dm, # === a & ] ]

candidateSourceGraph[ _Graph, { lo_Integer, hi_Integer }, dm_, vs_List ] :=
  AdjacencyGraph[ vs, distanceMask[ dm, lo <= # <= hi & ] ]


distanceMask[ dm_, predicate_ ] :=
  With[ { mask = Boole @ Map[ predicate, dm, { 2 } ] },
    mask - DiagonalMatrix @ Diagonal @ mask ]


(* k-diagonal distance list, length n, wrap-around implicit. *)

kDiagonals[ k_Integer, cyc_List, dm_, idx_Association ] :=
  With[ { n = Length @ cyc },
    Table[ dm[[ idx @ cyc[[ i ]], idx @ cyc[[ Mod[ i + k - 1, n ] + 1 ]] ]], { i, n } ]
  ]


(* Slot match per shape: exact / range / any-constant. *)

matchPolygonSlot[ k_Integer, a_Integer, cyc_List, dm_, idx_Association ] :=
  AllTrue[ kDiagonals[ k, cyc, dm, idx ], # === a & ]

matchPolygonSlot[ k_Integer, { lo_Integer, hi_Integer }, cyc_List, dm_, idx_Association ] :=
  With[ { ds = kDiagonals[ k, cyc, dm, idx ] },
    Length[ Union @ ds ] === 1 && lo <= First @ ds <= hi
  ]

matchPolygonSlot[ k_Integer, Automatic, cyc_List, dm_, idx_Association ] :=
  Length[ Union @ kDiagonals[ k, cyc, dm, idx ] ] === 1


(* ===================== InfraRegularPolygonQ ===================== *)

(* Tests whether `cycle` is a regular n-gon w.r.t. metric tuple As.
   Accepts open ({v1, ..., vn}) or closed ({v1, ..., vn, v1}) input.
   Same slot grammar as FindInfraRegularPolygon. *)

InfraRegularPolygonQ[ graph_Graph, cycle_List, As_List ] /; Length[ cycle ] >= 3 :=
  With[ { open = If[ First @ cycle === Last @ cycle, Most @ cycle, cycle ] },
    With[ {
        n   = Length @ open,
        dm  = GraphDistanceMatrix @ graph,
        vs  = VertexList @ graph },
      With[ { idx = AssociationThread[ vs -> Range @ Length @ vs ] },
        DuplicateFreeQ[ open ] &&
        Length[ As ] >= 1 && Length[ As ] <= Floor[ n / 2 ] &&
        validSlotShapesQ[ As ] &&
        AllTrue[ Range @ Length @ As,
          matchPolygonSlot[ #, As[[ # ]], open, dm, idx ] & ]
      ]
    ]
  ]

InfraRegularPolygonQ[ _Graph, cycle_List, _List ] /; Length[ cycle ] < 3 := False


validSlotShapesQ[ As_List ] :=
  AllTrue[ As, MatchQ[ _Integer | { _Integer, _Integer } | Automatic ] ]
