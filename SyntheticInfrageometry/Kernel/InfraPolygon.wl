Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[matchPolygonSlot]
PackageScope[kDiagonals]
PackageScope[findPolygonCore]


(* ===================== InfraPolygon wrapper ===================== *)

(* InfraPolygon[{poly}] is the unary form: poly = {seg1, ..., segn} is a closed
   chain of unary InfraSegment sides (Last[path_j] === First[path_{j+1}], and
   the last side returns to the first corner).  InfraPolygon[{poly1, ..., polyk}]
   is the multi-realisation form.  Auto-flatten on nested wrappers. *)

InfraPolygon[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPolygon[ _List ] ] ] :=
  InfraPolygon[ Flatten[ reps /. InfraPolygon[ xs_List ] :> xs, 1 ] ]

(* "Sides" = the InfraSegment legs per realisation. *)
InfraPolygon[ reps_List ][ "Sides" ] := reps

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraPolygon[ reps_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraPolygon[ reps ] ]
InfraPolygon[ reps_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraPolygon[ reps ] ]
InfraPolygon[ reps_List ][ "Measure" ] := InfraMeasure[ InfraPolygon[ reps ] ]
InfraPolygon[ reps_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraPolygon[ reps ], Method -> "Probability" ]
(* "Length" = per-realisation perimeter edge count (sum of leg edge counts). *)
InfraPolygon[ reps_List ][ "Length" ] :=
  Replace[ reps,
    { { }              -> 0,
      segs : { _InfraSegment .. } :> Total[ ( Length[ #[[ 1, 1 ]] ] - 1 ) & /@ segs ] },
    { 1 } ]

(* "Vertices" = corner vertices per realisation, as unary InfraPoints. *)
InfraPolygon[ reps_List ][ "Vertices" ] :=
  Map[ poly |-> ( InfraPoint[ { # } ] & /@ Most @ polylineToKnots[ poly ] ), reps ]


(* ===================== FindInfraPolygon ===================== *)

(* Polygon through corners p1, ..., pn (n >= 3): each side (p_i, p_{i+1 mod n})
   is a geodesic InfraSegment; a realisation is one geodesic per side, and the
   Cartesian product over sides enumerates all polygons.  Sides reuse
   findSegmentCore, so Properties / Method forward to the side geodesics. *)

Options[ FindInfraPolygon ] = { Properties -> { }, Method -> "Exhaustive" };

FindInfraPolygon[ graph_Graph, vertices_List /; Length[ vertices ] >= 3,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { core = findPolygonCore[ graph, vertices, count, opts ] },
    If[ core === $Failed, $Failed, InfraPolygon[ { # } ] & /@ core ]
  ]


findPolygonCore[ graph_Graph, vertices_List, count_, opts : OptionsPattern[ FindInfraSegment ] ] :=
  With[ { corners = polygonCorner /@ vertices },
    With[ { sideReals = Function[ pair,
        findSegmentCore[ graph, pair[[ 1 ]], pair[[ 2 ]], All, opts ] ] /@
        Partition[ Append[ corners, First @ corners ], 2, 1 ] },
      If[ MemberQ[ sideReals, $Failed ], $Failed,
        infraCap[
          Map[ paths |-> ( InfraSegment[ { # } ] & /@ paths ), Tuples @ sideReals ],
          count ]
      ]
    ]
  ]

polygonCorner[ InfraPoint[ { v_ } ] ] := v
polygonCorner[ v_ ]                   := v


(* ===================== InfraPolygonQ ===================== *)

(* A polygon is a closed chain of geodesic InfraSegment sides: every leg a
   geodesic and consecutive legs (cyclically) share their endpoint. *)

InfraPolygonQ[ graph_Graph, InfraPolygon[ reps_List ] ] :=
  AllTrue[ reps, InfraPolygonQ[ graph, # ] & ]

InfraPolygonQ[ graph_Graph, poly : { _InfraSegment .. } ] :=
  AllTrue[ poly, InfraSegmentQ[ graph, #[[ 1, 1 ]] ] & ] &&
  AllTrue[ Partition[ Append[ poly, First @ poly ], 2, 1 ],
    pair |-> Last[ pair[[ 1, 1, 1 ]] ] === First[ pair[[ 2, 1, 1 ]] ] ]

InfraPolygonQ[ _Graph, _ ] := False


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

Options[ FindInfraRegularPolygon ] = {
  Properties -> { },
  Method     -> "Exhaustive",
  "From"     -> All
};

FindInfraRegularPolygon[ graph_Graph, As_List, n_Integer /; n >= 3,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { core = Module[ { properties, methodSpec, dm, idx, vs, candidates, pruning, methodHead,
              fromSpec, anchor, radius, workGraph, workVs, workDm },
      properties = OptionValue[ FindInfraRegularPolygon, { opts }, Properties ];
      methodSpec = OptionValue[ FindInfraRegularPolygon, { opts }, Method ];
      fromSpec   = OptionValue[ FindInfraRegularPolygon, { opts }, "From" ];
      Catch[
        If[ properties =!= { },
          Message[ FindInfraRegularPolygon::badproperty, First @ properties ]; Throw[ $Failed ] ];
        If[ Length[ As ] < 1 || Length[ As ] > Floor[ n / 2 ],
          Message[ FindInfraRegularPolygon::badcount, As, n ]; Throw[ $Failed ] ];
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
    ] },
    If[ core === $Failed, $Failed,
      With[ { capped = infraCap[ core, count ] },
        If[ capped === $Failed, $Failed,
          (* promote each closed corner cycle to a closed chain of geodesic
             InfraSegment sides (first shortest path per side) *)
          Map[
            cyc |-> InfraPolygon[ {
              MapThread[ { a, b } |-> InfraSegment[ { FindShortestPath[ graph, a, b ] } ],
                { cyc, RotateLeft @ cyc } ] } ],
            capped ] ]
      ]
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


normalizeAnchor[ InfraPoint[ { v_ } ] ]                              := v
normalizeAnchor[ InfraPoint[ vs_List ] ]                             := vs
normalizeAnchor[ list_List ] /; AllTrue[ list, MatchQ[ InfraPoint[ { _ } ] ] ] :=
  list[[ All, 1, 1 ]]
normalizeAnchor[ v_ ]                                                := v


anchorContainedQ[ cyc_List, anchor_List ] := IntersectingQ[ cyc, anchor ]
anchorContainedQ[ cyc_List, anchor_ ]     := MemberQ[ cyc, anchor ]


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
        AllTrue[ As, MatchQ[ _Integer | { _Integer, _Integer } | Automatic ] ] &&
        AllTrue[ Range @ Length @ As,
          matchPolygonSlot[ #, As[[ # ]], open, dm, idx ] & ]
      ]
    ]
  ]

InfraRegularPolygonQ[ _Graph, cycle_List, _List ] /; Length[ cycle ] < 3 := False


(* ===================== Scene-DSL constructors ===================== *)

(* Regular n-gon (As distance tuple + size n) and the general through-corner
   polygon (a single vertex list) are disambiguated by arity. *)

dispatchConstruction[ graph_Graph, InfraPolygon[ As_List, n_Integer, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindInfraRegularPolygon[ graph, As, n, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraRegularPolygon ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      True, <||> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraPolygon[ verts_List, opts___Rule ] ] :=
  capBranches[
    #[[ 1, 1 ]] & /@ FindInfraPolygon[ graph, verts, All,
      Sequence @@ FilterRules[ { opts }, Options[ FindInfraPolygon ] ] ],
    extractBranches[ { opts } ] ]
