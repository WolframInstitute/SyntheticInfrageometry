Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[matchPolygonSlot]
PackageScope[kDiagonals]
PackageScope[findPolygonCore]


(* ===================== FindInfraPolygon ===================== *)

(* a polygon through the listed corners is its sides: one geodesic (p_i, p_{i+1 mod n}) per consecutive pair, each a directed path graph on the substrate vertices, consecutive sides sharing their corner and the last closing on the first.  The instance is the List of sides, since a corner is a fact about the polygon and not about the closed walk, which may even retrace a side; the count-less call is one polygon, a bounded count and All a List of them, and the Cartesian product over sides is the class *)

FindInfraPolygon::badmethod = "Method `1` is not supported by FindInfraPolygon.";

Options[ FindInfraPolygon ] = { Method -> Automatic };

FindInfraPolygon[ graph_Graph, vertices_List /; Length[ vertices ] >= 3,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  findPolygonCore[ FindInfraPolygon, graph, vertices, count, opts ]


(* "Exhaustive" with All forms the product; a bounded count streams that many geodesics per side, in candidate ("Greedy", "Exhaustive") or random ("RandomGreedy") order, and reads members of their product off its mixed-radix index.  head is the calling symbol, read for its options and messages *)

findPolygonCore[ head_, graph_Graph, vertices_List, count_, opts : OptionsPattern[] ] :=
  With[ { methodSpec = resolveMethod[ OptionValue[ head, { opts }, Method ], count ],
          corners = polygonCorner /@ vertices },
    If[ ! MatchQ[ methodName @ methodSpec, "Exhaustive" | "Greedy" | "RandomGreedy" ],
      Message[ MessageName[ head, "badmethod" ], methodSpec ]; $Failed,
      (* a bounded count streams more geodesics per side than it needs: with one per side the product is a single tuple, and a witness has no room to avoid degeneracy *)
      With[ { sideCount = If[ count === All, All, UpTo[ Max[ 8, 2 countLimit @ count ] ] ] },
        With[ { sides = findSegmentCore[ graph, #1, #2, sideCount, Method -> methodSpec ] & @@@
                Partition[ Append[ corners, First @ corners ], 2, 1 ] },
          countTake[
            Map[ geodesicGraph, If[ count === All, Tuples @ sides, nonDegenerateFirst[ sides, countLimit @ count ] ], { 2 } ],
            count ] ] ] ] ]


(* a bounded count takes the product's members in mixed-radix order, but a degenerate polygon -- one whose closed side sequence walks an edge twice, the 1-3-9 triangle of GridGraph[{3,3}] closing along 9-6-3-2-1 -- is a poor witness for a class that also holds honest ones.  The scan window is wide enough to pass over the degenerate prefix and still exact: it yields Min[n, |class|] polygons, degenerate ones only once the window is spent *)

nonDegenerateFirst[ sides_List, n_Integer ] :=
  With[ { sizes = Length /@ sides },
    { total = Times @@ sizes },
    { scanned = Table[
        MapThread[ Part, { sides, 1 + IntegerDigits[ j, MixedRadix @ sizes, Length @ sides ] } ],
        { j, 0, Min[ total, Max[ 200, 20 n ] ] - 1 } ] },
    Take[ Join[ Select[ scanned, ! polygonRetracesQ @ # & ], Select[ scanned, polygonRetracesQ ] ],
      UpTo[ Min[ n, total ] ] ]
  ]

nonDegenerateFirst[ sides_List, Infinity ] := Tuples @ sides


(* the closed vertex sequence of the polygon, and whether it repeats an edge *)

polygonRetracesQ[ tuple_List ] :=
  With[ { closed = Join @@ Prepend[ Rest /@ Rest @ tuple, First @ tuple ] },
    ! DuplicateFreeQ[ Sort /@ Partition[ closed, 2, 1 ] ]
  ]

polygonCorner[ v_ ]                   := v


(* ===================== InfraPolygonQ ===================== *)

(* every side a geodesic and consecutive sides, cyclically, sharing their endpoint *)

InfraPolygonQ[ graph_Graph, polys : { { __Graph } .. } ] :=
  AllTrue[ polys, InfraPolygonQ[ graph, # ] & ]

InfraPolygonQ[ graph_Graph, sides : { __Graph } ] :=
  With[ { seqs = walkSequence /@ sides },
    AllTrue[ seqs, InfraSegmentQ[ graph, # ] & ] &&
    AllTrue[ Partition[ Append[ seqs, First @ seqs ], 2, 1 ], pair |-> Last[ pair[[ 1 ]] ] === First[ pair[[ 2 ]] ] ] ]

InfraPolygonQ[ _Graph, _ ] := False


(* ===================== FindInfraRegularPolygon ===================== *)

(* a regular n-gon w.r.t. the metric tuple As is a cyclic sequence v_1, ..., v_n with d(v_i, v_{i+k mod n}) satisfying As[[k]] for every i and k; a slot is an exact integer, a range {lo, hi} constant across i, or Automatic.  The instance is the polygon on those corners: its sides, one shortest path each.

   The family is carried by the FindCycle candidate sweep, filtered by the slot predicates.  The sweep is not lazy -- every n-cycle of the candidate graph is materialised before any is tested -- so "Greedy" and "RandomGreedy" here only order what the count takes, in candidate and random order respectively; the class is the same under all three *)

FindInfraRegularPolygon::badproperty = "Property `1` is not supported by FindInfraRegularPolygon.";
FindInfraRegularPolygon::badmethod   = "Method `1` is not supported by FindInfraRegularPolygon.";
FindInfraRegularPolygon::badcount    = "Diagonal tuple `1` has length exceeding Floor[n/2] for n = `2`.";

Options[ FindInfraRegularPolygon ] = {
  Properties -> { },
  Method     -> Automatic,
  "From"     -> All
};

FindInfraRegularPolygon[ graph_Graph, As_List, n_Integer /; n >= 3,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  With[ { core = Module[ { properties, methodSpec, dm, idx, vs, candidates, pruning, methodHead,
              fromSpec, anchor, radius, workGraph, workVs, workDm },
      properties = OptionValue[ FindInfraRegularPolygon, { opts }, Properties ];
      methodSpec = resolveMethod[ OptionValue[ FindInfraRegularPolygon, { opts }, Method ], count ];
      fromSpec   = OptionValue[ FindInfraRegularPolygon, { opts }, "From" ];
      Catch[
        If[ properties =!= { },
          Message[ FindInfraRegularPolygon::badproperty, First @ properties ]; Throw[ $Failed ] ];
        If[ Length[ As ] < 1 || Length[ As ] > Floor[ n / 2 ],
          Message[ FindInfraRegularPolygon::badcount, As, n ]; Throw[ $Failed ] ];
        methodHead = methodName @ methodSpec;
        If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
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
        candidates = greedyBranch[ methodHead /. "Exhaustive" -> "Greedy" ] @
          applyPruning[ candidates, pruning ];
        If[ anchor =!= None && radius === All,
          candidates = Select[ candidates, anchorContainedQ[ #, anchor ] & ] ];
        DeleteDuplicates @ Select[ candidates,
          cyc |-> AllTrue[ Range @ Length @ As,
            matchPolygonSlot[ #, As[[ # ]], cyc, dm, idx ] & ] ]
      ]
    ] },
    If[ core === $Failed, $Failed,
      countTake[
        Map[ cyc |-> MapThread[ { a, b } |-> geodesicGraph @ FindShortestPath[ graph, a, b ],
            { cyc, RotateLeft @ cyc } ],
          core ],
        count ] ]
  ]


parseFromSpec[ All ]                              := { None, All }
parseFromSpec[ ( anchor_ -> r_Integer ) ] /; r >= 0 :=
  { normalizeAnchor @ anchor, r }
parseFromSpec[ anchor_ ] /; ! MatchQ[ anchor, _Rule ] :=
  { normalizeAnchor @ anchor, All }


normalizeAnchor[ fam_Association ]                                   := Keys @ fam
normalizeAnchor[ list_List ] /; AllTrue[ list, MatchQ[ _Association ] ] :=
  list[[ All, 1, 1 ]]
normalizeAnchor[ v_ ]                                                := v


anchorContainedQ[ cyc_List, anchor_List ] := IntersectingQ[ cyc, anchor ]
anchorContainedQ[ cyc_List, anchor_ ]     := MemberQ[ cyc, anchor ]


(* integer/range slot 1 -> distance subgraph, Automatic -> g itself; self-loops removed via the diagonal mask *)

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

(* the corner cycle carries the test; a polygon is read at its corners, a cycle graph as its closed walk *)

InfraRegularPolygonQ[ graph_Graph, cycle_List, As_List ] /;
    Length[ cycle ] >= 3 && ! MatchQ[ cycle, { __Graph } | { { __Graph } .. } ] :=
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

InfraRegularPolygonQ[ _Graph, cycle_List, _List ] /;
    Length[ cycle ] < 3 && ! MatchQ[ cycle, { __Graph } | { { __Graph } .. } ] := False

InfraRegularPolygonQ[ graph_Graph, polys : { { __Graph } .. }, As_List ] :=
  AllTrue[ polys, InfraRegularPolygonQ[ graph, #, As ] & ]

InfraRegularPolygonQ[ graph_Graph, sides : { __Graph }, As_List ] :=
  InfraRegularPolygonQ[ graph, Most @ polylineToKnots @ sides, As ]

InfraRegularPolygonQ[ graph_Graph, w_Graph, As_List ] :=
  InfraRegularPolygonQ[ graph, walkSequence @ w, As ]


(* ===================== Scene-DSL constructors ===================== *)

(* the scene engine binds the closed corner sequence of a regular polygon and the closed vertex sequence of a polygon through corners *)

dispatchConstruction[ graph_Graph, InfraPolygon[ As_List, n_Integer, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      Most @* polylineToVertexSeq /@ FindInfraRegularPolygon[ graph, As, n, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraRegularPolygon ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      True, <||> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraPolygon[ verts_List, opts___Rule ] ] :=
  capBranches[
    polylineToVertexSeq /@ FindInfraPolygon[ graph, verts, All,
      Sequence @@ FilterRules[ { opts }, Options[ FindInfraPolygon ] ] ],
    extractBranches[ { opts } ] ]
