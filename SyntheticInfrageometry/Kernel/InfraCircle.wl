Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findCircleCore]
PackageScope[cycleToVertexSequence]
PackageScope[findCyclesWithPruning]
PackageScope[admissibleCircleSet]
PackageScope[extractAdmissibleCycle]
PackageScope[greedyFirstAdmissibleCycle]


(* ===================== InfraCircle wrapper ===================== *)

(* InfraCircle[{cycle}] is the unary form; InfraCircle[{cycle1, ..., cyclek}]
   is the multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraCircle[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraCircle[ _List ] ] ] :=
  InfraCircle[ Flatten[ reps /. InfraCircle[ xs_List ] :> xs, 1 ] ]

(* "Length" = circumference per realisation: k for a k-vertex open cycle
   (wrap-around edge implicit, so #edges == #vertices). *)
InfraCircle[ reps_List ][ "Length" ] := Length /@ reps


(* ===================== FindInfraCircle ===================== *)

(* A circle of radius r around c is a simple cycle in the level-surface
   subgraph at distance ~r from c.  Returns open vertex sequences
   { v0, v1, ..., vk } (the wrap-around edge is implicit).  The single
   axis is Properties (a set, order-insensitive):
     "Separating" -- cycle's vertex set disconnects c from
       { v : d(c, v) > rmax }; the topological condition that makes a
       level-surface cycle a genuine circle.
     "Shortest"   -- only cycles tied at the minimum admissible length
       (the canonical-optimum reading); the length sweep stops at the
       first non-empty length class.
   Default {"Separating", "Shortest"} returns the canonical infra-circle
   (shortest separating cycle) and its ties.  Drop "Shortest" to accept
   progressively longer separating cycles; drop "Separating" to accept
   any simple cycle in the level surface.  Unknown property names
   (including "Connected", since cycles are always connected) raise
   ::badproperty.  The algorithm is a single length-by-length sweep
   with FindCycle; there is no Method axis. *)

FindInfraCircle::badproperty = "Property `1` is not supported by FindInfraCircle.";

Options[ FindInfraCircle ] = {
  Properties -> { "Separating", "Shortest" }
};

FindInfraCircle[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraCircle, count,
    findCircleCore[ graph, ##, count, opts ] &, p, r ]


findCircleCore[ graph_Graph, p_, r_, count_, opts : OptionsPattern[ FindInfraCircle ] ] :=
  Module[ { properties, unknown, range, localG, levelSet, radius, levelGraph,
            vertsTest, tied, needed, k, kMax, batch, matching, accumulated },
    properties = OptionValue[ FindInfraCircle, { opts }, Properties ];
    Catch[
      unknown = Complement[ properties, { "Separating", "Shortest" } ];
      If[ unknown =!= { },
        Message[ FindInfraCircle::badproperty, First @ unknown ]; Throw[ $Failed ] ];
      range = Replace[ r, d_?NumericQ :> { d, d } ];
      localG = If[ NumericQ[ range[[ 2 ]] ],
                   NeighborhoodGraph[ graph, p, Ceiling[ range[[ 2 ]] ] + 2 ], graph ];
      levelSet = Select[ VertexList[ localG ],
        range[[ 1 ]] <= GraphDistance[ localG, p, # ] <= range[[ 2 ]] & ];
      radius = If[ NumericQ[ r ], r, Mean[ r ] ];
      levelGraph = Subgraph[ localG, levelSet ];
      vertsTest  = admissibleCircleVerts[ localG, p, radius,
                     DeleteCases[ properties, "Shortest" ] ];
      tied = MemberQ[ properties, "Shortest" ];
      needed = Switch[ count, _Integer, count, UpTo[ _Integer ], First @ count, _, Infinity ];
      kMax = VertexCount[ levelGraph ];
      accumulated = { };
      k = 3;
      While[ k <= kMax,
        batch    = cycleToVertexSequence /@ FindCycle[ levelGraph, { k }, All ];
        matching = Select[ batch, vertsTest ];
        If[ matching =!= { },
          accumulated = Join[ accumulated, matching ];
          If[ tied || Length[ accumulated ] >= needed, Break[ ] ]
        ];
        k++
      ];
      accumulated
    ]
  ]


cycleToVertexSequence[ cyc_List ] := First /@ cyc

(* The findCyclesWithPruning / admissibleCircleSet / extractAdmissibleCycle /
   greedyFirstAdmissibleCycle helpers below are no longer used by
   FindInfraCircle (collapsed to a single length sweep in findCircleCore);
   they remain because FindInfraEllipse still consumes them. *)

(* Integer pruning caps FindCycle's enumeration; Infinity enumerates all;
   Bernoulli subsamples post-enumeration. *)
findCyclesWithPruning[ g_Graph, Infinity ]                  := FindCycle[ g, Infinity, All ]
findCyclesWithPruning[ g_Graph, n_Integer /; n >= 1 ]       := FindCycle[ g, Infinity, n ]
findCyclesWithPruning[ g_Graph, p_?NumericQ /; 0 < p < 1 ] :=
  applyPruning[ FindCycle[ g, Infinity, All ], p ]


admissibleCircleVerts[ localG_Graph, center_, radius_, properties_List ] :=
  With[ { tests = propertyPredicateCircle[ localG, center, radius, # ] & /@ properties },
    verts |-> AllTrue[ tests, # @ verts & ]
  ]

admissibleCircleSet[ levelGraph_Graph, vertsTest_ ] :=
  t |-> Length[ t ] >= 3 &&
        AnyTrue[ FindCycle[ Subgraph[ levelGraph, t ], Infinity, All ],
          cyc |-> vertsTest[ cycleToVertexSequence @ cyc ] ]


propertyPredicateCircle[ localG_Graph, center_, radius_, "Separating" ] :=
  verts |-> SeparatingSetQ[ localG, verts, center, radius ]

propertyPredicateCircle[ _, _, _, other_ ] :=
  ( Message[ FindInfraCircle::badproperty, other ]; Throw[ $Failed ] )


extractAdmissibleCycle[ localG_Graph, vertsTest_, t_List ] :=
  SelectFirst[
    cycleToVertexSequence /@ FindCycle[ Subgraph[ localG, t ], Length @ t, All ],
    vertsTest, Missing[ ] ]

(* Iterate FindCycle by length, returning the first admissible cycle.
   On a connected level-surface subgraph the bound is VertexCount; the
   loop short-circuits at the first hit and never enumerates longer
   cycles. *)

greedyFirstAdmissibleCycle[ levelGraph_Graph, vertsTest_ ] :=
  Module[ { k = 3, kMax = VertexCount @ levelGraph, found = Missing[ ] },
    While[ k <= kMax && MissingQ @ found,
      found = SelectFirst[
        cycleToVertexSequence /@ FindCycle[ levelGraph, { k }, All ],
        vertsTest, Missing[ ] ];
      k++
    ];
    If[ MissingQ @ found, { }, { found } ]
  ]


(* ===================== FindInfraCycle ===================== *)

(* Simple cycles on graph (topological, not metric circles), returned as
   InfraCircle wrappers for direct use with NullHomotopicQ /
   FindInfraHomotopy.  Sorted by length ascending. *)

FindInfraCycle[ graph_Graph, n : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  FindInfraCycle[ graph, { 1, VertexCount[ graph ] }, n ]

FindInfraCycle[ graph_Graph, { k_Integer },
    n : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  infraCap[
    InfraCircle[ { # } ] & /@ (cycleToVertexSequence /@ FindCycle[ graph, { k }, All ]),
    n
  ]

FindInfraCycle[ graph_Graph, { kMin_Integer, kMax_ },
    n : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { maxK = Min[ kMax, VertexCount[ graph ] ] },
    infraCap[
      InfraCircle[ { # } ] & /@ SortBy[ Length ] @
        Flatten[ cycleToVertexSequence /@ FindCycle[ graph, { # }, All ] & /@ Range[ kMin, maxK ], 1 ],
      n
    ]
  ]


(* ===================== InfraCircleQ ===================== *)

(* InfraCircleQ[g, cycle]: the vertex sequence is a metric circle iff
   consecutive vertices (and the wrap-around) are adjacent and the
   underlying vertex set is a metric shell.  Accepts open ({v0, ..., vk},
   vk != v0) and closed ({v0, ..., vk, v0}) input. *)

InfraCircleQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    With[ {
        verts = Most @ closed,
        pairs = Partition[ closed, 2, 1 ] },
      DuplicateFreeQ[ verts ] &&
      AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
      Length[ FindInfraShellParameters[ graph, verts ] ] > 0
    ]
  ]

InfraCircleQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraCircle[ center_, r_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindInfraCircle[ graph, center, r, All ],
      "Select" /. { opts } /. "Select" -> None,
      True, <| "Center" -> center,
               "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]
