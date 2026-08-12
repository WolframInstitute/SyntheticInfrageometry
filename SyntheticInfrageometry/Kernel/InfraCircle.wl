Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraCircle wrapper ===================== *)

(* InfraCircle[{cycle}] is the unary form; InfraCircle[{cycle1, ..., cyclek}]
   is the multi-realisation form.  Set canonicalisation and the shared accessors
   come from defineInfraBundleRules (Tools.wl). *)

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
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  spreadFind[ InfraCircle, count,
    { p0, r0 } |-> Module[ { properties, unknown, range, localG, levelSet, radius, levelGraph,
              vertsTest, tied, needed, k, kMax, batch, matching, accumulated },
      properties = OptionValue[ FindInfraCircle, { opts }, Properties ];
      Catch[
        unknown = Complement[ properties, { "Separating", "Shortest" } ];
        If[ unknown =!= { },
          Message[ FindInfraCircle::badproperty, First @ unknown ]; Throw[ $Failed ] ];
        range = Replace[ r0, d_?NumericQ :> { d, d } ];
        localG = If[ NumericQ[ range[[ 2 ]] ],
                     NeighborhoodGraph[ graph, p0, Ceiling[ range[[ 2 ]] ] + 2 ], graph ];
        levelSet = Select[ VertexList[ localG ],
          range[[ 1 ]] <= GraphDistance[ localG, p0, # ] <= range[[ 2 ]] & ];
        radius = range[[ 2 ]];
        levelGraph = Subgraph[ localG, levelSet ];
        vertsTest  = admissibleCircleVerts[ localG, p0, radius,
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
    ], p, r ]


admissibleCircleVerts[ localG_Graph, center_, radius_, properties_List ] :=
  With[ { tests = propertyPredicateCircle[ localG, center, radius, # ] & /@ properties },
    verts |-> AllTrue[ tests, # @ verts & ]
  ]


(* Separating = deleting the cycle traps the center within { d <= rmax },
   i.e. disconnects c from { v : d(c, v) > rmax }.  The shortest such cycle
   hugs the inner edge rmin; no clean cut at the mean (that is why the
   circle here differs from SeparatingSetQ, which FindInfraShell still uses). *)
propertyPredicateCircle[ localG_Graph, center_, radius_, "Separating" ] :=
  verts |-> With[ { rem = VertexDelete[ localG, verts ] },
    { cc = SelectFirst[ ConnectedComponents[ rem ], MemberQ[ #, center ] & ] },
    cc =!= Missing[ "NotFound" ] &&
    AllTrue[ cc, GraphDistance[ localG, center, # ] <= radius & ] ]

propertyPredicateCircle[ _, _, _, other_ ] :=
  ( Message[ FindInfraCircle::badproperty, other ]; Throw[ $Failed ] )


(* ===================== FindInfraCycle ===================== *)

(* Simple cycles on graph (topological, not metric circles), returned as
   InfraCircle wrappers for direct use with NullHomotopicQ /
   FindInfraHomotopy.  Sorted by length ascending. *)

FindInfraCycle[ graph_Graph, n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  FindInfraCycle[ graph, { 1, VertexCount[ graph ] }, n ]

FindInfraCycle[ graph_Graph, { k_Integer },
    n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { cycles = cycleToVertexSequence /@ FindCycle[ graph, { k }, All ] },
    bundleTake[ InfraCircle, cycles, n ] ]

FindInfraCycle[ graph_Graph, { kMin_Integer, kMax_ },
    n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { cycles = SortBy[ Length ] @ Flatten[
        cycleToVertexSequence /@ FindCycle[ graph, { # }, All ] & /@
          Range[ kMin, Min[ kMax, VertexCount[ graph ] ] ], 1 ] },
    bundleTake[ InfraCircle, cycles, n ] ]


(* ===================== InfraCircleQ ===================== *)

(* InfraCircleQ[g, cycle]: the vertex sequence is a metric circle iff
   consecutive vertices (and the wrap-around) are adjacent and the
   underlying vertex set is a metric shell.  Accepts open ({v0, ..., vk},
   vk != v0) and closed ({v0, ..., vk, v0}) input. *)

InfraCircleQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    { verts = Most @ closed,
      pairs = Partition[ closed, 2, 1 ] },
    DuplicateFreeQ[ verts ] &&
    AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
    InfraShellQ[ graph, verts ]
  ]

InfraCircleQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraCircle[ center_, r_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      FindInfraCircle[ graph, center, r, All ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      True, <| "Center" -> center,
               "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]
