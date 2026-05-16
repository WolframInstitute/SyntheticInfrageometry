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


(* ===================== FindInfraCircle ===================== *)

(* A circle of radius r around c is a simple cycle in the level-surface
   subgraph at distance ~r from c.  Returns open vertex sequences
   { v0, v1, ..., vk } (the wrap-around edge is implicit).  Two
   orthogonal axes:
     Properties -- filters every realisation must satisfy.  Empty
       (default) means any simple cycle in the level surface.
       "Separating" requires the cycle's vertex set to disconnect c
       from { v : d(c, v) > r }.  "Connected" is not a meaningful
       property for cycles (always connected) and raises ::badproperty.
     Method     -- algorithmic strategy.  "Exhaustive" (default) does
       direct cycle enumeration via FindCycle + filter + length sort
       (so count = 1 returns the shortest admissible cycle).  The
       nested {"Exhaustive", "Pruning" -> spec} caps the FindCycle
       count or Bernoulli-subsamples the result list.  "Peel" runs
       findAllMinimalAdmissible on the level-set vertices with cycle-
       supporting admissibility, extracting one short cycle from each
       leaf.  "Greedy" returns the first admissible cycle found. *)

FindInfraCircle::badmethod   = "Method `1` is not supported by FindInfraCircle.";
FindInfraCircle::badproperty = "Property `1` is not supported by FindInfraCircle.";

Options[ FindInfraCircle ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraCircle[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraCircle, count,
    findCircleCore[ graph, ##, opts ] &, p, r ]


findCircleCore[ graph_Graph, p_, r_, opts : OptionsPattern[ FindInfraCircle ] ] :=
  Module[ { properties, methodSpec, methodHead, pruning, range, localG, levelSet, radius, levelGraph },
    properties = OptionValue[ FindInfraCircle, { opts }, Properties ];
    methodSpec = OptionValue[ FindInfraCircle, { opts }, Method ];
    methodHead = methodName @ methodSpec;
    pruning    = Replace[ methodSpec,
                  { { _String, subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ),
                    _ :> Infinity } ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    localG = If[ NumericQ[ range[[ 2 ]] ],
                 NeighborhoodGraph[ graph, p, Ceiling[ range[[ 2 ]] ] + 2 ], graph ];
    levelSet = Select[ VertexList[ localG ],
      range[[ 1 ]] <= GraphDistance[ localG, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    levelGraph = Subgraph[ localG, levelSet ];
    Catch[
      With[ { vertsTest = admissibleCircleVerts[ localG, p, radius, properties ] },
        Switch[ methodHead,
          "Exhaustive",
            SortBy[ Length ] @
              Select[ cycleToVertexSequence /@ findCyclesWithPruning[ levelGraph, pruning ],
                And[ Length[ # ] >= 3, vertsTest[ # ] ] & ],
          "Peel",
            DeleteDuplicatesBy[ Sort ] @
              SortBy[ Length ] @
              DeleteMissing[
                extractAdmissibleCycle[ levelGraph, vertsTest, # ] & /@
                  findAllMinimalAdmissible[ levelGraph, levelSet,
                    admissibleCircleSet[ levelGraph, vertsTest ], pruning ] ],
          "Greedy",
            greedyFirstAdmissibleCycle[ levelGraph, vertsTest ],
          _, Message[ FindInfraCircle::badmethod, methodSpec ]; $Failed
        ]
      ]
    ]
  ]


cycleToVertexSequence[ cyc_List ] := First /@ cyc

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
   FindInfraNullHomotopy.  Sorted by length ascending. *)

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
