Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findEllipseCore]


(* ===================== InfraEllipse wrapper ===================== *)

(* InfraEllipse[{cycle}] is the unary form; InfraEllipse[{cycle1, ..., cyclek}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraEllipse[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraEllipse[ _List ] ] ] :=
  InfraEllipse[ Flatten[ reps /. InfraEllipse[ xs_List ] :> xs, 1 ] ]

InfraEllipse[ reps_List ][ "Realizations" ] := reps
InfraEllipse[ reps_List ][ "Length" ]       := Length @ reps
InfraEllipse[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindInfraEllipse ===================== *)

(* An ellipse for foci {p1, p2} at sum c is a simple cycle in the induced
   subgraph on { v : cMin <= d(p1,v) + d(p2,v) <= cMax }.  Two orthogonal
   axes matching FindInfraCircle:
     Properties -- empty (default) means any simple cycle in the level
       surface; "Separating" requires the cycle to disconnect the near
       region from the far region.  "Connected" raises ::badproperty
       (cycles are always connected).
     Method     -- "Exhaustive" (default; FindCycle + filter + length sort,
       accepts nested {"Exhaustive", "Pruning" -> spec}) | "Peel" (BFS
       peel-DAG, cycle extraction from each leaf) | "Greedy" (first
       admissible cycle by ascending length, one realisation). *)

FindInfraEllipse::badmethod   = "Method `1` is not supported by FindInfraEllipse.";
FindInfraEllipse::badproperty = "Property `1` is not supported by FindInfraEllipse.";

Options[ FindInfraEllipse ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraEllipse[ graph_Graph, foci : { _, _ }, c_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraEllipse, count,
    findEllipseCore[ graph, ##, opts ] &, foci, c ]


findEllipseCore[ graph_Graph, { p1_, p2_ }, c_,
    opts : OptionsPattern[ FindInfraEllipse ] ] :=
  Module[ { properties, methodSpec, methodHead, pruning, range, verts, idx, dm, row1, row2,
            levelSet, levelGraph },
    properties = OptionValue[ FindInfraEllipse, { opts }, Properties ];
    methodSpec = OptionValue[ FindInfraEllipse, { opts }, Method ];
    methodHead = methodName @ methodSpec;
    pruning    = Replace[ methodSpec,
                  { { _String, subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ),
                    _ :> Infinity } ];
    range      = Replace[ c, d_?NumericQ :> { d, d } ];
    verts      = VertexList[ graph ];
    idx        = AssociationThread[ verts, Range @ Length @ verts ];
    dm         = GraphDistanceMatrix[ graph ];
    row1       = dm[[ idx @ p1 ]];
    row2       = dm[[ idx @ p2 ]];
    levelSet   = ellipticLevelSet[ verts, row1, row2, range ];
    levelGraph = Subgraph[ graph, levelSet ];
    Catch[
      With[ { vertsTest = admissibleEllipticCycleVerts[ graph, verts, row1, row2, range, properties ] },
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
          _, Message[ FindInfraEllipse::badmethod, methodSpec ]; $Failed
        ]
      ]
    ]
  ]


admissibleEllipticCycleVerts[ graph_Graph, verts_List, row1_List, row2_List, range_List,
    properties_List ] :=
  With[ { nf = ellipticNearFar[ verts, row1, row2, range ] },
    With[ { tests = propertyPredicateEllipticCycle[ graph, nf[[ 1 ]], nf[[ 2 ]], # ] & /@ properties },
      v |-> AllTrue[ tests, # @ v & ]
    ]
  ]


propertyPredicateEllipticCycle[ graph_Graph, nearVerts_List, farVerts_List, "Separating" ] :=
  verts |-> nearVerts =!= { } && farVerts =!= { } &&
            SeparatesQ[ graph, verts, First @ nearVerts, First @ farVerts ]

propertyPredicateEllipticCycle[ _, _, _, other_ ] :=
  ( Message[ FindInfraEllipse::badproperty, other ]; Throw[ $Failed ] )


(* ===================== InfraEllipseQ ===================== *)

(* cycle is an ellipse iff it is a cyclic path whose vertex set is an elliptic shell. *)

InfraEllipseQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    With[ {
        verts = Most @ closed,
        pairs = Partition[ closed, 2, 1 ] },
      DuplicateFreeQ[ verts ] &&
      AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
      InfraEllipticShellQ[ graph, verts ]
    ]
  ]

InfraEllipseQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False
