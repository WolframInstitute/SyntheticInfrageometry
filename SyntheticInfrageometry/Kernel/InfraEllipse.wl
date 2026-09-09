Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraEllipse wrapper ===================== *)


InfraEllipse[ reps_List ][ "Length" ] := Length /@ reps
(* ===================== FindInfraEllipse ===================== *)

(* an ellipse for foci {p1, p2} is a simple cycle in the induced subgraph on { v : cMin <= d(p1, v) + d(p2, v) <= cMax }.  The family is carried by the FindCycle length sweep, which materialises every shorter cycle first; there is no elliptic pool, the circle's carrier having no two-focus analogue.  One class under every Method: branch orders the ties within a length grade, pruning caps the cycles kept per grade *)

FindInfraEllipse::badproperty = "Property `1` is not supported by FindInfraEllipse.";
FindInfraEllipse::badmethod   = "Method `1` is not supported by FindInfraEllipse.";

Options[ FindInfraEllipse ] = {
  Properties -> { "Separating", "Shortest" },
  Method     -> Automatic
};

FindInfraEllipse[ graph_Graph, foci : { _, _ }, c_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraEllipse, { opts }, Properties ],
      methodSpec = resolveMethod[ OptionValue[ FindInfraEllipse, { opts }, Method ], count ] },
    { methodHead = methodName @ methodSpec },
    If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
      Message[ FindInfraEllipse::badmethod, methodSpec ]; Throw[ $Failed ] ];
    With[ { branch  = greedyBranch[ methodHead /. "Exhaustive" -> "Greedy" ],
            pruning = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
      spreadFind[ InfraEllipse, count,
        findEllipseSweep[ graph, ##, properties, count, branch, pruning ] &,
        foci, c ] ]
  ]


findEllipseSweep[ graph_Graph, foci_List, c_, properties_List, count_, branch_, pruning_ ] :=
  Module[ { unknown, range, verts, idx, dm, row1, row2,
            levelGraph, vertsTest, tied, needed, k, kMax, batch, matching, accumulated },
    Catch[
      unknown = Complement[ properties, { "Separating", "Shortest" } ];
      If[ unknown =!= { },
        Message[ FindInfraEllipse::badproperty, First @ unknown ]; Throw[ $Failed ] ];
      range      = Replace[ c, d_?NumericQ :> { d, d } ];
      verts      = VertexList[ graph ];
      idx        = AssociationThread[ verts, Range @ Length @ verts ];
      dm         = GraphDistanceMatrix[ graph ];
      row1       = dm[[ idx @ foci[[ 1 ]] ]];
      row2       = dm[[ idx @ foci[[ 2 ]] ]];
      levelGraph = Subgraph[ graph, ellipticLevelSet[ verts, row1, row2, range ] ];
      vertsTest  = admissibleEllipticCycleVerts[ graph, verts, row1, row2, range,
                     DeleteCases[ properties, "Shortest" ] ];
      tied       = MemberQ[ properties, "Shortest" ];
      needed     = countLimit @ count;
      kMax       = VertexCount[ levelGraph ];
      accumulated = { };
      k = 3;
      While[ k <= kMax,
        batch    = branch @ applyPruning[ cycleToVertexSequence /@ FindCycle[ levelGraph, { k }, All ], pruning ];
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


admissibleEllipticCycleVerts[ graph_Graph, verts_List, row1_List, row2_List, range_List,
    properties_List ] :=
  With[ { nf = ellipticNearFar[ verts, row1, row2, range ] },
    { tests = propertyPredicateEllipticCycle[ graph, nf[[ 1 ]], nf[[ 2 ]], # ] & /@ properties },
    v |-> AllTrue[ tests, # @ v & ]
  ]


propertyPredicateEllipticCycle[ graph_Graph, nearVerts_List, farVerts_List, "Separating" ] :=
  verts |-> nearVerts =!= { } && farVerts =!= { } &&
            SeparatesQ[ graph, verts, First @ nearVerts, First @ farVerts ]

propertyPredicateEllipticCycle[ _, _, _, other_ ] :=
  ( Message[ FindInfraEllipse::badproperty, other ]; Throw[ $Failed ] )


(* ===================== InfraEllipseQ ===================== *)

(* cycle is an ellipse iff it is a cyclic path whose vertex set is an elliptic shell. *)

InfraEllipseQ[ graph_Graph, e_InfraEllipse ] :=
  AllTrue[ First @ e, InfraEllipseQ[ graph, # ] & ]

InfraEllipseQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    { verts = Most @ closed,
      pairs = Partition[ closed, 2, 1 ] },
    DuplicateFreeQ[ verts ] &&
    AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
    InfraEllipticShellQ[ graph, verts ]
  ]

InfraEllipseQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False
