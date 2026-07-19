Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraEllipse wrapper ===================== *)

(* InfraEllipse[{cycle}] is the unary form; InfraEllipse[{cycle1, ..., cyclek}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraEllipse[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraEllipse[ _List ] ] ] :=
  InfraEllipse[ Flatten[ reps /. InfraEllipse[ xs_List ] :> xs, 1 ] ]

InfraEllipse[ reps_List ][ "Realizations" ] := reps
(* "Length" = circumference per realisation (#edges == #vertices for an open cycle). *)
InfraEllipse[ reps_List ][ "Length" ]       := Length /@ reps
InfraEllipse[ reps_List ][ "First" ]        := First @ reps
(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraEllipse[ reps_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraEllipse[ reps ] ]
InfraEllipse[ reps_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraEllipse[ reps ] ]
InfraEllipse[ reps_List ][ "Measure" ] := InfraMeasure[ InfraEllipse[ reps ] ]
InfraEllipse[ reps_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraEllipse[ reps ], Method -> "Probability" ]
(* ===================== FindInfraEllipse ===================== *)

(* An ellipse for foci {p1, p2} at sum c is a simple cycle in the induced
   subgraph on { v : cMin <= d(p1,v) + d(p2,v) <= cMax }.  Single axis
   Properties, matching FindInfraCircle:
     "Separating" -- the cycle disconnects the near region
       {v : sum < cMin} from the far region {v : sum > cMax}; the
       topological condition that makes a level cycle a genuine ellipse.
     "Shortest"   -- only cycles tied at the minimum admissible length;
       the length sweep stops at the first non-empty length class.
   Default {"Separating", "Shortest"} returns the canonical infra-ellipse
   (shortest separating cycle) and its ties.  Unknown property names
   (including "Connected", since cycles are always connected) raise
   ::badproperty.  Single length-by-length FindCycle sweep; no Method
   axis. *)

FindInfraEllipse::badproperty = "Property `1` is not supported by FindInfraEllipse.";

Options[ FindInfraEllipse ] = {
  Properties -> { "Separating", "Shortest" }
};

FindInfraEllipse[ graph_Graph, foci : { _, _ }, c_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  spreadFind[ InfraEllipse, count,
    { foci0, c0 } |-> Module[ { properties, unknown, range, verts, idx, dm, row1, row2,
              levelGraph, vertsTest, tied, needed, k, kMax, batch, matching, accumulated },
      properties = OptionValue[ FindInfraEllipse, { opts }, Properties ];
      Catch[
        unknown = Complement[ properties, { "Separating", "Shortest" } ];
        If[ unknown =!= { },
          Message[ FindInfraEllipse::badproperty, First @ unknown ]; Throw[ $Failed ] ];
        range      = Replace[ c0, d_?NumericQ :> { d, d } ];
        verts      = VertexList[ graph ];
        idx        = AssociationThread[ verts, Range @ Length @ verts ];
        dm         = GraphDistanceMatrix[ graph ];
        row1       = dm[[ idx @ foci0[[ 1 ]] ]];
        row2       = dm[[ idx @ foci0[[ 2 ]] ]];
        levelGraph = Subgraph[ graph, ellipticLevelSet[ verts, row1, row2, range ] ];
        vertsTest  = admissibleEllipticCycleVerts[ graph, verts, row1, row2, range,
                       DeleteCases[ properties, "Shortest" ] ];
        tied       = MemberQ[ properties, "Shortest" ];
        needed     = Switch[ count, _Integer, count, UpTo[ _Integer ], First @ count, _, Infinity ];
        kMax       = VertexCount[ levelGraph ];
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
    ],
    Replace[ foci, InfraPoint[ { v_ } ] :> v, { 1 } ], c ]


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
