Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findEllipticShellCore]
PackageScope[ellipticLevelSet]
PackageScope[ellipticNearFar]


(* ===================== InfraEllipticShell wrapper ===================== *)

(* InfraEllipticShell[{set}] is the unary form; InfraEllipticShell[{set1, ..., setk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraEllipticShell[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraEllipticShell[ _List ] ] ] :=
  InfraEllipticShell[ Flatten[ reps /. InfraEllipticShell[ xs_List ] :> xs, 1 ] ]

InfraEllipticShell[ reps_List ][ "Realizations" ] := reps
InfraEllipticShell[ reps_List ][ "Length" ]       := Length @ reps
InfraEllipticShell[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindInfraEllipticShell ===================== *)

(* Elliptic shell for foci {p1, p2} at sum c: level set
   { v : cMin <= d(p1,v) + d(p2,v) <= cMax }.  Two orthogonal axes:
     Properties -- empty (default) returns the level set itself; "Separating"
       requires disconnecting the near region {d_sum < cMin} from the far
       region {d_sum > cMax}; "Connected" requires ConnectedGraphQ.
     Method     -- "Exhaustive" (default; BFS peel-DAG over the level set) |
       {"Exhaustive", "Pruning" -> spec} | "Greedy" (DFS, one realisation).
   When Properties is empty, Method is ignored. *)

FindInfraEllipticShell::badmethod   = "Method `1` is not supported by FindInfraEllipticShell.";
FindInfraEllipticShell::badproperty = "Property `1` is not supported by FindInfraEllipticShell.";

Options[ FindInfraEllipticShell ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraEllipticShell[ graph_Graph, foci : { _, _ }, c_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraEllipticShell, count,
    findEllipticShellCore[ graph, ##, opts ] &, foci, c ]


findEllipticShellCore[ graph_Graph, { p1_, p2_ }, c_,
    opts : OptionsPattern[ FindInfraEllipticShell ] ] :=
  Module[ { properties, methodSpec, methodHead, pruning, range, verts, idx, dm, row1, row2,
            levelSet, admissible },
    properties = OptionValue[ FindInfraEllipticShell, { opts }, Properties ];
    methodSpec = OptionValue[ FindInfraEllipticShell, { opts }, Method ];
    methodHead = methodName @ methodSpec;
    pruning    = Replace[ methodSpec,
                  { { "Exhaustive", subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ),
                    _ :> Infinity } ];
    range = Replace[ c, d_?NumericQ :> { d, d } ];
    verts = VertexList[ graph ];
    idx   = AssociationThread[ verts, Range @ Length @ verts ];
    dm    = GraphDistanceMatrix[ graph ];
    row1  = dm[[ idx @ p1 ]];
    row2  = dm[[ idx @ p2 ]];
    levelSet = ellipticLevelSet[ verts, row1, row2, range ];
    If[ properties === { },
      { levelSet },
      Catch[
        admissible = admissibleEllipticShell[ graph, verts, row1, row2, range, properties ];
        Switch[ methodHead,
          "Exhaustive", findAllMinimalAdmissible[ graph, levelSet, admissible, pruning ],
          "Greedy",     findGreedyMinimalAdmissible[ graph, levelSet, admissible ],
          _,            Message[ FindInfraEllipticShell::badmethod, methodSpec ]; $Failed
        ]
      ]
    ]
  ]


(* { v : cMin <= d(p1,v) + d(p2,v) <= cMax } as a vertex list *)

ellipticLevelSet[ verts_List, row1_List, row2_List, range_List ] :=
  Pick[ verts, Thread[ range[[ 1 ]] <= row1 + row2 <= range[[ 2 ]] ] ]


(* near region {d_sum < cMin} and far region {d_sum > cMax} *)

ellipticNearFar[ verts_List, row1_List, row2_List, range_List ] :=
  With[ { sums = row1 + row2 },
    { Pick[ verts, Thread[ sums < range[[ 1 ]] ] ],
      Pick[ verts, Thread[ sums > range[[ 2 ]] ] ] }
  ]


admissibleEllipticShell[ graph_Graph, verts_List, row1_List, row2_List, range_List,
    properties_List ] :=
  With[ { nf = ellipticNearFar[ verts, row1, row2, range ] },
    With[ { tests = propertyPredicateEllipticShell[ graph, nf[[ 1 ]], nf[[ 2 ]], # ] & /@ properties },
      t |-> AllTrue[ tests, # @ t & ]
    ]
  ]


propertyPredicateEllipticShell[ graph_Graph, nearVerts_List, farVerts_List, "Separating" ] :=
  t |-> nearVerts =!= { } && farVerts =!= { } &&
        SeparatesQ[ graph, t, First @ nearVerts, First @ farVerts ]

propertyPredicateEllipticShell[ graph_Graph, _, _, "Connected" ] :=
  t |-> t =!= { } && ConnectedGraphQ @ Subgraph[ graph, t ]

propertyPredicateEllipticShell[ _, _, _, other_ ] :=
  ( Message[ FindInfraEllipticShell::badproperty, other ]; Throw[ $Failed ] )


(* ===================== InfraEllipticShellQ ===================== *)

(* vs is an elliptic shell iff there exist foci p1, p2 and constant c with
   vs == { v : d(p1,v) + d(p2,v) == c }. *)

InfraEllipticShellQ[ graph_Graph, vs_List ] :=
  Module[ { verts, idx, dm },
    verts = VertexList[ graph ];
    idx   = AssociationThread[ verts, Range @ Length @ verts ];
    dm    = GraphDistanceMatrix[ graph ];
    AnyTrue[ Subsets[ verts, { 2 } ], fociPair |->
      With[ { sums = dm[[ idx @ fociPair[[ 1 ]] ]] + dm[[ idx @ fociPair[[ 2 ]] ]] },
        With[ { c = sums[[ idx @ First @ vs ]] },
          Sort[ vs ] === Sort @ Pick[ verts, Thread[ sums == c ] ]
        ]
      ]
    ]
  ]
