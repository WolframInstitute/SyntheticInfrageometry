Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[ellipticLevelSet]
PackageScope[ellipticNearFar]


(* ===================== InfraEllipticShell wrapper ===================== *)

(* InfraEllipticShell[{set}] is the unary form; InfraEllipticShell[{set1, ..., setk}] is the
   multi-realisation form.  Set canonicalisation and the shared accessors
   come from defineInfraBundleRules (Tools.wl). *)

(* "Volume" = vertex count per realisation. *)
InfraEllipticShell[ reps_List ][ "Volume" ] := Length /@ reps
(* ===================== FindInfraEllipticShell ===================== *)

(* Elliptic shell for foci {p1, p2} at sum c: level set
   { v : cMin <= d(p1,v) + d(p2,v) <= cMax }.  Two orthogonal axes:
     Properties -- empty (default) returns the level set itself; "Separating"
       requires disconnecting the near region {d_sum < cMin} from the far
       region {d_sum > cMax}; "Connected" requires ConnectedGraphQ.
     Method     -- Automatic (default) reads the count: All is the exhaustive
       BFS peel-DAG over the level set, a bounded count the lazy peel.  Also
       {"Exhaustive", "Pruning" -> spec} | "Greedy" (DFS, backtracking at each
       leaf, first `count` minimals) | "RandomGreedy" (random peels, seed via
       ambient SeedRandom).
   When Properties is empty, Method is ignored. *)

FindInfraEllipticShell::badmethod   = "Method `1` is not supported by FindInfraEllipticShell.";
FindInfraEllipticShell::badproperty = "Property `1` is not supported by FindInfraEllipticShell.";
FindInfraEllipticShell::shortfall   = "\"RandomGreedy\" drew `1` distinct shells of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";

Options[ FindInfraEllipticShell ] = {
  Properties -> { },
  Method     -> Automatic
};

FindInfraEllipticShell[ graph_Graph, foci : { _, _ }, c_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraEllipticShell, count,
    { foci0, c0 } |-> Module[ { properties, methodSpec, methodHead, pruning, range, verts, idx, dm, row1, row2,
              levelSet, admissible },
      properties = OptionValue[ FindInfraEllipticShell, { opts }, Properties ];
      methodSpec = resolveMethod[ OptionValue[ FindInfraEllipticShell, { opts }, Method ], count ];
      methodHead = methodName @ methodSpec;
      pruning    = Replace[ methodSpec,
                    { { "Exhaustive", subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ),
                      _ :> Infinity } ];
      range = Replace[ c0, d_?NumericQ :> { d, d } ];
      verts = VertexList[ graph ];
      idx   = AssociationThread[ verts, Range @ Length @ verts ];
      dm    = GraphDistanceMatrix[ graph ];
      row1  = dm[[ idx @ foci0[[ 1 ]] ]];
      row2  = dm[[ idx @ foci0[[ 2 ]] ]];
      levelSet = ellipticLevelSet[ verts, row1, row2, range ];
      If[ properties === { },
        { levelSet },
        Catch[
          admissible = admissibleEllipticShell[ graph, verts, row1, row2, range, properties ];
          Switch[ methodHead,
            "Exhaustive",   findAllMinimalAdmissible[ graph, levelSet, admissible, pruning ],
            "Greedy" | "ShuffledGreedy",
                            findGreedyMinimalAdmissible[ graph, levelSet, admissible, count,
                              greedyBranch @ methodHead ],
            "RandomGreedy", randomDraws[
              { } |-> findGreedyMinimalAdmissible[ graph, levelSet, admissible, 1, randomBranch ],
              count, FindInfraEllipticShell ],
            _,              Message[ FindInfraEllipticShell::badmethod, methodSpec ]; $Failed
          ]
        ]
      ]
    ],
    Replace[ foci, InfraPoint[ v_ ] :> v, { 1 } ], c ]


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
    { tests = propertyPredicateEllipticShell[ graph, nf[[ 1 ]], nf[[ 2 ]], # ] & /@ properties },
    t |-> AllTrue[ tests, # @ t & ]
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

InfraEllipticShellQ[ graph_Graph, s : _InfraEllipticShell | _InfraSet ] :=
  AllTrue[ If[ Head[ s ] === InfraSet, { First @ s }, First @ s ], InfraEllipticShellQ[ graph, # ] & ]

InfraEllipticShellQ[ graph_Graph, vs_List ] :=
  Module[ { verts, idx, dm },
    verts = VertexList[ graph ];
    idx   = AssociationThread[ verts, Range @ Length @ verts ];
    dm    = GraphDistanceMatrix[ graph ];
    AnyTrue[ Subsets[ verts, { 2 } ], fociPair |->
      With[ { sums = dm[[ idx @ fociPair[[ 1 ]] ]] + dm[[ idx @ fociPair[[ 2 ]] ]] },
        { c = sums[[ idx @ First @ vs ]] },
        Sort[ vs ] === Sort @ Pick[ verts, Thread[ sums == c ] ]
      ]
    ]
  ]
