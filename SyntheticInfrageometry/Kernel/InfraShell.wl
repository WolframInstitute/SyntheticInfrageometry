Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findShellCore]


(* ===================== InfraShell wrapper ===================== *)

InfraShell[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraShell[ _List ] ] ] :=
  InfraShell[ Flatten[ reps /. InfraShell[ xs_List ] :> xs, 1 ] ]

InfraShell /: Part[ InfraShell[ reps_List ], i_Integer ] := InfraShell[ { reps[[ i ]] } ]
InfraShell /: Part[ InfraShell[ reps_List ], spec_ ]     := InfraShell[ reps[[ spec ]] ]

InfraShell[ reps_List ][ "Realizations" ] := reps
InfraShell[ reps_List ][ "Length" ]       := Length @ reps
InfraShell[ reps_List ][ "Expand" ]       := InfraShell[ { # } ] & /@ reps
InfraShell[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindShell ===================== *)

(* A shell of radius r around c is a vertex set carved out of the level
   surface { v : d(c, v) = r }.  Three recipes: "Metric" returns the
   level surface itself; "Separating" returns connected separating
   subsets minimal under inclusion; "Embedding" ranks vertices by
   proximity to the Euclidean sphere of radius r. *)

FindShell::badmethod = "Method `1` is not supported by FindShell.";

Options[ FindShell ] = { Method -> "Metric" };

FindShell[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraShell, count,
    findShellCore[ graph, ##, count, opts ] &, p, r ]


findShellCore[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindShell ] ] :=
  Module[ { spec, methodName, range, levelSet, radius, shells },
    spec = OptionValue[ FindShell, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    shells = Switch[ methodName,
      "Metric",     { levelSet },
      "Separating", FindMinimalSeparatingSubgraphs[ graph, levelSet, p, radius ],
      "Embedding",  embeddingRankShellVertices[ graph, p, radius, levelSet, parseEmbeddingMethod[ spec, "LevelSet" ] ],
      _,            Message[ FindShell::badmethod, spec ]; $Failed
    ];
    Which[
      shells === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ shells ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ shells, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ shells, count ],
      count === All,                       shells,
      True,                                shells
    ]
  ]


embeddingRankShellVertices[ graph_Graph, p_, radius_, levelSet_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, centerPt, pool },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerPt = coords[[ vertexIndex[ p ] ]];
    pool = If[ embOpts[ "Pool" ] === "AllVertices", VertexList[ graph ], levelSet ];
    List /@ SortBy[ pool,
      v |-> Abs[ EuclideanDistance[ coords[[ vertexIndex[ v ] ]], centerPt ] - radius ] ]
  ]


(* ===================== FindShellParameters ===================== *)

(* For a vertex set vs, return the {center, radius} pairs for which vs
   is a metric shell: in some component of g \ vs, a center c is
   equidistant from every vertex of vs, dominates that component, and
   is strictly closer to the inside than to the outside. *)

FindShellParameters[ graph_Graph, vs_List ] :=
  Module[ { rem, comps },
    rem = VertexDelete[ graph, vs ];
    comps = ConnectedComponents[ rem ];
    Flatten[ Table[
      With[ {
          distMatrix = GraphDistanceMatrix[ Subgraph[ graph, comp ] ],
          otherVertices = Complement[ VertexList[ rem ], comp ] },
        With[ { scores = Max /@ distMatrix },
          With[ { centers = Pick[ comp, scores, Min[ scores ] ] },
            Select[
              { #, GraphDistance[ graph, #, First[ vs ] ] } & /@ centers,
              pair |-> With[ { v = pair[[ 1 ]], r = pair[[ 2 ]] },
                AllTrue[ vs, GraphDistance[ graph, v, # ] == r & ] &&
                AllTrue[ comp, GraphDistance[ graph, v, # ] <= r & ] &&
                AllTrue[ otherVertices, GraphDistance[ graph, v, # ] > r & ]
              ]
            ]
          ]
        ]
      ],
      { comp, comps }
    ], 1 ]
  ]


(* ===================== ShellQ ===================== *)

ShellQ[ graph_Graph, vs_List ] :=
  Length[ FindShellParameters[ graph, vs ] ] > 0


(* ===================== SeparatesQ ===================== *)

(* SeparatesQ tests whether deleting vs disconnects u from v.  Endpoint
   deletion does not count as separation. *)

SeparatesQ[ graph_Graph, vs_List, u_, v_ ] :=
  If[ MemberQ[ vs, u ] || MemberQ[ vs, v ], False,
    GraphDistance[ VertexDelete[ graph, vs ], u, v ] === Infinity
  ]
