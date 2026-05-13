Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findShellCore]


(* ===================== InfraShell wrapper ===================== *)

(* InfraShell[{set}] is the unary form; InfraShell[{set1, ..., setk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraShell[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraShell[ _List ] ] ] :=
  InfraShell[ Flatten[ reps /. InfraShell[ xs_List ] :> xs, 1 ] ]


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
    findShellCore[ graph, ##, opts ] &, p, r ]


findShellCore[ graph_Graph, p_, r_, opts : OptionsPattern[ FindShell ] ] :=
  Module[ { spec, methodName, range, localG, levelSet, radius },
    spec = OptionValue[ FindShell, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    (* Localize: level surface + one outside layer for the SeparatingSetQ test. *)
    localG = If[ NumericQ[ range[[ 2 ]] ],
                 NeighborhoodGraph[ graph, p, Ceiling[ range[[ 2 ]] ] + 1 ], graph ];
    levelSet = Select[ VertexList[ localG ],
      range[[ 1 ]] <= GraphDistance[ localG, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    Switch[ methodName,
      "Metric",     { levelSet },
      "Separating", FindMinimalSeparatingSubgraphs[ localG, levelSet, p, radius ],
      (* Embedding uses global GraphEmbedding -> keep the original graph. *)
      "Embedding",  embeddingRankShellVertices[ graph, p, radius, levelSet, parseEmbeddingMethod[ spec, "LevelSet" ] ],
      _,            Message[ FindShell::badmethod, spec ]; $Failed
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
