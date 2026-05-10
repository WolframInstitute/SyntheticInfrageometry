Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findCircleCore]


(* ===================== InfraCircle wrapper ===================== *)

InfraCircle[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraCircle[ _List ] ] ] :=
  InfraCircle[ Flatten[ reps /. InfraCircle[ xs_List ] :> xs, 1 ] ]

InfraCircle /: Part[ InfraCircle[ reps_List ], i_Integer ] := InfraCircle[ { reps[[ i ]] } ]
InfraCircle /: Part[ InfraCircle[ reps_List ], spec_ ]     := InfraCircle[ reps[[ spec ]] ]

InfraCircle[ reps_List ][ "Realizations" ] := reps
InfraCircle[ reps_List ][ "Length" ]       := Length @ reps
InfraCircle[ reps_List ][ "Expand" ]       := InfraCircle[ { # } ] & /@ reps
InfraCircle[ reps_List ][ "First" ]        := First @ reps


(* ===================== FindCircle ===================== *)

(* A circle of radius r around c is a separating cycle in the level-surface
   subgraph: a cyclic vertex sequence whose removal disconnects c from
   { v : d(c, v) > r }.  Returns open vertex sequences { v0, v1, ..., vk }
   (the wrap-around edge is implicit). *)

FindCircle::badmethod = "Method `1` is not supported by FindCircle.";
FindCircle::nyi = "Pool `1` is not yet implemented for FindCircle; use \"LevelSet\".";

Options[ FindCircle ] = { Method -> "Metric" };

FindCircle[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraCircle, count,
    findCircleCore[ graph, ##, count, opts ] &, p, r ]


findCircleCore[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindCircle ] ] :=
  Module[ { range, levelSet, radius, levelGraph, allCycles, vertexCycles, separating, spec, methodName, ranked },
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    levelSet = Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    levelGraph = Subgraph[ graph, levelSet ];
    allCycles = FindCycle[ levelGraph, Infinity, All ];
    separating = If[ allCycles === {}, {},
      vertexCycles = (First /@ #) & /@ allCycles;
      FindSeparatingCycles[ graph, vertexCycles, p, radius ]
    ];
    spec = OptionValue[ FindCircle, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    ranked = Switch[ methodName,
      "Metric", separating,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec, "LevelSet" ] },
          If[ embOpts[ "Pool" ] =!= "LevelSet",
            Message[ FindCircle::nyi, embOpts[ "Pool" ] ]; separating,
            embeddingRankCircles[ graph, separating, p, radius, embOpts ]
          ]
        ],
      _, Message[ FindCircle::badmethod, spec ]; $Failed
    ];
    Which[
      ranked === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ ranked ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ ranked, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ ranked, count ],
      count === All,                       ranked,
      True,                                ranked
    ]
  ]


embeddingRankCircles[ graph_Graph, cycles_List, center_, radius_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, centerIdx },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    centerIdx = vertexIndex[ center ];
    SortBy[ cycles,
      cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ], centerIdx, radius ] ]
  ]


(* ===================== CircleQ ===================== *)

(* CircleQ[g, cycle]: the vertex sequence is a metric circle iff
   consecutive vertices (and the wrap-around) are adjacent and the
   underlying vertex set is a metric shell.  Accepts open ({v0, ..., vk},
   vk != v0) and closed ({v0, ..., vk, v0}) input. *)

CircleQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    With[ {
        verts = Most @ closed,
        pairs = Partition[ closed, 2, 1 ] },
      DuplicateFreeQ[ verts ] &&
      AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
      Length[ FindShellParameters[ graph, verts ] ] > 0
    ]
  ]

CircleQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False
