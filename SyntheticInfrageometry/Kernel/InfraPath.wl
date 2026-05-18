Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findInfraPathCore]
PackageScope[extendInfraPathCore]
PackageScope[concatenatePathPair]


(* ===================== InfraPath wrapper ===================== *)

InfraPath[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPath[ _List ] ] ] :=
  InfraPath[ Flatten[ reps /. InfraPath[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraPath ===================== *)

(* A path from p1 to p2 is a simple vertex sequence (p1, ..., p2) with
   consecutive adjacency.  Wraps the Wolfram built-in FindPath with the
   project calling triple and the multi-anchor spread.  InfraPathQ \supset
   InfraSegmentQ \supset InfraLineQ. *)

Options[ FindInfraPath ] = { };

FindInfraPath[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPath, count,
    findInfraPathCore[ graph, ##, kspec, count ] &, p1, p2 ]


findInfraPathCore[ _Graph, p1_, p1_, _, _ ] := { }

(* kspec = Infinity, count = 1: FindShortestPath (Dijkstra) is faster than DFS *)
findInfraPathCore[ graph_Graph, p1_, p2_, Infinity, 1 | UpTo[ 1 ] ] :=
  Replace[ FindShortestPath[ graph, p1, p2 ],
    { {} -> {}, path_List :> { path }, _ -> {} } ]

findInfraPathCore[ graph_Graph, p1_, p2_, kspec_, count_ ] :=
  Replace[
    FindPath[ graph, p1, p2, kspec, count /. UpTo[ n_ ] :> n ],
    Except[ _List ] -> { } ]


(* ===================== ExtendInfraPath ===================== *)

(* ExtendInfraPath[g, path, n] extends a walk by per-step rule for n steps
   per requested side (or until inextensible).  "CurvatureMinimizing" picks
   each next vertex MinimalBy edge curvature; "ShortestPath" keeps the
   last K+1 walked vertices on a geodesic; "LongestPath" picks neighbours
   MaximalBy distance to the last K vertices (Lex tuple or Sum). *)

Options[ ExtendInfraPath ] = {
  Method    -> "CurvatureMinimizing",
  "Length"  -> Automatic,
  "Side"    -> "Both",
  "Pruning" -> Infinity
};

ExtendInfraPath[ graph_Graph, path_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPath, count,
    extendInfraPathCore[ graph, ##, opts ] &, path ]


extendInfraPathCore[ _Graph, walk_List, OptionsPattern[ ExtendInfraPath ] ] /;
    Length[ walk ] < 1 := { walk }

extendInfraPathCore[ graph_Graph, walk_List, opts : OptionsPattern[ ExtendInfraPath ] ] :=
  With[ { side    = OptionValue[ ExtendInfraPath, { opts }, "Side" ],
          length  = OptionValue[ ExtendInfraPath, { opts }, "Length" ],
          pruning = OptionValue[ ExtendInfraPath, { opts }, "Pruning" ],
          spec    = OptionValue[ ExtendInfraPath, { opts }, Method ] },
    With[ { extender = pickPathExtender[ graph, spec ] },
      With[ { forward  = If[ side === "Backward", { walk },
                            extendOneSide[ walk, extender, length, pruning ] ],
              backward = If[ side === "Forward", { walk },
                            Reverse /@ extendOneSide[ Reverse @ walk, extender, length, pruning ] ] },
        Switch[ side,
          "Forward",  forward,
          "Backward", backward,
          "Both",
            DeleteDuplicates @ Select[
              Join @@@ Tuples[ { Drop[ #, -Length[ walk ] ] & /@ backward, forward } ],
              DuplicateFreeQ ]
        ]
      ]
    ]
  ]


(* One-side frontier BFS over walk space.  Walks with no admissible
   next vertex freeze; walks at step budget exit alive. *)

extendOneSide[ seed_List, extender_, length_, pruning_ ] :=
  Module[ { live = { seed }, dead = { }, steps = 0,
            maxSteps = length /. Automatic -> Infinity },
    While[ live =!= { } && steps < maxSteps,
      With[ { pairs = ( p |-> { p, extender[ p ] } ) /@ live },
        dead = Join[ dead, Cases[ pairs, { p_, { } } :> p ] ];
        live = applyPruning[
          Flatten[ Cases[ pairs,
            { p_, nexts : { __ } } :> ( Append[ p, # ] & /@ nexts ) ], 1 ],
          pruning ]
      ];
      steps++
    ];
    DeleteDuplicates @ Join[ dead, live ]
  ]


(* Per-method extender: closure path |-> list of admissible next vertices. *)

pickPathExtender[ graph_Graph, spec_ ] :=
  With[ { sub = Replace[ spec, { { _String, rest___ } :> { rest }, _ :> { } } ] },
    Switch[ methodName @ spec,
      "CurvatureMinimizing", curvaturePathExtender[ graph, sub ],
      "ShortestPath",  shortestWindowPathExtender[ graph, sub ],
      "LongestPath",   pushWindowPathExtender[ graph, sub ]
    ]
  ]


curvaturePathExtender[ graph_Graph, sub_List ] :=
  With[ { edgeKappa = buildEdgeKappa[ graph, parseCurvatureSpec[
              "Curvature" /. sub /. "Curvature" -> "FormanRicciCurvature" ] ] },
    path |-> With[ { v = Last @ path,
        cands = Select[ AdjacencyList[ graph, Last @ path ], ! MemberQ[ path, # ] & ] },
      If[ cands === { }, { },
        MinimalBy[ cands, w |-> edgeKappa[ v, w ] ] ]
    ]
  ]


shortestWindowPathExtender[ graph_Graph, sub_List ] :=
  With[ { window = "Window" /. sub /. "Window" -> 2 },
    path |-> With[ { cands = Select[ AdjacencyList[ graph, Last @ path ], ! MemberQ[ path, # ] & ],
        windowLen = If[ window === All || window === Infinity,
            Length @ path, Min[ window, Length @ path ] ] },
      Select[ cands, w |-> GraphDistance[ graph, path[[ -windowLen ]], w ] == windowLen ]
    ]
  ]


pushWindowPathExtender[ graph_Graph, sub_List ] :=
  With[ { window = "Window"      /. sub /. "Window"      -> All,
          agg    = "Aggregation" /. sub /. "Aggregation" -> "Lex",
          vidx   = AssociationThread[ VertexList @ graph, Range @ VertexCount @ graph ],
          dmat   = GraphDistanceMatrix @ graph },
    path |-> With[ { cands = Select[ AdjacencyList[ graph, Last @ path ], ! MemberQ[ path, # ] & ],
        hIdx = With[ { rev = vidx /@ Reverse @ Most @ path },
          If[ window === All || window === Infinity, rev, Take[ rev, UpTo[ window - 1 ] ] ] ] },
      Which[
        cands === { },     { },
        hIdx  === { },     cands,
        agg === "Lex",     MaximalBy[ cands, w |-> dmat[[ hIdx, vidx[ w ] ]] ],
        agg === "Sum",     MaximalBy[ cands, w |-> Total @ dmat[[ hIdx, vidx[ w ] ]] ]
      ]
    ]
  ]


(* ===================== ConcatenateInfraPath ===================== *)

(* path concatenation: all pairs (walk1, walk2) with Last[walk1] === First[walk2] *)

ConcatenateInfraPath[ path1_, path2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  infraSpreadAndCartesian[ InfraPath, count, concatenatePathPair, path1, path2 ]

concatenatePathPair[ walk1_List, walk2_List ] :=
  If[ Last[ walk1 ] === First[ walk2 ], { Join[ walk1, Rest @ walk2 ] }, { } ]
