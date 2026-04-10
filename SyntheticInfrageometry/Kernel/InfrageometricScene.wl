(* ::Package:: *)

(* ===================== Helpers ===================== *)

toVertexSet[ v_ ] /; AtomQ[ v ] := { v }
toVertexSet[ vs_List ] := vs

topologicalLevels[ dag_Graph ] :=
  Module[ { remaining, levels, current },
    remaining = VertexList[ dag ];
    levels = {};
    While[ remaining =!= {},
      current = Select[ remaining,
        v |-> VertexInDegree[ Subgraph[ dag, remaining ], v ] == 0 ];
      AppendTo[ levels, current ];
      remaining = Complement[ remaining, current ]
    ];
    levels
  ]

resolveExpression[ expr_, bindings_Association, graph_Graph ] :=
  (expr /. Normal[ bindings ]) /.
    { InfraDistance[ x_, y_ ] :> GraphDistance[ graph, x, y ],
      InfraSegmentQ[ s_ ] :> SegmentQ[ graph, s ],
      InfraCircleQ[ c_, center_, r_ ] :> CircleQ[ graph, c, center, r ],
      InfraLineQ[ s_ ] :> LineQ[ graph, s ],
      InfraParallelQ[ l1_, l2_ ] :> ParallelQ[ graph, l1, l2 ],
      InfraIntersectQ[ s1_, s2_ ] :> IntersectQ[ s1, s2 ] }

extractBranches[ opts_List ] :=
  Lookup[ Association[ opts ], "Branches", All ]

(* ===================== Scene ===================== *)

InfrageometricScene::usage = "InfrageometricScene[objects, hypotheses] constructs a scene descriptor tracking construction steps, dependencies, and assertions.";
InfrageometricScene[ objects_List, hypotheses_List ] :=
  Module[ { constructions, assertions, deps, edges, dag, steps },
    constructions = Association @ Cases[ hypotheses,
      (key_ == rhs_) /; MemberQ[ objects, key ] ||
        (ListQ[ key ] && AllTrue[ key, MemberQ[ objects, # ] & ]) :> (key -> rhs) ];
    assertions = Select[ hypotheses,
      h |-> !MatchQ[ h, (key_ == _) /; MemberQ[ objects, key ] ||
        (ListQ[ key ] && AllTrue[ key, MemberQ[ objects, # ] & ]) ] ];
    deps = Map[
      rhs |-> Intersection[ Cases[ rhs, Alternatives @@ objects, Infinity ], objects ],
      constructions
    ];
    edges = Flatten @ KeyValueMap[
      { key, depList } |-> With[ { targets = If[ ListQ[ key ], key, { key } ] },
        DirectedEdge[ #1, #2 ] & @@@ Tuples[ { depList, targets } ]
      ],
      deps
    ];
    dag = Graph[ objects, edges, DirectedEdges -> True ];
    steps = topologicalLevels[ dag ];
    InfrageometricScene[ <|
      "Objects" -> objects,
      "Constructions" -> constructions,
      "Assertions" -> assertions,
      "DependencyGraph" -> dag,
      "Steps" -> steps
    |> ]
  ]

InfrageometricScene[ data_Association ][ prop_String ] := data[ prop ]

(* ===================== Construction Dispatch ===================== *)

dispatchConstruction[ graph_Graph, InfraPoint[] ] :=
  VertexList[ graph ]

dispatchConstruction[ graph_Graph, InfraPoint[ v_ ] ] /; MemberQ[ VertexList[ graph ], v ] :=
  { v }

dispatchConstruction[ graph_Graph, InfraPoint[ pool_String ] ] :=
  Switch[ pool,
    "Center", GraphCenter[ graph ],
    "Periphery", GraphPeriphery[ graph ],
    _, VertexList[ graph ]
  ]

dispatchConstruction[ graph_Graph, InfraPoint[ origin_, dist_ ] ] /; !MatchQ[ dist, _Rule ] :=
  Module[ { origins },
    origins = If[ StringQ[ origin ],
      Switch[ origin,
        "Center", GraphCenter[ graph ],
        "Periphery", GraphPeriphery[ graph ],
        _, VertexList[ graph ] ],
      If[ MemberQ[ VertexList[ graph ], origin ], { origin }, origin ]
    ];
    Union @ Flatten @ Table[
      Select[ VertexList[ graph ],
        v |-> GraphDistance[ graph, o, v ] == dist ],
      { o, origins }
    ]
  ]

dispatchConstruction[ graph_Graph, InfraPoint[ n_Integer, opts___Rule ] ] :=
  Module[ { dist, pool, distMatrix, finiteMax, dMin, dMax, auxEdges, auxGraph, cliques },
    dist = "Distance" /. {opts} /. "Distance" -> "Max";
    pool = VertexList[ graph ];
    distMatrix = GraphDistanceMatrix[ Subgraph[ graph, pool ] ];
    finiteMax = Max @ Select[ Flatten[ distMatrix ], # < Infinity & ];
    { dMin, dMax } = Switch[ dist,
      "Max", { finiteMax, finiteMax },
      _List, dist /. Infinity -> finiteMax,
      _, { dist, finiteMax }
    ];
    auxEdges = Select[
      Subsets[ pool, { 2 } ],
      pair |-> With[ { d = GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] },
        dMin <= d <= dMax ]
    ];
    auxGraph = Graph[ pool, UndirectedEdge @@@ auxEdges ];
    cliques = FindClique[ auxGraph, { n, VertexCount[ auxGraph ] }, All ];
    cliques = Select[ cliques, Length[ # ] >= n & ];
    If[ cliques === {}, {}, RandomSample[ #, n ] & /@ cliques ]
  ]

dispatchConstruction[ graph_Graph, InfraSegment[ p1_, p2_, opts___Rule ] ] :=
  FindSegment[ graph, p1, p2, extractBranches[ {opts} ],
    Sequence @@ DeleteDuplicatesBy[
      Join[ FilterRules[ {opts}, Options[ FindSegment ] ], { "Select" -> None } ],
      First ] ]

dispatchConstruction[ graph_Graph, InfraLine[ path_List, opts___Rule ] ] :=
  FindLine[ graph, path, extractBranches[ {opts} ],
    Sequence @@ DeleteDuplicatesBy[
      Join[ FilterRules[ {opts}, Options[ FindLine ] ], { "Select" -> None } ],
      First ] ]

dispatchConstruction[ graph_Graph, InfraLine[ p1_, p2_, opts___Rule ] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  FindLine[ graph, p1, p2, extractBranches[ {opts} ],
    Sequence @@ DeleteDuplicatesBy[
      Join[ FilterRules[ {opts}, Options[ FindLine ] ], { "Select" -> None } ],
      First ] ]

dispatchConstruction[ graph_Graph, InfraCircle[ center_, r_, opts___Rule ] ] :=
  FindCircle[ graph, center, r, extractBranches[ {opts} ],
    Sequence @@ DeleteDuplicatesBy[
      Join[ FilterRules[ {opts}, Options[ FindCircle ] ], { "Select" -> None } ],
      First ] ]

(* ===================== Evaluation Engine ===================== *)

evaluateConstruction[ graph_Graph, sym_, (InfraIntersectionPoint|InfraIntersection)[ obj1_, obj2_ ], bindings_Association ] :=
  With[ { common = Intersection[
      toVertexSet @ resolveExpression[ obj1, bindings, graph ],
      toVertexSet @ resolveExpression[ obj2, bindings, graph ] ] },
    Append[ bindings, sym -> # ] & /@ common
  ]

evaluateConstruction[ graph_Graph, sym_, rhs_, bindings_Association ] :=
  With[ { results = dispatchConstruction[ graph, resolveExpression[ rhs, bindings, graph ] ] },
    If[ results === {} || results === {{}},
      {},
      Append[ bindings, sym -> # ] & /@ results
    ]
  ]

evaluateConstruction[ graph_Graph, syms_List, rhs_, bindings_Association ] :=
  With[ { tuples = dispatchConstruction[ graph, resolveExpression[ rhs, bindings, graph ] ] },
    If[ tuples === {},
      {},
      Join[ bindings, AssociationThread[ syms, # ] ] & /@ tuples
    ]
  ]

evaluateStep[ graph_Graph, step_List, constructions_Association, branches_List ] :=
  Module[ { tupleKeys, tupleParts, singleSyms, processedBranches },
    tupleKeys = Select[ Keys[ constructions ], ListQ ];
    tupleParts = Flatten[ Select[ tupleKeys, ContainsAny[ step ] ] ];
    singleSyms = Select[ Complement[ step, tupleParts ], KeyExistsQ[ constructions, # ] & ];
    processedBranches = Fold[
      { currentBranches, sym } |->
        Flatten[
          evaluateConstruction[ graph, sym, constructions[ sym ], # ] & /@ currentBranches,
          1 ],
      branches,
      singleSyms
    ];
    Fold[
      { currentBranches, tupleKey } |->
        Flatten[
          evaluateConstruction[ graph, tupleKey, constructions[ tupleKey ], # ] & /@ currentBranches,
          1 ],
      processedBranches,
      Select[ tupleKeys, ContainsAny[ step ] ]
    ]
  ]

filterAssertions[ graph_Graph, assertions_List, branches_List ] :=
  If[ assertions === {},
    branches,
    Select[ branches,
      bindings |-> And @@ (TrueQ[ resolveExpression[ #, bindings, graph ] ] & /@ assertions)
    ]
  ]

(* ===================== FindSceneInstance ===================== *)

pruneBranches[ branches_List, 0 | 0. ] := branches
pruneBranches[ branches_List, p_ ] :=
  With[ { kept = Pick[ branches, UnitStep[ RandomReal[ {0, 1}, Length[ branches ] ] - p ], 1 ] },
    If[ kept === {}, { RandomChoice[ branches ] }, kept ]
  ]

Options[ FindSceneInstance ] = { "PruningProbability" -> 0 };

FindSceneInstance::usage = "FindSceneInstance[scene, graph] evaluates all construction steps and returns InfrageometricInstance objects. FindSceneInstance[scene, graph, n] evaluates up to n steps.";
FindSceneInstance[ scene_InfrageometricScene, graph_Graph, opts : OptionsPattern[] ] :=
  FindSceneInstance[ scene, graph, Length[ scene[ "Steps" ] ], opts ]

FindSceneInstance[ scene_InfrageometricScene, graph_Graph, nSteps_Integer, opts : OptionsPattern[] ] :=
  Module[ { steps, constructions, assertions, branches, prob },
    steps = Take[ scene[ "Steps" ], UpTo[ nSteps ] ];
    constructions = scene[ "Constructions" ];
    assertions = scene[ "Assertions" ];
    prob = OptionValue[ "PruningProbability" ];
    branches = { <||> };
    Do[
      branches = evaluateStep[ graph, step, constructions, branches ];
      branches = pruneBranches[ branches, prob ],
      { step, steps }
    ];
    branches = filterAssertions[ graph, assertions, branches ];
    InfrageometricInstance /@ branches
  ]

InfrageometricInstance::usage = "InfrageometricInstance[bindings] wraps a solved binding assignment from FindSceneInstance.";
