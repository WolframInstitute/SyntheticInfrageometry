Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[toVertexSet]
PackageScope[topologicalLevels]
PackageScope[resolveExpression]
PackageScope[extractBranches]
PackageScope[dispatchConstruction]
PackageScope[evaluateConstruction]
PackageScope[evaluateStep]
PackageScope[filterAssertions]
PackageScope[pruneBranches]
PackageScope[parseGeometricSteps]

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
      InfraSphereQ[ c_ ] :> SphereQ[ graph, c ],
      InfraLineQ[ s_ ] :> LineQ[ graph, s ],
      InfraParallelQ[ l1_, l2_ ] :> ParallelQ[ graph, l1, l2 ],
      InfraIntersectQ[ s1_, s2_ ] :> IntersectingQ[ s1, s2 ] }

extractBranches[ opts_List ] :=
  Lookup[ Association[ opts ], "Branches", All ]

(* ===================== Scene ===================== *)

isConstruction[ objects_List, h_ ] :=
  MatchQ[ h, (key_ == _) /; MemberQ[ objects, key ] ||
    (ListQ[ key ] && AllTrue[ key, MemberQ[ objects, # ] & ]) ]

extractConstructions[ objects_List, hyps_List ] :=
  Association @ Cases[ hyps, (key_ == rhs_) /;
    MemberQ[ objects, key ] || (ListQ[ key ] && AllTrue[ key, MemberQ[ objects, # ] & ]) :> (key -> rhs) ]

extractAssertions[ objects_List, hyps_List ] :=
  Select[ hyps, h |-> !isConstruction[ objects, h ] ]

parseGeometricSteps[ objects_List, hypotheses_List ] :=
  Module[ { gSteps, globalAssertions, constructions, steps, labels },
    gSteps = Cases[ hypotheses, _InfraGeometricStep ];
    globalAssertions = Select[ hypotheses,
      h |-> !MatchQ[ h, _InfraGeometricStep ] && !isConstruction[ objects, h ] ];
    constructions = <||>;
    steps = {};
    labels = {};
    Do[
      Module[ { hyps, label, stepConstructions, stepObjects },
        hyps = gStep[[ 1 ]];
        label = If[ Length[ gStep ] >= 2, gStep[[ 2 ]], None ];
        stepConstructions = extractConstructions[ objects, hyps ];
        constructions = Join[ constructions, stepConstructions ];
        stepObjects = Flatten[ If[ ListQ[ # ], #, { # } ] & /@ Keys[ stepConstructions ] ];
        AppendTo[ steps, stepObjects ];
        AppendTo[ labels, label ];
        globalAssertions = Join[ globalAssertions, extractAssertions[ objects, hyps ] ]
      ],
      { gStep, gSteps }
    ];
    <| "Constructions" -> constructions, "Steps" -> steps,
       "Assertions" -> globalAssertions, "Labels" -> labels |>
  ]

InfraScene[ objects_List, hypotheses_List ] /;
  MemberQ[ hypotheses, _InfraGeometricStep ] :=
  Module[ { parsed },
    parsed = parseGeometricSteps[ objects, hypotheses ];
    InfraScene[ <|
      "Objects" -> objects,
      "Constructions" -> parsed[ "Constructions" ],
      "Assertions" -> parsed[ "Assertions" ],
      "DependencyGraph" -> None,
      "Steps" -> parsed[ "Steps" ],
      "Labels" -> parsed[ "Labels" ],
      "ManualSteps" -> True
    |> ]
  ]

InfraScene[ objects_List, hypotheses_List ] :=
  Module[ { constructions, assertions, deps, edges, dag, steps },
    constructions = extractConstructions[ objects, hypotheses ];
    assertions = extractAssertions[ objects, hypotheses ];
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
    InfraScene[ <|
      "Objects" -> objects,
      "Constructions" -> constructions,
      "Assertions" -> assertions,
      "DependencyGraph" -> dag,
      "Steps" -> steps,
      "Labels" -> ConstantArray[ None, Length[ steps ] ],
      "ManualSteps" -> False
    |> ]
  ]

InfraScene[ data_Association ][ prop_String ] := data[ prop ]

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
  Module[ { extensions, context, branches },
    extensions = findLineExtensions[ graph, path ];
    context = <| "Cyclic" -> False, "Endpoints" -> { First[ path ], Last[ path ] } |>;
    extensions = applySelect[ graph, extensions,
      "Select" /. {opts} /. "Select" -> None, context ];
    branches = extractBranches[ {opts} ];
    If[ branches === All, extensions, Take[ extensions, UpTo[ branches ] ] ]
  ]

dispatchConstruction[ graph_Graph, InfraLine[ p1_, p2_, opts___Rule ] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  FindLine[ graph, p1, p2, extractBranches[ {opts} ],
    Sequence @@ DeleteDuplicatesBy[
      Join[ FilterRules[ {opts}, Options[ FindLine ] ], { "Select" -> None } ],
      First ] ]

dispatchConstruction[ graph_Graph, InfraSphere[ center_, r_, opts___Rule ] ] :=
  FindSphere[ graph, center, r, extractBranches[ {opts} ],
    Sequence @@ DeleteDuplicatesBy[
      Join[ FilterRules[ {opts}, Options[ FindSphere ] ], { "Select" -> None } ],
      First ] ]

(* ===================== Evaluation Engine ===================== *)

evaluateConstruction[ graph_Graph, sym_, InfraIntersection[ obj1_, obj2_ ], bindings_Association ] :=
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

(* ===================== FindInfraScene ===================== *)

pruneBranches[ branches_List, 0 | 0. ] := branches
pruneBranches[ branches_List, p_ ] :=
  With[ { kept = Pick[ branches, UnitStep[ RandomReal[ {0, 1}, Length[ branches ] ] - p ], 1 ] },
    If[ kept === {}, { RandomChoice[ branches ] }, kept ]
  ]

skipBoundKeys[ step_List, bindings_Association ] :=
  Select[ step, s |-> !KeyExistsQ[ bindings, s ] ]

Options[ FindInfraScene ] = { "PruningProbability" -> 0 };

FindInfraScene[ scene_InfraScene, graph_Graph, opts : OptionsPattern[] ] :=
  FindInfraScene[ scene, graph, Length[ scene[ "Steps" ] ], <||>, opts ]

FindInfraScene[ scene_InfraScene, graph_Graph, nSteps_Integer, opts : OptionsPattern[] ] :=
  FindInfraScene[ scene, graph, nSteps, <||>, opts ]

FindInfraScene[ scene_InfraScene, graph_Graph, init_Association, opts : OptionsPattern[] ] :=
  FindInfraScene[ scene, graph, Length[ scene[ "Steps" ] ], init, opts ]

FindInfraScene[ scene_InfraScene, graph_Graph, nSteps_Integer, init_Association, opts : OptionsPattern[] ] :=
  Module[ { steps, constructions, assertions, branches, prob },
    steps = Take[ scene[ "Steps" ], UpTo[ nSteps ] ];
    constructions = scene[ "Constructions" ];
    assertions = scene[ "Assertions" ];
    prob = OptionValue[ "PruningProbability" ];
    branches = { init };
    Do[
      Module[ { effective },
        effective = skipBoundKeys[ step, First[ branches, <||> ] ];
        If[ effective =!= {},
          branches = evaluateStep[ graph, effective, constructions, branches ];
          branches = pruneBranches[ branches, prob ]
        ]
      ],
      { step, steps }
    ];
    branches = filterAssertions[ graph, assertions, branches ];
    InfraInstance /@ branches
  ]
