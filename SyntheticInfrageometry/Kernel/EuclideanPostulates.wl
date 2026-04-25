Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]


(* ===================== Points ===================== *)

Options[ FindPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindPoint[ graph_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Module[ { from, pool, dist, range, distMatrix, vertexIndex, auxiliaryGraph, cliques, thresholds, finiteMax, maxCl },
    from = OptionValue[ "From" ];
    pool = Which[
      StringQ[ from ] && from == "Center", GraphCenter[ graph ],
      StringQ[ from ] && from == "Periphery", GraphPeriphery[ graph ],
      StringQ[ from ], VertexList[ graph ],
      MemberQ[ VertexList[ graph ], from ], { from },
      ListQ[ from ], from,
      True, VertexList[ graph ]
    ];
    dist = OptionValue[ "Distance" ];
    maxCl = OptionValue[ "MaxCliques" ];
    If[ n == 1 || dist === None,
      RandomSample[ pool, UpTo[ n ] ],
      vertexIndex = Lookup[ AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ], pool ];
      distMatrix = GraphDistanceMatrix[ graph ][[ vertexIndex, vertexIndex ]];
      finiteMax = Max[ Select[ Flatten[ distMatrix ], # < Infinity & ] ];
      distMatrix = Replace[ distMatrix, Infinity -> finiteMax + 1, {2} ];
      If[ dist === "Max",
        thresholds = Reverse @ DeleteCases[ Union @@ distMatrix, 0 | _?( # > finiteMax & ) ];
        cliques = {};
        Do[
          auxiliaryGraph = AdjacencyGraph[ pool,
            UnitStep[ distMatrix - d ] * UnitStep[ finiteMax - distMatrix ] * (1 - IdentityMatrix[ Length[ vertexIndex ] ]) ];
          cliques = FindClique[ auxiliaryGraph, { n, VertexCount[ auxiliaryGraph ] }, maxCl ];
          If[ cliques =!= {}, Break[] ],
          { d, thresholds }
        ];
        If[ cliques === {}, {}, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ],
        range = Replace[ dist, { d_?NumericQ :> { d, finiteMax }, { dMin_, dMax_ } :> { dMin, dMax /. Infinity -> finiteMax } } ];
        auxiliaryGraph = AdjacencyGraph[ pool,
          UnitStep[ distMatrix - range[[ 1 ]] ] * UnitStep[ range[[ 2 ]] - distMatrix ] *
            (1 - IdentityMatrix[ Length[ vertexIndex ] ]) ];
        cliques = FindClique[ auxiliaryGraph, { Min[ n, VertexCount[ auxiliaryGraph ] ], VertexCount[ auxiliaryGraph ] }, maxCl ];
        If[ cliques === {}, {}, RandomSample[ RandomChoice[ cliques ], UpTo[ n ] ] ]
      ]
    ]
  ]

FindPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindPoint[ graph, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== Segments ===================== *)

Options[ FindSegment ] = { "Select" -> None };

FindSegment[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] :=
  Module[ { d, paths, context },
    d = GraphDistance[ graph, p1, p2 ];
    If[ d === Infinity, Return[ {} ] ];
    paths = FindPath[ graph, p1, p2, { d }, All ];
    context = <| "Cyclic" -> False, "Endpoints" -> { p1, p2 } |>;
    applySelect[ graph, paths, OptionValue[ "Select" ], context ]
  ]

FindSegment[ graph_Graph, p1_, p2_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindSegment[ graph, p1, p2, All, opts ], UpTo[ n ] ]

FindSegment[ graph_Graph, p1_, p2_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindSegment[ graph, p1, p2, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]

FindSegment[ graph_Graph, { p1_, p2_ }, args___ ] :=
  FindSegment[ graph, p1, p2, args ]


(* ===================== Lines ===================== *)

Options[ FindLine ] = { "Select" -> None };

FindLine[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { geodesics, allExtensions, context },
    geodesics = FindSegment[ graph, p1, p2, All ];
    allExtensions = Union @ Flatten[
      findLineExtensions[ graph, # ] & /@ geodesics, 1 ];
    context = <| "Cyclic" -> False, "Endpoints" -> { p1, p2 } |>;
    applySelect[ graph, allExtensions, OptionValue[ "Select" ], context ]
  ]

FindLine[ graph_Graph, p1_, p2_, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Take[ FindLine[ graph, p1, p2, All, opts ], UpTo[ n ] ]

FindLine[ graph_Graph, p1_, p2_, n_Integer : 1, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  With[ { result = FindLine[ graph, p1, p2, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]

findLineExtensions[ graph_Graph, segment_List ] :=
  Module[ { p1, p2, d, extendBefore, extendAfter, pairs, maxPairs },
    If[ Length[ segment ] < 2, Return[ { segment } ] ];
    p1 = First[ segment ];
    p2 = Last[ segment ];
    d = GraphDistance[ graph, p1, p2 ];
    extendBefore = Select[ VertexList[ graph ],
      candidate |-> GraphDistance[ graph, candidate, p1 ] + d == GraphDistance[ graph, candidate, p2 ]
    ];
    extendAfter = Select[ VertexList[ graph ],
      candidate |-> GraphDistance[ graph, candidate, p2 ] + d == GraphDistance[ graph, p1, candidate ]
    ];
    pairs = Tuples[ { extendBefore, extendAfter } ];
    maxPairs = MaximalBy[ pairs, GraphDistance[ graph, #[[ 1 ]], #[[ 2 ]] ] & ];
    If[ maxPairs === { { p1, p2 } }, Return[ { segment } ] ];
    Flatten[
      With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
        With[ { db = GraphDistance[ graph, s, p1 ], da = GraphDistance[ graph, p2, e ] },
          With[ { bp = If[ db == 0, { {} }, Most /@ FindPath[ graph, s, p1, { db }, All ] ],
                  ap = If[ da == 0, { {} }, Rest /@ FindPath[ graph, p2, e, { da }, All ] ] },
            Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ]
          ]
        ]
      ] & /@ maxPairs,
      1
    ]
  ]


(* ===================== Spheres ===================== *)

Options[ FindSphere ] = { "Select" -> None, Method -> "SeparatingCycle" };

FindSphere[ graph_Graph, p_, r_, All, opts : OptionsPattern[] ] :=
  Module[ { method, range, cs, allCycles, vertexCycles, context, equiSet },
    method = OptionValue[ Method ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    Switch[ method,
      "MetricCircle",
        equiSet = Select[ VertexList[ graph ],
          range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ];
        { equiSet },
      "SeparatingCycle",
        cs = Subgraph[ graph, Select[ VertexList[ graph ],
          range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ] ];
        allCycles = FindCycle[ cs, Infinity, All ];
        If[ allCycles === {}, Return[ {} ] ];
        vertexCycles = (First /@ #) & /@ allCycles;
        vertexCycles = FindSeparatingCycles[ graph, vertexCycles, p, If[ NumericQ[ r ], r, Mean[ r ] ] ];
        context = <| "Cyclic" -> True, "Center" -> p, "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |>;
        applySelect[ graph, vertexCycles, OptionValue[ "Select" ], context ],
      _, $Failed
    ]
  ]

FindSphere[ graph_Graph, p_, r_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindSphere[ graph, p, r, All, opts ], UpTo[ n ] ]

FindSphere[ graph_Graph, p_, r_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindSphere[ graph, p, r, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]
