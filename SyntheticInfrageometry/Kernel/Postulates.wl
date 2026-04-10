(* ::Package:: *)

(* ===================== Points ===================== *)

FindPoint::usage = "FindPoint[graph] finds a random vertex. FindPoint[graph, n] finds up to n vertices. Options: \"From\" (\"Random\"|\"Center\"|\"Periphery\"), \"Distance\" (number, {min,max}, or \"Max\"), \"MaxCliques\".";
Options[ FindPoint ] = { "From" -> "Random", "Distance" -> None, "MaxCliques" -> All };

FindPoint[ graph_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
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

(* ===================== Segments ===================== *)

FindSegment::usage = "FindSegment[graph, p1, p2] finds all geodesics. FindSegment[graph, p1, p2, n] returns up to n. FindSegment[graph, {p1, p2}] also accepted. Option: \"Select\" sorts results by criterion.";
Options[ FindSegment ] = { "Select" -> None };

FindSegment[ graph_Graph, p1_, p2_, n : (_Integer | All) : 1, opts : OptionsPattern[] ] :=
  Module[ { d, paths, context },
    d = GraphDistance[ graph, p1, p2 ];
    If[ d === Infinity, Return[ {} ] ];
    paths = FindPath[ graph, p1, p2, { d }, All ];
    context = <| "Cyclic" -> False, "Endpoints" -> { p1, p2 } |>;
    paths = applySelect[ graph, paths, OptionValue[ "Select" ], context ];
    If[ n === All, paths, Take[ paths, UpTo[ n ] ] ]
  ]

FindSegment[ graph_Graph, { p1_, p2_ }, n_Integer : 1, opts : OptionsPattern[] ] :=
  FindSegment[ graph, p1, p2, n, opts ]

(* ===================== Lines ===================== *)

Options[ FindLine ] = { "Select" -> None };

FindLine[ graph_Graph, p1_, p2_, n : (_Integer | All) : 1, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { geodesics, allExtensions, context },
    geodesics = FindSegment[ graph, p1, p2, All ];
    allExtensions = Union @ Flatten[
      findLineExtensions[ graph, # ] & /@ geodesics, 1 ];
    context = <| "Cyclic" -> False, "Endpoints" -> { p1, p2 } |>;
    allExtensions = applySelect[ graph, allExtensions, OptionValue[ "Select" ], context ];
    If[ n === All, allExtensions, Take[ allExtensions, UpTo[ n ] ] ]
  ]

FindLine[ graph_Graph, segment_List, opts : OptionsPattern[] ] :=
  First @ FindLine[ graph, segment, 1, opts ]

FindLine[ graph_Graph, segment_List, n : (_Integer | All), opts : OptionsPattern[] ] :=
  Module[ { allExtensions, context },
    allExtensions = findLineExtensions[ graph, segment ];
    context = <| "Cyclic" -> False, "Endpoints" -> SegmentEndpoints[ segment ] |>;
    allExtensions = applySelect[ graph, allExtensions, OptionValue[ "Select" ], context ];
    If[ n === All, allExtensions, Take[ allExtensions, UpTo[ n ] ] ]
  ]

findLineExtensions[ graph_Graph, segment_List ] :=
  Module[ { p1, p2, d, extendBefore, extendAfter, pairs, maxPairs,
            startVertices, endVertices, beforePaths, afterPaths },
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
    startVertices = Union[ maxPairs[[ All, 1 ]] ];
    endVertices = Union[ maxPairs[[ All, 2 ]] ];
    beforePaths = Flatten[ Table[
      With[ { db = GraphDistance[ graph, s, p1 ] },
        If[ db == 0, { {} }, Most /@ FindPath[ graph, s, p1, { db }, All ] ]
      ],
      { s, startVertices }
    ], 1 ];
    afterPaths = Flatten[ Table[
      With[ { da = GraphDistance[ graph, p2, e ] },
        If[ da == 0, { {} }, Rest /@ FindPath[ graph, p2, e, { da }, All ] ]
      ],
      { e, endVertices }
    ], 1 ];
    If[ beforePaths === {}, beforePaths = { {} } ];
    If[ afterPaths === {}, afterPaths = { {} } ];
    Flatten[ Outer[ Join[ #1, segment, #2 ] &, beforePaths, afterPaths, 1 ], 1 ]
  ]

(* ===================== Circles ===================== *)

FindCircle::usage = "FindCircle[graph, p, r] finds separating cycles at distance r from p. FindCircle[graph, p, {rMin, rMax}] uses a distance range. FindCircle[graph, p, r, n] returns up to n. Option: \"Select\" sorts results by criterion.";
Options[ FindCircle ] = { "Select" -> None };

FindCircle[ graph_Graph, p_, r_, n : (_Integer | All) : 1, opts : OptionsPattern[] ] :=
  Module[ { range, cs, allCycles, vertexCycles, context },
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    cs = Subgraph[ graph, Select[ VertexList[ graph ],
      range[[ 1 ]] <= GraphDistance[ graph, p, # ] <= range[[ 2 ]] & ] ];
    allCycles = FindCycle[ cs, Infinity, All ];
    If[ allCycles === {}, Return[ {} ] ];
    vertexCycles = (First /@ #) & /@ allCycles;
    vertexCycles = FindSeparatingCycles[ graph, vertexCycles, p, If[ NumericQ[ r ], r, Mean[ r ] ] ];
    context = <| "Cyclic" -> True, "Center" -> p, "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |>;
    vertexCycles = applySelect[ graph, vertexCycles, OptionValue[ "Select" ], context ];
    If[ n === All, vertexCycles, Take[ vertexCycles, UpTo[ n ] ] ]
  ]
