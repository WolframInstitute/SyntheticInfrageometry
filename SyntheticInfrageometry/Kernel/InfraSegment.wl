Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findSegmentCore]
PackageScope[extendSegmentCore]
PackageScope[formanEdgeKappa]
PackageScope[wolframVertexKappa]
PackageScope[ollivierEdgeKappa]


(* ===================== InfraSegment wrapper ===================== *)

InfraSegment[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraSegment[ _List ] ] ] :=
  InfraSegment[ Flatten[ reps /. InfraSegment[ xs_List ] :> xs, 1 ] ]


(* ===================== FindSegment ===================== *)

(* A segment between p1 and p2: a geodesic vertex sequence
   (p1 = v0, v1, ..., vk = p2) with k = d(p1, p2) and consecutive vi adjacent. *)

Options[ FindSegment ] = { Method -> "ShortestPath" };

FindSegment[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findSegmentCore[ graph, ##, count, opts ] &, p1, p2 ]


findSegmentCore[ _Graph, p1_, p1_, ___ ] := { }

findSegmentCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindSegment ] ] :=
  With[ { spec = OptionValue[ FindSegment, { opts }, Method ] },
    With[ { subOpts = Replace[ spec, { { _String, rest___ } :> { rest }, _ :> { } } ] },
      Switch[ methodName @ spec,
        "ShortestPath",
          If[ count === 1,
            With[ { path = FindShortestPath[ graph, p1, p2 ] },
              If[ path === { }, { }, { path } ] ],
            With[ { d = GraphDistance[ graph, p1, p2 ] },
              If[ d === Infinity, { },
                FindPath[ graph, p1, p2, { d }, count /. UpTo[ k_ ] :> k ] ] ]
          ],
        "ShortestPathExtension",
          extendedOutPaths[ graph, p1, p2,
            "Pruning"            /. subOpts /. "Pruning"            -> Infinity,
            "ShortestPathWindow" /. subOpts /. "ShortestPathWindow" -> 2,
            countLimit[ count ] ],
        "CurvatureMinimizing",
          With[ { prune = "Pruning" /. subOpts /. "Pruning" -> Infinity,
                  pool  = "Pool"    /. subOpts /. "Pool"    -> "ShortestPaths",
                  curvatureSpec = parseCurvatureSpec[
                    "Curvature" /. subOpts /. "Curvature" -> "Forman" ] },
            pulledPaths[ graph, p1, p2,
              buildEdgeKappa[ graph, curvatureSpec ],
              prune, countLimit[ count ],
              If[ pool === "ShortestPaths", geodesicDAGNeighbors[ graph, p1, p2 ], Automatic ] ]
          ],
        "Embedding",
          takeUpTo[ embeddingFindSegmentPaths[ graph, p1, p2, parseEmbeddingMethod[ spec ] ],
            countLimit[ count ] ]
      ]
    ]
  ]


(* Per-edge / per-vertex curvature lookups.  Shared by buildEdgeKappa (the
   pulledPaths frontier sweep) and by pathCurvatureScores in PathSpace.wl
   (the SelectPath "MinCurvature" pool selector). *)

formanEdgeKappa[ graph_Graph, KeyValuePattern[ { "Head" -> "Forman", "Method" -> formanMethod_ } ] ] :=
  With[ { fEdges = WolframInstitute`Infrageometry`FormanRicciCurvature[
        graph, "MaxCellDimension" -> If[ formanMethod === "Triangles", 2, 1 ] ] },
    With[ { fSym = Join[ fEdges,
            AssociationThread[
              UndirectedEdge[ #[[ 2 ]], #[[ 1 ]] ] & /@ Keys[ fEdges ],
              Values[ fEdges ] ] ] },
      { v, w } |-> fSym[ UndirectedEdge[ v, w ] ]
    ]
  ]

wolframVertexKappa[ graph_Graph, KeyValuePattern[ { "Head" -> "Wolfram", "Dimension" -> dim_, "Radii" -> radii_ } ] ] :=
  If[ radii === Automatic,
    WolframInstitute`Infrageometry`WolframRicciCurvature[ graph, "Dimension" -> dim ],
    WolframInstitute`Infrageometry`WolframRicciCurvature[ graph, radii, "Dimension" -> dim ] ]

ollivierEdgeKappa[ graph_Graph, KeyValuePattern[ { "Head" -> "Ollivier" } ] ] :=
  With[ { oEdges = WolframInstitute`Infrageometry`OllivierRicciCurvature[ graph ] },
    With[ { oSym = Join[ oEdges,
            AssociationThread[
              UndirectedEdge[ #[[ 2 ]], #[[ 1 ]] ] & /@ Keys[ oEdges ],
              Values[ oEdges ] ] ] },
      { v, w } |-> oSym[ UndirectedEdge[ v, w ] ]
    ]
  ]


(* buildEdgeKappa[g, spec]: (v, w) |-> kappa(v, w) closure that the
   CurvatureMinimizing frontier sweep MinimalBy's on.  For vertex-curvature
   heads the asymmetric mapping {v, w} |-> kappa(w) is used (legal for a
   greedy frontier sweep that picks one neighbour at a time). *)

buildEdgeKappa[ graph_Graph, spec : KeyValuePattern[ "Head" -> "Forman" ] ] :=
  formanEdgeKappa[ graph, spec ]

buildEdgeKappa[ graph_Graph, spec : KeyValuePattern[ "Head" -> "Wolfram" ] ] :=
  With[ { kappa = wolframVertexKappa[ graph, spec ] }, { v, w } |-> kappa[ w ] ]

buildEdgeKappa[ graph_Graph, spec : KeyValuePattern[ "Head" -> "Ollivier" ] ] :=
  ollivierEdgeKappa[ graph, spec ]


embeddingFindSegmentPaths[ graph_Graph, p1_, p2_, embOpts_Association ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ],
          prune = embOpts[ "Pruning" ] },
    With[ { extendFn = If[ embOpts[ "Pool" ] === "ShortestPaths",
              With[ { dagNbrs = geodesicDAGNeighbors[ graph, p1, p2 ] },
                path |-> Lookup[ dagNbrs, Key @ Last @ path, { } ] ],
              path |-> Complement[ AdjacencyList[ graph, Last @ path ], path ] ] },
      With[ { paths = generateEmbeddingPaths[ extendFn, { p1 }, Last[ # ] === p2 &, prune ],
              ep    = Lookup[ vertexIndex, { p1, p2 } ] },
        SortBy[ paths,
          path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
      ]
    ]
  ]


(* ===================== ExtendSegment ===================== *)

(* ExtendSegment[g, segment] extends a vertex sequence to a maximal geodesic line.
   ExtendSegment[g, a, b, c, d, n] is the Tarski A4 synthetic-extension axiom:
   x with B(a, b, x) and d(b, x) == d(c, d). *)

Options[ ExtendSegment ] = { Method -> "ShortestPath", "Length" -> Automatic, "Pruning" -> Infinity };

ExtendSegment[ graph_Graph, segment_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    extendSegmentCore[ graph, ##, opts ] &, segment ]

ExtendSegment[ graph_Graph, a_, b_, c_, d_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, OptionsPattern[] ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    With[ { vs = If[ target === Infinity, { },
        Select[ VertexList[ graph ],
          x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target ] ] },
      With[ { capped = infraCap[ vs, count ] },
        If[ capped === $Failed, $Failed, InfraPoint[ { # } ] & /@ capped ]
      ]
    ]
  ]


extendSegmentCore[ graph_Graph, segment_List, opts : OptionsPattern[ ExtendSegment ] ] /;
    Length[ segment ] < 2 := { segment }

extendSegmentCore[ graph_Graph, segment_List, opts : OptionsPattern[ ExtendSegment ] ] :=
  With[ { spec = OptionValue[ ExtendSegment, { opts }, Method ] /. Automatic -> "ShortestPath",
          lengthCap = OptionValue[ ExtendSegment, { opts }, "Length" ],
          pruning = OptionValue[ ExtendSegment, { opts }, "Pruning" ] },
    With[ { subOpts = Replace[ spec, { { _String, rest___ } :> { rest }, _ :> { } } ] },
      With[ { rawExtensions = Switch[ methodName @ spec,
          "ShortestPath",
            findLineExtensions[ graph, segment, pruning ],
          "Greedy",
            findLineExtensionsGreedy[ graph, segment ],
          "ShortestPathExtension",
            With[ { prune  = "Pruning"            /. subOpts /. "Pruning"            -> Infinity,
                    window = "ShortestPathWindow" /. subOpts /. "ShortestPathWindow" -> 2 },
              findLineExtensionsWith[ graph, segment,
                { s, p, db } |-> extendedOutPaths[ graph, s, p, prune, window, Infinity ],
                { p, e, da } |-> extendedOutPaths[ graph, p, e, prune, window, Infinity ] ]
            ],
          "CurvatureMinimizing",
            With[ { prune = "Pruning" /. subOpts /. "Pruning" -> Infinity,
                    curvatureSpec = parseCurvatureSpec[
                      "Curvature" /. subOpts /. "Curvature" -> "Forman" ] },
              With[ { edgeKappa = buildEdgeKappa[ graph, curvatureSpec ] },
                findLineExtensionsWith[ graph, segment,
                  { s, p, db } |-> pulledPaths[ graph, s, p, edgeKappa, prune, Infinity, Automatic ],
                  { p, e, da } |-> pulledPaths[ graph, p, e, edgeKappa, prune, Infinity, Automatic ] ]
              ]
            ],
          "Embedding",
            embeddingExtendSegment[ graph, segment, parseEmbeddingMethod[ spec ] ]
      ] },
        If[ IntegerQ @ lengthCap,
          truncateAroundOriginal[ #, segment, lengthCap ] & /@ rawExtensions,
          rawExtensions ]
      ]
    ]
  ]


truncateAroundOriginal[ ext_List, segment_List, n_Integer ] :=
  With[ { offset = First @ First @ SequencePosition[ ext, segment ] },
    Take[ ext, { Max[ 1, offset - n ],
                 Min[ Length @ ext, offset + Length @ segment - 1 + n ] } ] ]


embeddingExtendSegment[ graph_Graph, segment_List, embOpts_Association ] :=
  Module[ { walkOut, walkIn },
    With[ { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
            vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
      With[ { segCoords = coords[[ Lookup[ vertexIndex, segment ] ]] },
        With[ { centroid = Mean[ segCoords ],
                diff = Last[ segCoords ] - First[ segCoords ] },
          With[ { direction = Which[
                  Length[ segCoords ] >= 3 && Norm[ diff ] > 0,
                    With[ { svd = SingularValueDecomposition[ ( # - centroid ) & /@ segCoords ] },
                      Normalize @ svd[[ 3, All, 1 ]] * Sign[ diff . svd[[ 3, All, 1 ]] ] ],
                  Norm[ diff ] > 0, Normalize[ diff ],
                  True, ConstantArray[ 0., Length @ First @ segCoords ]
                ] },
            If[ Norm[ direction ] == 0, { segment },
              With[ { basePoint = First[ segCoords ] },
                With[ { signedProj = v |-> ( coords[[ vertexIndex[ v ] ]] - basePoint ) . direction,
                        perpDist   = v |-> With[ { c = coords[[ vertexIndex[ v ] ]] - basePoint },
                                            Norm[ c - ( c . direction ) direction ] ] },
                  walkOut[ current_, visited_ ] :=
                    With[ { adj = Complement[ AdjacencyList[ graph, current ], visited ],
                            currentP = signedProj[ current ] },
                      With[ { cands = Select[ adj, signedProj[ # ] > currentP & ] },
                        If[ cands === { }, { },
                          With[ { best = First @ MinimalBy[ MaximalBy[ cands, signedProj ], perpDist ] },
                            Prepend[ walkOut[ best, Append[ visited, best ] ], best ] ] ] ] ];
                  walkIn[ current_, visited_ ] :=
                    With[ { adj = Complement[ AdjacencyList[ graph, current ], visited ],
                            currentP = signedProj[ current ] },
                      With[ { cands = Select[ adj, signedProj[ # ] < currentP & ] },
                        If[ cands === { }, { },
                          With[ { best = First @ MinimalBy[ MinimalBy[ cands, signedProj ], perpDist ] },
                            Prepend[ walkIn[ best, Append[ visited, best ] ], best ] ] ] ] ];
                  { Join[ Reverse @ walkIn[ First @ segment, segment ], segment,
                          walkOut[ Last @ segment, segment ] ] }
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]


(* ===================== SegmentQ ===================== *)

(* A vertex sequence (v0, ..., vk) is a geodesic from v0 to vk iff consecutive
   vertices are adjacent and the total edge count equals d(v0, vk). *)

SegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

SegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False


(* ===================== UniqueSegmentQ ===================== *)

(* UniqueSegmentQ[g, u, v]: GeodesicMultiplicity[g, u, v] == 1.
   UniqueSegmentQ[g]: every vertex pair admits a unique geodesic (geodetic graph). *)

UniqueSegmentQ[ graph_Graph, u_, v_ ] := GeodesicMultiplicity[ graph, u, v ] == 1

UniqueSegmentQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> UniqueSegmentQ[ graph, pair[[ 1 ]], pair[[ 2 ]] ] ]
