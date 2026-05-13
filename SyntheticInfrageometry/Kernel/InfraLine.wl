Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineCore]
PackageScope[findLineExtensions]
PackageScope[findLineExtensionsWith]
PackageScope[findLineExtensionsGreedy]
PackageScope[findParallelCore]
PackageScope[findPerpendicularCore]
PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


(* FindLine returns InfraSegment (a maximal geodesic is a longest-form segment);
   this file owns the line-shaped Find / construction / predicate operations. *)


(* ===================== FindLine ===================== *)

(* A line through p1, p2: a maximal geodesic extension (a, ..., p1, ..., p2, ..., b)
   every contiguous sub-sequence of which is a geodesic, inextensible at both ends. *)

Options[ FindLine ] = { "Maximality" -> "Extension", Method -> "ShortestPath", "Pruning" -> Infinity };

FindLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findLineCore[ graph, ##, opts ] &, p1, p2 ]


findLineCore[ graph_Graph, p1_, p2_, opts : OptionsPattern[ FindLine ] ] /;
    MemberQ[ VertexList[ graph ], p1 ] :=
  With[ { spec = OptionValue[ FindLine, { opts }, Method ] /. Automatic -> "ShortestPath",
          pruning = OptionValue[ FindLine, { opts }, "Pruning" ] },
    With[ { method = methodName[ spec ] },
      With[ { middles = Switch[ method,
                "ShortestPath" | "Greedy" | "Embedding", allGeodesics[ graph, p1, p2 ],
                "ShortestPathExtension" | "CurvatureMinimizing",
                  With[ { paths = findSegmentCore[ graph, p1, p2, All, Method -> spec ] },
                    If[ ListQ[ paths ], paths, { } ] ]
              ] },
        With[ { allExtensions = With[ { ext = Union @ Flatten[
                  Switch[ method,
                    "Greedy",
                      findLineExtensionsGreedy[ graph, # ] & /@ middles,
                    _,
                      findLineExtensions[ graph, #, pruning ] & /@ middles
                  ], 1 ] },
                If[ OptionValue[ FindLine, { opts }, "Maximality" ] === "Diameter",
                  Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
                  ext ] ] },
          Switch[ method,
            "ShortestPath" | "ShortestPathExtension" | "CurvatureMinimizing" | "Greedy", allExtensions,
            "Embedding",
              With[ { embOpts = parseEmbeddingMethod[ spec ] },
                If[ embOpts[ "Pool" ] === "ShortestPaths",
                  embeddingRankLines[ graph, allExtensions, p1, p2, embOpts ],
                  allExtensions ]
              ]
          ]
        ]
      ]
    ]
  ]


embeddingRankLines[ graph_Graph, lines_List, p1_, p2_, embOpts_Association ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { ep = Lookup[ vertexIndex, { p1, p2 } ] },
      SortBy[ lines,
        line |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, line ], ep ] ]
    ]
  ]


(* Maximal geodesic extensions of a segment.  Asymmetric: each side is
   extended independently to its maximal admissible length; among pairs
   that achieve a valid joint geodesic (degenerate triangle inequality
   d(s, e) == d(s, p1) + d + d(p2, e)) we keep those with maximum total
   extension length b_s + a_e.  The "With" form takes prefix and suffix
   path-enumeration closures (ExtendSegment swaps in extended-out /
   curvature-pulled enumerators for the non-ShortestPath methods). *)

findLineExtensions[ graph_Graph, segment_List, pruning_ : Infinity ] :=
  findLineExtensionsWith[ graph, segment,
    { s, p, db } |-> applyPruning[ FindPath[ graph, s, p, { db }, All ], pruning ],
    { p, e, da } |-> applyPruning[ FindPath[ graph, p, e, { da }, All ], pruning ] ]


findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_ ] :=
  findLineExtensionsWith[ graph, segment, prefixFn, suffixFn, True & ]

findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_, admissible_ ] /; Length[ segment ] < 2 :=
  { segment }

findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_, admissible_ ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    With[ { extendBefore = Select[ VertexList[ graph ],
              c |-> admissible[ c ] && GraphDistance[ graph, c, p1 ] + d == GraphDistance[ graph, c, p2 ] ],
            extendAfter  = Select[ VertexList[ graph ],
              c |-> admissible[ c ] && GraphDistance[ graph, c, p2 ] + d == GraphDistance[ graph, p1, c ] ] },
      With[ { validPairs = Select[ Tuples[ { extendBefore, extendAfter } ],
              pair |-> GraphDistance[ graph, pair[[1]], pair[[2]] ] ==
                       GraphDistance[ graph, pair[[1]], p1 ] + d + GraphDistance[ graph, p2, pair[[2]] ] ] },
        With[ { maxPairs = MaximalBy[ validPairs,
                GraphDistance[ graph, #[[1]], p1 ] + GraphDistance[ graph, p2, #[[2]] ] & ] },
          If[ maxPairs === { } || maxPairs === { { p1, p2 } }, { segment },
            Flatten[
              With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
                With[ { db = GraphDistance[ graph, s, p1 ], da = GraphDistance[ graph, p2, e ] },
                  With[ { bp = If[ db == 0, { {} },
                                  Most /@ Select[ prefixFn[ s, p1, db ], AllTrue[ #, admissible ] & ] ],
                          ap = If[ da == 0, { {} },
                                  Rest /@ Select[ suffixFn[ p2, e, da ], AllTrue[ #, admissible ] & ] ] },
                    Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ] ] ]
              ] & /@ maxPairs,
              1 ]
          ]
        ]
      ]
    ]
  ]


(* Greedy maximal geodesic extension: walk vertex-by-vertex outward from each
   endpoint, accepting the first neighbor that extends the geodesic by one
   step.  Returns exactly one chain — maximally inextensible but not
   necessarily of maximum total length. *)

findLineExtensionsGreedy[ graph_Graph, segment_List ] :=
  findLineExtensionsGreedy[ graph, segment, True & ]

findLineExtensionsGreedy[ graph_Graph, segment_List, admissible_ ] :=
  { greedyWalkBoth[ graph, segment, admissible ] }

greedyWalkBoth[ graph_Graph, segment_List, admissible_ ] /; Length[ segment ] < 2 := segment

greedyWalkBoth[ graph_Graph, segment_List, admissible_ ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    Join[ Reverse @ greedyWalk[ graph, p1, p2, d, admissible ],
          segment,
          greedyWalk[ graph, p2, p1, d, admissible ] ] ]


greedyWalk[ graph_Graph, h_, a_, db_, admissible_ ] :=
  With[ { v = SelectFirst[ AdjacencyList[ graph, h ],
            c |-> admissible[ c ] && GraphDistance[ graph, c, a ] == db + 1, Missing[] ] },
    If[ MissingQ[ v ], { },
      Prepend[ greedyWalk[ graph, v, a, db + 1, admissible ], v ] ]
  ]


(* ===================== FindParallel ===================== *)

(* FindParallel[g, line, p]: maximal sub-segment of a maximal geodesic
   through p whose vertices all lie at distance r = d(p, line) from line. *)

Options[ FindParallel ] = { Method -> "ShortestPath", "Pruning" -> Infinity };

FindParallel[ graph_Graph, line_, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findParallelCore[ graph, ##, opts ] &, line, p ]


findParallelCore[ graph_Graph, line_List, p_, opts : OptionsPattern[ FindParallel ] ] :=
  With[ { spec = OptionValue[ FindParallel, { opts }, Method ] /. Automatic -> "ShortestPath",
          pruning = OptionValue[ FindParallel, { opts }, "Pruning" ] },
    Switch[ methodName @ spec,
      "ShortestPath", findParallelExtensions[ graph, line, p, pruning ],
      "Greedy",       findParallelExtensionsGreedy[ graph, line, p ],
      "Embedding",
        embeddingRankParallels[ graph, findParallelExtensions[ graph, line, p, pruning ], line, p,
          parseEmbeddingMethod[ spec, "LevelSet" ] ]
    ]
  ]


(* Maximal level-set geodesics through p: every vertex of the result lies at
   distance r = d(p, line) from line, and the chain is a geodesic in graph.
   Seeded by each level-set neighbor of p, extended on both sides via the
   line-extension machinery with an extra admissibility predicate. *)

findParallelExtensions[ graph_Graph, line_List, p_, pruning_ : Infinity ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    With[ { r = lineDist[ p ] },
      If[ r === Infinity, { },
        With[ { admissible = c |-> lineDist[ c ] == r },
          With[ { seeds = Select[ AdjacencyList[ graph, p ], admissible ] },
            With[ { chains = Flatten[
                    findLineExtensionsWith[ graph, { p, # },
                      { s, q, db } |-> applyPruning[ FindPath[ graph, s, q, { db }, All ], pruning ],
                      { q, e, da } |-> applyPruning[ FindPath[ graph, q, e, { da }, All ], pruning ],
                      admissible ] & /@ seeds,
                    1 ] },
              DeleteDuplicates @ Map[ canonicalLine, Select[ chains, Length[ # ] >= 2 & ] ]
            ]
          ]
        ]
      ]
    ]
  ]


findParallelExtensionsGreedy[ graph_Graph, line_List, p_ ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    With[ { r = lineDist[ p ] },
      If[ r === Infinity, { },
        With[ { admissible = c |-> lineDist[ c ] == r },
          With[ { seed = SelectFirst[ AdjacencyList[ graph, p ], admissible, Missing[] ] },
            If[ MissingQ[ seed ], { { p } },
              { greedyWalkBoth[ graph, { p, seed }, admissible ] } ]
          ]
        ]
      ]
    ]
  ]


embeddingRankParallels[ graph_Graph, parallels_List, line_List, p_, embOpts_Association ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { lineDir = coords[[ vertexIndex[ Last[ line ] ] ]] - coords[[ vertexIndex[ First[ line ] ] ]],
            pCoord  = coords[[ vertexIndex[ p ] ]],
            n = Length[ coords ] },
      With[ { augmented = Join[ coords, { pCoord - lineDir / 2, pCoord + lineDir / 2 } ] },
        SortBy[ parallels,
          par |-> EmbeddingHausdorffDistance[ augmented, Lookup[ vertexIndex, par ], { n + 1, n + 2 } ] ]
      ]
    ]
  ]


(* ===================== FindPerpendicular ===================== *)

(* Foot of the perpendicular from p to L (Euclid I.12, isosceles base midpoint):
   for each pair {a, b} of L-vertices equidistant from p, the midpoint of the
   line-arc from a to b along L is a candidate foot. *)

Options[ FindPerpendicular ] = { Method -> "Metric" };

FindPerpendicular[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    findPerpendicularCore[ graph, ##, opts ] &, line, point ]


findPerpendicularCore[ graph_Graph, line_List, point_, opts : OptionsPattern[ FindPerpendicular ] ] :=
  With[ { spec = OptionValue[ FindPerpendicular, { opts }, Method ] },
    Switch[ methodName @ spec,
      "Metric",
        With[ { distances = GraphDistance[ graph, point, # ] & /@ line },
          Union @ Flatten[
            ( group |-> Map[
                pair |-> With[ { lo = Min @@ pair, hi = Max @@ pair },
                  If[ OddQ[ hi - lo ],
                    line[[ lo ;; hi ]][[ Ceiling[ ( hi - lo + 1 ) / 2 ] ]],
                    Nothing ] ],
                Subsets[ group, { 2 } ] ]
            ) /@ Values @ GroupBy[ Range @ Length @ line, distances[[ # ]] & ]
          ]
        ],
      "Embedding",
        embeddingRankPerpendicularFeet[ graph, line, point, parseEmbeddingMethod @ spec ]
    ]
  ]


embeddingRankPerpendicularFeet[ graph_Graph, line_List, point_, embOpts_Association ] :=
  With[ { coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ],
          vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ] },
    With[ { lineStart = coords[[ vertexIndex[ First[ line ] ] ]],
            lineEnd   = coords[[ vertexIndex[ Last[ line ] ] ]],
            pointCoord = coords[[ vertexIndex[ point ] ]] },
      With[ { dir = lineEnd - lineStart },
        With[ { dirNorm = dir . dir },
          With[ { foot = If[ dirNorm == 0, lineStart,
                  lineStart + ( ( pointCoord - lineStart ) . dir / dirNorm ) * dir ] },
            SortBy[ line, v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], foot ] ]
          ]
        ]
      ]
    ]
  ]


(* ===================== FindCommonLine ===================== *)

(* Canonical maximal geodesics through every vertex in verts. *)

findCommonLineCore[ graph_Graph, verts_List ] :=
  With[ { uverts = DeleteDuplicates @ Catenate[ infraUnionSpread /@ verts ] },
    If[ Length[ uverts ] < 2, { },
      DeleteDuplicates @ Select[
        canonicalLine[ #[[ 1, 1 ]] ] & /@ FindLine[ graph, First @ uverts, uverts[[ 2 ]], All ],
        line |-> SubsetQ[ line, uverts ] ]
    ]
  ]

FindCommonLine[ graph_Graph, verts_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { capped = infraCap[ findCommonLineCore[ graph, verts ], count ] },
    If[ capped === $Failed, $Failed, InfraSegment[ { # } ] & /@ capped ]
  ]


(* ===================== SegmentLineAngle ===================== *)

(* Length-valued angle surrogate: d(p2, L) when p1 in L, Infinity otherwise. *)

Options[ SegmentLineAngle ] = { Method -> "Metric" };

SegmentLineAngle[ graph_Graph, p1_, p2_, line_List, opts : OptionsPattern[] ] :=
  If[ ! MemberQ[ line, p1 ], Infinity,
    Min[ GraphDistance[ graph, p2, # ] & /@ line ] ]

SegmentLineAngle[ graph_Graph, segment_List, line_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  SegmentLineAngle[ graph, First[ segment ], Last[ segment ], line, opts ]


(* ===================== LineQ ===================== *)

(* A segment is a line iff no extension preserves the geodesic property. *)

LineQ[ graph_Graph, segment_List ] :=
  SegmentQ[ graph, segment ] &&
  Length[ First @ findLineExtensions[ graph, segment ] ] == Length[ segment ]


(* ===================== ParallelQ ===================== *)

(* Definition-alpha parallelism: l1 and l2 are disjoint and the distance from
   each vertex of l1 to l2 is constant up to threshold. *)

ParallelQ[ distanceMatrix_List, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Min[ distanceMatrix[[ #, l2 ]] ] & /@ l1 },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold ]
  ]

ParallelQ[ graph_Graph, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Table[ Min[ GraphDistance[ graph, v, # ] & /@ l2 ], { v, l1 } ] },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold ]
  ]


(* ===================== PencilDirections / PencilCardinality / LineCount ===================== *)

(* Canonical maximal geodesics through origin, one per projective direction class
   at origin.  LineCount: canonical maximal geodesics overall. *)

PencilDirections[ graph_Graph, origin_ ] :=
  DeleteDuplicates @ Map[ canonicalLine, Flatten[
    Map[ #[[ 1, 1 ]] &, FindLine[ graph, origin, #, All ] ] & /@
      DeleteCases[ VertexList[ graph ], origin ],
    1 ] ]

PencilCardinality[ graph_Graph, origin_ ] := Length @ PencilDirections[ graph, origin ]

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]


(* ===================== Helpers: canonical lines ===================== *)

(* canonicalLine: lexicographic minimum of a line and its reversal.
   allCanonicalLines: every canonical maximal geodesic in the graph
   (consumed by PencilDirections, LineCount, and ProjectiveGeometry.wl). *)

canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine[ #[[ 1, 1 ]] ] & /@ FindLine[ graph, #[[ 1 ]], #[[ 2 ]], All ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]
