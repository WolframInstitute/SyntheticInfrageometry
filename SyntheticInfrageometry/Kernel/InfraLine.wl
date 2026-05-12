Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineCore]
PackageScope[findLineExtensions]
PackageScope[findLineExtensionsWith]
PackageScope[findParallelCore]
PackageScope[findPerpendicularCore]
PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


(* FindLine returns InfraSegment (a maximal geodesic is a longest-form segment);
   this file owns the line-shaped Find / construction / predicate operations. *)


(* ===================== canonicalLine / allCanonicalLines ===================== *)

(* canonicalLine: lexicographic minimum of a line and its reversal. *)

canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine[ #[[ 1, 1 ]] ] & /@ FindLine[ graph, #[[ 1 ]], #[[ 2 ]], All ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]


(* ===================== FindLine ===================== *)

(* A line through p1, p2: a maximal geodesic extension (a, ..., p1, ..., p2, ..., b)
   every contiguous sub-sequence of which is a geodesic, inextensible at both ends. *)

Options[ FindLine ] = { "Maximality" -> "Extension", Method -> "Shortest" };

FindLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findLineCore[ graph, ##, opts ] &, p1, p2 ]


findLineCore[ graph_Graph, p1_, p2_, opts : OptionsPattern[ FindLine ] ] /;
    MemberQ[ VertexList[ graph ], p1 ] :=
  With[ { spec = OptionValue[ FindLine, { opts }, Method ] /. Automatic -> "Shortest" },
    With[ { method = methodName[ spec ] },
      With[ { middles = Switch[ method,
                "Shortest" | "Embedding", allGeodesics[ graph, p1, p2 ],
                "ShortestPathExtension" | "CurvatureMinimizing",
                  With[ { paths = findSegmentCore[ graph, p1, p2, All, Method -> spec ] },
                    If[ ListQ[ paths ], paths, { } ] ]
              ] },
        With[ { allExtensions = With[ { ext = Union @ Flatten[ findLineExtensions[ graph, # ] & /@ middles, 1 ] },
                If[ OptionValue[ FindLine, { opts }, "Maximality" ] === "Diameter",
                  Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
                  ext ] ] },
          Switch[ method,
            "Shortest" | "ShortestPathExtension" | "CurvatureMinimizing", allExtensions,
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


(* Maximal geodesic extensions of a segment.  The "With" form takes prefix and
   suffix path-enumeration closures (ExtendSegment swaps in extended-out /
   curvature-pulled enumerators for the non-Shortest methods). *)

findLineExtensions[ graph_Graph, segment_List ] :=
  findLineExtensionsWith[ graph, segment,
    { s, p, db } |-> FindPath[ graph, s, p, { db }, All ],
    { p, e, da } |-> FindPath[ graph, p, e, { da }, All ] ]


findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_ ] /; Length[ segment ] < 2 :=
  { segment }

findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_ ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    With[ { extendBefore = Select[ VertexList[ graph ],
              c |-> GraphDistance[ graph, c, p1 ] + d == GraphDistance[ graph, c, p2 ] ],
            extendAfter = Select[ VertexList[ graph ],
              c |-> GraphDistance[ graph, c, p2 ] + d == GraphDistance[ graph, p1, c ] ] },
      With[ { maxPairs = MaximalBy[ Tuples[ { extendBefore, extendAfter } ],
                GraphDistance[ graph, #[[ 1 ]], #[[ 2 ]] ] & ] },
        If[ maxPairs === { { p1, p2 } }, { segment },
          Flatten[
            With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
              With[ { db = GraphDistance[ graph, s, p1 ], da = GraphDistance[ graph, p2, e ] },
                With[ { bp = If[ db == 0, { {} }, Most /@ prefixFn[ s, p1, db ] ],
                        ap = If[ da == 0, { {} }, Rest /@ suffixFn[ p2, e, da ] ] },
                  Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ] ] ]
            ] & /@ maxPairs,
            1 ]
        ]
      ]
    ]
  ]


(* ===================== FindParallel ===================== *)

(* FindParallel[g, line, p]: maximal sub-segment of a maximal geodesic
   through p whose vertices all lie at distance r = d(p, line) from line. *)

Options[ FindParallel ] = { Method -> "Metric" };

FindParallel[ graph_Graph, line_, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findParallelCore[ graph, ##, opts ] &, line, p ]


findParallelCore[ graph_Graph, line_List, p_, opts : OptionsPattern[ FindParallel ] ] :=
  With[ { spec = OptionValue[ FindParallel, { opts }, Method ] },
    Switch[ methodName @ spec,
      "Metric", findParallelMetric[ graph, line, p ],
      "Embedding",
        embeddingRankParallels[ graph, findParallelMetric[ graph, line, p ], line, p,
          parseEmbeddingMethod[ spec, "LevelSet" ] ]
    ]
  ]


findParallelMetric[ graph_Graph, line_List, p_ ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    With[ { r = lineDist[ p ] },
      If[ r === Infinity, { },
        With[ { levelSet = Select[ VertexList[ graph ], lineDist[ # ] == r & ] },
          With[ { segments = ( l |-> With[ { idx = First[ FirstPosition[ l, p, { 0 } ], 0 ] },
                  If[ idx === 0, { },
                    l[[
                      idx - LengthWhile[ Reverse @ Take[ l, idx - 1 ], MemberQ[ levelSet, # ] & ]
                      ;;
                      idx + LengthWhile[ Drop[ l, idx ], MemberQ[ levelSet, # ] & ] ]] ] ]
              ) /@ PencilDirections[ graph, p ] },
            With[ { dedup = DeleteDuplicates[ canonicalLine /@ Select[ segments, Length[ # ] >= 2 & ] ] },
              Select[ dedup,
                a |-> ! AnyTrue[ dedup, b |-> Length[ b ] > Length[ a ] && SubsetQ[ b, a ] ] ]
            ]
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
