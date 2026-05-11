Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineCore]
PackageScope[findLineExtensions]
PackageScope[findLineExtensionsWith]
PackageScope[findParallelCore]
PackageScope[findPerpendicularCore]
PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


(* InfraLine has no multi-realisation wrapper of its own: FindLine returns
   InfraSegment (a maximal geodesic is just a longest-form segment).  This
   file owns the line-shaped Find / construction / predicate operations.   *)


(* ===================== canonicalLine / allCanonicalLines ===================== *)

(* canonicalLine collapses the two orientations of a maximal geodesic to a
   single canonical vertex sequence (lexicographic minimum of the line and
   its reversal).  allCanonicalLines enumerates every canonical maximal
   geodesic in the graph.  Used by PencilDirections, FindCommonLine,
   ParallelQ, ProjectiveGeometry predicates.                                *)

canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine /@ FindLine[ graph, #[[ 1 ]], #[[ 2 ]], All ][ "Realizations" ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]


(* ===================== FindLine ===================== *)

(* A line through p1 and p2 is a maximal geodesic extension: a vertex
   sequence (a, ..., p1, ..., p2, ..., b) every contiguous sub-sequence of
   which is a geodesic and that cannot be extended at either end without
   breaking the geodesic property.  Method axis shared with FindSegment. *)

FindLine::badmethod = "Method `1` is not supported by FindLine.";
FindLine::nyi = "Pool `1` is not yet implemented for FindLine; use \"ShortestPaths\".";

Options[ FindLine ] = { "Maximality" -> "Extension", Method -> "Shortest" };

FindLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findLineCore[ graph, ##, count, opts ] &, p1, p2 ]


findLineCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindLine ] ] /;
    MemberQ[ VertexList[ graph ], p1 ] :=
  Module[ { spec, methodName, middles, allExtensions, diam, ranked },
    spec = OptionValue[ FindLine, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, Automatic -> "Shortest", _ :> spec } ];
    middles = Switch[ methodName,
      "Shortest" | "Embedding", allGeodesics[ graph, p1, p2 ],
      "ShortestPathExtension" | "CurvatureMinimizing",
        With[ { paths = findSegmentCore[ graph, p1, p2, All, Method -> spec ] },
          If[ paths === $Failed || ! ListQ[ paths ], { }, paths ]
        ],
      _, $Failed
    ];
    If[ middles === $Failed, Message[ FindLine::badmethod, spec ]; Return[ $Failed ] ];
    allExtensions = Union @ Flatten[ findLineExtensions[ graph, # ] & /@ middles, 1 ];
    If[ OptionValue[ FindLine, { opts }, "Maximality" ] === "Diameter",
      diam = GraphDiameter[ graph ];
      allExtensions = Select[ allExtensions, line |-> Length[ line ] - 1 == diam ]
    ];
    ranked = Switch[ methodName,
      "Shortest" | "ShortestPathExtension" | "CurvatureMinimizing", allExtensions,
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          If[ embOpts[ "Pool" ] =!= "ShortestPaths",
            Message[ FindLine::nyi, embOpts[ "Pool" ] ]; allExtensions,
            embeddingRankLines[ graph, allExtensions, p1, p2, embOpts ]
          ]
        ]
    ];
    Which[
      MatchQ[ count, _Integer ] && Length[ ranked ] < count, $Failed,
      MatchQ[ count, _Integer ],                              Take[ ranked, count ],
      MatchQ[ count, UpTo[ _Integer ] ],                      Take[ ranked, count ],
      count === All,                                          ranked,
      True,                                                   ranked
    ]
  ]

findLineCore[ _Graph, _, _, _, OptionsPattern[ FindLine ] ] := $Failed


embeddingRankLines[ graph_Graph, lines_List, p1_, p2_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, ep },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    ep = Lookup[ vertexIndex, { p1, p2 } ];
    SortBy[ lines,
      line |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, line ], ep ] ]
  ]


(* findLineExtensions / findLineExtensionsWith take a segment and return
   the maximal geodesic extensions through it.  The "With" form takes
   prefix and suffix path-enumeration closures; ExtendSegment swaps in
   extended-out / curvature-pulled enumerators for the non-Shortest
   methods.                                                              *)

findLineExtensions[ graph_Graph, segment_List ] :=
  findLineExtensionsWith[ graph, segment,
    { s, p, db } |-> FindPath[ graph, s, p, { db }, All ],
    { p, e, da } |-> FindPath[ graph, p, e, { da }, All ] ]


findLineExtensionsWith[ graph_Graph, segment_List, prefixFn_, suffixFn_ ] :=
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
          With[ { bp = If[ db == 0, { {} }, Most /@ prefixFn[ s, p1, db ] ],
                  ap = If[ da == 0, { {} }, Rest /@ suffixFn[ p2, e, da ] ] },
            Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ]
          ]
        ]
      ] & /@ maxPairs,
      1
    ]
  ]


(* ===================== FindParallel ===================== *)

(* FindParallel[g, line, p] constructs parallels to line through p.
   Method "Metric" (default): a parallel is the maximal sub-segment of a
   maximal geodesic through p whose vertices all lie at distance
   r = d(p, line) from line. *)

FindParallel::badmethod = "Method `1` is not supported by FindParallel.";

Options[ FindParallel ] = { Method -> "Metric" };

FindParallel[ graph_Graph, line_, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraSegment, count,
    findParallelCore[ graph, ##, count, opts ] &, line, p ]


findParallelCore[ graph_Graph, line_List, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindParallel ] ] :=
  Module[ { spec, methodName, parallels },
    spec = OptionValue[ FindParallel, { opts }, Method ];
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    parallels = Switch[ methodName,
      "Metric", findParallelMetric[ graph, line, p ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec, "LevelSet" ] },
          embeddingRankParallels[ graph, findParallelMetric[ graph, line, p ], line, p, embOpts ]
        ],
      _, Message[ FindParallel::badmethod, methodName ]; $Failed
    ];
    Which[
      parallels === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ parallels ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ parallels, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ parallels, count ],
      count === All,                       parallels,
      True,                                parallels
    ]
  ]


findParallelMetric[ graph_Graph, line_List, p_ ] :=
  Module[ { lineDist, r, levelSet, segments, dedup, maximalThrough },
    maximalThrough = { l, q, S } |-> With[
      { idx = First[ FirstPosition[ l, q, { 0 } ], 0 ] },
      If[ idx === 0, {},
        With[
          { lo = idx - LengthWhile[ Reverse @ Take[ l, idx - 1 ], MemberQ[ S, # ] & ],
            hi = idx + LengthWhile[ Drop[ l, idx ], MemberQ[ S, # ] & ] },
          l[[ lo ;; hi ]]
        ]
      ]
    ];
    lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ];
    r = lineDist[ p ];
    If[ r === Infinity, Return[ {} ] ];
    levelSet = Select[ VertexList[ graph ], lineDist[ # ] == r & ];
    segments = maximalThrough[ #, p, levelSet ] & /@ PencilDirections[ graph, p ];
    dedup = DeleteDuplicates[ canonicalLine /@ Select[ segments, Length[ # ] >= 2 & ] ];
    Select[ dedup,
      a |-> ! AnyTrue[ dedup, b |-> Length[ b ] > Length[ a ] && SubsetQ[ b, a ] ]
    ]
  ]


embeddingRankParallels[ graph_Graph, parallels_List, line_List, p_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, lineDir, pCoord, refStart, refEnd, augmented, n },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    lineDir = coords[[ vertexIndex[ Last[ line ] ] ]] - coords[[ vertexIndex[ First[ line ] ] ]];
    pCoord = coords[[ vertexIndex[ p ] ]];
    refStart = pCoord - lineDir / 2;
    refEnd   = pCoord + lineDir / 2;
    augmented = Append[ Append[ coords, refStart ], refEnd ];
    n = Length[ coords ];
    SortBy[ parallels,
      par |-> EmbeddingHausdorffDistance[ augmented,
        Lookup[ vertexIndex, par ], { n + 1, n + 2 } ] ]
  ]


(* ===================== FindPerpendicular ===================== *)

(* Foot of the perpendicular from point p to line L by Euclid I.12 (the
   isosceles base midpoint construction): for each pair {a, b} of line
   vertices equidistant from p, the midpoint of the line-arc between them
   is a candidate foot.  Multi-valued; the union of all such midpoints
   is returned. *)

FindPerpendicular::nyi = "Method `1` is not yet implemented for FindPerpendicular; only \"Metric\" is currently available.";
FindPerpendicular::badmethod = "Method `1` is not supported by FindPerpendicular.";

Options[ FindPerpendicular ] = { Method -> "Metric" };

FindPerpendicular[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    findPerpendicularCore[ graph, ##, count, opts ] &, line, point ]


findPerpendicularCore[ graph_Graph, line_List, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindPerpendicular ] ] :=
  Module[ { spec = OptionValue[ FindPerpendicular, { opts }, Method ], methodName, distances, byIndex, feet },
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    feet = Switch[ methodName,
      "Metric",
        distances = GraphDistance[ graph, point, # ] & /@ line;
        byIndex = Values @ GroupBy[ Range @ Length @ line, distances[[ # ]] & ];
        Union @ Flatten @ Table[
          Table[
            With[ { lo = Min[ pair[[ 1 ]], pair[[ 2 ]] ], hi = Max[ pair[[ 1 ]], pair[[ 2 ]] ] },
              If[ OddQ[ hi - lo ],
                line[[ lo ;; hi ]][[ Ceiling[ ( hi - lo + 1 ) / 2 ] ]],
                Nothing
              ]
            ],
            { pair, Subsets[ group, { 2 } ] }
          ],
          { group, byIndex }
        ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          embeddingRankPerpendicularFeet[ graph, line, point, embOpts ]
        ],
      "Spectral" | "Resistance", Message[ FindPerpendicular::nyi, methodName ]; $Failed,
      _, Message[ FindPerpendicular::badmethod, methodName ]; $Failed
    ];
    Which[
      feet === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ feet ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ feet, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ feet, count ],
      count === All,                       feet,
      True,                                feet
    ]
  ]


embeddingRankPerpendicularFeet[ graph_Graph, line_List, point_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, lineStart, lineEnd, dir, dirNorm, pointCoord, foot },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    lineStart = coords[[ vertexIndex[ First[ line ] ] ]];
    lineEnd   = coords[[ vertexIndex[ Last[ line ] ] ]];
    dir = lineEnd - lineStart;
    dirNorm = dir . dir;
    pointCoord = coords[[ vertexIndex[ point ] ]];
    foot = If[ dirNorm == 0, lineStart,
      lineStart + ( ( pointCoord - lineStart ) . dir / dirNorm ) * dir ];
    SortBy[ line,
      v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], foot ] ]
  ]


(* ===================== FindCommonLine ===================== *)

(* Lines containing every vertex in the input list -- canonical maximal
   geodesics through the first two listed vertices that also pass through
   every other listed vertex.  Constructive companion of CollinearQ.    *)

findCommonLineCore[ graph_Graph, verts_List ] :=
  Module[ { uverts, candidates },
    uverts = DeleteDuplicates @ Catenate[ infraUnionSpread /@ verts ];
    If[ Length[ uverts ] < 2, Return[ {} ] ];
    candidates = canonicalLine /@ FindLine[ graph, First @ uverts, uverts[[ 2 ]], All ][ "Realizations" ];
    DeleteDuplicates @ Select[ candidates, line |-> SubsetQ[ line, uverts ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, All ] :=
  InfraSegment[ findCommonLineCore[ graph, verts ] ]

FindCommonLine[ graph_Graph, verts_List, UpTo[ n_Integer ] ] :=
  With[ { result = FindCommonLine[ graph, verts, All ] },
    InfraSegment[ Take[ result[ "Realizations" ], UpTo[ n ] ] ]
  ]

FindCommonLine[ graph_Graph, verts_List, n_Integer : 1 ] :=
  With[ { result = FindCommonLine[ graph, verts, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


(* ===================== SegmentLineAngle ===================== *)

(* Length-valued surrogate for the angle between segment p1 p2 and a line
   L containing p1: returns d(p2, L) when p1 lies on L, Infinity otherwise. *)

SegmentLineAngle::nyi = "Method `1` is not yet implemented for SegmentLineAngle; only \"Metric\" is currently available.";
SegmentLineAngle::badmethod = "Method `1` is not supported by SegmentLineAngle.";

Options[ SegmentLineAngle ] = { Method -> "Metric" };

SegmentLineAngle[ graph_Graph, p1_, p2_, line_List, opts : OptionsPattern[] ] :=
  Module[ { method = OptionValue[ Method ] },
    Switch[ method,
      "Metric", If[ ! MemberQ[ line, p1 ], Infinity,
        Min[ GraphDistance[ graph, p2, # ] & /@ line ] ],
      "Spectral", Message[ SegmentLineAngle::nyi, method ]; $Failed,
      _, Message[ SegmentLineAngle::badmethod, method ]; $Failed
    ]
  ]

SegmentLineAngle[ graph_Graph, segment_List, line_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  SegmentLineAngle[ graph, First[ segment ], Last[ segment ], line, opts ]


(* ===================== LineQ ===================== *)

(* LineQ tests maximality: a segment is a line iff no extension preserves
   the geodesic property (findLineExtensions returns the segment itself). *)

LineQ[ graph_Graph, segment_List ] :=
  SegmentQ[ graph, segment ] &&
  Length[ First @ findLineExtensions[ graph, segment ] ] == Length[ segment ]


(* ===================== ParallelQ ===================== *)

(* ParallelQ tests definition-alpha parallelism: l1 and l2 are parallel iff
   they are disjoint and the distance from each vertex of l1 to l2 is
   constant (up to threshold). *)

ParallelQ[ distanceMatrix_List, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Min[ distanceMatrix[[ #, l2 ]] ] & /@ l1 },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold
    ]
  ]

ParallelQ[ graph_Graph, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Table[ Min[ GraphDistance[ graph, v, # ] & /@ l2 ], { v, l1 } ] },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold
    ]
  ]


(* ===================== PencilDirections / PencilCardinality / LineCount ===================== *)

(* PencilDirections lists the canonical maximal geodesics through origin
   - one per projective direction class at that vertex.  PencilCardinality
   counts them.  LineCount is the projective-incidence "number of lines"
   in the graph (canonical maximal geodesics overall).                    *)

PencilDirections[ graph_Graph, origin_ ] :=
  DeleteDuplicates @ Map[ canonicalLine, Flatten[
    FindLine[ graph, origin, #, All ][ "Realizations" ] & /@
      DeleteCases[ VertexList[ graph ], origin ],
    1 ] ]

PencilCardinality[ graph_Graph, origin_ ] := Length @ PencilDirections[ graph, origin ]

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]
