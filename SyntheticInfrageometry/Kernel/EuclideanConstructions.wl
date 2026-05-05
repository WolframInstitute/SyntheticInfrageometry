Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findMidpointCore]
PackageScope[findPerpendicularCore]
PackageScope[findBisectingHyperplaneCore]
PackageScope[completeEquilateralTriangleCore]


(* ===================== Messages ===================== *)

FindMidpoint::nyi = "Method `1` is not yet implemented for FindMidpoint; only \"Metric\" is currently available.";
FindMidpoint::badmethod = "Method `1` is not supported by FindMidpoint.";
FindPerpendicular::nyi = "Method `1` is not yet implemented for FindPerpendicular; only \"Metric\" is currently available.";
FindPerpendicular::badmethod = "Method `1` is not supported by FindPerpendicular.";
CompleteEquilateralTriangle::nyi = "Method `1` is not yet implemented for CompleteEquilateralTriangle; only \"Metric\" is currently available.";
CompleteEquilateralTriangle::badmethod = "Method `1` is not supported by CompleteEquilateralTriangle.";
SegmentLineAngle::nyi = "Method `1` is not yet implemented for SegmentLineAngle; only \"Metric\" is currently available.";
SegmentLineAngle::badmethod = "Method `1` is not supported by SegmentLineAngle.";


(* ===================== FindMidpoint ===================== *)

(* The midpoint of a segment of length k is the central vertex
   (Ceiling[(k+1)/2]).  FindMidpoint[g, p1, p2, *] collects midpoints
   across every geodesic from p1 to p2 (multi-valued in general). *)

Options[ FindMidpoint ] = { Method -> "Metric" };

(* FindMidpoint[g, segment] returns the midpoint vertex of an explicit
   segment as a wrapped InfraPoint singleton (no count form for this
   shape).  FindMidpoint[g, p1, p2, count, opts] dispatches over multi-
   realisation endpoints with Cartesian + per-pair-count + union. *)

FindMidpoint[ graph_Graph, segment_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  Module[ { spec = OptionValue[ Method ], methodName },
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    Switch[ methodName,
      "Metric",   InfraPoint[ { segment[[ Ceiling[ Length[ segment ] / 2 ] ]] } ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          InfraPoint[ { First @ embeddingRankMidpoints[ graph, First[ segment ], Last[ segment ], embOpts ] } ]
        ],
      "Spectral" | "Resistance", Message[ FindMidpoint::nyi, methodName ]; $Failed,
      _, Message[ FindMidpoint::badmethod, methodName ]; $Failed
    ]
  ]

FindMidpoint[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    findMidpointCore[ graph, ##, count, opts ] &, p1, p2 ]


findMidpointCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ FindMidpoint ] ] :=
  Module[ { spec = OptionValue[ FindMidpoint, { opts }, Method ], methodName, segs, midpoints },
    methodName = Replace[ spec, { m_String :> m, { m_String, ___ } :> m, _ :> spec } ];
    midpoints = Switch[ methodName,
      "Metric",
        segs = allGeodesics[ graph, p1, p2 ];
        If[ segs === {}, {},
          DeleteDuplicates[ #[[ Ceiling[ Length[ # ] / 2 ] ]] & /@ segs ]
        ],
      "Embedding",
        With[ { embOpts = parseEmbeddingMethod[ spec ] },
          embeddingRankMidpoints[ graph, p1, p2, embOpts ]
        ],
      "Spectral" | "Resistance", Message[ FindMidpoint::nyi, methodName ]; $Failed,
      _, Message[ FindMidpoint::badmethod, methodName ]; $Failed
    ];
    Which[
      midpoints === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ midpoints ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ midpoints, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ midpoints, count ],
      count === All,                       midpoints,
      True,                                midpoints
    ]
  ]


(* embeddingRankMidpoints sorts vertices by their Euclidean distance to
   (coord(p1) + coord(p2)) / 2.  Pool "ShortestPaths" ranks only the
   metric interval { w : d(p1, w) + d(w, p2) == d(p1, p2) } -- vertices
   that lie on at least one geodesic; "AllPaths" ranks every vertex. *)

embeddingRankMidpoints[ graph_Graph, p1_, p2_, embOpts_Association ] :=
  Module[ { coords, vertexIndex, target, pool, total },
    coords = resolveEmbeddingCoords[ graph, embOpts[ "Coordinates" ] ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    target = ( coords[[ vertexIndex[ p1 ] ]] + coords[[ vertexIndex[ p2 ] ]] ) / 2;
    pool = If[ embOpts[ "Pool" ] === "AllPaths", VertexList[ graph ],
      total = GraphDistance[ graph, p1, p2 ];
      Select[ VertexList[ graph ],
        GraphDistance[ graph, p1, # ] + GraphDistance[ graph, #, p2 ] == total & ]
    ];
    SortBy[ pool,
      v |-> EuclideanDistance[ coords[[ vertexIndex[ v ] ]], target ] ]
  ]


(* ===================== FindPerpendicular ===================== *)

(* Foot of the perpendicular from point p to line L by Euclid I.12 (the
   isosceles base midpoint construction): for each pair {a, b} of line
   vertices equidistant from p, the midpoint of the line-arc between them
   is a candidate foot.  Multi-valued; the union of all such midpoints
   is returned. *)

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


(* embeddingRankPerpendicularFeet sorts line vertices by their proximity to
   the foot of the Euclidean perpendicular dropped from coord(point) onto
   the polyline of `line` under the embedding -- i.e. each line vertex's
   distance from the projection of point onto the affine line through
   coord(line[[1]]) -> coord(line[[-1]]). *)

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


(* ===================== FindBisectingHyperplane ===================== *)

(* A bisecting hyperplane between p1 and p2 is an inclusion-minimal vertex
   subset of the (windowed) bisector { v : lo <= d(p1, v) - d(p2, v) <= hi }
   whose removal disconnects p1 from p2 in graph -- the codim-1 graph
   analog of the perpendicular bisector hyperplane.  Multiple such
   hyperplanes can coexist (e.g. the four cuts of a thickened bisector
   on an even cycle).  The default window {0, 0} is the strict equidistant
   set; passing {-1, 1} thickens it to recover the parity-stranded middle
   pair when d(p1, p2) is odd.  Returns a list of vertex sets (one per
   hyperplane); n / UpTo[n] / All control how many.  Returns $Failed
   when fewer than n are available; {} when none exist (under All). *)

FindBisectingHyperplane[ graph_Graph, p1_, p2_ ] :=
  FindBisectingHyperplane[ graph, p1, p2, { 0, 0 }, 1 ]

FindBisectingHyperplane[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) ] :=
  FindBisectingHyperplane[ graph, p1, p2, { 0, 0 }, count ]

FindBisectingHyperplane[ graph_Graph, p1_, p2_,
    window : { _Integer, _Integer } ] :=
  FindBisectingHyperplane[ graph, p1, p2, window, 1 ]

FindBisectingHyperplane[ graph_Graph, p1_, p2_,
    window : { _Integer, _Integer }, count : ( _Integer | UpTo[ _Integer ] | All ) ] :=
  infraSpreadAndCartesian[ InfraPlane, count,
    findBisectingHyperplaneCore[ graph, ##, window, count ] &, p1, p2 ]


findBisectingHyperplaneCore[ graph_Graph, p1_, p2_,
    { lo_Integer, hi_Integer }, count : ( _Integer | UpTo[ _Integer ] | All ) ] :=
  Module[ { bisector, hyperplanes },
    bisector = Pick[ VertexList[ graph ],
      MapThread[ { x, y } |-> lo <= x - y <= hi,
        { GraphDistance[ graph, p1 ], GraphDistance[ graph, p2 ] } ] ];
    hyperplanes = FindPairSeparators[ graph, Complement[ bisector, { p1, p2 } ], p1, p2 ];
    Which[
      MatchQ[ count, _Integer ] && Length[ hyperplanes ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ hyperplanes, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ hyperplanes, count ],
      count === All,                       hyperplanes,
      True,                                hyperplanes
    ]
  ]


(* ===================== CompleteEquilateralTriangle ===================== *)

(* Apex of an equilateral triangle on segment p1 p2 (Euclid I.1): the
   intersection of the spheres of radius d(p1, p2) around p1 and p2 -
   vertices c with d(p1, c) == d(p2, c) == d(p1, p2). *)

Options[ CompleteEquilateralTriangle ] = { Method -> "Metric" };

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    completeEquilateralTriangleCore[ graph, ##, count, opts ] &, p1, p2 ]


completeEquilateralTriangleCore[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ CompleteEquilateralTriangle ] ] :=
  Module[ { method = OptionValue[ CompleteEquilateralTriangle, { opts }, Method ], r, apexes },
    apexes = Switch[ method,
      "Metric",
        r = GraphDistance[ graph, p1, p2 ];
        If[ r === Infinity, {},
          Intersection[
            Select[ VertexList[ graph ], GraphDistance[ graph, p1, # ] == r & ],
            Select[ VertexList[ graph ], GraphDistance[ graph, p2, # ] == r & ]
          ]
        ],
      "Spectral" | "Resistance", Message[ CompleteEquilateralTriangle::nyi, method ]; $Failed,
      _, Message[ CompleteEquilateralTriangle::badmethod, method ]; $Failed
    ];
    Which[
      apexes === $Failed, $Failed,
      MatchQ[ count, _Integer ] && Length[ apexes ] < count, $Failed,
      MatchQ[ count, _Integer ],          Take[ apexes, count ],
      MatchQ[ count, UpTo[ _Integer ] ],  Take[ apexes, count ],
      count === All,                       apexes,
      True,                                apexes
    ]
  ]


(* ===================== InfraAngle ===================== *)

(* InfraAngle[q1, p, q2] removes the open ball around p of radius
   Min[d(p, q1), d(p, q2)], then measures how far q1 and q2 are forced
   to travel outside that neighborhood, normalized by that radius. *)

InfraAngle[ graph_Graph, { q1_, p_, q2_ } ] :=
  Module[ { radius, rem },
    radius = Min[ GraphDistance[ graph, p, q1 ], GraphDistance[ graph, p, q2 ] ];
    rem = VertexDelete[ graph,
      Select[ VertexList[ graph ], GraphDistance[ graph, p, # ] < radius & ]
    ];
    GraphDistance[ rem, q1, q2 ] / radius
  ]


(* ===================== SegmentLineAngle ===================== *)

(* Length-valued surrogate for the angle between segment p1 p2 and a line
   L containing p1: returns d(p2, L) when p1 lies on L, Infinity otherwise.
   Name is historical - the value is a length, not a normalised angle. *)

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
