Package["WolframInstitute`SyntheticInfrageometry`"]


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

FindMidpoint[ graph_Graph, segment_List, opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  Module[ { method = OptionValue[ Method ] },
    Switch[ method,
      "Metric", segment[[ Ceiling[ Length[ segment ] / 2 ] ]],
      "Spectral" | "Resistance", Message[ FindMidpoint::nyi, method ]; $Failed,
      _, Message[ FindMidpoint::badmethod, method ]; $Failed
    ]
  ]

FindMidpoint[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] :=
  Module[ { method = OptionValue[ Method ], segs },
    Switch[ method,
      "Metric",
        segs = allGeodesics[ graph, p1, p2 ];
        If[ segs === {}, {},
          DeleteDuplicates[ #[[ Ceiling[ Length[ # ] / 2 ] ]] & /@ segs ]
        ],
      "Spectral" | "Resistance", Message[ FindMidpoint::nyi, method ]; $Failed,
      _, Message[ FindMidpoint::badmethod, method ]; $Failed
    ]
  ]

FindMidpoint[ graph_Graph, p1_, p2_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  With[ { result = FindMidpoint[ graph, p1, p2, All, opts ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

FindMidpoint[ graph_Graph, p1_, p2_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindMidpoint[ graph, p1, p2, UpTo[ n ], opts ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
  ]


(* ===================== FindPerpendicular ===================== *)

(* Foot of the perpendicular from point p to line L by Euclid I.12 (the
   isosceles base midpoint construction): for each pair {a, b} of line
   vertices equidistant from p, the midpoint of the line-arc between them
   is a candidate foot.  Multi-valued; the union of all such midpoints
   is returned. *)

Options[ FindPerpendicular ] = { Method -> "Metric" };

FindPerpendicular[ graph_Graph, line_List, point_, All, opts : OptionsPattern[] ] :=
  Module[ { method = OptionValue[ Method ], distances, byIndex, feet },
    Switch[ method,
      "Metric",
        distances = GraphDistance[ graph, point, # ] & /@ line;
        byIndex = Values @ GroupBy[ Range @ Length @ line, distances[[ # ]] & ];
        feet = Union @ Flatten @ Table[
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
        ];
        feet,
      "Spectral" | "Resistance", Message[ FindPerpendicular::nyi, method ]; $Failed,
      _, Message[ FindPerpendicular::badmethod, method ]; $Failed
    ]
  ]

FindPerpendicular[ graph_Graph, line_List, point_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  With[ { result = FindPerpendicular[ graph, line, point, All, opts ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

FindPerpendicular[ graph_Graph, line_List, point_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindPerpendicular[ graph, line, point, UpTo[ n ], opts ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
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

FindBisectingHyperplane[ graph_Graph, { p1_, p2_ }, args___ ] :=
  FindBisectingHyperplane[ graph, p1, p2, args ]

FindBisectingHyperplane[ graph_Graph, p1 : Except[_List], p2 : Except[_List] ] :=
  FindBisectingHyperplane[ graph, p1, p2, { 0, 0 }, 1 ]

FindBisectingHyperplane[ graph_Graph, p1 : Except[_List], p2 : Except[_List],
    count : (_Integer | UpTo[_Integer] | All) ] :=
  FindBisectingHyperplane[ graph, p1, p2, { 0, 0 }, count ]

FindBisectingHyperplane[ graph_Graph, p1 : Except[_List], p2 : Except[_List],
    window : { _Integer, _Integer } ] :=
  FindBisectingHyperplane[ graph, p1, p2, window, 1 ]

FindBisectingHyperplane[ graph_Graph, p1 : Except[_List], p2 : Except[_List],
    { lo_Integer, hi_Integer }, All ] :=
  Module[ { bisector },
    bisector = Pick[ VertexList[ graph ],
      MapThread[ { x, y } |-> lo <= x - y <= hi,
        { GraphDistance[ graph, p1 ], GraphDistance[ graph, p2 ] } ] ];
    FindPairSeparators[ graph, Complement[ bisector, { p1, p2 } ], p1, p2 ]
  ]

FindBisectingHyperplane[ graph_Graph, p1 : Except[_List], p2 : Except[_List],
    { lo_Integer, hi_Integer }, UpTo[ n_Integer ] ] :=
  Take[ FindBisectingHyperplane[ graph, p1, p2, { lo, hi }, All ], UpTo[ n ] ]

FindBisectingHyperplane[ graph_Graph, p1 : Except[_List], p2 : Except[_List],
    { lo_Integer, hi_Integer }, n_Integer ] :=
  With[ { result = FindBisectingHyperplane[ graph, p1, p2, { lo, hi }, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== CompleteEquilateralTriangle ===================== *)

(* Apex of an equilateral triangle on segment p1 p2 (Euclid I.1): the
   intersection of the spheres of radius d(p1, p2) around p1 and p2 -
   vertices c with d(p1, c) == d(p2, c) == d(p1, p2). *)

Options[ CompleteEquilateralTriangle ] = { Method -> "Metric" };

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_, All, opts : OptionsPattern[] ] :=
  Module[ { method = OptionValue[ Method ], r },
    Switch[ method,
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
    ]
  ]

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  With[ { result = CompleteEquilateralTriangle[ graph, p1, p2, All, opts ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = CompleteEquilateralTriangle[ graph, p1, p2, UpTo[ n ], opts ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
  ]


(* ===================== GraphAngle ===================== *)

(* GraphAngle[q1, p, q2] removes the open ball around p of radius
   Min[d(p, q1), d(p, q2)], then measures how far q1 and q2 are forced
   to travel outside that neighborhood, normalized by that radius. *)

GraphAngle[ graph_Graph, { q1_, p_, q2_ } ] :=
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
