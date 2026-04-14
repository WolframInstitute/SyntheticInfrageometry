Package["WolframInstitute`SyntheticInfrageometry`"]

$InfraPointColor = RGBColor[ 0.93, 0.50, 0.50 ];
$InfraSegmentColor = RGBColor[ 0.50, 0.60, 0.93 ];
$InfraCircleColor = RGBColor[ 0.50, 0.86, 0.62 ];


SetAttributes[ PointViewer, HoldRest ]

PointViewer[ g_Graph, sym_: None ] :=
  With[ { ptColor = $InfraPointColor },
    Manipulate[
      seed;
      With[{
        pts = FindPoint[ g, n, "From" -> from, "MaxCliques" -> 100, "Distance" -> Which[
          useMax, "Max",
          minD > 0 && maxD > 0, { minD, maxD },
          minD > 0, minD,
          maxD > 0, { 1, maxD },
          True, None
        ] ]
      },
        If[ sym =!= None, sym = pts ];
        HighlightGraph[ g,
          Style[ #, Directive[ ptColor, AbsolutePointSize[ 12 ] ] ] & /@ pts,
          ImageSize -> 600
        ]
      ],
      { { n, 1, "Number of points" }, ControlType -> InputField },
      { { from, "Random", "From" }, { "Random", "Center", "Periphery" } },
      Delimiter,
      { { useMax, False, "Max separation" }, { True, False } },
      { { minD, 0, "Min distance" }, ControlType -> InputField },
      { { maxD, 0, "Max distance" }, ControlType -> InputField },
      { { seed, 0 }, None },
      Button[ "Resample", seed++ ],
      TrackedSymbols :> { seed, n, from, useMax, minD, maxD },
      SaveDefinitions -> True
    ]
  ]


SegmentViewer[ g_Graph ] :=
  With[{
    initPts = RandomSample[ VertexList[ g ], 2 ],
    nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
    ptColor = $InfraPointColor,
    segColor = $InfraSegmentColor
  },
    Manipulate[
      seed;
      Module[ { segments, pathEdges, colors, vertexHighlights, edgeHighlights, highlights },
        segments = If[ p1 === p2 || GraphDistance[ g, p1, p2 ] === Infinity,
          {},
          FindSegment[ g, p1, p2, n, "Select" -> sel ]
        ];
        pathEdges = (UndirectedEdge @@@ Partition[ #, 2, 1 ]) & /@ segments;
        colors = Table[
          If[ Length[ segments ] == 1,
            segColor,
            Blend[ { Lighter[ segColor, 0.3 ], Darker[ segColor, 0.3 ] },
              (i - 1) / Max[ Length[ segments ] - 1, 1 ] ]
          ],
          { i, Length[ segments ] }
        ];
        vertexHighlights = Join[
          { Style[ p1, Directive[ ptColor, AbsolutePointSize[ 14 ] ] ],
            Style[ p2, Directive[ ptColor, AbsolutePointSize[ 14 ] ] ] },
          Style[ #, Directive[ segColor, AbsolutePointSize[ 10 ] ] ] & /@
            DeleteDuplicates[ Flatten[ segments ] ]
        ];
        edgeHighlights = MapThread[
          Style[ #1, Directive[ AbsoluteThickness[ 3 ], #2 ] ] &,
          { pathEdges, colors }
        ];
        highlights = Join[ edgeHighlights, vertexHighlights ];
        EventHandler[
          HighlightGraph[ g, highlights, ImageSize -> 600 ],
          { "MouseClicked" :> With[ { mp = MousePosition[ "Graphics" ] },
            If[ mp =!= None,
              With[ { clicked = First @ nearestFunc[ mp ] },
                p1 = p2; p2 = clicked; seed++
              ]
            ]
          ] },
          PassEventsDown -> True
        ]
      ],
      { { p1, initPts[[ 1 ]] }, None },
      { { p2, initPts[[ 2 ]] }, None },
      { { seed, 0 }, None },
      { { sel, None, "Select" }, ControlType -> InputField },
      { { n, 1, "Segments" }, ControlType -> InputField },
      Button[ "Resample", With[ { pts = RandomSample[ VertexList[ g ], 2 ] },
        p1 = pts[[ 1 ]]; p2 = pts[[ 2 ]]; seed++ ] ],
      TrackedSymbols :> { p1, p2, seed, n, sel },
      SaveDefinitions -> True
    ]
  ]


CircleViewer[ g_Graph ] :=
  With[{
    initPt = RandomChoice[ VertexList[ g ] ],
    nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
    circColor = $InfraCircleColor
  },
    Manipulate[
      seed;
      Module[ { circles, cycleEdges, colors, vertexHighlights, edgeHighlights, highlights },
        circles = If[ r < 1,
          {},
          FindCircle[ g, p, r, n, "Select" -> sel ]
        ];
        cycleEdges = (UndirectedEdge @@@ Partition[ Append[ #, First[ # ] ], 2, 1 ]) & /@ circles;
        colors = Table[
          If[ Length[ circles ] == 1,
            circColor,
            Blend[ { Lighter[ circColor, 0.3 ], Darker[ circColor, 0.3 ] },
              (i - 1) / Max[ Length[ circles ] - 1, 1 ] ]
          ],
          { i, Length[ circles ] }
        ];
        vertexHighlights = Style[ #, Directive[ circColor, AbsolutePointSize[ 10 ] ] ] & /@
          DeleteDuplicates[ Flatten[ circles ] ];
        edgeHighlights = MapThread[
          Style[ #1, Directive[ AbsoluteThickness[ 3 ], #2 ] ] &,
          { cycleEdges, colors }
        ];
        highlights = Join[ edgeHighlights, vertexHighlights ];
        EventHandler[
          HighlightGraph[ g, highlights, ImageSize -> 600 ],
          { "MouseClicked" :> With[ { mp = MousePosition[ "Graphics" ] },
            If[ mp =!= None,
              With[ { clicked = First @ nearestFunc[ mp ] },
                p = clicked; seed++
              ]
            ]
          ] },
          PassEventsDown -> True
        ]
      ],
      { { p, initPt }, None },
      { { seed, 0 }, None },
      { { r, 3, "Radius" }, ControlType -> InputField },
      { { n, 1, "Circles" }, ControlType -> InputField },
      { { sel, None, "Select" }, ControlType -> InputField },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, sel },
      SaveDefinitions -> True
    ]
  ]
