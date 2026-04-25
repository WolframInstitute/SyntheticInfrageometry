Package["WolframInstitute`SyntheticInfrageometry`"]

$InfraPointColor = RGBColor[ 0.90, 0.10, 0.15 ];
$InfraSegmentColor = RGBColor[ 0.10, 0.30, 0.90 ];
$InfraCircleColor = RGBColor[ 0.00, 0.60, 0.25 ];

$InfraSegmentSelectOptions = { None, "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraCircleSelectOptions = { None, "ShortestCircumference", "LongestCircumference",
  "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

instancePalette[ baseColor_, n_Integer ] :=
  If[ n <= 1, { baseColor },
    Table[ ColorData[ "DarkRainbow" ][ (i - 1) / (n - 1) ], { i, n } ]
  ]


SetAttributes[ PointViewer, HoldRest ]

PointViewer[ g_Graph, sym_: None ] :=
  With[ { ptColor = $InfraPointColor, diam = GraphDiameter[ g ] },
    Manipulate[
      seed;
      With[{
        pts = FindPoint[ g, UpTo[ n ], "From" -> from, "MaxCliques" -> 100,
          "Distance" -> Switch[ separation, "None", None, "Max", "Max", "Range", distRange ] ]
      },
        If[ sym =!= None, sym = pts ];
        HighlightGraph[ g,
          Style[ #, Directive[ ptColor, AbsolutePointSize[ 12 ] ] ] & /@ pts,
          ImageSize -> 600
        ]
      ],
      Grid[ {
        { Control[ { { n, 1, "Points" }, ControlType -> InputField } ],
          Control[ { { from, "Random", "From" }, { "Random", "Center", "Periphery" } } ] },
        { Control[ { { separation, "None", "Separation" }, { "None", "Max", "Range" } } ],
          Control[ { { distRange, { 0, diam }, "Distance" }, 0, diam, 1,
            ControlType -> IntervalSlider, Enabled -> Dynamic[ separation === "Range" ] } ] }
      }, Alignment -> Center, ItemSize -> { { Scaled[ 0.5 ], Scaled[ 0.5 ] } } ],
      { { seed, 0 }, None },
      Button[ "Resample", seed++ ],
      TrackedSymbols :> { seed, n, from, separation, distRange },
      SaveDefinitions -> True
    ]
  ]


SegmentViewer[ g_Graph ] :=
  With[{
    initPts = RandomSample[ VertexList[ g ], 2 ],
    nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
    ptColor = $InfraPointColor,
    segColor = $InfraSegmentColor,
    selOpts = $InfraSegmentSelectOptions
  },
    Manipulate[
      seed;
      Module[ { segments, pathEdges, colors, vertexHighlights, edgeHighlights, highlights },
        segments = If[ p1 === p2 || GraphDistance[ g, p1, p2 ] === Infinity,
          {},
          FindSegment[ g, p1, p2, n, "Select" -> sel ]
        ];
        pathEdges = (UndirectedEdge @@@ Partition[ #, 2, 1 ]) & /@ segments;
        colors = instancePalette[ segColor, Length[ segments ] ];
        vertexHighlights = Join[
          { Style[ p1, Directive[ ptColor, AbsolutePointSize[ 16 ] ] ],
            Style[ p2, Directive[ ptColor, AbsolutePointSize[ 16 ] ] ] },
          Style[ #, Directive[ Darker[ segColor, 0.2 ], AbsolutePointSize[ 9 ] ] ] & /@
            DeleteDuplicates[ Flatten[ segments ] ]
        ];
        edgeHighlights = MapThread[
          Style[ #1, Directive[ AbsoluteThickness[ 4 ], #2 ] ] &,
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
      { { n, 1, "Segments" }, 1, 12, 1, Appearance -> "Labeled" },
      { { sel, None, "Select (ambiguity resolver)" }, selOpts, ControlType -> SetterBar },
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
    circColor = $InfraCircleColor,
    ptColor = $InfraPointColor,
    selOpts = $InfraCircleSelectOptions,
    diam = Max[ GraphDiameter[ g ], 2 ]
  },
    Manipulate[
      seed;
      Module[ { circles, cycleEdges, colors, vertexHighlights, edgeHighlights, highlights },
        circles = If[ r < 1,
          {},
          FindCircle[ g, p, r, n, "Select" -> sel ]
        ];
        cycleEdges = (UndirectedEdge @@@ Partition[ Append[ #, First[ # ] ], 2, 1 ]) & /@ circles;
        colors = instancePalette[ circColor, Length[ circles ] ];
        vertexHighlights = Join[
          { Style[ p, Directive[ ptColor, AbsolutePointSize[ 16 ] ] ] },
          Style[ #, Directive[ Darker[ circColor, 0.2 ], AbsolutePointSize[ 9 ] ] ] & /@
            DeleteDuplicates[ Flatten[ circles ] ]
        ];
        edgeHighlights = MapThread[
          Style[ #1, Directive[ AbsoluteThickness[ 4 ], #2 ] ] &,
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
      { { r, Max[ 1, Round[ diam / 3 ] ], "Radius" }, 1, diam, 1, Appearance -> "Labeled" },
      { { n, 1, "Circles" }, 1, 12, 1, Appearance -> "Labeled" },
      { { sel, None, "Select (ambiguity resolver)" }, selOpts, ControlType -> SetterBar },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, sel },
      SaveDefinitions -> True
    ]
  ]
