Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[$InfraPointColor]
PackageScope[$InfraSegmentColor]
PackageScope[$InfraSphereColor]


$InfraPointColor = RGBColor[ 0.90, 0.10, 0.15 ];
$InfraSegmentColor = RGBColor[ 0.10, 0.30, 0.90 ];
$InfraSphereColor = RGBColor[ 0.00, 0.60, 0.25 ];

$InfraSegmentSelectOptions = { None, "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraSphereSelectOptions = { None, "ShortestCircumference", "LongestCircumference",
  "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

instancePalette[ baseColor_, n_Integer ] :=
  If[ n <= 1, { baseColor },
    Table[ ColorData[ "DarkRainbow" ][ (i - 1) / (n - 1) ], { i, n } ]
  ]


Options[ InfraDiffuseHighlight ] = {
  "Cyclic" -> False,
  "OpacityRange" -> { 0.25, 1.0 },
  "ThicknessRange" -> { 1.0, 5.0 },
  "PointSizeRange" -> { 6, 14 }
};

InfraDiffuseHighlight[ graph_Graph, candidates_List, opts : OptionsPattern[] ] :=
  InfraDiffuseHighlight[ graph, candidates, $InfraSegmentColor, opts ]

InfraDiffuseHighlight[ graph_Graph, candidates_List, color_, opts : OptionsPattern[] ] :=
  Module[ {
    cyclic = TrueQ @ OptionValue[ "Cyclic" ],
    oRange = OptionValue[ "OpacityRange" ],
    tRange = OptionValue[ "ThicknessRange" ],
    pRange = OptionValue[ "PointSizeRange" ],
    flat, vertexCounts, edgeCounts, vMax, eMax, vSpecs, eSpecs },
    flat = Replace[ candidates, x : Except[ _List ] :> { x }, { 1 } ];
    vertexCounts = Counts @ Flatten[ flat ];
    edgeCounts = Counts @ Flatten[
      Map[
        path |-> If[ Length[ path ] < 2, {},
          Sort /@ Partition[ If[ cyclic && Length[ path ] >= 2, Append[ path, First[ path ] ], path ], 2, 1 ] ],
        flat ], 1 ];
    vMax = Max[ 1, Max @ Append[ Values @ vertexCounts, 1 ] ];
    eMax = Max[ 1, Max @ Append[ Values @ edgeCounts, 1 ] ];
    vSpecs = KeyValueMap[
      { v, k } |-> Style[ v, Directive[ color,
        Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) k / vMax ],
        AbsolutePointSize[ pRange[[ 1 ]] + ( pRange[[ 2 ]] - pRange[[ 1 ]] ) k / vMax ] ] ],
      vertexCounts ];
    eSpecs = KeyValueMap[
      { e, k } |-> Style[ UndirectedEdge @@ e, Directive[ color,
        Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) k / eMax ],
        AbsoluteThickness[ tRange[[ 1 ]] + ( tRange[[ 2 ]] - tRange[[ 1 ]] ) k / eMax ] ] ],
      edgeCounts ];
    HighlightGraph[ graph, Join[ eSpecs, vSpecs ] ]
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
          FindSegment[ g, p1, p2, UpTo[ n ], "Select" -> sel ]
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


SphereViewer[ g_Graph ] :=
  With[{
    initPt = RandomChoice[ VertexList[ g ] ],
    nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
    sphColor = $InfraSphereColor,
    ptColor = $InfraPointColor,
    selOpts = $InfraSphereSelectOptions,
    diam = Max[ GraphDiameter[ g ], 2 ]
  },
    Manipulate[
      seed;
      Module[ { spheres, cycleEdges, colors, vertexHighlights, edgeHighlights, highlights },
        spheres = If[ r < 1,
          {},
          FindSphere[ g, p, r, UpTo[ n ], "Select" -> sel ]
        ];
        cycleEdges = (UndirectedEdge @@@ Partition[ Append[ #, First[ # ] ], 2, 1 ]) & /@ spheres;
        colors = instancePalette[ sphColor, Length[ spheres ] ];
        vertexHighlights = Join[
          { Style[ p, Directive[ ptColor, AbsolutePointSize[ 16 ] ] ] },
          Style[ #, Directive[ Darker[ sphColor, 0.2 ], AbsolutePointSize[ 9 ] ] ] & /@
            DeleteDuplicates[ Flatten[ spheres ] ]
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
      { { n, 1, "Spheres" }, 1, 12, 1, Appearance -> "Labeled" },
      { { sel, None, "Select (ambiguity resolver)" }, selOpts, ControlType -> SetterBar },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, sel },
      SaveDefinitions -> True
    ]
  ]
