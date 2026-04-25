Package["WolframInstitute`SyntheticInfrageometry`"]


InfraSceneViewer[ scene_InfraScene, graph_Graph ] :=
  InfraSceneViewer[ scene, graph, <||> ]

InfraSceneViewer[ scene_InfraScene, graph_Graph, init_Association ] :=
  Module[ { objects, steps, constructions, nSteps,
             ptColor, segColor, circColor, typeOf, typeColor, grouped, palette },
    objects = scene[ "Objects" ];
    steps = scene[ "Steps" ];
    constructions = scene[ "Constructions" ];
    nSteps = Length[ steps ];

    ptColor = RGBColor[ 0.93, 0.50, 0.50 ];
    segColor = RGBColor[ 0.50, 0.60, 0.93 ];
    circColor = RGBColor[ 0.50, 0.86, 0.62 ];

    typeOf = Association @ Table[
      obj -> Switch[ Head[ constructions[ obj ] ],
        InfraPoint, "point",
        InfraIntersection, "point",
        InfraIntersectionPoint, "point",
        InfraSegment, "segment",
        InfraLine, "segment",
        InfraCircle, "circle",
        _, "point"
      ],
      { obj, Select[ objects, KeyExistsQ[ constructions, # ] & ] }
    ];
    typeColor = <| "point" -> ptColor, "segment" -> segColor, "circle" -> circColor |>;
    grouped = GroupBy[ objects, Lookup[ typeOf, #, "point" ] & ];
    palette = Association @ Table[
      With[ { tp = Lookup[ typeOf, obj, "point" ], grp = grouped[ Lookup[ typeOf, obj, "point" ] ] },
        obj -> If[ Length[ grp ] <= 1,
          typeColor[ tp ],
          Lighter[ typeColor[ tp ], 0.15 * (1 - 2 * (Position[ grp, obj ][[ 1, 1 ]] - 1) / (Length[ grp ] - 1)) ]
        ]
      ],
      { obj, objects }
    ];

    With[ { pal = palette, tOf = typeOf, stp = steps, obj = objects, nS = nSteps,
             sc = scene, gr = graph, ini = init },
      DynamicModule[
        { step = 1, inst = 1, aggregate = True, enabled = obj,
          fixed = ini, instances, nInst },

        instances = FindInfraScene[ sc, gr, 1, fixed ];
        nInst = Length[ instances ];

        Column[ {
          Dynamic[
            renderScene[ gr, stp, step, pal, tOf, obj, enabled, fixed, instances, nInst, inst, aggregate ],
            TrackedSymbols :> { step, inst, aggregate, enabled, fixed, instances, nInst }
          ],
          Delimiter,
          Row[ { "Step ",
            Slider[ Dynamic[ step,
              ( step = #;
                instances = FindInfraScene[ sc, gr, step, fixed ];
                nInst = Length[ instances ];
                inst = Min[ inst, Max[ nInst, 1 ] ] ) & ],
              { 1, nS, 1 } ],
            " ", Dynamic[ step ], "/", nS } ],
          Delimiter,
          Row[ {
            Button[ "Fix & advance",
              If[ nInst > 0 && step < nS,
                fixed = instances[[ Min[ inst, nInst ], 1 ]];
                step = step + 1;
                instances = FindInfraScene[ sc, gr, step, fixed ];
                nInst = Length[ instances ];
                inst = 1 ],
              Enabled -> Dynamic[ nInst > 0 && step < nS ] ],
            " ",
            Button[ "Reset",
              fixed = ini;
              step = 1;
              instances = FindInfraScene[ sc, gr, 1, fixed ];
              nInst = Length[ instances ];
              inst = 1 ]
          } ],
          Delimiter,
          Row[ { "Aggregate ", Checkbox[ Dynamic[ aggregate ] ] } ],
          Row[ { "Instance ",
            Slider[ Dynamic[ inst ], { 1, Dynamic[ Max[ nInst, 1 ] ], 1 },
              Enabled -> Dynamic[ !aggregate ] ],
            " ", Dynamic[ Min[ inst, Max[ nInst, 1 ] ] ], "/", Dynamic[ nInst ] } ],
          Delimiter,
          Dynamic[
            TogglerBar[ Dynamic[ enabled ],
              Thread[ obj -> (Style[ ToString[ # ], Bold, pal[ # ] ] & /@ obj) ] ]
          ]
        }, Alignment -> Left ]
      ]
    ]
  ]


renderScene[ graph_, steps_, step_, palette_, typeOf_, objects_, enabled_,
             fixed_, instances_, nInst_, inst_, aggregate_ ] :=
  Module[ { fixedObjects, currentObjects, highlights },
    fixedObjects = Intersection[ enabled, Flatten[ Take[ steps, step - 1 ] ] ];
    currentObjects = Intersection[ enabled, steps[[ step ]] ];

    highlights = Flatten @ Table[
      highlightsFor[ fixed[ o ], Lookup[ typeOf, o, "point" ], Darker[ palette[ o ], 0.2 ], 14 ],
      { o, Select[ fixedObjects, KeyExistsQ[ fixed, # ] & ] } ];

    If[ nInst > 0,
      highlights = Join[ highlights,
        If[ aggregate,
          aggregateHighlights[ instances, currentObjects, typeOf, palette ],
          With[ { binding = instances[[ Min[ inst, nInst ], 1 ]] },
            Flatten @ Table[
              highlightsFor[ binding[ o ], Lookup[ typeOf, o, "point" ], palette[ o ], 12 ],
              { o, Select[ currentObjects, KeyExistsQ[ binding, # ] & ] } ] ] ] ] ];

    HighlightGraph[ graph, highlights, ImageSize -> 600 ]
  ]


highlightsFor[ val_, "point", color_, ptSize_ ] :=
  { Style[ val, Directive[ color, AbsolutePointSize[ ptSize ] ] ] }

highlightsFor[ val_, type_, color_, ptSize_ ] :=
  Module[ { verts, edges },
    verts = toVertexSet[ val ];
    edges = If[ type === "circle",
      UndirectedEdge @@@ Partition[ Append[ val, First[ val ] ], 2, 1 ],
      UndirectedEdge @@@ Partition[ val, 2, 1 ] ];
    Join[
      Style[ #, Directive[ color, AbsolutePointSize[ ptSize - 2 ] ] ] & /@ verts,
      Style[ #, Directive[ AbsoluteThickness[ 3 ], color ] ] & /@ edges
    ]
  ]


aggregateHighlights[ instances_, currentObjects_, typeOf_, palette_ ] :=
  Module[ { bnd, vertexFreqs, maxFreqs, edgeFreqs, vertexColorMap, vertexH, edgeH },
    bnd = instances[[ All, 1 ]];
    vertexFreqs = Association @ Table[
      o -> Counts @ Flatten[ toVertexSet[ #[ o ] ] & /@ bnd ],
      { o, currentObjects } ];
    maxFreqs = Association @ Table[
      o -> Max[ Append[ Values[ vertexFreqs[ o ] ], 1 ] ],
      { o, currentObjects } ];
    edgeFreqs = Association @ Table[
      o -> Counts @ Flatten[
        With[ { path = #[ o ] },
          If[ Lookup[ typeOf, o, "point" ] === "circle",
            UndirectedEdge @@@ Partition[ Append[ path, First[ path ] ], 2, 1 ],
            UndirectedEdge @@@ Partition[ path, 2, 1 ] ]
        ] & /@ bnd ],
      { o, Select[ currentObjects, Lookup[ typeOf, #, "point" ] =!= "point" & ] } ];
    vertexColorMap = <||>;
    Do[
      KeyValueMap[
        Function[ { v, freq },
          vertexColorMap[ v ] = Append[
            Lookup[ vertexColorMap, v, {} ],
            Lighter[ palette[ o ], 1 - freq / maxFreqs[ o ] ] ] ],
        vertexFreqs[ o ] ],
      { o, currentObjects } ];
    vertexH = KeyValueMap[
      Function[ { v, cs },
        Style[ v, Directive[
          Switch[ Length[ cs ],
            0, GrayLevel[ 0.8 ],
            1, First[ cs ],
            _, Blend[ cs ] ],
          AbsolutePointSize[ 8 + 6 * Min[ Length[ cs ], Length[ currentObjects ] ] /
            Max[ Length[ currentObjects ], 1 ] ] ] ] ],
      vertexColorMap ];
    edgeH = Flatten @ Table[
      With[ { counts = edgeFreqs[ o ],
              maxF = Max[ Append[ Values[ edgeFreqs[ o ] ], 1 ] ] },
        KeyValueMap[
          Function[ { edge, freq },
            Style[ edge, Directive[ AbsoluteThickness[ 3 ],
              Opacity[ freq / maxF, palette[ o ] ] ] ] ],
          counts ] ],
      { o, Keys[ edgeFreqs ] } ];
    Join[ edgeH, vertexH ]
  ]
