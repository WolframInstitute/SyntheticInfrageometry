Package["WolframInstitute`SyntheticInfrageometry`"]


InfraSceneViewer[ scene_InfraScene, graph_Graph ] :=
  Module[ { objects, steps, constructions, nSteps, allInstances, nInstances, bindings,
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

    allInstances = FindInfraScene[ scene, graph ];
    nInstances = Length[ allInstances ];
    bindings = If[ nInstances == 0, {}, allInstances[[ All, 1 ]] ];

    If[ nInstances == 0,
      HighlightGraph[ graph, {}, ImageSize -> 600, PlotLabel -> "No instances found" ],

      With[ { pal = palette, tOf = typeOf, bnd = bindings, nI = nInstances, nS = nSteps,
               stp = steps, obj = objects },
        Manipulate[
          Module[ { resolved, active, vertexHighlights, edgeHighlights, infoText },
            resolved = Flatten[ Take[ stp, step ] ];
            active = Intersection[ enabled, resolved ];
            vertexHighlights = {};
            edgeHighlights = {};

            If[ aggregate,
              Module[ { vertexFreqs, maxFreqs, edgeFreqs, vertexColorMap },
                vertexFreqs = Association @ Table[
                  o -> Counts @ Flatten[ toVertexSet[ #[ o ] ] & /@ bnd ],
                  { o, active }
                ];
                maxFreqs = Association @ Table[
                  o -> Max[ Append[ Values[ vertexFreqs[ o ] ], 1 ] ],
                  { o, active }
                ];
                edgeFreqs = Association @ Table[
                  o -> Counts @ Flatten[
                    With[ { path = #[ o ] },
                      If[ Lookup[ tOf, o, "point" ] === "circle",
                        UndirectedEdge @@@ Partition[ Append[ path, First[ path ] ], 2, 1 ],
                        UndirectedEdge @@@ Partition[ path, 2, 1 ]
                      ]
                    ] & /@ bnd
                  ],
                  { o, Select[ active, Lookup[ tOf, #, "point" ] =!= "point" & ] }
                ];
                vertexColorMap = <||>;
                Do[
                  KeyValueMap[
                    { vertex, freq } |->
                      ( vertexColorMap[ vertex ] = Append[
                          Lookup[ vertexColorMap, vertex, {} ],
                          Lighter[ pal[ o ], 1 - freq / maxFreqs[ o ] ]
                        ] ),
                    vertexFreqs[ o ]
                  ],
                  { o, active }
                ];
                vertexHighlights = KeyValueMap[
                  { vertex, colors } |-> Style[ vertex, Directive[
                    Switch[ Length[ colors ],
                      0, GrayLevel[ 0.8 ],
                      1, First[ colors ],
                      _, Blend[ colors ]
                    ],
                    AbsolutePointSize[ 8 + 6 * Min[ Length[ colors ], Length[ active ] ] / Max[ Length[ active ], 1 ] ]
                  ] ],
                  vertexColorMap
                ];
                edgeHighlights = Flatten @ KeyValueMap[
                  { o2, counts } |->
                    With[ { maxF = Max[ Append[ Values[ counts ], 1 ] ] },
                      KeyValueMap[
                        { edge, freq } |-> Style[ edge, Directive[ AbsoluteThickness[ 3 ], Opacity[ freq / maxF, pal[ o2 ] ] ] ],
                        counts
                      ]
                    ],
                  edgeFreqs
                ];
                infoText = Row[ { "Step ", step, "/", nS, " \[LongDash] ", nI, " instances (aggregate)" } ];
              ],

              Module[ { binding },
                binding = bnd[[ Min[ inst, nI ] ]];
                Do[
                  With[ { val = binding[ o ], tp = Lookup[ tOf, o, "point" ] },
                    If[ !MissingQ[ val ],
                      If[ tp === "point",
                        AppendTo[ vertexHighlights,
                          Style[ val, Directive[ pal[ o ], AbsolutePointSize[ 12 ] ] ] ],
                        Module[ { verts, edges },
                          verts = toVertexSet[ val ];
                          edges = If[ tp === "circle",
                            UndirectedEdge @@@ Partition[ Append[ val, First[ val ] ], 2, 1 ],
                            UndirectedEdge @@@ Partition[ val, 2, 1 ]
                          ];
                          vertexHighlights = Join[ vertexHighlights,
                            Style[ #, Directive[ pal[ o ], AbsolutePointSize[ 10 ] ] ] & /@ verts ];
                          edgeHighlights = Join[ edgeHighlights,
                            Style[ #, Directive[ AbsoluteThickness[ 3 ], pal[ o ] ] ] & /@ edges ];
                        ]
                      ]
                    ]
                  ],
                  { o, active }
                ];
                infoText = Row[ { "Step ", step, "/", nS, " \[LongDash] instance ", Min[ inst, nI ], "/", nI } ];
              ]
            ];

            Column[ {
              HighlightGraph[ graph,
                Join[ edgeHighlights, vertexHighlights ],
                ImageSize -> 600
              ],
              Style[ infoText, Gray, 10 ]
            }, Alignment -> Center ]
          ],
          { { step, nS, "Step" }, 1, nS, 1, Appearance -> "Labeled" },
          Delimiter,
          { { aggregate, True, "Aggregate" }, { True, False } },
          { { inst, 1, "Instance" }, 1, Max[ nI, 1 ], 1, Appearance -> "Labeled",
            Enabled -> Dynamic[ !aggregate ] },
          Delimiter,
          { { enabled, obj, "Objects" },
            Thread[ obj -> (Style[ ToString[ # ], Bold, pal[ # ] ] & /@ obj) ],
            TogglerBar },
          TrackedSymbols :> { step, enabled, aggregate, inst },
          SaveDefinitions -> True
        ]
      ]
    ]
  ]
