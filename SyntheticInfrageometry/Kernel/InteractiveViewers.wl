Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraSceneViewer ===================== *)

(* Step-navigated viewer for an InfraScene on a graph. Each construction step
   produces a multi-spread of bindings; the diffuse picture across that spread
   is rendered via `InfraSceneHighlight`. Pre-fixed objects from earlier steps
   are overlaid on top in a darker shade. *)

InfraSceneViewer[ scene_InfraScene, graph_Graph ] :=
  InfraSceneViewer[ scene, graph, <||> ]

InfraSceneViewer[ scene_InfraScene, graph_Graph, init_Association ] :=
  Module[ { objects, steps, constructions, nSteps,
            typeColor, typeOf, grouped, palette },

    objects       = scene[ "Objects" ];
    steps         = scene[ "Steps" ];
    constructions = scene[ "Constructions" ];
    nSteps        = Length @ steps;

    typeColor = <|
      "point"   -> RGBColor[ 0.93, 0.50, 0.50 ],
      "segment" -> RGBColor[ 0.50, 0.60, 0.93 ],
      "shell"   -> RGBColor[ 0.50, 0.86, 0.62 ],
      "circle"  -> RGBColor[ 0.30, 0.70, 0.85 ] |>;

    typeOf = AssociationMap[
      o |-> Switch[ Head @ Lookup[ constructions, o ],
        InfraPoint,        "point",
        InfraIntersection, "point",
        InfraSegment,      "segment",
        InfraLine,         "segment",
        InfraShell,        "shell",
        InfraCircle,       "circle",
        _,                 "point" ],
      Select[ objects, KeyExistsQ[ constructions, # ] & ] ];

    grouped = GroupBy[ objects, Lookup[ typeOf, #, "point" ] & ];

    palette = AssociationMap[
      o |-> With[ { tp = Lookup[ typeOf, o, "point" ] },
        With[ { grp = grouped[ tp ] },
          If[ Length @ grp <= 1, typeColor[ tp ],
            Lighter[ typeColor[ tp ],
              0.15 * ( 1 - 2 * ( FirstPosition[ grp, o ][[ 1 ]] - 1 ) / ( Length @ grp - 1 ) ) ] ] ] ],
      objects ];

    DynamicModule[ {
        step      = 1,
        inst      = 1,
        aggregate = True,
        enabled   = objects,
        fixed     = init,
        instances = FindInfraScene[ scene, graph, 1, init ],
        nInst     = 0 },
      nInst = Length @ instances;

      Column[ {
        Dynamic[
          With[ {
              currentObjects = Intersection[ enabled, steps[[ step ]] ],
              fixedObjects   = Intersection[ enabled, Flatten @ Take[ steps, step - 1 ] ] },
            With[ {
                activeObjects = If[ nInst == 0, {},
                  If[ aggregate, currentObjects,
                    Select[ currentObjects, KeyExistsQ[ instances[[ Min[ inst, nInst ], 1 ]], # ] & ] ] ] },
              With[ {
                  wrap = obj |-> <| "point" -> InfraPoint, "segment" -> InfraSegment,
                                    "shell" -> InfraShell, "circle" -> InfraCircle |>[
                    Lookup[ typeOf, obj, "point" ] ] },
                With[ {
                  multiObjs = Which[
                    nInst == 0, {},
                    aggregate,
                      ( wrap[ # ][ Through[ instances[[ All, 1 ]][ # ] ] ] -> palette[ # ] ) & /@ activeObjects,
                    True,
                      ( wrap[ # ][ { instances[[ Min[ inst, nInst ], 1 ]][ # ] } ] -> palette[ # ] ) & /@ activeObjects ] },
                HighlightGraph[
                  InfraSceneHighlight[ graph, multiObjs ],
                  Flatten @ Table[
                    With[ {
                        value = fixed[ o ],
                        color = Darker[ palette[ o ], 0.2 ],
                        tp    = Lookup[ typeOf, o, "point" ] },
                      If[ tp === "point",
                        { Style[ value, Directive[ color, AbsolutePointSize[ 14 ] ] ] },
                        Join[
                          Style[ #, Directive[ color, AbsolutePointSize[ 12 ] ] ] & /@ toVertexSet @ value,
                          Style[ #, Directive[ AbsoluteThickness[ 3 ], color ] ] & /@
                            EdgeList @ Subgraph[ graph, toVertexSet @ value ] ] ] ],
                    { o, Select[ fixedObjects, KeyExistsQ[ fixed, # ] & ] } ],
                  ImageSize -> 600 ] ] ] ] ],
          TrackedSymbols :> { step, inst, aggregate, enabled, fixed, instances, nInst } ],

        Delimiter,
        Row[ { "Step ",
          Slider[ Dynamic[ step,
            ( step = #;
              instances = FindInfraScene[ scene, graph, step, fixed ];
              nInst = Length @ instances;
              inst = Min[ inst, Max[ nInst, 1 ] ] ) & ],
            { 1, nSteps, 1 } ],
          " ", Dynamic[ step ], "/", nSteps } ],

        Delimiter,
        Row[ {
          Button[ "Fix & advance",
            If[ nInst > 0 && step < nSteps,
              fixed     = instances[[ Min[ inst, nInst ], 1 ]];
              step      = step + 1;
              instances = FindInfraScene[ scene, graph, step, fixed ];
              nInst     = Length @ instances;
              inst      = 1 ],
            Enabled -> Dynamic[ nInst > 0 && step < nSteps ] ],
          " ",
          Button[ "Reset",
            fixed     = init;
            step      = 1;
            instances = FindInfraScene[ scene, graph, 1, init ];
            nInst     = Length @ instances;
            inst      = 1 ]
        } ],

        Delimiter,
        Row[ { "Aggregate ", Checkbox[ Dynamic[ aggregate ] ] } ],
        Row[ { "Instance ",
          Slider[ Dynamic[ inst ], { 1, Dynamic[ Max[ nInst, 1 ] ], 1 },
            Enabled -> Dynamic[ ! aggregate ] ],
          " ", Dynamic[ Min[ inst, Max[ nInst, 1 ] ] ], "/", Dynamic[ nInst ] } ],

        Delimiter,
        Dynamic[ TogglerBar[ Dynamic[ enabled ],
          Thread[ objects -> ( Style[ ToString @ #, Bold, palette[ # ] ] & /@ objects ) ] ] ]
      }, Alignment -> Left ]
    ]
  ]
