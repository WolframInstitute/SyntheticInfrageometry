Package["WolframInstitute`SyntheticInfrageometry`"]

(* Manipulate-based interactive viewers built on top of `InfraSceneHighlight`.
   Per-primitive viewers (`PointViewer`, `SegmentViewer`, `ShellViewer`,
   `CircleViewer`) drive a single `Find*[..., All]` enumeration; the scene-level
   `InfraSceneViewer` walks the construction-step DAG of an `InfraScene`. *)


$InfraSegmentSelectOptions = { None, "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraCircleSelectOptions = { None, "ShortestCircumference", "LongestCircumference",
  "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };


(* ===================== Per-object viewers ===================== *)

(* Each viewer is a `Manipulate` that drives a `FindX[..., All]` enumeration,
   optionally pipes the result through a path-space selector, takes up to `n`,
   and renders the resulting multi-object via `InfraSceneHighlight`. Endpoints
   / centre are overlaid as a separate fixed-colour highlight on top. *)

SetAttributes[ PointViewer, HoldRest ]

PointViewer[ g_Graph, sym_: None ] :=
  With[ { diam = GraphDiameter[ g ] },
    Manipulate[
      seed;
      With[ { pts = FindPoint[ g, UpTo[ n ], "From" -> from, "MaxCliques" -> 100,
          "Distance" -> Switch[ separation, "None", None, "Max", "Max", "Range", distRange ] ] },
        If[ sym =!= None, sym = pts ];
        InfraSceneHighlight[ g, { InfraPoint[ pts ] -> $InfraPointColor }, ImageSize -> 600 ] ],
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
  With[ {
      initPts     = RandomSample[ VertexList[ g ], 2 ],
      nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
      selOpts     = $InfraSegmentSelectOptions },
    Manipulate[
      seed;
      With[ {
          segments = If[ p1 === p2 || GraphDistance[ g, p1, p2 ] === Infinity, {},
            Take[
              applyPathSpaceSelector[ g, FindSegment[ g, p1, p2, All ],
                sel, <| "Cyclic" -> False, "Endpoints" -> { p1, p2 } |> ],
              UpTo[ n ] ] ] },
        EventHandler[
          HighlightGraph[
            InfraSceneHighlight[ g, { InfraSegment[ segments ] -> $InfraSegmentColor } ],
            { Style[ p1, Directive[ $InfraPointColor, AbsolutePointSize[ 16 ] ] ],
              Style[ p2, Directive[ $InfraPointColor, AbsolutePointSize[ 16 ] ] ] },
            ImageSize -> 600 ],
          { "MouseClicked" :> With[ { mp = MousePosition[ "Graphics" ] },
            If[ mp =!= None,
              With[ { clicked = First @ nearestFunc[ mp ] },
                p1 = p2; p2 = clicked; seed++ ] ] ] },
          PassEventsDown -> True
        ]
      ],
      { { p1, initPts[[ 1 ]] }, None },
      { { p2, initPts[[ 2 ]] }, None },
      { { seed, 0 }, None },
      { { n, 12, "Segments" }, 1, 12, 1, Appearance -> "Labeled" },
      { { sel, None, "Select (ambiguity resolver)" }, selOpts, ControlType -> SetterBar },
      Button[ "Resample", With[ { pts = RandomSample[ VertexList[ g ], 2 ] },
        p1 = pts[[ 1 ]]; p2 = pts[[ 2 ]]; seed++ ] ],
      TrackedSymbols :> { p1, p2, seed, n, sel },
      SaveDefinitions -> True
    ]
  ]


ShellViewer[ g_Graph ] :=
  With[ {
      initPt      = RandomChoice[ VertexList[ g ] ],
      nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
      diam        = Max[ GraphDiameter[ g ], 2 ] },
    Manipulate[
      seed;
      With[ {
          shells = If[ r < 1, {},
            Take[ FindShell[ g, p, r, All, Method -> method ], UpTo[ n ] ] ] },
        EventHandler[
          HighlightGraph[
            InfraSceneHighlight[ g, { InfraShell[ shells ] -> $InfraShellColor } ],
            { Style[ p, Directive[ $InfraPointColor, AbsolutePointSize[ 16 ] ] ] },
            ImageSize -> 600 ],
          { "MouseClicked" :> With[ { mp = MousePosition[ "Graphics" ] },
            If[ mp =!= None,
              With[ { clicked = First @ nearestFunc[ mp ] }, p = clicked; seed++ ] ] ] },
          PassEventsDown -> True
        ]
      ],
      { { p, initPt }, None },
      { { seed, 0 }, None },
      { { r, Max[ 1, Round[ diam / 3 ] ], "Radius" }, 1, diam, 1, Appearance -> "Labeled" },
      { { n, 12, "Shells" }, 1, 12, 1, Appearance -> "Labeled" },
      { { method, "Metric", "Method" }, { "Metric", "Separating" }, ControlType -> SetterBar },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, method },
      SaveDefinitions -> True
    ]
  ]


CircleViewer[ g_Graph ] :=
  With[ {
      initPt      = RandomChoice[ VertexList[ g ] ],
      nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
      selOpts     = $InfraCircleSelectOptions,
      diam        = Max[ GraphDiameter[ g ], 2 ] },
    Manipulate[
      seed;
      With[ {
          circles = If[ r < 1, {},
            Take[
              applyPathSpaceSelector[ g, FindCircle[ g, p, r, All ],
                sel, <| "Cyclic" -> True, "Center" -> p, "Radius" -> r |> ],
              UpTo[ n ] ] ] },
        EventHandler[
          HighlightGraph[
            InfraSceneHighlight[ g, { InfraCircle[ circles ] -> $InfraCircleColor } ],
            { Style[ p, Directive[ $InfraPointColor, AbsolutePointSize[ 16 ] ] ] },
            ImageSize -> 600 ],
          { "MouseClicked" :> With[ { mp = MousePosition[ "Graphics" ] },
            If[ mp =!= None,
              With[ { clicked = First @ nearestFunc[ mp ] }, p = clicked; seed++ ] ] ] },
          PassEventsDown -> True
        ]
      ],
      { { p, initPt }, None },
      { { seed, 0 }, None },
      { { r, Max[ 1, Round[ diam / 3 ] ], "Radius" }, 1, diam, 1, Appearance -> "Labeled" },
      { { n, 12, "Circles" }, 1, 12, 1, Appearance -> "Labeled" },
      { { sel, None, "Select (ambiguity resolver)" }, selOpts, ControlType -> SetterBar },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, sel },
      SaveDefinitions -> True
    ]
  ]


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
