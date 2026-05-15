Package["WolframInstitute`SyntheticInfrageometry`"]

(* Manipulate-based interactive viewers built on top of `InfraSceneHighlight`.
   Per-primitive viewers (`PointViewer`, `SegmentViewer`, `ShellViewer`,
   `CircleViewer`) drive a single `Find*[..., All]` enumeration; the scene-level
   `InfraSceneViewer` walks the construction-step DAG of an `InfraScene`. *)


$InfraSegmentSelectOptions = { None, "Central", "Peripheral", "EmbeddingClosest" };

$InfraCircleSelectOptions = { None, "Central", "Peripheral",
  "ShortestCircumference", "LongestCircumference", "EmbeddingClosest" };


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
      With[ { pts = FindInfraPoint[ g, UpTo[ n ], "From" -> from, "MaxCliques" -> 100,
          "Distance" -> Switch[ separation, "None", None, "Max", "Max", "Range", distRange ] ] },
        If[ sym =!= None, sym = pts ];
        InfraSceneHighlight[ g, { pts -> $InfraPointColor }, ImageSize -> 600 ] ],
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
              applySelectOption[ g, #[[ 1, 1 ]] & /@ FindInfraSegment[ g, p1, p2, All ],
                sel, False, <| "Endpoints" -> { p1, p2 } |> ],
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
            #[[ 1, 1 ]] & /@ Take[ FindInfraShell[ g, p, r, All, Properties -> properties ], UpTo[ n ] ] ] },
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
      { { properties, { }, "Properties" },
        { { } -> "Level set", { "Separating" } -> "Separating", { "Separating", "Connected" } -> "Sep+Conn" },
        ControlType -> SetterBar },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, properties },
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
              applySelectOption[ g, #[[ 1, 1 ]] & /@ FindInfraCircle[ g, p, r, All ],
                sel, True, <| "Center" -> p, "Radius" -> r |> ],
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

(* Step-navigated viewer for an InfraScene on a graph, in the spirit of
   Wolfram's `GeometricScene` widget: a single graphics pane with two
   prev/next bars - one for the construction step, one for the branch
   within that step's multi-spread. `InfraSceneHighlight` does the
   per-head colouring; no separate palette is needed. *)

InfraSceneViewer[ scene_InfraScene, graph_Graph ] :=
  InfraSceneViewer[ scene, graph, <||> ]

InfraSceneViewer[ scene_InfraScene, graph_Graph, init_Association ] :=
  With[ {
      nSteps = Length @ scene[ "Steps" ],
      labels = scene[ "Labels" ],
      wrap   = AssociationMap[
        Switch[ Head @ Lookup[ scene[ "Constructions" ], #, InfraPoint ],
          InfraSegment | InfraLine, InfraSegment,
          InfraShell,               InfraShell,
          InfraPlane,               InfraPlane,
          InfraCircle,              InfraCircle,
          _,                        InfraPoint ] &,
        scene[ "Objects" ] ] },
    DynamicModule[ {
        step      = 1,
        branch    = 1,
        instances = FindInfraScene[ scene, graph, 1, init ] },
      Column[ {
        Row[ {
          Button[ "\[LeftPointer]",
            step = Max[ step - 1, 1 ];
            instances = FindInfraScene[ scene, graph, step, init ];
            branch = 1 ],
          Spacer[ 8 ],
          Dynamic @ Style[
            Row[ { "step ", step, "/", nSteps,
              If[ labels[[ step ]] === None, "", Row[ { ": ", labels[[ step ]] } ] ] } ],
            Bold ],
          Spacer[ 8 ],
          Button[ "\[RightPointer]",
            step = Min[ step + 1, nSteps ];
            instances = FindInfraScene[ scene, graph, step, init ];
            branch = 1 ]
        } ],
        Row[ {
          Button[ "\[LeftPointer]",
            branch = Max[ branch - 1, 1 ] ],
          Spacer[ 8 ],
          Dynamic @ Row[ { "branch ", Min[ branch, Max[ Length @ instances, 1 ] ],
            "/", Length @ instances } ],
          Spacer[ 8 ],
          Button[ "\[RightPointer]",
            branch = Min[ branch + 1, Max[ Length @ instances, 1 ] ] ]
        } ],
        Dynamic @ If[ instances === {},
          InfraSceneHighlight[ graph, {}, ImageSize -> 600 ],
          InfraSceneHighlight[ graph,
            KeyValueMap[
              wrap[ #1 ][ { #2 } ] &,
              instances[[ Min[ branch, Length @ instances ], 1 ]] ],
            ImageSize -> 600 ] ]
      }, Alignment -> Left ]
    ]
  ]
