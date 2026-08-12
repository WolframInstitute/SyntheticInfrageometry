Package["WolframInstitute`SyntheticInfrageometry`"]

(* Manipulate-based interactive viewers built on top of `InfraSceneHighlight`.
   Per-primitive viewers (`PointViewer`, `SegmentViewer`, `ShellViewer`,
   `CircleViewer`) drive a single `Find*[..., All]` enumeration; the scene-level
   `InfraSceneViewer` walks the construction-step DAG of an `InfraScene`. *)


$InfraSegmentSelectOptions = { None, "Central", "Peripheral", "EmbeddingClosest" };

$InfraCircleSelectOptions = { None, "Central", "Peripheral",
  "MinLength", "MaxLength", "EmbeddingClosest" };


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
              applySelectOption[ g, segReps @ FindInfraSegment[ g, p1, p2, All ],
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
            Take[ FindInfraShell[ g, p, r, All, Properties -> properties ][ "Realizations" ], UpTo[ n ] ] ] },
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
              applySelectOption[ g, FindInfraCircle[ g, p, r, All ][ "Realizations" ],
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
   Wolfram's `GeometricScene` widget: chrome-styled step / branch bars over
   a single `InfraSceneHighlight` pane, a per-step hide toggle (eye icon),
   and a `Fixed` checkbox that locks the current branch as the initial
   bindings for all later steps (the fix-and-advance workflow). A step
   whose object has no realisation shows an alert chip and keeps the last
   constructible step on screen. Rendering options forward to
   `InfraSceneHighlight`. *)

Options[ InfraSceneViewer ] = {
  "OpacityRange"   :> $InfraOpacityRange,
  "ThicknessRange" :> $InfraEdgeThickness,
  "PointSizeRange" -> 18,
  ImageSize        -> 500
};

InfraSceneViewer[ scene_InfraScene, graph_Graph, init : _Association : <||>, opts : OptionsPattern[ ] ] :=
  With[ {
      nSteps  = Length @ scene[ "Steps" ],
      labels  = scene[ "Labels" ],
      objects = scene[ "Objects" ],
      imgW    = OptionValue[ InfraSceneViewer, { opts }, ImageSize ],
      hlOpts  = FilterRules[ Join[ { opts }, Options[ InfraSceneViewer ] ], Options[ InfraSceneHighlight ] ],
      objStep = Association @@ Flatten[ MapIndexed[
        { syms, i } |-> ( ( # -> First[ i ] ) & /@ Flatten[ { syms } ] ), scene[ "Steps" ] ] ],
      wrap    = AssociationMap[
        Switch[ Head @ Lookup[ scene[ "Constructions" ], #, InfraPoint ],
          InfraSegment | InfraLine, InfraSegment,
          InfraShell,               InfraShell,
          InfraPlane,               InfraPlane,
          InfraCircle,              InfraCircle,
          InfraPolygon,             InfraPolygon,
          InfraTriangle,            InfraTriangle,
          InfraPolyline,            InfraPolyline,
          _,                        InfraPoint ] &,
        scene[ "Objects" ] ] },
    DynamicModule[ {
        step = 1, branch = 1, mode = "Branch",
        fixStack = { }, hiddenSteps = { },
        shown = { }, shownStep = 1, deadQ = False },
      With[ {
          effInit = Function[ If[ fixStack === { }, init, Last[ fixStack ][[ 2 ]] ] ],
          shownQ  = obj |-> ! MemberQ[ hiddenSteps, objStep[ obj ] ] },
        {
          refresh = Function[
            With[ { new = FindInfraScene[ scene, graph, step, effInit[ ] ] },
              If[ new === { }, deadQ = True,
                deadQ = False; shown = new; shownStep = step;
                branch = Min[ branch, Length @ new ] ] ] ] },
        {
          goStep = s |-> ( step = Clip[ s, { 1, nSteps } ]; branch = 1; refresh[ ] ) },
        refresh[ ];
        Pane[ Column[ {
          Grid[ { {
            Spacer[ 30 ],
            Row[ {
              iconButton[ leftArrowIcon, goStep[ step - 1 ] ], "  ",
              ActionMenu[
                Dynamic @ textChip[ Row[ { "step ", step, "/", nSteps,
                  If[ labels[[ step ]] === None, "", Row[ { ": ", labels[[ step ]] } ] ],
                  "  ", downChevron } ] ],
                Table[
                  Row[ { "step ", k,
                    If[ labels[[ k ]] === None, "", Row[ { ": ", labels[[ k ]] } ] ] } ] :> goStep[ k ],
                  { k, nSteps } ],
                Appearance -> None ],
              "  ", iconButton[ rightArrowIcon, goStep[ step + 1 ] ] } ],
            Dynamic @ iconButton[
              If[ MemberQ[ hiddenSteps, step ], eyeClosedIcon, eyeOpenIcon ],
              If[ MemberQ[ hiddenSteps, step ],
                hiddenSteps = DeleteCases[ hiddenSteps, step ],
                AppendTo[ hiddenSteps, step ] ] ]
          } }, Alignment -> { { Center, Center, Right }, Center },
            ItemSize -> { { 2.4, Scaled[ .8 ], 2.4 }, Automatic } ],
          Grid[ { {
            SetterBar[ Dynamic @ mode, { "Branch", "Diffuse" } ],
            Dynamic @ Which[
              deadQ, alertChip[ Row[ { "does not exist \[LongDash] showing step ", shownStep } ] ],
              mode === "Diffuse", textChip[ Row[ { Length @ shown, " branches diffused" } ] ],
              True, Row[ {
                Checkbox[ Dynamic[ MemberQ[ fixStack[[ All, 1 ]], step ],
                  nv |-> ( If[ nv,
                      AppendTo[ fixStack, { step, shown[[ Min[ branch, Length @ shown ], 1 ]] } ],
                      fixStack = Select[ fixStack, First[ # ] < step & ] ];
                    refresh[ ] ) ] ], " Fixed",
                Spacer[ 14 ],
                iconButton[ leftArrowIcon, branch = Max[ branch - 1, 1 ] ],
                textChip[ Row[ { "branch ", Min[ branch, Max[ Length @ shown, 1 ] ],
                  "/", Length @ shown } ] ],
                iconButton[ rightArrowIcon, branch = Min[ branch + 1, Max[ Length @ shown, 1 ] ] ] } ] ]
          } }, Alignment -> { { Left, Right }, Center },
            ItemSize -> { { Scaled[ .4 ], Scaled[ .6 ] }, Automatic } ],
          Dynamic @ If[ shown === { },
            InfraSceneHighlight[ graph, { }, Sequence @@ hlOpts, ImageSize -> imgW ],
            InfraSceneHighlight[ graph,
              If[ mode === "Diffuse",
                With[ { boundKeys = Keys @ First[ shown ][[ 1 ]] },
                  ( obj |-> wrap[ obj ][
                      DeleteDuplicates[ #[[ 1 ]][ obj ] & /@ shown ] ] ) /@
                  Select[ objects, MemberQ[ boundKeys, # ] && shownQ[ # ] & ] ],
                KeyValueMap[
                  wrap[ #1 ][ { #2 } ] &,
                  KeySelect[ shown[[ Min[ branch, Length @ shown ], 1 ]], shownQ ] ] ],
              Sequence @@ hlOpts, ImageSize -> imgW ] ]
        }, Alignment -> Center ], ImageSize -> imgW ]
      ]
    ]
  ]


(* ===================== Viewer chrome ===================== *)

(* GeometricScene-style control chrome: rounded icon chips with a blue
   accent, dark-mode aware via LightDarkSwitched. *)

makeIcon[ icon_, roundedSide_: "both", widthFactor_: 1, heightFactor_: 1,
    background_: LightDarkSwitched[ GrayLevel[ .9 ], GrayLevel[ .3 ] ] ] :=
  Graphics[ {
    { background, Rectangle[ { -widthFactor, -heightFactor }, { widthFactor, heightFactor },
      RoundingRadius -> Switch[ roundedSide, "left", { Left -> .5 }, "right", { Right -> .5 }, "both", .5 ] ] },
    { Thick, RGBColor[ 0.161, 0.667, 0.887 ], icon } },
    ImageSize -> 20 { widthFactor, heightFactor }, AspectRatio -> Full, PlotRangePadding -> None ]

leftArrowIcon  := makeIcon[ Line[ { { .25, .5 }, { -.25, 0 }, { .25, -.5 } } ], "left", 1, 1.45 ]
rightArrowIcon := makeIcon[ Line[ { { -.25, .5 }, { .25, 0 }, { -.25, -.5 } } ], "right", 1, 1.45 ]

downChevron := Graphics[ { RGBColor[ 0.161, 0.667, 0.887 ], Thick, CapForm[ "Round" ],
  Line[ { { -1, .35 }, { 0, -.5 }, { 1, .35 } } ] },
  ImageSize -> 11, AspectRatio -> 1, PlotRangePadding -> None ]

eyeGlyph = { Circle[ { 0, 0 }, { .7, .45 } ], Disk[ { 0, 0 }, .18 ] };

eyeOpenIcon   := makeIcon[ eyeGlyph, "both", 1.2, 1 ]
eyeClosedIcon := makeIcon[ { eyeGlyph, Line[ { { -.85, -.6 }, { .85, .6 } } ] }, "both", 1.2, 1,
  LightDarkSwitched[ GrayLevel[ .75 ], GrayLevel[ .22 ] ] ]

textChip[ content_ ] := Framed[
  Style[ content, 13, Bold, FontFamily -> "Helvetica", LightDarkSwitched[ GrayLevel[ .15 ], White ] ],
  Background -> LightDarkSwitched[ GrayLevel[ .9 ], GrayLevel[ .3 ] ],
  FrameStyle -> RGBColor[ 0.161, 0.667, 0.887 ], RoundingRadius -> 5,
  FrameMargins -> { { 10, 10 }, { 5, 5 } }, ContentPadding -> False ]

alertChip[ content_ ] := Framed[
  Style[ content, 13, Bold, FontFamily -> "Helvetica", RGBColor[ 0.75, 0.25, 0.2 ] ],
  Background -> LightDarkSwitched[ RGBColor[ 0.99, 0.93, 0.92 ], GrayLevel[ .25 ] ],
  FrameStyle -> RGBColor[ 0.86, 0.35, 0.3 ], RoundingRadius -> 5,
  FrameMargins -> { { 10, 10 }, { 5, 5 } }, ContentPadding -> False ]

SetAttributes[ iconButton, HoldRest ]

iconButton[ icon_, action_ ] :=
  MouseAppearance[ EventHandler[ icon, { "MouseClicked" :> action }, PassEventsUp -> False ], "LinkHand" ]
