Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[$InfraPointColor]
PackageScope[$InfraSegmentColor]
PackageScope[$InfraShellColor]
PackageScope[$InfraCircleColor]
PackageScope[$InfraSceneHighlightPalette]
PackageScope[$InfraOpacityRange]
PackageScope[$InfraThicknessRange]
PackageScope[$InfraPointSizeRange]


$InfraPointColor   = RGBColor[ 0.78, 0.30, 0.55 ];
$InfraSegmentColor = RGBColor[ 0.92, 0.45, 0.30 ];
$InfraShellColor   = RGBColor[ 0.30, 0.70, 0.50 ];
$InfraCircleColor  = RGBColor[ 0.20, 0.55, 0.65 ];

$InfraOpacityRange   = { 0.40, 1.0 };
$InfraThicknessRange = { 1.0, 5.0 };
$InfraPointSizeRange = { 6, 14 };

$InfraSegmentSelectOptions = { None, "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraCircleSelectOptions = { None, "ShortestCircumference", "LongestCircumference",
  "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraSceneHighlightPalette := Join[
  { $InfraSegmentColor, $InfraShellColor, $InfraCircleColor, $InfraPointColor },
  Table[ ColorData[ "DarkRainbow" ][ k / 5 ], { k, 1, 5 } ]
];


(* ===================== InfraSceneHighlight ===================== *)

(* Diffuse rendering of a list of multi-objects on a graph.
   A multi-object is a list of representations.  By default each
   representation is auto-classified against the graph: a value matching
   `MemberQ[VertexList[g], rep]` is a single vertex (rendered as a point);
   anything else is a list of vertices (rendered as the induced subgraph).
   Auto-classification is fragile when vertices are list-named and might
   collide with the list-of-vertices interpretation, so callers can wrap
   a multi-object explicitly using the singular scene heads with a single
   List arg:

     InfraPoint  [ {v1, v2, ...} ]           -- vertices, no edges
     InfraSegment[ {seg1, seg2, ...} ]       -- sequential edges (Partition)
     InfraLine   [ {line1, line2, ...} ]     -- sequential edges (Partition)
     InfraShell  [ {set1, set2, ...} ]       -- induced subgraph edges
     InfraCircle [ {cyc1, cyc2, ...} ]       -- sequential edges + auto-closure

   The arg shape (a single List) selects the rendering interpretation; the
   scene-construction shapes of these heads (e.g. `InfraSegment[p1, p2]`,
   `InfraShell[c, r]`, `InfraCircle[c, r]`) take more args and never collide.
   Each entry may be plain or wrapped as `entry -> color`. *)

Options[ InfraSceneHighlight ] = {
  "OpacityRange"   :> $InfraOpacityRange,
  "ThicknessRange" :> $InfraThicknessRange,
  "PointSizeRange" :> $InfraPointSizeRange
};

InfraSceneHighlight[ graph_Graph, multiObjects_List, opts : OptionsPattern[] ] :=
  Module[ { triples, oRange, tRange, pRange, vEntries, eEntries },

    triples = MapIndexed[
      { item, idx } |-> Replace[
        If[ MatchQ[ item, _Rule ], List @@ item,
          { item, Switch[ Head @ item,
              InfraPoint,   $InfraPointColor,
              InfraSegment, $InfraSegmentColor,
              InfraLine,    $InfraSegmentColor,
              InfraShell,   $InfraShellColor,
              InfraCircle,  $InfraCircleColor,
              _,            $InfraSceneHighlightPalette[[
                              1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ] } ],
        {
          { InfraPoint  [ b_List ], c_ } :> { b, c, "Points" },
          { InfraSegment[ b_List ], c_ } :> { b, c, "Paths"  },
          { InfraLine   [ b_List ], c_ } :> { b, c, "Paths"  },
          { InfraShell  [ b_List ], c_ } :> { b, c, "Sets"   },
          { InfraCircle [ b_List ], c_ } :> { b, c, "Cycles" },
          { b_, c_ }                     :> { b, c, Automatic }
        } ],
      multiObjects ];
    oRange = OptionValue[ "OpacityRange" ];
    tRange = OptionValue[ "ThicknessRange" ];
    pRange = OptionValue[ "PointSizeRange" ];

    With[ {
        repVerts = { type, rep } |-> Switch[ type,
          "Points", { rep },
          "Paths",  rep,
          "Cycles", rep,
          "Sets",   rep,
          _,        If[ MemberQ[ VertexList @ graph, rep ], { rep }, rep ]
        ],
        repEdges = { type, rep } |-> Switch[ type,
          "Points", {},
          "Paths",  If[ Length @ rep >= 2, Sort /@ Partition[ rep, 2, 1 ], {} ],
          "Cycles", With[ {
              closed = If[ Length @ rep >= 2 && First @ rep === Last @ rep,
                rep, Append[ rep, First @ rep ] ] },
            If[ Length @ closed >= 2, Sort /@ Partition[ closed, 2, 1 ], {} ] ],
          "Sets",   Sort /@ ( List @@@ EdgeList @ Subgraph[ graph, rep ] ),
          _,        If[ MemberQ[ VertexList @ graph, rep ], {},
                      Sort /@ ( List @@@ EdgeList @ Subgraph[ graph, rep ] ) ]
        ] },

      vEntries = MapThread[
        { reps, color, type } |-> With[ {
            counts  = Counts @ Catenate[ repVerts[ type, # ] & /@ reps ],
            numReps = Max[ Length @ reps, 1 ] },
          AssociationMap[
            v |-> { color, counts[ v ] / numReps },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]] } ];

      eEntries = MapThread[
        { reps, color, type } |-> With[ {
            counts  = Counts @ Catenate[ repEdges[ type, # ] & /@ reps ],
            numReps = Max[ Length @ reps, 1 ] },
          AssociationMap[
            e |-> { color, counts[ e ] / numReps },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]] } ];
    ];

    HighlightGraph[ graph, Join[
      KeyValueMap[
        { e, cs } |-> With[ {
            w      = Mean @ cs[[ All, 2 ]],
            shades = Lighter[ #[[ 1 ]], 1 - #[[ 2 ]] ] & /@ cs },
          Style[ UndirectedEdge @@ e, Directive[
            If[ Length @ shades == 1, cs[[ 1, 1 ]], Blend @ shades ],
            Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) w ],
            AbsoluteThickness[ tRange[[ 1 ]] + ( tRange[[ 2 ]] - tRange[[ 1 ]] ) w ] ] ] ],
        Merge[ eEntries, Identity ] ],
      KeyValueMap[
        { v, cs } |-> With[ {
            w      = Mean @ cs[[ All, 2 ]],
            shades = Lighter[ #[[ 1 ]], 1 - #[[ 2 ]] ] & /@ cs },
          Style[ v, Directive[
            If[ Length @ shades == 1, cs[[ 1, 1 ]], Blend @ shades ],
            Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) w ],
            AbsolutePointSize[ pRange[[ 1 ]] + ( pRange[[ 2 ]] - pRange[[ 1 ]] ) w ] ] ] ],
        Merge[ vEntries, Identity ] ] ] ]
  ]


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
