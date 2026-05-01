Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[$InfraPointColor]
PackageScope[$InfraSegmentColor]
PackageScope[$InfraSphereColor]
PackageScope[$InfraSceneHighlightPalette]


$InfraPointColor   = RGBColor[ 0.90, 0.10, 0.15 ];
$InfraSegmentColor = RGBColor[ 0.10, 0.30, 0.90 ];
$InfraSphereColor  = RGBColor[ 0.00, 0.60, 0.25 ];

$InfraSegmentSelectOptions = { None, "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraSphereSelectOptions = { None, "ShortestCircumference", "LongestCircumference",
  "FrechetCentral", "FrechetPeripheral",
  "MeanFrechetCentral", "MeanFrechetPeripheral",
  "HausdorffCentral", "HausdorffPeripheral", "EmbeddingClosest" };

$InfraSceneHighlightPalette := Join[
  { $InfraSegmentColor, $InfraSphereColor, $InfraPointColor },
  Table[ ColorData[ "DarkRainbow" ][ k / 5 ], { k, 1, 5 } ]
];


(* ===================== InfraSceneHighlight ===================== *)

(* Diffuse rendering of a list of multi-objects on a graph.
   A multi-object is a list of representations (a vertex, vertex sequence, or
   vertex set). Within one multi-object, vertex/edge intensity scales with how
   many representations of that object pass through it. Across multi-objects,
   colors blend on shared vertices/edges. Each entry may be plain (default
   palette colour by position) or wrapped as `multiObj -> color`. *)

Options[ InfraSceneHighlight ] = {
  "Cyclic"         -> False,
  "OpacityRange"   -> { 0.25, 1.0 },
  "ThicknessRange" -> { 1.0, 5.0 },
  "PointSizeRange" -> { 6, 14 }
};

InfraSceneHighlight[ graph_Graph, multiObjects_List, opts : OptionsPattern[] ] :=
  Module[ { pairs, n, cyclics, oRange, tRange, pRange, vEntries, eEntries },

    pairs = MapIndexed[
      { item, idx } |-> If[ MatchQ[ item, _Rule ],
        List @@ item,
        { item, $InfraSceneHighlightPalette[[ 1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] } ],
      multiObjects ];
    n       = Length @ pairs;
    cyclics = With[ { c = OptionValue[ "Cyclic" ] },
      If[ ListQ[ c ], PadRight[ c, n, False ], ConstantArray[ c, n ] ] ];
    oRange = OptionValue[ "OpacityRange" ];
    tRange = OptionValue[ "ThicknessRange" ];
    pRange = OptionValue[ "PointSizeRange" ];

    vEntries = MapThread[
      { reps, color } |-> With[ { counts = Counts @ Flatten[ toVertexSet /@ reps ] },
        With[ { maxCount = Max[ Append[ Values @ counts, 1 ] ] },
          AssociationMap[
            v |-> { color, counts[ v ] / maxCount },
            Keys @ counts ] ] ],
      { pairs[[ All, 1 ]], pairs[[ All, 2 ]] } ];

    eEntries = MapThread[
      { reps, color, cyc } |-> With[ {
          counts = Counts @ Flatten @ Map[
            path |-> If[ Length @ path >= 2,
              Sort /@ Partition[ If[ cyc, Append[ path, First @ path ], path ], 2, 1 ],
              {} ],
            reps ] },
        With[ { maxCount = Max[ Append[ Values @ counts, 1 ] ] },
          AssociationMap[
            e |-> { color, counts[ e ] / maxCount },
            Keys @ counts ] ] ],
      { pairs[[ All, 1 ]], pairs[[ All, 2 ]], cyclics } ];

    HighlightGraph[ graph, Join[
      KeyValueMap[
        { e, cs } |-> With[ {
            w      = Mean @ cs[[ All, 2 ]],
            shades = Lighter[ #[[ 1 ]], 1 - #[[ 2 ]] ] & /@ cs },
          Style[ UndirectedEdge @@ e, Directive[
            If[ Length @ shades == 1, First @ shades, Blend @ shades ],
            Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) w ],
            AbsoluteThickness[ tRange[[ 1 ]] + ( tRange[[ 2 ]] - tRange[[ 1 ]] ) w ] ] ] ],
        Merge[ eEntries, Identity ] ],
      KeyValueMap[
        { v, cs } |-> With[ {
            w      = Mean @ cs[[ All, 2 ]],
            shades = Lighter[ #[[ 1 ]], 1 - #[[ 2 ]] ] & /@ cs },
          Style[ v, Directive[
            If[ Length @ shades == 1, First @ shades, Blend @ shades ],
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
              applyPathSpaceSelector[ g, FindSegment[ g, p1, p2, All ],
                sel, <| "Cyclic" -> False, "Endpoints" -> { p1, p2 } |> ],
              UpTo[ n ] ] ] },
        EventHandler[
          HighlightGraph[
            InfraSceneHighlight[ g, { segments -> $InfraSegmentColor } ],
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


SphereViewer[ g_Graph ] :=
  With[ {
      initPt      = RandomChoice[ VertexList[ g ] ],
      nearestFunc = Nearest[ GraphEmbedding[ g ] -> VertexList[ g ] ],
      selOpts     = $InfraSphereSelectOptions,
      diam        = Max[ GraphDiameter[ g ], 2 ] },
    Manipulate[
      seed;
      With[ {
          spheres = If[ r < 1, {},
            Take[
              applyPathSpaceSelector[ g, FindSphere[ g, p, r, All ],
                sel, <| "Cyclic" -> True, "Center" -> p, "Radius" -> r |> ],
              UpTo[ n ] ] ] },
        EventHandler[
          HighlightGraph[
            InfraSceneHighlight[ g, { spheres -> $InfraSphereColor }, "Cyclic" -> True ],
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
      { { n, 12, "Spheres" }, 1, 12, 1, Appearance -> "Labeled" },
      { { sel, None, "Select (ambiguity resolver)" }, selOpts, ControlType -> SetterBar },
      Button[ "Resample", p = RandomChoice[ VertexList[ g ] ]; seed++ ],
      TrackedSymbols :> { p, seed, r, n, sel },
      SaveDefinitions -> True
    ]
  ]
