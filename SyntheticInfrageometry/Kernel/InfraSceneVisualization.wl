Package["WolframInstitute`SyntheticInfrageometry`"]

PackageExport[$InfraPointColor]
PackageExport[$InfraSegmentColor]
PackageExport[$InfraLineColor]
PackageExport[$InfraShellColor]
PackageExport[$InfraBallColor]
PackageExport[$InfraPlaneColor]
PackageExport[$InfraCircleColor]
PackageExport[$InfraRayColor]
PackageExport[$InfraTopologyColor]
PackageExport[$InfraPalette]
PackageExport[$InfraStrikeOutPalette]
PackageExport[$InfraPointSizes]
PackageExport[$InfraAccentPointSize]
PackageScope[$infraColors]
PackageScope[$infraShapeColors]
PackageScope[$InfraOpacityRange]
PackageScope[$InfraEdgeThickness]
PackageScope[$InfraPointSize]
PackageScope[$InfraSceneImageSize]
PackageScope[infraInk]
PackageScope[parseHighlightStyle]
PackageScope[normalizeHighlightSpec]


(* ===================== Palette ===================== *)

(* the one place an object colour is written down; a literal rather than a shipped asset, so $InfraPointColor does not depend on file I/O at load time *)

$infraColors = <|
  "Point"    -> RGBColor[ 0.95, 0.08, 0.08 ],
  "Segment"  -> RGBColor[ 0.92, 0.45, 0.30 ],
  "Line"     -> RGBColor[ 0.78, 0.35, 0.22 ],
  "Shell"    -> RGBColor[ 0.30, 0.70, 0.50 ],
  "Ball"     -> RGBColor[ 0.55, 0.80, 0.65 ],
  "Plane"    -> RGBColor[ 0.55, 0.45, 0.80 ],
  "Circle"   -> RGBColor[ 0.20, 0.55, 0.65 ],
  "Ray"      -> RGBColor[ 0.95, 0.65, 0.45 ],
  "Path"     -> RGBColor[ 0.85, 0.62, 0.32 ],
  "Topology" -> RGBColor[ 0.85, 0.55, 0.75 ]
|>;

(* which colour each SHAPE class is drawn in when the addition-order palette is switched off.  Seven of the ten named colours are unreachable this way: they name a construction, and a construction is not recoverable from its carrier -- a ball and a bisecting hyperplane are the same vertex list.  They stay in $infraColors as the palette a caller cites by name *)
$infraShapeColors = <|
  "Point" -> "Point", "Density" -> "Point",
  "Set" -> "Ball", "SetFamily" -> "Ball",
  "Walk" -> "Path", "Polyline" -> "Path", "PolylineFamily" -> "Path"
|>;

$InfraPointColor    = $infraColors[ "Point" ];
$InfraSegmentColor  = $infraColors[ "Segment" ];
$InfraLineColor     = $infraColors[ "Line" ];
$InfraShellColor    = $infraColors[ "Shell" ];
$InfraBallColor     = $infraColors[ "Ball" ];
$InfraPlaneColor    = $infraColors[ "Plane" ];
$InfraCircleColor   = $infraColors[ "Circle" ];
$InfraRayColor      = $infraColors[ "Ray" ];
$InfraWalkColor     = $infraColors[ "Path" ];
$InfraTopologyColor = $infraColors[ "Topology" ];

$InfraPalette := Dataset @ KeyValueMap[
  { name, color } |-> <|
    "Primitive" -> name,
    "Color" -> color,
    "Symbol" -> "$Infra" <> name <> "Color",
    "Shapes" -> Keys @ Select[ $infraShapeColors, # === name & ] |>,
  $infraColors ]

$InfraOpacityRange  = { 0.40, 1.0 };
$InfraEdgeThickness = 9.0;
(* at 14 a single-realisation point swallowed several mesh cells on a Medium plane *)
$InfraPointSize     = 6;

(* one absolute value per class, independent of the graph, with three-pixel gaps so the classes stay distinguishable; the accent is a separate role, not a class *)
$InfraPointSizes      = <| Small -> 4, Medium -> 7, Large -> 10 |>;
$InfraAccentPointSize = 12;

$InfraSceneImageSize = Medium;

(* colours belong to the ORDER objects are added to a scene, not to object types *)
$InfraStrikeOutPalette := ColorData[ 112, "ColorList" ];


(* ===================== Per-object style spec ===================== *)

(* True resolves to Arrowheads[Medium], a symbolic size that scales with the plot rather than with the stroke *)
resolveArrowSpec[ spec_ ] := Replace[ spec, {
  Automatic | None | False -> None,
  True :> Arrowheads[ Medium ],
  a_Arrowheads :> a,
  other_ :> Arrowheads[ other ] } ]

parseHighlightStyle[ spec_, defaults_Association ] :=
  Replace[
    Fold[
      { rec, elem } |-> Replace[ elem, {
        ( VertexStyle         -> v_ ) :> MapAt[ Append[ #, v ] &, rec, "VertexDir" ],
        ( VertexSize          -> v_ ) :> Append[ rec, "VertexSize" -> v ],
        ( VertexShapeFunction -> v_ ) :> Append[ rec, "VertexShapeFunction" -> v ],
        ( EdgeStyle           -> v_ ) :> Append[ rec, "EdgeStyle" -> v ],
        ( EdgeShapeFunction   -> v_ ) :> Append[ rec, "EdgeShapeFunction" -> v ],
        ( ( k : "OpacityRange" | "ThicknessRange" | "PointSizeRange" ) -> v_ ) :> Append[ rec, k -> v ],
        ( d : ( _Thickness | _AbsoluteThickness | Thick | Thin | _Dashing | Dashed | Dotted | DotDashed ) ) :>
          MapAt[ Append[ #, d ] &, rec, "EdgeDir" ],
        ( d : ( _PointSize | _AbsolutePointSize ) ) :>
          MapAt[ Append[ #, d ] &, rec, "VertexDir" ],
        (* an object's own arrowhead, True on and False off: caught here so it reaches the stroke instead of being buried in a vertex/edge Directive, where it would do nothing *)
        ( a : ( _Arrowheads | True | False ) ) :> Append[ rec, "Arrowheads" -> resolveArrowSpec[ a ] ],
        d_ :> MapAt[ Append[ #, d ] &, MapAt[ Append[ #, d ] &, rec, "VertexDir" ], "EdgeDir" ]
      } ],
      Join[ defaults, <|
        "VertexDir" -> { }, "EdgeDir" -> { }, "EdgeStyle" -> None,
        "EdgeShapeFunction" -> None, "VertexSize" -> None, "VertexShapeFunction" -> None |> ],
      normalizeHighlightSpec @ spec ],
    (* an explicit appearance directive supersedes the matching count-driven diffusion: suppress the *Range so the user's value is the only one emitted on that channel *)
    r_Association :> With[ {
        edgeThick  = ! FreeQ[ { r[ "EdgeDir" ], r[ "EdgeStyle" ] },
          Thickness | AbsoluteThickness | Thick | Thin ],
        vertPtSize = ! FreeQ[ r[ "VertexDir" ], _PointSize | _AbsolutePointSize ] || r[ "VertexSize" ] =!= None,
        anyOpacity = ! FreeQ[ { r[ "VertexDir" ], r[ "EdgeDir" ], r[ "EdgeStyle" ] }, _Opacity ] },
      Join[ r, <|
        "VertexDir" -> Directive @@ r[ "VertexDir" ],
        "EdgeDir"   -> Directive @@ r[ "EdgeDir" ],
        If[ edgeThick,  "ThicknessRange" -> None, Nothing ],
        If[ vertPtSize, "PointSizeRange" -> None, Nothing ],
        If[ anyOpacity, "OpacityRange"   -> None, Nothing ] |> ] ] ]

normalizeHighlightSpec[ Automatic ]          := { }
normalizeHighlightSpec[ list_List ]          := list
normalizeHighlightSpec[ Directive[ d___ ] ]  := { d }
normalizeHighlightSpec[ x_ ]                 := { x }


(* ===================== The ink table ===================== *)

(* ONE ROW PER SHAPE CLASS -- what an object contributes to each rendering channel, read off its shape by inkClass and off nothing else.  "Verts" / "Edges" are raw occupation counts and "Norm" the heaviest mass they are divided by, so a channel encodes relative mass within the object; "Strokes" are the vertex sequences drawn as one joined line, "Dots" whether the point-size channel is on, "Knots" the vertices drawn as points on top of the sides *)

infraInk[ graph_Graph, obj_ ] := With[ { class = inkClass[ graph, obj ] },
  Join[
    <| "Class" -> class, "Verts" -> toDensity[ graph, obj ], "Edges" -> <||>,
       "Norm" -> 1, "Strokes" -> { }, "Dots" -> False, "Knots" -> { } |>,
    Switch[ class,
      "Point",
        <| "Dots" -> True |>,
      "Density",
        <| "Dots" -> True, "Norm" -> infraNumReps @ obj |>,
      "Set" | "SetFamily",
        <| "Edges" -> infraEdgeMultiset[ graph, obj ], "Norm" -> infraNumReps @ obj |>,
      "Walk",
        <| "Edges"   -> infraEdgeMultiset[ graph, walkGraphs @ obj ],
           "Norm"    -> infraNumReps @ walkGraphs @ obj,
           "Strokes" -> Catenate[ If[ bundleQ @ #, { }, walkRealisations @ # ] & /@ walkGraphs @ obj ] |>,
      "Polyline" | "PolylineFamily",
        With[ { chains = If[ MatchQ[ obj, { { __Graph } .. } ], obj, { obj } ] },
          { verts = Merge[ Counts /@ polylineToVertexSeqs @ chains, Total ] },
          <| "Verts"   -> verts,
             "Edges"   -> Merge[ infraEdgeMultiset[ graph, # ] & /@ chains, Total ],
             (* the heaviest mass, not the number of chains: a polyline that retraces a side visits its vertices twice, and a fraction above 1 lerps the opacity past opaque *)
             "Norm"    -> Max[ 1, Max @ Values @ verts ],
             "Strokes" -> polylineToVertexSeqs @ chains,
             (* a closed chain repeats its first knot at the end, so the corner set drops the repeat *)
             "Knots"   -> Catenate[
               Replace[ polylineToKnots @ #, ks_List /; First @ ks === Last @ ks :> Most @ ks ] & /@ chains ] |> ] ] ] ]


(* ===================== InfraSceneHighlight ===================== *)


(* a channel value is None, a scalar base measure t -- a fuzzy object distributes it as t * count/norm, conserving the total measure across realisations -- or a {min, max} envelope interpolated by weight, whose floor keeps rare elements visible *)
Options[ InfraSceneHighlight ] = Join[
  {
    "OpacityRange"   :> $InfraOpacityRange,
    "ThicknessRange" :> $InfraEdgeThickness,
    "PointSizeRange" -> Automatic,
    "Arrowheads"     -> Automatic,
    "Palette"        -> Automatic,
    ImageSize        :> $InfraSceneImageSize
  },
  Options[ HighlightGraph ]
];

InfraSceneHighlight[ graph_Graph, obj : Except[_List], opts : OptionsPattern[] ] :=
  InfraSceneHighlight[ graph, { obj }, opts ]

InfraSceneHighlight[ graph_Graph, multiObjects_List, opts : OptionsPattern[] ] :=
  Module[ { entries, ranges, vEntries, eEntries, objects, arrowSpec, palette },

    (* one head per path object, at its end, the value doubling as the head spec; the option is the default every object inherits, and an Arrowheads in an object's own style overrides it for that object alone *)
    arrowSpec = resolveArrowSpec @ OptionValue[ "Arrowheads" ];

    (* colour by addition order, None restoring the shape-keyed behaviour; an explicit obj -> colour is parsed before this runs, so a caller's own colour still wins *)
    palette = Replace[ OptionValue[ "Palette" ], {
      Automatic :> $InfraStrikeOutPalette,
      None -> None,
      list_List /; Length[ list ] > 0 :> list,
      other_ :> { other } } ];

    objects = DeleteCases[
      Replace[ #, Style[ obj_, dirs__ ] :> ( obj -> Directive[ dirs ] ) ] & /@ multiObjects,
      $Failed | ( $Failed -> _ ) | ( _ -> $Failed ) | { } ];

    ranges = <|
      "OpacityRange"   -> OptionValue[ "OpacityRange" ],
      "ThicknessRange" -> OptionValue[ "ThicknessRange" ],
      "PointSizeRange" -> OptionValue[ "PointSizeRange" ],
      "Arrowheads"     -> arrowSpec |>;

    entries = MapIndexed[
      { item, idx } |-> With[ {
          obj    = If[ MatchQ[ item, _Rule ], First @ item, item ],
          record = parseHighlightStyle[ If[ MatchQ[ item, _Rule ], Last @ item, Automatic ], ranges ] },
        { ink = infraInk[ graph, obj ] },
        Join[ ink, <|
          "Color" -> If[ palette === None,
            $infraColors @ $infraShapeColors @ ink[ "Class" ],
            palette[[ 1 + Mod[ First @ idx - 1, Length @ palette ] ]] ],
          "Record" -> Append[ record, "PointSizeRange" -> Replace[ record[ "PointSizeRange" ],
            Automatic :> If[ ink[ "Dots" ], $InfraPointSize, None ] ] ] |> ] ],
      objects ];

    (* the knots of a polyline and the corners of a polygon ride on top of the sides as ordinary point ink, so the subdivision and the defining corners stay visible; appended last, so no earlier object's palette slot moves *)
    entries = Join[ entries,
      Cases[ entries, e_Association /; e[ "Knots" ] =!= { } :>
        With[ { record = parseHighlightStyle[ $InfraPointColor, ranges ] },
          { knots = KeySort @ Counts @ e[ "Knots" ] },
          <| "Class" -> "Density", "Verts" -> knots, "Edges" -> <||>,
             (* a family shares its corners, so the knot masses are counts over the members like any other density *)
             "Norm" -> Max @ Values @ knots, "Strokes" -> { }, "Dots" -> True, "Knots" -> { },
             "Color" -> $InfraPointColor,
             "Record" -> Append[ record,
               "PointSizeRange" -> Replace[ record[ "PointSizeRange" ], Automatic :> $InfraPointSize ] ] |> ] ] ];

    vEntries = Map[
      e |-> AssociationMap[
        v |-> { e[ "Color" ], e[ "Verts" ][ v ] / e[ "Norm" ], e[ "Record" ] },
        Keys @ e[ "Verts" ] ],
      entries ];

    eEntries = Map[
      e |-> AssociationMap[
        edge |-> { e[ "Color" ], e[ "Edges" ][ edge ] / e[ "Norm" ], e[ "Record" ] },
        Keys @ e[ "Edges" ] ],
      entries ];

    (* colour and opacity ride per-element Style[] specs; thickness and point size are rerouted to top-level EdgeStyle / VertexShapeFunction, which HighlightGraph silently ignores inside Style[] *)
    With[ { lerp = { spec, w } |-> If[ ListQ @ spec, spec[[ 1 ]] + ( spec[[ 2 ]] - spec[[ 1 ]] ) w, spec w ] },
      {
          (* all edge styling rides top-level EdgeStyle: HighlightGraph gives a highlight Style priority over EdgeStyle and drops AbsoluteThickness inside it, so an edge listed both ways renders at default thickness *)
          edgeData = KeyValueMap[
            { e, cs } |-> With[ { ue = UndirectedEdge @@ e, last = Last @ cs },
              { color = last[[ 1 ]], w = last[[ 2 ]], rec = last[[ 3 ]] },
              { oList = If[ rec[ "OpacityRange" ] === None, { },
                  { Opacity[ lerp[ rec[ "OpacityRange" ], w ] ] } ],
                tList = If[ rec[ "ThicknessRange" ] === None, { },
                  { AbsoluteThickness[ lerp[ rec[ "ThicknessRange" ], w ] ] } ],
                eDirs = List @@ rec[ "EdgeDir" ] },
              <|
                "EdgeStyle" -> ( ue -> Directive[ color, Sequence @@ oList, Sequence @@ tList, Sequence @@ eDirs,
                    Sequence @@ If[ rec[ "EdgeStyle" ] === None, { }, { rec[ "EdgeStyle" ] } ] ] ),
                "EdgeShapeFunction" -> If[ rec[ "EdgeShapeFunction" ] === None, Nothing,
                  ue -> rec[ "EdgeShapeFunction" ] ]
              |> ],
            Merge[ eEntries, Identity ] ],
          vertexData = KeyValueMap[
            { v, cs } |-> With[ { last = Last @ cs },
              { color = last[[ 1 ]], w = last[[ 2 ]], rec = last[[ 3 ]] },
              { oList = If[ rec[ "OpacityRange" ] === None, { },
                  { Opacity[ lerp[ rec[ "OpacityRange" ], w ] ] } ],
                vDirs = List @@ rec[ "VertexDir" ] },
              Which[
                rec[ "VertexShapeFunction" ] =!= None,
                  <| "VSF" -> ( v -> rec[ "VertexShapeFunction" ] ) |>,
                (* point sizing is rerouted to a top-level VertexShapeFunction, since HighlightGraph drops it inside Style[] specs *)
                rec[ "PointSizeRange" ] =!= None || ! FreeQ[ vDirs, _AbsolutePointSize | _PointSize ],
                  With[ { body = Flatten[ { color, oList,
                      If[ rec[ "PointSizeRange" ] === None, { },
                        { AbsolutePointSize[ lerp[ rec[ "PointSizeRange" ], w ] ] } ], vDirs } ] },
                    <| "VSF" -> ( v -> ( Append[ body, Point[ #1 ] ] & ) ) |> ],
                True,
                  <| "Style" -> Style[ v, Directive[ color, Sequence @@ oList, Sequence @@ vDirs ] ],
                     "VSize" -> If[ rec[ "VertexSize" ] === None, Nothing, v -> rec[ "VertexSize" ] ] |>
              ] ],
            Merge[ vEntries, Identity ] ] },
        {
          coords    = AssociationThread[ VertexList @ graph -> GraphEmbedding @ graph ],
          edgeStyle = Association @ Cases[ edgeData, kv_Association :> kv[ "EdgeStyle" ] ]
        },
        (* a walk is one stroke: HighlightGraph draws each edge separately with a butt cap and ignores a CapForm / JoinForm in the edge directive, so a bend leaves a wedge of background bitten out of the ribbon.  The ink table hands over the vertex sequences to stroke -- closed ones already closed, a branching DAG contributing none, since it stands for many walks with no single stroke.  Each maximal run of equal-styled consecutive steps is redrawn as one joined Line, carried by the EdgeShapeFunction of its first unclaimed edge; each edge takes at most one rule, since Graph keeps only the first *)
        {
          strokes = Catenate @ Cases[ entries,
            e_Association /; e[ "Record" ][ "EdgeShapeFunction" ] === None :>
              Catenate @ Map[
                walk |-> With[ {
                    runs = Select[ SplitBy[ Partition[ walk, 2, 1 ], edgeStyle[ UndirectedEdge @@ Sort @ # ] & ],
                      Length[ # ] >= 2 & ] },
                  MapIndexed[
                    { steps, position } |-> { UndirectedEdge @@ Sort @ # & /@ steps,
                                coords /@ Prepend[ Last /@ steps, First @ First @ steps ],
                                First[ position ] === Length[ runs ],
                                e[ "Record" ][ "Arrowheads" ] },
                    runs ] ],
                Select[ e[ "Strokes" ], Length[ # ] >= 2 & ] ] ]
        },
        {
          joinRules = Last @ Fold[
            { state, stroke } |-> Replace[ DeleteDuplicates @ Select[ First @ stroke, ! KeyExistsQ[ First @ state, # ] & ], {
                { } -> state,
                fresh_ :> {
                  Join[ First @ state, AssociationThread[ fresh -> True ] ],
                  Join[ Last @ state,
                    { First @ fresh -> ( { JoinForm[ "Round" ],
                        If[ stroke[[ 4 ]] =!= None && stroke[[ 3 ]],
                          Sequence @@ { stroke[[ 4 ]], Arrow @ stroke[[ 2 ]] },
                          Line @ stroke[[ 2 ]] ] } & ) },
                    ( # -> ( { } & ) ) & /@ Rest @ fresh ] } } ],
            { <| |>, { } },
            strokes ]
        },

        HighlightGraph[ graph,
          Cases[ vertexData, kv_Association /; KeyExistsQ[ kv, "Style" ] :> kv[ "Style" ] ],
          Sequence @@ DeleteCases[ {
            EdgeStyle           -> DeleteCases[ Cases[ edgeData,   kv_Association :> kv[ "EdgeStyle" ] ], Nothing ],
            EdgeShapeFunction   -> Join[ DeleteCases[ Cases[ edgeData, kv_Association :> kv[ "EdgeShapeFunction" ] ], Nothing ], joinRules ],
            VertexShapeFunction -> Cases[ vertexData, kv_Association /; KeyExistsQ[ kv, "VSF" ] :> kv[ "VSF" ] ],
            VertexSize          -> DeleteCases[ Cases[ vertexData, kv_Association /; KeyExistsQ[ kv, "VSize" ] :> kv[ "VSize" ] ], Nothing ]
          }, _ -> { } ],
          FilterRules[ { opts }, Options @ HighlightGraph ],
          ImageSize -> OptionValue[ ImageSize ] ]
    ]
  ]
