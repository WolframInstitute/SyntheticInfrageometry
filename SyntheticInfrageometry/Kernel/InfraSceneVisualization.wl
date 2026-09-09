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
PackageScope[$infraHeadColors]
PackageScope[$InfraSceneHighlightPalette]
PackageScope[$InfraOpacityRange]
PackageScope[$InfraEdgeThickness]
PackageScope[$InfraPointSize]
PackageScope[$InfraSceneImageSize]
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

(* which colour each wrapper head is drawn in; several wrappers deliberately share one.  The point shape wears no head, so its colour is looked up by shape below; a walk is a Graph and takes the path colour *)
$infraHeadColors = <|
  InfraSegment -> "Segment", InfraPolyline -> "Segment",
  InfraLine -> "Line",
  Graph -> "Path",
  InfraShell -> "Shell", InfraEllipticShell -> "Shell",
  InfraBall -> "Ball",
  InfraPlane -> "Plane",
  InfraCircle -> "Circle", InfraEllipse -> "Circle", InfraPolygon -> "Circle", InfraTriangle -> "Circle",
  InfraRay -> "Ray"
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
    "Heads" -> Keys @ Select[ $infraHeadColors, # === name & ] |>,
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

$InfraSceneHighlightPalette := Join[
  { $InfraSegmentColor, $InfraShellColor, $InfraCircleColor, $InfraPointColor, $InfraRayColor },
  Table[ ColorData[ "DarkRainbow" ][ k / 5 ], { k, 1, 5 } ]
];


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


(* ===================== InfraSceneHighlight ===================== *)


(* a channel value is None, a scalar base measure t -- a fuzzy object distributes it as t * count/numReps, conserving the total measure across realisations -- or a {min, max} envelope interpolated by weight, whose floor keeps rare elements visible *)
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
  Module[ { triples, knotTriples, ranges, defaultRecord, vEntries, eEntries, objects, arrowSpec, palette },

    (* one head per path object, at its end, the value doubling as the head spec; the option is the default every object inherits, and an Arrowheads in an object's own style overrides it for that object alone *)
    arrowSpec = resolveArrowSpec @ OptionValue[ "Arrowheads" ];

    (* colour by addition order, None restoring the type-keyed behaviour; an explicit obj -> colour is parsed before this runs, so a caller's own colour still wins *)
    palette = Replace[ OptionValue[ "Palette" ], {
      Automatic :> $InfraStrikeOutPalette,
      None -> None,
      list_List /; Length[ list ] > 0 :> list,
      other_ :> { other } } ];

    (* a family of polygons -- a List of leg Lists -- draws as the bundle of its legs *)
    objects = DeleteCases[
      Replace[ #, {
        Style[ obj_, dirs__ ] :> ( obj -> Directive[ dirs ] ),
        polys : { { __Graph } .. } :> Catenate @ polys,
        ( polys : { { __Graph } .. } -> c_ ) :> ( Catenate @ polys -> c ) } ] & /@ multiObjects,
      $Failed | ( $Failed -> _ ) | ( _ -> $Failed ) | { } ];

    ranges = <|
      "OpacityRange"   -> OptionValue[ "OpacityRange" ],
      "ThicknessRange" -> OptionValue[ "ThicknessRange" ],
      "PointSizeRange" -> OptionValue[ "PointSizeRange" ],
      "Arrowheads"     -> arrowSpec |>;
    defaultRecord = parseHighlightStyle[ Automatic, ranges ];

    (* the object rides along as a fifth element so the density computation can read its occupation uniformly across densities, walk graphs and bundles *)
    triples = MapIndexed[
      { item, idx } |-> With[ {
          obj    = If[ MatchQ[ item, _Rule ], First @ item, item ],
          record = parseHighlightStyle[ If[ MatchQ[ item, _Rule ], Last @ item, Automatic ], ranges ] },
        Append[ If[ MatchQ[ obj, _Association | _Graph | { __Graph } ], obj, None ] ] @
        Replace[
          { obj, If[ palette === None,
              Lookup[ $infraColors,
                Lookup[ $infraHeadColors, Head @ obj, If[ pointQ[ graph, obj ], "Point", None ] ],
                $InfraSceneHighlightPalette[[
                  1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ],
              palette[[ 1 + Mod[ First @ idx - 1, Length @ palette ] ]] ],
            record },
          {
            (* density = mass / total mass, so a sharp point draws full size and a spread one fades *)
            { fam_Association, c_, u_ } :> { Keys @ fam, c, "Points", u },
            (* a walk graph on position pairs is drawn as its vertex sequence, a closed one as a cycle; a substrate DAG stays the compact atom *)
            { w_Graph, c_, u_ } /; closedWalkQ[ w ] :> { { walkSequence @ w }, c, "Cycles", u },
            { w_Graph, c_, u_ } /; positionSpelledQ[ w ] :> { walkRealisations @ w, c, "Paths", u },
            { w_Graph, c_, u_ } :> { { w }, c, "Paths", u },
            { ws : { __Graph }, c_, u_ } /; AllTrue[ ws, closedWalkQ ] :> { walkSequence /@ ws, c, "Cycles", u },
            { ws : { __Graph }, c_, u_ } :> { Catenate[ walkRealisations /@ ws ], c, "Paths", u },
            (* a bare vertex is a legal highlight object: wrap it as a one-vertex point *)
            { b_, c_, u_ } /; pointQ[ graph, b ] :> { { b }, c, "Points", u },
            (* a plain vertex List is a set or a point family: it flows into the scene with no glue *)
            { list_List, c_, u_ } /; SubsetQ[ VertexList @ graph, list ] :> { list, c, "Points", u },
            (* a List of vertex sets is a family of sets, each drawn with its induced edges *)
            { sets : { __List }, c_, u_ } /; SubsetQ[ VertexList @ graph, Catenate @ sets ] :> { sets, c, "Sets", u },
            { b_, c_, u_ }                      :> { b, c, Automatic, u }
          } ] ],
      objects ];

    triples = Apply[
      { reps, color, type, record, obj } |-> { reps, color, type,
        Append[ record, "PointSizeRange" -> Replace[ record[ "PointSizeRange" ],
          Automatic :> If[ MatchQ[ type, "Points" | "PointSet" ], $InfraPointSize, None ] ] ], obj },
      triples, { 1 } ];

    (* the per-type dispatch is shared with InfraMeasure (Tools.wl); only the Automatic-type branch stays local, since it needs the graph *)
    With[ {
        repVerts = { type, rep } |-> Switch[ type,
          "Points" | "Paths" | "Cycles" | "Sets" | "PointSet", infraRepVerts[ type, rep ],
          _, If[ MemberQ[ VertexList @ graph, rep ], { rep }, rep ]
        ],
        repEdges = { type, rep } |-> Switch[ type,
          "Points" | "PointSet",        { },
          "Paths" | "Cycles" | "Sets",  infraRepEdges[ graph, type, rep ],
          _, If[ MemberQ[ VertexList @ graph, rep ], { },
                Sort /@ ( List @@@ EdgeList @ Subgraph[ graph, rep ] ) ]
        ] },

      vEntries = MapThread[
        { reps, color, type, record, obj } |-> With[ {
            counts  = If[ obj =!= None, infraVertexMultiset[ obj ],
                          Counts @ Catenate[ repVerts[ type, # ] & /@ reps ] ],
            numReps = If[ obj =!= None, infraNumReps[ obj ], Max[ Length @ reps, 1 ] ],
            wts     = record[ "Weights" ] },
          { norm = If[ AssociationQ @ wts, Max @ Values @ wts, numReps ] },
          AssociationMap[
            v |-> { color, ( If[ AssociationQ @ wts, wts[ v ], counts[ v ] ] ) / norm, record },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]], triples[[ All, 5 ]] } ];

      eEntries = MapThread[
        (* DAG segments read their edge occupation off GeodesicEdgeOccupation: no enumeration *)
        { reps, color, type, record, obj } |-> With[ {
            counts  = If[ obj =!= None, infraEdgeMultiset[ graph, obj ],
                          Counts @ Catenate[ repEdges[ type, # ] & /@ reps ] ],
            numReps = If[ obj =!= None, infraNumReps[ obj ], Max[ Length @ reps, 1 ] ] },
          AssociationMap[
            e |-> { color, counts[ e ] / numReps, record },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]], triples[[ All, 5 ]] } ];
    ];

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
        (* a walk is one stroke: HighlightGraph draws each edge separately with a butt cap and ignores a CapForm / JoinForm in the edge directive, so a bend leaves a wedge of background bitten out of the ribbon.  A substrate path graph is ONE walk, so it is spelled out here by walkSequence -- cheap, no enumeration -- and gets its stroke and its end arrowhead like a position-spelled walk; a branching DAG stands for many walks with no single stroke, and stays the compact atom.  Each maximal run of equal-styled consecutive steps is redrawn as one joined Line, carried by the EdgeShapeFunction of its first unclaimed edge; each edge takes at most one rule, since Graph keeps only the first *)
        {
          strokes = Catenate @ Cases[ triples,
            { reps_, _, type : "Paths" | "Cycles", record_, _ } /; record[ "EdgeShapeFunction" ] === None :>
              Catenate @ Map[
                walk |-> With[ {
                    runs = Select[ SplitBy[ Partition[ walk, 2, 1 ], edgeStyle[ UndirectedEdge @@ Sort @ # ] & ],
                      Length[ # ] >= 2 & ] },
                  MapIndexed[
                    { steps, position } |-> { UndirectedEdge @@ Sort @ # & /@ steps,
                                coords /@ Prepend[ Last /@ steps, First @ First @ steps ],
                                First[ position ] === Length[ runs ],
                                record[ "Arrowheads" ] },
                    runs ] ],
                Replace[
                  Cases[ Replace[ reps, w_Graph /; PathGraphQ[ w ] :> walkSequence @ w, { 1 } ],
                    r_List /; Length[ r ] >= 2 && FreeQ[ r, _Graph ] ],
                  w_ :> If[ type === "Cycles" && Last[ w ] =!= First[ w ], Append[ w, First @ w ], w ], { 1 } ] ] ]
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
