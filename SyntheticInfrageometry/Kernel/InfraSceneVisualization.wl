Package["WolframInstitute`SyntheticInfrageometry`"]

PackageExport[$InfraPointColor]
PackageExport[$InfraSegmentColor]
PackageExport[$InfraLineColor]
PackageExport[$InfraShellColor]
PackageExport[$InfraBallColor]
PackageExport[$InfraPlaneColor]
PackageExport[$InfraCircleColor]
PackageExport[$InfraRayColor]
PackageExport[$InfraObjectColor]
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
  "Object"   -> RGBColor[ 0.55, 0.70, 0.85 ],
  "Topology" -> RGBColor[ 0.85, 0.55, 0.75 ]
|>;

(* which colour each wrapper head is drawn in; several wrappers deliberately share one *)
$infraHeadColors = <|
  InfraPoint -> "Point", InfraEffectivePoint -> "Point",
  InfraSegment -> "Segment", InfraPolyline -> "Segment",
  InfraLine -> "Line",
  InfraWalk -> "Path", InfraLoop -> "Path", InfraString -> "Path",
  InfraShell -> "Shell", InfraEllipticShell -> "Shell", InfraSet -> "Shell",
  InfraBall -> "Ball",
  InfraPlane -> "Plane",
  InfraCircle -> "Circle", InfraEllipse -> "Circle", InfraPolygon -> "Circle", InfraTriangle -> "Circle",
  InfraRay -> "Ray",
  InfraObject -> "Object"
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
$InfraObjectColor   = $infraColors[ "Object" ];
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

    (* one head per path object, at its end, the value doubling as the head spec; True resolves to Arrowheads[Medium], a symbolic size that scales with the plot rather than with the stroke *)
    arrowSpec = Replace[ OptionValue[ "Arrowheads" ], {
      Automatic | None | False -> None,
      True :> Arrowheads[ Medium ],
      a_Arrowheads :> a,
      other_ :> Arrowheads[ other ] } ];

    (* colour by addition order, None restoring the type-keyed behaviour; an explicit obj -> colour is parsed before this runs, so a caller's own colour still wins *)
    palette = Replace[ OptionValue[ "Palette" ], {
      Automatic :> $InfraStrikeOutPalette,
      None -> None,
      list_List /; Length[ list ] > 0 :> list,
      other_ :> { other } } ];

    objects = DeleteCases[
      Replace[ #, {
        Style[ obj_, dirs__ ] :> ( obj -> Directive[ dirs ] ),
        list_List /; Length[ list ] > 0 && SameQ @@ (Head /@ list) &&
            MatchQ[ First @ list, _[ _List ] ] :>
          Head[ First @ list ][ Join @@ list[[ All, 1 ]] ] } ] & /@ multiObjects,
      _[ $Failed ] | ( _[ $Failed ] -> _ ) | ( _ -> _[ $Failed ] ) | { } ];

    ranges = <|
      "OpacityRange"   -> OptionValue[ "OpacityRange" ],
      "ThicknessRange" -> OptionValue[ "ThicknessRange" ],
      "PointSizeRange" -> OptionValue[ "PointSizeRange" ] |>;
    defaultRecord = parseHighlightStyle[ Automatic, ranges ];

    (* the original wrapper rides along as a fifth element so the density computation can read its measure uniformly across bundle, weighted and DAG forms *)
    triples = MapIndexed[
      { item, idx } |-> With[ {
          obj    = If[ MatchQ[ item, _Rule ], First @ item, item ],
          record = parseHighlightStyle[ If[ MatchQ[ item, _Rule ], Last @ item, Automatic ], ranges ] },
        Append[ If[ MatchQ[ Head @ obj, InfraPoint | InfraEffectivePoint | InfraObject | InfraSet | $infraBundleHeads ], obj, None ] ] @
        Replace[
          { obj, If[ palette === None,
              Lookup[ $infraColors, Lookup[ $infraHeadColors, Head @ obj, None ],
                $InfraSceneHighlightPalette[[
                  1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ],
              palette[[ 1 + Mod[ First @ idx - 1, Length @ palette ] ]] ],
            record },
          {
            (* density = mass / total mass, so a sharp effective point draws full size and a spread one fades *)
            { InfraEffectivePoint[ m_Association ], c_, u_ } :> { Keys @ m, c, "Points", u },
            { InfraPoint   [ v_ ], c_, u_ } :> { { v }, c, "Points", u },
            (* a plain List of atoms is what the point finders return: it must flow into the scene with no glue *)
            { list : { __InfraPoint }, c_, u_ } :> { #[[ 1 ]] & /@ list, c, "Points", u },
            { InfraSegment [ dag_Graph ], c_, u_ } :> { { dag }, c, "Paths" , u },
            { InfraSegment [ b : { __Graph } ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraSegment [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraLine    [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraWalk    [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraLoop    [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraString  [ b_List ], c_, u_ } :> { b, c, "Cycles", u },
            { InfraShell        [ b_List ], c_, u_ } :> { b, c, "Sets"  , u },
            { InfraBall         [ b_List ], c_, u_ } :> { b, c, "Sets"  , u },
            { InfraEllipticShell[ b_List ], c_, u_ } :> { b, c, "Sets"  , u },
            { InfraPlane        [ b_List ], c_, u_ } :> { b, c, "Sets"  , u },
            { InfraCircle       [ b_List ], c_, u_ } :> { b, c, "Cycles", u },
            { InfraEllipse      [ b_List ], c_, u_ } :> { b, c, "Cycles", u },
            { InfraPolygon      [ b_List ], c_, u_ } :> { polylineToVertexSeqs[ b ], c, "Cycles", u },
            { InfraTriangle     [ b_List ], c_, u_ } :> { polylineToVertexSeqs[ b ], c, "Cycles", u },
            { InfraRay     [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraObject  [ b_List ], c_, u_ } :> { { b }, c, "Sets", u },
            { InfraPolyline[ b_List ], c_, u_ } :> { polylineToVertexSeqs[ b ], c, "Paths", u },
            { InfraSet      [ b_List ], c_, u_ } :> { { b }, c, "Sets", u },
            (* a bare vertex is a legal highlight object: wrap it as a one-vertex point *)
            { b_, c_, u_ } /; MemberQ[ VertexList @ graph, b ] :> { { b }, c, "Points", u },
            { b_, c_, u_ }                      :> { b, c, Automatic, u }
          } ] ],
      objects ];

    (* the knots are drawn on top of the path so the subdivision is visible *)
    knotTriples = Cases[ objects,
      ( InfraPolyline[ b_List ] | ( InfraPolyline[ b_List ] -> _ ) ) :>
        { polylineToKnotVertices[ b ], $InfraPointColor, "PointSet", defaultRecord, None } ];

    (* corner vertices as points, so the defining corners stand out from the geodesic sides *)
    knotTriples = Join[ knotTriples, Cases[ objects,
      ( ( InfraPolygon | InfraTriangle )[ b_List ] |
        ( ( InfraPolygon | InfraTriangle )[ b_List ] -> _ ) ) :>
        { Map[ Most @ polylineToKnots[ # ] &, b ], $InfraPointColor, "PointSet", defaultRecord, None } ] ];

    triples = Join[ triples, knotTriples ];

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
        (* a walk is one stroke: HighlightGraph draws each edge separately with a butt cap and ignores a CapForm / JoinForm in the edge directive, so a bend leaves a wedge of background bitten out of the ribbon.  Each maximal run of equal-styled consecutive steps is redrawn as one joined Line, carried by the EdgeShapeFunction of its first unclaimed edge; each edge takes at most one rule, since Graph keeps only the first *)
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
                                First[ position ] === Length[ runs ] },
                    runs ] ],
                Replace[ Cases[ reps, r_List /; Length[ r ] >= 2 && FreeQ[ r, _Graph ] ],
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
                        If[ arrowSpec =!= None && stroke[[ 3 ]],
                          Sequence @@ { arrowSpec, Arrow @ stroke[[ 2 ]] },
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
