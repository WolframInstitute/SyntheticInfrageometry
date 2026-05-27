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
PackageScope[$InfraSceneHighlightPalette]
PackageScope[$InfraOpacityRange]
PackageScope[$InfraThicknessRange]
PackageScope[$InfraPointSizeRange]
PackageScope[parseHighlightStyle]
PackageScope[normalizeHighlightSpec]


$InfraPointColor   = RGBColor[ 0.95, 0.08, 0.08 ];
$InfraSegmentColor = RGBColor[ 0.92, 0.45, 0.30 ];
$InfraLineColor    = RGBColor[ 0.78, 0.35, 0.22 ];
$InfraShellColor   = RGBColor[ 0.30, 0.70, 0.50 ];
$InfraBallColor    = RGBColor[ 0.55, 0.80, 0.65 ];
$InfraPlaneColor   = RGBColor[ 0.55, 0.45, 0.80 ];
$InfraCircleColor  = RGBColor[ 0.20, 0.55, 0.65 ];
$InfraRayColor     = RGBColor[ 0.95, 0.65, 0.45 ];
$InfraPathColor    = RGBColor[ 0.85, 0.62, 0.32 ];
$InfraObjectColor   = RGBColor[ 0.55, 0.70, 0.85 ];
$InfraTopologyColor = RGBColor[ 0.85, 0.55, 0.75 ];

$InfraOpacityRange   = { 0.40, 1.0 };
$InfraThicknessRange = { 1.0, 5.0 };
$InfraPointSizeRange = { 6, 14 };

$InfraSceneHighlightPalette := Join[
  { $InfraSegmentColor, $InfraShellColor, $InfraCircleColor, $InfraPointColor, $InfraRayColor },
  Table[ ColorData[ "DarkRainbow" ][ k / 5 ], { k, 1, 5 } ]
];


(* ===================== Per-object style spec ===================== *)

(* Parse the RHS of a per-object `obj -> spec` entry into a style record.
   `spec` is a colour / Directive[...] (applied to both channels), or a flat
   list whose entries are auto-classified by key: VertexStyle / VertexSize /
   VertexShapeFunction (vertex channel), EdgeStyle / EdgeShapeFunction (edge
   channel), "OpacityRange" / "ThicknessRange" / "PointSizeRange" (per-object
   diffusion overrides), and any bare directive (both colour channels).  The
   three ranges default to the global option values passed in `defaults`. *)
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
        d_ :> MapAt[ Append[ #, d ] &, MapAt[ Append[ #, d ] &, rec, "VertexDir" ], "EdgeDir" ]
      } ],
      Join[ defaults, <|
        "VertexDir" -> { }, "EdgeDir" -> { }, "EdgeStyle" -> None,
        "EdgeShapeFunction" -> None, "VertexSize" -> None, "VertexShapeFunction" -> None |> ],
      normalizeHighlightSpec @ spec ],
    r_Association :> Join[ r, <|
      "VertexDir" -> Directive @@ r[ "VertexDir" ],
      "EdgeDir"   -> Directive @@ r[ "EdgeDir" ] |> ] ]

normalizeHighlightSpec[ Automatic ]          := { }
normalizeHighlightSpec[ list_List ]          := list
normalizeHighlightSpec[ Directive[ d___ ] ]  := { d }
normalizeHighlightSpec[ x_ ]                 := { x }


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
     InfraShell  [ {set1, set2, ...} ]       -- induced subgraph edges
     InfraPlane  [ {set1, set2, ...} ]       -- induced subgraph edges
     InfraCircle [ {cyc1, cyc2, ...} ]       -- sequential edges + auto-closure
     InfraRay    [ {ray1, ray2, ...} ]       -- sequential edges (Partition)

   The arg shape (a single List) selects the rendering interpretation; the
   scene-construction shapes of these heads (e.g. `InfraSegment[p1, p2]`,
   `InfraShell[c, r]`, `InfraPlane[p1, p2]`, `InfraCircle[c, r]`) take more
   args and never collide.
   Each entry may be plain, `entry -> color`, `entry -> Directive[dirs]`,
   `Style[entry, dirs__]`, or `entry -> {opts...}` with a flat option list that
   `parseHighlightStyle` auto-sorts into three channels: vertex appearance
   (`VertexStyle` / `VertexSize` / `VertexShapeFunction`), edge appearance
   (`EdgeStyle` / `EdgeShapeFunction`), and per-object diffusion ranges
   (`"OpacityRange"` / `"ThicknessRange"` / `"PointSizeRange"`, overriding the
   global option for that object only).  A colour / `Directive` / bare directive
   applies to both vertex and edge colour channels.  Colour + opacity ride the
   per-element `Style[]` highlight specs; sizing / thickness are rerouted to
   top-level `VertexShapeFunction` / `VertexSize` / `EdgeStyle` /
   `EdgeShapeFunction` rule lists because HighlightGraph ignores
   `AbsolutePointSize` / `AbsoluteThickness` inside Style[] highlight specs. *)

(* Diffuse-encoding ranges.  "OpacityRange" and "ThicknessRange" are on by
   default, so multi-realisation visit counts are visible out of the box via
   opacity gradient on vertices/edges and thickness gradient on edges.
   "PointSizeRange" defaults to None, which suppresses the AbsolutePointSize
   directive entirely so highlighted vertices inherit the underlying graph's
   point size.  Opt into the point-size channel by setting "PointSizeRange"
   to a {min, max} pair (e.g. $InfraPointSizeRange recovers the previous
   default); set any *Range to None to suppress that channel. *)
Options[ InfraSceneHighlight ] = Join[
  {
    "OpacityRange"   :> $InfraOpacityRange,
    "ThicknessRange" :> $InfraThicknessRange,
    "PointSizeRange" -> None
  },
  Options[ HighlightGraph ]
];

InfraSceneHighlight[ graph_Graph, obj : Except[_List], opts : OptionsPattern[] ] :=
  InfraSceneHighlight[ graph, { obj }, opts ]

InfraSceneHighlight[ graph_Graph, multiObjects_List, opts : OptionsPattern[] ] :=
  Module[ { triples, knotTriples, ranges, defaultRecord, vEntries, eEntries, objects },

    (* Normalise each item: unwrap Style[obj, dirs__] into obj -> Directive[dirs];
       merge {InfraX[{r1}],...} into InfraX[{r1,...}];
       then strip $Failed / empty entries. *)
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

    triples = MapIndexed[
      { item, idx } |-> With[ {
          obj    = If[ MatchQ[ item, _Rule ], First @ item, item ],
          record = parseHighlightStyle[ If[ MatchQ[ item, _Rule ], Last @ item, Automatic ], ranges ] },
        Replace[
          { obj, Switch[ Head @ obj,
              InfraPoint,    $InfraPointColor,
              InfraSegment,  $InfraSegmentColor,
              InfraLine,     $InfraLineColor,
              InfraPath,     $InfraPathColor,
              InfraLoop,     $InfraPathColor,
              InfraString,   $InfraPathColor,
              InfraShell,         $InfraShellColor,
              InfraBall,          $InfraBallColor,
              InfraEllipticShell, $InfraShellColor,
              InfraPlane,         $InfraPlaneColor,
              InfraCircle,        $InfraCircleColor,
              InfraEllipse,       $InfraCircleColor,
              InfraPolygon,       $InfraCircleColor,
              InfraTriangle,      $InfraCircleColor,
              InfraRay,           $InfraRayColor,
              InfraObject,        $InfraObjectColor,
              InfraPolyline,      $InfraSegmentColor,
              InfraSet,           $InfraShellColor,
              _,             $InfraSceneHighlightPalette[[
                               1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ],
            record },
          {
            { InfraPoint   [ b_List ], c_, u_ } :> { b, c, "Points", u },
            { InfraSegment [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraLine    [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
            { InfraPath    [ b_List ], c_, u_ } :> { b, c, "Paths" , u },
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
            { b_, c_, u_ }                      :> { b, c, Automatic, u }
          } ] ],
      objects ];

    (* Each InfraPolyline item additionally emits a knot triple (the leg
       endpoints rendered as points in $InfraPointColor).  Drawn on top of
       the path so the subdivision is visible.  *)
    knotTriples = Cases[ objects,
      ( InfraPolyline[ b_List ] | ( InfraPolyline[ b_List ] -> _ ) ) :>
        { polylineToKnotVertices[ b ], $InfraPointColor, "PointSet", defaultRecord } ];

    (* Polygon / triangle items emit their corner vertices as points, so the
       defining corners stand out from the geodesic sides. *)
    knotTriples = Join[ knotTriples, Cases[ objects,
      ( ( InfraPolygon | InfraTriangle )[ b_List ] |
        ( ( InfraPolygon | InfraTriangle )[ b_List ] -> _ ) ) :>
        { Map[ Most @ polylineToKnots[ # ] &, b ], $InfraPointColor, "PointSet", defaultRecord } ] ];

    triples = Join[ triples, knotTriples ];

    With[ {
        repVerts = { type, rep } |-> Switch[ type,
          "Points",   { rep },
          "Paths",    rep,
          "Cycles",   rep,
          "Sets",     rep,
          "PointSet", rep,
          _,          If[ MemberQ[ VertexList @ graph, rep ], { rep }, rep ]
        ],
        repEdges = { type, rep } |-> Switch[ type,
          "Points",   {},
          "Paths",    If[ Length @ rep >= 2, Sort /@ Partition[ rep, 2, 1 ], {} ],
          "Cycles",   With[ {
              closed = If[ Length @ rep >= 2 && First @ rep === Last @ rep,
                rep, Append[ rep, First @ rep ] ] },
            If[ Length @ closed >= 2, Sort /@ Partition[ closed, 2, 1 ], {} ] ],
          "Sets",     Sort /@ ( List @@@ EdgeList @ Subgraph[ graph, rep ] ),
          "PointSet", {},
          _,          If[ MemberQ[ VertexList @ graph, rep ], {},
                        Sort /@ ( List @@@ EdgeList @ Subgraph[ graph, rep ] ) ]
        ] },

      vEntries = MapThread[
        { reps, color, type, record } |-> With[ {
            counts  = Counts @ Catenate[ repVerts[ type, # ] & /@ reps ],
            numReps = Max[ Length @ reps, 1 ] },
          AssociationMap[
            v |-> { color, counts[ v ] / numReps, record },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]] } ];

      eEntries = MapThread[
        { reps, color, type, record } |-> With[ {
            counts  = Counts @ Catenate[ repEdges[ type, # ] & /@ reps ],
            numReps = Max[ Length @ reps, 1 ] },
          AssociationMap[
            e |-> { color, counts[ e ] / numReps, record },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]] } ];
    ];

    (* Colour + opacity ride per-element Style[] highlight specs (the channel
       HighlightGraph honours).  Edge thickness and vertex point-size are
       rerouted to top-level EdgeStyle / VertexShapeFunction rules because
       HighlightGraph silently ignores AbsoluteThickness / AbsolutePointSize
       inside Style[edge/vertex, ...].  A *Range record value of None
       suppresses that channel; ranges are per-object. *)
    With[ { lerp = { range, w } |-> range[[ 1 ]] + ( range[[ 2 ]] - range[[ 1 ]] ) w },
      With[ {
          edgeData = KeyValueMap[
            { e, cs } |-> With[ { ue = UndirectedEdge @@ e, last = Last @ cs },
              With[ { color = last[[ 1 ]], w = last[[ 2 ]], rec = last[[ 3 ]] },
                With[ {
                    oList = If[ rec[ "OpacityRange" ] === None, { },
                      { Opacity[ lerp[ rec[ "OpacityRange" ], w ] ] } ],
                    tList = If[ rec[ "ThicknessRange" ] === None, { },
                      { AbsoluteThickness[ lerp[ rec[ "ThicknessRange" ], w ] ] } ],
                    eDirs = List @@ rec[ "EdgeDir" ] },
                  <|
                    "Style" -> Style[ ue, Directive[ color, Sequence @@ oList, Sequence @@ eDirs ] ],
                    "EdgeStyle" -> If[ tList === { } && rec[ "EdgeStyle" ] === None, Nothing,
                      ue -> Directive[ color, Sequence @@ oList, Sequence @@ tList, Sequence @@ eDirs,
                        Sequence @@ If[ rec[ "EdgeStyle" ] === None, { }, { rec[ "EdgeStyle" ] } ] ] ],
                    "EdgeShapeFunction" -> If[ rec[ "EdgeShapeFunction" ] === None, Nothing,
                      ue -> rec[ "EdgeShapeFunction" ] ]
                  |> ] ] ],
            Merge[ eEntries, Identity ] ],
          vertexData = KeyValueMap[
            { v, cs } |-> With[ { last = Last @ cs },
              With[ { color = last[[ 1 ]], w = last[[ 2 ]], rec = last[[ 3 ]] },
                With[ {
                    oList = If[ rec[ "OpacityRange" ] === None, { },
                      { Opacity[ lerp[ rec[ "OpacityRange" ], w ] ] } ],
                    vDirs = List @@ rec[ "VertexDir" ] },
                  Which[
                    rec[ "VertexShapeFunction" ] =!= None,
                      <| "VSF" -> ( v -> rec[ "VertexShapeFunction" ] ) |>,
                    rec[ "PointSizeRange" ] =!= None,
                      With[ { body = Flatten[ { color, oList,
                          AbsolutePointSize[ lerp[ rec[ "PointSizeRange" ], w ] ], vDirs } ] },
                        <| "VSF" -> ( v -> ( Append[ body, Point[ #1 ] ] & ) ) |> ],
                    True,
                      <| "Style" -> Style[ v, Directive[ color, Sequence @@ oList, Sequence @@ vDirs ] ],
                         "VSize" -> If[ rec[ "VertexSize" ] === None, Nothing, v -> rec[ "VertexSize" ] ] |>
                  ] ] ] ],
            Merge[ vEntries, Identity ] ] },

        HighlightGraph[ graph,
          Join[
            Cases[ edgeData,   kv_Association :> kv[ "Style" ] ],
            Cases[ vertexData, kv_Association /; KeyExistsQ[ kv, "Style" ] :> kv[ "Style" ] ] ],
          Sequence @@ DeleteCases[ {
            EdgeStyle           -> DeleteCases[ Cases[ edgeData,   kv_Association :> kv[ "EdgeStyle" ] ], Nothing ],
            EdgeShapeFunction   -> DeleteCases[ Cases[ edgeData,   kv_Association :> kv[ "EdgeShapeFunction" ] ], Nothing ],
            VertexShapeFunction -> Cases[ vertexData, kv_Association /; KeyExistsQ[ kv, "VSF" ] :> kv[ "VSF" ] ],
            VertexSize          -> DeleteCases[ Cases[ vertexData, kv_Association /; KeyExistsQ[ kv, "VSize" ] :> kv[ "VSize" ] ], Nothing ]
          }, _ -> { } ],
          FilterRules[ { opts }, Options @ HighlightGraph ] ]
      ]
    ]
  ]
