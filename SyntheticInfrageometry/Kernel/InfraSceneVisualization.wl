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
   Each entry may be plain, `entry -> color`, `entry -> Directive[dirs]`, or
   `Style[entry, dirs__]`.  Any user-supplied graphics directives are appended
   *after* the computed `{color, opacity, size/thickness}` so that later
   same-type directives win (Wolfram's `Directive` semantics), letting the
   caller override colour, opacity, `AbsolutePointSize`, `AbsoluteThickness`,
   etc. on a per-object basis. *)

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
  Module[ { triples, knotTriples, oRange, tRange, pRange, vEntries, eEntries, objects },

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

    triples = MapIndexed[
      { item, idx } |-> With[ {
          obj     = If[ MatchQ[ item, _Rule ], First @ item, item ],
          userDir = If[ MatchQ[ item, _Rule ],
            Replace[ Last @ item, { d_Directive :> d, x_ :> Directive[ x ] } ],
            Directive[] ] },
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
              InfraRay,           $InfraRayColor,
              InfraObject,        $InfraObjectColor,
              InfraPolyline,      $InfraSegmentColor,
              InfraSet,           $InfraShellColor,
              _,             $InfraSceneHighlightPalette[[
                               1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ],
            userDir },
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
            { InfraPolygon      [ b_List ], c_, u_ } :> { b, c, "Cycles", u },
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
        { polylineToKnotVertices[ b ], $InfraPointColor, "PointSet", Directive[] } ];

    triples = Join[ triples, knotTriples ];
    oRange = OptionValue[ "OpacityRange" ];
    tRange = OptionValue[ "ThicknessRange" ];
    pRange = OptionValue[ "PointSizeRange" ];

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
        { reps, color, type, userDir } |-> With[ {
            counts  = Counts @ Catenate[ repVerts[ type, # ] & /@ reps ],
            numReps = Max[ Length @ reps, 1 ] },
          AssociationMap[
            v |-> { color, counts[ v ] / numReps, userDir },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]] } ];

      eEntries = MapThread[
        { reps, color, type, userDir } |-> With[ {
            counts  = Counts @ Catenate[ repEdges[ type, # ] & /@ reps ],
            numReps = Max[ Length @ reps, 1 ] },
          AssociationMap[
            e |-> { color, counts[ e ] / numReps, userDir },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]] } ];
    ];

    (* A *Range option of None suppresses its directive.  Opacity is on by
       default; thickness and point-size are opt-in. *)
    With[ {
        opacityDir = If[ oRange === None, {},
          { r |-> Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) r ] } ],
        thicknessDir = If[ tRange === None, {},
          { r |-> AbsoluteThickness[ tRange[[ 1 ]] + ( tRange[[ 2 ]] - tRange[[ 1 ]] ) r ] } ],
        pointSizeDir = If[ pRange === None, {},
          { r |-> AbsolutePointSize[ pRange[[ 1 ]] + ( pRange[[ 2 ]] - pRange[[ 1 ]] ) r ] } ] },

      HighlightGraph[ graph, Join[
        KeyValueMap[
          { e, cs } |-> With[ { last = Last @ cs },
            Style[ UndirectedEdge @@ e, Directive[
              last[[ 1 ]],
              Sequence @@ ( # [ last[[ 2 ]] ] & /@ Join[ opacityDir, thicknessDir ] ),
              Sequence @@ last[[ 3 ]] ] ] ],
          Merge[ eEntries, Identity ] ],
        KeyValueMap[
          { v, cs } |-> With[ { last = Last @ cs },
            Style[ v, Directive[
              last[[ 1 ]],
              Sequence @@ ( # [ last[[ 2 ]] ] & /@ Join[ opacityDir, pointSizeDir ] ),
              Sequence @@ last[[ 3 ]] ] ] ],
          Merge[ vEntries, Identity ] ] ],
        FilterRules[ { opts }, Options @ HighlightGraph ] ]
    ]
  ]
