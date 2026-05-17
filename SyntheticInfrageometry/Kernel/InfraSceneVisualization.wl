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
   Each entry may be plain or wrapped as `entry -> color`. *)

Options[ InfraSceneHighlight ] = Join[
  {
    "OpacityRange"   :> $InfraOpacityRange,
    "ThicknessRange" :> $InfraThicknessRange,
    "PointSizeRange" :> $InfraPointSizeRange
  },
  Options[ HighlightGraph ]
];

InfraSceneHighlight[ graph_Graph, multiObjects_List, opts : OptionsPattern[] ] :=
  Module[ { triples, knotTriples, oRange, tRange, pRange, vEntries, eEntries, objects },

    (* Normalise each item: merge {InfraX[{r1}],...} into InfraX[{r1,...}];
       then strip $Failed / empty entries. *)
    objects = DeleteCases[
      Replace[ #,
        list_List /; Length[ list ] > 0 && SameQ @@ (Head /@ list) &&
            MatchQ[ First @ list, _[ _List ] ] :>
          Head[ First @ list ][ Join @@ list[[ All, 1 ]] ] ] & /@ multiObjects,
      _[ $Failed ] | ( _[ $Failed ] -> _ ) | ( _ -> _[ $Failed ] ) | { } ];

    triples = MapIndexed[
      { item, idx } |-> Replace[
        If[ MatchQ[ item, _Rule ], List @@ item,
          { item, Switch[ Head @ item,
              InfraPoint,    $InfraPointColor,
              InfraSegment,  $InfraSegmentColor,
              InfraLine,     $InfraLineColor,
              InfraPath,     $InfraPathColor,
              InfraShell,         $InfraShellColor,
              InfraBall,          $InfraBallColor,
              InfraEllipticShell, $InfraShellColor,
              InfraPlane,         $InfraPlaneColor,
              InfraCircle,        $InfraCircleColor,
              InfraEllipse,       $InfraCircleColor,
              InfraRay,           $InfraRayColor,
              InfraObject,        $InfraObjectColor,
              InfraPolyline,      $InfraSegmentColor,
              _,             $InfraSceneHighlightPalette[[
                               1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ] } ],
        {
          { InfraPoint   [ b_List ], c_ } :> { b, c, "Points" },
          { InfraSegment [ b_List ], c_ } :> { b, c, "Paths"  },
          { InfraLine    [ b_List ], c_ } :> { b, c, "Paths"  },
          { InfraPath    [ b_List ], c_ } :> { b, c, "Paths"  },
          { InfraShell        [ b_List ], c_ } :> { b, c, "Sets"   },
          { InfraBall         [ b_List ], c_ } :> { b, c, "Sets"   },
          { InfraEllipticShell[ b_List ], c_ } :> { b, c, "Sets"   },
          { InfraPlane        [ b_List ], c_ } :> { b, c, "Sets"   },
          { InfraCircle       [ b_List ], c_ } :> { b, c, "Cycles" },
          { InfraEllipse      [ b_List ], c_ } :> { b, c, "Cycles" },
          { InfraRay     [ b_List ], c_ } :> { b, c, "Paths"  },
          { InfraObject  [ b_List ], c_ } :> { { b }, c, "Sets"  },
          { InfraPolyline[ b_List ], c_ } :> { polylineToVertexSeqs[ b ], c, "Paths" },
          { b_, c_ }                      :> { b, c, Automatic }
        } ],
      objects ];

    (* Each InfraPolyline item additionally emits a knot triple (the leg
       endpoints rendered as points in $InfraPointColor).  Drawn on top of
       the path so the subdivision is visible.  *)
    knotTriples = Cases[ objects,
      ( InfraPolyline[ b_List ] | ( InfraPolyline[ b_List ] -> _ ) ) :>
        { polylineToKnotVertices[ b ], $InfraPointColor, "PointSet" } ];

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
        { e, cs } |-> With[ { last = Last @ cs },
          Style[ UndirectedEdge @@ e, Directive[
            last[[ 1 ]],
            Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) last[[ 2 ]] ],
            AbsoluteThickness[ tRange[[ 1 ]] + ( tRange[[ 2 ]] - tRange[[ 1 ]] ) last[[ 2 ]] ] ] ] ],
        Merge[ eEntries, Identity ] ],
      KeyValueMap[
        { v, cs } |-> With[ { last = Last @ cs },
          Style[ v, Directive[
            last[[ 1 ]],
            Opacity[ oRange[[ 1 ]] + ( oRange[[ 2 ]] - oRange[[ 1 ]] ) last[[ 2 ]] ],
            AbsolutePointSize[ pRange[[ 1 ]] + ( pRange[[ 2 ]] - pRange[[ 1 ]] ) last[[ 2 ]] ] ] ] ],
        Merge[ vEntries, Identity ] ] ],
      FilterRules[ { opts }, Options @ HighlightGraph ] ]
  ]
