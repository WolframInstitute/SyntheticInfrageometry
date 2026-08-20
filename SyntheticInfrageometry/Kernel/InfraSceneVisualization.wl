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

(* $infraColors is the one place an object colour is written down.  The $Infra*Color symbols and the
   head -> colour dispatch inside InfraSceneHighlight both read from it, and $InfraPalette exposes it
   as a Dataset.  It is a literal rather than a shipped asset on purpose: $InfraPointColor must not
   depend on file I/O at load time. *)

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
  InfraPoint -> "Point", InfraMesoPoint -> "Point",
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
(* a marked point reads as a dot on the substrate, not a blob covering its neighbours:
   at 14 a single-realisation point swallowed several mesh cells on a Medium plane *)
$InfraPointSize     = 6;

(* the house figure size, so a scene never carries a magic pixel number: a bare scene
   renders at $InfraSceneImageSize, and a multi-panel GraphicsRow / GraphicsGrid sets
   its own symbolic size while its panels inherit this one. *)
$InfraSceneImageSize = Medium;

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
   diffusion overrides).  A bare directive is routed by family: line-appearance
   directives (Thickness / AbsoluteThickness / Thick / Thin / Dashing / Dashed /
   Dotted / DotDashed) go to the edge channel, point-appearance directives
   (PointSize / AbsolutePointSize) to the vertex channel, and everything else
   (colours, Opacity, ...) to both.  The
   three ranges default to the global option values passed in `defaults`;
   a range value is None (off), a scalar base measure (distributed across
   realisations as measure * weight), or a {min, max} envelope.
   An explicit appearance directive overrides the matching count-diffusion: a
   thickness directive suppresses "ThicknessRange", a point-size directive or
   a VertexSize value suppresses "PointSizeRange", and an Opacity directive
   suppresses "OpacityRange", so the user's value is the only one emitted on
   that channel.
   VertexSize is a plain graph-coordinate passthrough for every value (numeric
   or symbolic); use AbsolutePointSize[n] for a constant on-screen point size
   (rerouted to a VertexShapeFunction since HighlightGraph drops point sizing
   inside Style[] highlight specs). *)
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
    (* An explicit appearance directive supersedes the matching count-driven
       diffusion: suppress the corresponding *Range so the user's value is the
       only one emitted on that channel, rather than winning on directive order
       alone. *)
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
   The per-object style spec (`entry -> color` / `-> Directive[..]` /
   `Style[entry, ..]` / `-> {opts..}`) is parsed by `parseHighlightStyle` — see
   there for channel routing, family-based directive dispatch, and the
   diffusion-range overrides. *)

(* Diffuse-encoding channels.  A channel value is None (off), a scalar base
   measure t (the absolute thickness / point size a crisp single-realisation
   object gets; a fuzzy object distributes it as t * count/numReps, so the
   total measure across realisations is conserved), or a {min, max} envelope
   (linear interpolation by weight; the floor keeps rare elements visible).
   "ThicknessRange" defaults to the base measure $InfraEdgeThickness;
   "OpacityRange" keeps the floored envelope.  "PointSizeRange" defaults to
   Automatic: point-shaped objects (InfraPoint, polyline knots, polygon
   corners) distribute the base measure $InfraPointSize, while vertices of
   path- and set-shaped objects inherit the underlying graph's point size. *)
Options[ InfraSceneHighlight ] = Join[
  {
    "OpacityRange"   :> $InfraOpacityRange,
    "ThicknessRange" :> $InfraEdgeThickness,
    "PointSizeRange" -> Automatic,
    ImageSize        :> $InfraSceneImageSize
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

    (* Each triple carries the original wrapper as a fifth element so the
       density computation can read its measure (infraVertexMultiset /
       infraEdgeMultiset / infraNumReps) uniformly across bundle, weighted,
       and DAG forms; None for non-Infra highlight objects. *)
    triples = MapIndexed[
      { item, idx } |-> With[ {
          obj    = If[ MatchQ[ item, _Rule ], First @ item, item ],
          record = parseHighlightStyle[ If[ MatchQ[ item, _Rule ], Last @ item, Automatic ], ranges ] },
        Append[ If[ MatchQ[ Head @ obj, InfraPoint | InfraMesoPoint | InfraObject | InfraSet | $infraBundleHeads ], obj, None ] ] @
        Replace[
          { obj, Lookup[ $infraColors, Lookup[ $infraHeadColors, Head @ obj, None ],
              $InfraSceneHighlightPalette[[
                1 + Mod[ First @ idx - 1, Length @ $InfraSceneHighlightPalette ] ]] ],
            record },
          {
            (* no "Weights" override: the generic path reads the measure via
               infraVertexMultiset / infraNumReps, so density = mass / total mass
               -- a sharp mesopoint draws full size, a spread one fades. *)
            { InfraMesoPoint[ m_Association ], c_, u_ } :> { Keys @ m, c, "Points", u },
            { InfraPoint   [ v_ ], c_, u_ } :> { { v }, c, "Points", u },
            (* a plain List of atoms is what the point finders return -- it must
               flow into the scene with no glue, exactly like a wrapper does *)
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
            (* a bare vertex is a legal highlight object: wrap it as a
               one-vertex point rather than letting it reach repVerts unlisted *)
            { b_, c_, u_ } /; MemberQ[ VertexList @ graph, b ] :> { { b }, c, "Points", u },
            { b_, c_, u_ }                      :> { b, c, Automatic, u }
          } ] ],
      objects ];

    (* Each InfraPolyline item additionally emits a knot triple (the leg
       endpoints rendered as points in $InfraPointColor).  Drawn on top of
       the path so the subdivision is visible.  *)
    knotTriples = Cases[ objects,
      ( InfraPolyline[ b_List ] | ( InfraPolyline[ b_List ] -> _ ) ) :>
        { polylineToKnotVertices[ b ], $InfraPointColor, "PointSet", defaultRecord, None } ];

    (* Polygon / triangle items emit their corner vertices as points, so the
       defining corners stand out from the geodesic sides. *)
    knotTriples = Join[ knotTriples, Cases[ objects,
      ( ( InfraPolygon | InfraTriangle )[ b_List ] |
        ( ( InfraPolygon | InfraTriangle )[ b_List ] -> _ ) ) :>
        { Map[ Most @ polylineToKnots[ # ] &, b ], $InfraPointColor, "PointSet", defaultRecord, None } ] ];

    triples = Join[ triples, knotTriples ];

    (* Resolve the Automatic point-size default per object type: point-shaped
       objects distribute the base measure, everything else stays off. *)
    triples = Apply[
      { reps, color, type, record, obj } |-> { reps, color, type,
        Append[ record, "PointSizeRange" -> Replace[ record[ "PointSizeRange" ],
          Automatic :> If[ MatchQ[ type, "Points" | "PointSet" ], $InfraPointSize, None ] ] ], obj },
      triples, { 1 } ];

    (* repVerts / repEdges share the per-type dispatch with InfraMeasure via
       infraRepVerts / infraRepEdges (Tools.wl); only the Automatic-type branch
       for non-Infra highlight objects stays local (it needs the graph). *)
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
        (* DAG segments read their edge occupation off Infrageometry's
           GeodesicEdgeOccupation via infraEdgeMultiset -- no enumeration *)
        { reps, color, type, record, obj } |-> With[ {
            counts  = If[ obj =!= None, infraEdgeMultiset[ graph, obj ],
                          Counts @ Catenate[ repEdges[ type, # ] & /@ reps ] ],
            numReps = If[ obj =!= None, infraNumReps[ obj ], Max[ Length @ reps, 1 ] ] },
          AssociationMap[
            e |-> { color, counts[ e ] / numReps, record },
            Keys @ counts ] ],
        { triples[[ All, 1 ]], triples[[ All, 2 ]], triples[[ All, 3 ]], triples[[ All, 4 ]], triples[[ All, 5 ]] } ];
    ];

    (* Colour + opacity ride per-element Style[] highlight specs (the channel
       HighlightGraph honours).  Edge thickness and vertex point-size are
       rerouted to top-level EdgeStyle / VertexShapeFunction rules because
       HighlightGraph silently ignores AbsoluteThickness / AbsolutePointSize
       inside Style[edge/vertex, ...].  A *Range record value of None
       suppresses that channel; ranges are per-object. *)
    With[ { lerp = { spec, w } |-> If[ ListQ @ spec, spec[[ 1 ]] + ( spec[[ 2 ]] - spec[[ 1 ]] ) w, spec w ] },
      {
          (* All edge styling (colour, opacity, thickness) rides top-level EdgeStyle,
             never a Style[edge, ..] highlight spec: HighlightGraph gives a highlight
             Style priority over EdgeStyle AND drops AbsoluteThickness inside it, so an
             edge listed both ways renders at default thickness.  EdgeStyle honours all
             three channels, so the count-driven thickness diffusion actually shows. *)
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
                (* a fixed AbsolutePointSize / PointSize in the vertex channel, or the
                   count-scaled "PointSizeRange" diffusion, is rerouted to a top-level
                   VertexShapeFunction since HighlightGraph drops point sizing in Style[] specs *)
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
        (* A walk is one stroke, not a chain of edges.  HighlightGraph draws each edge
           separately with a butt cap and ignores a CapForm / JoinForm placed in the edge
           directive, so a bend leaves a wedge of background bitten out of the ribbon.
           Each maximal run of equal-styled consecutive steps is therefore redrawn as one
           joined Line, carried by the EdgeShapeFunction of its first unclaimed edge while
           the run's remaining unclaimed edges draw nothing.  Each edge takes at most one
           rule -- a walk that repeats an edge, and overlapping realisations, would otherwise
           hand Graph two rules for it, and only the first survives. *)
        {
          strokes = Catenate @ Cases[ triples,
            { reps_, _, type : "Paths" | "Cycles", record_, _ } /; record[ "EdgeShapeFunction" ] === None :>
              Catenate @ Map[
                walk |-> Map[
                  steps |-> { UndirectedEdge @@ Sort @ # & /@ steps,
                              coords /@ Prepend[ Last /@ steps, First @ First @ steps ] },
                  Select[ SplitBy[ Partition[ walk, 2, 1 ], edgeStyle[ UndirectedEdge @@ Sort @ # ] & ],
                    Length[ # ] >= 2 & ] ],
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
                    { First @ fresh -> ( { JoinForm[ "Round" ], Line @ Last @ stroke } & ) },
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
