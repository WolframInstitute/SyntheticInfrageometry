VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g, { FindInfraSegment[ g, 1, 16, All ] } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-single-multiobject"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ InfraSceneHighlight[ g,
      { InfraLine @ FindInfraLine[ g, 1, 9, All ] -> RGBColor[ 0.8, 0.2, 0.2 ] } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-explicit-color-rule"
]

VerificationTest[
  With[ { g = CycleGraph[ 8 ], vs = VertexList[ CycleGraph[ 8 ] ] },
    Head @ InfraSceneHighlight[ g, { { Append[ vs, First @ vs ] } } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-self-closing-cycle"
]

VerificationTest[
  Head @ InfraSceneHighlight[ PathGraph[ Range[ 5 ] ], { } ],
  Graph,
  TestID -> "InfraSceneHighlight-empty-input-still-graph"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g, { InfraSet @ FindInfraPoint[ g, 5 ] } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-vertex-singletons"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g,
      { FindInfraSegment[ g, 1, 16, All ] -> Blue,
        { 1, 16 }                                   -> Red } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-multiple-objects-blend"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g,
      { FindInfraSegment[ g, 1, 16, All ] -> Blue,
        InfraCircle @ FindInfraCircle[ g, 1, 2, All ] -> Green } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-mixed-segment-and-circle"
]

(* InfraPoint wrapper: each rep treated as a single vertex (no edges).
   On a list-named-vertex graph, this is the case where auto-detection
   could be ambiguous between "single list-vertex" and "list of vertices". *)
VerificationTest[
  With[ { g = MeshConnectivityGraph @ DiscretizeRegion[
        Rectangle[], MaxCellMeasure -> 0.1 ] },
    With[ {
        pts  = Take[ VertexList @ g, 2 ],
        opts = Options @
          InfraSceneHighlight[ g, { InfraMesoPoint[ InfraSet[ Take[ VertexList @ g, 2 ] ] ] -> Red } ] },
      Length @ Flatten @ Cases[ opts,
        HoldPattern[ VertexShapeFunction -> rules_ ] :>
          Cases[ rules, ( v_ -> _ ) /; MemberQ[ pts, v ] ], Infinity ] > 0 &&
      Length @ Cases[ GraphHighlightStyle /. opts, _UndirectedEdge -> _, Infinity ] == 0
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-InfraPoint-vertices-only"
]

(* InfraShell wrapper: each rep is a vertex set, edges are induced subgraph.
   On a 4x4 grid, the level set at radius {1, 2} from vertex 1 has four
   induced subgraph edges; verify they are highlighted. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { styles = EdgeStyle /. Options @ InfraSceneHighlight[ g,
          { InfraShell @ FindInfraShell[ g, 1, { 1, 2 }, All ] -> Green } ] },
      Length @ Cases[ styles, _UndirectedEdge -> _, Infinity ] > 0
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-InfraShell-induced-edges"
]

(* InfraCircle wrapper: each rep is a cyclic vertex sequence, edges are
   sequential pairs plus auto-closure (last, first).  On the 4-cycle
   { 1, 2, 6, 5 } in GridGraph[{4, 4}], expect 4 highlighted edges:
   {1,2}, {2,6}, {6,5}, {5,1}. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], cyc = { 1, 2, 6, 5 } },
    With[ { styles = EdgeStyle /. Options @
          InfraSceneHighlight[ g, { InfraCircle[ { cyc } ] -> Blue } ] },
      Length @ Cases[ styles, _UndirectedEdge -> _, Infinity ] == 4
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-InfraCircle-auto-closure"
]

(* InfraCircle idempotence on pre-closed input: passing
   { 1, 2, 6, 5, 1 } produces the same edge set as { 1, 2, 6, 5 }. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], open = { 1, 2, 6, 5 }, closed = { 1, 2, 6, 5, 1 } },
    With[ {
        sOpen   = EdgeStyle /. Options @ InfraSceneHighlight[ g, { InfraCircle[ { open   } ] -> Blue } ],
        sClosed = EdgeStyle /. Options @ InfraSceneHighlight[ g, { InfraCircle[ { closed } ] -> Blue } ] },
      Sort @ Cases[ sOpen,   ( e_UndirectedEdge -> _ ) :> e, Infinity ] ===
      Sort @ Cases[ sClosed, ( e_UndirectedEdge -> _ ) :> e, Infinity ] &&
      Length @ Cases[ sOpen, _UndirectedEdge -> _, Infinity ] == 4
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-InfraCircle-idempotent-on-closed-input"
]

(* InfraSegment wrapper: sequential-edge semantics via Partition.  Verify
   that for a path of length 4 the highlighted edges are exactly the 3
   sequential pairs. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], path = { 1, 2, 3, 4 } },
    With[ { styles = EdgeStyle /. Options @
          InfraSceneHighlight[ g, { InfraSegment[ { path } ] -> Blue } ] },
      Length @ Cases[ styles, _UndirectedEdge -> _, Infinity ] == 3
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-InfraSegment-sequential-edges"
]

(* Per-object style override via Rule -> Directive[...]: an explicit
   AbsolutePointSize is rerouted to a top-level VertexShapeFunction (it is a
   no-op inside a Style[] highlight spec), so it must reach the produced
   options. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    ! FreeQ[
      Options @ InfraSceneHighlight[ g,
        { InfraSet[ { 1, 6, 11 } ] -> Directive[ Blue, AbsolutePointSize[ 25 ] ] } ],
      AbsolutePointSize[ 25 ] ]
  ],
  True,
  TestID -> "InfraSceneHighlight-rule-directive-pointsize-override"
]

(* Per-object style override via Style[obj, dirs__]: equivalent to the
   Rule -> Directive[...] form. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    ! FreeQ[
      Options @ InfraSceneHighlight[ g,
        { Style[ InfraSet[ { 1, 6 } ], Green, AbsolutePointSize[ 30 ] ] } ],
      AbsolutePointSize[ 30 ] ]
  ],
  True,
  TestID -> "InfraSceneHighlight-style-wrapper-pointsize-override"
]

(* Edge-level override: AbsoluteThickness on InfraSegment must reach the
   produced EdgeStyle directive. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { styles = EdgeStyle /. Options @ InfraSceneHighlight[ g,
          { InfraSegment[ { { 1, 2, 3, 4 } } ] -> Directive[ Orange, AbsoluteThickness[ 8 ] ] } ] },
      ! FreeQ[ styles, AbsoluteThickness[ 8 ] ]
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-rule-directive-thickness-override"
]

(* An explicit Opacity directive overrides the on-by-default "OpacityRange"
   count-diffusion: only the user's opacity is emitted on each edge, so no
   gradient-induced opacity values appear alongside it. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    With[ { styles = GraphHighlightStyle /. Options @ InfraSceneHighlight[ g,
          { InfraSegment[ { { 1, 2, 3, 4 } } ] -> Directive[ Orange, Opacity[ 0.3 ] ] } ] },
      ! FreeQ[ styles, Opacity[ 0.3 ] ] &&
      Cases[ styles, Opacity[ x_ ] /; x =!= 0.3, Infinity ] === { }
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-explicit-opacity-overrides-opacityrange"
]

(* Regression test for the Flatten level bug: edges must actually be
   highlighted on a graph whose vertices are 2-lists.  Pre-fix, the bare
   Flatten call inside InfraSceneHighlight collapsed list-named vertices
   to scalars and HighlightGraph silently received malformed edges, so
   GraphHighlightStyle ended up empty of EdgeStyle entries. *)
VerificationTest[
  With[ {
      g = MeshConnectivityGraph @ DiscretizeRegion[
        Rectangle[], MaxCellMeasure -> 0.1 ] },
    With[ {
        vs  = VertexList @ g,
        seg = FindInfraSegment[ g,
          First @ VertexList @ g, Last @ VertexList @ g, All ] },
      MatchQ[ First @ vs, { _, _ } ] &&
      Length @ Cases[
        EdgeStyle /. Options @ InfraSceneHighlight[ g, { seg -> Red } ],
        ( UndirectedEdge[ { _, _ }, { _, _ } ] -> _ ), Infinity ] > 0
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-list-vertex-edges-actually-highlighted"
]

(* Structured per-object spec: a flat option list is auto-sorted into the
   vertex / edge / diffusion channels.  EdgeStyle and VertexSize must reach
   the produced top-level Graph options (not just the Style[] highlight
   specs), and "ThicknessRange" must scale the per-edge thickness there. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ {
        opts = Options @ InfraSceneHighlight[ g, {
          InfraSegment[ { { 1, 2, 3, 4, 5 } } ] -> {
            VertexStyle      -> Red,
            VertexSize       -> Large,
            EdgeStyle        -> Directive[ Blue, AbsoluteThickness[ 7 ] ],
            "ThicknessRange" -> { 1, 9 } } } ] },
      ! FreeQ[ Cases[ opts, HoldPattern[ EdgeStyle -> e_ ] :> e, Infinity ], AbsoluteThickness[ 7 ] ] &&
      Cases[ opts, HoldPattern[ VertexSize -> _ ], Infinity ] =!= { }
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-structured-spec-toplevel-routing"
]

(* "PointSizeRange" opt-in reroutes vertex sizing to top-level
   VertexShapeFunction rules (HighlightGraph ignores AbsolutePointSize in
   Style[] highlight specs). *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    Cases[
      Options @ InfraSceneHighlight[ g,
        { InfraSegment[ { { 1, 2, 3, 4, 5 } } ] -> { "PointSizeRange" -> { 8, 20 } } } ],
      HoldPattern[ VertexShapeFunction -> _ ], Infinity ] =!= { }
  ],
  True,
  TestID -> "InfraSceneHighlight-pointsizerange-reroutes-to-vsf"
]

(* VertexSize is a plain graph-coordinate passthrough for every value: a
   numeric per-entry VertexSize stays on the top-level VertexSize channel (no
   AbsolutePointSize reroute), exactly like a symbolic one. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ { opts = Options @ InfraSceneHighlight[ g,
          { InfraPoint[13] -> { VertexStyle -> Blue, VertexSize -> 12 } } ] },
      Cases[ opts, HoldPattern[ VertexSize -> _ ], Infinity ] =!= { } &&
      FreeQ[ opts, AbsolutePointSize ]
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-numeric-vertexsize-stays-graphcoord"
]

(* AbsolutePointSize[n] is the explicit constant-on-screen-size path: it
   reroutes to a top-level VertexShapeFunction (HighlightGraph drops point
   sizing inside Style[] specs) and suppresses the "PointSizeRange" diffusion. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ { opts = Options @ InfraSceneHighlight[ g,
          { InfraPoint[13] -> { Blue, AbsolutePointSize[ 12 ],
            "PointSizeRange" -> { 4, 30 } } } ] },
      Cases[ opts, HoldPattern[ VertexShapeFunction -> _ ], Infinity ] =!= { } &&
      ! FreeQ[ opts, AbsolutePointSize[ 12 ] ] &&
      FreeQ[ opts, AbsolutePointSize[ 4 ] | AbsolutePointSize[ 30 ] ]
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-abspointsize-overrides-pointsizerange"
]

(* Scalar "ThicknessRange": the base measure is distributed across
   realisations.  On CycleGraph[4] the two geodesics 1-2-3 and 1-4-3 split
   the measure, so every edge renders at exactly half the base thickness. *)
VerificationTest[
  With[ { g = CycleGraph[ 4 ] },
    With[ { opts = Options @ InfraSceneHighlight[ g,
          { FindInfraSegment[ g, 1, 3, All ] }, "ThicknessRange" -> 8 ] },
      ! FreeQ[ opts, AbsoluteThickness[ 4 ] ] && FreeQ[ opts, AbsoluteThickness[ 8 ] ]
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-scalar-thickness-distributes-measure"
]

(* A crisp single-realisation object carries the full base measure. *)
VerificationTest[
  With[ { g = PathGraph[ Range[ 4 ] ] },
    ! FreeQ[
      Options @ InfraSceneHighlight[ g,
        { InfraSegment[ { { 1, 2, 3, 4 } } ] }, "ThicknessRange" -> 8 ],
      AbsoluteThickness[ 8 ] ]
  ],
  True,
  TestID -> "InfraSceneHighlight-scalar-thickness-crisp-full-measure"
]

(* Default point sizing: a fuzzy InfraPoint distributes the base point measure
   over its candidate vertices (two candidates -> half each), a crisp one gets
   the whole measure.  Stated as the ratio, so the palette may retune the base. *)
VerificationTest[
  With[ { g = GridGraph[ { 7, 7 } ] },
    { (* a UNIFORM mesopoint (here a ball) is uniformly bright: its diffuseness
         is its extent, not a per-vertex fade *)
      Union @ Cases[ Options @ InfraSceneHighlight[ g, { InfraMesoPoint[ FindInfraBall[ g, 25, 2 ] ] } ],
        AbsolutePointSize[ s_ ] :> s, Infinity ],
      (* a NON-uniform mesopoint draws its heaviest vertex full and the rest smaller *)
      With[ { sizes = Cases[ Options @ InfraSceneHighlight[ g, { FindInfraMidpoint[ g, 1, 49 ] } ],
                AbsolutePointSize[ s_ ] :> s, Infinity ] },
        { Max @ sizes, Max @ sizes > Min @ sizes } ] } ],
  { { 6 }, { 6, True } },
  TestID -> "InfraSceneHighlight-mesopoint-relative-mass"
]

(* A highlighted walk is drawn as ONE joined stroke through its vertices, not as a
   chain of separately butt-capped edges: a bend must not bite a wedge of background
   out of the ribbon. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Cases[ Options @ InfraSceneHighlight[ g, { InfraWalk[ { { 1, 2, 6, 10, 11 } } ] } ],
      Line[ q_ ] :> q, Infinity ] === { GraphEmbedding[ g ][[ { 1, 2, 6, 10, 11 } ]] }
  ],
  True,
  TestID -> "InfraSceneHighlight-walk-is-one-stroke"
]

(* A walk that repeats an edge still paints every step: an edge whose shape function
   draws nothing has to lie on one of the joined strokes. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { opts = Options @ InfraSceneHighlight[ g, { InfraWalk[ { { 1, 2, 6, 2, 3, 7 } } ] } ],
      xy   = AssociationThread[ VertexList[ g ] -> GraphEmbedding[ g ] ] },
    { stroked = Union @ Catenate[ Sort /@ Partition[ #, 2, 1 ] & /@ Cases[ opts, Line[ q_ ] :> q, Infinity ] ],
      blank   = Cases[ opts, ( e_UndirectedEdge -> f_Function ) /; FreeQ[ f, _Line ] :> Sort[ xy /@ List @@ e ], Infinity ] },
    blank =!= { } && SubsetQ[ stroked, blank ]
  ],
  True,
  TestID -> "InfraSceneHighlight-repeated-edge-walk-keeps-every-step"
]

(* The Automatic point-size default stays off for non-point objects: a set
   highlight emits no VertexShapeFunction, its vertices inherit the graph's
   point size. *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Cases[
      Options @ InfraSceneHighlight[ g,
        { InfraShell @ FindInfraShell[ g, 1, { 1, 2 }, All ] } ],
      HoldPattern[ VertexShapeFunction -> _ ], Infinity ] === { }
  ],
  True,
  TestID -> "InfraSceneHighlight-automatic-pointsize-off-for-sets"
]

(* A symbolic per-entry VertexSize (Large / Tiny / Scaled[..]) stays on the
   graph-coordinate HighlightGraph VertexSize channel. *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    Cases[
      Options @ InfraSceneHighlight[ g,
        { InfraPoint[13] -> { VertexSize -> Large } } ],
      HoldPattern[ VertexSize -> _ ], Infinity ] =!= { }
  ],
  True,
  TestID -> "InfraSceneHighlight-symbolic-vertexsize-stays-graphcoord"
]


(* ===== point-layer highlight objects ===== *)

(* the point finders return a plain List of atoms; it must flow into the scene
   with no glue, and distribute the point-size measure like any bundle *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { Cases[ Options @ InfraSceneHighlight[ g, { { InfraPoint[3], InfraPoint[9] } -> Red } ],
        AbsolutePointSize[ s_ ] :> s, Infinity ],
      Cases[ Options @ InfraSceneHighlight[ g, { InfraPoint[3] } ],
        AbsolutePointSize[ s_ ] :> s, Infinity ] } ],
  { { 3, 3 }, { 6 } },
  TestID -> "InfraSceneHighlight-atom-list-distributes-size"
]

(* a bare vertex is a legal highlight object *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Cases[ Options @ InfraSceneHighlight[ g, { 7 -> Red } ], AbsolutePointSize[ s_ ] :> s, Infinity ] ],
  { 6 },
  TestID -> "InfraSceneHighlight-bare-vertex"
]
