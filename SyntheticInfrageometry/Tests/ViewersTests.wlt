VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g, { FindSegment[ g, 1, 16, All ] } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-single-multiobject"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ InfraSceneHighlight[ g,
      { FindLine[ g, 1, 9, All ] -> RGBColor[ 0.8, 0.2, 0.2 ] } ]
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
    Head @ InfraSceneHighlight[ g, { FindPoint[ g, 5 ] } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-vertex-singletons"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g,
      { FindSegment[ g, 1, 16, All ] -> Blue,
        { 1, 16 }                    -> Red } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-multiple-objects-blend"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraSceneHighlight[ g,
      { FindSegment[ g, 1, 16, All ] -> Blue,
        InfraCircle @ FindCircle[ g, 1, 2, All ] -> Green } ]
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
        pts    = Take[ VertexList @ g, 2 ],
        styles = GraphHighlightStyle /. Options @
          InfraSceneHighlight[ g, { InfraPoint[ Take[ VertexList @ g, 2 ] ] -> Red } ] },
      Length @ Cases[ styles, ( v_List -> _ ) /; MemberQ[ pts, v ], Infinity ] > 0 &&
      Length @ Cases[ styles, _UndirectedEdge -> _, Infinity ] == 0
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
    With[ { styles = GraphHighlightStyle /. Options @ InfraSceneHighlight[ g,
          { InfraShell[ FindShell[ g, 1, { 1, 2 }, All ] ] -> Green } ] },
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
    With[ { styles = GraphHighlightStyle /. Options @
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
        sOpen   = GraphHighlightStyle /. Options @ InfraSceneHighlight[ g, { InfraCircle[ { open   } ] -> Blue } ],
        sClosed = GraphHighlightStyle /. Options @ InfraSceneHighlight[ g, { InfraCircle[ { closed } ] -> Blue } ] },
      Sort @ Cases[ sOpen,   e_UndirectedEdge -> _, Infinity ] ===
      Sort @ Cases[ sClosed, e_UndirectedEdge -> _, Infinity ]
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
    With[ { styles = GraphHighlightStyle /. Options @
          InfraSceneHighlight[ g, { InfraSegment[ { path } ] -> Blue } ] },
      Length @ Cases[ styles, _UndirectedEdge -> _, Infinity ] == 3
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-InfraSegment-sequential-edges"
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
        seg = FindSegment[ g,
          First @ VertexList @ g, Last @ VertexList @ g, All ] },
      MatchQ[ First @ vs, { _, _ } ] &&
      Length @ Cases[
        GraphHighlightStyle /. Options @ InfraSceneHighlight[ g, { seg -> Red } ],
        ( UndirectedEdge[ { _, _ }, { _, _ } ] -> _ ), Infinity ] > 0
    ]
  ],
  True,
  TestID -> "InfraSceneHighlight-list-vertex-edges-actually-highlighted"
]
