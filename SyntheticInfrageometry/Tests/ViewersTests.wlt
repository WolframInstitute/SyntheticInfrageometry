VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraDiffuseHighlight[ g, FindSegment[ g, 1, 16, All ] ]
  ],
  Graph,
  TestID -> "InfraDiffuseHighlight-segments-returns-graph"
]

VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    Head @ InfraDiffuseHighlight[ g, FindLine[ g, 1, 9, All ], RGBColor[ 0.8, 0.2, 0.2 ] ]
  ],
  Graph,
  TestID -> "InfraDiffuseHighlight-lines-explicit-color"
]

VerificationTest[
  With[ { g = CycleGraph[ 8 ] },
    Head @ InfraDiffuseHighlight[ g, { VertexList[ g ] }, "Cyclic" -> True ]
  ],
  Graph,
  TestID -> "InfraDiffuseHighlight-cyclic-sphere"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ] },
    Head @ InfraDiffuseHighlight[ g, { } ]
  ],
  Graph,
  TestID -> "InfraDiffuseHighlight-empty-candidates-still-graph"
]

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    Head @ InfraDiffuseHighlight[ g, FindPoint[ g, 5 ] /. v_Integer :> { v } ]
  ],
  Graph,
  TestID -> "InfraDiffuseHighlight-singletons-as-vertex-clouds"
]
