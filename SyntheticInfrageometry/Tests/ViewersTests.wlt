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
  With[ { g = CycleGraph[ 8 ] },
    Head @ InfraSceneHighlight[ g, { { VertexList[ g ] } }, "Cyclic" -> True ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-cyclic-sphere"
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
        FindSphere[ g, 1, 2, All ]    -> Green },
      "Cyclic" -> { False, True } ]
  ],
  Graph,
  TestID -> "InfraSceneHighlight-per-object-cyclic"
]
