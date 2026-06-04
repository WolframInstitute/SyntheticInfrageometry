BeginTestSection["ExampleGraphs"]

(* ===== PunchHole ===== *)

VerificationTest[
  Head @ PunchHole[ GridGraph[ { 10, 10 } ], 1 ],
  Graph,
  TestID -> "PunchHole-random-returns-graph"
]

VerificationTest[
  VertexCount @ PunchHole[ GridGraph[ { 10, 10 } ], 0 ],
  99,
  TestID -> "PunchHole-radius-0-removes-one-vertex"
]

VerificationTest[
  With[ { g = GridGraph[ { 11, 11 } ], c = 60 },
    MemberQ[ VertexList @ PunchHole[ g, c -> 1 ], c ]
  ],
  False,
  TestID -> "PunchHole-explicit-center-removed"
]

VerificationTest[
  With[ { g = Fold[ PunchHole, GridGraph[ { 12, 12 } ], { 2, 1 } ] },
    VertexCount[ g ] < 144 && VertexCount[ g ] > 0
  ],
  True,
  TestID -> "PunchHole-fold-over-radii-shrinks-graph"
]

VerificationTest[
  VertexCount @ Fold[ PunchHole, GridGraph[ { 20, 20 } ], { } ],
  400,
  TestID -> "PunchHole-fold-empty-list-keeps-everything"
]

(* ===== TorusTessellation: vertex counts and regularity ===== *)

VerificationTest[
  VertexCount @ TorusTessellation[ { 5, 5 }, "Square" ],
  25,
  TestID -> "TorusTessellation-Square-vertex-count"
]

VerificationTest[
  Union @ VertexDegree @ TorusTessellation[ { 5, 5 }, "Square" ],
  { 4 },
  TestID -> "TorusTessellation-Square-is-4-regular"
]

VerificationTest[
  VertexCount @ TorusTessellation[ { 5, 5 }, "Triangular" ],
  25,
  TestID -> "TorusTessellation-Triangular-vertex-count"
]

VerificationTest[
  Union @ VertexDegree @ TorusTessellation[ { 5, 5 }, "Triangular" ],
  { 6 },
  TestID -> "TorusTessellation-Triangular-is-6-regular"
]

VerificationTest[
  VertexCount @ TorusTessellation[ { 4, 4 }, "Hexagonal" ],
  32,
  TestID -> "TorusTessellation-Hexagonal-vertex-count"
]

VerificationTest[
  Union @ VertexDegree @ TorusTessellation[ { 4, 4 }, "Hexagonal" ],
  { 3 },
  TestID -> "TorusTessellation-Hexagonal-is-3-regular"
]

(* ===== TorusTessellation: shape defaults to Triangular ===== *)

VerificationTest[
  IsomorphicGraphQ[ TorusTessellation[ { 5, 5 } ], TorusTessellation[ { 5, 5 }, "Triangular" ] ],
  True,
  TestID -> "TorusTessellation-default-shape-is-Triangular"
]

(* ===== TorusTessellation: vertex-transitivity ===== *)

VerificationTest[
  VertexTransitiveGraphQ @ TorusTessellation[ { 4, 4 }, "Square" ],
  True,
  TestID -> "TorusTessellation-Square-is-vertex-transitive"
]

VerificationTest[
  VertexTransitiveGraphQ @ TorusTessellation[ { 4, 4 }, "Triangular" ],
  True,
  TestID -> "TorusTessellation-Triangular-is-vertex-transitive"
]

VerificationTest[
  VertexTransitiveGraphQ @ TorusTessellation[ { 3, 3 }, "Hexagonal" ],
  True,
  TestID -> "TorusTessellation-Hexagonal-is-vertex-transitive"
]

(* ===== Composes with paclet primitives ===== *)

VerificationTest[
  With[ { g = TorusTessellation[ { 4, 4 }, "Square" ] },
    MatchQ[ FindInfraSegment[ g, First @ VertexList @ g, Last @ VertexList @ g, All ], { InfraSegment[ { _ } ] .. } ]
  ],
  True,
  TestID -> "TorusTessellation-Square-feeds-FindInfraSegment"
]

EndTestSection[]
