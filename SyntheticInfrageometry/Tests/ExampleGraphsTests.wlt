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
  VertexCount @ TorusTessellation[ "Rectangular", { 5, 5 } ],
  25,
  TestID -> "TorusTessellation-Rectangular-vertex-count"
]

VerificationTest[
  Union @ VertexDegree @ TorusTessellation[ "Rectangular", { 5, 5 } ],
  { 4 },
  TestID -> "TorusTessellation-Rectangular-is-4-regular"
]

VerificationTest[
  VertexCount @ TorusTessellation[ "Triangular", { 5, 5 } ],
  25,
  TestID -> "TorusTessellation-Triangular-vertex-count"
]

VerificationTest[
  Union @ VertexDegree @ TorusTessellation[ "Triangular", { 5, 5 } ],
  { 6 },
  TestID -> "TorusTessellation-Triangular-is-6-regular"
]

VerificationTest[
  VertexCount @ TorusTessellation[ "Hexagonal", { 4, 4 } ],
  32,
  TestID -> "TorusTessellation-Hexagonal-vertex-count"
]

VerificationTest[
  Union @ VertexDegree @ TorusTessellation[ "Hexagonal", { 4, 4 } ],
  { 3 },
  TestID -> "TorusTessellation-Hexagonal-is-3-regular"
]

(* ===== TorusTessellation: vertex-transitivity ===== *)

VerificationTest[
  VertexTransitiveGraphQ @ TorusTessellation[ "Rectangular", { 4, 4 } ],
  True,
  TestID -> "TorusTessellation-Rectangular-is-vertex-transitive"
]

VerificationTest[
  VertexTransitiveGraphQ @ TorusTessellation[ "Triangular", { 4, 4 } ],
  True,
  TestID -> "TorusTessellation-Triangular-is-vertex-transitive"
]

VerificationTest[
  VertexTransitiveGraphQ @ TorusTessellation[ "Hexagonal", { 3, 3 } ],
  True,
  TestID -> "TorusTessellation-Hexagonal-is-vertex-transitive"
]

(* ===== Composes with paclet primitives ===== *)

VerificationTest[
  With[ { g = TorusTessellation[ "Rectangular", { 4, 4 } ] },
    MatchQ[ FindSegment[ g, First @ VertexList @ g, Last @ VertexList @ g, All ], { InfraSegment[ { _ } ] .. } ]
  ],
  True,
  TestID -> "TorusTessellation-Rectangular-feeds-FindSegment"
]

EndTestSection[]
