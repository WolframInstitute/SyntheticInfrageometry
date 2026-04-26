BeginTestSection["Tessellations"]

(* ===== TorusTessellation ===== *)

VerificationTest[
  With[{g = TorusTessellation["Square", {3, 4}]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {12, 24, {4}},
  TestID -> "TorusTessellation-Square-counts-and-regular-degree"
]

VerificationTest[
  With[{g = TorusTessellation["Triangle", {3, 4}]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {12, 36, {6}},
  TestID -> "TorusTessellation-Triangle-counts-and-regular-degree"
]

VerificationTest[
  With[{g = TorusTessellation["Hexagon", {3, 4}]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {24, 36, {3}},
  TestID -> "TorusTessellation-Hexagon-counts-and-regular-degree"
]

VerificationTest[
  AllTrue[{"Square", "Triangle", "Hexagon"},
    ConnectedGraphQ @ TorusTessellation[#, {3, 4}] &
  ],
  True,
  TestID -> "TorusTessellation-connected"
]

VerificationTest[
  AllTrue[{"Square", "Triangle", "Hexagon"},
    VertexTransitiveGraphQ @ TorusTessellation[#, {4, 5}] &
  ],
  True,
  TestID -> "TorusTessellation-vertex-transitive"
]

VerificationTest[
  IsomorphicGraphQ[
    TorusTessellation["Square", 4],
    TorusTessellation["Square", {4, 4}]
  ],
  True,
  TestID -> "TorusTessellation-int-form-equals-square-form"
]

(* Volume-growth invariants are pointwise constant on a vertex-
   transitive graph; in particular the 1-ball is the same size at every
   vertex. *)
VerificationTest[
  With[{g = TorusTessellation["Triangle", {4, 5}]},
    Length @ Union[
      Length @ NeighborhoodGraph[g, #, 1] & /@ VertexList[g]
    ]
  ],
  1,
  TestID -> "TorusTessellation-ball-volume-pointwise-constant"
]

EndTestSection[]
