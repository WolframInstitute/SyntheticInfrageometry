BeginTestSection["Tessellations"]

(* ===== TorusTessellation ===== *)

(* p = 3 (triangles): V = nm, E = 3 nm, F = 2 nm, valency 6 *)
VerificationTest[
  With[{g = TorusTessellation[{4, 5}, 3]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {20, 60, {6}},
  TestID -> "TorusTessellation-triangle-counts"
]

(* p = 4 (squares): V = nm, E = 2 nm, F = nm, valency 4 *)
VerificationTest[
  With[{g = TorusTessellation[{4, 5}, 4]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {20, 40, {4}},
  TestID -> "TorusTessellation-square-counts"
]

(* p = 6 (hexagons): V = 2 nm, E = 3 nm, F = nm, valency 3 *)
VerificationTest[
  With[{g = TorusTessellation[{4, 5}, 6]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {40, 60, {3}},
  TestID -> "TorusTessellation-hexagon-counts"
]

VerificationTest[
  AllTrue[{3, 4, 6},
    ConnectedGraphQ @ TorusTessellation[{4, 5}, #] &
  ],
  True,
  TestID -> "TorusTessellation-connected"
]

VerificationTest[
  AllTrue[{3, 4, 6},
    VertexTransitiveGraphQ @ TorusTessellation[{4, 5}, #] &
  ],
  True,
  TestID -> "TorusTessellation-vertex-transitive"
]

(* Single-int alias = square fundamental-domain grid *)
VerificationTest[
  IsomorphicGraphQ[
    TorusTessellation[4, 4],
    TorusTessellation[{4, 4}, 4]
  ],
  True,
  TestID -> "TorusTessellation-int-shorthand"
]

(* Pointwise constant 1-ball volume *)
VerificationTest[
  AllTrue[{3, 4, 6},
    With[{g = TorusTessellation[{4, 5}, #]},
      Length @ Union[
        VertexCount @ NeighborhoodGraph[g, v, 1] & /@ VertexList[g]
      ] == 1
    ] &
  ],
  True,
  TestID -> "TorusTessellation-ball-volume-constant"
]

(* Non-Euclidean p rejected *)
VerificationTest[
  TorusTessellation[{4, 5}, 5],
  $Failed,
  {TorusTessellation::badp},
  TestID -> "TorusTessellation-pentagon-rejected"
]

VerificationTest[
  TorusTessellation[{4, 5}, 7],
  $Failed,
  {TorusTessellation::badp},
  TestID -> "TorusTessellation-heptagon-rejected"
]

(* ===== SchlafliTessellation placeholder ===== *)

VerificationTest[
  SchlafliTessellation[{5, 4}],
  $Failed,
  {SchlafliTessellation::nyi},
  TestID -> "SchlafliTessellation-not-yet-implemented"
]

VerificationTest[
  SchlafliTessellation[{4, 4}, 10],
  $Failed,
  {SchlafliTessellation::nyi},
  TestID -> "SchlafliTessellation-placeholder-any-args"
]

EndTestSection[]
