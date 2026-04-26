BeginTestSection["Tessellations"]

(* ===== TorusTessellation Schlafli {p, q} dispatch ===== *)

(* {3, 6}: V = nm, E = 3 nm, F = 2 nm, valency 6 *)
VerificationTest[
  With[{g = TorusTessellation[{4, 5}, {3, 6}]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {20, 60, {6}},
  TestID -> "TorusTessellation-3-6-counts"
]

(* {4, 4}: V = nm, E = 2 nm, F = nm, valency 4 *)
VerificationTest[
  With[{g = TorusTessellation[{4, 5}, {4, 4}]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {20, 40, {4}},
  TestID -> "TorusTessellation-4-4-counts"
]

(* {6, 3}: V = 2 nm, E = 3 nm, F = nm, valency 3 *)
VerificationTest[
  With[{g = TorusTessellation[{4, 5}, {6, 3}]},
    {VertexCount[g], EdgeCount[g], Union[VertexDegree[g]]}
  ],
  {40, 60, {3}},
  TestID -> "TorusTessellation-6-3-counts"
]

(* ===== Integer shorthand reads q from the Euclidean formula ===== *)

VerificationTest[
  IsomorphicGraphQ[
    TorusTessellation[{4, 5}, 3],
    TorusTessellation[{4, 5}, {3, 6}]
  ],
  True,
  TestID -> "TorusTessellation-int-shorthand-3"
]

VerificationTest[
  IsomorphicGraphQ[
    TorusTessellation[{4, 5}, 4],
    TorusTessellation[{4, 5}, {4, 4}]
  ],
  True,
  TestID -> "TorusTessellation-int-shorthand-4"
]

VerificationTest[
  IsomorphicGraphQ[
    TorusTessellation[{4, 5}, 6],
    TorusTessellation[{4, 5}, {6, 3}]
  ],
  True,
  TestID -> "TorusTessellation-int-shorthand-6"
]

(* ===== Connectedness + vertex-transitivity ===== *)

VerificationTest[
  AllTrue[{{3, 6}, {4, 4}, {6, 3}},
    ConnectedGraphQ @ TorusTessellation[{4, 5}, #] &
  ],
  True,
  TestID -> "TorusTessellation-connected"
]

VerificationTest[
  AllTrue[{{3, 6}, {4, 4}, {6, 3}},
    VertexTransitiveGraphQ @ TorusTessellation[{4, 5}, #] &
  ],
  True,
  TestID -> "TorusTessellation-vertex-transitive"
]

(* ===== Pointwise constant 1-ball volume ===== *)

VerificationTest[
  AllTrue[{{3, 6}, {4, 4}, {6, 3}},
    With[{g = TorusTessellation[{4, 5}, #]},
      Length @ Union[
        VertexCount @ NeighborhoodGraph[g, v, 1] & /@ VertexList[g]
      ] == 1
    ] &
  ],
  True,
  TestID -> "TorusTessellation-ball-volume-constant"
]

(* ===== Single-int alias = square fundamental-domain grid ===== *)

VerificationTest[
  IsomorphicGraphQ[
    TorusTessellation[4, {4, 4}],
    TorusTessellation[{4, 4}, {4, 4}]
  ],
  True,
  TestID -> "TorusTessellation-square-grid-alias"
]

(* ===== Non-torus Schlafli pairs rejected ===== *)

VerificationTest[
  TorusTessellation[{4, 5}, {5, 4}],
  $Failed,
  {TorusTessellation::nontorus},
  TestID -> "TorusTessellation-hyperbolic-5-4-rejected"
]

VerificationTest[
  TorusTessellation[{4, 5}, {7, 3}],
  $Failed,
  {TorusTessellation::nontorus},
  TestID -> "TorusTessellation-hyperbolic-7-3-rejected"
]

VerificationTest[
  TorusTessellation[{4, 5}, {3, 5}],
  $Failed,
  {TorusTessellation::nontorus},
  TestID -> "TorusTessellation-spherical-3-5-rejected"
]

(* Integer shorthand for non-Euclidean p falls into the same trap *)

VerificationTest[
  TorusTessellation[{4, 5}, 5],
  $Failed,
  {TorusTessellation::nontorus},
  TestID -> "TorusTessellation-int-5-rejected"
]

EndTestSection[]
