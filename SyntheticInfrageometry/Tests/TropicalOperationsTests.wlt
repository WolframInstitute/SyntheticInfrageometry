BeginTestSection["TropicalOperations"]

(* ===== TropicalPlus / TropicalTimes ===== *)

VerificationTest[
  TropicalPlus[3, 1, 2],
  1,
  TestID -> "TropicalPlus-scalars"
]

VerificationTest[
  TropicalPlus[],
  Infinity,
  TestID -> "TropicalPlus-empty-identity"
]

VerificationTest[
  TropicalPlus[{1, 5, 2}, {3, 0, 4}],
  {1, 0, 2},
  TestID -> "TropicalPlus-vectors-elementwise"
]

VerificationTest[
  TropicalPlus[{{1, 2}, {3, 4}}, {{5, 0}, {2, 6}}],
  {{1, 0}, {2, 4}},
  TestID -> "TropicalPlus-matrices-elementwise"
]

VerificationTest[
  TropicalTimes[3, 1, 2],
  6,
  TestID -> "TropicalTimes-scalars"
]

VerificationTest[
  TropicalTimes[],
  0,
  TestID -> "TropicalTimes-empty-identity"
]

VerificationTest[
  TropicalTimes[{1, 5, 2}, {3, 0, 4}],
  {4, 5, 6},
  TestID -> "TropicalTimes-vectors-elementwise"
]

(* ===== TropicalDot ===== *)

VerificationTest[
  TropicalDot[{1, 2, 3}, {4, 5, 6}],
  5,
  TestID -> "TropicalDot-vector-vector"
]

VerificationTest[
  TropicalDot[{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}],
  {{6, 7}, {8, 9}},
  TestID -> "TropicalDot-matrix-matrix"
]

VerificationTest[
  TropicalDot[{{1, 2}, {3, 4}}, {5, 6}],
  {6, 8},
  TestID -> "TropicalDot-matrix-vector"
]

VerificationTest[
  With[{D = GraphDistanceMatrix[CycleGraph[5]]}, TropicalDot[D, D] === D],
  True,
  TestID -> "TropicalDot-distance-matrix-idempotent"
]

(* ===== TropicalMatrixPower ===== *)

VerificationTest[
  TropicalMatrixPower[{{0, 1, 5}, {1, 0, 2}, {5, 2, 0}}, 0],
  {{0, Infinity, Infinity}, {Infinity, 0, Infinity}, {Infinity, Infinity, 0}},
  TestID -> "TropicalMatrixPower-zero-tropical-identity"
]

VerificationTest[
  TropicalMatrixPower[{{0, 1, 5}, {1, 0, 2}, {5, 2, 0}}, 1],
  {{0, 1, 5}, {1, 0, 2}, {5, 2, 0}},
  TestID -> "TropicalMatrixPower-one-identity"
]

VerificationTest[
  (* On a graph distance matrix the tropical idempotent: D^k = D for k >= 1. *)
  With[{D = GraphDistanceMatrix[GridGraph[{3, 3}]]},
    TropicalMatrixPower[D, 4] === D
  ],
  True,
  TestID -> "TropicalMatrixPower-distance-matrix-idempotent"
]

VerificationTest[
  (* Tropical identity acts as identity under TropicalDot. *)
  With[{A = {{0, 1, 5}, {1, 0, 2}, {5, 2, 0}},
        I0 = TropicalMatrixPower[{{0, 1, 5}, {1, 0, 2}, {5, 2, 0}}, 0]},
    TropicalDot[I0, A] === A && TropicalDot[A, I0] === A
  ],
  True,
  TestID -> "TropicalMatrixPower-identity-acts-as-identity"
]

(* ===== A -> D: tropical fixed point of the cost-adjacency matrix is the distance matrix.
        Build the cost adjacency A (0 on the diagonal, 1 on edges, Infinity off-edge)
        and check A^otimes(diam) === GraphDistanceMatrix[g], with A^otimes(k) stable
        for k >= diam (Kleene-star fixed point). ===== *)

VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    With[{n = VertexCount[g], adj = Normal @ AdjacencyMatrix[g]},
      With[{costA = Table[Which[i == j, 0, adj[[i, j]] == 1, 1, True, Infinity], {i, n}, {j, n}]},
        TropicalMatrixPower[costA, GraphDiameter[g]] === GraphDistanceMatrix[g]
      ]
    ]
  ],
  True,
  TestID -> "TropicalMatrixPower-A-to-D-GridGraph"
]

VerificationTest[
  With[{g = CycleGraph[6]},
    With[{n = VertexCount[g], adj = Normal @ AdjacencyMatrix[g]},
      With[{costA = Table[Which[i == j, 0, adj[[i, j]] == 1, 1, True, Infinity], {i, n}, {j, n}]},
        TropicalMatrixPower[costA, GraphDiameter[g]] === GraphDistanceMatrix[g]
      ]
    ]
  ],
  True,
  TestID -> "TropicalMatrixPower-A-to-D-CycleGraph"
]

VerificationTest[
  (* Stability past the diameter: A^otimes(diam + k) === A^otimes(diam) for k >= 0. *)
  With[{g = GridGraph[{3, 3}]},
    With[{n = VertexCount[g], adj = Normal @ AdjacencyMatrix[g], d = GraphDiameter[g]},
      With[{costA = Table[Which[i == j, 0, adj[[i, j]] == 1, 1, True, Infinity], {i, n}, {j, n}]},
        TropicalMatrixPower[costA, d + 5] === TropicalMatrixPower[costA, d]
      ]
    ]
  ],
  True,
  TestID -> "TropicalMatrixPower-stable-past-diameter"
]

VerificationTest[
  (* On a disconnected graph the tropical fixed point still recovers
     GraphDistanceMatrix, with Infinity entries between components. *)
  With[{g = Graph[{1, 2, 3, 4}, {1 <-> 2, 3 <-> 4}]},
    With[{n = VertexCount[g], adj = Normal @ AdjacencyMatrix[g]},
      With[{costA = Table[Which[i == j, 0, adj[[i, j]] == 1, 1, True, Infinity], {i, n}, {j, n}]},
        TropicalMatrixPower[costA, 5] === GraphDistanceMatrix[g]
      ]
    ]
  ],
  True,
  TestID -> "TropicalMatrixPower-A-to-D-disconnected"
]

EndTestSection[]
