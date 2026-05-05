Package["WolframInstitute`SyntheticInfrageometry`"]

(* TropicalDot, used by DistanceMatrixQ for the tropical-idempotence
   form of the triangle inequality, lives in TropicalOperations.wl. *)


(* ===================== MetricInterval ===================== *)

(* The metric interval I(u, v) = { w : d(u, w) + d(w, v) = d(u, v) } -
   the union of all geodesics from u to v.  Master operation of the
   metric-algebra layer; every other primitive can be reduced to it. *)

MetricInterval[ graph_Graph, u_, v_ ] :=
  With[ { d = GraphDistance[ graph, u, v ] },
    If[ d === Infinity, {},
      Select[ VertexList[ graph ],
        w |-> GraphDistance[ graph, u, w ] + GraphDistance[ graph, w, v ] == d
      ]
    ]
  ]


(* ===================== GeodesicCount ===================== *)

(* GeodesicCount[g, u, v] = (A^d)[u, v] where A is the adjacency matrix
   and d = d(u, v).  Identity: a walk of length d(u, v) from u to v is
   automatically a simple geodesic, so the (u, v) entry of A^d counts
   geodesics.  Polynomial-time replacement for path enumeration. *)

GeodesicCount[ graph_Graph, u_, v_ ] :=
  With[ { d = GraphDistance[ graph, u, v ], V = VertexList[ graph ] },
    Which[
      d === Infinity, 0,
      d == 0, 1,
      True,
      With[ { ui = First @ FirstPosition[ V, u ],
              vi = First @ FirstPosition[ V, v ] },
        MatrixPower[ Normal @ AdjacencyMatrix[ graph ], d ][[ ui, vi ]]
      ]
    ]
  ]


(* ===================== DistanceMultiplicityMatrix ===================== *)

(* The pair (D, M): D is the graph distance matrix, M[i, j] is the number
   of geodesics from vertex i to vertex j (= (A^{D[i,j]})[i, j]).  M is
   the multiplicity counterpart of the metric. *)

DistanceMultiplicityMatrix[ graph_Graph ] :=
  Module[ { V, n, dMat, A, mMat, powers, maxD, finiteD },
    V = VertexList[ graph ];
    n = Length[ V ];
    dMat = GraphDistanceMatrix[ graph ];
    A = Normal @ AdjacencyMatrix[ graph ];
    finiteD = Cases[ Flatten @ dMat, _Integer ];
    maxD = If[ finiteD === {}, 0, Max @ finiteD ];
    powers = NestList[ #.A &, IdentityMatrix[ n ], maxD ];
    mMat = Table[
      With[ { d = dMat[[ i, j ]] },
        If[ d === Infinity, 0, powers[[ d + 1, i, j ]] ]
      ],
      { i, n }, { j, n }
    ];
    { dMat, mMat }
  ]


(* ===================== DistanceMatrixQ ===================== *)

(* A square non-negative symmetric zero-diagonal matrix M is the distance
   matrix of some graph iff it is tropical-idempotent: M (x)_min M = M
   under (min, +) multiplication.  This is the matrix form of the
   triangle inequality and saturates it. *)

DistanceMatrixQ[ M_?MatrixQ ] :=
  And[
    SquareMatrixQ[ M ],
    Transpose[ M ] === M,
    AllTrue[ Diagonal[ M ], # === 0 & ],
    AllTrue[ Flatten[ M ],
      # === Infinity || ( IntegerQ[ # ] && # >= 0 ) || ( NumericQ[ # ] && # >= 0 ) & ],
    TropicalDot[ M, M ] === M
  ]

DistanceMatrixQ[ _ ] := False


(* ===================== MedianVertices ===================== *)

(* The metric medians of a vertex set vs are the argmin of total distance:
   { w : Sum d(w, x) over x in vs is minimal }.  A graph is a median graph
   iff every triple has a unique median. *)

MedianVertices[ graph_Graph, vs_List ] :=
  With[ { V = VertexList[ graph ] },
    MinimalBy[ V,
      w |-> Total @ ( GraphDistance[ graph, w, # ] & /@ vs )
    ]
  ]
