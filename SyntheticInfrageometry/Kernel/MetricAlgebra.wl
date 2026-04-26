Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[tropicalProduct]
PackageScope[tropicalPower]


(* ===================== Tropical matrix helpers (internal) ===================== *)

tropicalProduct[ A_?MatrixQ, B_?MatrixQ ] := Inner[ Plus, A, B, Min ]

tropicalPower[ A_?MatrixQ, 1 ] := A
tropicalPower[ A_?MatrixQ, k_Integer /; k > 1 ] :=
  Nest[ tropicalProduct[ #, A ] &, A, k - 1 ]


(* ===================== MetricInterval ===================== *)

MetricInterval[ graph_Graph, u_, v_ ] :=
  With[ { d = GraphDistance[ graph, u, v ] },
    If[ d === Infinity, {},
      Select[ VertexList[ graph ],
        w |-> GraphDistance[ graph, u, w ] + GraphDistance[ graph, w, v ] == d
      ]
    ]
  ]


(* ===================== Tarski primitives ===================== *)

BetweennessQ[ graph_Graph, u_, w_, v_ ] :=
  With[ { duw = GraphDistance[ graph, u, w ],
          dwv = GraphDistance[ graph, w, v ],
          duv = GraphDistance[ graph, u, v ] },
    duw =!= Infinity && dwv =!= Infinity && duv =!= Infinity && duw + dwv == duv
  ]

EquidistanceQ[ graph_Graph, a_, b_, c_, d_ ] :=
  GraphDistance[ graph, a, b ] === GraphDistance[ graph, c, d ]


(* ===================== GeodesicCount ===================== *)

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

DistanceMatrixQ[ M_?MatrixQ ] :=
  And[
    SquareMatrixQ[ M ],
    Transpose[ M ] === M,
    AllTrue[ Diagonal[ M ], # === 0 & ],
    AllTrue[ Flatten[ M ],
      # === Infinity || ( IntegerQ[ # ] && # >= 0 ) || ( NumericQ[ # ] && # >= 0 ) & ],
    tropicalProduct[ M, M ] === M
  ]

DistanceMatrixQ[ _ ] := False


(* ===================== MedianVertices ===================== *)

MedianVertices[ graph_Graph, vs_List ] :=
  With[ { V = VertexList[ graph ] },
    MinimalBy[ V,
      w |-> Total @ ( GraphDistance[ graph, w, # ] & /@ vs )
    ]
  ]
