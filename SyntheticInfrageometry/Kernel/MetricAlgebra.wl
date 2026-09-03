Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== MetricInterval ===================== *)

(* I(u, v) = { w : d(u, w) + d(w, v) == d(u, v) }, the union of all geodesics from u to v *)

MetricInterval[ graph_Graph, u_, v_ ] :=
  With[ { d = GraphDistance[ graph, u, v ] },
    If[ d === Infinity, {},
      Select[ VertexList[ graph ],
        w |-> GraphDistance[ graph, u, w ] + GraphDistance[ graph, w, v ] == d
      ]
    ]
  ]


(* ===================== GeodesicMultiplicity ===================== *)

(* (A^d)[u, v] with d = d(u, v): a walk of length d(u, v) is automatically a simple geodesic, so the entry counts geodesics *)

GeodesicMultiplicity[ graph_Graph, u_, v_ ] :=
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


(* ===================== GeodesicMultiplicityMatrix ===================== *)

(* D the distance matrix, M[i, j] = (A^{D[i,j]})[i, j] the number of geodesics from i to j *)

GeodesicMultiplicityMatrix[ graph_Graph ] :=
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


(* ===================== MedianVertices ===================== *)

(* argmin over w of Sum_x d(w, x) for x in vs; a graph is median iff every triple has a unique median, and median graphs are the 1-skeletons of CAT(0) cube complexes (Chepoi 2000, https://doi.org/10.1006/aama.1999.0681) *)

MedianVertices[ graph_Graph, vs_List ] :=
  With[ { V = VertexList[ graph ] },
    MinimalBy[ V,
      w |-> Total @ ( GraphDistance[ graph, w, # ] & /@ vs )
    ]
  ]


(* ===================== Segment hull ===================== *)

(* the smallest superset closed under metric intervals: FixedPoint of T |-> T union MetricInterval over pairs.  The Farber-Jamison geodesic convex hull -- an abstract convexity, not a metric one *)

Options[ FindSegmentHull ] = { "LineStructure" -> None };

FindSegmentHull[ graph_Graph, s : Except[ _Rule | _RuleDelayed ], OptionsPattern[] ] :=
  With[ { spec = OptionValue[ "LineStructure" ], S = hullVertices @ s },
    InfraSet @ If[ spec === None,
      FixedPoint[
        T |-> Union[ T, Catenate @ Map[
          pair |-> MetricInterval[ graph, pair[[ 1 ]], pair[[ 2 ]] ], Subsets[ T, { 2 } ] ] ],
        Union @ S
      ],
      With[ { lines = Replace[ spec, ls_InfraLineStructure :> ls[ "Lines" ] ] },
        FixedPoint[
          T |-> Union[ T, Catenate @ Map[
            ell |-> With[ { idx = Flatten @ Position[ ell, Alternatives @@ T ] },
              If[ Length[ idx ] >= 2, ell[[ Min[ idx ] ;; Max[ idx ] ]], { } ] ],
            lines ] ],
          Union @ S
        ]
      ]
    ]
  ]

(* S equals its own segment hull *)

Options[ SegmentHullQ ] = { "LineStructure" -> None };

SegmentHullQ[ graph_Graph, s : Except[ _Rule | _RuleDelayed ], opts : OptionsPattern[] ] :=
  With[ { vs = hullVertices @ s }, FindSegmentHull[ graph, vs, opts ][ "Vertices" ] === Union @ vs ]
