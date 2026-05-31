Package["WolframInstitute`SyntheticInfrageometry`"]


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


(* ===================== GeodesicMultiplicity ===================== *)

(* GeodesicMultiplicity[g, u, v] = (A^d)[u, v] where A is the adjacency matrix
   and d = d(u, v).  Identity: a walk of length d(u, v) from u to v is
   automatically a simple geodesic, so the (u, v) entry of A^d counts
   geodesics.  Polynomial-time replacement for path enumeration. *)

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

(* The pair (D, M): D is the graph distance matrix, M[i, j] is the number
   of geodesics from vertex i to vertex j (= (A^{D[i,j]})[i, j]).  M is
   the multiplicity counterpart of the metric. *)

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

(* The metric medians of a vertex set vs are the argmin of total distance:
   { w : Sum d(w, x) over x in vs is minimal }.  A graph is a median graph
   iff every triple has a unique median -- and median graphs coincide with
   1-skeletons of CAT(0) cube complexes (Chepoi 2000,
   https://doi.org/10.1006/aama.1999.0681). *)

MedianVertices[ graph_Graph, vs_List ] :=
  With[ { V = VertexList[ graph ] },
    MinimalBy[ V,
      w |-> Total @ ( GraphDistance[ graph, w, # ] & /@ vs )
    ]
  ]


(* ===================== Segment hull ===================== *)

(* The segment hull of a vertex set S is the smallest superset closed under
   taking metric intervals: FixedPoint of T |-> T union over pairs of
   MetricInterval.  The Farber-Jamison geodesic convex hull (no metric
   convexity is involved -- the closed sets form only an abstract convexity;
   cf. Wiki/Concepts/Hulls.md).  Companion line hull: FindLineHull in
   InfraLine.wl.  Option "LineStructure": None (default) closes under all
   geodesics; an InfraLineStructure (or bare list of lines) closes under the
   chosen-geodesic stretch between members on each line of that fixed family. *)

Options[ FindSegmentHull ] = { "LineStructure" -> None };

FindSegmentHull[ graph_Graph, S_List, OptionsPattern[] ] :=
  With[ { spec = OptionValue[ "LineStructure" ] },
    If[ spec === None,
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

(* S is segment-closed: it equals its own segment hull (every geodesic, or
   every chosen-line stretch under "LineStructure", between members lies in S). *)

Options[ SegmentHullQ ] = { "LineStructure" -> None };

SegmentHullQ[ graph_Graph, S_List, opts : OptionsPattern[] ] := FindSegmentHull[ graph, S, opts ] === Union @ S
