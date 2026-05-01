Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindTarskiReflection ===================== *)

(* The reflection of x through a in Tarski geometry: a vertex x' such
   that B(x, a, x') and ax == ax'.  On a graph, x' lies on the geodesic
   continuation of x past a at distance d(a, x).  Multi-valued in
   general (cycles and graphs with multiple geodesics admit several). *)

FindTarskiReflection[ graph_Graph, x_, a_, All ] :=
  With[ { r = GraphDistance[ graph, a, x ] },
    If[ r === Infinity, { },
      Select[ VertexList[ graph ],
        y |-> BetweennessQ[ graph, x, a, y ] && GraphDistance[ graph, a, y ] === r
      ]
    ]
  ]

FindTarskiReflection[ graph_Graph, x_, a_, UpTo[ n_Integer ] ] :=
  With[ { result = FindTarskiReflection[ graph, x, a, All ] },
    Take[ result, UpTo[ n ] ]
  ]

FindTarskiReflection[ graph_Graph, x_, a_, n_Integer : 1 ] :=
  With[ { result = FindTarskiReflection[ graph, x, a, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== FindTarskiMidpoint ===================== *)

(* The synthetic midpoint of (a, b) in Tarski geometry: a vertex m with
   B(a, m, b) and am == mb.  Defined only from B and E - no metric
   centrality, no interval-center heuristic.  Returns { } when no such
   vertex exists; in particular, odd-distance pairs have no synthetic
   midpoint, while FindMidpoint still picks a central interval element. *)

FindTarskiMidpoint[ graph_Graph, a_, b_, All ] :=
  Select[ VertexList[ graph ],
    m |-> BetweennessQ[ graph, a, m, b ] &&
          GraphDistance[ graph, a, m ] === GraphDistance[ graph, m, b ]
  ]

FindTarskiMidpoint[ graph_Graph, a_, b_, UpTo[ n_Integer ] ] :=
  With[ { result = FindTarskiMidpoint[ graph, a, b, All ] },
    Take[ result, UpTo[ n ] ]
  ]

FindTarskiMidpoint[ graph_Graph, a_, b_, n_Integer : 1 ] :=
  With[ { result = FindTarskiMidpoint[ graph, a, b, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]
