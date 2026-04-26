Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindTropicalSegment ===================== *)

(* In the pure-metric reading of tropical convexity, the tropical segment
   between u and v is the vertex set of a graph geodesic.
   FindTropicalSegment returns these vertex sets (one per geodesic, each
   sorted), deduplicated. *)

FindTropicalSegment[ graph_Graph, u_, v_, All ] :=
  DeleteDuplicates[ Sort /@ allGeodesics[ graph, u, v ] ]

FindTropicalSegment[ graph_Graph, u_, v_, UpTo[ n_Integer ] ] :=
  Take[ FindTropicalSegment[ graph, u, v, All ], UpTo[ n ] ]

FindTropicalSegment[ graph_Graph, u_, v_, n_Integer : 1 ] :=
  With[ { result = FindTropicalSegment[ graph, u, v, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== FindGeodesicConvexHull ===================== *)

(* The geodesic-convex hull of a vertex set S is the smallest superset
   closed under taking metric intervals: FixedPoint of S |-> S union
   union over pairs of MetricInterval. *)

FindGeodesicConvexHull[ graph_Graph, S_List ] :=
  FixedPoint[
    set |-> Sort @ DeleteDuplicates @ Flatten[
      { set,
        Map[
          pair |-> MetricInterval[ graph, pair[[ 1 ]], pair[[ 2 ]] ],
          Subsets[ set, { 2 } ]
        ]
      }
    ],
    Sort @ DeleteDuplicates @ S
  ]
