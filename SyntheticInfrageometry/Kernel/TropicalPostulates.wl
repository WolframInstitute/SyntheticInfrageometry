Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindTropicalSegment ===================== *)

FindTropicalSegment[ graph_Graph, u_, v_, All ] :=
  DeleteDuplicates[ Sort /@ FindSegment[ graph, u, v, All ] ]

FindTropicalSegment[ graph_Graph, u_, v_, UpTo[ n_Integer ] ] :=
  Take[ FindTropicalSegment[ graph, u, v, All ], UpTo[ n ] ]

FindTropicalSegment[ graph_Graph, u_, v_, n_Integer : 1 ] :=
  With[ { result = FindTropicalSegment[ graph, u, v, UpTo[ n ] ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]


(* ===================== FindGeodesicConvexHull ===================== *)

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
