Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Pointwise predicates ===================== *)

TropicalSegmentQ[ graph_Graph, S_List, u_, v_ ] :=
  MemberQ[ FindTropicalSegment[ graph, u, v, All ], Sort @ DeleteDuplicates @ S ]

GeodesicallyConvexQ[ graph_Graph, S_List ] :=
  With[ { set = Sort @ DeleteDuplicates @ S },
    FindGeodesicConvexHull[ graph, set ] === set
  ]

UniqueTropicalSegmentQ[ graph_Graph, u_, v_ ] :=
  Length @ FindTropicalSegment[ graph, u, v, All ] == 1


(* ===================== Tropical axioms ===================== *)

TropicalT1Q[ graph_Graph ] := ConnectedGraphQ[ graph ]
