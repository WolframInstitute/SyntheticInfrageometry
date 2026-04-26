Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Pointwise predicates ===================== *)

(* TropicalSegmentQ: the vertex set S realises a tropical segment from u
   to v - i.e. S is the sorted vertex set of some geodesic u -> v. *)

TropicalSegmentQ[ graph_Graph, S_List, u_, v_ ] :=
  MemberQ[ FindTropicalSegment[ graph, u, v, All ], Sort @ DeleteDuplicates @ S ]


(* GeodesicallyConvexQ: S is closed under taking metric intervals - it
   equals its own geodesic-convex hull. *)

GeodesicallyConvexQ[ graph_Graph, S_List ] :=
  With[ { set = Sort @ DeleteDuplicates @ S },
    FindGeodesicConvexHull[ graph, set ] === set
  ]


(* UniqueTropicalSegmentQ: the tropical segment u -> v is single-valued
   (only one geodesic, equivalently UniqueSegmentQ). *)

UniqueTropicalSegmentQ[ graph_Graph, u_, v_ ] :=
  Length @ FindTropicalSegment[ graph, u, v, All ] == 1


(* ===================== Tropical axioms ===================== *)

(* T1: every two vertices admit a tropical segment.  In the pure-metric
   reading this is the connectedness of the graph. *)

TropicalT1Q[ graph_Graph ] := ConnectedGraphQ[ graph ]
