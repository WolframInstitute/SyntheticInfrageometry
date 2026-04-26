Package["WolframInstitute`SyntheticInfrageometry`"]


(* SegmentQ tests whether a vertex sequence (v0, ..., vk) realises a
   geodesic from v0 to vk: consecutive vertices adjacent and total length
   equal to d(v0, vk). *)

SegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

SegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False


(* LineQ tests maximality: a segment is a line iff no extension preserves
   the geodesic property (findLineExtensions returns the segment itself). *)

LineQ[ graph_Graph, segment_List ] :=
  SegmentQ[ graph, segment ] &&
  Length[ First @ findLineExtensions[ graph, segment ] ] == Length[ segment ]


(* FindSphereParameters[g, cycle] returns the {center, radius} pairs for
   which the cycle is a metric sphere: in some component of g \ cycle, a
   center c is equidistant (= r) from every cycle vertex, dominates that
   component (d(c, w) <= r), and is strictly closer to the inside than to
   the outside (d(c, w) > r for w outside).  This is the constructive
   companion of SphereQ. *)

FindSphereParameters[ graph_Graph, cycle_List ] :=
  Module[ { rem, comps },
    rem = VertexDelete[ graph, cycle ];
    comps = ConnectedComponents[ rem ];
    Flatten[ Table[
      Module[ { subgraph, distMatrix, scores, minScore, centers, otherVertices },
        subgraph = Subgraph[ graph, comp ];
        distMatrix = GraphDistanceMatrix[ subgraph ];
        scores = Max /@ distMatrix;
        minScore = Min[ scores ];
        centers = Pick[ comp, scores, minScore ];
        otherVertices = Complement[ VertexList[ rem ], comp ];
        Select[
          { #, GraphDistance[ graph, #, First[ cycle ] ] } & /@ centers,
          pair |-> With[ { v = pair[[ 1 ]], r = pair[[ 2 ]] },
            AllTrue[ cycle, GraphDistance[ graph, v, # ] == r & ] &&
            AllTrue[ comp, GraphDistance[ graph, v, # ] <= r & ] &&
            AllTrue[ otherVertices, GraphDistance[ graph, v, # ] > r & ]
          ]
        ]
      ],
      { comp, comps }
    ], 1 ]
  ]

(* SphereQ[g, vs] : the vertex set vs is a metric sphere of g iff
   FindSphereParameters returns at least one valid (center, radius). *)

SphereQ[ graph_Graph, vs_List ] :=
  Length[ FindSphereParameters[ graph, vs ] ] > 0


(* ParallelQ tests definition-alpha parallelism: l1 and l2 are parallel iff
   they are disjoint and the distance from each vertex of l1 to l2 is
   constant (up to threshold).  Distance to a set is min of pointwise
   distances. *)

ParallelQ[ distanceMatrix_List, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Min[ distanceMatrix[[ #, l2 ]] ] & /@ l1 },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold
    ]
  ]

ParallelQ[ graph_Graph, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Table[ Min[ GraphDistance[ graph, v, # ] & /@ l2 ], { v, l1 } ] },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold
    ]
  ]


(* ===================== UniqueSegmentQ ===================== *)

(* UniqueSegmentQ[g, u, v]: there is a unique geodesic from u to v
   (GeodesicCount == 1).  Whole-graph form UniqueSegmentQ[g] is the
   geodetic-graph predicate: every pair of vertices admits a unique
   geodesic. *)

UniqueSegmentQ[ graph_Graph, u_, v_ ] := GeodesicCount[ graph, u, v ] == 1

UniqueSegmentQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> UniqueSegmentQ[ graph, pair[[ 1 ]], pair[[ 2 ]] ]
  ]
