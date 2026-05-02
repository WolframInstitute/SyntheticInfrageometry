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


(* FindShellParameters[g, vs] returns the {center, radius} pairs for
   which the vertex set vs is a metric shell: in some component of g \ vs,
   a center c is equidistant (= r) from every vertex of vs, dominates that
   component (d(c, w) <= r), and is strictly closer to the inside than to
   the outside (d(c, w) > r for w outside).  This is the constructive
   companion of ShellQ. *)

FindShellParameters[ graph_Graph, vs_List ] :=
  Module[ { rem, comps },
    rem = VertexDelete[ graph, vs ];
    comps = ConnectedComponents[ rem ];
    Flatten[ Table[
      With[ {
          distMatrix = GraphDistanceMatrix[ Subgraph[ graph, comp ] ],
          otherVertices = Complement[ VertexList[ rem ], comp ] },
        With[ { scores = Max /@ distMatrix },
          With[ { centers = Pick[ comp, scores, Min[ scores ] ] },
            Select[
              { #, GraphDistance[ graph, #, First[ vs ] ] } & /@ centers,
              pair |-> With[ { v = pair[[ 1 ]], r = pair[[ 2 ]] },
                AllTrue[ vs, GraphDistance[ graph, v, # ] == r & ] &&
                AllTrue[ comp, GraphDistance[ graph, v, # ] <= r & ] &&
                AllTrue[ otherVertices, GraphDistance[ graph, v, # ] > r & ]
              ]
            ]
          ]
        ]
      ],
      { comp, comps }
    ], 1 ]
  ]

(* ShellQ[g, vs] : the vertex set vs is a metric shell of g iff
   FindShellParameters returns at least one valid (center, radius). *)

ShellQ[ graph_Graph, vs_List ] :=
  Length[ FindShellParameters[ graph, vs ] ] > 0


(* CircleQ[g, cycle] : the vertex sequence cycle = (v0, ..., vk) is a
   metric circle of g iff (a) consecutive vertices are adjacent and the
   wrap-around edge (vk, v0) exists, (b) the vertex set is a metric shell.
   Accepts both open ({v0, ..., vk}, vk != v0) and closed ({v0, ..., vk, v0})
   input. *)

CircleQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    With[ {
        verts = Most @ closed,
        pairs = Partition[ closed, 2, 1 ] },
      DuplicateFreeQ[ verts ] &&
      AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
      Length[ FindShellParameters[ graph, verts ] ] > 0
    ]
  ]

CircleQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False


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


(* SeparatesQ tests whether deleting the vertex set vs disconnects u from v.
   Endpoint deletion does not count as separation. *)

SeparatesQ[ graph_Graph, vs_List, u_, v_ ] :=
  If[ MemberQ[ vs, u ] || MemberQ[ vs, v ], False,
    GraphDistance[ VertexDelete[ graph, vs ], u, v ] === Infinity
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
