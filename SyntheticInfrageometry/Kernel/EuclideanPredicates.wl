Package["WolframInstitute`SyntheticInfrageometry`"]


SegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

SegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False

LineQ[ graph_Graph, segment_List ] :=
  SegmentQ[ graph, segment ] &&
  Length[ First @ findLineExtensions[ graph, segment ] ] == Length[ segment ]

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

SphereQ[ graph_Graph, vs_List ] :=
  Length[ FindSphereParameters[ graph, vs ] ] > 0

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

UniqueSegmentQ[ graph_Graph, u_, v_ ] := GeodesicCount[ graph, u, v ] == 1

UniqueSegmentQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> UniqueSegmentQ[ graph, pair[[ 1 ]], pair[[ 2 ]] ]
  ]
