Package["WolframInstitute`SyntheticInfrageometry`"]


SegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

SegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False

CircleQ[ graph_Graph, cycle_List, center_, radius_ ] :=
  AllTrue[ Partition[ Append[ cycle, First[ cycle ] ], 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
  AllTrue[ cycle, GraphDistance[ graph, center, # ] == radius & ] &&
  SeparatingCycleQ[ graph, cycle, center, radius ]

LineQ[ graph_Graph, segment_List ] :=
  SegmentQ[ graph, segment ] &&
  Length[ First @ FindLine[ graph, segment, 1 ] ] == Length[ segment ]

IntersectQ[ set1_List, set2_List ] :=
  Intersection[ set1, set2 ] =!= {}

ParallelQ[ distanceMatrix_List, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectQ[ l1, l2 ], False,
    With[ { lineDistances = Min[ distanceMatrix[[ #, l2 ]] ] & /@ l1 },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold
    ]
  ]

ParallelQ[ graph_Graph, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectQ[ l1, l2 ], False,
    With[ { lineDistances = Table[ Min[ GraphDistance[ graph, v, # ] & /@ l2 ], { v, l1 } ] },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold
    ]
  ]

SegmentLineAngle[ distanceMatrix_List, p1_Integer, p2_Integer, line_List ] :=
  If[ !MemberQ[ line, p1 ], Infinity, Min[ distanceMatrix[[ p2, line ]] ] ]

SegmentLineAngle[ graph_Graph, p1_, p2_, line_List ] :=
  If[ !MemberQ[ line, p1 ], Infinity, Min[ GraphDistance[ graph, p2, # ] & /@ line ] ]

SegmentLineAngle[ graph_Graph, segment_List, line_List ] /; Length[ segment ] >= 2 :=
  SegmentLineAngle[ graph, First[ segment ], Last[ segment ], line ]
