(* ::Package:: *)

SegmentQ::usage = "SegmentQ[graph, segment] tests whether a list of vertices forms a valid geodesic segment.";
SegmentQ[ graph_Graph, segment_List ] /; Length[ segment ] >= 2 :=
  GraphDistance[ graph, First[ segment ], Last[ segment ] ] == Length[ segment ] - 1 &&
  AllTrue[ Partition[ segment, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ]

SegmentQ[ _Graph, segment_List ] /; Length[ segment ] < 2 := False

CircleQ::usage = "CircleQ[graph, cycle, center, radius] tests whether a cycle is a valid metric circle: connected, equidistant from center, and separating.";
CircleQ[ graph_Graph, cycle_List, center_, radius_ ] :=
  AllTrue[ Partition[ Append[ cycle, First[ cycle ] ], 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
  AllTrue[ cycle, GraphDistance[ graph, center, # ] == radius & ] &&
  SeparatingCycleQ[ graph, cycle, center, radius ]

LineQ::usage = "LineQ[graph, segment] tests whether a segment is maximal — cannot be extended to a longer geodesic.";
LineQ[ graph_Graph, segment_List ] :=
  SegmentQ[ graph, segment ] &&
  Length[ First @ FindLine[ graph, segment, 1 ] ] == Length[ segment ]

IntersectQ::usage = "IntersectQ[set1, set2] tests whether two sets have a non-empty intersection.";
IntersectQ[ set1_List, set2_List ] :=
  Intersection[ set1, set2 ] =!= {}

ParallelQ::usage = "ParallelQ[graph, l1, l2] tests whether two lines are parallel (constant distance). ParallelQ[graph, l1, l2, threshold] allows distance variation up to threshold. Also accepts a distance matrix instead of graph.";
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

SegmentLineAngle::usage = "SegmentLineAngle[graph, p1, p2, line] or SegmentLineAngle[graph, segment, line] measures the distance from segment endpoint to a line.";
SegmentLineAngle[ distanceMatrix_List, p1_Integer, p2_Integer, line_List ] :=
  If[ !MemberQ[ line, p1 ], Infinity, Min[ distanceMatrix[[ p2, line ]] ] ]

SegmentLineAngle[ graph_Graph, p1_, p2_, line_List ] :=
  If[ !MemberQ[ line, p1 ], Infinity, Min[ GraphDistance[ graph, p2, # ] & /@ line ] ]

SegmentLineAngle[ graph_Graph, segment_List, line_List ] /; Length[ segment ] >= 2 :=
  SegmentLineAngle[ graph, First[ segment ], Last[ segment ], line ]
