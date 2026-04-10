MetricMidpoint::usage = "MetricMidpoint[segment] finds the midpoint of a segment.";
MetricMidpoint[ segment_List ] /; Length[ segment ] >= 2 :=
  segment[[ Ceiling[ Length[ segment ] / 2 ] ]]

MetricPerpendicular::usage = "MetricPerpendicular[graph, line, point] finds the foot of perpendicular from point to line via the isosceles base midpoint construction (Euclid I.12). MetricPerpendicular[graph, line, point, n] finds n feet. Also accepts a distance matrix instead of graph.";
MetricPerpendicular[ distanceMatrix_List, line_List, point_Integer, n_ : 1 ] :=
  Module[ { distances, byIndex, feet },
    distances = distanceMatrix[[ point, line ]];
    byIndex = Values @ GroupBy[ Range @ Length @ line, distances[[ # ]] & ];
    feet = Union @ Flatten @ Table[
      Table[
        With[ { lo = Min[ pair[[1]], pair[[2]] ], hi = Max[ pair[[1]], pair[[2]] ] },
          If[ OddQ[ hi - lo ], MetricMidpoint[ line[[ lo ;; hi ]] ], Nothing ]
        ],
        { pair, Subsets[ group, { 2 } ] }
      ],
      { group, byIndex }
    ];
    If[ n === All, feet, Take[ feet, UpTo[ n ] ] ]
  ]

MetricPerpendicular[ graph_Graph, line_List, point_, n_ : 1 ] :=
  Module[ { distances, byIndex, feet },
    distances = GraphDistance[ graph, point, # ] & /@ line;
    byIndex = Values @ GroupBy[ Range @ Length @ line, distances[[ # ]] & ];
    feet = Union @ Flatten @ Table[
      Table[
        With[ { lo = Min[ pair[[1]], pair[[2]] ], hi = Max[ pair[[1]], pair[[2]] ] },
          If[ OddQ[ hi - lo ], MetricMidpoint[ line[[ lo ;; hi ]] ], Nothing ]
        ],
        { pair, Subsets[ group, { 2 } ] }
      ],
      { group, byIndex }
    ];
    If[ n === All, feet, Take[ feet, UpTo[ n ] ] ]
  ]

CompleteEquilateralTriangle::usage = "CompleteEquilateralTriangle[graph, p1, p2] finds a vertex equidistant from both endpoints. CompleteEquilateralTriangle[graph, p1, p2, n] finds n such vertices. Also accepts a distance matrix instead of graph.";
CompleteEquilateralTriangle[ distanceMatrix_List, p1_Integer, p2_Integer, n_ : 1 ] :=
  With[ { r = distanceMatrix[[ p1, p2 ]],
           circle1 = Flatten @ Position[ distanceMatrix[[ p1 ]], distanceMatrix[[ p1, p2 ]] ],
           circle2 = Flatten @ Position[ distanceMatrix[[ p2 ]], distanceMatrix[[ p1, p2 ]] ] },
    With[ { candidates = Intersection[ circle1, circle2 ] },
      If[ n === All, candidates, Take[ candidates, UpTo[ n ] ] ]
    ]
  ]

CompleteEquilateralTriangle[ graph_Graph, p1_, p2_, n_ : 1 ] :=
  With[ { r = GraphDistance[ graph, p1, p2 ] },
    If[ r === Infinity, {},
      With[ { candidates = Intersection[
          Select[ VertexList[ graph ], GraphDistance[ graph, p1, # ] == r & ],
          Select[ VertexList[ graph ], GraphDistance[ graph, p2, # ] == r & ] ] },
        If[ n === All, candidates, Take[ candidates, UpTo[ n ] ] ]
      ]
    ]
  ]

MetricParallel::usage = "MetricParallel[graph, line, point] constructs a parallel line through point to line via the double-perpendicular construction (Euclid I.31). MetricParallel[graph, line, point, n] returns n parallel lines.";
MetricParallel[ graph_Graph, line_List, point_, n_ : 1 ] :=
  Module[ { feet, foot, fpLine, dFromFoot, dFromPoint, symmPairs, candidates },
    If[ line === {}, Return[ {} ] ];
    feet = MetricPerpendicular[ graph, line, point ];
    If[ feet === {}, Return[ {} ] ];
    foot = First @ feet;
    fpLine = FindLine[ graph, { foot, point } ];
    dFromFoot = AssociationMap[ GraphDistance[ graph, foot, # ] &, VertexList[ graph ] ];
    dFromPoint = AssociationMap[ GraphDistance[ graph, point, # ] &, VertexList[ graph ] ];
    symmPairs = Select[
      Subsets[ VertexList[ graph ], { 2 } ],
      Function[ { uv },
        With[ { u = uv[[ 1 ]], v = uv[[ 2 ]] },
          dFromPoint[ u ] == dFromPoint[ v ] &&
          dFromPoint[ u ] + dFromPoint[ v ] == GraphDistance[ graph, u, v ] &&
          dFromFoot[ u ] == dFromFoot[ v ] &&
          !( MemberQ[ fpLine, u ] && MemberQ[ fpLine, v ] )
        ]
      ]
    ];
    candidates = Union @ Flatten[ Table[
      With[ { extended = FindLine[ graph, { pair[[ 1 ]], point, pair[[ 2 ]] } ] },
        If[ !IntersectQ[ extended, line ], { extended }, {} ]
      ],
      { pair, symmPairs }
    ], 1 ];
    If[ n === All, candidates, Take[ candidates, UpTo[ n ] ] ]
  ]
