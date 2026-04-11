Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[pathFilterPairwiseDistances]
PackageScope[applySelect]


(* ===================== Distance Metrics ===================== *)

HausdorffDistance[ d_List, setX_, setY_ ] :=
  With[ { distSubMatrix = d[[ setX, setY ]] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]

HausdorffDistance[ g_Graph, setX_List, setY_List ] :=
  With[ { distSubMatrix = Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] },
    Max[ Max[ Min /@ distSubMatrix ], Max[ Min /@ Transpose @ distSubMatrix ] ]
  ]

FrechetDistance[ d_List, setX_, setY_, f_ : Max ] :=
  f[ Diagonal[ d[[ setX, setY ]] ] ]

FrechetDistance[ g_Graph, setX_List, setY_List, f_ : Max ] :=
  f[ Diagonal[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ] ]

MinimalSeparationDistance[ d_List, setX_, setY_ ] :=
  Min[ d[[ setX, setY ]] ]

MinimalSeparationDistance[ g_Graph, setX_List, setY_List ] :=
  Min[ Outer[ GraphDistance[ g, #1, #2 ] &, setX, setY, 1 ] ]

(* ===================== Embedding Distances ===================== *)

EmbeddingHausdorffDistance[ coords_List, path_List, { p1_, p2_ } ] /; Length[ path ] >= 2 :=
  RegionHausdorffDistance[ Line[ coords[[ path ]] ], Line[ { coords[[ p1 ]], coords[[ p2 ]] } ] ]

EmbeddingHausdorffDistance[ _List, path_List, { _, _ } ] /; Length[ path ] < 2 := 0

EmbeddingCircleDistance[ coords_List, cycle_List, centerIdx_Integer, radius_ ] /; Length[ cycle ] >= 3 :=
  Module[ { centerPt, cyclePts, cycleRegion, nPts, circlePoints, circleRegion },
    centerPt = coords[[ centerIdx ]];
    cyclePts = coords[[ cycle ]];
    cycleRegion = Line[ Append[ cyclePts, First[ cyclePts ] ] ];
    nPts = Max[ 64, 4 * Length[ cycle ] ];
    circlePoints = Table[
      centerPt + radius * { Cos[ t ], Sin[ t ] },
      { t, 0, 2 Pi - 2 Pi / nPts, 2 Pi / nPts }
    ];
    circleRegion = Line[ Append[ circlePoints, First[ circlePoints ] ] ];
    RegionHausdorffDistance[ cycleRegion, circleRegion ]
  ]

EmbeddingCircleDistance[ _List, cycle_List, _Integer, _ ] /; Length[ cycle ] < 3 := Infinity

(* ===================== Centrality ===================== *)

CentralElement[ distanceMatrix_List, n_ : 1 ] :=
  Module[ { scores, minScore, pool, selected, remaining },
    scores = Max /@ distanceMatrix;
    minScore = Min[ scores ];
    pool = Flatten @ Position[ scores, minScore ];
    If[ Length[ pool ] <= n, pool,
      selected = { First @ pool };
      remaining = Rest @ pool;
      Do[
        With[ { best = First @ MaximalBy[ remaining, idx |-> Min[ distanceMatrix[[ idx, selected ]] ] ] },
          AppendTo[ selected, best ];
          remaining = DeleteCases[ remaining, best ]
        ],
        { n - 1 }
      ];
      selected
    ]
  ]

PeripheralElement[ distanceMatrix_List, n_ : 1 ] :=
  Module[ { scores, maxScore, pool, selected, remaining },
    scores = Max /@ distanceMatrix;
    maxScore = Max[ scores ];
    pool = Flatten @ Position[ scores, maxScore ];
    If[ Length[ pool ] <= n, pool,
      selected = { First @ pool };
      remaining = Rest @ pool;
      Do[
        With[ { best = First @ MaximalBy[ remaining, idx |-> Max[ distanceMatrix[[ idx, selected ]] ] ] },
          AppendTo[ selected, best ];
          remaining = DeleteCases[ remaining, best ]
        ],
        { n - 1 }
      ];
      selected
    ]
  ]

(* ===================== Segment Utilities ===================== *)

SegmentEndpoints[ segment_List ] := { First[ segment ], Last[ segment ] }

(* ===================== Path Selection (internal) ===================== *)

pathFilterPairwiseDistances[ graph_Graph, paths_List, baseDist_, cyclic_ ] :=
  Module[ { distMatrix, vertexIndex, pathDistance },
    distMatrix = GraphDistanceMatrix[ graph ];
    vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
    pathDistance = If[ cyclic,
      ( Min @ Table[ baseDist[ #1, RotateLeft[ #2, k ], #3 ], { k, 0, Length[ #2 ] - 1 } ] & ),
      baseDist
    ];
    (# + Transpose[ # ]) & @ PadRight[ Table[
      pathDistance[ distMatrix, Lookup[ vertexIndex, paths[[ i ]] ], Lookup[ vertexIndex, paths[[ j ]] ] ],
      { i, Length[ paths ] }, { j, i - 1 } ], { Length[ paths ], Length[ paths ] } ]
  ]

applySelect[ _Graph, paths_List, None, _Association ] := paths

applySelect[ graph_Graph, paths_List, methods_List, context_Association ] :=
  Fold[ applySelect[ graph, #1, #2, context ] &, paths, methods ]

applySelect[ graph_Graph, paths_List, method_String, context_Association ] :=
  Module[ { cyclic, pd, scores, coords, vertexIndex },
    If[ Length[ paths ] <= 1, Return[ paths ] ];
    cyclic = TrueQ @ context[ "Cyclic" ];
    Switch[ method,
      "ShortestCircumference", MinimalBy[ paths, Length ],
      "LongestCircumference", MaximalBy[ paths, Length ],
      "FrechetCentral",
        pd = pathFilterPairwiseDistances[ graph, paths, FrechetDistance, cyclic ];
        scores = Max /@ pd;
        Pick[ paths, scores, Min[ scores ] ],
      "FrechetPeripheral",
        pd = pathFilterPairwiseDistances[ graph, paths, FrechetDistance, cyclic ];
        scores = Max /@ pd;
        Pick[ paths, scores, Max[ scores ] ],
      "MeanFrechetCentral",
        pd = pathFilterPairwiseDistances[ graph, paths, FrechetDistance[ ##, Mean ] &, cyclic ];
        scores = Max /@ pd;
        Pick[ paths, scores, Min[ scores ] ],
      "MeanFrechetPeripheral",
        pd = pathFilterPairwiseDistances[ graph, paths, FrechetDistance[ ##, Mean ] &, cyclic ];
        scores = Max /@ pd;
        Pick[ paths, scores, Max[ scores ] ],
      "HausdorffCentral",
        pd = pathFilterPairwiseDistances[ graph, paths, HausdorffDistance, cyclic ];
        scores = Max /@ pd;
        Pick[ paths, scores, Min[ scores ] ],
      "HausdorffPeripheral",
        pd = pathFilterPairwiseDistances[ graph, paths, HausdorffDistance, cyclic ];
        scores = Max /@ pd;
        Pick[ paths, scores, Max[ scores ] ],
      "EmbeddingClosest",
        coords = GraphEmbedding[ graph ];
        vertexIndex = AssociationThread[ VertexList[ graph ], Range @ VertexCount[ graph ] ];
        If[ cyclic,
          MinimalBy[ paths, cycle |-> EmbeddingCircleDistance[ coords, Lookup[ vertexIndex, cycle ],
            vertexIndex @ context[ "Center" ], context[ "Radius" ] ] ],
          With[ { ep = Lookup[ vertexIndex, context[ "Endpoints" ] ] },
            MinimalBy[ paths, path |-> EmbeddingHausdorffDistance[ coords, Lookup[ vertexIndex, path ], ep ] ]
          ]
        ],
      _, paths
    ]
  ]

(* ===================== Separating Cycles ===================== *)

SeparatingCycleQ[ graph_Graph, cycle_List, center_, radius_ ] :=
  Module[ { rem, comps, centerComp },
    rem = VertexDelete[ graph, cycle ];
    comps = ConnectedComponents[ rem ];
    centerComp = SelectFirst[ comps, MemberQ[ #, center ] & ];
    centerComp =!= Missing[ "NotFound" ] &&
    AllTrue[ centerComp, GraphDistance[ graph, center, # ] <= radius & ] &&
    AllTrue[ Complement[ VertexList[ rem ], centerComp ], GraphDistance[ graph, center, # ] > radius & ]
  ]

FindSeparatingCycles[ graph_Graph, cycles_List, center_, radius_ ] :=
  Select[ cycles, SeparatingCycleQ[ graph, #, center, radius ] & ]
