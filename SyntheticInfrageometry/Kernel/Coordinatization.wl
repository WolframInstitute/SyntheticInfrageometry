Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[LaminarLayers]
PackageScope[laminarLayersFromSources]
PackageScope[axisLayerIndex]
PackageScope[findLongestPaths]
PackageScope[findLongestGeodesicThrough]


FindRadarBasis[ g_, n_ : 1, m_ : All ] :=
  Module[ { v = VertexList[ g ], dm = GraphDistanceMatrix[ g ], vc = VertexCount[ g ], found = {}, mask, last },
    Map[ v[[ # ]] &,
      Catch[ Scan[
        k |-> (
          mask = 2^k - 1;
          last = BitShiftLeft[ 2^k - 1, vc - k ];
          While[ mask <= last,
            With[ { s = Pick[ Range[ vc ], IntegerDigits[ mask, 2, vc ], 1 ] },
              If[ DuplicateFreeQ[ dm[[ All, s ]] ],
                AppendTo[ found, s ];
                If[ Length @ found >= n, Throw[ found ] ]
              ]
            ];
            mask = With[ { c = BitAnd[ mask, -mask ] }, { r = mask + c },
              BitOr[ r, Quotient[ BitXor[ r, mask ], 4 c ] ] ]
          ]
        ),
        Replace[ m, {
          All :> Range[ vc ],
          _Integer :> Range[ m ],
          { min_, max_ } :> Range[ min, max ],
          { num_ } :> { num }
        } ]
      ]; Throw[ found ] ]
    ]
  ]

RadarBasisQ[ g_Graph, b_List ] :=
  DuplicateFreeQ[ GraphDistance[ g, # ] & /@ b ]

RadarCoordinates[ g_Graph, v_, b_List ] :=
  GraphDistance[ g, v, # ] & /@ b


LaminarLayers[ g_Graph, line_List ] :=
  List /@ line

LaminarLayers[ g_Graph, dag_Graph ] :=
  laminarLayersFromSources[ dag, Select[ VertexList[ dag ], VertexInDegree[ dag, # ] == 0 & ] ]

laminarLayersFromSources[ dag_Graph, sources_List ] :=
  Module[ { depth, maxDepth },
    depth = v |-> Min[ GraphDistance[ dag, #, v ] & /@ sources ];
    maxDepth = Max[ depth /@ VertexList[ dag ] ];
    Table[ Select[ VertexList[ dag ], depth[ # ] == k & ], { k, 0, maxDepth } ]
  ]


axisLayerIndex[ g_Graph, axis_List, v_ ] :=
  Module[ { dists, minD, proj },
    dists = GraphDistance[ g, v, # ] & /@ axis;
    minD = Min[ dists ];
    proj = Pick[ axis, dists, minD ];
    Min[ Flatten[ FirstPosition[ axis, # ] & /@ proj ] ] - 1
  ]

axisLayerIndex[ g_Graph, dag_Graph, v_ ] :=
  Module[ { layers, verts, dists, minD, proj },
    layers = LaminarLayers[ g, dag ];
    verts = VertexList[ dag ];
    dists = GraphDistance[ g, v, # ] & /@ verts;
    minD = Min[ dists ];
    proj = Pick[ verts, dists, minD ];
    Min @ Flatten @ Table[
      Position[ layers, u ][[ All, 1 ]] - 1,
      { u, proj }
    ]
  ]


Options[ AxesCoordinates ] = { Method -> "ShortestPaths", "Origin" -> None };

AxesCoordinates[ g_Graph, axes_List, v_, opts : OptionsPattern[] ] /; !MemberQ[ VertexList[ g ], axes ] :=
  Switch[ OptionValue[ Method ],
    "ShortestPaths",
      With[ { origin = OptionValue[ "Origin" ] },
        If[ origin === None,
          axisLayerIndex[ g, #, v ] & /@ axes,
          With[ { originIdx = axisLayerIndex[ g, #, origin ] & /@ axes,
                   vIdx = axisLayerIndex[ g, #, v ] & /@ axes },
            vIdx - originIdx
          ]
        ]
      ],
    "ParallelLines", $Failed,
    _, $Failed
  ]

AxesCoordinates[ g_Graph, axes_List, opts : OptionsPattern[] ] /; !MemberQ[ VertexList[ g ], axes ] :=
  Association[ # -> AxesCoordinates[ g, axes, #, opts ] & /@ VertexList[ g ] ]

AxesCoordinates[ g_Graph, c_, v_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] && MemberQ[ VertexList[ g ], v ] :=
  AxesCoordinates[ g, FindOrthogonalAxes[ g, c, All ], v, "Origin" -> c, opts ]

AxesCoordinates[ g_Graph, c_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { axes = FindOrthogonalAxes[ g, c, All ] },
    Association[ # -> AxesCoordinates[ g, axes, #, "Origin" -> c, opts ] & /@ VertexList[ g ] ]
  ]


(* ===================== Orthogonal axes ===================== *)

findLongestPaths[ g_Graph, n_, epsilon_ : 0 ] :=
  Module[ { vertices, distMatrix, maxDist, pairs, numPairs, counts },
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    maxDist = Max[ distMatrix ];
    pairs = DeleteDuplicatesBy[ Position[ distMatrix, _?( # >= maxDist - epsilon & ) ], Sort ];
    pairs = Select[ pairs, #[[ 1 ]] =!= #[[ 2 ]] & ];
    numPairs = Length[ pairs ];
    If[ numPairs == 0, Return[ {} ] ];
    counts = If[ n === All,
      ConstantArray[ All, numPairs ],
      RandomSample @ Table[ Quotient[ n, numPairs ] + Boole[ i <= Mod[ n, numPairs ] ], { i, numPairs } ]
    ];
    Flatten[
      Cases[
        Transpose[ { pairs, counts } ],
        { { i_, j_ }, cnt_ /; cnt =!= 0 } :> FindPath[ g, vertices[[ i ]], vertices[[ j ]], { distMatrix[[ i, j ]] }, cnt ]
      ],
      1
    ]
  ]

findLongestGeodesicThrough[ g_Graph, v_, n_ ] :=
  Module[ { vertices, distMatrix, vertexIdx, distFromVertex, candidates, maxLen, pairs, counts },
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIdx = FirstPosition[ vertices, v ][[ 1 ]];
    distFromVertex = distMatrix[[ vertexIdx ]];
    candidates = Select[
      Subsets[ Range @ Length[ vertices ], { 2 } ],
      distMatrix[[ #[[ 1 ]], #[[ 2 ]] ]] == distFromVertex[[ #[[ 1 ]] ]] + distFromVertex[[ #[[ 2 ]] ]] &
    ];
    If[ candidates === {}, Return[ {} ] ];
    maxLen = Max[ distFromVertex[[ #[[ 1 ]] ]] + distFromVertex[[ #[[ 2 ]] ]] & /@ candidates ];
    pairs = Select[ candidates, distFromVertex[[ #[[ 1 ]] ]] + distFromVertex[[ #[[ 2 ]] ]] == maxLen & ];
    counts = If[ n === All,
      ConstantArray[ All, Length[ pairs ] ],
      With[ { l = Length[ pairs ] },
        RandomSample @ Table[ Quotient[ n, l ] + Boole[ i <= Mod[ n, l ] ], { i, l } ]
      ]
    ];
    Flatten[
      MapThread[
        { pair, cnt } |-> If[ cnt === 0, {},
          Take[
            Flatten[
              Outer[
                { p1, p2 } |-> Join[ Reverse[ p1 ], Rest[ p2 ] ],
                FindPath[ g, v, vertices[[ pair[[ 1 ]] ]], { distFromVertex[[ pair[[ 1 ]] ]] }, All ],
                FindPath[ g, v, vertices[[ pair[[ 2 ]] ]], { distFromVertex[[ pair[[ 2 ]] ]] }, All ],
                1
              ],
              1
            ],
            UpTo[ Replace[ cnt, All -> Infinity ] ]
          ]
        ],
        { pairs, counts }
      ],
      1
    ]
  ]

orthogonalGreedy[ g_Graph, paths_List, opts_List ] :=
  Module[ { distMatrix, vertices, vertexIndex, minSeparation, thickness, distanceFunction, pick,
            pickFirst, axes, candidates, next, previousIndices, previousEndpoints, separation, closeAxes, scores },
    If[ paths === {}, Return[ {} ] ];
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIndex = AssociationThread[ vertices, Range @ Length @ vertices ];
    distanceFunction = "DistanceFunction" /. opts /. "DistanceFunction" -> "MinEndpoint";
    minSeparation = "MinSeparation" /. opts /. "MinSeparation" -> Automatic;
    minSeparation = Replace[ minSeparation, Automatic -> (Length[ First[ paths ] ] - 1) / 2 ];
    thickness = "AxisThickness" /. opts /. "AxisThickness" -> 0;
    pickFirst = ! TrueQ[ "RandomPick" /. opts /. "RandomPick" -> False ];
    pick = If[ pickFirst, First, RandomChoice ];
    axes = { pick[ paths ] };
    previousIndices = Lookup[ vertexIndex, axes[[ 1 ]] ];
    previousEndpoints = { vertexIndex[ axes[[ 1, 1 ]] ], vertexIndex[ axes[[ 1, -1 ]] ] };
    candidates = Complement[ paths, axes ];
    While[ candidates =!= {},
      scores = Switch[ distanceFunction,
        "MinEndpoint",
          ( Min[
              distMatrix[[ vertexIndex[ #[[ 1 ]] ], previousEndpoints ]],
              distMatrix[[ vertexIndex[ #[[ -1 ]] ], previousEndpoints ]]
            ] & ) /@ candidates,
        "Hausdorff",
          ( p |-> HausdorffDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates,
        "Separation",
          ( p |-> MinimalSeparationDistance[ distMatrix, Lookup[ vertexIndex, p ], previousIndices ] ) /@ candidates,
        _, Return[ axes ]
      ];
      separation = Max[ scores ];
      If[ separation < minSeparation, Break[] ];
      next = pick[ candidates[[ Flatten @ Position[ scores, separation ] ]] ];
      closeAxes = If[ thickness == 0, { next },
        Select[ candidates, HausdorffDistance[ distMatrix, Lookup[ vertexIndex, # ], Lookup[ vertexIndex, next ] ] <= thickness & ]
      ];
      axes = Join[ axes, closeAxes ];
      previousIndices = Union[ previousIndices, Flatten[ Lookup[ vertexIndex, # ] & /@ closeAxes ] ];
      previousEndpoints = Union[ previousEndpoints, Flatten[ { vertexIndex[ #[[ 1 ]] ], vertexIndex[ #[[ -1 ]] ] } & /@ closeAxes ] ];
      candidates = Complement[ candidates, closeAxes ];
    ];
    axes
  ]

Options[ FindOrthogonalAxes ] = {
  "DistanceFunction" -> "MinEndpoint",
  "MinLength" -> Automatic,
  "MinSeparation" -> Automatic,
  "AxisThickness" -> 0,
  "RandomPick" -> False
};

FindOrthogonalAxes[ g_Graph, All, opts : OptionsPattern[] ] :=
  Module[ { distMatrix, minLength, paths },
    distMatrix = GraphDistanceMatrix[ g ];
    minLength = Replace[ OptionValue[ "MinLength" ], Automatic -> Max[ distMatrix ] ];
    paths = findLongestPaths[ g, All, Max[ distMatrix ] - minLength ];
    orthogonalGreedy[ g, paths, { opts } ]
  ]

FindOrthogonalAxes[ g_Graph, UpTo[ n_Integer ], opts : OptionsPattern[] ] :=
  Take[ FindOrthogonalAxes[ g, All, opts ], UpTo[ n ] ]

FindOrthogonalAxes[ g_Graph, n_Integer : 1, opts : OptionsPattern[] ] :=
  With[ { result = FindOrthogonalAxes[ g, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]

FindOrthogonalAxes[ g_Graph, v_, All, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], v ] :=
  orthogonalGreedy[ g, findLongestGeodesicThrough[ g, v, All ], { opts } ]

FindOrthogonalAxes[ g_Graph, v_, UpTo[ n_Integer ], opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], v ] :=
  Take[ FindOrthogonalAxes[ g, v, All, opts ], UpTo[ n ] ]

FindOrthogonalAxes[ g_Graph, v_, n_Integer : 1, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], v ] :=
  With[ { result = FindOrthogonalAxes[ g, v, UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]
