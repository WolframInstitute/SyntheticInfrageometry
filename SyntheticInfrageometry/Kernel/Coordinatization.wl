Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[LaminarLayers]
PackageScope[laminarLayersFromSources]
PackageScope[axisLayerIndex]
PackageScope[findLongestPaths]
PackageScope[findLongestGeodesicThrough]


(* A spatial radar basis (resolving set) is a vertex set B such that the
   distance vector v |-> (d(v, b))_{b in B} is injective.  FindRadarBasis
   enumerates such bases by ascending size; m restricts the candidate
   sizes (All, integer max, {min, max}, or {exact}). *)

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

(* RadarBasisQ tests whether a vertex list b resolves the graph: the
   pointwise distance map v |-> (d(v, b1), ..., d(v, bk)) is injective. *)

RadarBasisQ[ g_Graph, b_List ] :=
  DuplicateFreeQ[ GraphDistance[ g, # ] & /@ b ]


(* RadarCoordinates of v with respect to b is its distance vector
   (d(v, b1), ..., d(v, bk)).  The two-argument form returns the
   Association of all vertices' radar coordinates. *)

RadarCoordinates[ g_Graph, v_, b_List ] :=
  GraphDistance[ g, v, # ] & /@ b

RadarCoordinates[ g_Graph, b_List ] /; !MemberQ[ VertexList[ g ], b ] :=
  Association[ # -> RadarCoordinates[ g, #, b ] & /@ VertexList[ g ] ]


(* ===================== Laminar layers ===================== *)

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


(* axisLayerIndex projects v onto an axis (a line or DAG) and returns the
   *list* of all 0-based positions/layers tied at the minimum graph
   distance.  The caller (OrthogonalCoordinates) reduces this to a scalar
   via the "Aggregation" option. *)

axisLayerIndex[ g_Graph, axis_List, v_ ] :=
  Module[ { dists, minD },
    dists = GraphDistance[ g, v, # ] & /@ axis;
    minD = Min[ dists ];
    Flatten[ Position[ dists, minD ] ] - 1
  ]

axisLayerIndex[ g_Graph, dag_Graph, v_ ] :=
  Module[ { layers, verts, dists, minD, proj },
    layers = LaminarLayers[ g, dag ];
    verts = VertexList[ dag ];
    dists = GraphDistance[ g, v, # ] & /@ verts;
    minD = Min[ dists ];
    proj = Pick[ verts, dists, minD ];
    Flatten @ Table[ Position[ layers, u ][[ All, 1 ]] - 1, { u, proj } ]
  ]


(* ===================== OrthogonalCoordinates ===================== *)

(* OrthogonalCoordinates projects each vertex onto each axis (a line or
   DAG) by shortest-path distance and returns the tuple of layer indices.
   With an "Origin" or a center vertex c the layers are signed - Z-valued
   displacements with c at {0, ..., 0}.  When the projection is
   multi-valued (ties), the "Aggregation" option chooses how to reduce
   the layer indices: "First" (default; earliest position), "Last",
   "Min", "Max", "Mean", or "Median".

   InfraPoint center: an InfraPoint[{v1, ..., vk}] center asks for axes
   that pass through at least one of {v1, ..., vk} as an interior point;
   each axis is signed relative to the first vi (in InfraPoint order)
   that lies on it. *)

aggregateLayer[ "First",  ix_List ] := First @ ix
aggregateLayer[ "Last",   ix_List ] := Last @ ix
aggregateLayer[ "Min",    ix_List ] := Min @ ix
aggregateLayer[ "Max",    ix_List ] := Max @ ix
aggregateLayer[ "Mean",   ix_List ] := Mean @ ix
aggregateLayer[ "Median", ix_List ] := Median @ ix

orthogonalCoordsCore[ g_Graph, axes_List, v_, anchors_, agg_String ] :=
  With[ { vIdx = aggregateLayer[ agg, axisLayerIndex[ g, #, v ] ] & /@ axes },
    If[ anchors === None,
      vIdx,
      vIdx - MapThread[ aggregateLayer[ agg, axisLayerIndex[ g, #1, #2 ] ] &, { axes, anchors } ]
    ]
  ]

perAxisAnchor[ axis_List,  vs_List ] := SelectFirst[ vs, MemberQ[ axis, # ] &,                  First @ vs ]
perAxisAnchor[ axis_Graph, vs_List ] := SelectFirst[ vs, MemberQ[ VertexList[ axis ], # ] &,    First @ vs ]


Options[ OrthogonalCoordinates ] = { "Origin" -> None, "Aggregation" -> "First" };

(* Explicit axes, single vertex *)
OrthogonalCoordinates[ g_Graph, axes_List, v_, opts : OptionsPattern[] ] /; !MemberQ[ VertexList[ g ], axes ] :=
  With[ { origin = OptionValue[ "Origin" ], agg = OptionValue[ "Aggregation" ] },
    orthogonalCoordsCore[ g, axes, v,
      If[ origin === None, None, ConstantArray[ origin, Length[ axes ] ] ],
      agg
    ]
  ]

(* Explicit axes, all vertices *)
OrthogonalCoordinates[ g_Graph, axes_List, opts : OptionsPattern[] ] /; !MemberQ[ VertexList[ g ], axes ] :=
  Association[ # -> OrthogonalCoordinates[ g, axes, #, opts ] & /@ VertexList[ g ] ]

(* Center vertex c, single vertex *)
OrthogonalCoordinates[ g_Graph, c_, v_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] && MemberQ[ VertexList[ g ], v ] :=
  OrthogonalCoordinates[ g, FindOrthogonalAxes[ g, c, All ], v, "Origin" -> c, opts ]

(* Center vertex c, all vertices *)
OrthogonalCoordinates[ g_Graph, c_, opts : OptionsPattern[] ] /; MemberQ[ VertexList[ g ], c ] :=
  With[ { axes = FindOrthogonalAxes[ g, c, All ] },
    Association[ # -> OrthogonalCoordinates[ g, axes, #, "Origin" -> c, opts ] & /@ VertexList[ g ] ]
  ]

(* InfraPoint singleton: degenerates to single-vertex center *)
OrthogonalCoordinates[ g_Graph, InfraPoint[ { v_ } ], rest___ ] :=
  OrthogonalCoordinates[ g, v, rest ]

(* InfraPoint multi-vertex, single vertex *)
OrthogonalCoordinates[ g_Graph, InfraPoint[ vs_List ], v_, opts : OptionsPattern[] ] /;
    MemberQ[ VertexList[ g ], v ] && SubsetQ[ VertexList[ g ], vs ] :=
  With[ { axes = FindOrthogonalAxes[ g, InfraPoint[ vs ], All ],
          agg  = OptionValue[ "Aggregation" ] },
    orthogonalCoordsCore[ g, axes, v, perAxisAnchor[ #, vs ] & /@ axes, agg ]
  ]

(* InfraPoint multi-vertex, all vertices *)
OrthogonalCoordinates[ g_Graph, InfraPoint[ vs_List ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { axes = FindOrthogonalAxes[ g, InfraPoint[ vs ], All ],
          agg  = OptionValue[ "Aggregation" ] },
    With[ { anchors = perAxisAnchor[ #, vs ] & /@ axes },
      Association[ # -> orthogonalCoordsCore[ g, axes, #, anchors, agg ] & /@ VertexList[ g ] ]
    ]
  ]


(* ===================== Orthogonal axes ===================== *)

(* FindOrthogonalAxes selects a maximal mutually-separated set of longest
   geodesics (or longest geodesics through a chosen center).
   "Orthogonality" is operationalised as a Hausdorff / endpoint
   separation between axes; greedy selection.  When constrained to pass
   through c as an interior point, each axis fixes a sign convention for
   the corresponding signed OrthogonalCoordinates layer.  An InfraPoint
   center accepts a vertex set; each returned axis must contain at least
   one of those vertices as an interior point. *)

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

findLongestGeodesicThrough[ g_Graph, v_, n_ ] /; MemberQ[ VertexList[ g ], v ] :=
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

findLongestGeodesicThrough[ g_Graph, anchors_List, n_ ] /; AllTrue[ anchors, MemberQ[ VertexList[ g ], # ] & ] :=
  Module[ { paths, maxLen },
    paths = DeleteDuplicates[ Flatten[ findLongestGeodesicThrough[ g, #, All ] & /@ anchors, 1 ] ];
    If[ paths === {}, Return[ {} ] ];
    maxLen = Max[ Length /@ paths ];
    paths = Select[ paths, Length[ # ] == maxLen & ];
    If[ n === All, paths,
      With[ { l = Length[ paths ] },
        If[ n >= l, paths, RandomSample[ paths, n ] ]
      ]
    ]
  ]


orthogonalGreedy[ g_Graph, paths_List, opts_List ] :=
  Module[ { distMatrix, vertices, vertexIndex, minSeparation, thickness, distanceFunction, pick,
            pickFirst, axes, candidates, next, previousIndices, previousEndpoints, separation, closeAxes, scores },
    If[ paths === {}, Return[ {} ] ];
    vertices = VertexList[ g ];
    distMatrix = GraphDistanceMatrix[ g ];
    vertexIndex = AssociationThread[ vertices, Range @ Length @ vertices ];
    distanceFunction = "AxisDistance" /. opts /. "AxisDistance" -> "MinEndpoint";
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
  "AxisDistance" -> "MinEndpoint",
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

(* InfraPoint singleton: degenerates to single-vertex center *)
FindOrthogonalAxes[ g_Graph, InfraPoint[ { v_ } ], rest___ ] :=
  FindOrthogonalAxes[ g, v, rest ]

FindOrthogonalAxes[ g_Graph, InfraPoint[ vs_List ], All, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  orthogonalGreedy[ g, findLongestGeodesicThrough[ g, vs, All ], { opts } ]

FindOrthogonalAxes[ g_Graph, InfraPoint[ vs_List ], UpTo[ n_Integer ], opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  Take[ FindOrthogonalAxes[ g, InfraPoint[ vs ], All, opts ], UpTo[ n ] ]

FindOrthogonalAxes[ g_Graph, InfraPoint[ vs_List ], n_Integer : 1, opts : OptionsPattern[] ] /; SubsetQ[ VertexList[ g ], vs ] :=
  With[ { result = FindOrthogonalAxes[ g, InfraPoint[ vs ], UpTo[ n ], opts ] },
    If[ Length[ result ] < n, $Failed, result ]
  ]
