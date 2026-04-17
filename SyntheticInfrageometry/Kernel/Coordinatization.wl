Package["WolframInstitute`SyntheticInfrageometry`"]

FindMetricBasis[ g_, n_ : 1, m_ : All ] :=
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

MetricBasisQ[ g_Graph, b_List ] :=
  DuplicateFreeQ[ GraphDistance[ g, # ] & /@ b ]

MetricCoordinates[ g_Graph, v_, b_List ] :=
  GraphDistance[ g, v, # ] & /@ b

MetricBisector[ g_Graph, { a_, b_ } ] :=
  Select[ VertexList[ g ], GraphDistance[ g, a, # ] == GraphDistance[ g, b, # ] & ]

MetricBisector[ g_Graph, a_, b_ ] :=
  MetricBisector[ g, { a, b } ]


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


FindLineProjection[ g_Graph, line_List, v_ ] :=
  Module[ { dists, minD },
    dists = GraphDistance[ g, v, # ] & /@ line;
    minD = Min[ dists ];
    Pick[ line, dists, minD ]
  ]


FindDAGProjection[ g_Graph, dag_Graph, v_ ] :=
  Module[ { verts, dists, minD },
    verts = VertexList[ dag ];
    dists = GraphDistance[ g, v, # ] & /@ verts;
    minD = Min[ dists ];
    Pick[ verts, dists, minD ]
  ]


LaminarCoordinates[ g_Graph, line_List, v_ ] :=
  Module[ { proj, minLayer },
    proj = FindLineProjection[ g, line, v ];
    minLayer = Min[ Flatten[ FirstPosition[ line, # ] & /@ proj ] ] - 1;
    { minLayer, GraphDistance[ g, v, First[ proj ] ] }
  ]

LaminarCoordinates[ g_Graph, dag_Graph, v_ ] :=
  Module[ { layers, proj, layerIndex },
    layers = LaminarLayers[ g, dag ];
    proj = FindDAGProjection[ g, dag, v ];
    layerIndex = Min @ Flatten @ Table[
      Position[ layers, u ][[ All, 1 ]] - 1,
      { u, proj }
    ];
    { layerIndex, GraphDistance[ g, v, First[ proj ] ] }
  ]

LaminarCoordinates[ g_Graph, line_ ] :=
  Association[ # -> LaminarCoordinates[ g, line, # ] & /@ VertexList[ g ] ]
