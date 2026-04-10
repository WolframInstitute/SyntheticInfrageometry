(* ::Package:: *)

FindMetricBasis::usage = "FindMetricBasis[graph, n, m] finds up to n metric bases. m specifies which basis sizes to try: All (default), an integer (max size), {min, max}, or {exact}.";
MetricBasisQ::usage = "MetricBasisQ[graph, basis] tests whether basis is a metric basis of graph.";
MetricCoordinates::usage = "MetricCoordinates[graph, vertex, basis] gives the distance coordinates of vertex with respect to basis.";
MetricBisector::usage = "MetricBisector[graph, {a, b}] returns the vertices equidistant from a and b.";

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
