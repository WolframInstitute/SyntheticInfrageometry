Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[comparisonAngleCos]
PackageScope[comparisonSideSquared]
PackageScope[triangleEuclideanDeficit]


(* ===================== Comparison-triangle math ===================== *)

(* cosine of the angle in M_k^2 at the vertex opposite "opposite"; k > 0 spherical, k = 0 Euclidean, k < 0 hyperbolic *)

comparisonAngleCos[ opposite_, adj1_, adj2_, k_ ] :=
  Which[
    k == 0,
      ( adj1^2 + adj2^2 - opposite^2 ) / ( 2 adj1 adj2 ),
    k > 0,
      With[ { s = Sqrt[ k ] },
        ( Cos[ opposite s ] - Cos[ adj1 s ] Cos[ adj2 s ] ) /
        ( Sin[ adj1 s ] Sin[ adj2 s ] )
      ],
    k < 0,
      With[ { s = Sqrt[ -k ] },
        ( Cosh[ adj1 s ] Cosh[ adj2 s ] - Cosh[ opposite s ] ) /
        ( Sinh[ adj1 s ] Sinh[ adj2 s ] )
      ]
  ]


(* squared comparison distance in M_k^2 from p to a probe at distance t along q-r, closed-form in t, c, cosAngle, k *)

comparisonSideSquared[ c_, t_, cosAngle_, k_ ] :=
  Which[
    k == 0,
      t^2 + c^2 - 2 t c cosAngle,
    k > 0,
      With[ { s = Sqrt[ k ] },
        ( ArcCos[ Cos[ t s ] Cos[ c s ] + Sin[ t s ] Sin[ c s ] cosAngle ] / s )^2
      ],
    k < 0,
      With[ { s = Sqrt[ -k ] },
        ( ArcCosh[ Cosh[ t s ] Cosh[ c s ] - Sinh[ t s ] Sinh[ c s ] cosAngle ] / s )^2
      ]
  ]


(* ===================== ComparisonTriangle ===================== *)


Options[ ComparisonTriangle ] = { "Curvature" -> 0 };

ComparisonTriangle[ a_?Positive, b_?Positive, c_?Positive, OptionsPattern[] ] :=
  Module[ { k = OptionValue[ "Curvature" ], xp, yp },
    If[ k == 0,
      xp = ( a^2 + c^2 - b^2 ) / ( 2 a );
      yp = Sqrt[ c^2 - xp^2 ];
      Triangle[ { { 0, 0 }, { a, 0 }, { xp, yp } } ],
      InfraComparisonTriangle[ <|
        "Sides"     -> { a, b, c },
        "Curvature" -> k,
        "Angles"    -> ArcCos /@ {
          comparisonAngleCos[ a, b, c, k ],
          comparisonAngleCos[ b, a, c, k ],
          comparisonAngleCos[ c, a, b, k ]
        }
      |> ]
    ]
  ]

ComparisonTriangle[ g_Graph, p_, q_, r_, opts : OptionsPattern[] ] :=
  ComparisonTriangle[
    GraphDistance[ g, q, r ],
    GraphDistance[ g, p, r ],
    GraphDistance[ g, p, q ],
    opts
  ]


InfraComparisonTriangle[ data_Association ][ key_String ] := data[ key ]


(* ===================== CATInequalityQ ===================== *)

(* "ApexSide" tests d(apex, x)^2 <= d_k_bar(apex', x')^2 over the interior probes x of the opposite side, "TwoRays" every cross-ray pair.
   Equivalent in a length space (Bridson-Haefliger II.1.7), inequivalent on a graph, where the sides are vertex sets rather than arcs.
   Indeterminate when k > 0 and the perimeter exceeds 2 Pi / Sqrt[k]. *)

Options[ CATInequalityQ ] = { Method -> "ApexSide" };

CATInequalityQ[ g_Graph, { p_, q_, r_ }, Optional[ k_?NumericQ, 0 ], OptionsPattern[] ] :=
  Module[
    { a = GraphDistance[ g, q, r ], b = GraphDistance[ g, p, r ], c = GraphDistance[ g, p, q ],
      sideOK, apexOK },
    If[ k > 0 && a + b + c >= 2 Pi / Sqrt[ k ], Return[ Indeterminate ] ];
    Switch[ OptionValue[ Method ],
      "ApexSide",
        sideOK[ apex_, end1_, end2_, oppLen_, sideQp_, sideQr_ ] :=
          With[ { cosBeta = comparisonAngleCos[ sideQr, oppLen, sideQp, k ] },
            AllTrue[ MetricInterval[ g, end1, end2 ],
              With[ { x = #, t = GraphDistance[ g, end1, # ] },
                t === 0 || t === oppLen ||
                GraphDistance[ g, apex, x ]^2 <=
                  comparisonSideSquared[ sideQp, t, cosBeta, k ]
              ] &
            ]
          ];
        sideOK[ p, q, r, a, c, b ] && sideOK[ q, p, r, b, c, a ] && sideOK[ r, p, q, c, b, a ],
      "TwoRays",
        apexOK[ apex_, end1_, end2_, lenAE1_, lenAE2_, oppLen_ ] :=
          With[ { cosApex = comparisonAngleCos[ oppLen, lenAE1, lenAE2, k ] },
            AllTrue[
              Tuples[ { MetricInterval[ g, apex, end1 ], MetricInterval[ g, apex, end2 ] } ],
              With[
                { x = #[[ 1 ]], y = #[[ 2 ]],
                  s = GraphDistance[ g, apex, #[[ 1 ]] ],
                  t = GraphDistance[ g, apex, #[[ 2 ]] ] },
                s === 0 || t === 0 ||
                GraphDistance[ g, x, y ]^2 <=
                  comparisonSideSquared[ s, t, cosApex, k ]
              ] &
            ]
          ];
        apexOK[ p, q, r, c, b, a ] && apexOK[ q, p, r, c, a, b ] && apexOK[ r, p, q, b, a, c ]
    ]
  ]


(* ===================== InfraCurvature ===================== *)

(* worst Euclidean thinness deficit Max[d(p, x)^2 - d_0_bar(t)^2] over the interior probes; positive means fatter than Euclidean, negative thinner *)

triangleEuclideanDeficit[ g_Graph, { p_, q_, r_ } ] :=
  With[
    { a = GraphDistance[ g, q, r ], b = GraphDistance[ g, p, r ], c = GraphDistance[ g, p, q ] },
    Max @ Flatten @ {
      sideDeficit[ g, p, q, r, a, c, b ],
      sideDeficit[ g, q, p, r, b, c, a ],
      sideDeficit[ g, r, p, q, c, b, a ]
    }
  ]

sideDeficit[ g_, apex_, end1_, end2_, oppLen_, sideQp_, sideQr_ ] :=
  With[ { cosBeta = ( oppLen^2 + sideQp^2 - sideQr^2 ) / ( 2 oppLen sideQp ) },
    Map[
      With[ { x = #, t = GraphDistance[ g, end1, # ] },
        If[ 0 < t < oppLen,
          GraphDistance[ g, apex, x ]^2 - ( t^2 + sideQp^2 - 2 t sideQp cosBeta ),
          -Infinity
        ]
      ] &,
      MetricInterval[ g, end1, end2 ]
    ]
  ]


(* the worst Euclidean deficit over the triangles inside B_L(v), divided by L^2 so it is scale-comparable across graphs *)

Options[ InfraCurvature ] = { "Radius" -> Automatic };

InfraCurvature[ g_Graph, v_, OptionsPattern[] ] :=
  Module[
    { L = Replace[ OptionValue[ "Radius" ], Automatic :> GraphDiameter[ g ] ],
      ball, triangles, deficits, worst },
    ball = Select[ VertexList[ g ], GraphDistance[ g, v, # ] <= L & ];
    triangles = Select[ Subsets[ ball, { 3 } ],
      With[
        { d12 = GraphDistance[ g, #[[1]], #[[2]] ],
          d13 = GraphDistance[ g, #[[1]], #[[3]] ],
          d23 = GraphDistance[ g, #[[2]], #[[3]] ] },
        d12 + d13 > d23 && d12 + d23 > d13 && d13 + d23 > d12
      ] &
    ];
    deficits = triangleEuclideanDeficit[ g, # ] & /@ triangles;
    worst = If[ deficits === { }, -Infinity, Max[ deficits ] ];
    worst / L^2
  ]

InfraCurvature[ g_Graph, opts : OptionsPattern[] ] :=
  AssociationMap[ InfraCurvature[ g, #, opts ] &, VertexList[ g ] ]
