Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[comparisonAngleCos]
PackageScope[comparisonSideSquared]
PackageScope[triangleEuclideanDeficit]


(* ===================== Comparison-triangle math ===================== *)

(* Cosine of the angle in M_k^2 at the vertex opposite "opposite",
   adjacent to "adj1" and "adj2".  k > 0 spherical, k = 0 Euclidean,
   k < 0 hyperbolic. *)

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


(* Square of the comparison distance d_k_bar(p, x) in M_k^2 from a vertex
   p to a probe x on the side q-r at distance t from q, given the side
   q-p length c and the cosine of the comparison angle at q.  Closed-form
   in t, c, cosAngle, k. *)

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

(* ComparisonTriangle[a, b, c, "Curvature" -> k] returns the comparison
   triangle in M_k^2 with side lengths {a, b, c} (a opposite vertex p,
   b opposite q, c opposite r).  k = 0 returns Triangle[{q, r, p}] in R^2;
   k != 0 returns InfraComparisonTriangle wrapping sides, curvature, angles. *)

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

(* CATInequalityQ[g, {p, q, r}, k : 0]: discrete-vertex check of the CAT(k)
   thinness inequality on each side at every interior geodesic-vertex
   (vertices of MetricInterval at integer t in (0, sideLength)).  Returns
   Indeterminate when k > 0 and the perimeter exceeds 2 Pi / Sqrt[k]. *)

CATInequalityQ[ g_Graph, { p_, q_, r_ }, k_ : 0 ] :=
  Module[
    { a = GraphDistance[ g, q, r ], b = GraphDistance[ g, p, r ], c = GraphDistance[ g, p, q ],
      sideOK },
    If[ k > 0 && a + b + c >= 2 Pi / Sqrt[ k ], Return[ Indeterminate ] ];
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
    sideOK[ p, q, r, a, c, b ] && sideOK[ q, p, r, b, c, a ] && sideOK[ r, p, q, c, b, a ]
  ]


(* ===================== InfraCurvature ===================== *)

(* triangleEuclideanDeficit[g, {p, q, r}] returns the worst Euclidean
   thinness deficit Max[d(p, x)^2 - d_0_bar(t)^2] over all interior
   probes x on every side.  -Infinity if no interior probes (degenerate
   triangle).  Sign convention: positive deficit -> graph triangle is
   "fatter" than its Euclidean comparison (locally positive curvature);
   negative deficit -> "thinner" (locally negative curvature); zero ->
   Euclidean.  Closed-form, no FindRoot. *)

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


(* InfraCurvature[g, v, "Radius" -> L] returns the worst Euclidean
   deficit over all triangles whose three vertices sit inside B_L(v),
   divided by L^2 (dimensionless, scale-comparable across graphs).
   Sign positive -> the L-ball contains a triangle fatter than Euclidean
   (locally positive curvature); negative -> the ball is uniformly
   thinner; -Infinity -> no non-degenerate triangle in the ball. *)

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
