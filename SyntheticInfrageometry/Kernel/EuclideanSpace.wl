Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findInfraScale]
PackageScope[findInfraSum]


(* ===================== Messages ===================== *)

InfraScalarProduct::badmethod = "Method `1` is not supported by InfraScalarProduct.";
InfraScalarProduct::nopara = "No parallelogram realises on the substrate; the parallelogram-law method has no values to return.";
findInfraScale::midrange = "The \"Midpoint\" scale method requires lambda in [0, 1]; got `1`.";
findInfraScale::badmethod = "Scale method `1` is not supported.";
findInfraSum::badmethod = "Sum method `1` is not supported.";


(* ===================== InfraScalarProduct ===================== *)

(* The base-point-relative inner product of u and v with respect to o.
   "Schoenberg" (default): direct distance formula
       <u, v>_o = ( d(o, u)^2 + d(o, v)^2 - d(u, v)^2 ) / 2.
   "Parallelogram": polarization identity
       <u, v>_o = ( ||u + v||_o^2 - ||u - v||_o^2 ) / 4
   computed via FindInfraLinearCombination on { {1,u}, {+/-1,v} }; multi-valued. *)

Options[ InfraScalarProduct ] = { Method -> "Schoenberg" };

InfraScalarProduct[ graph_Graph, o_, u_, v_, OptionsPattern[] ] :=
  Switch[ OptionValue[ Method ],
    "Schoenberg",
      With[ { d = { a, b } |-> GraphDistance[ graph, a, b ] },
        ( d[ o, u ]^2 + d[ o, v ]^2 - d[ u, v ]^2 ) / 2
      ],
    "Parallelogram",
      Module[ { plus, minus, vals },
        plus  = #[[ 1, 1 ]] & /@ FindInfraLinearCombination[ graph, o, { { 1, u }, {  1, v } }, All, "ScaleMethod" -> "Line" ];
        minus = #[[ 1, 1 ]] & /@ FindInfraLinearCombination[ graph, o, { { 1, u }, { -1, v } }, All, "ScaleMethod" -> "Line" ];
        If[ plus === { } || minus === { },
          Message[ InfraScalarProduct::nopara ]; $Failed,
          vals = DeleteDuplicates @ Flatten @ Outer[
            ( GraphDistance[ graph, o, #1 ]^2 - GraphDistance[ graph, o, #2 ]^2 ) / 4 &,
            plus, minus
          ];
          If[ Length[ vals ] == 1, First @ vals, vals ]
        ]
      ],
    other_, Message[ InfraScalarProduct::badmethod, other ]; $Failed
  ]


(* ===================== findInfraScale ===================== *)

(* Scalar multiplication: realise lambda * u from origin o on the graph.
   Returns a bare list of vertex realisations.
   "Metric"   - strict integer match: collinear with o,u via distance additivity, at distance |lambda| r.
   "Line"     - walk realisations of FindLine, snap by index to lambda * (uIdx - oIdx).
   "Midpoint" - dyadic bisection via strict equidistant midpoints; lambda in [0, 1].
   Automatic  - integer lambda -> Metric; dyadic in [0,1] -> Midpoint; else -> Line. *)

findInfraScale[ graph_Graph, o_, u_, lambda_, Automatic ] :=
  Which[
    IntegerQ[ lambda ],
      findInfraScale[ graph, o, u, lambda, "Metric" ],
    Element[ Rationalize[ lambda, 0 ], Rationals ] &&
      IntegerQ[ Log2 @ Denominator @ Rationalize[ lambda, 0 ] ] &&
      0 <= lambda <= 1,
      findInfraScale[ graph, o, u, lambda, "Midpoint" ],
    True,
      findInfraScale[ graph, o, u, lambda, "Line" ]
  ]

findInfraScale[ graph_Graph, o_, u_, lambda_, "Metric" ] :=
  Which[
    lambda == 0, { o },
    lambda == 1, { u },
    True,
      With[ { r = GraphDistance[ graph, o, u ] },
        With[ { target = Abs[ lambda ] r },
          If[ r === Infinity, { },
            DeleteDuplicates @ Select[ VertexList[ graph ],
              GraphDistance[ graph, o, # ] == target &&
              If[ lambda > 0,
                GraphDistance[ graph, o, # ] + GraphDistance[ graph, #, u ] == r ||
                  r + GraphDistance[ graph, u, # ] == GraphDistance[ graph, o, # ],
                GraphDistance[ graph, #, o ] + r == GraphDistance[ graph, #, u ]
              ] &
            ]
          ]
        ]
      ]
  ]

findInfraScale[ graph_Graph, o_, u_, lambda_, "Line" ] :=
  Module[ { r, lines, snap },
    Which[ lambda == 0, Return[ { o } ], lambda == 1, Return[ { u } ] ];
    r = GraphDistance[ graph, o, u ];
    If[ r === Infinity, Return[ { } ] ];
    lines = #[[ 1, 1 ]] & /@ Quiet @ FindLine[ graph, o, u, All ];
    If[ ! ListQ[ lines ] || lines === { }, Return[ { } ] ];
    snap = line |->
      With[ { oIdx = First @ FirstPosition[ line, o, { 0 } ],
              uIdx = First @ FirstPosition[ line, u, { 0 } ] },
        With[ { targetIdx = oIdx + Round[ lambda ( uIdx - oIdx ) ] },
          If[ oIdx > 0 && uIdx > 0 && 1 <= targetIdx <= Length[ line ],
            { line[[ targetIdx ]] },
            { }
          ]
        ]
      ];
    DeleteDuplicates @ Flatten @ ( snap /@ lines )
  ]

findInfraScale[ graph_Graph, o_, u_, lambda_, "Midpoint" ] :=
  Module[ { rational, depth, scaledLambda, bits, asList = { o }, bsList = { u }, midpointer, mids },
    Which[
      lambda == 0, Return[ { o } ],
      lambda == 1, Return[ { u } ],
      ! TrueQ[ 0 < lambda < 1 ],
        Message[ findInfraScale::midrange, lambda ]; Return[ { } ]
    ];
    rational = Rationalize[ lambda, 0 ];
    depth = If[ Element[ rational, Rationals ] && IntegerQ[ Log2 @ Denominator[ rational ] ],
      Log2 @ Denominator[ rational ],
      8
    ];
    scaledLambda = Round[ lambda 2^depth ];
    bits = IntegerDigits[ scaledLambda, 2, depth ];
    midpointer = { a, b } |->
      With[ { r = GraphDistance[ graph, a, b ] },
        If[ EvenQ[ r ],
          Select[ VertexList[ graph ],
            GraphDistance[ graph, a, # ] == r/2 &&
            GraphDistance[ graph, b, # ] == r/2 &
          ],
          { }
        ]
      ];
    Do[
      mids = DeleteDuplicates @ Flatten @ Outer[ midpointer, asList, bsList, 1 ];
      If[ mids === { }, Break[ ] ];
      If[ bit == 0, bsList = mids, asList = mids ],
      { bit, bits }
    ];
    asList
  ]

findInfraScale[ _Graph, _, _, _, other_ ] :=
  ( Message[ findInfraScale::badmethod, other ]; { } )


(* ===================== findInfraSum ===================== *)

(* Vector addition (parallelogram closure): realise u + v from origin o.
   Returns a bare list of vertex realisations.
   "Metric"   - { w in V : d(u, w) = d(o, v), d(v, w) = d(o, u), w != o }.
   "Parallel" - intersection of FindParallel through u (parallel to o-v line)
                with FindParallel through v (parallel to o-u line); strip {o, u, v}. *)

findInfraSum[ graph_Graph, o_, u_, v_, "Metric" ] :=
  With[ { rU = GraphDistance[ graph, o, u ], rV = GraphDistance[ graph, o, v ] },
    DeleteDuplicates @ Select[ VertexList[ graph ],
      GraphDistance[ graph, u, # ] == rV &&
      GraphDistance[ graph, v, # ] == rU &&
      # =!= o &
    ]
  ]

findInfraSum[ graph_Graph, o_, u_, v_, "Parallel" ] :=
  Module[ { linesOV, linesOU, parallelsAtU, parallelsAtV },
    linesOV = #[[ 1, 1 ]] & /@ Quiet @ FindLine[ graph, o, v, All ];
    linesOU = #[[ 1, 1 ]] & /@ Quiet @ FindLine[ graph, o, u, All ];
    If[ ! ListQ[ linesOV ] || ! ListQ[ linesOU ], Return[ { } ] ];
    parallelsAtU = Flatten[
      ( #[[ 1, 1 ]] & /@ Quiet @ FindParallel[ graph, #, u, All ] ) & /@ linesOV, 1
    ];
    parallelsAtV = Flatten[
      ( #[[ 1, 1 ]] & /@ Quiet @ FindParallel[ graph, #, v, All ] ) & /@ linesOU, 1
    ];
    DeleteDuplicates @ DeleteCases[
      Flatten @ Outer[ Intersection, parallelsAtU, parallelsAtV, 1 ],
      o | u | v
    ]
  ]

findInfraSum[ _Graph, _, _, _, other_ ] :=
  ( Message[ findInfraSum::badmethod, other ]; { } )


(* ===================== FindInfraLinearCombination ===================== *)

(* Multi-valued vertex realisation of sum_i lambda_i u_i from base point o.
   Each scaled term lambda_i u_i is computed via findInfraScale; partial sums
   are composed pairwise via findInfraSum, left-to-right. *)

Options[ FindInfraLinearCombination ] = {
  "ScaleMethod" -> Automatic,
  "SumMethod"   -> "Metric"
};

FindInfraLinearCombination[ graph_Graph, o_, terms_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { lambdas = terms[[ All, 1 ]], us = terms[[ All, 2 ]],
          scaleM = OptionValue[ "ScaleMethod" ], sumM = OptionValue[ "SumMethod" ] },
    infraSpreadAndCartesian[ InfraPoint, count,
      Function[ Null,
        With[ { thisO = #1, thisUs = { ##2 } },
          With[ { scaled = MapThread[ findInfraScale[ graph, thisO, #2, #1, scaleM ] &,
                                       { lambdas, thisUs } ] },
            If[ Length[ scaled ] == 0,
              { thisO },
              Fold[
                Function[ { acc, next },
                  DeleteDuplicates @ Flatten @ Outer[
                    findInfraSum[ graph, thisO, #1, #2, sumM ] &, acc, next, 1
                  ]
                ],
                First @ scaled, Rest @ scaled
              ]
            ]
          ]
        ]
      ],
      o, Sequence @@ us
    ]
  ]


(* ===================== InfraAngle ===================== *)

(* InfraAngle[graph, {q1, p, q2}, Method -> ...] computes an angle at p.

   Method -> "PunchOut" (default).  Removes the open ball around p of
   radius Min[d(p, q1), d(p, q2)], then measures how far q1 and q2 are
   forced to travel outside that neighborhood, normalized by the radius.

   Method -> "Comparison".  The Alexandrov comparison-triangle angle at p
   in the Euclidean (k = 0) model: cos a = (d(p, q1)^2 + d(p, q2)^2 -
   d(q1, q2)^2) / (2 d(p, q1) d(p, q2)).

   Method -> {"Comparison", "Curvature" -> k}.  The same comparison angle
   in the model space M_k^2 (spherical for k > 0, Euclidean for k = 0,
   hyperbolic for k < 0) using the corresponding law of cosines. *)

Options[ InfraAngle ] = { Method -> "PunchOut" };

InfraAngle[ graph_Graph, { q1_, p_, q2_ }, OptionsPattern[] ] :=
  Switch[ OptionValue[ Method ],
    "PunchOut",
      Module[ { radius, rem },
        radius = Min[ GraphDistance[ graph, p, q1 ], GraphDistance[ graph, p, q2 ] ];
        rem = VertexDelete[ graph,
          Select[ VertexList[ graph ], GraphDistance[ graph, p, # ] < radius & ]
        ];
        GraphDistance[ rem, q1, q2 ] / radius
      ],
    "Comparison",
      ArcCos @ comparisonAngleCos[
        GraphDistance[ graph, q1, q2 ],
        GraphDistance[ graph, p, q1 ],
        GraphDistance[ graph, p, q2 ],
        0
      ],
    { "Comparison", ___ },
      With[ { k = "Curvature" /. Rest @ OptionValue[ Method ] /. "Curvature" -> 0 },
        ArcCos @ comparisonAngleCos[
          GraphDistance[ graph, q1, q2 ],
          GraphDistance[ graph, p, q1 ],
          GraphDistance[ graph, p, q2 ],
          k
        ]
      ],
    _, $Failed
  ]
