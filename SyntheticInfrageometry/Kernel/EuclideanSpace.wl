Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findInfraScale]
PackageScope[findInfraSum]


(* ===================== InfraScalarProduct ===================== *)

(* Base-point-relative inner product of u, v wrt o.
   "Alexandrov" (default; equivalent to {"Alexandrov", "Curvature" -> 0}):
       <u, v>_o = d(o, u) d(o, v) cos theta_k,  where theta_k is the comparison
       angle at o in M_k^2.  k = 0 collapses to (d(o,u)^2 + d(o,v)^2 - d(u,v)^2) / 2
       (the polar form of the squared metric); k != 0 uses the spherical /
       hyperbolic law of cosines via comparisonAngleCos.
   "Parallelogram": polarisation <u, v>_o = (||u + v||_o^2 - ||u - v||_o^2) / 4
       over realisations of u + v and u - v on the substrate (multi-valued). *)

InfraScalarProduct::badmethod = "Method `1` is not supported by InfraScalarProduct.";

Options[ InfraScalarProduct ] = { Method -> "Alexandrov" };

InfraScalarProduct[ graph_Graph, o_, u_, v_, OptionsPattern[] ] :=
  Switch[ methodName @ OptionValue[ Method ],
    "Alexandrov",
      With[ { k = Lookup[ methodOptions @ OptionValue[ Method ], "Curvature", 0 ],
              d = { a, b } |-> GraphDistance[ graph, a, b ] },
        If[ k === 0,
          ( d[ o, u ]^2 + d[ o, v ]^2 - d[ u, v ]^2 ) / 2,
          d[ o, u ] d[ o, v ] comparisonAngleCos[ d[ u, v ], d[ o, u ], d[ o, v ], k ]
        ]
      ],
    "Parallelogram",
      With[ { plus  = #[ "Vertex" ] & /@ FindInfraLinearCombination[
                graph, o, { { 1, u }, {  1, v } }, All, "ScaleMethod" -> "Line" ],
              minus = #[ "Vertex" ] & /@ FindInfraLinearCombination[
                graph, o, { { 1, u }, { -1, v } }, All, "ScaleMethod" -> "Line" ] },
        If[ plus === { } || minus === { }, $Failed,
          With[ { vals = DeleteDuplicates @ Flatten @ Outer[
                ( GraphDistance[ graph, o, #1 ]^2 - GraphDistance[ graph, o, #2 ]^2 ) / 4 &,
                plus, minus ] },
            If[ Length[ vals ] == 1, First @ vals, vals ]
          ]
        ]
      ],
    _, Message[ InfraScalarProduct::badmethod, OptionValue[ Method ] ]; $Failed
  ]


(* ===================== FindInfraLinearCombination ===================== *)

(* Multi-valued vertex realisation of Sum_i lambda_i u_i from base point o.
   Each scaled term lambda_i u_i is computed via findInfraScale; partial sums
   are composed pairwise via findInfraSum, left-to-right. *)

Options[ FindInfraLinearCombination ] = {
  "ScaleMethod" -> Automatic,
  "SumMethod"   -> "Metric"
};

FindInfraLinearCombination[ graph_Graph, o_, terms_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[ { lambdas = terms[[ All, 1 ]], us = terms[[ All, 2 ]],
          scaleM = OptionValue[ "ScaleMethod" ], sumM = OptionValue[ "SumMethod" ] },
    spreadFind[ InfraPoint, count,
      Function[ Null,
        With[ { thisO = #1, thisUs = { ##2 } },
          { scaled = MapThread[ findInfraScale[ graph, thisO, #2, #1, scaleM ] &,
                                { lambdas, thisUs } ] },
          If[ Length[ scaled ] == 0,
            { thisO },
            Fold[
              { acc, next } |->
                DeleteDuplicates @ Flatten @ Outer[
                  findInfraSum[ graph, thisO, #1, #2, sumM ] &, acc, next, 1 ],
              First @ scaled, Rest @ scaled
            ]
          ]
        ]
      ],
      o, Sequence @@ us
    ]
  ]


(* ===================== InfraAngle ===================== *)

(* An angle at p between q1, q2.
   "Arclength" (default): the open ball B(p, min(d(p, q1), d(p, q2))) is removed
   and d(q1, q2) in the remaining graph is normalised by the radius -- a
   synthetic radian measure of the detour around p.
   "Alexandrov": Alexandrov comparison-triangle angle in model space M_k^2
   (k = 0 by default; "Curvature" -> k inside the Method list overrides).
   The un-arccos'd scalar product (d(p,q1)^2 + d(p,q2)^2 - d(q1,q2)^2)/2 is
   not an angle -- use InfraScalarProduct[g, p, q1, q2] for that. *)

InfraAngle::badmethod = "Method `1` is not supported by InfraAngle.";

Options[ InfraAngle ] = { Method -> "Arclength" };

(* Accept InfraPoint atoms anywhere in the triple -- paclet-wide
   convention that wrapped and bare-vertex forms are interchangeable. *)
InfraAngle[ graph_Graph, triple : { _, _, _ }, opts : OptionsPattern[] ] /;
    ! FreeQ[ triple, _InfraPoint ] :=
  InfraAngle[ graph, triple /. InfraPoint[ v_ ] :> v, opts ]

InfraAngle[ graph_Graph, { q1_, p_, q2_ }, OptionsPattern[] ] :=
  Switch[ methodName @ OptionValue[ Method ],
    "Arclength",
      With[ { radius = Min[ GraphDistance[ graph, p, q1 ], GraphDistance[ graph, p, q2 ] ] },
        { rem = VertexDelete[ graph,
            Select[ VertexList[ graph ], GraphDistance[ graph, p, # ] < radius & ] ] },
        GraphDistance[ rem, q1, q2 ] / radius
      ],
    "Alexandrov",
      With[ { k = Lookup[ methodOptions @ OptionValue[ Method ], "Curvature", 0 ] },
        ArcCos @ comparisonAngleCos[
          GraphDistance[ graph, q1, q2 ],
          GraphDistance[ graph, p, q1 ],
          GraphDistance[ graph, p, q2 ],
          k ]
      ],
    _, Message[ InfraAngle::badmethod, OptionValue[ Method ] ]; $Failed
  ]


(* ===================== Helpers: findInfraScale ===================== *)

(* Scalar multiplication: realise lambda * u from origin o on the graph.
   "Metric"   - strict integer match: collinear with o, u via distance additivity
                at distance |lambda| r.
   "Line"     - realisations of FindInfraLine, snap by index to lambda (uIdx - oIdx).
   "Midpoint" - dyadic bisection via strict equidistant midpoints; lambda in [0, 1].
   Automatic  - integer lambda -> Metric;  dyadic in [0,1] -> Midpoint;  else -> Line. *)

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

findInfraScale[ graph_Graph, o_, u_, 0, "Metric" ] := { o }
findInfraScale[ graph_Graph, o_, u_, 1, "Metric" ] := { u }
findInfraScale[ graph_Graph, o_, u_, lambda_, "Metric" ] :=
  With[ { r = GraphDistance[ graph, o, u ] },
    If[ r === Infinity, { },
      With[ { target = Abs[ lambda ] r },
        DeleteDuplicates @ Select[ VertexList[ graph ],
          GraphDistance[ graph, o, # ] == target &&
          If[ lambda > 0,
            GraphDistance[ graph, o, # ] + GraphDistance[ graph, #, u ] == r ||
              r + GraphDistance[ graph, u, # ] == GraphDistance[ graph, o, # ],
            GraphDistance[ graph, #, o ] + r == GraphDistance[ graph, #, u ]
          ] & ]
      ]
    ]
  ]

findInfraScale[ graph_Graph, o_, u_, 0, "Line" ] := { o }
findInfraScale[ graph_Graph, o_, u_, 1, "Line" ] := { u }
findInfraScale[ graph_Graph, o_, u_, lambda_, "Line" ] :=
  With[ { r = GraphDistance[ graph, o, u ] },
    If[ r === Infinity, { },
      With[ { lines = FindInfraLine[ graph, o, u, All ][ "Realizations" ] },
        DeleteDuplicates @ Flatten @ ( ( line |->
          With[ { oIdx = First @ FirstPosition[ line, o, { 0 } ],
                  uIdx = First @ FirstPosition[ line, u, { 0 } ] },
            { targetIdx = oIdx + Round[ lambda ( uIdx - oIdx ) ] },
            If[ oIdx > 0 && uIdx > 0 && 1 <= targetIdx <= Length[ line ],
              { line[[ targetIdx ]] }, { } ]
          ] ) /@ lines )
      ]
    ]
  ]

findInfraScale[ graph_Graph, o_, u_, 0, "Midpoint" ] := { o }
findInfraScale[ graph_Graph, o_, u_, 1, "Midpoint" ] := { u }
findInfraScale[ graph_Graph, o_, u_, lambda_, "Midpoint" ] /; 0 < lambda < 1 :=
  Module[ { asList = { o }, bsList = { u }, mids,
            rational = Rationalize[ lambda, 0 ] },
    With[ { depth = If[ Element[ rational, Rationals ] && IntegerQ[ Log2 @ Denominator[ rational ] ],
                       Log2 @ Denominator[ rational ], 8 ] },
      With[ { bits = IntegerDigits[ Round[ lambda 2^depth ], 2, depth ],
              midpointer = { a, b } |->
                With[ { r = GraphDistance[ graph, a, b ] },
                  If[ EvenQ[ r ],
                    Select[ VertexList[ graph ],
                      GraphDistance[ graph, a, # ] == r/2 &&
                      GraphDistance[ graph, b, # ] == r/2 & ],
                    { } ] ] },
        Do[
          mids = DeleteDuplicates @ Flatten @ Outer[ midpointer, asList, bsList, 1 ];
          If[ mids === { }, Break[ ] ];
          If[ bit == 0, bsList = mids, asList = mids ],
          { bit, bits } ]
      ];
      asList
    ]
  ]


(* ===================== Helpers: findInfraSum ===================== *)

(* Vector addition (parallelogram closure): realise u + v from origin o.
   "Metric"   - { w : d(u, w) = d(o, v) and d(v, w) = d(o, u), w != o }.
   "Parallel" - intersection of parallels through u (parallel to line o, v) with
                parallels through v (parallel to line o, u), minus {o, u, v}. *)

findInfraSum[ graph_Graph, o_, u_, v_, "Metric" ] :=
  With[ { rU = GraphDistance[ graph, o, u ], rV = GraphDistance[ graph, o, v ] },
    DeleteDuplicates @ Select[ VertexList[ graph ],
      GraphDistance[ graph, u, # ] == rV &&
      GraphDistance[ graph, v, # ] == rU &&
      # =!= o & ]
  ]

findInfraSum[ graph_Graph, o_, u_, v_, "Parallel" ] :=
  With[ { linesOV = FindInfraLine[ graph, o, v, All ][ "Realizations" ],
          linesOU = FindInfraLine[ graph, o, u, All ][ "Realizations" ] },
    { parallelsAtU = Flatten[
        ( FindInfraParallel[ graph, #, u, All ][ "Realizations" ] ) & /@ linesOV, 1 ],
      parallelsAtV = Flatten[
        ( FindInfraParallel[ graph, #, v, All ][ "Realizations" ] ) & /@ linesOU, 1 ] },
    DeleteDuplicates @ DeleteCases[
      Flatten @ Outer[ Intersection, parallelsAtU, parallelsAtV, 1 ],
      o | u | v ]
  ]
