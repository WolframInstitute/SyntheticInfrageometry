Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findQuadricCore]


(* ===================== FindInfraQuadric ===================== *)

(* Distance-quadric region.  For foci {p1, ..., pk} and weights {w1, ..., wk},
   the signed-distance sum is S(v) = sum_i w_i d(p_i, v).  Scalar c selects
   the solid half-space { v : S(v) <= c }; pair c = {cMin, cMax} selects the
   band { v : cMin <= S(v) <= cMax }.  Default weights = all 1s (Euclidean
   ellipsoid interior); {1, -1} gives a one-branch hyperboloid via the scalar
   form, or a two-sided hyperboloid via a symmetric band {-c, c}.  Each focus
   may be a bare vertex or an InfraPoint wrapper; multi-realisation
   InfraPoint foci are reduced to their first realisation. *)

FindInfraQuadric[ graph_Graph, foci_List, c_ ] :=
  FindInfraQuadric[ graph, foci, c, ConstantArray[ 1, Length @ foci ] ]

FindInfraQuadric[ graph_Graph, foci_List, c_, weights_List ] :=
  InfraObject @ findQuadricCore[
    graph,
    Replace[ foci, InfraPoint[ { v_, ___ } ] :> v, { 1 } ],
    c, weights ]


findQuadricCore[ graph_Graph, foci_List, c_, weights_List ] :=
  With[
    { dm   = GraphDistanceMatrix @ graph,
      idxs = VertexIndex[ graph, # ] & /@ foci,
      vs   = VertexList @ graph },
    With[ { sums = Total @ MapThread[ #1 * dm[[ #2 ]] &, { weights, idxs } ] },
      Pick[ vs,
        Replace[ c,
          { { cMin_, cMax_ } :> Thread[ cMin <= sums <= cMax ],
            c0_ :> Thread[ sums <= c0 ] } ] ]
    ]
  ]
