Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindInfraQuadric ===================== *)

(* S(v) = Sum_i w_i d(p_i, v); a scalar c selects { v : S(v) <= c }, a pair the band cMin <= S(v) <= cMax *)

FindInfraQuadric[ graph_Graph, foci_List, c_ ] :=
  FindInfraQuadric[ graph, foci, c, ConstantArray[ 1, Length @ foci ] ]

FindInfraQuadric[ graph_Graph, foci_List, c_, weights_List ] :=
  InfraObject @ With[
    { foci0 = Replace[ foci, { InfraPoint[ v_ ] :> v, InfraSet[ vs_List ] :> First[ vs ] }, { 1 } ] },
    { dm   = GraphDistanceMatrix @ graph,
      idxs = VertexIndex[ graph, # ] & /@ foci0,
      vs   = VertexList @ graph },
    { sums = Total @ MapThread[ #1 * dm[[ #2 ]] &, { weights, idxs } ] },
    Pick[ vs,
      Replace[ c,
        { { cMin_, cMax_ } :> Thread[ cMin <= sums <= cMax ],
          c0_ :> Thread[ sums <= c0 ] } ] ]
  ]
