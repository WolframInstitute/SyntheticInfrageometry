Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[wasserstein1]


(* ===================== Messages ===================== *)

FormanRicciCurvature::badmethod = "Method `1` is not supported by FormanRicciCurvature; expected \"Simple\" or \"Triangles\".";


(* ===================== FormanRicciCurvature ===================== *)

(* Discrete Forman-Ricci curvature on the edges of a graph.
       Method -> "Simple"     :  F(e) = 4 - deg(u) - deg(v)
       Method -> "Triangles"  :  F(e) = 4 - deg(u) - deg(v) + 3 #triangles(e)
   The "Simple" form is the 1-skeleton specialization of Forman's CW
   curvature with all weights = 1.  The "Triangles" form fills each
   3-cycle as a 2-cell; the +3 per triangle is +1 for shared face plus
   +2 from the two parallel edges.  Returns Association[edge -> kappa]. *)

Options[ FormanRicciCurvature ] = { Method -> "Simple" }

FormanRicciCurvature[ graph_Graph, OptionsPattern[] ] :=
  With[ { deg = AssociationThread[ VertexList[ graph ], VertexDegree[ graph ] ] },
    Switch[ OptionValue[ Method ],
      "Simple",
        AssociationMap[
          e |-> 4 - deg[ e[[ 1 ]] ] - deg[ e[[ 2 ]] ],
          EdgeList[ graph ]
        ],
      "Triangles",
        With[ { adj = AssociationMap[ AdjacencyList[ graph, # ] &, VertexList[ graph ] ] },
          AssociationMap[
            e |-> 4 - deg[ e[[ 1 ]] ] - deg[ e[[ 2 ]] ]
                    + 3 Length @ Intersection[ adj[ e[[ 1 ]] ], adj[ e[[ 2 ]] ] ],
            EdgeList[ graph ]
          ]
        ],
      m_, Message[ FormanRicciCurvature::badmethod, m ]; $Failed
    ]
  ]


(* ===================== OllivierRicciCurvature ===================== *)

(* Ollivier-Ricci curvature on the edges of a graph with idleness alpha = 0:
       kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v),
   where mu_x is the uniform probability measure on the open neighborhood
   N(x) and W_1 is the Wasserstein-1 (Earth-Mover) distance under graph
   distance.  Returns Association[edge -> kappa]. *)

OllivierRicciCurvature[ graph_Graph ] :=
  Module[ { vs, idx, adj, dist },
    vs   = VertexList[ graph ];
    idx  = AssociationThread[ vs, Range @ Length @ vs ];
    adj  = AssociationMap[ AdjacencyList[ graph, # ] &, vs ];
    dist = GraphDistanceMatrix[ graph ];
    AssociationMap[
      e |-> With[
        { nu = adj[ e[[ 1 ]] ], nv = adj[ e[[ 2 ]] ] },
        1 - wasserstein1[
              ConstantArray[ 1.0 / Length[ nu ], Length[ nu ] ],
              ConstantArray[ 1.0 / Length[ nv ], Length[ nv ] ],
              dist[[ idx /@ nu, idx /@ nv ]]
            ] / dist[[ idx[ e[[ 1 ]] ], idx[ e[[ 2 ]] ] ]]
      ],
      EdgeList[ graph ]
    ]
  ]


(* Wasserstein-1 distance between probability vectors mu, nu on finite
   point sets given the m-by-n cost matrix.  Solved as a transport LP
   via LinearOptimization. *)

wasserstein1[ mu_List, nu_List, costs_List ] :=
  Module[ { m = Length[ mu ], n = Length[ nu ], vars },
    vars = Array[ t, { m, n } ];
    LinearOptimization[
      Total[ Flatten[ costs * vars ] ],
      Join[
        Table[ Total[ vars[[ i, All ]] ] == mu[[ i ]], { i, m } ],
        Table[ Total[ vars[[ All, j ]] ] == nu[[ j ]], { j, n } ],
        Thread[ Flatten[ vars ] >= 0 ]
      ],
      Flatten[ vars ],
      "PrimalMinimumValue"
    ]
  ]


(* ===================== WolframRicciScalar ===================== *)

(* Volume-comparison Ricci scalar at vertex v and integer scale r:
       R(r) = 6 (d + 2) / r^2 (1 - V(r) / V_E(d, r)),
       V_E(d, r) = pi^(d/2) r^d / Gamma[d/2 + 1],
   where V(r) = |B_r(v)| is the closed metric ball volume.  The local
   dimension d is either supplied via "Dimension" -> d or read off from
   the log-difference (Log V(r+1) - Log V(r)) / (Log(r+1) - Log r)
   when "Dimension" -> Automatic (default).  In Automatic mode the
   default radius range stops at eccentricity(v) - 1 because the local
   dimension at the boundary radius would need V(r+1).  Returns
   Association[ r -> R(r) ];  with All in place of v, returns
   Association[ vertex -> Association[ r -> R(r) ] ]. *)

Options[ WolframRicciScalar ] = { "Dimension" -> Automatic }

WolframRicciScalar[ graph_Graph, v_, { rmin_Integer, rmax_Integer }, OptionsPattern[] ] :=
  Module[ { dim = OptionValue[ "Dimension" ], vols },
    vols = With[ { c = KeySort @ Counts @ DeleteCases[ GraphDistance[ graph, v ], Infinity ] },
      AssociationThread[ Keys[ c ] -> Accumulate[ Values[ c ] ] ]
    ];
    AssociationMap[
      r |-> With[
        { vr = vols[ r ],
          dr = If[ dim === Automatic,
                   N[ ( Log[ vols[ r + 1 ] ] - Log[ vols[ r ] ] )
                     / ( Log[ r + 1 ] - Log[ r ] ) ],
                   dim ] },
        N[ 6 ( dr + 2 ) / r^2 ( 1 - vr Gamma[ dr / 2 + 1 ] / ( Pi^( dr / 2 ) r^dr ) ) ]
      ],
      Range[ rmin, rmax ]
    ]
  ] /; v =!= All

WolframRicciScalar[ graph_Graph, v_, opts : OptionsPattern[] ] :=
  With[ {
      ecc = Max @ DeleteCases[ GraphDistance[ graph, v ], Infinity ],
      drop = Boole[ OptionValue[ "Dimension" ] === Automatic ]
    },
    WolframRicciScalar[ graph, v, { 1, ecc - drop }, opts ]
  ] /; v =!= All

WolframRicciScalar[ graph_Graph, All, args___ ] :=
  AssociationMap[ WolframRicciScalar[ graph, #, args ] &, VertexList[ graph ] ]
