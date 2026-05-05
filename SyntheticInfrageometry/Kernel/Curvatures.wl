Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[wasserstein1]


(* ===================== Messages ===================== *)

FormanRicci::badmethod = "Method `1` is not supported by FormanRicci; expected \"Simple\" or \"Triangles\".";


(* ===================== FormanRicci ===================== *)

(* Discrete Forman-Ricci curvature on the edges of a graph.
       Method -> "Simple"     :  F(e) = 4 - deg(u) - deg(v)
       Method -> "Triangles"  :  F(e) = 4 - deg(u) - deg(v) + 3 #triangles(e)
   The "Simple" form is the 1-skeleton specialization of Forman's CW
   curvature with all weights = 1.  The "Triangles" form fills each
   3-cycle as a 2-cell; the +3 per triangle is +1 for shared face plus
   +2 from the two parallel edges.  Returns Association[edge -> kappa]. *)

Options[ FormanRicci ] = { Method -> "Simple" }

FormanRicci[ graph_Graph, OptionsPattern[] ] :=
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
      m_, Message[ FormanRicci::badmethod, m ]; $Failed
    ]
  ]


(* ===================== OllivierRicci ===================== *)

(* Ollivier-Ricci curvature on the edges of a graph with idleness alpha = 0:
       kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v),
   where mu_x is the uniform probability measure on the open neighborhood
   N(x) and W_1 is the Wasserstein-1 (Earth-Mover) distance under graph
   distance.  Returns Association[edge -> kappa]. *)

OllivierRicci[ graph_Graph ] :=
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


(* ===================== WolframRicci ===================== *)

(* Volume-comparison Ricci scalar at vertex v and integer radius r:
       R(v, r) = 6 (d + 2) / r^2 (1 - V(r) / V_E(d, r)),
       V_E(d, r) = pi^(d/2) r^d / Gamma[d/2 + 1],
   where V(r) = |B_r(v)| is the closed metric ball volume.  The local
   dimension d is either supplied via "Dimension" -> d or read off from
   the log-difference (Log V(r+1) - Log V(r)) / (Log(r+1) - Log r)
   when "Dimension" -> Automatic (default); the latter caps the per-vertex
   valid radius range at eccentricity(v) - 1 because V(r+1) must exist.

   WolframRicci[graph]               returns Association[v -> mean_r R(v, r)]
   averaging over r = 1, ..., ecc(v) (- 1 in Automatic dimension mode).
   WolframRicci[graph, {rmin, rmax}] averages over the intersection of
   [rmin, rmax] with that per-vertex valid range.
   WolframRicci[graph, r_Integer]    returns Association[v -> R(v, r)],
   no averaging.  Vertices whose valid range is empty map to Indeterminate. *)

Options[ WolframRicci ] = { "Dimension" -> Automatic }

WolframRicci[ graph_Graph,
    range : (_Integer | { _Integer, _Integer } | All) : All,
    OptionsPattern[] ] :=
  With[ { dim = OptionValue[ "Dimension" ] },
    AssociationMap[
      v |-> wolframRicciAtVertex[ graph, v, range, dim ],
      VertexList[ graph ]
    ]
  ]


(* Per-vertex helper: builds V(r) by accumulating distance counts, picks
   the valid radius window (capped at ecc(v), or ecc(v) - 1 when the
   local dimension is read off Automatic), and returns Mean over that
   window of the volume-comparison scalar.  An empty window yields
   Indeterminate. *)

wolframRicciAtVertex[ graph_Graph, v_, range_, dim_ ] :=
  Module[ { vols, top, rs },
    vols = With[ { c = KeySort @ Counts @ DeleteCases[ GraphDistance[ graph, v ], Infinity ] },
      AssociationThread[ Keys[ c ] -> Accumulate[ Values[ c ] ] ]
    ];
    top = Max[ Keys[ vols ] ] - Boole[ dim === Automatic ];
    rs = Switch[ range,
      All,                    Range[ 1, top ],
      _Integer,               If[ 1 <= range <= top, { range }, { } ],
      { _Integer, _Integer }, Range[ Max[ 1, range[[ 1 ]] ], Min[ top, range[[ 2 ]] ] ]
    ];
    If[ rs === { },
      Indeterminate,
      Mean[ ( r |-> With[
          { dr = If[ dim === Automatic,
                     N[ ( Log[ vols[ r + 1 ] ] - Log[ vols[ r ] ] )
                       / ( Log[ r + 1 ] - Log[ r ] ) ],
                     dim ],
            vr = vols[ r ] },
          N[ 6 ( dr + 2 ) / r^2 ( 1 - vr Gamma[ dr / 2 + 1 ] / ( Pi^( dr / 2 ) r^dr ) ) ]
        ] ) /@ rs
      ]
    ]
  ]
