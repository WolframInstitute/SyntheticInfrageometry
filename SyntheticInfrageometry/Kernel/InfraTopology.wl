Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraTopologicalSpace wrapper ===================== *)

(* InfraTopologicalSpace[g, top] bundles a graph with its specialization
   preorder digraph.  `top` is the FULL preorder (with reflexive self-loops),
   i.e. BallTopologyGraph[g, r, "Reduced" -> False]: required so that the
   continuity check below is an O(1) hash lookup against the transitive
   closure.  Cache once per (graph, radius) and reuse in sweep loops. *)

Options[ InfraTopologicalSpace ] = { "Dual" -> False };

InfraTopologicalSpace[ graph_Graph, r_?NumericQ, OptionsPattern[] ] :=
  InfraTopologicalSpace[ graph,
    BallTopologyGraph[ graph, r, "Reduced" -> False,
      "Dual" -> TrueQ @ OptionValue[ "Dual" ] ] ]

InfraTopologicalSpace[ graph_Graph, _Graph ][ "Graph" ]      := graph
InfraTopologicalSpace[ _Graph, top_Graph   ][ "Topology" ]   := top


(* ===================== Ball topology (Alexandrov specialization) ===================== *)

(* BallTopologyGraph[g, r]: directed graph of the specialization preorder of
   the Alexandrov topology on V(g) whose closed-set subbasis is the family of
   closed r-balls.  Edge q -> p iff q in cl({p}) iff N_r(p) subset N_r(q).
   "Reduced" -> True (default) returns the Hasse-style transitive reduction
   (self-loops implicit and dropped); "Reduced" -> False returns the full
   preorder digraph including the reflexive self-loops. *)

Options[ BallTopologyGraph ] = { "Reduced" -> True, "Dual" -> False };

BallTopologyGraph[ graph_Graph, r_, OptionsPattern[] ] :=
  With[ { ind = UnitStep[ r - GraphDistanceMatrix[ graph ] ] },
    With[ { preorder = AdjacencyGraph[
              VertexList[ graph ],
              Outer[ Boole[ AllTrue[ #1 - #2, NonNegative ] ] &, ind, ind, 1 ],
              DirectedEdges -> True
            ] },
      With[ { reduced = If[ TrueQ @ OptionValue[ "Reduced" ],
                TransitiveReductionGraph @ preorder, preorder ] },
        If[ TrueQ @ OptionValue[ "Dual" ], ReverseGraph @ reduced, reduced ]
      ]
    ]
  ]


(* BallClosure[g, r, p]: cl(p) = { q : N_r(p) subset N_r(q) } in the
   Alexandrov topology with closed-set subbasis the closed r-balls.
   "Dual" -> True: dual closure cl_dual(p) = { q : N_r(q) subset N_r(p) }. *)

BallClosure[ graph_Graph, r_, InfraPoint[ { { v_ } } ], opts : OptionsPattern[] ] :=
  BallClosure[ graph, r, v, opts ]

Options[ BallClosure ] = { "Dual" -> False };

BallClosure[ graph_Graph, r_, p_, OptionsPattern[] ] :=
  With[ { V = VertexList[ graph ], ind = UnitStep[ r - GraphDistanceMatrix[ graph ] ] },
    With[ { row = ind[[ First @ FirstPosition[ V, p ] ]] },
      If[ TrueQ @ OptionValue[ "Dual" ],
        Pick[ V, AllTrue[ row - #, NonNegative ] & /@ ind ],
        Pick[ V, AllTrue[ # - row, NonNegative ] & /@ ind ]
      ]
    ]
  ]


(* BallContinuousMapQ[g, r, h, s, map]: vertex map f: V(g) -> V(h) is
   continuous for the r-ball topology on g and the s-ball topology on h iff
   it is monotone for the specialization preorder, q <=_g p ==> f(q) <=_h f(p).
   Hasse edges of the source suffice (transitivity propagates through target);
   use the full preorder for the target so EdgeQ is an O(1) hash lookup.
   "Dual" -> True applies the dual topology on both sides; dual-to-dual
   continuity is equivalent to primal-to-primal by relabeling.
   map: Association, list of Rule, or callable. *)

Options[ BallContinuousMapQ ] = { "Dual" -> False };

BallContinuousMapQ[ g_Graph, r_, h_Graph, s_, map_, OptionsPattern[] ] :=
  With[ { dual = TrueQ @ OptionValue[ "Dual" ] },
    BallContinuousMapQ[
      InfraTopologicalSpace[ g, r, "Dual" -> dual ],
      InfraTopologicalSpace[ h, s, "Dual" -> dual ],
      map ]
  ]

BallContinuousMapQ[
    InfraTopologicalSpace[ _, src_Graph ],
    InfraTopologicalSpace[ _, tgt_Graph ],
    map_ ] :=
  With[ { f = If[ MatchQ[ map, { __Rule } ], Association @ map, map ] },
    AllTrue[ EdgeList[ src ],
      e |-> e[[ 1 ]] === e[[ 2 ]] || EdgeQ[ tgt, f @ e[[ 1 ]] -> f @ e[[ 2 ]] ]
    ]
  ]
