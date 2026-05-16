Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Ball topology (Alexandrov specialization) ===================== *)

(* BallTopologyGraph[g, r]: directed graph of the specialization preorder of
   the Alexandrov topology on V(g) whose closed-set subbasis is the family of
   closed r-balls.  Edge q -> p iff q in cl({p}) iff N_r(p) subset N_r(q).
   "Reduced" -> True (default) returns the Hasse-style transitive reduction
   (self-loops implicit and dropped); "Reduced" -> False returns the full
   preorder digraph including the reflexive self-loops. *)

Options[ BallTopologyGraph ] = { "Reduced" -> True };

BallTopologyGraph[ graph_Graph, r_, OptionsPattern[] ] :=
  With[ { ind = UnitStep[ r - GraphDistanceMatrix[ graph ] ] },
    With[ { preorder = AdjacencyGraph[
              VertexList[ graph ],
              Outer[ Boole[ AllTrue[ #1 - #2, NonNegative ] ] &, ind, ind, 1 ],
              DirectedEdges -> True
            ] },
      If[ TrueQ @ OptionValue[ "Reduced" ],
        TransitiveReductionGraph @ preorder,
        preorder
      ]
    ]
  ]


(* BallClosure[g, r, p]: cl(p) = { q : N_r(p) subset N_r(q) } in the
   Alexandrov topology with closed-set subbasis the closed r-balls. *)

BallClosure[ graph_Graph, r_, InfraPoint[ { { v_ } } ] ] :=
  BallClosure[ graph, r, v ]

BallClosure[ graph_Graph, r_, p_ ] :=
  With[ { V = VertexList[ graph ], ind = UnitStep[ r - GraphDistanceMatrix[ graph ] ] },
        { row = ind[[ First @ FirstPosition[ V, p ] ]] },
    Pick[ V, AllTrue[ # - row, NonNegative ] & /@ ind ]
  ]


(* BallContinuousMapQ[g, r, h, s, map]: vertex map f: V(g) -> V(h) is
   continuous for the r-ball topology on g and the s-ball topology on h iff
   it is monotone for the specialization preorder, q <=_g p ==> f(q) <=_h f(p).
   Hasse edges of the source suffice (transitivity propagates through target);
   use the full preorder for the target so EdgeQ is an O(1) hash lookup.
   map: Association, list of Rule, or callable. *)

BallContinuousMapQ[ g_Graph, r_, h_Graph, s_, map_ ] :=
  With[ { f   = If[ MatchQ[ map, { __Rule } ], Association @ map, map ],
          tgt = BallTopologyGraph[ h, s, "Reduced" -> False ] },
    AllTrue[ EdgeList @ BallTopologyGraph[ g, r ],
      e |-> EdgeQ[ tgt, f @ e[[ 1 ]] -> f @ e[[ 2 ]] ]
    ]
  ]
