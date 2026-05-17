Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraTopologicalSpace wrapper ===================== *)

(* Auto-reduce: fires only when the input topology is not already the Hasse
   diagram (i.e. has edges that TransitiveReductionGraph would remove). *)
InfraTopologicalSpace[ graph_Graph, topo_Graph ] :=
  With[ { hasse = TransitiveReductionGraph @ topo },
    InfraTopologicalSpace[ graph, hasse ] /; EdgeCount[ hasse ] < EdgeCount[ topo ]
  ]

InfraTopologicalSpace[ graph_Graph, "Topology" -> {"Ball", r_} ] :=
  InfraTopologicalSpace[ graph, InfraBallTopology[ graph, r ] ]

InfraTopologicalSpace[ graph_Graph, _Graph ][ "Graph" ]    := graph
InfraTopologicalSpace[ _Graph, hasse_Graph ][ "Topology" ] := hasse

(* Display: underlying graph (gray) overlaid with Hasse arrows ($InfraTopologyColor). *)
InfraTopologicalSpace /: MakeBoxes[
    InfraTopologicalSpace[ graph_Graph, hasse_Graph ], form_ ] :=
  With[ { coords = Thread[ VertexList[ graph ] -> GraphEmbedding[ graph ] ] },
    ToBoxes[ Show[
      Graph[ graph,
        VertexCoordinates -> coords,
        EdgeStyle -> Directive[ GrayLevel[ 0.70 ], Thickness[ 0.006 ] ] ],
      Graph[ VertexList[ graph ], EdgeList[ hasse ],
        DirectedEdges -> True,
        VertexCoordinates -> coords,
        VertexStyle -> Transparent,
        VertexLabels -> None,
        EdgeStyle -> Directive[ $InfraTopologyColor, Thickness[ 0.005 ], Arrowheads[ Medium ] ] ]
    ], form ]
  ]


(* ===================== InfraBallTopology ===================== *)

(* InfraBallTopology[g, r]: Hasse diagram of the specialization preorder of
   the Alexandrov topology on V(g) with closed-set subbasis the closed r-balls.
   Edge q -> p iff B_r(p) subset B_r(q), with transitive edges removed. *)

Options[ InfraBallTopology ] = { "Dual" -> False };

InfraBallTopology[ graph_Graph, r_, OptionsPattern[] ] :=
  With[ { ind = UnitStep[ r - GraphDistanceMatrix[ graph ] ] },
    With[ { hasse = TransitiveReductionGraph @ AdjacencyGraph[
              VertexList[ graph ],
              Outer[ Boole[ AllTrue[ #1 - #2, NonNegative ] ] &, ind, ind, 1 ],
              DirectedEdges -> True ] },
      If[ TrueQ @ OptionValue[ "Dual" ], ReverseGraph @ hasse, hasse ]
    ]
  ]


(* ===================== InfraTopologicalGraph ===================== *)

(* Convenience shell: produces an InfraTopologicalSpace displayed with MakeBoxes. *)
InfraTopologicalGraph[ graph_Graph, r_, opts___ ] :=
  InfraTopologicalSpace[ graph, InfraBallTopology[ graph, r, opts ] ]


(* ===================== InfraPointClosure ===================== *)

(* InfraPointClosure[ts, p]: cl(p) wrapped as InfraPoint[{closure_vertices}].
   Multi-InfraPoint input: union of closures of all vertices across all realizations. *)

InfraPointClosure[ ts_InfraTopologicalSpace, InfraPoint[ realizations_List ] ] :=
  InfraPoint[{ Union @@ Map[
    VertexInComponent[ ts[ "Topology" ], # ]&,
    Union @@ realizations
  ]}]

InfraPointClosure[ ts_InfraTopologicalSpace, p_ ] :=
  InfraPoint[{ VertexInComponent[ ts[ "Topology" ], p ] }]


(* ===================== InfraInterior ===================== *)

(* InfraInterior[ts, s]: int(S) = V \ cl(V\S), wrapped as InfraPoint[{interior_vertices}].
   Multi-InfraPoint input: computes interior of the union of all realizations. *)

InfraInterior[ ts_InfraTopologicalSpace, InfraPoint[ realizations_List ] ] :=
  With[ { vertices = VertexList[ ts[ "Graph" ] ],
          topo     = ts[ "Topology" ],
          verts    = Union @@ realizations },
    InfraPoint[{ Complement[ vertices,
      Union @@ Map[ VertexInComponent[ topo, # ]&, Complement[ vertices, verts ] ]
    ]}]
  ]

InfraInterior[ ts_InfraTopologicalSpace, p_ ] :=
  InfraInterior[ ts, InfraPoint[ {{p}} ] ]


(* ===================== ContinuousMapQ ===================== *)

(* ContinuousMapQ[f, s1, s2]: vertex map f: V(g1) -> V(g2) is continuous iff it
   is monotone for the specialization preorder - every Hasse edge q -> p in s1
   maps to a pair f(q), f(p) connected in the transitive closure of s2's Hasse.
   map: Association, list of Rule, or callable. *)

ContinuousMapQ[ f_, s1_InfraTopologicalSpace, s2_InfraTopologicalSpace ] :=
  With[ { tgtClosure = TransitiveClosureGraph[ s2[ "Topology" ] ],
          map = If[ MatchQ[ f, { __Rule } ], Association @ f, f ] },
    AllTrue[ EdgeList[ s1[ "Topology" ] ],
      e |-> EdgeQ[ tgtClosure, map @ e[[ 1 ]] -> map @ e[[ 2 ]] ]
    ]
  ]
