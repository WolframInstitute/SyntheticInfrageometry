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


(* ===================== InfraSet ===================== *)

(* Wrapper for an arbitrary vertex subset of the underlying graph. *)

InfraSet[ vs_List ] /; MemberQ[ vs, _InfraSet ] :=
  InfraSet[ Union @@ Replace[ vs, s_InfraSet :> s[ "Vertices" ], {1} ] ]

(* InfraPoint is special: each realisation IS a vertex (possibly a list vertex
   label like {i,j}), so rs is already the vertex list -- no flattening needed. *)
InfraSet[ InfraPoint[ vs_List ] ] :=
  InfraSet[ Sort @ DeleteDuplicates @ vs ]

(* All other Infra* container wrappers have realisations that are vertex-lists
   (InfraBall, InfraShell, InfraSegment, ...): rs = {{vs_1}, {vs_2}, ...}.
   Flatten one level to union all vertex-sets into a flat list. *)
InfraSet[ wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSet[ Sort @ DeleteDuplicates @ Flatten[ rs, 1 ] ]

InfraSet[ vs_List ][ "Vertices" ] := vs
InfraSet[ vs_List ][ "Length" ]   := Length[ vs ]


(* ===================== InfraSetClosure ===================== *)

(* cl(S): union of VertexInComponent over all s in S. *)

InfraSetClosure[ ts_InfraTopologicalSpace, InfraSet[ vs_List ] ] :=
  InfraSet[ Union @@ Map[ v |-> VertexInComponent[ ts[ "Topology" ], {v} ], vs ] ]

InfraSetClosure[ ts_InfraTopologicalSpace, wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSetClosure[ ts, InfraSet[ wrapper[ rs ] ] ]


(* ===================== InfraSetInterior ===================== *)

(* int(S) = V \ cl(V\S). *)

InfraSetInterior[ ts_InfraTopologicalSpace, InfraSet[ vs_List ] ] :=
  With[ { vertices = VertexList[ ts[ "Graph" ] ], topo = ts[ "Topology" ] },
    InfraSet[ Complement[ vertices,
      Union @@ Map[ v |-> VertexInComponent[ topo, {v} ], Complement[ vertices, vs ] ]
    ]]
  ]

InfraSetInterior[ ts_InfraTopologicalSpace, wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSetInterior[ ts, InfraSet[ wrapper[ rs ] ] ]


(* ===================== InfraSetBoundary ===================== *)

(* bd(S) = cl(S) \ int(S). *)

InfraSetBoundary[ ts_InfraTopologicalSpace, s_InfraSet ] :=
  InfraSet[ Complement[
    InfraSetClosure[ ts, s ][ "Vertices" ],
    InfraSetInterior[ ts, s ][ "Vertices" ]
  ]]

InfraSetBoundary[ ts_InfraTopologicalSpace, wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSetBoundary[ ts, InfraSet[ wrapper[ rs ] ] ]


(* ===================== InfraSetNeighborhood ===================== *)

(* Unique minimal open neighborhood of S in ts: the principal upset of S in the
   specialization preorder = union of VertexOutComponent over vertices of S.
   Unique because Alexandrov topologies are closed under arbitrary intersections. *)

InfraSetNeighborhood[ ts_InfraTopologicalSpace, InfraSet[ vs_List ] ] :=
  InfraSet[ Union @@ Map[ v |-> VertexOutComponent[ ts[ "Topology" ], {v} ], vs ] ]

InfraSetNeighborhood[ ts_InfraTopologicalSpace, wrapper_Symbol[ rs_List ] ] /;
    wrapper =!= InfraSet && StringStartsQ[ SymbolName @ wrapper, "Infra" ] :=
  InfraSetNeighborhood[ ts, InfraSet[ wrapper[ rs ] ] ]


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
