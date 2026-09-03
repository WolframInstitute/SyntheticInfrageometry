Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraPlane wrapper ===================== *)


InfraPlane[ reps_List ][ "Volume" ] := Length /@ reps
(* ===================== FindInfraBisectingHyperplane ===================== *)

(* the bisector slab B = { v : lo <= d(p1, v) - d(p2, v) <= hi }.  On a non-bipartite graph the strict equidistant set may fail to separate, so widen the window to {-1, 1} to recover the parity-stranded band. *)

FindInfraBisectingHyperplane::badmethod   = "Method `1` is not supported by FindInfraBisectingHyperplane.";
FindInfraBisectingHyperplane::badproperty = "Property `1` is not supported by FindInfraBisectingHyperplane.";
FindInfraBisectingHyperplane::shortfall   = "\"RandomGreedy\" drew `1` distinct hyperplanes of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";

Options[ FindInfraBisectingHyperplane ] = {
  Properties -> { },
  Method     -> Automatic
};

FindInfraBisectingHyperplane[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  FindInfraBisectingHyperplane[ graph, p1, p2, { 0, 0 }, count, opts ]

FindInfraBisectingHyperplane[ graph_Graph, p1_, p2_,
    window : { _Integer, _Integer },
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraPlane, count,
    { q1, q2 } |-> Module[ { properties, methodSpec, methodHead, pruning, bisector, aux, admissible },
      properties = OptionValue[ FindInfraBisectingHyperplane, { opts }, Properties ];
      methodSpec = resolveMethod[ OptionValue[ FindInfraBisectingHyperplane, { opts }, Method ], count ];
      methodHead = methodName @ methodSpec;
      pruning    = Replace[ methodSpec, { { "Exhaustive", subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ), _ :> Infinity } ];
      bisector   = Complement[
        Pick[ VertexList[ graph ],
          MapThread[ { x, y } |-> window[[1]] <= x - y <= window[[2]],
            { GraphDistance[ graph, q1 ], GraphDistance[ graph, q2 ] } ] ],
        { q1, q2 } ];
      If[ properties === { },
        { bisector },
        Catch[
          (* aux graph on bisector + {q1, q2}: direct edges plus pairs joined through components of the complement *)
          aux = With[ { nodes = Union[ bisector, { q1, q2 } ] },
            { components = ConnectedComponents @ Subgraph[ graph,
                Complement[ VertexList[ graph ], nodes ] ] },
            { paired = Flatten[
                ( comp |-> UndirectedEdge @@@ Subsets[
                    Intersection[ nodes, Union @@ ( AdjacencyList[ graph, # ] & /@ comp ) ],
                    { 2 } ] ) /@ components, 1 ],
              direct = Cases[ EdgeList[ graph ],
                ( UndirectedEdge | DirectedEdge )[ u_, v_ ] /;
                  MemberQ[ nodes, u ] && MemberQ[ nodes, v ] :> UndirectedEdge[ u, v ] ] },
            Graph[ nodes, DeleteDuplicates[ Join[ paired, direct ] ] ] ];
          admissible = admissibleBisectingHyperplane[ graph, aux, q1, q2, properties ];
          Switch[ methodHead,
            "Exhaustive",   findAllMinimalAdmissible[ graph, bisector, admissible, pruning ],
            "Greedy" | "ShuffledGreedy",
                            findGreedyMinimalAdmissible[ graph, bisector, admissible, count,
                              greedyBranch @ methodHead ],
            "RandomGreedy", randomDraws[
              { } |-> findGreedyMinimalAdmissible[ graph, bisector, admissible, 1, randomBranch ],
              count, FindInfraBisectingHyperplane ],
            _,              Message[ FindInfraBisectingHyperplane::badmethod, methodSpec ]; $Failed
          ]
        ]
      ]
    ], p1, p2 ]

admissibleBisectingHyperplane[ graph_Graph, aux_Graph, p1_, p2_, properties_List ] :=
  With[ { tests = propertyPredicate[ graph, aux, p1, p2, # ] & /@ properties },
    T |-> AllTrue[ tests, # @ T & ]
  ]

propertyPredicate[ _, aux_Graph, p1_, p2_, "Separating" ] :=
  T |-> SeparatesQ[ aux, T, p1, p2 ]

propertyPredicate[ graph_Graph, _, _, _, "Connected" ] :=
  T |-> T =!= { } && ConnectedGraphQ @ Subgraph[ graph, T ]

propertyPredicate[ _, _, _, _, other_ ] :=
  ( Message[ FindInfraBisectingHyperplane::badproperty, other ]; Throw[ $Failed ] )


(* ===================== InfraPlaneQ ===================== *)

(* h sits inside the bisector slab and separates p1 from p2; the three-argument form is the inert scene assertion *)

InfraPlaneQ[ graph_Graph, h : _InfraPlane | _InfraSet, p1_, p2_, window_ : 0 ] :=
  AllTrue[ If[ Head[ h ] === InfraSet, { First @ h }, First @ h ],
    InfraPlaneQ[ graph, #, p1, p2, window ] & ]

InfraPlaneQ[ graph_Graph, h_List, p1_, p2_, window_ : 0 ] :=
  With[ { bounds = If[ ListQ @ window, window, { -window, window } ] },
    SeparatesQ[ graph, h, p1, p2 ] &&
    AllTrue[ h,
      bounds[[ 1 ]] <= GraphDistance[ graph, p1, # ] - GraphDistance[ graph, p2, # ] <= bounds[[ 2 ]] & ]
  ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraPlane[ p1_, p2_, opts___Rule ] ] :=
  dispatchConstruction[ graph, InfraPlane[ p1, p2, { 0, 0 }, opts ] ]

dispatchConstruction[ graph_Graph, InfraPlane[ p1_, p2_,
    window : { _Integer, _Integer }, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      FindInfraBisectingHyperplane[ graph, p1, p2, window, All, Properties -> { "Separating" } ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]
