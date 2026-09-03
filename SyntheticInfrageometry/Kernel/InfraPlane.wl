Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraPlane wrapper ===================== *)

(* InfraPlane[{set}] is the unary form; InfraPlane[{set1, ..., setk}] is the
   multi-realisation form.  Set canonicalisation and the shared accessors
   come from defineInfraBundleRules (Tools.wl). *)

(* "Volume" = vertex count per realisation. *)
InfraPlane[ reps_List ][ "Volume" ] := Length /@ reps
(* ===================== FindInfraBisectingHyperplane ===================== *)

(* A bisecting hyperplane between p1 and p2 is a vertex subset of the
   bisector slab B = { v : lo <= d(p1, v) - d(p2, v) <= hi }.  Two
   orthogonal axes:
     Properties -- a list of predicates the result must satisfy.
        Empty (default) means no filter: return the slab B itself, one
        realisation, the codim-1 perpendicular-bisector level set.
        "Separating" requires SeparatesQ[aux, T, p1, p2].
        "Connected" requires ConnectedGraphQ @ Subgraph[graph, T].
        Properties compose via AND; the resulting closure gates the peel.
     Method     -- how to enumerate inclusion-minimal subsets satisfying
        Properties.  Automatic (default) reads the count: All is the
        exhaustive top-down BFS over the peel-DAG, deduplicated, and a
        bounded count is the lazy peel.  The nested form {"Exhaustive",
        "Pruning" -> spec} caps per-layer branching via applyPruning.
        "Greedy" is the top-down DFS, backtracking at each leaf, first
        `count` minimals; "RandomGreedy" draws random peels instead
        (seed via ambient SeedRandom).
   When Properties is empty, Method is ignored.  On a non-bipartite graph
   the strict equidistant set may fail to separate, so widen the window
   or use {-1, 1} to recover the parity-stranded band. *)

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
          (* aux graph on bisector + {q1, q2}: direct edges plus pairs joined
             through connected components of the complement *)
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

(* h is a bisecting hyperplane between p1 and p2: h sits inside the bisector
   slab { v : lo <= d(p1, v) - d(p2, v) <= hi } and separates p1 from p2.
   window is a half-width w (the slab { -w, w }, default 0 = strict bisector)
   or an explicit { lo, hi }, matching FindInfraBisectingHyperplane's window.
   The three-argument InfraPlaneQ[h, p1, p2] is the inert scene assertion; it
   stays unevaluated until FindInfraScene supplies the graph. *)

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
