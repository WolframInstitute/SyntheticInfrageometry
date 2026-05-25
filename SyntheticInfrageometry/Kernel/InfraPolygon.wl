Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findRegularPolygonCore]
PackageScope[regularPolygonDiagonalsQ]


(* ===================== InfraPolygon wrapper ===================== *)

(* InfraPolygon[{cycle}] is the unary form; InfraPolygon[{cycle1, ..., cyclek}]
   is the multi-realisation form.  Auto-flatten on nested wrappers. *)

InfraPolygon[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPolygon[ _List ] ] ] :=
  InfraPolygon[ Flatten[ reps /. InfraPolygon[ xs_List ] :> xs, 1 ] ]

(* "Length" = vertex count per realisation (= edge count, wrap-around implicit). *)
InfraPolygon[ reps_List ][ "Length" ] := Length /@ reps


(* ===================== FindInfraRegularPolygon ===================== *)

(* A regular n-gon w.r.t. metric tuple {A_1, ..., A_m}: a cyclic vertex
   sequence v_1, ..., v_n with d(v_i, v_{i+k}) == A_k for every i and every
   k = 1, ..., m.  Length m may be anywhere from 1 (equilateral) to Floor[n/2]
   (full metric profile fixed).  The k = 1 constraint collapses the search to
   length-n cycles of the distance-A_1 graph; remaining k post-filter. *)

FindInfraRegularPolygon::badproperty = "Property `1` is not supported by FindInfraRegularPolygon.";
FindInfraRegularPolygon::badmethod   = "Method `1` is not supported by FindInfraRegularPolygon.";
FindInfraRegularPolygon::badcount    = "Diagonal tuple `1` has length exceeding Floor[n/2] for n = `2`.";

Options[ FindInfraRegularPolygon ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraRegularPolygon[ graph_Graph, As : { _?Positive .. }, n_Integer /; n >= 3,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  With[ { core = findRegularPolygonCore[ graph, As, n, opts ] },
    If[ core === $Failed, $Failed,
      With[ { capped = infraCap[ core, count ] },
        If[ capped === $Failed, $Failed, InfraPolygon[ { # } ] & /@ capped ]
      ]
    ]
  ]


findRegularPolygonCore[ graph_Graph, As_List, n_Integer, opts : OptionsPattern[ FindInfraRegularPolygon ] ] :=
  Module[ { properties, methodSpec, dm, idx, distAGraph, candidates, pruning },
    properties = OptionValue[ FindInfraRegularPolygon, { opts }, Properties ];
    methodSpec = OptionValue[ FindInfraRegularPolygon, { opts }, Method ];
    Catch[
      If[ properties =!= { },
        Message[ FindInfraRegularPolygon::badproperty, First @ properties ]; Throw[ $Failed ] ];
      If[ Length[ As ] > Floor[ n / 2 ],
        Message[ FindInfraRegularPolygon::badcount, As, n ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec },
        If[ methodHead =!= "Exhaustive",
          Message[ FindInfraRegularPolygon::badmethod, methodSpec ]; Throw[ $Failed ] ]
      ];
      pruning = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity;
      dm  = GraphDistanceMatrix @ graph;
      idx = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount @ graph ];
      distAGraph = distanceLevelGraph[ graph, dm, First @ As ];
      candidates = cycleToVertexSequence /@ applyPruning[ FindCycle[ distAGraph, { n }, All ], pruning ];
      If[ Length[ As ] >= 2,
        candidates = Select[ candidates, regularPolygonDiagonalsQ[ #, As, n, dm, idx ] & ]
      ];
      DeleteDuplicates @ candidates
    ]
  ]


(* Distance-d graph: same vertex set as graph, edges between pairs at graph
   distance exactly d.  Built from the precomputed distance matrix. *)

distanceLevelGraph[ graph_Graph, dm_, d_ ] :=
  With[ { vs = VertexList @ graph },
    Graph[ vs,
      Cases[ Subsets[ Range @ Length @ vs, { 2 } ],
        { i_, j_ } /; dm[[ i, j ]] === d :> UndirectedEdge[ vs[[ i ]], vs[[ j ]] ] ]
    ]
  ]


regularPolygonDiagonalsQ[ cyc_List, As_List, n_Integer, dm_, idx_ ] :=
  AllTrue[ Range[ 2, Length @ As ],
    k |-> With[ { target = As[[ k ]] },
      AllTrue[ Range[ n ],
        i |-> dm[[ idx @ cyc[[ i ]],
                   idx @ cyc[[ Mod[ i + k - 1, n ] + 1 ]] ]] === target ] ] ]


(* ===================== InfraRegularPolygonQ ===================== *)

(* Tests whether `cycle` is a regular n-gon w.r.t. metric tuple As: every
   consecutive pair at distance A_1 in graph, and every k-diagonal at
   distance A_k for k = 2, ..., Length[As].  Accepts open ({v1, ..., vn})
   or closed ({v1, ..., vn, v1}) input. *)

InfraRegularPolygonQ[ graph_Graph, cycle_List, As : { _?Positive .. } ] /; Length[ cycle ] >= 3 :=
  With[ { open = If[ First @ cycle === Last @ cycle, Most @ cycle, cycle ] },
    With[ { n = Length @ open,
            dm  = GraphDistanceMatrix @ graph,
            idx = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount @ graph ] },
      DuplicateFreeQ[ open ] && Length[ As ] <= Floor[ n / 2 ] &&
      AllTrue[ Range[ n ],
        i |-> dm[[ idx @ open[[ i ]], idx @ open[[ Mod[ i, n ] + 1 ]] ]] === First @ As ] &&
      regularPolygonDiagonalsQ[ open, As, n, dm, idx ]
    ]
  ]

InfraRegularPolygonQ[ _Graph, cycle_List, _List ] /; Length[ cycle ] < 3 := False
