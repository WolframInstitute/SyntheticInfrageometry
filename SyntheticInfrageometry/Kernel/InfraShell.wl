Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findShellCore]


(* ===================== InfraShell wrapper ===================== *)

(* InfraShell[{set}] is the unary form; InfraShell[{set1, ..., setk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraShell[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraShell[ _List ] ] ] :=
  InfraShell[ Flatten[ reps /. InfraShell[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraShell ===================== *)

(* A shell of radius r around c is a vertex subset of the level surface
   { v : rmin <= d(c, v) <= rmax }.  Two orthogonal axes:
     Properties -- filters every realisation must satisfy.  Empty
       (default) means no filter: return the level surface itself, one
       realisation.  "Separating" requires SeparatingSetQ; "Connected"
       requires ConnectedGraphQ on the induced subgraph.  Properties
       compose via AND.
     Method     -- how to enumerate inclusion-minimal subsets satisfying
       Properties.  "Exhaustive" (default) is top-down BFS over the
       peel-DAG, deduplicated; the nested form {"Exhaustive", "Pruning"
       -> spec} caps per-layer branching via applyPruning.  "Greedy" is
       top-down DFS, no backtracking, one realisation.
   When Properties is empty, Method is ignored. *)

FindInfraShell::badmethod   = "Method `1` is not supported by FindInfraShell.";
FindInfraShell::badproperty = "Property `1` is not supported by FindInfraShell.";

Options[ FindInfraShell ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraShell[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraShell, count,
    findShellCore[ graph, ##, opts ] &, p, r ]


findShellCore[ graph_Graph, p_, r_, opts : OptionsPattern[ FindInfraShell ] ] :=
  Module[ { properties, methodSpec, methodHead, pruning, range, localG, levelSet, radius, admissible },
    properties = OptionValue[ FindInfraShell, { opts }, Properties ];
    methodSpec = OptionValue[ FindInfraShell, { opts }, Method ];
    methodHead = methodName @ methodSpec;
    pruning    = Replace[ methodSpec,
                  { { "Exhaustive", subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ),
                    _ :> Infinity } ];
    range = Replace[ r, d_?NumericQ :> { d, d } ];
    localG = If[ NumericQ[ range[[ 2 ]] ],
                 NeighborhoodGraph[ graph, p, Ceiling[ range[[ 2 ]] ] + 1 ], graph ];
    levelSet = Select[ VertexList[ localG ],
      range[[ 1 ]] <= GraphDistance[ localG, p, # ] <= range[[ 2 ]] & ];
    radius = If[ NumericQ[ r ], r, Mean[ r ] ];
    If[ properties === { },
      { levelSet },
      Catch[
        admissible = admissibleShell[ localG, p, radius, properties ];
        Switch[ methodHead,
          "Exhaustive", findAllMinimalAdmissible[ localG, levelSet, admissible, pruning ],
          "Greedy",     findGreedyMinimalAdmissible[ localG, levelSet, admissible ],
          _,            Message[ FindInfraShell::badmethod, methodSpec ]; $Failed
        ]
      ]
    ]
  ]


admissibleShell[ localG_Graph, center_, radius_, properties_List ] :=
  With[ { tests = propertyPredicateShell[ localG, center, radius, # ] & /@ properties },
    t |-> AllTrue[ tests, # @ t & ]
  ]


propertyPredicateShell[ localG_Graph, center_, radius_, "Separating" ] :=
  t |-> SeparatingSetQ[ localG, t, center, radius ]

propertyPredicateShell[ localG_Graph, _, _, "Connected" ] :=
  t |-> t =!= { } && ConnectedGraphQ @ Subgraph[ localG, t ]

propertyPredicateShell[ _, _, _, other_ ] :=
  ( Message[ FindInfraShell::badproperty, other ]; Throw[ $Failed ] )


(* ===================== FindInfraOsculatingShell ===================== *)

(* Osculating shells at the k-vertex window centered on path[[i]]:
   for every vertex c equidistant from all k window-vertices (common
   distance r), the level set { v : d(c, v) == r }, one unary
   InfraShell[{set}] per such center, sorted by ascending radius
   (with center as a tie-break).  Forwards Properties / Method to
   FindInfraShell.  path is a vertex list or any InfraPath wrapper
   (multi-realisation paths are spread and unioned). *)

Options[ FindInfraOsculatingShell ] = Options[ FindInfraShell ];

FindInfraOsculatingShell[ graph_Graph, path_, i_Integer, k_Integer,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  Module[ { walks, vlist, vidx, dm, pairs, sets, capped },
    walks = infraSpread @ If[ ListQ @ path, InfraPath[ { path } ], path ];
    vlist = VertexList @ graph;
    vidx  = AssociationThread[ vlist -> Range @ Length @ vlist ];
    dm    = GraphDistanceMatrix @ graph;
    pairs = SortBy[
      DeleteDuplicates @ Flatten[
        Map[
          walk |-> With[ {
              lo = Clip[ i - Floor[ ( k - 1 ) / 2 ], { 1, Length @ walk } ],
              hi = Clip[ i + Ceiling[ ( k - 1 ) / 2 ], { 1, Length @ walk } ] },
            With[ { cols = Lookup[ vidx, walk[[ lo ;; hi ]] ] },
              MapThread[
                If[ SameQ @@ #2, { #1, First @ #2 }, Nothing ] &,
                { vlist, dm[[ All, cols ]] } ]
            ]
          ],
          walks ],
        1 ],
      { Last, First } ];
    sets = Flatten[
      ( ( #[[ 1, 1 ]] & ) /@ FindInfraShell[ graph, #[[ 1 ]], #[[ 2 ]], All, opts ] & ) /@ pairs,
      1 ];
    capped = infraCap[ sets, count ];
    If[ capped === $Failed, $Failed, InfraShell[ { # } ] & /@ capped ]
  ]


(* ===================== FindInfraShellParameters ===================== *)

(* For a vertex set vs, return the {center, radius} pairs for which vs
   is a metric shell: in some component of g \ vs, a center c is
   equidistant from every vertex of vs, dominates that component, and
   is strictly closer to the inside than to the outside. *)

FindInfraShellParameters[ graph_Graph, vs_List ] :=
  Module[ { rem, comps },
    rem = VertexDelete[ graph, vs ];
    comps = ConnectedComponents[ rem ];
    Flatten[ Table[
      With[ {
          distMatrix = GraphDistanceMatrix[ Subgraph[ graph, comp ] ],
          otherVertices = Complement[ VertexList[ rem ], comp ] },
        With[ { scores = Max /@ distMatrix },
          With[ { centers = Pick[ comp, scores, Min[ scores ] ] },
            Select[
              { #, GraphDistance[ graph, #, First[ vs ] ] } & /@ centers,
              pair |-> With[ { v = pair[[ 1 ]], r = pair[[ 2 ]] },
                AllTrue[ vs, GraphDistance[ graph, v, # ] == r & ] &&
                AllTrue[ comp, GraphDistance[ graph, v, # ] <= r & ] &&
                AllTrue[ otherVertices, GraphDistance[ graph, v, # ] > r & ]
              ]
            ]
          ]
        ]
      ],
      { comp, comps }
    ], 1 ]
  ]


(* ===================== InfraShellQ ===================== *)

InfraShellQ[ graph_Graph, vs_List ] :=
  Length[ FindInfraShellParameters[ graph, vs ] ] > 0


(* ===================== SeparatesQ ===================== *)

(* SeparatesQ tests whether deleting vs disconnects u from v.  Endpoint
   deletion does not count as separation. *)

SeparatesQ[ graph_Graph, vs_List, u_, v_ ] :=
  If[ MemberQ[ vs, u ] || MemberQ[ vs, v ], False,
    GraphDistance[ VertexDelete[ graph, vs ], u, v ] === Infinity
  ]


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraShell[ center_, r_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindInfraShell[ graph, center, r, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraShell ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Center" -> center,
                "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]
