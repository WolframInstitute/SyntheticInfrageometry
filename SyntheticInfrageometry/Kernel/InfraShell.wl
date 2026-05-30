Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findShellCore]


(* ===================== InfraShell wrapper ===================== *)

(* InfraShell[{set}] is the unary form; InfraShell[{set1, ..., setk}] is the
   multi-realisation form.  Only auto-flatten on nested wrappers. *)

InfraShell[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraShell[ _List ] ] ] :=
  InfraShell[ Flatten[ reps /. InfraShell[ xs_List ] :> xs, 1 ] ]

(* "Volume" = vertex count per realisation. *)
InfraShell[ reps_List ][ "Volume" ] := Length /@ reps


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


(* ===================== FindInfraShellCenter ===================== *)

(* Center / radius of a metric shell, two methods.

   Method -> "MaximalChordsBisectors" (default): bisect the shell's
   longest chords and union the midpoints into one weighted InfraPoint --
   mass = how many chords bisect there, heaviest support vertex = best
   center; radius = sorted union of the integer arms { Floor[d/2],
   Ceil[d/2] }.  Sub-options "Maximality" ("PerVertex" (default): each
   vertex's own farthest shell partner(s) | "Diameter": only the globally
   longest chords), "Metric" ("Extrinsic" (default) | "Intrinsic":
   antipodes by shell-subgraph distance; midpoints/parity/arms always use
   ambient distance), "Parity" (All (default) | "Even" pure single-vertex
   bisectors | "Odd" two-vertex mesopoints).

   Method -> "EquidistantPoints": the exact centers -- every vertex
   equidistant from the whole shell at a common finite radius > 0;
   unweighted InfraPoint, radius = sorted distinct common distances.

   Both return { InfraPoint[support, masses], radii }. *)

FindInfraShellCenter::badmethod     = "Method `1` is not MaximalChordsBisectors or EquidistantPoints.";
FindInfraShellCenter::badmaximality = "Maximality `1` is not PerVertex or Diameter.";
FindInfraShellCenter::badmetric     = "Metric `1` is not Extrinsic or Intrinsic.";
FindInfraShellCenter::badparity     = "Parity `1` is not All, Even, or Odd.";

Options[ FindInfraShellCenter ] = { Method -> "MaximalChordsBisectors" };

FindInfraShellCenter[ graph_Graph, shell_InfraShell, opts : OptionsPattern[] ] :=
  FindInfraShellCenter[ graph, Union @@ First[ shell ], opts ]

FindInfraShellCenter[ graph_Graph, vs_List, OptionsPattern[] ] :=
  With[ { spec = OptionValue[ Method ] },
    Switch[ methodName @ spec,
      "MaximalChordsBisectors", maximalChordsBisectors[ graph, vs, methodOptions @ spec ],
      "EquidistantPoints",      equidistantShellPoints[ graph, vs ],
      _, Message[ FindInfraShellCenter::badmethod, spec ]; $Failed ] ]

maximalChordsBisectors[ graph_Graph, vs_List, mopts_List ] :=
  Module[ { maximality, metric, parity, dm, idx, dsel, chords, kept, midpoints, arms },
    maximality = Lookup[ mopts, "Maximality", "PerVertex" ];
    metric     = Lookup[ mopts, "Metric", "Extrinsic" ];
    parity     = Lookup[ mopts, "Parity", All ];
    Switch[ maximality, "PerVertex" | "Diameter", Null, _, Message[ FindInfraShellCenter::badmaximality, maximality ]; Return[ $Failed ] ];
    Switch[ metric, "Extrinsic" | "Intrinsic", Null, _, Message[ FindInfraShellCenter::badmetric, metric ]; Return[ $Failed ] ];
    Switch[ parity, All | "Even" | "Odd", Null, _, Message[ FindInfraShellCenter::badparity, parity ]; Return[ $Failed ] ];
    dm  = GraphDistanceMatrix[ graph ];
    idx = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount @ graph ];
    dsel = If[ metric === "Intrinsic",
               With[ { subg = Subgraph[ graph, vs ] },
                 With[ { rows = Lookup[ AssociationThread[ VertexList[ subg ] -> Range @ VertexCount @ subg ], vs ] },
                   GraphDistanceMatrix[ subg ][[ rows, rows ]] ] ],
               dm[[ Lookup[ idx, vs ], Lookup[ idx, vs ] ]] ];
    chords = Switch[ maximality,
      "Diameter",
        With[ { dmax = Max @ Select[ Flatten @ dsel, Positive[ # ] && # =!= Infinity & ] },
          { vs[[ #[[ 1 ]] ]], vs[[ #[[ 2 ]] ]] } & /@
            Select[ Subsets[ Range @ Length @ vs, { 2 } ], dsel[[ #[[ 1 ]], #[[ 2 ]] ]] == dmax & ] ],
      "PerVertex",
        DeleteDuplicates[ Sort /@ Flatten[
          Table[
            With[ { row = ReplacePart[ dsel[[ i ]], i -> Infinity ] },
              With[ { ecc = Max @ Select[ row, # =!= Infinity & ] },
                { vs[[ i ]], # } & /@ Pick[ vs, Thread[ row == ecc ], True ] ] ],
            { i, Length[ vs ] } ], 1 ] ] ];
    kept = Select[ chords,
      With[ { d = dm[[ idx @ #[[ 1 ]], idx @ #[[ 2 ]] ]] },
        Switch[ parity, All, True, "Even", EvenQ[ d ], "Odd", OddQ[ d ] ] ] & ];
    midpoints = Counts @ Catenate[ shellMidpointVertices[ dm, idx, # ] & /@ kept ];
    arms = Union @@ ( With[ { d = dm[[ idx @ #[[ 1 ]], idx @ #[[ 2 ]] ]] }, { Floor[ d/2 ], Ceiling[ d/2 ] } ] & /@ kept );
    { InfraPoint[ Keys @ midpoints, Values @ midpoints ], Sort @ arms }
  ]

equidistantShellPoints[ graph_Graph, vs_List ] :=
  With[ { dm  = GraphDistanceMatrix[ graph ],
          idx = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount @ graph ] },
    { rows = Lookup[ idx, vs ] },
    { centers = Select[ VertexList[ graph ],
        c |-> With[ { ds = dm[[ idx @ c, rows ]] },
          SameQ @@ ds && First[ ds ] =!= Infinity && First[ ds ] > 0 ] ] },
    { InfraPoint[ centers ],
      Sort @ DeleteDuplicates[ ( dm[[ idx @ #, First @ rows ]] & ) /@ centers ] } ]

(* Midpoints of the chord {a, b}: vertices on some a-b geodesic at the
   middle distance(s) Floor[d/2] / Ceiling[d/2] -- the union over all
   geodesics, computed directly from the distance matrix. *)

shellMidpointVertices[ dm_, idx_, { a_, b_ } ] :=
  With[ { ia = idx @ a, ib = idx @ b },
    With[ { d = dm[[ ia, ib ]], verts = Keys @ idx },
      With[ { half = { Floor[ d/2 ], Ceiling[ d/2 ] } },
        Pick[ verts,
          MapThread[ #1 + #2 == d && MemberQ[ half, #1 ] &, { dm[[ ia ]], dm[[ All, ib ]] } ],
          True ] ] ] ]


(* ===================== InfraShellQ ===================== *)

(* vs is a metric shell iff some vertex c is equidistant from every
   vertex of vs at a common finite radius r, and vs is exactly the level
   set { v : d(c, v) == r }. *)

InfraShellQ[ graph_Graph, vs_List ] :=
  AnyTrue[ VertexList[ graph ],
    c |-> With[ { ds = GraphDistance[ graph, c, # ] & /@ vs },
      SameQ @@ ds && First[ ds ] =!= Infinity &&
      Sort @ Select[ VertexList[ graph ], GraphDistance[ graph, c, # ] === First[ ds ] & ] === Sort[ vs ]
    ] ]


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
