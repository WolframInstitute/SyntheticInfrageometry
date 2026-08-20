Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== InfraShell wrapper ===================== *)

(* InfraShell[{set}] is the unary form; InfraShell[{set1, ..., setk}] is the
   multi-realisation form.  Set canonicalisation and the shared accessors
   come from defineInfraBundleRules (Tools.wl). *)

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
       Properties.  Automatic (default) reads the count: All is the
       exhaustive top-down BFS over the peel-DAG, deduplicated, and a
       bounded count is the lazy peel.  The nested form {"Exhaustive",
       "Pruning" -> spec} caps per-layer branching via applyPruning.
       "Greedy" is the top-down DFS, backtracking at each leaf, first
       `count` minimals; "RandomGreedy" draws random peels instead
       (seed via ambient SeedRandom).
   When Properties is empty, Method is ignored. *)

FindInfraShell::badmethod   = "Method `1` is not supported by FindInfraShell.";
FindInfraShell::badproperty = "Property `1` is not supported by FindInfraShell.";
FindInfraShell::shortfall   = "\"RandomGreedy\" drew `1` distinct shells of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";

Options[ FindInfraShell ] = {
  Properties -> { },
  Method     -> Automatic
};

FindInfraShell[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraShell, count,
    { p0, r0 } |-> Module[ { properties, methodSpec, methodHead, pruning, range, localG, levelSet, radius, admissible },
      properties = OptionValue[ FindInfraShell, { opts }, Properties ];
      methodSpec = resolveMethod[ OptionValue[ FindInfraShell, { opts }, Method ], count ];
      methodHead = methodName @ methodSpec;
      pruning    = Replace[ methodSpec,
                    { { "Exhaustive", subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ),
                      _ :> Infinity } ];
      range = Replace[ r0, d_?NumericQ :> { d, d } ];
      localG = If[ NumericQ[ range[[ 2 ]] ],
                   NeighborhoodGraph[ graph, p0, Ceiling[ range[[ 2 ]] ] + 1 ], graph ];
      levelSet = Select[ VertexList[ localG ],
        range[[ 1 ]] <= GraphDistance[ localG, p0, # ] <= range[[ 2 ]] & ];
      radius = If[ NumericQ[ r0 ], r0, Mean[ r0 ] ];
      If[ properties === { },
        { levelSet },
        Catch[
          admissible = admissibleShell[ localG, p0, radius, properties ];
          Switch[ methodHead,
            "Exhaustive",   findAllMinimalAdmissible[ localG, levelSet, admissible, pruning ],
            "Greedy",       findGreedyMinimalAdmissible[ localG, levelSet, admissible, count ],
            "RandomGreedy", randomDraws[
              { } |-> findGreedyMinimalAdmissible[ localG, levelSet, admissible, 1, randomBranch ],
              count, FindInfraShell ],
            _,              Message[ FindInfraShell::badmethod, methodSpec ]; $Failed
          ]
        ]
      ]
    ], p, r ]


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
   FindInfraShell.  path is a vertex list or any InfraWalk wrapper
   (multi-realisation paths are spread and unioned). *)

Options[ FindInfraOsculatingShell ] = Options[ FindInfraShell ];

FindInfraOsculatingShell[ graph_Graph, path_, i_Integer, k_Integer,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[ ] ] :=
  Module[ { walks, vlist, vidx, dm, pairs, sets, capped },
    walks = infraSpread @ If[ ListQ @ path, InfraWalk[ { path } ], path ];
    vlist = VertexList @ graph;
    vidx  = AssociationThread[ vlist -> Range @ Length @ vlist ];
    dm    = GraphDistanceMatrix @ graph;
    pairs = SortBy[
      DeleteDuplicates @ Flatten[
        Map[
          walk |-> With[ {
              lo = Clip[ i - Floor[ ( k - 1 ) / 2 ], { 1, Length @ walk } ],
              hi = Clip[ i + Ceiling[ ( k - 1 ) / 2 ], { 1, Length @ walk } ] },
            { cols = Lookup[ vidx, walk[[ lo ;; hi ]] ] },
            MapThread[
              If[ SameQ @@ #2, { #1, First @ #2 }, Nothing ] &,
              { vlist, dm[[ All, cols ]] } ]
          ],
          walks ],
        1 ],
      { Last, First } ];
    sets = Flatten[
      ( FindInfraShell[ graph, #[[ 1 ]], #[[ 2 ]], All, opts ][ "Realizations" ] & ) /@ pairs,
      1 ];
    bundleTake[ InfraShell, sets, count ]
  ]


(* ===================== FindInfraShellCenter ===================== *)

(* Center / radius of a metric shell, two methods.  Both return a list
   of estimates { InfraMesoPoint[support, masses], r }, one per radius r,
   sorted ascending.

   Method -> "MaximalChordsBisectors" (default): bisect the shell's
   longest chords and bin the midpoints by radius -- within a radius,
   mass = how many chords bisect at that vertex, heaviest support vertex
   = best center.  An even chord contributes one estimate at r = d/2; an
   odd chord splits into r = Floor[d/2] (vertices nearer the lower
   endpoint) and r = Ceil[d/2].  Sub-options "Maximality" ("PerVertex" (default): each
   vertex's own farthest shell partner(s) | "Diameter": only the globally
   longest chords), "Distance" ("Extrinsic" (default) | "Intrinsic":
   antipodes by shell-subgraph distance -- "Intrinsic" is empty on a
   genuine sphere, whose vertices induce an edgeless subgraph, and is
   meaningful only on a connected shell; midpoints/parity/arms always use
   ambient distance), "Parity" (All (default) | "Even" pure single-vertex
   bisectors | "Odd" two-vertex mesopoints).

   Method -> "EquidistantPoints": the exact centers -- every vertex
   equidistant from the whole shell at a common finite radius > 0,
   grouped into one unweighted-InfraPoint estimate per radius. *)

FindInfraShellCenter::badmethod     = "Method `1` is not MaximalChordsBisectors or EquidistantPoints.";
FindInfraShellCenter::badmaximality = "Maximality `1` is not PerVertex or Diameter.";
FindInfraShellCenter::baddistance   = "Distance `1` is not Extrinsic or Intrinsic.";
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
  Module[ { maximality, distance, parity, dm, idx, dsel, chords, kept, radiiBins },
    maximality = Lookup[ mopts, "Maximality", "PerVertex" ];
    distance   = Lookup[ mopts, "Distance", "Extrinsic" ];
    parity     = Lookup[ mopts, "Parity", All ];
    Switch[ maximality, "PerVertex" | "Diameter", Null, _, Message[ FindInfraShellCenter::badmaximality, maximality ]; Return[ $Failed ] ];
    Switch[ distance, "Extrinsic" | "Intrinsic", Null, _, Message[ FindInfraShellCenter::baddistance, distance ]; Return[ $Failed ] ];
    Switch[ parity, All | "Even" | "Odd", Null, _, Message[ FindInfraShellCenter::badparity, parity ]; Return[ $Failed ] ];
    dm  = GraphDistanceMatrix[ graph ];
    idx = AssociationThread[ VertexList[ graph ] -> Range @ VertexCount @ graph ];
    dsel = If[ distance === "Intrinsic",
               With[ { subg = Subgraph[ graph, vs ] },
                 { rows = Lookup[ AssociationThread[ VertexList[ subg ] -> Range @ VertexCount @ subg ], vs ] },
                 GraphDistanceMatrix[ subg ][[ rows, rows ]] ],
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
              { ecc = Max @ Select[ row, # =!= Infinity & ] },
              { vs[[ i ]], # } & /@ Pick[ vs, Thread[ row == ecc ], True ] ],
            { i, Length[ vs ] } ], 1 ] ] ];
    kept = Select[ chords,
      With[ { d = dm[[ idx @ #[[ 1 ]], idx @ #[[ 2 ]] ]] },
        Switch[ parity, All, True, "Even", EvenQ[ d ], "Odd", OddQ[ d ] ] ] & ];
    radiiBins = GroupBy[ Catenate[ chordMidpointRadii[ dm, idx, # ] & /@ kept ], Last -> First ];
    KeyValueMap[ { r, vlist } |-> With[ { ct = Counts @ vlist }, { InfraMesoPoint[ ct ], r } ], KeySort @ radiiBins ]
  ]

(* Centers equidistant from vs, binned by their common radius r = d(c, vs) > 0,
   each bin an InfraPoint.  The equidistant locus is FindInfraEquidistantSet;
   here it is split into nonempty positive-radius shells. *)

equidistantShellPoints[ graph_Graph, vs_List ] :=
  With[ { ds = AssociationThread[ VertexList[ graph ], GraphDistance[ graph, First @ vs ] ] },
    { centers = Select[ FindInfraEquidistantSet[ graph, vs ][ "Vertices" ], c |-> 0 < ds[ c ] < Infinity ] },
    KeyValueMap[ { r, cs } |-> { InfraMesoPoint[ cs, ConstantArray[ 1, Length @ cs ] ], r }, KeySort @ GroupBy[ centers, ds ] ] ]

(* Midpoint incidences { v, r } of the chord {a, b} (a = lower endpoint):
   vertices v on some a-b geodesic at a middle distance r = d(a, v) in
   { Floor[d/2], Ceil[d/2] }.  An even chord yields one r = d/2; an odd
   chord splits into r = Floor (vertices nearer a) and r = Ceil (nearer b). *)

chordMidpointRadii[ dm_, idx_, chord_ ] :=
  With[ { ab = Sort @ chord, verts = Keys @ idx },
    { ia = idx @ ab[[ 1 ]], ib = idx @ ab[[ 2 ]] },
    { d = dm[[ ia, ib ]] },
    { half = { Floor[ d/2 ], Ceiling[ d/2 ] } },
    Cases[ Transpose @ { verts, dm[[ ia ]], dm[[ All, ib ]] },
      { v_, da_, db_ } /; da + db == d && MemberQ[ half, da ] :> { v, da } ] ]


(* ===================== InfraShellQ ===================== *)

(* vs is a metric shell iff some vertex c is equidistant from every
   vertex of vs at a common finite radius r, and vs is exactly the level
   set { v : d(c, v) == r }. *)

InfraShellQ[ graph_Graph, s : _InfraShell | _InfraSet ] :=
  AllTrue[ If[ Head[ s ] === InfraSet, { First @ s }, First @ s ], InfraShellQ[ graph, # ] & ]

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
      FindInfraShell[ graph, center, r, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraShell ] ] ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Center" -> center,
                "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]
