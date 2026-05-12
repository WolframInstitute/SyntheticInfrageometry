Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[toVertexSet]
PackageScope[infraVertexSet]
PackageScope[topologicalLevels]
PackageScope[resolveExpression]
PackageScope[extractBranches]
PackageScope[capBranches]
PackageScope[applySelectOption]
PackageScope[constructionPatternQ]
PackageScope[dispatchConstruction]
PackageScope[evaluateConstruction]
PackageScope[evaluateStep]


(* ===================== Helpers ===================== *)

(* Atomic-vs-list normalization: a single vertex becomes a singleton list. *)
toVertexSet[ v_ ] /; AtomQ[ v ] := { v }
toVertexSet[ vs_List ] := vs

(* Topological levels of a DAG; each level is the current source-vertex set. *)
topologicalLevels[ dag_Graph ] :=
  Module[ { remaining = VertexList @ dag, levels = {} },
    While[ remaining =!= {},
      With[ { current = Select[ remaining,
          v |-> VertexInDegree[ Subgraph[ dag, remaining ], v ] == 0 ] },
        AppendTo[ levels, current ];
        remaining = Complement[ remaining, current ] ] ];
    levels
  ]

(* Substitute scene-object bindings into an expression, then resolve
   InfraDistance / InfraQ heads against the graph. *)
resolveExpression[ expr_, bindings_Association, graph_Graph ] :=
  ( expr /. Normal[ bindings ] ) /.
    { InfraDistance[ x_, y_ ]      :> GraphDistance[ graph, x, y ],
      InfraSegmentQ[ s_ ]          :> SegmentQ[ graph, s ],
      InfraShellQ[ vs_ ]           :> ShellQ[ graph, vs ],
      InfraPlaneQ[ h_, p1_, p2_ ]  :> SeparatesQ[ graph, h, p1, p2 ] &&
                                       AllTrue[ h, GraphDistance[ graph, p1, # ] ==
                                                    GraphDistance[ graph, p2, # ] & ],
      InfraCircleQ[ c_ ]           :> CircleQ[ graph, c ],
      InfraLineQ[ s_ ]             :> LineQ[ graph, s ],
      InfraParallelQ[ l1_, l2_ ]   :> ParallelQ[ graph, l1, l2 ],
      InfraIntersectQ[ s1_, s2_ ]  :> IntersectingQ[ s1, s2 ],
      InfraRevolutionQ[ vs_, axis_, profile_ ] :> RevolutionQ[ graph, vs, axis, profile ] }

extractBranches[ opts_List ] :=
  Lookup[ Association @ opts, "Branches", All ]

(* "Is this hypothesis a construction equation `key == rhs` whose key is one
   of the scene objects (or a list of them)?" Used to split hypotheses into
   constructions vs. assertions. *)
constructionPatternQ[ objects_List, h_ ] :=
  MatchQ[ h, ( key_ == _ ) /;
    ( MemberQ[ objects, key ] || ( ListQ[ key ] && SubsetQ[ objects, key ] ) ) ]

capBranches[ paths_List, All ]              := paths
capBranches[ paths_List, n_Integer ]        := Take[ paths, UpTo[ n ] ]
capBranches[ paths_List, UpTo[ n_Integer ] ] := Take[ paths, UpTo[ n ] ]
capBranches[ other_, _ ]                    := other

(* The "Select" hypothesis option accepts None, a criterion string, or a list
   thereof.  "EmbeddingClosest" routes to EmbeddingClosestPaths/Cycles using
   ctx ("Endpoints" for paths, "Center"+"Radius" for cycles); the legacy
   criterion strings translate into the new SelectPath / SelectCycle "From"
   pool spec with All count to preserve set-shaped semantics. *)
applySelectOption[ _Graph, paths_, None, _, _ ] := paths
applySelectOption[ graph_Graph, paths_, list_List, cyclic_, ctx_ ] :=
  Fold[ applySelectOption[ graph, #1, #2, cyclic, ctx ] &, paths, list ]
applySelectOption[ graph_Graph, paths_, "EmbeddingClosest", True,  ctx_ ] :=
  EmbeddingClosestCycles[ graph, paths, { ctx[ "Center" ], ctx[ "Radius" ] } ]
applySelectOption[ graph_Graph, paths_, "EmbeddingClosest", False, ctx_ ] :=
  EmbeddingClosestPaths[ graph, paths, ctx[ "Endpoints" ] ]
applySelectOption[ graph_Graph, paths_, name_String, True,  _ ] :=
  SelectCycle[ graph, paths, All, "From" -> selectFromName[ name ] ]
applySelectOption[ graph_Graph, paths_, name_String, False, _ ] :=
  SelectPath[ graph, paths, All, "From" -> selectFromName[ name ] ]

selectFromName[ "Central"    ] := "Center"
selectFromName[ "Peripheral" ] := "Periphery"
selectFromName[ name_String  ] := name


(* ===================== InfraDistance ===================== *)

(* infraVertexSet -- collect the underlying vertex set from any Infra* head
   or bare vertex.  Path realisations (InfraSegment / InfraLine / InfraRay /
   InfraCircle) and set realisations (InfraShell / InfraPlane) both flatten
   under Union @@ to the same vertex-set form needed for distance.  Bare
   vertices fall through to the singleton case. *)

infraVertexSet[ InfraPoint[ vs_List ] ] := vs
infraVertexSet[ ( InfraSegment | InfraLine | InfraRay | InfraCircle
                | InfraShell | InfraPlane )[ reps_List ] ] :=
  Union @@ reps
infraVertexSet[ list_List ] /;
    list =!= { } && AllTrue[ list,
      MatchQ[ ( InfraPoint | InfraSegment | InfraLine | InfraRay |
                InfraCircle | InfraShell | InfraPlane )[ { _ } ] ] ] :=
  infraVertexSet[ Head[ First @ list ] @ ( #[[ 1, 1 ]] & /@ list ) ]
infraVertexSet[ v_ ] := { v }


(* InfraDistance[g, p, q] -- graph distance between two arguments, each of
   which can be a bare vertex or any Infra* wrapper.  Aggregates pairwise
   GraphDistance over the cross-product of underlying vertex sets via the
   "Aggregation" option (Min by default = the infra-observer's nearest
   reading; Max = diameter; Mean / any List -> Number function also work). *)

Options[ InfraDistance ] = { "Aggregation" -> Min }

InfraDistance[ g_Graph, p_, q_, OptionsPattern[] ] :=
  OptionValue[ "Aggregation" ] @
    Flatten @ Outer[ GraphDistance[ g, #1, #2 ] &,
      infraVertexSet[ p ], infraVertexSet[ q ], 1 ]


(* ===================== Scene ===================== *)

(* Manual-step form: hypotheses contain explicit InfraGeometricStep blocks. *)
InfraScene[ objects_List, hypotheses_List ] /;
  MemberQ[ hypotheses, _InfraGeometricStep ] :=
  Module[ { gSteps, perStep, constructions, steps, labels, assertions },

    gSteps = Cases[ hypotheses, _InfraGeometricStep ];

    perStep = Map[
      gStep |-> With[ { hyps = gStep[[ 1 ]] },
        <| "Constructions" -> Association @ Cases[ hyps,
              ( key_ == rhs_ ) /; constructionPatternQ[ objects, key == rhs ] :> ( key -> rhs ) ],
           "Assertions" -> Select[ hyps, ! constructionPatternQ[ objects, # ] & ],
           "Label"      -> If[ Length @ gStep >= 2, gStep[[ 2 ]], None ] |> ],
      gSteps ];

    constructions = Join @@ ( #[ "Constructions" ] & /@ perStep );
    steps  = Flatten[ If[ ListQ @ #, #, { # } ] & /@ Keys @ #[ "Constructions" ] ] & /@ perStep;
    labels = #[ "Label" ] & /@ perStep;
    assertions = Join[
      Select[ hypotheses,
        h |-> ! MatchQ[ h, _InfraGeometricStep ] && ! constructionPatternQ[ objects, h ] ],
      Flatten[ #[ "Assertions" ] & /@ perStep ] ];

    InfraScene[ <|
      "Objects"         -> objects,
      "Constructions"   -> constructions,
      "Assertions"      -> assertions,
      "DependencyGraph" -> None,
      "Steps"           -> steps,
      "Labels"          -> labels,
      "ManualSteps"     -> True
    |> ]
  ]

(* Auto-step form: hypotheses are bare constructions and assertions; steps are
   the topological levels of the dependency DAG. *)
InfraScene[ objects_List, hypotheses_List ] :=
  Module[ { constructions, assertions, dag, steps },
    constructions = Association @ Cases[ hypotheses,
      ( key_ == rhs_ ) /; constructionPatternQ[ objects, key == rhs ] :> ( key -> rhs ) ];
    assertions = Select[ hypotheses, ! constructionPatternQ[ objects, # ] & ];
    dag = Graph[ objects,
      Flatten @ KeyValueMap[
        { key, rhs } |-> With[ {
            deps    = Intersection[ Cases[ rhs, Alternatives @@ objects, Infinity ], objects ],
            targets = If[ ListQ @ key, key, { key } ] },
          DirectedEdge[ #1, #2 ] & @@@ Tuples[ { deps, targets } ] ],
        constructions ],
      DirectedEdges -> True ];
    steps = topologicalLevels[ dag ];
    InfraScene[ <|
      "Objects"         -> objects,
      "Constructions"   -> constructions,
      "Assertions"      -> assertions,
      "DependencyGraph" -> dag,
      "Steps"           -> steps,
      "Labels"          -> ConstantArray[ None, Length @ steps ],
      "ManualSteps"     -> False
    |> ]
  ]

InfraScene[ data_Association ][ prop_String ] := data[ prop ]


(* ===================== Instance accessors =====================
   InfraInstance[bindings] is the wrapper returned by FindInfraScene.
   The two-argument forms read out one or several bindings; bare
   instance[[1]] continues to work. *)

InfraInstance[ inst_InfraInstance, sym_ ] /; ! ListQ[ sym ] :=
  inst[[ 1 ]][ sym ]

InfraInstance[ inst_InfraInstance, syms_List ] :=
  inst[[ 1 ]] /@ syms

InfraInstance[ bindings_Association, sym_ ] /; ! ListQ[ sym ] :=
  bindings[ sym ]

InfraInstance[ bindings_Association, syms_List ] :=
  bindings /@ syms


(* ===================== Construction Dispatch ===================== *)

(* Each clause maps an InfraHead expression with bindings already substituted
   into its concrete graph realization (vertex, vertex set, vertex sequence,
   list thereof). For multi-valued constructions (segments, lines, spheres),
   the full enumeration is computed first, then the path-space selector named
   by "Select" is applied, and finally the result is capped to "Branches". *)

dispatchConstruction[ graph_Graph, InfraPoint[] ] :=
  VertexList @ graph

dispatchConstruction[ graph_Graph, InfraPoint[ v_ ] ] /; MemberQ[ VertexList @ graph, v ] :=
  { v }

dispatchConstruction[ graph_Graph, InfraPoint[ pool_String ] ] :=
  Switch[ pool,
    "Center",    GraphCenter[ graph ],
    "Periphery", GraphPeriphery[ graph ],
    _,           VertexList @ graph
  ]

dispatchConstruction[ graph_Graph, InfraPoint[ origin_, dist_ ] ] /; ! MatchQ[ dist, _Rule ] :=
  Union @ Flatten @ Map[
    o |-> Select[ VertexList @ graph, v |-> GraphDistance[ graph, o, v ] == dist ],
    If[ StringQ @ origin,
      Switch[ origin,
        "Center",    GraphCenter[ graph ],
        "Periphery", GraphPeriphery[ graph ],
        _,           VertexList @ graph ],
      If[ MemberQ[ VertexList @ graph, origin ], { origin }, origin ] ] ]

dispatchConstruction[ graph_Graph, InfraPoint[ n_Integer, opts___Rule ] ] :=
  Module[ { dist, distMatrix, finiteMax, dMin, dMax, auxGraph, cliques },
    dist          = "Distance" /. { opts } /. "Distance" -> "Max";
    distMatrix    = GraphDistanceMatrix @ graph;
    finiteMax     = Max @ Select[ Flatten @ distMatrix, # < Infinity & ];
    { dMin, dMax } = Switch[ dist,
      "Max", { finiteMax, finiteMax },
      _List, dist /. Infinity -> finiteMax,
      _,     { dist, finiteMax } ];
    auxGraph = Graph[ VertexList @ graph,
      UndirectedEdge @@@ Select[ Subsets[ VertexList @ graph, { 2 } ],
        pair |-> With[ { d = GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] },
          dMin <= d <= dMax ] ] ];
    cliques = Select[ FindClique[ auxGraph, { n, VertexCount @ auxGraph }, All ],
      Length @ # >= n & ];
    If[ cliques === {}, {}, RandomSample[ #, n ] & /@ cliques ]
  ]

dispatchConstruction[ graph_Graph, InfraSegment[ p1_, p2_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindSegment[ graph, p1, p2, All ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraLine[ path_List, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      findLineExtensions[ graph, path ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { First @ path, Last @ path } |> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraLine[ p1_, p2_, opts___Rule ] ] /;
  MemberQ[ VertexList @ graph, p1 ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindLine[ graph, p1, p2, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindLine ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraShell[ center_, r_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindShell[ graph, center, r, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindShell ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Center" -> center,
                "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraCircle[ center_, r_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindCircle[ graph, center, r, All ],
      "Select" /. { opts } /. "Select" -> None,
      True, <| "Center" -> center,
               "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraPlane[ p1_, p2_, opts___Rule ] ] :=
  dispatchConstruction[ graph, InfraPlane[ p1, p2, { 0, 0 }, opts ] ]

dispatchConstruction[ graph_Graph, InfraPlane[ p1_, p2_,
    window : { _Integer, _Integer }, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindBisectingHyperplane[ graph, p1, p2, window, All ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]

dispatchConstruction[ graph_Graph, InfraRevolution[ axis_, profile_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      #[[ 1, 1 ]] & /@ FindRevolution[ graph, axis, profile, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindRevolution ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Axis" -> axis, "Profile" -> profile |> ],
    extractBranches[ { opts } ] ]


(* ===================== Evaluation Engine ===================== *)

(* Each call extends every input branch by the multi-spread of one step:
   one symbol bound to each result, or a tuple of symbols thread-bound to each
   tuple-result. *)

evaluateConstruction[ graph_Graph, sym_, InfraIntersection[ obj1_, obj2_ ], bindings_Association ] :=
  Append[ bindings, sym -> # ] & /@
    Intersection[
      toVertexSet @ resolveExpression[ obj1, bindings, graph ],
      toVertexSet @ resolveExpression[ obj2, bindings, graph ] ]

evaluateConstruction[ graph_Graph, sym_, rhs_, bindings_Association ] :=
  With[ { results = dispatchConstruction[ graph, resolveExpression[ rhs, bindings, graph ] ] },
    If[ results === {} || results === {{}}, {},
      Append[ bindings, sym -> # ] & /@ results ] ]

evaluateConstruction[ graph_Graph, syms_List, rhs_, bindings_Association ] :=
  With[ { tuples = dispatchConstruction[ graph, resolveExpression[ rhs, bindings, graph ] ] },
    If[ tuples === {}, {},
      Join[ bindings, AssociationThread[ syms, # ] ] & /@ tuples ] ]

(* One construction step over a list of branches. Tuple-keyed constructions
   (sym lists) are evaluated last so their parts are not double-bound. *)
evaluateStep[ graph_Graph, step_List, constructions_Association, branches_List ] :=
  With[ { tupleKeys = Select[ Keys @ constructions, ListQ ] },
    With[ {
        tuplesInStep = Select[ tupleKeys, ContainsAny[ #, step ] & ],
        tupleParts   = Flatten @ Select[ tupleKeys, ContainsAny[ #, step ] & ] },
      Fold[
        { currentBranches, key } |->
          Flatten[ evaluateConstruction[ graph, key, constructions[ key ], # ] & /@
            currentBranches, 1 ],
        branches,
        Join[
          Select[ Complement[ step, tupleParts ], KeyExistsQ[ constructions, # ] & ],
          tuplesInStep ] ] ] ]


(* ===================== FindInfraScene ===================== *)

Options[ FindInfraScene ] = { "PruneProbability" -> 0 };

FindInfraScene[ scene_InfraScene, graph_Graph, opts : OptionsPattern[] ] :=
  FindInfraScene[ scene, graph, Length @ scene[ "Steps" ], <||>, opts ]

FindInfraScene[ scene_InfraScene, graph_Graph, nSteps_Integer, opts : OptionsPattern[] ] :=
  FindInfraScene[ scene, graph, nSteps, <||>, opts ]

FindInfraScene[ scene_InfraScene, graph_Graph, init_Association, opts : OptionsPattern[] ] :=
  FindInfraScene[ scene, graph, Length @ scene[ "Steps" ], init, opts ]

FindInfraScene[ scene_InfraScene, graph_Graph, nSteps_Integer, init_Association,
    opts : OptionsPattern[] ] :=
  Module[ { branches = { init }, prob = OptionValue[ "PruneProbability" ],
            objects = scene[ "Objects" ] },
    Do[
      With[ { effective = Select[ step,
          ! KeyExistsQ[ First[ branches, <||> ], # ] & ] },
        If[ effective =!= {},
          branches = evaluateStep[ graph, effective, scene[ "Constructions" ], branches ];
          If[ prob > 0,
            branches = With[ { kept = Pick[ branches,
                UnitStep[ RandomReal[ { 0, 1 }, Length @ branches ] - prob ], 1 ] },
              If[ kept === {}, { RandomChoice @ branches }, kept ] ] ] ] ],
      { step, Take[ scene[ "Steps" ], UpTo[ nSteps ] ] } ];
    InfraInstance /@ If[ scene[ "Assertions" ] === {}, branches,
      Select[ branches, b |-> And @@ (
        With[ { vars = Intersection[
              Cases[ #, Alternatives @@ objects, { 0, Infinity } ], objects ] },
          ! SubsetQ[ Keys @ b, vars ] ||
            TrueQ[ resolveExpression[ #, b, graph ] ] ] & /@ scene[ "Assertions" ] ) ] ]
  ]
