Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[toVertexSet]
PackageScope[infraVertexSet]
PackageScope[sceneAssertionRules]
PackageScope[resolveExpression]
PackageScope[extractBranches]
PackageScope[capBranches]
PackageScope[applySelectOption]
PackageScope[constructionPatternQ]
PackageScope[dispatchConstruction]
PackageScope[evaluateConstruction]


(* ===================== Helpers ===================== *)

(* Atomic-vs-list normalization: a single vertex becomes a singleton list. *)
toVertexSet[ v_ ] /; AtomQ[ v ] := { v }
toVertexSet[ vs_List ] := vs

(* Every scene assertion, as (inert user-facing form :> the named predicate the
   graph is injected into).  resolveExpression rewrites through this table and
   the guard below decides admissibility by it, so neither a head nor an arity
   can be accepted without a rule that decides it. *)
sceneAssertionRules[ graph_ ] :=
  { InfraDistance[ x_, y_ ]      :> GraphDistance[ graph, x, y ],
    InfraWalkQ[ w_ ]             :> InfraWalkQ[ graph, w ],
    InfraSegmentQ[ s_ ]          :> InfraSegmentQ[ graph, s ],
    InfraShellQ[ vs_ ]           :> InfraShellQ[ graph, vs ],
    InfraBallQ[ vs_ ]            :> InfraBallQ[ graph, vs ],
    InfraPlaneQ[ h_, p1_, p2_ ]  :> InfraPlaneQ[ graph, h, p1, p2 ],
    InfraCircleQ[ c_ ]           :> InfraCircleQ[ graph, c ],
    InfraLineQ[ s_ ]             :> InfraLineQ[ graph, s ],
    InfraParallelQ[ l1_, l2_ ]   :> InfraParallelQ[ graph, l1, l2 ],
    InfraIntersectQ[ s1_, s2_ ]  :> IntersectingQ[ s1, s2 ],
    InfraPolylineQ[ poly_ ]      :> InfraPolylineQ[ graph, poly ],
    InfraRegularPolygonQ[ c_, as_ ] :> InfraRegularPolygonQ[ graph, c, as ],
    InfraRevolutionQ[ vs_, axis_, profile_ ] :> InfraRevolutionQ[ graph, vs, axis, profile ] }

(* Substitute scene-object bindings into an expression, then resolve
   InfraDistance / InfraQ heads against the graph. *)
resolveExpression[ expr_, bindings_Association, graph_Graph ] :=
  ( expr /. Normal[ bindings ] ) /. sceneAssertionRules[ graph ]

(* An Infra*Q subexpression with no rule in that table never receives the graph,
   so it stays inert, TrueQ reads it as False, and the scene silently rejects
   every branch.  Matching the whole shape rather than the head catches a known
   head at an unsupported arity too: InfraSegmentQ[s, extra] misses its rule and
   fails exactly as invisibly as an unexported head.  Only the left sides are
   read, so the graph the table is built with is immaterial. *)
undecidableAssertions[ assertions_List ] :=
  With[ { decidable = Alternatives @@ Keys @ sceneAssertionRules[ Null ] },
    DeleteDuplicates @ Cases[ assertions,
      token : head_Symbol[ ___ ] /;
        StringMatchQ[ SymbolName[ head ], "Infra" ~~ ___ ~~ "Q" ] &&
          ! MatchQ[ token, decidable ],
      { 0, Infinity } ] ]

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
   thereof.  "EmbeddingClosest" routes to EmbeddingClosest using ctx
   ("Endpoints" for paths, "Center"+"Radius" for cycles); the legacy
   criterion strings translate into the new SelectInfraWalk "From" pool spec
   with All count to preserve set-shaped semantics. *)
applySelectOption[ _Graph, paths_, None, _, _ ] := paths
applySelectOption[ graph_Graph, paths_, list_List, cyclic_, ctx_ ] :=
  Fold[ applySelectOption[ graph, #1, #2, cyclic, ctx ] &, paths, list ]
applySelectOption[ graph_Graph, paths_, "EmbeddingClosest", True,  ctx_ ] :=
  EmbeddingClosest[ graph, paths, { ctx[ "Center" ], ctx[ "Radius" ] } ]
applySelectOption[ graph_Graph, paths_, "EmbeddingClosest", False, ctx_ ] :=
  EmbeddingClosest[ graph, paths, ctx[ "Endpoints" ] ]
applySelectOption[ graph_Graph, paths_, name_String, True,  _ ] :=
  SelectInfraWalk[ graph, paths, All, "From" -> selectFromName[ name ], "Cyclic" -> True ]
applySelectOption[ graph_Graph, paths_, name_String, False, _ ] :=
  SelectInfraWalk[ graph, paths, All, "From" -> selectFromName[ name ] ]

selectFromName[ "Central"    ] := "Center"
selectFromName[ "Peripheral" ] := "Periphery"
selectFromName[ name_String  ] := name


(* ===================== InfraDistance ===================== *)

(* infraVertexSet -- collect the underlying vertex set from any Infra* head
   or bare vertex.  Path realisations (InfraSegment / InfraLine / InfraRay /
   InfraCircle / InfraEllipse) and set realisations (InfraShell / InfraPlane /
   InfraEllipticShell / InfraBall) both flatten under Union @@ to the same
   vertex-set form needed for distance.  InfraPolyline realisations are
   multi-leg sequences flattened by polylineToVertexSeqs first.  Bare
   vertices fall through to the singleton case. *)

infraVertexSet[ InfraPoint[ v_ ] ] := { v }
infraVertexSet[ list : { __InfraPoint } ] := DeleteDuplicates[ #[[ 1 ]] & /@ list ]
infraVertexSet[ InfraEffectivePoint[ m_Association ] ] := Keys @ m
infraVertexSet[ ( InfraObject | InfraSet )[ vs_List ] ] := vs
infraVertexSet[ ( InfraSegment | InfraWalk | InfraLoop | InfraString | InfraLine | InfraRay
                | InfraCircle | InfraEllipse
                | InfraShell | InfraEllipticShell | InfraPlane | InfraBall )[ reps_List ] ] :=
  Union @@ reps
infraVertexSet[ InfraSegment[ dag_Graph ] ] := VertexList[ dag ]
infraVertexSet[ ( InfraPolyline | InfraPolygon | InfraTriangle )[ reps_List ] ] :=
  Union @@ polylineToVertexSeqs[ reps ]
infraVertexSet[ list_List ] /;
    list =!= { } && AllTrue[ list,
      MatchQ[ ( InfraPoint | InfraSegment | InfraWalk | InfraLoop | InfraString | InfraLine | InfraRay |
                InfraCircle | InfraEllipse | InfraShell | InfraEllipticShell | InfraPlane | InfraBall |
                InfraPolyline | InfraPolygon | InfraTriangle | InfraObject | InfraSet )[ { _ } ] ] ] :=
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


(* ===================== InfraIntersection / InfraUnion ===================== *)

(* Standalone vertex-set intersection / union across any number of Infra*
   objects.  Each object contributes its full vertex set (union across
   realisations, via infraVertexSet).  Guarded on the *realisation* shape --
   a single list payload -- not merely on the head: InfraCircle[c, r] is a
   scene constructor whose vertex set is unknown until dispatched, and
   matching it here collapsed scene hypotheses to InfraSet[{}] at
   scene-construction time. *)

$infraRealisationPattern =
  ( InfraPoint | InfraObject | InfraSet | InfraSegment | InfraWalk | InfraLoop |
    InfraString | InfraLine | InfraRay | InfraCircle | InfraEllipse | InfraShell |
    InfraEllipticShell | InfraPlane | InfraBall | InfraPolyline | InfraPolygon |
    InfraTriangle )[ _List ] | InfraSegment[ _Graph ] | InfraEffectivePoint[ _Association ];

InfraIntersection[ args__ ] /; AllTrue[ { args }, MatchQ[ $infraRealisationPattern ] ] :=
  InfraSet[ Intersection @@ ( infraVertexSet /@ { args } ) ]

InfraUnion[ args__ ] /; AllTrue[ { args }, MatchQ[ $infraRealisationPattern ] ] :=
  InfraSet[ Union @@ ( infraVertexSet /@ { args } ) ]


(* ===================== Scene ===================== *)

InfraScene::badassertion = "`1` is not a scene assertion the graph can be \
injected into; it stays inert, so the scene would reject every branch without a \
message.";

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

    With[ { undecidable = undecidableAssertions[ assertions ] },
      If[ undecidable =!= { },
        Message[ InfraScene::badassertion, First @ undecidable ];
        Return[ $Failed, Module ] ] ];

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
  Module[ { constructions, assertions, dag, steps = { }, remaining },
    constructions = Association @ Cases[ hypotheses,
      ( key_ == rhs_ ) /; constructionPatternQ[ objects, key == rhs ] :> ( key -> rhs ) ];
    assertions = Select[ hypotheses, ! constructionPatternQ[ objects, # ] & ];
    With[ { undecidable = undecidableAssertions[ assertions ] },
      If[ undecidable =!= { },
        Message[ InfraScene::badassertion, First @ undecidable ];
        Return[ $Failed, Module ] ] ];
    dag = Graph[ objects,
      Flatten @ KeyValueMap[
        { key, rhs } |-> With[ {
            deps    = Intersection[ Cases[ rhs, Alternatives @@ objects, Infinity ], objects ],
            targets = If[ ListQ @ key, key, { key } ] },
          DirectedEdge[ #1, #2 ] & @@@ Tuples[ { deps, targets } ] ],
        constructions ],
      DirectedEdges -> True ];
    (* topological levels of the dependency DAG: each step is the current source set *)
    remaining = VertexList @ dag;
    While[ remaining =!= { },
      With[ { current = Select[ remaining,
          v |-> VertexInDegree[ Subgraph[ dag, remaining ], v ] == 0 ] },
        AppendTo[ steps, current ];
        remaining = Complement[ remaining, current ] ] ];
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

(* dispatchConstruction[graph, Head[args]] maps an InfraHead expression with
   bindings already substituted into its concrete graph realization (vertex,
   vertex set, vertex sequence, list thereof).  Per-primitive scene-DSL rules
   live in their respective Infra*.wl files; helpers capBranches /
   applySelectOption / extractBranches are PackageScope here. *)


(* ===================== Evaluation Engine ===================== *)

(* Each call extends every input branch by the multi-spread of one step:
   one symbol bound to each result, or a tuple of symbols thread-bound to each
   tuple-result. *)

(* Each operand is itself a construction (InfraCircle[a, r], ...), so it must
   be dispatched before its vertex set exists; an operand already bound to a
   vertex set has no dispatch rule and is read directly. *)
evaluateConstruction[ graph_Graph, sym_, InfraIntersection[ objs__ ], bindings_Association ] :=
  Append[ bindings, sym -> # ] & /@
    Intersection @@ Map[
      obj |-> With[ { resolved = resolveExpression[ obj, bindings, graph ] },
        { realisations = dispatchConstruction[ graph, resolved ] },
        If[ ListQ[ realisations ],
          Union @@ ( toVertexSet /@ realisations ),
          toVertexSet[ resolved ] ] ],
      { objs } ]

evaluateConstruction[ graph_Graph, sym_, rhs_, bindings_Association ] :=
  With[ { results = dispatchConstruction[ graph, resolveExpression[ rhs, bindings, graph ] ] },
    (* A construction that yields no result -- empty, or a dispatch with no
       matching rule (non-list) -- simply stops propagating this branch. *)
    If[ ! ListQ[ results ] || results === {} || results === {{}}, {},
      Append[ bindings, sym -> # ] & /@ results ] ]

evaluateConstruction[ graph_Graph, syms_List, rhs_, bindings_Association ] :=
  With[ { tuples = dispatchConstruction[ graph, resolveExpression[ rhs, bindings, graph ] ] },
    If[ ! ListQ[ tuples ] || tuples === {}, {},
      Join[ bindings, AssociationThread[ syms, # ] ] & /@ tuples ] ]

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
          (* tuple-keyed constructions (sym lists) are evaluated last so their
             parts are not double-bound *)
          branches = With[ { constructions = scene[ "Constructions" ] },
            { tuplesInStep = Select[ Select[ Keys @ constructions, ListQ ],
                ContainsAny[ #, effective ] & ] },
            Fold[
              { currentBranches, key } |->
                Flatten[ evaluateConstruction[ graph, key, constructions[ key ], # ] & /@
                  currentBranches, 1 ],
              branches,
              Join[
                Select[ Complement[ effective, Flatten @ tuplesInStep ],
                  KeyExistsQ[ constructions, # ] & ],
                tuplesInStep ] ] ];
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
