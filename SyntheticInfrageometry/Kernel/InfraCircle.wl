Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


(* ===================== InfraCircle wrapper ===================== *)

(* InfraCircle[{cycle}] is the unary form; InfraCircle[{cycle1, ..., cyclek}]
   is the multi-realisation form.  Set canonicalisation and the shared accessors
   come from defineInfraBundleRules (Tools.wl). *)

(* "Length" = circumference per realisation: k for a k-vertex open cycle
   (wrap-around edge implicit, so #edges == #vertices). *)
InfraCircle[ reps : Except[ { __Graph }, _List ] ][ "Length" ] := Length /@ reps

(* the enumerated form answers ["Multiplicity"] too, so the accessor does not
   depend on which of the two shapes FindInfraCircle happened to return *)
InfraCircle[ reps : Except[ { __Graph }, _List ] ][ "Multiplicity" ] := Length @ reps


(* ===== pool form: InfraCircle[{dag_Graph, ...}] ===== *)

(* The whole family of shortest separating cycles of a band, held compactly as
   one arc-folded geodesic DAG per pool atom: the seam arc s1, ..., sm as a
   forced chain s1 -> ... -> sm -> v, then the cut-shell geodesic interval
   v -> ... -> u, so the DAG's source -> sink paths are exactly that atom's
   circles and the closing edge u - s1 stays implicit as in every InfraCircle
   realisation.  Distinct atoms carry disjoint families, so the measure layer's
   per-slot DP (atomFamilySize / GeodesicOccupation, Tools.wl) already sums to
   the family's count and occupation -- the enumeration-free marginals, on a
   family too large to list.  ["Length"] is one number here rather than one per
   realisation: a pool's realisations are all tied at the minimum
   circumference.  See Wiki/Concepts/CirclePoolStructure.md. *)

InfraCircle[ dags : { __Graph } ][ "Graph" ]              := dags
InfraCircle[ dags : { __Graph } ][ "Vertices" ]           := Union @@ ( VertexList /@ dags )
InfraCircle[ dags : { __Graph } ][ "Length" ]             := 1 + Max @ Values @ dagLayers @ First @ dags
InfraCircle[ dags : { __Graph } ][ "Multiplicity" ]       := infraNumReps @ InfraCircle @ dags
InfraCircle[ dags : { __Graph } ][ "OccupationCount" ]    := infraVertexMultiset @ InfraCircle @ dags
InfraCircle[ dags : { __Graph } ][ "OccupationMeasure" ]  := InfraMeasure @ InfraCircle @ dags
InfraCircle[ dags : { __Graph } ][ "Measure" ]            := InfraMeasure @ InfraCircle @ dags
InfraCircle[ dags : { __Graph } ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraCircle @ dags, Method -> "Probability" ]
InfraCircle[ dags : { __Graph } ][ "Realizations" ]       := Catenate[ dagGeodesics /@ dags ]
InfraCircle[ dags : { __Graph } ][ "First" ]              := First @ dagGeodesics[ First @ dags, 1 ]

(* lazy: atoms are consumed in order, each stopping at the residual budget, so a
   bounded ask never enumerates a family it does not need *)
InfraCircle[ dags : { __Graph } ][ "Realizations", spec_ ] :=
  infraCap[
    Fold[ { acc, dag } |-> If[ Length @ acc >= countLimit @ spec, acc,
        Join[ acc, dagGeodesics[ dag, countLimit @ spec - Length @ acc ] ] ],
      { }, dags ],
    spec ]
(* ===================== FindInfraCircle ===================== *)

(* A circle of radius r around c is a simple cycle in the level-surface
   subgraph at distance ~r from c.  Returns open vertex sequences
   { v0, v1, ..., vk } (the wrap-around edge is implicit).  The single
   axis is Properties (a set, order-insensitive):
     "Separating" -- cycle's vertex set disconnects c from
       { v : d(c, v) > rmax }; the topological condition that makes a
       level-surface cycle a genuine circle.
     "Shortest"   -- only cycles tied at the minimum admissible length
       (the canonical-optimum reading); the length sweep stops at the
       first non-empty length class.
   Default {"Separating", "Shortest"} returns the canonical infra-circle
   (shortest separating cycle) and its ties.  Drop "Shortest" to accept
   progressively longer separating cycles; drop "Separating" to accept
   any simple cycle in the level surface.  Unknown property names
   (including "Connected", since cycles are always connected) raise
   ::badproperty.  There is no Method axis.

   On the default Properties, a single (centre, band) anchor and an integer
   band, the answer comes from the circle pool (circlePool below), whose size
   is polynomial in |V| however large the family is: count = All returns the
   pool itself as InfraCircle[{dags}], a bounded count streams circles off it
   lazily.  Off the pool's certified class -- and on any other Properties value
   -- the answer comes from the FindCycle length sweep, which materialises
   every shorter cycle first.  A pool that refuses is reported by
   ::uncertified, which stays quiet when the family is empty -- nothing was
   lost then -- and fires when circles exist that the carrier could not hold:
   the answer is still exact, but its marginals then come from enumeration
   rather than from the DP.  Several anchors fall back too, since
   circles of different centres can coincide and per-atom masses would
   double-count. *)

FindInfraCircle::badproperty = "Property `1` is not supported by FindInfraCircle.";
FindInfraCircle::uncertified = "The circle pool of the band `1` around `2` is not certified exact; the family comes from the cycle sweep instead, so it is enumerated rather than carried.";

Options[ FindInfraCircle ] = {
  Properties -> { "Separating", "Shortest" }
};

FindInfraCircle[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
  With[
    { properties = OptionValue[ FindInfraCircle, { opts }, Properties ],
      anchors = Tuples[ infraSpread /@ { p, r } ] },
    { pool = If[ Length @ anchors === 1 && Sort @ properties === { "Separating", "Shortest" },
        circlePool[ graph, Sequence @@ First @ anchors ], Null ] },
    Which[
      pool === Null || pool === $Failed,
        (* a refusal costs nothing when the family is empty -- there is no DP to
           lose and the sweep agrees -- so ::uncertified is reserved for the
           case where circles exist that the carrier could not hold *)
        With[ { swept = spreadFind[ InfraCircle, count,
                  findCircleSweep[ graph, ##, properties, count ] &, p, r ] },
          If[ pool === $Failed && ! MatchQ[ swept, InfraCircle[ { } ] | $Failed ],
            Message[ FindInfraCircle::uncertified, r, p ] ];
          swept ],
      count === All,
        InfraCircle @ pool,
      True,
        bundleTake[ InfraCircle,
          If[ pool === { }, { }, InfraCircle[ pool ][ "Realizations", UpTo @ countLimit @ count ] ],
          count ]
    ] ]


(* circlePool[graph, c, r]: the circle pool of the band { v : r1 <= d(c, v) <= r2 }
   -- the minimum-length atoms carrying a cycle that separates c from
   { v : d(c, v) > r2 }, each folded into one geodesic DAG (see the pool form
   above).  A radial seam P -- one geodesic from c to just outside the band,
   kept inside it -- meets every separating cycle and cuts the annulus into a
   disk; an atom is a contiguous arc S of P together with a pair (u, v) of
   cut-shell vertices flanking S's ends, and it carries the cycles
   S ++ (a v-u geodesic of the shell minus P).  Admissibility is tested on one
   representative per atom.

   $Failed when the carrier is not certified to hold the whole family.  Two
   conditions, both measured (Wiki/Concepts/CirclePoolStructure.md: 24 of 36
   bands certified, no false certification).  Separation is an atom invariant
   -- so one representative may decide a whole DAG, and a minimal circle's
   complementary arc is forced to be geodesic, so one seam suffices -- exactly
   when winding about c is defined, i.e. on a planar local graph.  And an empty
   pool with a non-empty exterior is the signature of a band no radial seam
   cuts open (a ball that wraps, a hole inside the band): a separating set
   exists where the carrier found no circle, minimum separating cycles need not
   be taut, and the carrier loses them silently.  The band being an integer one
   is not a mathematical condition but a seam-indexing one. *)

circlePool[ _Graph, _, r_ ] /;
  ! AllTrue[ Replace[ r, d_?NumericQ :> { d, d } ], IntegerQ ] := $Failed

circlePool[ graph_Graph, center_, r_ ] :=
  With[
    { band = Replace[ r, d_?NumericQ :> { d, d } ] },
    { localG = NeighborhoodGraph[ graph, center, Last @ band + 2 ] },
    { dist = AssociationThread[ VertexList @ localG, GraphDistance[ localG, center ] ] },
    { outside = Select[ VertexList @ localG, Lookup[ dist, Key @ # ] === Last @ band + 1 & ],
      shellVs = Select[ VertexList @ localG,
        First @ band <= Lookup[ dist, Key @ # ] <= Last @ band & ] },
    { shell = Subgraph[ localG, shellVs ],
      seam = If[ outside === { }, { },
        Take[ FindShortestPath[ localG, center, First @ outside ],
          { First @ band + 1, Last @ band + 1 } ] ] },
    { cut = VertexDelete[ shell, seam ], cutSet = Complement[ shellVs, seam ] },
    { dags = Catenate @ Map[
        arc |-> Map[
          ends |-> With[ { interval = GeodesicIntervalGraph[ cut, Last @ ends, First @ ends ] },
            Graph[ Join[ arc, VertexList @ interval ],
              Join[ DirectedEdge @@@ Partition[ Append[ arc, Last @ ends ], 2, 1 ],
                    EdgeList @ interval ] ] ],
          Select[
            If[ Length @ arc === 1,
              Subsets[ Intersection[ AdjacencyList[ shell, First @ arc ], cutSet ], { 2 } ],
              Tuples[ Intersection[ AdjacencyList[ shell, # ], cutSet ] & /@
                { First @ arc, Last @ arc } ] ],
            GraphDistance[ cut, Last @ #, First @ # ] < Infinity & ] ],
        Catenate @ Table[ Take[ seam, { i, j } ],
          { i, Length @ seam }, { j, i, Length @ seam } ] ] },
    { separating = admissibleCircleVerts[ localG, center, Last @ band, { "Separating" } ] },
    { pool = Replace[
        Catch @ Scan[
          class |-> With[
            { admissible = Select[ class, separating @ First @ dagGeodesics[ #, 1 ] & ] },
            If[ admissible =!= { }, Throw @ admissible ] ],
          Values @ KeySort @ GroupBy[ dags, 1 + Max @ Values @ dagLayers @ # & ] ],
        Null -> { } ] },
    If[ PlanarGraphQ @ localG && ( pool =!= { } || outside === { } ), pool, $Failed ]
  ]


(* the length sweep: FindCycle by ascending length on the level subgraph,
   filtered by the Properties predicates.  Exact for every Properties value,
   and exponential in the debris below the minimal separating length -- the
   wall circlePool exists to avoid. *)

findCircleSweep[ graph_Graph, center_, r_, properties_List, count_ ] :=
  Module[ { unknown, range, localG, levelSet, radius, levelGraph,
            vertsTest, tied, needed, k, kMax, batch, matching, accumulated },
    Catch[
      unknown = Complement[ properties, { "Separating", "Shortest" } ];
      If[ unknown =!= { },
        Message[ FindInfraCircle::badproperty, First @ unknown ]; Throw[ $Failed ] ];
      range = Replace[ r, d_?NumericQ :> { d, d } ];
      localG = If[ NumericQ[ range[[ 2 ]] ],
                   NeighborhoodGraph[ graph, center, Ceiling[ range[[ 2 ]] ] + 2 ], graph ];
      levelSet = Select[ VertexList[ localG ],
        range[[ 1 ]] <= GraphDistance[ localG, center, # ] <= range[[ 2 ]] & ];
      radius = range[[ 2 ]];
      levelGraph = Subgraph[ localG, levelSet ];
      vertsTest  = admissibleCircleVerts[ localG, center, radius,
                     DeleteCases[ properties, "Shortest" ] ];
      tied = MemberQ[ properties, "Shortest" ];
      needed = Switch[ count, _Integer, count, UpTo[ _Integer ], First @ count, _, Infinity ];
      kMax = VertexCount[ levelGraph ];
      accumulated = { };
      k = 3;
      While[ k <= kMax,
        batch    = cycleToVertexSequence /@ FindCycle[ levelGraph, { k }, All ];
        matching = Select[ batch, vertsTest ];
        If[ matching =!= { },
          accumulated = Join[ accumulated, matching ];
          If[ tied || Length[ accumulated ] >= needed, Break[ ] ]
        ];
        k++
      ];
      accumulated
    ]
  ]


admissibleCircleVerts[ localG_Graph, center_, radius_, properties_List ] :=
  With[ { tests = propertyPredicateCircle[ localG, center, radius, # ] & /@ properties },
    verts |-> AllTrue[ tests, # @ verts & ]
  ]


(* Separating = deleting the cycle traps the center within { d <= rmax },
   i.e. disconnects c from { v : d(c, v) > rmax }.  The shortest such cycle
   hugs the inner edge rmin; no clean cut at the mean (that is why the
   circle here differs from SeparatingSetQ, which FindInfraShell still uses). *)
propertyPredicateCircle[ localG_Graph, center_, radius_, "Separating" ] :=
  verts |-> With[ { rem = VertexDelete[ localG, verts ] },
    { cc = SelectFirst[ ConnectedComponents[ rem ], MemberQ[ #, center ] & ] },
    cc =!= Missing[ "NotFound" ] &&
    AllTrue[ cc, GraphDistance[ localG, center, # ] <= radius & ] ]

propertyPredicateCircle[ _, _, _, other_ ] :=
  ( Message[ FindInfraCircle::badproperty, other ]; Throw[ $Failed ] )


(* ===================== FindInfraCycle ===================== *)

(* Simple cycles on graph (topological, not metric circles), returned as
   InfraCircle wrappers for direct use with NullHomotopicQ /
   FindInfraHomotopy.  Sorted by length ascending. *)

FindInfraCycle[ graph_Graph, n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  FindInfraCycle[ graph, { 1, VertexCount[ graph ] }, n ]

FindInfraCycle[ graph_Graph, { k_Integer },
    n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { cycles = cycleToVertexSequence /@ FindCycle[ graph, { k }, All ] },
    bundleTake[ InfraCircle, cycles, n ] ]

FindInfraCycle[ graph_Graph, { kMin_Integer, kMax_ },
    n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { cycles = SortBy[ Length ] @ Flatten[
        cycleToVertexSequence /@ FindCycle[ graph, { # }, All ] & /@
          Range[ kMin, Min[ kMax, VertexCount[ graph ] ] ], 1 ] },
    bundleTake[ InfraCircle, cycles, n ] ]


(* ===================== InfraCircleQ ===================== *)

(* InfraCircleQ[g, cycle]: the vertex sequence is a metric circle iff
   consecutive vertices (and the wrap-around) are adjacent and the
   underlying vertex set is a metric shell.  Accepts open ({v0, ..., vk},
   vk != v0) and closed ({v0, ..., vk, v0}) input. *)

InfraCircleQ[ graph_Graph, InfraCircle[ dags : { __Graph } ] ] :=
  AllTrue[ Catenate[ dagGeodesics /@ dags ], InfraCircleQ[ graph, # ] & ]

InfraCircleQ[ graph_Graph, c_InfraCircle ] :=
  AllTrue[ First @ c, InfraCircleQ[ graph, # ] & ]

InfraCircleQ[ graph_Graph, cycle_List ] /; Length[ cycle ] >= 3 :=
  With[ {
      closed = If[ First @ cycle === Last @ cycle, cycle, Append[ cycle, First @ cycle ] ] },
    { verts = Most @ closed,
      pairs = Partition[ closed, 2, 1 ] },
    DuplicateFreeQ[ verts ] &&
    AllTrue[ pairs, EdgeQ[ graph, UndirectedEdge @@ # ] & ] &&
    InfraShellQ[ graph, verts ]
  ]

InfraCircleQ[ _Graph, cycle_List ] /; Length[ cycle ] < 3 := False


(* ===================== Scene-DSL constructor ===================== *)

dispatchConstruction[ graph_Graph, InfraCircle[ center_, r_, opts___Rule ] ] :=
  capBranches[
    applySelectOption[ graph,
      FindInfraCircle[ graph, center, r, All ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      True, <| "Center" -> center,
               "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]
