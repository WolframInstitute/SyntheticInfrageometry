Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]


PackageScope[circlePool]


(* ===================== FindInfraCircle ===================== *)

(* a circle of radius r around c is a simple cycle in the level surface at distance ~r from c, returned as a directed cycle graph on the substrate vertices; the count-less call is one circle, a bounded count and All a List of them -- closed walks have no acyclic union to carry them, so All enumerates.
   On the default Properties, a single anchor and an integer band the family is carried internally by the circle pool (circlePool), polynomial in |V| however large the family is, and a bounded count streams circles off its atoms in candidate ("Greedy", "Exhaustive") or random ("RandomGreedy") order; otherwise by the FindCycle length sweep, which materialises every shorter cycle first.  One class under every Method *)

FindInfraCircle::badproperty = "Property `1` is not supported by FindInfraCircle.";
FindInfraCircle::badmethod   = "Method `1` is not supported by FindInfraCircle.";
FindInfraCircle::uncertified = "The circle pool of the band `1` around `2` is not certified exact; the family comes from the cycle sweep instead, so it is enumerated rather than carried.";

Options[ FindInfraCircle ] = {
  Properties -> { "Separating", "Shortest" },
  Method     -> Automatic
};

FindInfraCircle[ graph_Graph, p_, r_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  Catch @ With[
    { properties = OptionValue[ FindInfraCircle, { opts }, Properties ],
      methodSpec = resolveMethod[ OptionValue[ FindInfraCircle, { opts }, Method ], count ],
      anchors = Tuples[ { Keys @ toDensity[ graph, p ], infraSpread @ r } ] },
    { methodHead = methodName @ methodSpec },
    If[ ! MatchQ[ methodHead, "Exhaustive" | "Greedy" | "RandomGreedy" ],
      Message[ FindInfraCircle::badmethod, methodSpec ]; Throw[ $Failed ] ];
    With[
      { branch = greedyBranch[ methodHead /. "Exhaustive" -> "Greedy" ],
        pruning = Replace[ methodSpec,
          { { "Exhaustive", subs___ } :> ( "Pruning" /. { subs } /. "Pruning" -> Infinity ), _ :> Infinity } ],
        pool = If[ Length @ anchors === 1 && Sort @ properties === { "Separating", "Shortest" },
          circlePool[ graph, Sequence @@ First @ anchors ], Null ] },
      If[ pool === Null || pool === $Failed,
        (* a refusal costs nothing on an empty family, so ::uncertified fires only when circles exist that the carrier could not hold *)
        With[ { swept = spreadFind[ geodesicCycleGraph, count,
                  findCircleSweep[ graph, ##, properties, count, branch, pruning ] &, toDensity[ graph, p ], r ] },
          If[ pool === $Failed && swept =!= { } && swept =!= $Failed,
            Message[ FindInfraCircle::uncertified, r, p ] ];
          swept ],
        countTake[
          geodesicCycleGraph /@ Fold[ { acc, dag } |-> If[ Length @ acc >= countLimit @ count, acc,
              Join[ acc, dagGeodesics[ dag, countLimit @ count - Length @ acc, branch ] ] ],
            { }, branch @ pool ],
          count ]
      ] ] ]


(* a radial seam P -- one geodesic from c to just outside the band, kept inside it -- meets every separating cycle and cuts the annulus into a disk; an atom is a contiguous arc S of P with a pair (u, v) of cut-shell vertices flanking its ends, carrying the cycles S ++ (a v-u geodesic of the shell minus P).
   $Failed when the carrier is not certified: separation is an atom invariant exactly when winding about c is defined, i.e. on a planar local graph, and an empty pool with a non-empty exterior is the signature of a band no radial seam cuts open, where minimum separating cycles need not be taut. *)

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


(* FindCycle by ascending length on the level subgraph, filtered by the Properties predicates: exact for every Properties value, exponential in the debris below the minimal separating length.  The first non-empty grade is certified only by exhausting the shorter ones, so every Method runs the same sweep and branch only orders the ties; pruning caps the cycles kept per length *)

findCircleSweep[ graph_Graph, center_, r_, properties_List, count_, branch_, pruning_ ] :=
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
      needed = countLimit @ count;
      kMax = VertexCount[ levelGraph ];
      accumulated = { };
      k = 3;
      While[ k <= kMax,
        batch    = branch @ applyPruning[ cycleToVertexSequence /@ FindCycle[ levelGraph, { k }, All ], pruning ];
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


(* the shortest separating cycle hugs the inner edge rmin, with no clean cut at the mean -- which is why this differs from the SeparatingSetQ FindInfraShell uses *)
propertyPredicateCircle[ localG_Graph, center_, radius_, "Separating" ] :=
  verts |-> With[ { rem = VertexDelete[ localG, verts ] },
    { cc = SelectFirst[ ConnectedComponents[ rem ], MemberQ[ #, center ] & ] },
    cc =!= Missing[ "NotFound" ] &&
    AllTrue[ cc, GraphDistance[ localG, center, # ] <= radius & ] ]

propertyPredicateCircle[ _, _, _, other_ ] :=
  ( Message[ FindInfraCircle::badproperty, other ]; Throw[ $Failed ] )


(* ===================== FindInfraCycle ===================== *)


FindInfraCycle[ graph_Graph, n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  FindInfraCycle[ graph, { 1, VertexCount[ graph ] }, n ]

FindInfraCycle[ graph_Graph, { k_Integer },
    n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { cycles = cycleToVertexSequence /@ FindCycle[ graph, { k }, All ] },
    countTake[ geodesicCycleGraph /@ cycles, n ] ]

FindInfraCycle[ graph_Graph, { kMin_Integer, kMax_ },
    n : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { cycles = SortBy[ Length ] @ Flatten[
        cycleToVertexSequence /@ FindCycle[ graph, { # }, All ] & /@
          Range[ kMin, Min[ kMax, VertexCount[ graph ] ] ], 1 ] },
    countTake[ geodesicCycleGraph /@ cycles, n ] ]


(* ===================== InfraCircleQ ===================== *)

(* a metric circle iff consecutive vertices and the wrap-around are adjacent and the vertex set is a metric shell; a cycle graph is read as its closed walk *)

InfraCircleQ[ graph_Graph, ws : { __Graph } ] := AllTrue[ ws, InfraCircleQ[ graph, # ] & ]

InfraCircleQ[ graph_Graph, w_Graph ] := AllTrue[ walkRealisations @ w, InfraCircleQ[ graph, # ] & ]

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
      walkSequence /@ FindInfraCircle[ graph, center, r, All ],
      "Select" /. { opts } /. "Select" -> None,
      True, <| "Center" -> center,
               "Radius" -> If[ NumericQ[ r ], r, Mean[ r ] ] |> ],
    extractBranches[ { opts } ] ]
