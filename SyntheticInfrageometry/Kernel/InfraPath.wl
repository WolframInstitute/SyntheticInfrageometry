Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findInfraPathCore]
PackageScope[extendInfraPathCore]
PackageScope[concatenatePathPair]
PackageScope[allNeighboursBaseFn]


(* ===================== InfraPath wrapper ===================== *)

InfraPath[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraPath[ _List ] ] ] :=
  InfraPath[ Flatten[ reps /. InfraPath[ xs_List ] :> xs, 1 ] ]


(* ===================== FindInfraPath ===================== *)

(* A walk from p1 to p2 (not necessarily simple, not necessarily geodesic).
   kspec restricts walk length; Properties impose per-step constraints:
   "Simple" (no revisits), "ShortestPath"+Window, "LongestPath"+Window,
   "EdgeMin"|"EdgeMax" with user-supplied f[a, b].  Method is "Exhaustive"
   (default, BFS) with optional Pruning. *)

FindInfraPath::badproperty = "Property `1` is not supported by FindInfraPath.";
FindInfraPath::badmethod   = "Method `1` is not supported by FindInfraPath.";

Options[ FindInfraPath ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraPath[ graph_Graph, p1_, p2_,
    kspec : ( _Integer | { _Integer } | { _Integer, _Integer } | Infinity ) : Infinity,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPath, count,
    findInfraPathCore[ graph, ##, kspec, count, opts ] &, p1, p2 ]


findInfraPathCore[ _Graph, p1_, p1_, _, _, ___ ] := { }

findInfraPathCore[ graph_Graph, p1_, p2_, kspec_, count_, opts : OptionsPattern[ FindInfraPath ] ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraPath, { opts }, Properties ],
      methodSpec = OptionValue[ FindInfraPath, { opts }, Method ] /. Automatic -> "Exhaustive" },
    With[ { methodHead = methodName @ methodSpec,
            pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity,
            fastPathQ  = properties === { } },
      Switch[ methodHead,
        "Exhaustive",
          If[ fastPathQ,
            Replace[
              FindPath[ graph, p1, p2, kspec, count /. UpTo[ n_ ] :> n ],
              Except[ _List ] -> { } ],
            Select[
              frontierSweep[ graph, p1, p2,
                makeCandidateFn[ graph, allNeighboursBaseFn,
                  properties, FindInfraPath::badproperty ],
                pruning, countLimit @ count ],
              walkLengthAdmissibleQ[ kspec ] ]
          ],
        _,
          Message[ FindInfraPath::badmethod, methodSpec ]; $Failed
      ]
    ]
  ]


(* Path-family base candidate function: every adjacent vertex of Last @ path,
   without simplicity filtering -- "Simple" Property handles that opt-in. *)

allNeighboursBaseFn[ g_Graph, path_List ] := AdjacencyList[ g, Last @ path ]


(* walkLengthAdmissibleQ[kspec]: predicate on a vertex sequence checking it
   has length compatible with kspec.  Path length = number of edges = Length - 1. *)

walkLengthAdmissibleQ[ Infinity ]                 := True &
walkLengthAdmissibleQ[ k_Integer ]                := Length[ # ] - 1 == k &
walkLengthAdmissibleQ[ { k_Integer } ]            := Length[ # ] - 1 == k &
walkLengthAdmissibleQ[ { kmin_Integer, kmax_Integer } ] :=
  kmin <= Length[ # ] - 1 <= kmax &


(* ===================== ExtendInfraPath ===================== *)

(* ExtendInfraPath[g, walk, n] extends a walk by per-step rules until
   inextensible ("Length" -> Automatic) or for the requested step budget.
   Same Properties as FindInfraPath; default Properties -> {} allows non-simple
   extensions. *)

ExtendInfraPath::badproperty = "Property `1` is not supported by ExtendInfraPath.";
ExtendInfraPath::badmethod   = "Method `1` is not supported by ExtendInfraPath.";

Options[ ExtendInfraPath ] = {
  Properties -> { },
  Method     -> "Exhaustive",
  "Length"   -> Automatic,
  "Side"     -> "Both"
};

ExtendInfraPath[ graph_Graph, path_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPath, count,
    extendInfraPathCore[ graph, ##, opts ] &, path ]


extendInfraPathCore[ _Graph, walk_List, OptionsPattern[ ExtendInfraPath ] ] /;
    Length[ walk ] < 1 := { walk }

extendInfraPathCore[ graph_Graph, walk_List, opts : OptionsPattern[ ExtendInfraPath ] ] :=
  Catch @ With[ {
      properties = OptionValue[ ExtendInfraPath, { opts }, Properties ],
      methodSpec = OptionValue[ ExtendInfraPath, { opts }, Method ] /. Automatic -> "Exhaustive",
      side       = OptionValue[ ExtendInfraPath, { opts }, "Side" ],
      length     = OptionValue[ ExtendInfraPath, { opts }, "Length" ] },
    With[ { methodHead = methodName @ methodSpec,
            pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
      If[ methodHead =!= "Exhaustive",
        Message[ ExtendInfraPath::badmethod, methodSpec ]; Throw[ $Failed ] ];
      With[ { candidateFn = makeCandidateFn[ graph, allNeighboursBaseFn,
                              properties, ExtendInfraPath::badproperty ],
              simpleQ     = MemberQ[ properties, "Simple" | { "Simple" } ] },
        With[ { forward  = If[ side === "Backward", { walk },
                              extendOneSide[ graph, walk, candidateFn, length, pruning ] ],
                backward = If[ side === "Forward", { walk },
                              Reverse /@ extendOneSide[ graph, Reverse @ walk, candidateFn,
                                                       length, pruning ] ] },
          Switch[ side,
            "Forward",  forward,
            "Backward", backward,
            "Both",
              With[ { joined = DeleteDuplicates[ Join @@@ Tuples[
                  { Drop[ #, -Length[ walk ] ] & /@ backward, forward } ] ] },
                If[ simpleQ, Select[ joined, DuplicateFreeQ ], joined ]
              ]
          ]
        ]
      ]
    ]
  ]


(* One-side frontier BFS over walk space.  Walks with no admissible next
   vertex freeze; walks at step budget exit alive. *)

extendOneSide[ graph_Graph, seed_List, candidateFn_, length_, pruning_ ] :=
  Module[ { live = { seed }, dead = { }, steps = 0,
            maxSteps = length /. Automatic -> Infinity },
    While[ live =!= { } && steps < maxSteps,
      With[ { pairs = ( p |-> { p, candidateFn[ graph, p ] } ) /@ live },
        dead = Join[ dead, Cases[ pairs, { p_, { } } :> p ] ];
        live = applyPruning[
          Flatten[ Cases[ pairs,
            { p_, nexts : { __ } } :> ( Append[ p, # ] & /@ nexts ) ], 1 ],
          pruning ]
      ];
      steps++
    ];
    DeleteDuplicates @ Join[ dead, live ]
  ]


(* ===================== ConcatenateInfraPath ===================== *)

(* path concatenation: all pairs (walk1, walk2) with Last[walk1] === First[walk2] *)

ConcatenateInfraPath[ path1_, path2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  infraSpreadAndCartesian[ InfraPath, count, concatenatePathPair, path1, path2 ]

concatenatePathPair[ walk1_List, walk2_List ] :=
  If[ Last[ walk1 ] === First[ walk2 ], { Join[ walk1, Rest @ walk2 ] }, { } ]


(* ===================== Scene-DSL constructor ===================== *)

(* InfraPath[v1, v2, ..., vk] inside a scene is the literal walk with the
   given vertices, valid iff each consecutive pair is a graph edge.  Non-
   simple chains kept (no DuplicateFreeQ filter). *)

dispatchConstruction[ graph_Graph, InfraPath[ vs__ ] ] :=
  With[ { walk = { vs } },
    If[ Length[ walk ] >= 2 &&
        AllTrue[ Partition[ walk, 2, 1 ], EdgeQ[ graph, UndirectedEdge @@ # ] & ],
      { walk },
      { } ]
  ]
