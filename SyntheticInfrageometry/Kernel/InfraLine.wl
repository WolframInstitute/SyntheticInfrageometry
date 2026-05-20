Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineCore]
PackageScope[findLineExtensions]
PackageScope[findLineExtensionsWith]
PackageScope[findLineExtensionsGreedy]
PackageScope[findParallelCore]
PackageScope[findPerpendicularCore]
PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


(* FindInfraLine returns InfraLine (a maximal geodesic is a line; the wrapper head
   distinguishes line-shaped Find output from segment-shaped Find output).
   This file owns the line-shaped Find / construction / predicate operations. *)


(* ===================== InfraLine wrapper ===================== *)

InfraLine[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraLine[ _List ] ] ] :=
  InfraLine[ Flatten[ reps /. InfraLine[ xs_List ] :> xs, 1 ] ]

(* "Length" = list of edge counts, one per realisation: |line| - 1. *)
InfraLine[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps


(* ===================== FindInfraLine ===================== *)

(* A line through p1, p2: a maximal geodesic extension (a, ..., p1, ..., p2, ..., b)
   every contiguous sub-sequence of which is a geodesic, inextensible at both ends.
   FindInfraLine[g, seg]: maximal geodesic lines containing seg as a sub-sequence
   (subsumes the deleted 2-arg ExtendInfraSegment). *)

FindInfraLine::badmethod   = "Method `1` is not supported by FindInfraLine.";
FindInfraLine::badproperty = "Property `1` is not supported by FindInfraLine (FindInfraLine accepts only Properties -> {}).";

Options[ FindInfraLine ] = {
  Properties   -> { },
  Method       -> "Exhaustive",
  "Maximality" -> "Extension"
};

FindInfraLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    ! ListQ[ p1 ] && Head[ p1 ] =!= InfraSegment :=
  infraSpreadAndCartesian[ InfraLine, count,
    findLineCore[ graph, ##, opts ] &, p1, p2 ]

(* Overload: extend a given segment to a maximal line.  count / opts shape
   matches the two-endpoint form; the segment list is taken as the line's
   middle and extended jointly via findLineExtensionsWith. *)

FindInfraLine[ graph_Graph, InfraSegment[{ walk_List, ___ }],
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  FindInfraLine[ graph, walk, count, opts ]

FindInfraLine[ graph_Graph, segment_List, count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  With[ { capped = infraCap[
      findLineCoreFromSegment[ graph, segment, opts ], count ] },
    If[ capped === $Failed, $Failed, InfraLine[ { # } ] & /@ capped ]
  ]


findLineCore[ graph_Graph, p1_, p2_, opts : OptionsPattern[ FindInfraLine ] ] /;
    MemberQ[ VertexList[ graph ], p1 ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraLine, { opts }, Properties ],
      methodSpec = OptionValue[ FindInfraLine, { opts }, Method ] /. Automatic -> "Exhaustive",
      maximality = OptionValue[ FindInfraLine, { opts }, "Maximality" ] },
    If[ properties =!= { },
      Message[ FindInfraLine::badproperty, properties ]; Throw[ $Failed ] ];
    With[ { methodHead = methodName @ methodSpec,
            pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
      With[ { middles = allGeodesics[ graph, p1, p2 ] },
        With[ { ext = Union @ Flatten[
              Switch[ methodHead,
                "Exhaustive",  findLineExtensions[ graph, #, pruning ] & /@ middles,
                "Greedy",      findLineExtensionsGreedy[ graph, # ]     & /@ middles,
                _,             Message[ FindInfraLine::badmethod, methodSpec ]; Throw[ $Failed ]
              ], 1 ] },
          If[ maximality === "Diameter",
            Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
            ext ]
        ]
      ]
    ]
  ]


findLineCoreFromSegment[ graph_Graph, segment_List, opts : OptionsPattern[ FindInfraLine ] ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraLine, { opts }, Properties ],
      methodSpec = OptionValue[ FindInfraLine, { opts }, Method ] /. Automatic -> "Exhaustive",
      maximality = OptionValue[ FindInfraLine, { opts }, "Maximality" ] },
    If[ properties =!= { },
      Message[ FindInfraLine::badproperty, properties ]; Throw[ $Failed ] ];
    With[ { methodHead = methodName @ methodSpec,
            pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
      With[ { ext = Switch[ methodHead,
            "Exhaustive",  findLineExtensions[ graph, segment, pruning ],
            "Greedy",      findLineExtensionsGreedy[ graph, segment ],
            _,             Message[ FindInfraLine::badmethod, methodSpec ]; Throw[ $Failed ]
          ] },
        If[ maximality === "Diameter",
          Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
          ext ]
      ]
    ]
  ]


(* Maximal geodesic extensions of a segment.  Asymmetric: each side is
   extended independently to its maximal admissible length; among pairs
   that achieve a valid joint geodesic (degenerate triangle inequality
   d(s, e) == d(s, p1) + d + d(p2, e)) we keep those with maximum total
   extension length b_s + a_e.  findLineExtensionsWith takes an optional
   admissibility predicate (used by FindInfraParallel to restrict to the
   level set). *)

findLineExtensions[ graph_Graph, segment_List, pruning_ : Infinity ] :=
  findLineExtensionsWith[ graph, segment, pruning, True & ]


findLineExtensionsWith[ graph_Graph, segment_List, pruning_, admissible_ ] /; Length[ segment ] < 2 :=
  { segment }

findLineExtensionsWith[ graph_Graph, segment_List, pruning_, admissible_ ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    With[ { extendBefore = Select[ VertexList[ graph ],
              c |-> admissible[ c ] && GraphDistance[ graph, c, p1 ] + d == GraphDistance[ graph, c, p2 ] ],
            extendAfter  = Select[ VertexList[ graph ],
              c |-> admissible[ c ] && GraphDistance[ graph, c, p2 ] + d == GraphDistance[ graph, p1, c ] ] },
      With[ { validPairs = Select[ Tuples[ { extendBefore, extendAfter } ],
              pair |-> GraphDistance[ graph, pair[[1]], pair[[2]] ] ==
                       GraphDistance[ graph, pair[[1]], p1 ] + d + GraphDistance[ graph, p2, pair[[2]] ] ] },
        With[ { maxPairs = MaximalBy[ validPairs,
                GraphDistance[ graph, #[[1]], p1 ] + GraphDistance[ graph, p2, #[[2]] ] & ] },
          If[ maxPairs === { } || maxPairs === { { p1, p2 } }, { segment },
            Flatten[
              With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
                With[ { db = GraphDistance[ graph, s, p1 ], da = GraphDistance[ graph, p2, e ] },
                  With[ { bp = If[ db == 0, { {} },
                                  Most /@ Select[
                                    applyPruning[ FindPath[ graph, s, p1, { db }, All ], pruning ],
                                    AllTrue[ #, admissible ] & ] ],
                          ap = If[ da == 0, { {} },
                                  Rest /@ Select[
                                    applyPruning[ FindPath[ graph, p2, e, { da }, All ], pruning ],
                                    AllTrue[ #, admissible ] & ] ] },
                    Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ] ] ]
              ] & /@ maxPairs,
              1 ]
          ]
        ]
      ]
    ]
  ]


(* Greedy maximal geodesic extension: walk vertex-by-vertex outward from each
   endpoint, accepting the first neighbor that extends the geodesic by one
   step.  Returns exactly one chain -- maximally inextensible but not
   necessarily of maximum total length. *)

findLineExtensionsGreedy[ graph_Graph, segment_List ] :=
  findLineExtensionsGreedy[ graph, segment, True & ]

findLineExtensionsGreedy[ graph_Graph, segment_List, admissible_ ] :=
  { greedyWalkBoth[ graph, segment, admissible ] }

greedyWalkBoth[ graph_Graph, segment_List, admissible_ ] /; Length[ segment ] < 2 := segment

greedyWalkBoth[ graph_Graph, segment_List, admissible_ ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    Join[ Reverse @ greedyWalk[ graph, p1, p2, d, admissible ],
          segment,
          greedyWalk[ graph, p2, p1, d, admissible ] ] ]


greedyWalk[ graph_Graph, h_, a_, db_, admissible_ ] :=
  With[ { v = SelectFirst[ AdjacencyList[ graph, h ],
            c |-> admissible[ c ] && GraphDistance[ graph, c, a ] == db + 1, Missing[] ] },
    If[ MissingQ[ v ], { },
      Prepend[ greedyWalk[ graph, v, a, db + 1, admissible ], v ] ]
  ]


(* ===================== FindInfraParallel ===================== *)

(* FindInfraParallel[g, line, p]: maximal sub-segment of a maximal geodesic
   through p whose vertices all lie at distance r = d(p, line) from line. *)

FindInfraParallel::badmethod   = "Method `1` is not supported by FindInfraParallel.";
FindInfraParallel::badproperty = "Property `1` is not supported by FindInfraParallel (FindInfraParallel accepts only Properties -> {}).";

Options[ FindInfraParallel ] = {
  Properties -> { },
  Method     -> "Exhaustive"
};

FindInfraParallel[ graph_Graph, line_, p_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraLine, count,
    findParallelCore[ graph, ##, opts ] &, line, p ]


findParallelCore[ graph_Graph, line_List, p_, opts : OptionsPattern[ FindInfraParallel ] ] :=
  Catch @ With[ {
      properties = OptionValue[ FindInfraParallel, { opts }, Properties ],
      methodSpec = OptionValue[ FindInfraParallel, { opts }, Method ] /. Automatic -> "Exhaustive" },
    If[ properties =!= { },
      Message[ FindInfraParallel::badproperty, properties ]; Throw[ $Failed ] ];
    With[ { methodHead = methodName @ methodSpec,
            pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
      Switch[ methodHead,
        "Exhaustive", findParallelExtensions[ graph, line, p, pruning ],
        "Greedy",     findParallelExtensionsGreedy[ graph, line, p ],
        _,            Message[ FindInfraParallel::badmethod, methodSpec ]; Throw[ $Failed ]
      ]
    ]
  ]


(* Maximal level-set geodesics through p: every vertex of the result lies at
   distance r = d(p, line) from line, and the chain is a geodesic in graph.
   Seeded by each level-set neighbor of p, extended on both sides via the
   line-extension machinery with an extra admissibility predicate. *)

findParallelExtensions[ graph_Graph, line_List, p_, pruning_ : Infinity ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    With[ { r = lineDist[ p ] },
      If[ r === Infinity, { },
        With[ { admissible = c |-> lineDist[ c ] == r },
          With[ { seeds = Select[ AdjacencyList[ graph, p ], admissible ] },
            With[ { chains = Flatten[
                    findLineExtensionsWith[ graph, { p, # }, pruning, admissible ] & /@ seeds,
                    1 ] },
              DeleteDuplicates @ Map[ canonicalLine, Select[ chains, Length[ # ] >= 2 & ] ]
            ]
          ]
        ]
      ]
    ]
  ]


findParallelExtensionsGreedy[ graph_Graph, line_List, p_ ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    With[ { r = lineDist[ p ] },
      If[ r === Infinity, { },
        With[ { admissible = c |-> lineDist[ c ] == r },
          With[ { seed = SelectFirst[ AdjacencyList[ graph, p ], admissible, Missing[] ] },
            If[ MissingQ[ seed ], { { p } },
              { greedyWalkBoth[ graph, { p, seed }, admissible ] } ]
          ]
        ]
      ]
    ]
  ]


(* ===================== FindInfraPerpendicular ===================== *)

(* Foot of the perpendicular from p to L (Euclid I.12, isosceles base midpoint):
   for each pair {a, b} of L-vertices equidistant from p, the midpoint of the
   line-arc from a to b along L is a candidate foot. *)

FindInfraPerpendicular::badmethod = "Method `1` is not supported by FindInfraPerpendicular.";

Options[ FindInfraPerpendicular ] = { Method -> "Metric" };

FindInfraPerpendicular[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraPoint, count,
    findPerpendicularCore[ graph, ##, opts ] &, line, point ]


findPerpendicularCore[ graph_Graph, line_List, point_, opts : OptionsPattern[ FindInfraPerpendicular ] ] :=
  With[ { spec = OptionValue[ FindInfraPerpendicular, { opts }, Method ] },
    Switch[ methodName @ spec,
      "Metric",
        With[ { distances = GraphDistance[ graph, point, # ] & /@ line },
          Union @ Flatten[
            ( group |-> Map[
                pair |-> With[ { lo = Min @@ pair, hi = Max @@ pair },
                  If[ OddQ[ hi - lo ],
                    line[[ lo ;; hi ]][[ Ceiling[ ( hi - lo + 1 ) / 2 ] ]],
                    Nothing ] ],
                Subsets[ group, { 2 } ] ]
            ) /@ Values @ GroupBy[ Range @ Length @ line, distances[[ # ]] & ]
          ]
        ],
      _,
        Message[ FindInfraPerpendicular::badmethod, spec ]; $Failed
    ]
  ]


(* ===================== FindInfraCommonLine ===================== *)

(* Canonical maximal geodesics through every vertex in verts. *)

findCommonLineCore[ graph_Graph, verts_List ] :=
  With[ { uverts = DeleteDuplicates @ Catenate[ infraUnionSpread /@ verts ] },
    If[ Length[ uverts ] < 2, { },
      DeleteDuplicates @ Select[
        canonicalLine[ #[[ 1, 1 ]] ] & /@ FindInfraLine[ graph, First @ uverts, uverts[[ 2 ]], All ],
        line |-> SubsetQ[ line, uverts ] ]
    ]
  ]

FindInfraCommonLine[ graph_Graph, verts_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { capped = infraCap[ findCommonLineCore[ graph, verts ], count ] },
    If[ capped === $Failed, $Failed, InfraLine[ { # } ] & /@ capped ]
  ]


(* ===================== InfraLineQ ===================== *)

(* A segment is a line iff no extension preserves the geodesic property. *)

InfraLineQ[ graph_Graph, segment_List ] :=
  InfraSegmentQ[ graph, segment ] &&
  Length[ First @ findLineExtensions[ graph, segment ] ] == Length[ segment ]


(* ===================== InfraParallelQ ===================== *)

(* Definition-alpha parallelism: l1 and l2 are disjoint and the distance from
   each vertex of l1 to l2 is constant up to threshold. *)

InfraParallelQ[ distanceMatrix_List, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Min[ distanceMatrix[[ #, l2 ]] ] & /@ l1 },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold ]
  ]

InfraParallelQ[ graph_Graph, l1_List, l2_List, threshold_ : 0 ] :=
  If[ IntersectingQ[ l1, l2 ], False,
    With[ { lineDistances = Table[ Min[ GraphDistance[ graph, v, # ] & /@ l2 ], { v, l1 } ] },
      Max[ lineDistances ] - Min[ lineDistances ] <= threshold ]
  ]


(* ===================== PencilDirections / PencilCardinality / LineCount ===================== *)

(* Canonical maximal geodesics through origin, one per projective direction class
   at origin.  LineCount: canonical maximal geodesics overall. *)

PencilDirections[ graph_Graph, origin_ ] :=
  DeleteDuplicates @ Map[ canonicalLine, Flatten[
    Map[ #[[ 1, 1 ]] &, FindInfraLine[ graph, origin, #, All ] ] & /@
      DeleteCases[ VertexList[ graph ], origin ],
    1 ] ]

PencilCardinality[ graph_Graph, origin_ ] := Length @ PencilDirections[ graph, origin ]

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]


(* ===================== Helpers: canonical lines ===================== *)

(* canonicalLine: lexicographic minimum of a line and its reversal.
   allCanonicalLines: every canonical maximal geodesic in the graph
   (consumed by PencilDirections, LineCount, and ProjectiveGeometry.wl). *)

canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine[ #[[ 1, 1 ]] ] & /@ FindInfraLine[ graph, #[[ 1 ]], #[[ 2 ]], All ] & /@
      Subsets[ VertexList[ graph ], { 2 } ],
    1
  ]


(* ===================== Scene-DSL constructor ===================== *)

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
      #[[ 1, 1 ]] & /@ FindInfraLine[ graph, p1, p2, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraLine ] ] ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]
