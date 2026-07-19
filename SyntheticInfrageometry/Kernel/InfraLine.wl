Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findLineExtensions]
PackageScope[findLineExtensionsWith]
PackageScope[findLineExtensionsGreedy]
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

(* occupation measures (see InfraMeasure): ["OccupationCount"] = raw c(v); ["OccupationMeasure"] == ["Measure"] = c(v)/N; ["ProbabilityMeasure"] = c(v)/Total. *)
InfraLine[ reps_List ][ "OccupationCount" ] := infraVertexMultiset[ InfraLine[ reps ] ]
InfraLine[ reps_List ][ "OccupationMeasure" ] := InfraMeasure[ InfraLine[ reps ] ]
InfraLine[ reps_List ][ "Measure" ] := InfraMeasure[ InfraLine[ reps ] ]
InfraLine[ reps_List ][ "ProbabilityMeasure" ] := InfraMeasure[ InfraLine[ reps ], Method -> "Probability" ]
(* line[[i]] = weighted InfraPoint of the i-th position across realisations
   (mass = multiplicity).  First/Last and multi-index Part bypass this. *)
InfraLine /: Part[ InfraLine[ reps_List ], i_Integer ] := columnInfraPoint[ reps, i ]


(* ===================== FindInfraLine ===================== *)

(* A line through p1, p2: a maximal geodesic extension (a, ..., p1, ..., p2, ..., b)
   every contiguous sub-sequence of which is a geodesic, inextensible at both ends.
   FindInfraLine[g, seg]: maximal geodesic lines containing seg as a sub-sequence
   (subsumes the deleted 2-arg ExtendInfraSegment). *)

FindInfraLine::badmethod    = "Method `1` is not supported by FindInfraLine.";
FindInfraLine::badproperty  = "Property `1` is not supported by FindInfraLine (FindInfraLine accepts only Properties -> {}).";
FindInfraLine::baddirection = "Direction `1` is not supported by FindInfraLine.";

Options[ FindInfraLine ] = {
  Properties   -> { },
  Method       -> "Exhaustive",
  "Maximality" -> "Extension",
  "Direction"  -> "BothSides"
};

FindInfraLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] /;
    ! ListQ[ p1 ] && Head[ p1 ] =!= InfraSegment :=
  spreadFind[ InfraLine, count,
    { q1, q2 } |-> Catch @ With[ {
        properties = OptionValue[ FindInfraLine, { opts }, Properties ],
        methodSpec = OptionValue[ FindInfraLine, { opts }, Method ] /. Automatic -> "Exhaustive",
        maximality = OptionValue[ FindInfraLine, { opts }, "Maximality" ],
        direction  = OptionValue[ FindInfraLine, { opts }, "Direction" ] },
      If[ properties =!= { },
        Message[ FindInfraLine::badproperty, properties ]; Throw[ $Failed ] ];
      If[ ! MatchQ[ direction, "Forward" | "Backward" | "BothSides" ],
        Message[ FindInfraLine::baddirection, direction ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec,
              pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        { middles = allGeodesics[ graph, q1, q2 ] },
        { ext = Union @ Flatten[
            Switch[ methodHead,
              "Exhaustive",  findLineExtensions[ graph, #, pruning, direction ] & /@ middles,
              "Greedy",      findLineExtensionsGreedy[ graph, #, direction ]     & /@ middles,
              _,             Message[ FindInfraLine::badmethod, methodSpec ]; Throw[ $Failed ]
            ], 1 ] },
        If[ maximality === "Diameter",
          Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
          ext ]
      ]
    ], p1, p2 ]

(* Overload: extend a given segment to a maximal line.  count / opts shape
   matches the two-endpoint form; the segment list is taken as the line's
   middle and extended jointly via findLineExtensions. *)

FindInfraLine[ graph_Graph, InfraSegment[ dag_Graph ],
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  FindInfraLine[ graph, First @ dagGeodesics[ dag ], count, opts ]

FindInfraLine[ graph_Graph, InfraSegment[{ walk_List, ___ }],
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  FindInfraLine[ graph, walk, count, opts ]

FindInfraLine[ graph_Graph, segment_List, count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] /; Length[ segment ] >= 2 :=
  With[ { core = Catch @ With[ {
        properties = OptionValue[ FindInfraLine, { opts }, Properties ],
        methodSpec = OptionValue[ FindInfraLine, { opts }, Method ] /. Automatic -> "Exhaustive",
        maximality = OptionValue[ FindInfraLine, { opts }, "Maximality" ],
        direction  = OptionValue[ FindInfraLine, { opts }, "Direction" ] },
      If[ properties =!= { },
        Message[ FindInfraLine::badproperty, properties ]; Throw[ $Failed ] ];
      If[ ! MatchQ[ direction, "Forward" | "Backward" | "BothSides" ],
        Message[ FindInfraLine::baddirection, direction ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec,
              pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        { ext = Switch[ methodHead,
            "Exhaustive",  findLineExtensions[ graph, segment, pruning, direction ],
            "Greedy",      findLineExtensionsGreedy[ graph, segment, direction ],
            _,             Message[ FindInfraLine::badmethod, methodSpec ]; Throw[ $Failed ]
          ] },
        If[ maximality === "Diameter",
          Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
          ext ]
      ]
    ] },
    If[ core === $Failed, $Failed,
      With[ { capped = infraCap[ core, count ] },
        If[ capped === $Failed, $Failed, InfraLine[ { # } ] & /@ capped ]
      ]
    ]
  ]


(* Maximal geodesic extensions of a segment.  Asymmetric Cartesian: each side
   is extended independently to its maximal admissible length; among pairs
   that achieve a valid joint geodesic (degenerate triangle inequality
   d(s, e) == d(s, p1) + d + d(p2, e)) we keep those with maximum total
   extension length b_s + a_e.  findLineExtensionsWith takes an optional
   admissibility predicate (used by FindInfraParallel to restrict to the
   level set).  Direction \[Element] {"Forward", "Backward", "BothSides"}
   controls which sides of the segment are extended; the default
   "BothSides" reproduces the old behaviour.  For "Forward" the backward
   anchor is pinned to p1 = First[segment]; for "Backward" the forward
   anchor is pinned to p2 = Last[segment]; the output set is identical
   under "BothSides" to per-step symmetric stepping (only intermediate
   enumeration differs). *)

findLineExtensions[ graph_Graph, segment_List, pruning_ : Infinity, direction_String : "BothSides" ] :=
  findLineExtensionsWith[ graph, segment, pruning, True &, direction ]


findLineExtensionsWith[ graph_Graph, segment_List, pruning_, admissible_,
    direction_String : "BothSides" ] /; Length[ segment ] < 2 :=
  { segment }

findLineExtensionsWith[ graph_Graph, segment_List, pruning_, admissible_,
    direction_String : "BothSides" ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    { extendBefore = If[ direction === "Forward", { p1 },
        Select[ VertexList[ graph ],
          c |-> admissible[ c ] && GraphDistance[ graph, c, p1 ] + d == GraphDistance[ graph, c, p2 ] ] ],
      extendAfter  = If[ direction === "Backward", { p2 },
        Select[ VertexList[ graph ],
          c |-> admissible[ c ] && GraphDistance[ graph, c, p2 ] + d == GraphDistance[ graph, p1, c ] ] ] },
    { validPairs = Select[ Tuples[ { extendBefore, extendAfter } ],
        pair |-> GraphDistance[ graph, pair[[1]], pair[[2]] ] ==
                 GraphDistance[ graph, pair[[1]], p1 ] + d + GraphDistance[ graph, p2, pair[[2]] ] ] },
    { maxPairs = MaximalBy[ validPairs,
        GraphDistance[ graph, #[[1]], p1 ] + GraphDistance[ graph, p2, #[[2]] ] & ] },
    If[ maxPairs === { } || maxPairs === { { p1, p2 } }, { segment },
      Flatten[
        With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
          { db = GraphDistance[ graph, s, p1 ], da = GraphDistance[ graph, p2, e ] },
          { bp = If[ db == 0, { {} },
                  Most /@ Select[
                    applyPruning[ FindPath[ graph, s, p1, { db }, All ], pruning ],
                    AllTrue[ #, admissible ] & ] ],
            ap = If[ da == 0, { {} },
                  Rest /@ Select[
                    applyPruning[ FindPath[ graph, p2, e, { da }, All ], pruning ],
                    AllTrue[ #, admissible ] & ] ] },
          Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ]
        ] & /@ maxPairs,
        1 ]
    ]
  ]


(* Greedy maximal geodesic extension: walk vertex-by-vertex outward from each
   endpoint, accepting the first neighbor that extends the geodesic by one
   step.  Returns exactly one chain -- maximally inextensible but not
   necessarily of maximum total length. *)

findLineExtensionsGreedy[ graph_Graph, segment_List ] :=
  findLineExtensionsGreedy[ graph, segment, "BothSides" ]

findLineExtensionsGreedy[ graph_Graph, segment_List, direction_String ] :=
  findLineExtensionsGreedy[ graph, segment, True &, direction ]

findLineExtensionsGreedy[ graph_Graph, segment_List, admissible_,
    direction_String : "BothSides" ] /; ! StringQ[ admissible ] :=
  { greedyWalkDirection[ graph, segment, admissible, direction ] }

greedyWalkDirection[ graph_Graph, segment_List, admissible_, direction_String ] /;
    Length[ segment ] < 2 := segment

greedyWalkDirection[ graph_Graph, segment_List, admissible_, direction_String ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ],
          d  = GraphDistance[ graph, First[ segment ], Last[ segment ] ] },
    Switch[ direction,
      "BothSides",
        Join[ Reverse @ greedyWalk[ graph, p1, p2, d, admissible ],
              segment,
              greedyWalk[ graph, p2, p1, d, admissible ] ],
      "Forward",
        Join[ segment, greedyWalk[ graph, p2, p1, d, admissible ] ],
      "Backward",
        Join[ Reverse @ greedyWalk[ graph, p1, p2, d, admissible ], segment ]
    ]
  ]


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
  spreadFind[ InfraLine, count,
    { line0, p0 } |-> Catch @ With[ {
        properties = OptionValue[ FindInfraParallel, { opts }, Properties ],
        methodSpec = OptionValue[ FindInfraParallel, { opts }, Method ] /. Automatic -> "Exhaustive" },
      If[ properties =!= { },
        Message[ FindInfraParallel::badproperty, properties ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec,
              pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        Switch[ methodHead,
          "Exhaustive", findParallelExtensions[ graph, line0, p0, pruning ],
          "Greedy",     findParallelExtensionsGreedy[ graph, line0, p0 ],
          _,            Message[ FindInfraParallel::badmethod, methodSpec ]; Throw[ $Failed ]
        ]
      ]
    ], line, p ]


(* Maximal level-set geodesics through p: every vertex of the result lies at
   distance r = d(p, line) from line, and the chain is a geodesic in graph.
   Seeded by each level-set neighbor of p, extended on both sides via the
   line-extension machinery with an extra admissibility predicate. *)

findParallelExtensions[ graph_Graph, line_List, p_, pruning_ : Infinity ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    { r = lineDist[ p ] },
    If[ r === Infinity, { },
      With[ { admissible = c |-> lineDist[ c ] == r },
        { seeds = Select[ AdjacencyList[ graph, p ], admissible ] },
        { chains = Flatten[
            findLineExtensionsWith[ graph, { p, # }, pruning, admissible ] & /@ seeds,
            1 ] },
        DeleteDuplicates @ Map[ canonicalLine, Select[ chains, Length[ # ] >= 2 & ] ]
      ]
    ]
  ]


findParallelExtensionsGreedy[ graph_Graph, line_List, p_ ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    { r = lineDist[ p ] },
    If[ r === Infinity, { },
      With[ { admissible = c |-> lineDist[ c ] == r },
        { seed = SelectFirst[ AdjacencyList[ graph, p ], admissible, Missing[] ] },
        If[ MissingQ[ seed ], { { p } },
          { greedyWalkDirection[ graph, { p, seed }, admissible, "BothSides" ] } ]
      ]
    ]
  ]


(* ===================== Sketch: Method dispatch (NOT WIRED) =====================
   Two honest, computable parallelism criteria; see Wiki/Concepts/Parallelism.md
   for the design rationale.

     (E) Equidistant   -- the current implementation (level-set construction).
                          phi_{L1}(v) := Min[d(v, u) : u in L1] is constant on L2.
                          A Euclidean *theorem* used as a graph *definition*.

     (T) Transversal   -- Euclid I.27 / I.29. Pick the shortest path t between
                          L1 and L2; the angle t makes with L1 at its L1-end
                          equals the angle t makes with L2 at its L2-end.

   Planned signature:

     Method -> "Equidistant" (default, current behaviour) |
               "Transversal" (Euclid I.27)

   InfraParallelQ would gain a _Graph overload because the transversal test
   needs the graph itself, not just the distance matrix.

   Sketch of the transversal branch (Euclid I.27, alternate-angle equality):

     findTransversalParallel[ graph_Graph, line_List, p_ ] :=
       Module[ { pencil, lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
         pencil = #[[ 1, 1 ]] & /@ FindInfraLine[ graph, p, All ];
         Select[ pencil, candidate |->
           DisjointQ[ candidate, line ] &&
           transversalAngleEqualQ[ graph, line, candidate ] ]
       ]

     transversalAngleEqualQ[ graph_Graph, l1_List, l2_List ] :=
       Module[ { dm, minPair, a, b, ap, bp, alpha, beta },
         dm = Outer[ GraphDistance[ graph, #1, #2 ] &, l1, l2 ];
         minPair = First @ Position[ dm, Min @@ Flatten @ dm ];
         a  = l1[[ minPair[[ 1 ]] ]];
         b  = l2[[ minPair[[ 2 ]] ]];
         ap = l1[[ If[ minPair[[ 1 ]] == Length[ l1 ],
                       minPair[[ 1 ]] - 1, minPair[[ 1 ]] + 1 ] ]];
         bp = l2[[ If[ minPair[[ 2 ]] == Length[ l2 ],
                       minPair[[ 2 ]] - 1, minPair[[ 2 ]] + 1 ] ]];
         alpha = InfraAngle[ graph, { ap, a, b } ];
         beta  = InfraAngle[ graph, { bp, b, a } ];
         alpha == beta
       ]

   Edge cases to settle on implementation:
     - Non-unique shortest transversal: require equality for all of them.
     - Orientation of {ap, bp} ("same side of the transversal"): the discrete
       analogue of Euclid's "alternate interior" is unresolved. Provisional
       choice in the sketch: pick the unique next-along-line vertex.
     - Lines of length 1 (no a' / b'): skip the anchor and try the next pair.
     - alpha == beta is a number equality with InfraAngle's "Arclength" measure;
       a tolerance form alpha - beta is below threshold may be wanted.

   Worked numbers in Wiki/Concepts/Parallelism.md:
     - GridGraph[{6,6}], L1 = {1..6}, L2 = {7..12}:
         (E) True; alpha = beta = 2  --> (T) True. Both agree.
     - PetersenGraph[], L1 = {1, 4, 2}, L2 = {3, 8, 7}:
         (E) False; alpha = beta = 3  --> (T) True. The criteria diverge.

   ============================================================================= *)


(* ===================== FindInfraPerpendicular ===================== *)

(* Perpendicular line(s) at `point` to `line`.  Returns InfraLine realisations
   through `point`.  "Metric" (default): foot-of-perpendicular construction
   (Euclid I.12, isosceles base midpoint) -- for each pair {a, b} of L-vertices
   equidistant from p, the midpoint of the line-arc from a to b is a candidate
   foot; the perpendicular line is the maximal geodesic through `point` and the
   foot.  "Projection" / "Coordinate" / "Arclength" / "Alexandrov" /
   {"Alexandrov", "Curvature" -> k}: enumerate maximal geodesics through
   `point` and select those passing InfraPerpendicularQ with the named method.
   Method-specific sub-options ("Equality", "ZeroTest", "Tolerance",
   "Curvature") are carried inside the Method spec and forwarded verbatim to
   InfraPerpendicularQ -- see its usage.  Option "Radius" -> All (default) |
   r_Integer localises both the candidate enumeration (workGraph =
   NeighborhoodGraph[g, point, r]) and the test. *)

FindInfraPerpendicular::badmethod = "Method `1` is not supported by FindInfraPerpendicular.";

Options[ FindInfraPerpendicular ] = {
  Method   -> "Metric",
  "Radius" -> All
};

FindInfraPerpendicular[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[] ] :=
  spreadFind[ InfraLine, count,
    { line0, point0 } |-> With[ {
        spec   = OptionValue[ FindInfraPerpendicular, { opts }, Method   ],
        radius = OptionValue[ FindInfraPerpendicular, { opts }, "Radius" ] },
      { workGraph = If[ radius === All, graph, NeighborhoodGraph[ graph, point0, radius ] ] },
      Switch[ methodName @ spec,
        "Metric",     perpendicularByMetric[ workGraph, line0, point0 ],
        "Projection" | "Coordinate" | "Arclength" | "Alexandrov",
                      perpendicularByQ[ workGraph, graph, line0, point0, spec, radius ],
        _,            Message[ FindInfraPerpendicular::badmethod, spec ]; $Failed
      ]
    ], line, point ]


(* "Metric" recipe: feet via isosceles base midpoint, then maximal lines through
   each foot and `point`.  Line restricted to vertices in workGraph. *)

(* Euclid I.12 feet only: isosceles base midpoints of equidistant pairs on
   line, point removed.  No line extension. *)

perpendicularFeet[ workGraph_Graph, line_List, point_ ] :=
  With[ { localLine = Select[ line, MemberQ[ VertexList[ workGraph ], # ] & ] },
    { distances = GraphDistance[ workGraph, point, # ] & /@ localLine },
    DeleteCases[ DeleteDuplicates @ Flatten[
      ( group |-> Map[
          pair |-> With[ { lo = Min @@ pair, hi = Max @@ pair },
            localLine[[ lo ;; hi ]][[ Ceiling[ ( hi - lo + 1 ) / 2 ] ]] ],
          Subsets[ group, { 2 } ] ]
      ) /@ Values @ GroupBy[ Range @ Length @ localLine, distances[[ # ]] & ],
      1 ], point ]
  ]

perpendicularByMetric[ workGraph_Graph, line_List, point_ ] :=
  With[ { feet = perpendicularFeet[ workGraph, line, point ] },
    Select[
      DeleteDuplicates @ Map[ canonicalLine, Catenate[
        Map[ foot |-> Catenate[
          findLineExtensions[ workGraph, # ] & /@ allGeodesics[ workGraph, foot, point ] ],
          feet ] ] ],
      Length[ # ] >= 2 & ]
  ]


(* Q-side recipe: candidates = canonical maximal geodesics through `point` in
   workGraph; selection via InfraPerpendicularQ on the original graph with
   "Radius" -> r so the test localises identically. *)

perpendicularByQ[ workGraph_Graph, graph_Graph, line_, point_, spec_, radius_ ] :=
  With[ { candidates = DeleteDuplicates @ Map[ canonicalLine, Catenate[
      Map[ neighbor |-> findLineExtensions[ workGraph, { point, neighbor } ],
        AdjacencyList[ workGraph, point ] ] ] ] },
    Select[ candidates,
      InfraPerpendicularQ[ graph, line, #,
        Method   -> spec,
        "Radius" -> radius ] & ]
  ]


(* ===================== FindInfraCommonLine ===================== *)

(* Canonical maximal geodesics through every vertex in verts. *)

FindInfraCommonLine[ graph_Graph, verts_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1 ] :=
  With[ { uverts = DeleteDuplicates @ Catenate[ infraVertexSet /@ verts ] },
    { common = If[ Length[ uverts ] < 2, { },
        DeleteDuplicates @ Select[
          canonicalLine[ #[[ 1, 1 ]] ] & /@ FindInfraLine[ graph, First @ uverts, uverts[[ 2 ]], All ],
          line |-> SubsetQ[ line, uverts ] ] ] },
    { capped = infraCap[ common, count ] },
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


(* ===================== InfraPerpendicularQ ===================== *)

(* Two lines are perpendicular if, at every common vertex p, the chosen test
   holds.  Tolerance / Equality / ZeroTest are carried as sub-options of
   the Method spec (so each tolerance has a clear per-method meaning):

   - "Projection" (default): the foot-of-perpendicular projection of one line
     onto the other compared to the intersection set.  Sub-option "Equality"
     selects the comparison: "Subset" (default; the natural geometric test,
     proj subset of intersection) or one of the InfraEqualQ methods "Set" /
     "Multiset" / "Diffuse" / "Overlap" (stricter / looser alternatives).

   - "Arclength" / "Alexandrov" / {"Alexandrov", "Curvature" -> k, ...}: split
     each line at p into left / right halves, pick the far endpoint of each
     half as a direction representative, and require the four corner-wedge
     angles InfraAngle[..., {a_pm, p, b_pm}, Method -> mtd] to be equal
     within sub-option "Tolerance" (default 0; in radians).  In Euclidean
     the wedges sum to 2 Pi so equality forces Pi/2; on a graph the
     synthetic angle is not perfectly additive around p, so equality at a
     common value != Pi/2 is the honest right-angle test.

   - "Coordinate": project each non-common vertex of one line onto the
     other (FindClosestInfraPoint, metric argmin) to get a signed coordinate
     along the receiving line, relative to p.  Sub-option "ZeroTest" is the
     predicate that decides whether the resulting coordinate cloud is at 0;
     values: "Mean" (default; |Mean[c]| <= tol), "Median" (|Median[c]| <= tol
     -- robust to outliers), "Contains" (Min[c] - tol <= 0 <= Max[c] + tol
     -- 0 lies in the coordinate range), a string spec with its own
     "Tolerance" sub-option (e.g. {"Mean", "Tolerance" -> 0.2}), or any user
     predicate function f (f[c] called directly and the result coerced to
     True / False).  The default "Tolerance" on the named ZeroTests is 0.

   Top-level option "Radius" -> All (default) | k_Integer restricts each
   test to a k-neighborhood of the common vertex via NeighborhoodGraph. *)

InfraPerpendicularQ::badmethod   = "Method `1` is not supported by InfraPerpendicularQ.";
InfraPerpendicularQ::badzerotest = "ZeroTest `1` is not supported by InfraPerpendicularQ \"Coordinate\".";

Options[ InfraPerpendicularQ ] = {
  Method   -> "Projection",
  "Radius" -> All
};

InfraPerpendicularQ[ graph_Graph, l1_, l2_, OptionsPattern[] ] :=
  With[ { seq1 = lineSequence[ l1 ], seq2 = lineSequence[ l2 ],
          mtd  = OptionValue[ Method ], radius = OptionValue[ "Radius" ] },
    { common = Intersection[ seq1, seq2 ],
      mtdHead = methodName @ mtd, mtdOpts = methodOptions @ mtd },
    Which[
      Length[ common ] == 0, False,
      mtdHead === "Projection",
        With[ { equality = Lookup[ mtdOpts, "Equality", "Subset" ] },
          AllTrue[ common, perpendicularAtProjection[ graph, seq1, seq2, #, equality, radius ] & ] ],
      mtdHead === "Coordinate",
        With[ { zeroTest = Lookup[ mtdOpts, "ZeroTest", "Mean" ] },
          AllTrue[ common, perpendicularAtCoordinate[ graph, seq1, seq2, #, zeroTest, radius ] & ] ],
      MemberQ[ { "Arclength", "Alexandrov" }, mtdHead ],
        With[ { tol = Lookup[ mtdOpts, "Tolerance", 0 ] },
          AllTrue[ common, perpendicularAtAngle[ graph, seq1, seq2, #, mtd, tol, radius ] & ] ],
      True, Message[ InfraPerpendicularQ::badmethod, mtd ]; $Failed
    ]
  ]


(* Ordered first-realisation vertex sequence; bare list is its own sequence.
   Companion to linePointSet (which gives the unordered union). *)

lineSequence[ ( InfraLine | InfraSegment | InfraPath | InfraRay )[ reps_List ] ] := First @ reps
lineSequence[ line_List ] := line


perpendicularAtProjection[ g_Graph, seq1_List, seq2_List, p_, equality_, radius_ ] :=
  Module[ { localG, ball, localSeq1, localSeq2, localCommon, proj12, proj21, compare },
    localG    = If[ radius === All, g, NeighborhoodGraph[ g, p, radius ] ];
    ball      = If[ radius === All, All, VertexList @ localG ];
    localSeq1 = If[ ball === All, seq1, Select[ seq1, MemberQ[ ball, # ] & ] ];
    localSeq2 = If[ ball === All, seq2, Select[ seq2, MemberQ[ ball, # ] & ] ];
    localCommon = Intersection[ localSeq1, localSeq2 ];
    proj12 = DeleteDuplicates @ Flatten[
      perpendicularFeet[ localG, localSeq1, # ] & /@ Complement[ localSeq2, localCommon ] ];
    proj21 = DeleteDuplicates @ Flatten[
      perpendicularFeet[ localG, localSeq2, # ] & /@ Complement[ localSeq1, localCommon ] ];
    compare = If[ equality === "Subset",
      proj |-> SubsetQ[ localCommon, proj ],
      proj |-> InfraEqualQ[ g, InfraPoint[ proj ], InfraPoint[ localCommon ], Method -> equality ] ];
    compare[ proj12 ] && compare[ proj21 ]
  ]


(* Four-corner symmetric perpendicularity at p: split each line at p into
   left/right halves (truncated to the local NeighborhoodGraph), pick the
   far endpoint of each half as a direction representative, and require the
   four corner-wedge angles at p to be equal within "Tolerance".  In a
   Euclidean limit the wedges sum to 2 Pi so equality forces Pi/2; on a
   graph the synthetic angle is not perfectly additive around p, so the
   equality at a common value != Pi/2 is the honest right-angle test. *)

perpendicularAtAngle[ g_Graph, seq1_List, seq2_List, p_, mtd_, tol_, radius_ ] :=
  Module[ { localG, ball, s1, s2, i1, i2, h1L, h1R, h2L, h2R, angles },
    localG = If[ radius === All, g, NeighborhoodGraph[ g, p, radius ] ];
    ball   = If[ radius === All, All, VertexList @ localG ];
    s1     = If[ ball === All, seq1, Select[ seq1, MemberQ[ ball, # ] & ] ];
    s2     = If[ ball === All, seq2, Select[ seq2, MemberQ[ ball, # ] & ] ];
    i1     = FirstPosition[ s1, p, { 0 }, { 1 }, Heads -> False ][[ 1 ]];
    i2     = FirstPosition[ s2, p, { 0 }, { 1 }, Heads -> False ][[ 1 ]];
    If[ i1 == 0 || i2 == 0, Return[ False, Module ] ];
    h1L = s1[[ ;; i1 - 1 ]]; h1R = s1[[ i1 + 1 ;; ]];
    h2L = s2[[ ;; i2 - 1 ]]; h2R = s2[[ i2 + 1 ;; ]];
    If[ h1L === { } || h1R === { } || h2L === { } || h2R === { },
      Return[ False, Module ] ];
    angles = InfraAngle[ localG, #, Method -> mtd ] & /@
      { { First[ h1L ], p, First[ h2L ] },
        { First[ h1L ], p, Last [ h2R ] },
        { Last [ h1R ], p, First[ h2L ] },
        { Last [ h1R ], p, Last [ h2R ] } };
    Max[ angles ] - Min[ angles ] <= tol
  ]


(* Coordinate perpendicularity at p: project each non-common vertex of one
   line onto the other (FindClosestInfraPoint, metric argmin) to get a
   signed coordinate along the receiving line, relative to p's position;
   tie-feet averaged.  Pass iff the chosen ZeroTest indicates the cloud is
   at 0 in both directions -- the projection feet are *balanced* around p,
   not (as in "Projection") *contained* in the intersection set.  ZeroTest
   spec: a bare name ("Mean" / "Median" / "Contains") uses Tolerance 0; the
   nested form {name, "Tolerance" -> t} carries its own tolerance; an
   arbitrary user predicate function f is called directly on the coordinate
   list and the result coerced to True / False.  Test bodies:
     "Mean"     -- Abs[ Mean[c] ]   <= tol
     "Median"   -- Abs[ Median[c] ] <= tol
     "Contains" -- Min[c] - tol <= 0 <= Max[c] + tol. *)

perpendicularAtCoordinate[ g_Graph, seq1_List, seq2_List, p_, zeroTest_, radius_ ] :=
  Module[ { localG, ball, s1, s2, i1, i2, signedCoord, c12, c21,
            ztHead, ztTol },
    localG = If[ radius === All, g, NeighborhoodGraph[ g, p, radius ] ];
    ball   = If[ radius === All, All, VertexList @ localG ];
    s1     = If[ ball === All, seq1, Select[ seq1, MemberQ[ ball, # ] & ] ];
    s2     = If[ ball === All, seq2, Select[ seq2, MemberQ[ ball, # ] & ] ];
    i1     = FirstPosition[ s1, p, { 0 }, { 1 }, Heads -> False ][[ 1 ]];
    i2     = FirstPosition[ s2, p, { 0 }, { 1 }, Heads -> False ][[ 1 ]];
    If[ i1 == 0 || i2 == 0, Return[ False, Module ] ];
    signedCoord[ seq_, pIdx_, v_ ] :=
      With[ { feet = #[[ 1, 1 ]] & /@ FindClosestInfraPoint[ localG, seq, v, All ] },
        Mean[ ( FirstPosition[ seq, #, { 0 }, { 1 }, Heads -> False ][[ 1 ]] - pIdx ) & /@ feet ]
      ];
    c12 = signedCoord[ s1, i1, # ] & /@ DeleteCases[ s2, p ];
    c21 = signedCoord[ s2, i2, # ] & /@ DeleteCases[ s1, p ];
    If[ Length[ c12 ] == 0 || Length[ c21 ] == 0, Return[ False, Module ] ];
    If[ StringQ[ zeroTest ] || MatchQ[ zeroTest, { _String, ___ } ],
      ztHead = methodName    @ zeroTest;
      ztTol  = Lookup[ methodOptions @ zeroTest, "Tolerance", 0 ];
      Switch[ ztHead,
        "Mean",     Abs[ N @ Mean[ c12 ] ]   <= ztTol && Abs[ N @ Mean[ c21 ] ]   <= ztTol,
        "Median",   Abs[ N @ Median[ c12 ] ] <= ztTol && Abs[ N @ Median[ c21 ] ] <= ztTol,
        "Contains", Min[ c12 ] - ztTol <= 0 <= Max[ c12 ] + ztTol
                 && Min[ c21 ] - ztTol <= 0 <= Max[ c21 ] + ztTol,
        _,          Message[ InfraPerpendicularQ::badzerotest, zeroTest ]; False
      ],
      (* user-supplied predicate function *)
      TrueQ @ zeroTest[ c12 ] && TrueQ @ zeroTest[ c21 ]
    ]
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


(* ===================== FindLineHull / LineHullQ ===================== *)

(* Line hull of S: the smallest superset closed under the line operator
   L(u, v) = the maximal geodesics through u, v, returned as an InfraSet.
   Equivalently, grow T by every line meeting T in >= 2 vertices, to a fixed
   point.  The De Bruijn-Erdos / Chen-Chvatal collinearity closure; cf. the
   segment hull (metric intervals) in MetricAlgebra.wl and the ball hull
   (intersection of balls) in InfraBall.wl.  S is any Infra* object, a list of
   them, or a bare vertex list.  Option "LineStructure": None (default) closes
   under all maximal geodesics; an InfraLineStructure (or a bare list of lines)
   closes under that fixed family instead -- still a unique fixed point. *)

Options[ FindLineHull ] = { "LineStructure" -> None };

FindLineHull[ graph_Graph, s : Except[ _Rule | _RuleDelayed ], OptionsPattern[] ] :=
  With[ { lines = Replace[ OptionValue[ "LineStructure" ],
            { None -> allCanonicalLines @ graph, ls_InfraLineStructure :> ls[ "Lines" ] } ],
          S = hullVertices @ s },
    InfraSet @ FixedPoint[
      T |-> Union[ T, Catenate @ Select[ lines, Length @ Intersection[ #, T ] >= 2 & ] ],
      Union @ S
    ]
  ]

(* S is line-closed: it already contains every line (of the chosen family)
   that meets it in two or more vertices. *)

Options[ LineHullQ ] = { "LineStructure" -> None };

LineHullQ[ graph_Graph, s : Except[ _Rule | _RuleDelayed ], opts : OptionsPattern[] ] :=
  With[ { vs = hullVertices @ s }, FindLineHull[ graph, vs, opts ][ "Vertices" ] === Union @ vs ]


(* ===================== UniversalLineQ ===================== *)

(* The Chen-Chvatal line through u, v is L(u, v) = the union of all maximal
   geodesics through both (the metrically-collinear vertices).  A line is
   universal when it fills a whole connected component (De Bruijn-Erdos /
   Chen-Chvatal).  UniversalLineQ[g, {u, v}] tests the single pair;
   UniversalLineQ[g] asks whether any pair spans a universal line. *)

UniversalLineQ[ graph_Graph, { u_, v_ } ] :=
  AnyTrue[ ConnectedComponents @ graph,
    c |-> ContainsAll[ c, { u, v } ] &&
      Union @ Catenate @ Select[ allCanonicalLines @ graph, ContainsAll[ #, { u, v } ] & ] === Sort @ c ]

UniversalLineQ[ graph_Graph ] :=
  AnyTrue[ Subsets[ VertexList @ graph, { 2 } ], UniversalLineQ[ graph, # ] & ]


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
