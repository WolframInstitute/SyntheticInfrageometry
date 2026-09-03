Package["WolframInstitute`SyntheticInfrageometry`"]

PackageImport["WolframInstitute`Infrageometry`"]

PackageScope[findLineExtensions]
PackageScope[findLineExtensionsWith]
PackageScope[findLineExtensionsGreedy]
PackageScope[canonicalLine]
PackageScope[allCanonicalLines]


(* ===================== InfraLine wrapper ===================== *)


InfraLine[ reps_List ][ "Length" ] := ( Length[ # ] - 1 ) & /@ reps

InfraLine /: Part[ InfraLine[ reps_List ], i_Integer ] := columnInfraPoint[ reps, i ]


(* ===================== FindInfraLine ===================== *)


FindInfraLine::badmethod    = "Method `1` is not supported by FindInfraLine.";
FindInfraLine::badproperty  = "Property `1` is not supported by FindInfraLine (FindInfraLine accepts only Properties -> {}).";
FindInfraLine::baddirection = "Direction `1` is not supported by FindInfraLine.";
FindInfraLine::shortfall    = "\"RandomGreedy\" drew `1` distinct lines of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";

Options[ FindInfraLine ] = {
  Properties   -> { },
  Method       -> Automatic,
  "Maximality" -> "Extension",
  "Direction"  -> "BothSides"
};

(* a list can be a genuine vertex (TessellationGraph labels its torus {i, j}), so the guard admits it *)
FindInfraLine[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] /;
    ( ! ListQ[ p1 ] || VertexQ[ graph, p1 ] ) && Head[ p1 ] =!= InfraSegment :=
  spreadFind[ InfraLine, count,
    { q1, q2 } |-> Catch @ With[ {
        properties = OptionValue[ FindInfraLine, { opts }, Properties ],
        methodSpec = resolveMethod[ OptionValue[ FindInfraLine, { opts }, Method ], count ],
        maximality = OptionValue[ FindInfraLine, { opts }, "Maximality" ],
        direction  = OptionValue[ FindInfraLine, { opts }, "Direction" ] },
      If[ properties =!= { },
        Message[ FindInfraLine::badproperty, properties ]; Throw[ $Failed ] ];
      If[ ! MatchQ[ direction, "Forward" | "Backward" | "BothSides" ],
        Message[ FindInfraLine::baddirection, direction ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec,
              pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        (* distinct middles give distinct lines and each yields at least one, so capping both is exact *)
        { cap = If[ maximality === "Diameter", Infinity, countLimit @ count ] },
        { middles = allGeodesics[ graph, q1, q2, cap /. Infinity -> All ] },
        { ext = Union @ Flatten[
            Switch[ methodHead,
              "Exhaustive",   findLineExtensions[ graph, #, pruning, direction, cap ] & /@ middles,
              "Greedy",       findLineExtensionsGreedy[ graph, #, True &, direction, cap ] & /@ middles,
              "ShuffledGreedy", findLineExtensionsGreedy[ graph, #, True &, direction, cap,
                                  RandomSample ] & /@ RandomSample[ middles ],
              (* one draw = a random middle plus a random extension of it *)
              "RandomGreedy", { randomDraws[
                                  { } |-> findLineExtensionsGreedy[ graph, RandomChoice @ middles,
                                    True &, direction, 1, randomBranch ],
                                  count, FindInfraLine ] },
              _,              Message[ FindInfraLine::badmethod, methodSpec ]; Throw[ $Failed ]
            ], 1 ] },
        If[ maximality === "Diameter",
          Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
          ext ]
      ]
    ], p1, p2 ]


FindInfraLine[ graph_Graph, InfraSegment[ dag_Graph ],
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  FindInfraLine[ graph, First @ dagGeodesics[ dag, 1 ], count, opts ]

FindInfraLine[ graph_Graph, InfraSegment[{ walk_List, ___ }],
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  FindInfraLine[ graph, walk, count, opts ]

FindInfraLine[ graph_Graph, segment_List, count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic,
    opts : OptionsPattern[] ] /; Length[ segment ] >= 2 && ! VertexQ[ graph, segment ] :=
  With[ { core = Catch @ With[ {
        properties = OptionValue[ FindInfraLine, { opts }, Properties ],
        methodSpec = resolveMethod[ OptionValue[ FindInfraLine, { opts }, Method ], count ],
        maximality = OptionValue[ FindInfraLine, { opts }, "Maximality" ],
        direction  = OptionValue[ FindInfraLine, { opts }, "Direction" ] },
      If[ properties =!= { },
        Message[ FindInfraLine::badproperty, properties ]; Throw[ $Failed ] ];
      If[ ! MatchQ[ direction, "Forward" | "Backward" | "BothSides" ],
        Message[ FindInfraLine::baddirection, direction ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec,
              pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        (* "Diameter" post-filters, so it must see the whole family *)
        { cap = If[ maximality === "Diameter", Infinity, countLimit @ count ] },
        { ext = Switch[ methodHead,
            "Exhaustive",   findLineExtensions[ graph, segment, pruning, direction, cap ],
            "Greedy",       findLineExtensionsGreedy[ graph, segment, True &, direction, cap ],
            "ShuffledGreedy", findLineExtensionsGreedy[ graph, segment, True &, direction, cap,
                                RandomSample ],
            "RandomGreedy", randomDraws[
                              { } |-> findLineExtensionsGreedy[ graph, segment, True &, direction, 1, randomBranch ],
                              count, FindInfraLine ],
            _,              Message[ FindInfraLine::badmethod, methodSpec ]; Throw[ $Failed ]
          ] },
        If[ maximality === "Diameter",
          Select[ ext, line |-> Length[ line ] - 1 == GraphDiameter[ graph ] ],
          ext ]
      ]
    ] },
    If[ core === $Failed, $Failed, bundleTake[ InfraLine, core, count ] ]
  ]


(* each side extended independently; a pair is joint-geodesic iff d(s, e) == d(s, p1) + d + d(p2, e), and the maxima of b_s + a_e are kept *)

findLineExtensions[ graph_Graph, segment_List, pruning_ : Infinity, direction_String : "BothSides",
    cap : ( _Integer | Infinity ) : Infinity ] :=
  findLineExtensionsWith[ graph, segment, pruning, True &, direction, cap ]


findLineExtensionsWith[ graph_Graph, segment_List, pruning_, admissible_,
    direction_String : "BothSides", cap : ( _Integer | Infinity ) : Infinity ] /;
    Length[ segment ] < 2 :=
  { segment }

findLineExtensionsWith[ graph_Graph, segment_List, pruning_, admissible_,
    direction_String : "BothSides", cap : ( _Integer | Infinity ) : Infinity ] :=
  With[ { p1 = First[ segment ], p2 = Last[ segment ], verts = VertexList[ graph ] },
    (* one compiled all-pairs matrix: the pair enumeration is quadratic, and per-pair GraphDistance re-ran a BFS each time *)
    { dm = GraphDistanceMatrix[ graph ],
      vidx = AssociationThread[ verts, Range @ Length @ verts ] },
    { d1 = dm[[ vidx[ p1 ] ]], d2 = dm[[ vidx[ p2 ] ]] },
    { d = d1[[ vidx[ p2 ] ]] },
    { extendBefore = If[ direction === "Forward", { p1 },
        Select[ verts, c |-> admissible[ c ] && d1[[ vidx[ c ] ]] + d == d2[[ vidx[ c ] ]] ] ],
      extendAfter  = If[ direction === "Backward", { p2 },
        Select[ verts, c |-> admissible[ c ] && d2[[ vidx[ c ] ]] + d == d1[[ vidx[ c ] ]] ] ] },
    { validPairs = Select[ Tuples[ { extendBefore, extendAfter } ],
        pair |-> dm[[ vidx[ pair[[ 1 ]] ], vidx[ pair[[ 2 ]] ] ]] ==
                 d1[[ vidx[ pair[[ 1 ]] ] ]] + d + d2[[ vidx[ pair[[ 2 ]] ] ]] ] },
    (* a finite cap may truncate the tied pairs; capped calls have trivial admissibility, so the post-filter cannot starve FindPath *)
    { maxPairs = With[ { tied = MaximalBy[ validPairs,
          d1[[ vidx[ #[[ 1 ]] ] ]] + d2[[ vidx[ #[[ 2 ]] ] ]] & ] },
        If[ cap === Infinity, tied, Take[ tied, UpTo[ cap ] ] ] ],
      fpCount = If[ cap === Infinity, All, cap ] },
    If[ maxPairs === { } || maxPairs === { { p1, p2 } }, { segment },
      With[ { lines = Flatten[
          With[ { s = #[[ 1 ]], e = #[[ 2 ]] },
            { db = d1[[ vidx[ s ] ]], da = d2[[ vidx[ e ] ]] },
            { bp = If[ db == 0, { {} },
                    Most /@ Select[
                      applyPruning[ cappedGeodesics[ graph, s, p1, db, fpCount ], pruning ],
                      AllTrue[ #, admissible ] & ] ],
              ap = If[ da == 0, { {} },
                    Rest /@ Select[
                      applyPruning[ cappedGeodesics[ graph, p2, e, da, fpCount ], pruning ],
                      AllTrue[ #, admissible ] & ] ] },
            Flatten[ Outer[ Join[ #1, segment, #2 ] &, bp, ap, 1 ], 1 ]
          ] & /@ maxPairs,
          1 ] },
        If[ cap === Infinity, lines, Take[ lines, UpTo[ cap ] ] ] ]
    ]
  ]


(* FindPath for the whole family, the geodesic-DAG lazy DFS when capped: FindPath's exact-length DFS can wander for seconds *)

cappedGeodesics[ graph_Graph, u_, v_, len_, All ] :=
  FindPath[ graph, u, v, { len }, All ]

cappedGeodesics[ graph_Graph, u_, v_, len_, n_Integer ] :=
  dagGeodesics[ GeodesicIntervalGraph[ graph, u, v ], n ]


(* grow the chain outward one admissible neighbour at a time, backtracking on exhaustion: complete, so a finite count is exact. The two sides cannot grow against the ORIGINAL endpoints, so the cross-distance is re-derived from the live frontiers at every step *)

findLineExtensionsGreedy[ graph_Graph, segment_List, admissible_, direction_String, count_,
    branch_ : Identity ] /; Length[ segment ] < 2 := { segment }

findLineExtensionsGreedy[ graph_Graph, segment_List, admissible_, direction_String, count_,
    branch_ : Identity ] :=
  (* Module locals rather than inlined into the recursive RHS: see greedyFrontierSweep in Tools.wl *)
  Module[ { cap = countLimit @ count, acc = { }, outward, grow,
            admitQ = admissible, pick = branch },
    outward[ h_, other_ ] :=
      With[ { d = GraphDistance[ graph, h, other ] },
        Select[ AdjacencyList[ graph, h ],
          c |-> admitQ[ c ] && GraphDistance[ graph, c, other ] == d + 1 ] ];
    grow[ left_, right_ ] :=
      With[ { hL = If[ left === { }, First @ segment, First @ left ],
              hR = If[ right === { }, Last @ segment, Last @ right ] },
        { leftCands = If[ direction === "Forward", { }, outward[ hL, hR ] ] },
        { leftSteps = If[ leftCands === { }, { left },
            ( Prepend[ left, # ] & ) /@ pick @ leftCands ] },
        Scan[
          newLeft |-> With[ { newHL = If[ newLeft === { }, First @ segment, First @ newLeft ] },
            { rightCands = If[ direction === "Backward", { }, outward[ hR, newHL ] ] },
            { chain = Join[ newLeft, segment, right ] },
            Which[
              leftCands === { } && rightCands === { },
                If[ ! MemberQ[ acc, chain ],
                  AppendTo[ acc, chain ];
                  If[ Length @ acc >= cap, Throw[ acc, findLineExtensionsGreedy ] ] ],
              rightCands === { },
                grow[ newLeft, right ],
              True,
                Scan[ v |-> grow[ newLeft, Append[ right, v ] ], pick @ rightCands ] ] ],
          leftSteps ]
      ];
    Catch[ grow[ { }, { } ]; acc, findLineExtensionsGreedy ]
  ]


(* ===================== FindInfraParallel ===================== *)


FindInfraParallel::badmethod   = "Method `1` is not supported by FindInfraParallel.";
FindInfraParallel::badproperty = "Property `1` is not supported by FindInfraParallel (FindInfraParallel accepts only Properties -> {}).";
FindInfraParallel::shortfall   = "\"RandomGreedy\" drew `1` distinct parallels of the `2` requested before exhausting its retry budget; use Method -> \"Exhaustive\" for the exact class.";

Options[ FindInfraParallel ] = {
  Properties -> { },
  Method     -> Automatic
};

FindInfraParallel[ graph_Graph, line_, p_,
    count : ( _Integer | UpTo[ _Integer ] | All | Automatic ) : Automatic, opts : OptionsPattern[] ] :=
  spreadFind[ InfraLine, count,
    { line0, p0 } |-> Catch @ With[ {
        properties = OptionValue[ FindInfraParallel, { opts }, Properties ],
        methodSpec = resolveMethod[ OptionValue[ FindInfraParallel, { opts }, Method ], count ] },
      If[ properties =!= { },
        Message[ FindInfraParallel::badproperty, properties ]; Throw[ $Failed ] ];
      With[ { methodHead = methodName @ methodSpec,
              pruning    = "Pruning" /. propertiesSubOpts[ methodSpec ] /. "Pruning" -> Infinity },
        Switch[ methodHead,
          "Exhaustive",   findParallelExtensions[ graph, line0, p0, pruning ],
          "Greedy",       findParallelExtensionsGreedy[ graph, line0, p0, count ],
          "ShuffledGreedy", findParallelExtensionsGreedy[ graph, line0, p0, count, RandomSample ],
          "RandomGreedy", randomDraws[
                            { } |-> findParallelExtensionsGreedy[ graph, line0, p0, 1, randomBranch ],
                            count, FindInfraParallel ],
          _,              Message[ FindInfraParallel::badmethod, methodSpec ]; Throw[ $Failed ]
        ]
      ]
    ], line, p ]


(* every vertex of the result lies at distance r = d(p, line) from line, and the chain is a geodesic in graph *)

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


findParallelExtensionsGreedy[ graph_Graph, line_List, p_, count_, branch_ : Identity ] :=
  With[ { lineDist = v |-> Min[ GraphDistance[ graph, v, # ] & /@ line ] },
    { r = lineDist[ p ] },
    If[ r === Infinity, { },
      With[ { admissible = c |-> lineDist[ c ] == r },
        { seeds = Select[ AdjacencyList[ graph, p ], admissible ] },
        If[ seeds === { }, { { p } },
          takeUpTo[
            DeleteDuplicates @ Flatten[
              ( s |-> findLineExtensionsGreedy[ graph, { p, s }, admissible, "BothSides", count, branch ] ) /@
                branch @ seeds,
              1 ],
            countLimit @ count ] ]
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


FindInfraPerpendicular::badmethod = "Method `1` is not supported by FindInfraPerpendicular.";

Options[ FindInfraPerpendicular ] = {
  Method   -> "Metric",
  "Radius" -> All
};

FindInfraPerpendicular[ graph_Graph, line_, point_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All, opts : OptionsPattern[] ] :=
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


(* Euclid I.12: the feet are the isosceles base midpoints of the pairs equidistant from point *)

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


FindInfraCommonLine[ graph_Graph, verts_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : All ] :=
  With[ { uverts = DeleteDuplicates @ Catenate[ infraVertexSet /@ verts ] },
    { common = If[ Length[ uverts ] < 2, { },
        DeleteDuplicates @ Select[
          canonicalLine /@ FindInfraLine[ graph, First @ uverts, uverts[[ 2 ]], All ][ "Realizations" ],
          line |-> SubsetQ[ line, uverts ] ] ] },
    bundleTake[ InfraLine, common, count ]
  ]


(* ===================== InfraLineQ ===================== *)

(* A segment is a line iff no extension preserves the geodesic property. *)

InfraLineQ[ graph_Graph, line_InfraLine ] :=
  AllTrue[ First @ line, InfraLineQ[ graph, # ] & ]

InfraLineQ[ graph_Graph, segment_List ] :=
  InfraSegmentQ[ graph, segment ] &&
  Length[ First @ findLineExtensions[ graph, segment ] ] == Length[ segment ]


(* ===================== InfraParallelQ ===================== *)

(* l1, l2 disjoint and d(v, l2) constant over v in l1, up to threshold *)

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

InfraParallelQ[ graph_Graph,
    l1 : _InfraLine | _InfraSegment | _InfraWalk | _InfraRay | _List,
    l2 : _InfraLine | _InfraSegment | _InfraWalk | _InfraRay | _List,
    threshold_ : 0 ] /; ! MatchQ[ { l1, l2 }, { _List, _List } ] :=
  With[ { reps1 = If[ ListQ @ l1, { l1 }, First @ l1 ],
          reps2 = If[ ListQ @ l2, { l2 }, First @ l2 ] },
    AllTrue[ Tuples[ { reps1, reps2 } ],
      pair |-> InfraParallelQ[ graph, pair[[ 1 ]], pair[[ 2 ]], threshold ] ]
  ]


(* ===================== InfraPerpendicularQ ===================== *)


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


lineSequence[ ( InfraLine | InfraSegment | InfraWalk | InfraRay )[ reps_List ] ] := First @ reps
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
      proj |-> InfraEqualQ[ g, InfraSet[ proj ], InfraSet[ localCommon ], Method -> equality ] ];
    compare[ proj12 ] && compare[ proj21 ]
  ]


(* the four corner-wedge angles at p equal: in the Euclidean limit they sum to 2 Pi so equality forces Pi/2, and on a graph the angle is not additive around p, so equality at a common value is the honest test *)

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


(* signed coordinates of the projection feet along the receiving line: perpendicular iff the cloud is balanced around p, rather than contained in the intersection as in "Projection" *)

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
      With[ { feet = #[ "Vertex" ] & /@ FindClosestInfraPoint[ localG, seq, v, All ] },
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


PencilDirections[ graph_Graph, origin_ ] :=
  DeleteDuplicates @ Map[ canonicalLine, Flatten[
    FindInfraLine[ graph, origin, #, All ][ "Realizations" ] & /@
      DeleteCases[ VertexList[ graph ], origin ],
    1 ] ]

PencilCardinality[ graph_Graph, origin_ ] := Length @ PencilDirections[ graph, origin ]

LineCount[ graph_Graph ] := Length @ allCanonicalLines[ graph ]


(* ===================== FindLineHull / LineHullQ ===================== *)

(* the smallest superset closed under L(u, v) = the maximal geodesics through u, v: the De Bruijn-Erdos / Chen-Chvatal collinearity closure *)

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


Options[ LineHullQ ] = { "LineStructure" -> None };

LineHullQ[ graph_Graph, s : Except[ _Rule | _RuleDelayed ], opts : OptionsPattern[] ] :=
  With[ { vs = hullVertices @ s }, FindLineHull[ graph, vs, opts ][ "Vertices" ] === Union @ vs ]


(* ===================== UniversalLineQ ===================== *)

(* the Chen-Chvatal line L(u, v) = the union of all maximal geodesics through both; universal when it fills a connected component *)

UniversalLineQ[ graph_Graph, { u_, v_ } ] :=
  AnyTrue[ ConnectedComponents @ graph,
    c |-> ContainsAll[ c, { u, v } ] &&
      Union @ Catenate @ Select[ allCanonicalLines @ graph, ContainsAll[ #, { u, v } ] & ] === Sort @ c ]

UniversalLineQ[ graph_Graph ] :=
  AnyTrue[ Subsets[ VertexList @ graph, { 2 } ], UniversalLineQ[ graph, # ] & ]


(* ===================== Helpers: canonical lines ===================== *)

(* the lexicographic minimum of a line and its reversal *)

canonicalLine[ line_List ] := First @ Sort @ { line, Reverse[ line ] }

allCanonicalLines[ graph_Graph ] :=
  DeleteDuplicates @ Flatten[
    canonicalLine /@ FindInfraLine[ graph, #[[ 1 ]], #[[ 2 ]], All ][ "Realizations" ] & /@
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
      FindInfraLine[ graph, p1, p2, All,
        Sequence @@ FilterRules[ { opts }, Options[ FindInfraLine ] ] ][ "Realizations" ],
      "Select" /. { opts } /. "Select" -> None,
      False, <| "Endpoints" -> { p1, p2 } |> ],
    extractBranches[ { opts } ] ]
