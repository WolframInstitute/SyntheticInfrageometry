BeginTestSection["EuclideanConstructions"]

(* ===== FindInfraMidpoint ===== *)

VerificationTest[
  InfraPoint @ FindInfraMidpoint[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  InfraPoint[{3}],
  TestID -> "FindInfraMidpoint-segment-odd-length"
]

VerificationTest[
  InfraPoint @ FindInfraMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}],
  InfraPoint[{2}],
  TestID -> "FindInfraMidpoint-segment-even-length-lower-central"
]

VerificationTest[
  InfraPoint @ FindInfraMidpoint[PathGraph[Range[5]], 1, 5],
  InfraPoint[{3}],
  TestID -> "FindInfraMidpoint-endpoints-strict-1"
]

VerificationTest[
  With[{g = GridGraph[{3, 3}], d = GraphDistance[GridGraph[{3, 3}], 1, 9]},
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraMidpoint[g, 1, 9, All]) ===
      Sort @ DeleteDuplicates[
        #[[ Ceiling[ Length[#] / 2 ] ]] & /@ FindPath[g, 1, 9, {d}, All]
      ]
  ],
  True,
  TestID -> "FindInfraMidpoint-all-matches-geodesic-midpoints"
]

VerificationTest[
  Length @ FindInfraMidpoint[GridGraph[{3, 3}], 1, 9, UpTo[2]] <= 2,
  True,
  TestID -> "FindInfraMidpoint-upto-soft"
]

VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], 1, 5, 100],
  $Failed,
  TestID -> "FindInfraMidpoint-strict-fails-when-too-few"
]

(* ===== FindInfraPerpendicular ===== *)

VerificationTest[
  InfraPoint @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All],
  InfraPoint[{2}],
  TestID -> "FindInfraPerpendicular-CycleGraph5"
]

VerificationTest[
  With[{feet = (#[[ 1, 1 ]] & /@ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All])},
    AllTrue[feet, MemberQ[{1, 2, 3, 4}, #] &]
  ],
  True,
  TestID -> "FindInfraPerpendicular-feet-on-line"
]

VerificationTest[
  InfraPoint @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, 1],
  InfraPoint[{2}],
  TestID -> "FindInfraPerpendicular-strict-1"
]

(* ===== FindInfraBisectingHyperplane ===== *)

(* Properties -> {} (default): the bisector slab itself as a single
   realisation. PathGraph[5], 1 to 5: slab = {3}. *)
VerificationTest[
  InfraPlane @ FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5],
  InfraPlane[{{3}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-path-center"
]

VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, All],
  FindInfraBisectingHyperplane[PathGraph[Range[5]], InfraPoint[{1}], InfraPoint[{5}], All],
  TestID -> "FindInfraBisectingHyperplane-list-form-equiv"
]

(* GridGraph[3,3]: slab is the antidiagonal {3, 5, 7}. *)
VerificationTest[
  InfraPlane @ FindInfraBisectingHyperplane[GridGraph[{3, 3}], 1, 9, All],
  InfraPlane[{{3, 5, 7}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-grid-antidiagonal"
]

(* PathGraph[6], 1 to 6 (odd distance): strict slab is empty. *)
VerificationTest[
  InfraPlane @ FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, All],
  InfraPlane[{{}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-odd-distance-empty"
]

(* Widening to {-1, 1} thickens the slab to {3, 4}. *)
VerificationTest[
  InfraPlane @ FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, All],
  InfraPlane[{{3, 4}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-thickened-path"
]

(* Properties -> {"Separating"}: on PathGraph[6] each of {3}, {4} is a
   minimal separator within the thickened slab. *)
VerificationTest[
  Sort @ (#[[ 1, 1 ]] & /@ FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, All, Properties -> {"Separating"}]),
  {{3}, {4}},
  TestID -> "FindInfraBisectingHyperplane-Separating-thickened-path"
]

(* CycleGraph[6], 1 to 4 (odd distance), thickened to {-1, 1}: cutting either
   arc requires one vertex from {2, 3} and one from {5, 6}; four minimal
   separators. *)
VerificationTest[
  Sort @ ( Sort /@ (#[[ 1, 1 ]] & /@ FindInfraBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, All, Properties -> {"Separating"}]) ),
  {{2, 5}, {2, 6}, {3, 5}, {3, 6}},
  TestID -> "FindInfraBisectingHyperplane-Separating-cycle-thickened"
]

VerificationTest[
  Length @ FindInfraBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, UpTo[2], Properties -> {"Separating"}],
  2,
  TestID -> "FindInfraBisectingHyperplane-Separating-upto-soft"
]

(* Default level-set mode yields exactly one realisation; asking for more fails. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, 5],
  $Failed,
  TestID -> "FindInfraBisectingHyperplane-LevelSet-fails-when-too-few"
]

VerificationTest[
  MatchQ[ FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5], { InfraPlane[ { _ } ] .. } ],
  True,
  TestID -> "FindInfraBisectingHyperplane-wraps-as-InfraPlane"
]

(* Method -> "Greedy": DFS peel returns one realisation. *)
VerificationTest[
  With[{result = FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, Properties -> {"Separating"}, Method -> "Greedy"]},
    Length[result] == 1 && MemberQ[{{3}, {4}}, First @ result[[1, 1]]]],
  True,
  TestID -> "FindInfraBisectingHyperplane-Greedy-returns-one-minimal"
]

VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, 2, Properties -> {"Separating"}, Method -> "Greedy"],
  $Failed,
  TestID -> "FindInfraBisectingHyperplane-Greedy-count-gt-1-fails"
]

(* Greedy on a slab that itself does not separate: empty, $Failed under count = 1. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, Properties -> {"Separating"}, Method -> "Greedy"],
  $Failed,
  TestID -> "FindInfraBisectingHyperplane-Greedy-empty-when-slab-does-not-separate"
]

(* GridGraph[{3, 3}]'s antidiagonal {3, 5, 7} is disconnected. *)
VerificationTest[
  ConnectedGraphQ @ Subgraph[GridGraph[{3, 3}], {3, 5, 7}],
  False,
  TestID -> "FindInfraBisectingHyperplane-grid-antidiagonal-disconnected-sanity"
]

(* Properties -> {"Separating", "Connected"} rejects the disconnected antidiagonal. *)
VerificationTest[
  FindInfraBisectingHyperplane[GridGraph[{3, 3}], 1, 9, All, Properties -> {"Separating", "Connected"}],
  {},
  TestID -> "FindInfraBisectingHyperplane-Connected-rejects-disconnected"
]

(* 4-cycle + chord: only minimal separator is the connected {1, 3}. *)
VerificationTest[
  With[{g = Graph[{1, 2, 3, 4}, {1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 1 <-> 3}]},
    Sort @ ( #[[1, 1]] & /@ FindInfraBisectingHyperplane[g, 2, 4, All, Properties -> {"Separating", "Connected"}] )],
  {{1, 3}},
  TestID -> "FindInfraBisectingHyperplane-Connected-accepts-chord"
]

VerificationTest[
  With[{g = Graph[{1, 2, 3, 4}, {1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 1 <-> 3}]},
    Sort @ First @ FindInfraBisectingHyperplane[g, 2, 4, Properties -> {"Separating", "Connected"}, Method -> "Greedy"][[1, 1]]],
  {1, 3},
  TestID -> "FindInfraBisectingHyperplane-Greedy-Connected"
]

(* Properties -> {"Connected"} alone: corner case -- inclusion-minimal connected
   subsets are single vertices. The greedy peel can drop everything from the slab
   one vertex at a time until one remains. *)
VerificationTest[
  With[{g = PathGraph[Range[5]],
        result = FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, {-1, 1}, Properties -> {"Connected"}, Method -> "Greedy"]},
    Length[result] == 1 && Length[First @ result[[1, 1]]] == 1],
  True,
  TestID -> "FindInfraBisectingHyperplane-Connected-alone-singleton"
]

(* Method -> {"Exhaustive", "Pruning" -> 1} nests the pruning sub-option;
   result fits in [1, 4] (4 = unpruned count). *)
VerificationTest[
  With[{n = BlockRandom[SeedRandom[7];
    Length @ FindInfraBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, All,
      Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}]]},
    1 <= n <= 4],
  True,
  TestID -> "FindInfraBisectingHyperplane-Pruning-bounded"
]

(* Sanity: every Separating realisation actually separates p1 from p2. *)
VerificationTest[
  With[{g = CycleGraph[6]},
    AllTrue[
      #[[1, 1]] & /@ FindInfraBisectingHyperplane[g, 1, 4, {-1, 1}, All, Properties -> {"Separating"}],
      sep |-> SeparatesQ[g, sep, 1, 4]]],
  True,
  TestID -> "FindInfraBisectingHyperplane-Separating-results-actually-separate"
]

(* badmethod message guard. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, Properties -> {"Separating"}, Method -> "Bogus"],
  $Failed,
  {FindInfraBisectingHyperplane::badmethod},
  TestID -> "FindInfraBisectingHyperplane-badmethod"
]

(* badproperty message guard. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraBisectingHyperplane::badproperty},
  TestID -> "FindInfraBisectingHyperplane-badproperty"
]

(* ===== CompleteInfraEquilateralTriangle ===== *)

VerificationTest[
  Sort @ (#[[ 1, 1 ]] & /@ CompleteInfraEquilateralTriangle[CycleGraph[6], 1, 3, All]),
  {5},
  TestID -> "CompleteInfraEquilateralTriangle-cycle6"
]

VerificationTest[
  InfraPoint @ CompleteInfraEquilateralTriangle[PathGraph[Range[5]], 1, 5, All],
  InfraPoint[{}],
  TestID -> "CompleteInfraEquilateralTriangle-path-no-apex"
]

VerificationTest[
  InfraPoint @ CompleteInfraEquilateralTriangle[CompleteGraph[4], 1, 2, 1],
  InfraPoint[{3}],
  TestID -> "CompleteInfraEquilateralTriangle-K4-strict-1"
]

(* ===== InfraSegmentLineAngle ===== *)

VerificationTest[
  InfraSegmentLineAngle[PathGraph[Range[5]], 1, 3, {1, 2, 3, 4, 5}],
  0,
  TestID -> "InfraSegmentLineAngle-segment-on-line"
]

VerificationTest[
  InfraSegmentLineAngle[GridGraph[{3, 3}], 1, 9, {1, 2, 3}],
  2,
  TestID -> "InfraSegmentLineAngle-grid-far-endpoint"
]

VerificationTest[
  InfraSegmentLineAngle[GridGraph[{3, 3}], 5, 9, {1, 2, 3}],
  Infinity,
  TestID -> "InfraSegmentLineAngle-near-endpoint-not-on-line"
]

VerificationTest[
  InfraSegmentLineAngle[PathGraph[Range[5]], {1, 2, 3}, {1, 2, 3, 4, 5}],
  0,
  TestID -> "InfraSegmentLineAngle-segment-form"
]

(* ===== FindInfraParallel: Method scaffolding ===== *)

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "ShortestPath"],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-explicit-shortestpath"
]

(* ===== FindInfraMidpoint Method -> "Embedding" ===== *)

VerificationTest[
  MemberQ[ (#[[ 1, 1 ]] & /@ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, All, Method -> "Metric" ]),
           First @ First @ First @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, 1, Method -> "Embedding" ] ],
  True,
  TestID -> "FindInfraMidpoint-Embedding-Geodesic-in-metric-set"
]

VerificationTest[
  Length @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, All, Method -> { "Embedding", "Pool" -> "ShortestPaths" } ],
  Length @ Select[ VertexList[ GridGraph[ { 5, 5 } ] ],
    GraphDistance[ GridGraph[ { 5, 5 } ], 1, # ] + GraphDistance[ GridGraph[ { 5, 5 } ], #, 25 ] ==
      GraphDistance[ GridGraph[ { 5, 5 } ], 1, 25 ] & ],
  TestID -> "FindInfraMidpoint-Embedding-Geodesic-pool-equals-metric-interval"
]

VerificationTest[
  Length @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, All, Method -> { "Embedding", "Pool" -> "AllPaths" } ],
  25,
  TestID -> "FindInfraMidpoint-Embedding-AllPaths-pool-equals-all-vertices"
]


(* ===== FindInfraPerpendicular Method -> "Embedding" ===== *)

VerificationTest[
  Sort @ (#[[ 1, 1 ]] & /@ FindInfraPerpendicular[ GridGraph[ { 5, 5 } ], { 1, 2, 3, 4, 5 }, 13, All, Method -> "Embedding" ]),
  { 1, 2, 3, 4, 5 },
  TestID -> "FindInfraPerpendicular-Embedding-pool-equals-line"
]

VerificationTest[
  First @ First @ First @ FindInfraPerpendicular[ GridGraph[ { 5, 5 } ], { 1, 2, 3, 4, 5 }, 13, All, Method -> "Embedding" ],
  3,
  TestID -> "FindInfraPerpendicular-Embedding-closest-foot-is-projection"
]


(* FindInfraMidpoint "Tarski" recipe is local: depends only on B(p1, d(p1, p2)). *)

VerificationTest[
  With[ { g = GridGraph[ { 10, 10 } ], p1 = 23, p2 = 67 },
    Sort @ (#[[ 1, 1 ]] & /@ FindInfraMidpoint[ g, p1, p2, All, Method -> "Tarski" ]) ===
      Sort @ (#[[ 1, 1 ]] & /@
        FindInfraMidpoint[ NeighborhoodGraph[ g, p1, GraphDistance[ g, p1, p2 ] ], p1, p2, All, Method -> "Tarski" ])
  ],
  True,
  TestID -> "FindInfraMidpoint-Tarski-locality"
]

EndTestSection[]
