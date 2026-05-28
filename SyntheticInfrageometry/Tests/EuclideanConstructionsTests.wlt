BeginTestSection["EuclideanConstructions"]

(* ===== FindInfraMidpoint ===== *)

(* Even distance -> single centre vertex. *)
VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  InfraPoint[{3}],
  TestID -> "FindInfraMidpoint-segment-even-distance-single"
]

(* Odd distance -> the two closest indices, a mesopoint (always non-empty). *)
VerificationTest[
  Sort @ First @ FindInfraMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}],
  {2, 3},
  TestID -> "FindInfraMidpoint-segment-odd-distance-mesopoint"
]

(* Tolerance widens the band beyond the closest offset (0.5 + 1 = 1.5). *)
VerificationTest[
  Sort @ First @ FindInfraMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}, "Tolerance" -> 1],
  {1, 2, 3, 4},
  TestID -> "FindInfraMidpoint-segment-tolerance-widens-band"
]

VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], 1, 5],
  InfraPoint[{3}],
  TestID -> "FindInfraMidpoint-endpoints-single"
]

(* Union over all geodesics matches the per-geodesic centre vertices. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}], d = GraphDistance[GridGraph[{3, 3}], 1, 9]},
    Sort @ First @ FindInfraMidpoint[g, 1, 9] ===
      Sort @ DeleteDuplicates[
        #[[ Ceiling[ Length[#] / 2 ] ]] & /@ FindPath[g, 1, 9, {d}, All]
      ]
  ],
  True,
  TestID -> "FindInfraMidpoint-union-matches-geodesic-midpoints"
]

(* ===== FindInfraMidpoint on InfraSegment wrappers ===== *)

VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], InfraSegment[{{1, 2, 3, 4, 5}}]],
  InfraPoint[{3}],
  TestID -> "FindInfraMidpoint-InfraSegment-single-walk"
]

(* Walks with different centres union into one mesopoint. *)
VerificationTest[
  Sort @ First @ FindInfraMidpoint[ PathGraph[ Range[ 7 ] ],
    InfraSegment[ { { 1, 2, 3, 4, 5, 6, 7 }, { 1, 2, 3, 4, 5 } } ] ],
  { 3, 4 },
  TestID -> "FindInfraMidpoint-InfraSegment-multi-walk-union"
]

VerificationTest[
  FindInfraMidpoint[ PathGraph[ Range[ 5 ] ],
    InfraSegment[ { { 1, 2, 3, 4, 5 }, { 5, 4, 3, 2, 1 } } ] ],
  InfraPoint[{ 3 }],
  TestID -> "FindInfraMidpoint-InfraSegment-dedup"
]

(* ===== FindInfraPerpendicular ===== *)

(* C5, line {1,2,3,4}, point 5: foot is 2, perpendicular line is the maximal
   geodesic {5, 1, 2} (canonical orientation: lex-min of seq and reverse). *)

VerificationTest[
  Sort /@ ( #[[ 1, 1 ]] & /@ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All] ),
  { { 1, 2, 5 } },
  TestID -> "FindInfraPerpendicular-CycleGraph5-Metric"
]

VerificationTest[
  Head /@ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All],
  { InfraLine },
  TestID -> "FindInfraPerpendicular-returns-InfraLine"
]

VerificationTest[
  (* every returned perp line must pass through both `point` and at least one
     vertex of `line` (a foot). *)
  With[{lines = FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All]},
    AllTrue[lines, MemberQ[#[[1, 1]], 5] && IntersectingQ[#[[1, 1]], {1, 2, 3, 4}] &]
  ],
  True,
  TestID -> "FindInfraPerpendicular-through-point-and-foot"
]

VerificationTest[
  Length @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, 1],
  1,
  TestID -> "FindInfraPerpendicular-strict-1"
]

(* Q-side dispatch on C5: result is a List (smoke test).  C5 is a degenerate
   configuration -- the metric perpendicular shares two vertices with `line`,
   so Q-side tests legitimately reject it.  Substantive Q-side tests live in
   the mesh-graph notebook demo. *)

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All,
    Method -> "Projection"],
  List,
  TestID -> "FindInfraPerpendicular-CycleGraph5-Projection-shape"
]

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All,
    Method -> {"Alexandrov", "Curvature" -> 0, "Tolerance" -> 0.5}],
  List,
  TestID -> "FindInfraPerpendicular-CycleGraph5-Alexandrov0-shape"
]

(* Radius option: same setup, restrict to NeighborhoodGraph[g, 5, 1].  The
   1-ball around 5 in C5 is {5, 1, 4}; line {1, 2, 3, 4} restricted is {1, 4}.
   Foot recipe finds... no equidistant pair with the right parity, so should
   return empty.  Just check it does not crash and returns a list. *)

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All, "Radius" -> 1],
  List,
  TestID -> "FindInfraPerpendicular-Radius-1"
]

(* ===== FindClosestInfraPoint ===== *)

VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[{13}], All],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-grid-InfraSegment"
]

VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraLine[{{1, 2, 3, 4, 5}}], InfraPoint[{13}], All],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-grid-InfraLine"
]

VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    {1, 2, 3, 4, 5}, 13, All],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-bare-list-and-vertex"
]

(* Point already on the segment: closest is itself. *)
VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[{3}], All],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-point-on-line"
]

(* Cartesian spread: two segment realisations, one point -> one closest per realisation. *)
VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}, {1, 6, 11, 16, 21}}],
    InfraPoint[{13}], All],
  InfraPoint[{3, 11}],
  TestID -> "FindClosestInfraPoint-multi-segment-Cartesian"
]

(* Cartesian spread: one segment, two point realisations. *)
VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[{11, 15}], All],
  InfraPoint[{1, 5}],
  TestID -> "FindClosestInfraPoint-multi-point-Cartesian"
]

(* Default count = 1 returns a single unary InfraPoint wrapper. *)
VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[{13}]],
  {InfraPoint[{3}]},
  TestID -> "FindClosestInfraPoint-default-count-1"
]

(* Strict count larger than available -> $Failed. *)
VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[{13}], 5],
  $Failed,
  TestID -> "FindClosestInfraPoint-strict-count-too-large-fails"
]

(* UpTo caps gracefully. *)
VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[{13}], UpTo[5]],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-UpTo-caps"
]

(* Tied minimisers (CycleGraph[5], point 1 to opposite arc {3, 4}: both at distance 2). *)
VerificationTest[
  InfraPoint @ FindClosestInfraPoint[CycleGraph[5],
    InfraSegment[{{3, 4}}], InfraPoint[{1}], All],
  InfraPoint[{3, 4}],
  TestID -> "FindClosestInfraPoint-ties-symmetric"
]

(* InfraPath and InfraRay heads are also accepted. *)
VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraPath[{{1, 2, 3, 4, 5}}], InfraPoint[{13}], All],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-InfraPath"
]

VerificationTest[
  InfraPoint @ FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraRay[{{1, 2, 3, 4, 5}}], InfraPoint[{13}], All],
  InfraPoint[{3}],
  TestID -> "FindClosestInfraPoint-InfraRay"
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

(* ===== FindInfraParallel: Method scaffolding ===== *)

VerificationTest[
  InfraLine @ FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Exhaustive"],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-explicit-exhaustive"
]

(* ===== FindInfraMidpoint Method -> "Embedding" ===== *)

(* Embedding returns the single nearest-coordinate vertex, which lies in the metric union. *)
VerificationTest[
  MemberQ[ First @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> "Metric" ],
           First @ First @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> "Embedding" ] ],
  True,
  TestID -> "FindInfraMidpoint-Embedding-in-metric-union"
]

VerificationTest[
  Length @ First @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> "Embedding" ],
  1,
  TestID -> "FindInfraMidpoint-Embedding-single-vertex"
]

VerificationTest[
  Length @ First @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> { "Embedding", "Pool" -> "AllPaths" } ],
  1,
  TestID -> "FindInfraMidpoint-Embedding-AllPaths-single-vertex"
]


(* FindInfraPerpendicular "Embedding" Method has been removed (see plan
   okey-we-need-to-majestic-puppy: path-family Embedding dropped).  Users
   wanting embedding-ranked feet compose with EmbeddingClosest. *)


(* ===== FindInfraGoldenSection ===== *)

(* Closest index to the golden index 1 + 10/phi = 7.18 -> vertex 7, always a single point. *)
VerificationTest[
  FindInfraGoldenSection[PathGraph[Range[11]], 1, 11],
  InfraPoint[{7}],
  TestID -> "FindInfraGoldenSection-single-point-vertex-7"
]

VerificationTest[
  FindInfraGoldenSection[PathGraph[Range[11]], 1, 11, "Tolerance" -> 0.5],
  InfraPoint[{7}],
  TestID -> "FindInfraGoldenSection-tolerance"
]

VerificationTest[
  FindInfraGoldenSection[PathGraph[Range[11]], InfraSegment[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}}]],
  InfraPoint[{7}],
  TestID -> "FindInfraGoldenSection-InfraSegment"
]

VerificationTest[
  Length @ First @ FindInfraGoldenSection[PathGraph[Range[11]], 1, 11, Method -> "Embedding"],
  1,
  TestID -> "FindInfraGoldenSection-Embedding-single-vertex"
]

EndTestSection[]
