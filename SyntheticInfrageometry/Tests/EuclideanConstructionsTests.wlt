BeginTestSection["EuclideanConstructions"]

(* ===== FindInfraMidpoint ===== *)

(* Even distance -> single centre vertex. *)
VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], {1, 2, 3, 4, 5}],
  InfraEffectivePoint[<|3 -> 1|>],
  TestID -> "FindInfraMidpoint-segment-even-distance-single"
]

(* Odd distance -> the two closest indices, a effective point (always non-empty). *)
VerificationTest[
  Sort @ FindInfraMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}]["Vertices"],
  {2, 3},
  TestID -> "FindInfraMidpoint-segment-odd-distance-effective point"
]

(* Tolerance widens the band beyond the closest offset (0.5 + 1 = 1.5). *)
VerificationTest[
  Sort @ FindInfraMidpoint[PathGraph[Range[4]], {1, 2, 3, 4}, "Tolerance" -> 1]["Vertices"],
  {1, 2, 3, 4},
  TestID -> "FindInfraMidpoint-segment-tolerance-widens-band"
]

VerificationTest[
  FindInfraMidpoint[PathGraph[Range[5]], 1, 5],
  InfraEffectivePoint[<|3 -> 1|>],
  TestID -> "FindInfraMidpoint-endpoints-single"
]

(* Union over all geodesics matches the per-geodesic centre vertices. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}], d = GraphDistance[GridGraph[{3, 3}], 1, 9]},
    Sort @ FindInfraMidpoint[g, 1, 9]["Vertices"] ===
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
  InfraEffectivePoint[<|3 -> 1|>],
  TestID -> "FindInfraMidpoint-InfraSegment-single-walk"
]

(* Walks with different centres union into one effective point. *)
VerificationTest[
  Sort @ FindInfraMidpoint[ PathGraph[ Range[ 7 ] ],
    InfraSegment[ { { 1, 2, 3, 4, 5, 6, 7 }, { 1, 2, 3, 4, 5 } } ] ][ "Vertices" ],
  { 3, 4 },
  TestID -> "FindInfraMidpoint-InfraSegment-multi-walk-union"
]

(* both reversed walks pass through 3, so the midpoint projection gives it
   mass 2 -- a measure whose occupation normalises to 1 *)
VerificationTest[
  FindInfraMidpoint[ PathGraph[ Range[ 5 ] ],
    InfraSegment[ { { 1, 2, 3, 4, 5 }, { 5, 4, 3, 2, 1 } } ] ],
  InfraEffectivePoint[ <| 3 -> 2 |> ],
  TestID -> "FindInfraMidpoint-InfraSegment-mass-of-shared-middle"
]

(* ===== FindInfraPerpendicular ===== *)

(* C5, line {1,2,3,4}, point 5: foot is 2, perpendicular line is the maximal
   geodesic {5, 1, 2} (canonical orientation: lex-min of seq and reverse). *)

VerificationTest[
  Sort /@ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All][ "Realizations" ],
  { { 1, 2, 5 } },
  TestID -> "FindInfraPerpendicular-CycleGraph5-Metric"
]

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All],
  InfraLine,
  TestID -> "FindInfraPerpendicular-returns-InfraLine"
]

VerificationTest[
  (* every returned perp line must pass through both `point` and at least one
     vertex of `line` (a foot). *)
  With[{lines = FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All]},
    AllTrue[lines[ "Realizations" ], MemberQ[#, 5] && IntersectingQ[#, {1, 2, 3, 4}] &]
  ],
  True,
  TestID -> "FindInfraPerpendicular-through-point-and-foot"
]

VerificationTest[
  Length @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, 1][ "Realizations" ],
  1,
  TestID -> "FindInfraPerpendicular-strict-1"
]

(* Q-side dispatch on C5: result is one InfraLine wrapper (smoke test).  C5 is
   a degenerate configuration -- the metric perpendicular shares two vertices
   with `line`, so Q-side tests legitimately reject it.  Substantive Q-side
   tests live in the mesh-graph notebook demo. *)

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All,
    Method -> "Projection"],
  InfraLine,
  TestID -> "FindInfraPerpendicular-CycleGraph5-Projection-shape"
]

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All,
    Method -> {"Alexandrov", "Curvature" -> 0, "Tolerance" -> 0.5}],
  InfraLine,
  TestID -> "FindInfraPerpendicular-CycleGraph5-Alexandrov0-shape"
]

(* Radius option: same setup, restrict to NeighborhoodGraph[g, 5, 1].  The
   1-ball around 5 in C5 is {5, 1, 4}; line {1, 2, 3, 4} restricted is {1, 4}.
   Foot recipe finds... no equidistant pair with the right parity, so should
   return empty.  Just check it does not crash and returns an InfraLine. *)

VerificationTest[
  Head @ FindInfraPerpendicular[CycleGraph[5], {1, 2, 3, 4}, 5, All, "Radius" -> 1],
  InfraLine,
  TestID -> "FindInfraPerpendicular-Radius-1"
]

(* ===== FindClosestInfraPoint ===== *)

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[13], All],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-grid-InfraSegment"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraLine[{{1, 2, 3, 4, 5}}], InfraPoint[13], All],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-grid-InfraLine"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    {1, 2, 3, 4, 5}, 13, All],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-bare-list-and-vertex"
]

(* Point already on the segment: closest is itself. *)
VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[3], All],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-point-on-line"
]

(* Cartesian spread: two segment realisations, one point -> one closest per realisation. *)
VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}, {1, 6, 11, 16, 21}}],
    InfraPoint[13], All],
  { InfraPoint[3], InfraPoint[11] },
  TestID -> "FindClosestInfraPoint-multi-segment-Cartesian"
]

(* Cartesian spread: one segment, two point realisations. *)
VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraSet[{11, 15}], All],
  { InfraPoint[1], InfraPoint[5] },
  TestID -> "FindClosestInfraPoint-multi-point-Cartesian"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[13]],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-default-count-1"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[13], 5],
  $Failed,
  TestID -> "FindClosestInfraPoint-strict-count-too-large-fails"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraSegment[{{1, 2, 3, 4, 5}}], InfraPoint[13], UpTo[5]],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-UpTo-caps"
]

(* Tied minimisers (CycleGraph[5], point 1 to opposite arc {3, 4}: both at distance 2). *)
VerificationTest[
  FindClosestInfraPoint[CycleGraph[5],
    InfraSegment[{{3, 4}}], InfraPoint[1], All],
  { InfraPoint[3], InfraPoint[4] },
  TestID -> "FindClosestInfraPoint-ties-symmetric"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraWalk[{{1, 2, 3, 4, 5}}], InfraPoint[13], All],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-InfraWalk"
]

VerificationTest[
  FindClosestInfraPoint[GridGraph[{5, 5}],
    InfraRay[{{1, 2, 3, 4, 5}}], InfraPoint[13], All],
  { InfraPoint[3] },
  TestID -> "FindClosestInfraPoint-InfraRay"
]

(* ===== FindInfraBisectingHyperplane ===== *)

(* Properties -> {} (default): the bisector slab itself as a single
   realisation. PathGraph[5], 1 to 5: slab = {3}. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5],
  InfraPlane[{{3}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-path-center"
]

VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, All],
  FindInfraBisectingHyperplane[PathGraph[Range[5]], InfraPoint[1], InfraPoint[5], All],
  TestID -> "FindInfraBisectingHyperplane-list-form-equiv"
]

(* GridGraph[3,3]: slab is the antidiagonal {3, 5, 7}. *)
VerificationTest[
  FindInfraBisectingHyperplane[GridGraph[{3, 3}], 1, 9, All],
  InfraPlane[{{3, 5, 7}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-grid-antidiagonal"
]

(* PathGraph[6], 1 to 6 (odd distance): strict slab is empty. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, All],
  InfraPlane[{{}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-odd-distance-empty"
]

(* Widening to {-1, 1} thickens the slab to {3, 4}. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, All],
  InfraPlane[{{3, 4}}],
  TestID -> "FindInfraBisectingHyperplane-LevelSet-thickened-path"
]

(* Properties -> {"Separating"}: on PathGraph[6] each of {3}, {4} is a
   minimal separator within the thickened slab. *)
VerificationTest[
  Sort @ FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, All, Properties -> {"Separating"}][ "Realizations" ],
  {{3}, {4}},
  TestID -> "FindInfraBisectingHyperplane-Separating-thickened-path"
]

(* CycleGraph[6], 1 to 4 (odd distance), thickened to {-1, 1}: cutting either
   arc requires one vertex from {2, 3} and one from {5, 6}; four minimal
   separators. *)
VerificationTest[
  Sort @ ( Sort /@ FindInfraBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, All, Properties -> {"Separating"}][ "Realizations" ] ),
  {{2, 5}, {2, 6}, {3, 5}, {3, 6}},
  TestID -> "FindInfraBisectingHyperplane-Separating-cycle-thickened"
]

VerificationTest[
  Length @ FindInfraBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, UpTo[2], Properties -> {"Separating"}][ "Realizations" ],
  2,
  TestID -> "FindInfraBisectingHyperplane-Separating-upto-soft"
]

VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, 5],
  $Failed,
  TestID -> "FindInfraBisectingHyperplane-LevelSet-fails-when-too-few"
]

VerificationTest[
  MatchQ[ FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5], InfraPlane[ { _ } ] ],
  True,
  TestID -> "FindInfraBisectingHyperplane-wraps-as-InfraPlane"
]

(* Method -> "Greedy", no count: the DFS peel returns one certified minimal. *)
VerificationTest[
  With[{realizations = FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, Properties -> {"Separating"}, Method -> "Greedy"][ "Realizations" ]},
    Length[realizations] == 1 && MemberQ[{{3}, {4}}, First @ realizations]],
  True,
  TestID -> "FindInfraBisectingHyperplane-Greedy-returns-one-minimal"
]

(* The peel backtracks, so a finite count is exact: both minimals of the
   {3} / {4} bisector come back under count 2. *)
VerificationTest[
  Sort @ FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, {-1, 1}, 2, Properties -> {"Separating"}, Method -> "Greedy"]["Realizations"],
  {{3}, {4}},
  TestID -> "FindInfraBisectingHyperplane-Greedy-count-is-exact"
]

(* Greedy on a slab that itself does not separate: empty wrapper. *)
VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[6]], 1, 6, Properties -> {"Separating"}, Method -> "Greedy"],
  InfraPlane[{}],
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
  InfraPlane[{}],
  TestID -> "FindInfraBisectingHyperplane-Connected-rejects-disconnected"
]

(* 4-cycle + chord: only minimal separator is the connected {1, 3}. *)
VerificationTest[
  With[{g = Graph[{1, 2, 3, 4}, {1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 1 <-> 3}]},
    Sort @ ( Sort /@ FindInfraBisectingHyperplane[g, 2, 4, All, Properties -> {"Separating", "Connected"}][ "Realizations" ] )],
  {{1, 3}},
  TestID -> "FindInfraBisectingHyperplane-Connected-accepts-chord"
]

VerificationTest[
  With[{g = Graph[{1, 2, 3, 4}, {1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 1, 1 <-> 3}]},
    Sort @ First @ FindInfraBisectingHyperplane[g, 2, 4, Properties -> {"Separating", "Connected"}, Method -> "Greedy"][ "Realizations" ]],
  {1, 3},
  TestID -> "FindInfraBisectingHyperplane-Greedy-Connected"
]

(* Properties -> {"Connected"} alone: corner case -- inclusion-minimal connected
   subsets are single vertices. The greedy peel can drop everything from the slab
   one vertex at a time until one remains. *)
VerificationTest[
  With[{g = PathGraph[Range[5]],
        realizations = FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, {-1, 1}, Properties -> {"Connected"}, Method -> "Greedy"][ "Realizations" ]},
    Length[realizations] == 1 && Length[First @ realizations] == 1],
  True,
  TestID -> "FindInfraBisectingHyperplane-Connected-alone-singleton"
]

(* Method -> {"Exhaustive", "Pruning" -> 1} nests the pruning sub-option;
   result fits in [1, 4] (4 = unpruned count). *)
VerificationTest[
  With[{n = BlockRandom[SeedRandom[7];
    Length @ FindInfraBisectingHyperplane[CycleGraph[6], 1, 4, {-1, 1}, All,
      Properties -> {"Separating"}, Method -> {"Exhaustive", "Pruning" -> 1}][ "Realizations" ]]},
    1 <= n <= 4],
  True,
  TestID -> "FindInfraBisectingHyperplane-Pruning-bounded"
]

(* Sanity: every Separating realisation actually separates p1 from p2. *)
VerificationTest[
  With[{g = CycleGraph[6]},
    AllTrue[
      FindInfraBisectingHyperplane[g, 1, 4, {-1, 1}, All, Properties -> {"Separating"}][ "Realizations" ],
      sep |-> SeparatesQ[g, sep, 1, 4]]],
  True,
  TestID -> "FindInfraBisectingHyperplane-Separating-results-actually-separate"
]

VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, Properties -> {"Separating"}, Method -> "Bogus"],
  $Failed,
  {FindInfraBisectingHyperplane::badmethod},
  TestID -> "FindInfraBisectingHyperplane-badmethod"
]

VerificationTest[
  FindInfraBisectingHyperplane[PathGraph[Range[5]], 1, 5, Properties -> {"Bogus"}],
  $Failed,
  {FindInfraBisectingHyperplane::badproperty},
  TestID -> "FindInfraBisectingHyperplane-badproperty"
]

(* ===== CompleteInfraEquilateralTriangle ===== *)

VerificationTest[
  Sort[ #[ "Vertex" ] & /@ CompleteInfraEquilateralTriangle[CycleGraph[6], 1, 3, All] ],
  {5},
  TestID -> "CompleteInfraEquilateralTriangle-cycle6"
]

VerificationTest[
  CompleteInfraEquilateralTriangle[PathGraph[Range[5]], 1, 5, All],
  { },
  TestID -> "CompleteInfraEquilateralTriangle-path-no-apex"
]

VerificationTest[
  CompleteInfraEquilateralTriangle[CompleteGraph[4], 1, 2, 1],
  { InfraPoint[3] },
  TestID -> "CompleteInfraEquilateralTriangle-K4-strict-1"
]

(* ===== FindInfraParallel: Method scaffolding ===== *)

VerificationTest[
  FindInfraParallel[GridGraph[{4, 4}], {1, 2, 3, 4}, 5, All, Method -> "Exhaustive"],
  InfraLine[{{5, 6, 7, 8}}],
  TestID -> "FindInfraParallel-explicit-exhaustive"
]

(* ===== FindInfraMidpoint Method -> "Embedding" ===== *)

(* Embedding returns the single nearest-coordinate vertex, which lies in the metric union. *)
VerificationTest[
  MemberQ[ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> "Metric" ][ "Vertices" ],
           FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> "Embedding" ][ "First" ] ],
  True,
  TestID -> "FindInfraMidpoint-Embedding-in-metric-union"
]

VerificationTest[
  Length @ FindInfraMidpoint[ GridGraph[ { 5, 5 } ], 1, 25, Method -> "Embedding" ][ "Vertices" ],
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
  InfraEffectivePoint[<|7 -> 1|>],
  TestID -> "FindInfraGoldenSection-single-point-vertex-7"
]

VerificationTest[
  FindInfraGoldenSection[PathGraph[Range[11]], 1, 11, "Tolerance" -> 0.5],
  InfraEffectivePoint[<|7 -> 1|>],
  TestID -> "FindInfraGoldenSection-tolerance"
]

VerificationTest[
  FindInfraGoldenSection[PathGraph[Range[11]], InfraSegment[{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}}]],
  InfraEffectivePoint[<|7 -> 1|>],
  TestID -> "FindInfraGoldenSection-InfraSegment"
]

VerificationTest[
  Length @ FindInfraGoldenSection[PathGraph[Range[11]], 1, 11, Method -> "Embedding"]["Vertices"],
  1,
  TestID -> "FindInfraGoldenSection-Embedding-single-vertex"
]

EndTestSection[]
