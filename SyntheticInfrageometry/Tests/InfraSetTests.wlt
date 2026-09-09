toDensity = WolframInstitute`SyntheticInfrageometry`PackageScope`toDensity;
pointQ    = WolframInstitute`SyntheticInfrageometry`PackageScope`pointQ;
multisetQ = WolframInstitute`SyntheticInfrageometry`PackageScope`multisetQ;
walkQ     = WolframInstitute`SyntheticInfrageometry`PackageScope`walkQ;
geodesicGraph        = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
infraSpread          = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;
infraVertexMultiset  = WolframInstitute`SyntheticInfrageometry`PackageScope`infraVertexMultiset;

BeginTestSection["InfraSet"]

(* The Alexandrov-topology operators (BallTopology / Topological* / ContinuousMapQ)
   now live in the Infrageometry paclet and are tested there.  A set carries no head:
   it IS a sorted vertex list, and <| v -> m |> is the density, so these tests cover
   both shapes, the anchor rule that produces the density, and the set operators. *)

(* ===== The shape ===== *)

(* Keys is a density's support and Length its size: the two accessors the set head used to own *)
VerificationTest[
  { Keys @ <| 1 -> 1, 3 -> 1, 5 -> 1 |>, Length @ <| 1 -> 1, 3 -> 1, 5 -> 1 |> },
  { {1, 3, 5}, 3 },
  TestID -> "set-support-and-size"
]

(* ===== the anchor rule builds the set ===== *)

(* a vertex is the unit mass, a List its Counts, an Association itself -- and the
   substrate is what tells a vertex labelled {i, j} from a two-element multiset *)
VerificationTest[
  With[ { g = PathGraph @ Range[ 7 ],
          t = Graph[ { {1, 1}, {2, 1} }, { {1, 1} <-> {2, 1} } ] },
    { toDensity[ g, 3 ], toDensity[ g, { 3, 1, 3 } ], toDensity[ g, <| 3 -> 2 |> ],
      toDensity[ t, {1, 1} ] } ],
  { <| 3 -> 1 |>, <| 1 -> 1, 3 -> 2 |>, <| 3 -> 2 |>, <| {1, 1} -> 1 |> },
  TestID -> "anchor-rule-reads-every-shape"
]

(* a set is already its support, and InfraUnion sorts it *)
VerificationTest[
  With[ { g = PathGraph @ Range[ 7 ] }, Sort @ InfraUnion[ g, FindInfraBall[ g, 4, 2 ] ] ],
  {2, 3, 4, 5, 6},
  TestID -> "set-from-InfraBall-support"
]

(* the set operators return the sorted List, so unions of unions stay one shape *)
VerificationTest[
  Sort @ InfraUnion[ PathGraph @ Range[ 7 ], <| 1 -> 1, 2 -> 1 |>, <| 2 -> 1, 3 -> 1 |> ],
  {1, 2, 3},
  TestID -> "InfraUnion-of-multisets"
]

(* ===== InfraBoundary / InfraInterior (combinatorial) ===== *)

(* On a path 1-2-3-4-5 the inner boundary of {2,3,4} is {2,4} (each touches an
   outside neighbor) and the interior is the single shielded vertex {3}. *)
VerificationTest[
  Sort @ InfraBoundary[ PathGraph @ Range[5], {2, 3, 4} ],
  {2, 4},
  TestID -> "InfraBoundary-path-inner"
]

VerificationTest[
  Sort @ InfraInterior[ PathGraph @ Range[5], {2, 3, 4} ],
  {3},
  TestID -> "InfraInterior-path-inner"
]

(* Interior and inner boundary partition S: disjoint, and together reconstruct S. *)
VerificationTest[
  With[ { g = GridGraph[ {3, 3} ], s = {2, 4, 5, 6, 8} },
    { Union[ InfraInterior[ g, s ], InfraBoundary[ g, s ] ],
      Intersection[ InfraInterior[ g, s ], InfraBoundary[ g, s ] ] } ],
  { {2, 4, 5, 6, 8}, {} },
  TestID -> "InfraBoundary-Interior-partition"
]

(* Output is the sorted List, and the density over the same support agrees with it. *)
VerificationTest[
  With[ { g = GridGraph[ {3, 3} ], ball = FindInfraBall[ GridGraph[ {3, 3} ], 5, 1 ] },
    { InfraBoundary[ g, ball ],
      InfraBoundary[ g, ball ] === InfraBoundary[ g, <| 2 -> 1, 4 -> 1, 5 -> 1, 6 -> 1, 8 -> 1 |> ] } ],
  { { 2, 4, 6, 8 }, True },
  TestID -> "InfraBoundary-returns-set-and-coerces"
]

(* Alexandrov method dispatches to the closed-r-ball topology and returns the set. *)
VerificationTest[
  ListQ @ InfraBoundary[ GridGraph[ {3, 3} ], {2, 4, 5, 6, 8},
    Method -> {"Alexandrov", "Radius" -> 1} ],
  True,
  TestID -> "InfraBoundary-Alexandrov-dispatch"
]

(* ===== InfraVolume ===== *)

(* Set-like: Count - Boundary == Interior (a partition of the vertex set). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ], ball = FindInfraBall[ GridGraph[ {5, 5} ], 13, 2 ] },
    InfraVolume[ g, ball, "Measure" -> "FullCount" ] - InfraVolume[ g, ball, "Measure" -> "Boundary" ]
      == InfraVolume[ g, ball, "Measure" -> "WithoutBoundary" ] ],
  True,
  TestID -> "InfraVolume-count-minus-boundary-equals-interior"
]

(* HalfBoundary weights the boundary by one half: on a radius-2 ball of the grid, 13 - 8/2 = 9 *)
VerificationTest[
  InfraVolume[ GridGraph[ {5, 5} ], FindInfraBall[ GridGraph[ {5, 5} ], 13, 2 ], "Measure" -> "HalfBoundary" ],
  9,
  TestID -> "InfraVolume-half-boundary"
]

(* A thin geodesic line (top row of a grid) is 1-D in a 2-D graph: empty interior. *)
VerificationTest[
  InfraVolume[ GridGraph[ {4, 4} ], geodesicGraph @ {1, 2, 3, 4}, "Measure" -> "WithoutBoundary" ],
  0,
  TestID -> "InfraVolume-thin-line-empty-interior"
]

(* Line vs set on the SAME (space-filling) vertex set: the curve has ~no interior
   (only the two pass-through corners), the induced 2-D region has full interior. *)
VerificationTest[
  With[
    { g = GridGraph[ {4, 4} ],
      snake = Catenate @ Table[ With[ { row = Range[ 4 (i - 1) + 1, 4 i ] }, If[ OddQ[ i ], row, Reverse[ row ] ] ], { i, 4 } ] },
    { InfraVolume[ g, geodesicGraph @ snake, "Measure" -> "WithoutBoundary" ],
      InfraVolume[ g, toDensity[ g, snake ], "Measure" -> "WithoutBoundary" ],
      InfraVolume[ g, geodesicGraph @ snake, "Measure" -> "FullCount" ]
        === InfraVolume[ g, toDensity[ g, snake ], "Measure" -> "FullCount" ] } ],
  { 2, 16, True },
  TestID -> "InfraVolume-line-vs-set-spanning-curve"
]

(* The line graph is the union of the walks, NOT the induced subgraph: two parallel
   grid rows stay disconnected, so neither row gains interior from the other. *)
VerificationTest[
  InfraVolume[ GridGraph[ {4, 4} ], geodesicGraph /@ {{1, 2, 3, 4}, {5, 6, 7, 8}}, "Measure" -> "WithoutBoundary" ],
  0,
  TestID -> "InfraVolume-line-union-not-induced"
]

(* ===== FindInfraEquidistantSet ===== *)

(* Every vertex of the equidistant set sees all anchors at one common distance. *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    AllTrue[ FindInfraEquidistantSet[ g, {1, 5, 21} ],
      v |-> SameQ @@ ( GraphDistance[ g, #, v ] & /@ {1, 5, 21} ) ] ],
  True,
  TestID -> "FindInfraEquidistantSet-all-equidistant"
]

(* E(p1, ..., pn) == intersection of the n-1 consecutive perpendicular bisectors. *)
VerificationTest[
  Module[ { g = GridGraph[ {4, 4, 4} ], ps = {1, 5, 21} },
    Sort @ FindInfraEquidistantSet[ g, ps ] ===
      Sort[ Intersection @@ MapThread[
        {a, b} |-> Select[ VertexList[ g ], v |-> GraphDistance[ g, a, v ] == GraphDistance[ g, b, v ] ],
        { Most[ ps ], Rest[ ps ] } ] ] ],
  True,
  TestID -> "FindInfraEquidistantSet-consecutive-bisector-identity"
]

(* For n == 2 the strict set is the window {0, 0} perpendicular bisector. *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    Sort @ FindInfraEquidistantSet[ g, {1, 25} ] ===
      Sort @ First @ FindInfraBisectingHyperplane[ g, 1, 25, {0, 0}, All ] ],
  True,
  TestID -> "FindInfraEquidistantSet-n2-equals-bisector"
]

(* Three corners of the square grid meet at the centre. *)
VerificationTest[
  FindInfraEquidistantSet[ GridGraph[ {5, 5} ], {1, 5, 21} ],
  {13},
  TestID -> "FindInfraEquidistantSet-three-corners-centre"
]

(* Output is the sorted vertex list. *)
VerificationTest[
  FindInfraEquidistantSet[ GridGraph[ {5, 5} ], {1, 5, 21} ],
  { 13 },
  TestID -> "FindInfraEquidistantSet-shape"
]

(* Widening the window can only grow the set (the strict set is a subset). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    SubsetQ[ FindInfraEquidistantSet[ g, {1, 25}, {-1, 1} ],
             FindInfraEquidistantSet[ g, {1, 25} ] ] ],
  True,
  TestID -> "FindInfraEquidistantSet-window-monotone"
]

(* ===== FindAdvancingInfraFront ===== *)

(* The bouncing front moves each vertex one step outward from the previous front
   and reflects it inward where it cannot advance. On a triangle from 1 it expands
   to {2,3} then turns straight back to {1}, oscillating with no dwell -- never
   empties, unlike the metric sphere. *)
VerificationTest[
  Sort /@ FindAdvancingInfraFront[ CompleteGraph[ 3 ], 1, 5 ],
  {{1}, {2, 3}, {1}, {2, 3}, {1}, {2, 3}},
  TestID -> "FindAdvancingInfraFront-triangle-bounces"
]

(* The defining property: on a finite connected graph the front never empties. *)
VerificationTest[
  Min[ Length /@ FindAdvancingInfraFront[ GridGraph[ {4, 30} ], 2, 120 ] ] >= 1,
  True,
  TestID -> "FindAdvancingInfraFront-never-empties"
]

(* steps + 1 fronts, each a sorted vertex list, the seed front S_0 = {origin}. *)
VerificationTest[
  With[ { f = FindAdvancingInfraFront[ CycleGraph[ 9 ], 1, 4 ] },
    Length[ f ] === 5 && AllTrue[ f, ListQ ] && First[ f ] === { 1 } ],
  True,
  TestID -> "FindAdvancingInfraFront-shape-and-seed"
]

(* Locality: S_{i+1} subset of S_i union N(S_i) (each vertex moves to a neighbour). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    AllTrue[
      Partition[ FindAdvancingInfraFront[ g, 13, 6 ], 2, 1 ],
      SubsetQ[ Union[ #[[ 1 ]], Union @@ ( AdjacencyList[ g, # ] & /@ #[[ 1 ]] ) ], #[[ 2 ]] ] & ] ],
  True,
  TestID -> "FindAdvancingInfraFront-local-step"
]

(* It bounces: from the centre of a path the wave runs to the ends, reflects, and
   refocuses back at the origin, so {5} recurs as a later front. *)
VerificationTest[
  MemberQ[ Rest @ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 12 ], {5} ],
  True,
  TestID -> "FindAdvancingInfraFront-refocuses-at-origin"
]

(* Immediate bounce: no two consecutive fronts are equal (longest run is 1). *)
VerificationTest[
  Max[ Length /@ Split[ Sort /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 30 ] ] ],
  1,
  TestID -> "FindAdvancingInfraFront-immediate-bounce"
]

(* a multiset origin seeds a multi-source front: S_0 is the source set. *)
VerificationTest[
  First @ FindAdvancingInfraFront[ GridGraph[ {6, 6} ], <| 1 -> 1, 36 -> 1 |>, 5 ],
  {1, 36},
  TestID -> "FindAdvancingInfraFront-multi-source-seed"
]


(* ===== the two projections of a bundle: support and occupation ===== *)

(* a set projects two ways: it IS its support, and its density is the all-ones
   measure on that same support, which agrees with the occupation reader *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ { ball = FindInfraBall[ g, 13, 2 ] },
      { d = toDensity[ g, ball ] },
      { Keys @ d === Sort @ ball,
        Values @ d === ConstantArray[ 1, Length @ d ],
        d === KeySort @ infraVertexMultiset @ ball } ]
  ],
  { True, True, True },
  TestID -> "set-density-is-all-ones-on-the-support"
]

(* a density and the List over the same support are both legal anchors, and both
   spread: several sources give the List of interval DAGs, one per source *)
VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] },
    With[ { shell = FindInfraShell[ g, 13, { 2, 2 } ] },
      { ListQ @ shell,
        MatchQ[ FindInfraSegment[ g, toDensity[ g, shell ], 13, All ], { __Graph } ],
        MatchQ[ FindInfraSegment[ g, shell, 13, All ], { __Graph } ] } ]
  ],
  { True, True, True },
  TestID -> "multiset-anchor-spreads"
]

(* the shape is the kind: a vertex, a multiset and a walk graph are three reads *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { pointQ[ g, 7 ], multisetQ[ g, <| 3 -> 1, 4 -> 1 |> ],
      walkQ[ g, geodesicGraph @ { 1, 2, 3 } ] } ],
  { True, True, True },
  TestID -> "shapes-stay-distinct"
]

(* ===== the DAG form marginalises to the metric interval ===== *)

(* FindInfraSegment returns the compact DAG form by default, and its support is the
   metric interval -- read without enumerating the family. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    Sort @ InfraUnion[ g, FindInfraSegment[g, 1, 16, All] ] === Sort @ MetricInterval[g, 1, 16]],
  True,
  TestID -> "DAG-support-is-MetricInterval"
]

(* The DAG and the family it stands for have the same support. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{dag = FindInfraSegment[g, 1, 16, All]},
      InfraUnion[ g, dag ] === InfraUnion[ g, ## ] & @@ ( geodesicGraph /@ infraSpread @ dag )]],
  True,
  TestID -> "DAG-support-equals-its-family's"
]

(* ===== density canonical form ===== *)

(* Multiplicities survive the DAG -> density conversion: the compact DAG and the
   enumerated family of the same segment give the SAME density.  They used to
   differ by association key order alone, which broke SameQ equality. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    With[{dag = FindInfraSegment[g, 1, 16, All]},
      toDensity[g, dag] === toDensity[g, geodesicGraph /@ infraSpread @ dag]]],
  True,
  TestID -> "density-DAG-and-enumerated-supports-are-SameQ"
]

(* The weights are the true geodesic occupation: counted by brute force over the
   whole enumerated family, they agree with the DP on the DAG. *)
VerificationTest[
  With[{g = GridGraph[{4, 4}]},
    {m = toDensity[g, FindInfraSegment[g, 1, 16, All]]},
    {paths = infraSpread @ FindInfraSegment[g, 1, 16, All]},
    AllTrue[Keys[m], m[#] == Count[paths, p_ /; MemberQ[p, #]] &]],
  True,
  TestID -> "density-DAG-weights-are-true-occupation"
]

(* Densities come out key-sorted, so equal ones built by different routes are SameQ
   even when their supports were discovered in different orders. *)
VerificationTest[
  With[{g = GridGraph[{3, 3}]},
    toDensity[g, <| 9 -> 1, 1 -> 1 |>] === toDensity[g, {9, 1}] === <| 1 -> 1, 9 -> 1 |>],
  True,
  TestID -> "density-key-order-canonicalised"
]

EndTestSection[]
