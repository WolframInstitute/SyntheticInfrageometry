BeginTestSection["InfraSet"]

(* The Alexandrov-topology operators (BallTopology / Topological* / ContinuousMapQ)
   now live in the Infrageometry paclet and are tested there. InfraSet stays here as
   a scene primitive; these tests cover the wrapper and its Infra* coercion. *)

(* ===== Accessors ===== *)

VerificationTest[
  InfraSet[ {1, 3, 5} ][ "Vertices" ],
  {1, 3, 5},
  TestID -> "InfraSet-Vertices-accessor"
]

VerificationTest[
  InfraSet[ {1, 3, 5} ][ "Length" ],
  3,
  TestID -> "InfraSet-Length-accessor"
]

VerificationTest[
  Head @ InfraSet[ {1, 2} ],
  InfraSet,
  TestID -> "InfraSet-Head"
]

(* ===== Infra* coercion ===== *)

(* InfraPoint: realisations are vertices directly *)
VerificationTest[
  InfraSet[ InfraPoint[ {3, 1, 3, 5} ] ][ "Vertices" ],
  {1, 3, 5},
  TestID -> "InfraSet-coerces-InfraPoint"
]

(* InfraBall: realisations are vertex-lists, flattened and unioned *)
VerificationTest[
  Sort @ InfraSet[ FindInfraBall[ PathGraph @ Range[7], 4, 2 ] ][ "Vertices" ],
  {2, 3, 4, 5, 6},
  TestID -> "InfraSet-coerces-InfraBall"
]

(* Nested InfraSets flatten to the union *)
VerificationTest[
  Sort @ InfraSet[ {InfraSet[ {1, 2} ], InfraSet[ {2, 3} ]} ][ "Vertices" ],
  {1, 2, 3},
  TestID -> "InfraSet-flattens-nested"
]

(* ===== InfraBoundary / InfraInterior (combinatorial) ===== *)

(* On a path 1-2-3-4-5 the inner boundary of {2,3,4} is {2,4} (each touches an
   outside neighbor) and the interior is the single shielded vertex {3}. *)
VerificationTest[
  Sort @ InfraBoundary[ PathGraph @ Range[5], {2, 3, 4} ][ "Vertices" ],
  {2, 4},
  TestID -> "InfraBoundary-path-inner"
]

VerificationTest[
  Sort @ InfraInterior[ PathGraph @ Range[5], {2, 3, 4} ][ "Vertices" ],
  {3},
  TestID -> "InfraInterior-path-inner"
]

(* Interior and inner boundary partition S: disjoint, and together reconstruct S. *)
VerificationTest[
  With[ { g = GridGraph[ {3, 3} ], s = {2, 4, 5, 6, 8} },
    { Union[ InfraInterior[ g, s ][ "Vertices" ], InfraBoundary[ g, s ][ "Vertices" ] ],
      Intersection[ InfraInterior[ g, s ][ "Vertices" ], InfraBoundary[ g, s ][ "Vertices" ] ] } ],
  { {2, 4, 5, 6, 8}, {} },
  TestID -> "InfraBoundary-Interior-partition"
]

(* Output is an InfraSet, and an Infra* wrapper input agrees with the equivalent set. *)
VerificationTest[
  With[ { g = GridGraph[ {3, 3} ], ball = FindInfraBall[ GridGraph[ {3, 3} ], 5, 1 ] },
    { Head @ InfraBoundary[ g, ball ],
      Sort @ InfraBoundary[ g, ball ][ "Vertices" ] ===
        Sort @ InfraBoundary[ g, InfraSet[ {2, 4, 5, 6, 8} ] ][ "Vertices" ] } ],
  { InfraSet, True },
  TestID -> "InfraBoundary-returns-InfraSet-and-coerces"
]

(* Alexandrov method dispatches to the closed-r-ball topology and returns an InfraSet. *)
VerificationTest[
  Head @ InfraBoundary[ GridGraph[ {3, 3} ], {2, 4, 5, 6, 8},
    Method -> {"Alexandrov", "Radius" -> 1} ],
  InfraSet,
  TestID -> "InfraBoundary-Alexandrov-dispatch"
]

(* ===== InfraVolume ===== *)

(* Set-like: Count - Boundary == Interior (a partition of the vertex set). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ], ball = FindInfraBall[ GridGraph[ {5, 5} ], 13, 2 ] },
    InfraVolume[ g, ball, "Volume" -> "Count" ] - InfraVolume[ g, ball, "Volume" -> "Boundary" ]
      == InfraVolume[ g, ball, "Volume" -> "Interior" ] ],
  True,
  TestID -> "InfraVolume-count-minus-boundary-equals-interior"
]

(* A thin geodesic line (top row of a grid) is 1-D in a 2-D graph: empty interior. *)
VerificationTest[
  InfraVolume[ GridGraph[ {4, 4} ], InfraLine[ {{1, 2, 3, 4}} ], "Volume" -> "Interior" ],
  0,
  TestID -> "InfraVolume-thin-line-empty-interior"
]

(* Line vs set on the SAME (space-filling) vertex set: the curve has ~no interior
   (only the two pass-through corners), the induced 2-D region has full interior. *)
VerificationTest[
  With[
    { g = GridGraph[ {4, 4} ],
      snake = Catenate @ Table[ With[ { row = Range[ 4 (i - 1) + 1, 4 i ] }, If[ OddQ[ i ], row, Reverse[ row ] ] ], { i, 4 } ] },
    { InfraVolume[ g, InfraLine[ {snake} ], "Volume" -> "Interior" ],
      InfraVolume[ g, InfraSet[ snake ], "Volume" -> "Interior" ],
      InfraVolume[ g, InfraLine[ {snake} ], "Volume" -> "Count" ]
        === InfraVolume[ g, InfraSet[ snake ], "Volume" -> "Count" ] } ],
  { 2, 16, True },
  TestID -> "InfraVolume-line-vs-set-spanning-curve"
]

(* The line graph is the union of the walks, NOT the induced subgraph: two parallel
   grid rows stay disconnected, so neither row gains interior from the other. *)
VerificationTest[
  InfraVolume[ GridGraph[ {4, 4} ], InfraLine[ {{1, 2, 3, 4}, {5, 6, 7, 8}} ], "Volume" -> "Interior" ],
  0,
  TestID -> "InfraVolume-line-union-not-induced"
]

(* ===== FindInfraEquidistantSet ===== *)

(* Every vertex of the equidistant set sees all anchors at one common distance. *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    AllTrue[ FindInfraEquidistantSet[ g, {1, 5, 21} ][ "Vertices" ],
      v |-> SameQ @@ ( GraphDistance[ g, #, v ] & /@ {1, 5, 21} ) ] ],
  True,
  TestID -> "FindInfraEquidistantSet-all-equidistant"
]

(* E(p1, ..., pn) == intersection of the n-1 consecutive perpendicular bisectors. *)
VerificationTest[
  Module[ { g = GridGraph[ {4, 4, 4} ], ps = {1, 5, 21} },
    Sort @ FindInfraEquidistantSet[ g, ps ][ "Vertices" ] ===
      Sort[ Intersection @@ MapThread[
        {a, b} |-> Select[ VertexList[ g ], v |-> GraphDistance[ g, a, v ] == GraphDistance[ g, b, v ] ],
        { Most[ ps ], Rest[ ps ] } ] ] ],
  True,
  TestID -> "FindInfraEquidistantSet-consecutive-bisector-identity"
]

(* For n == 2 the strict set is the window {0, 0} perpendicular bisector. *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    Sort @ FindInfraEquidistantSet[ g, {1, 25} ][ "Vertices" ] ===
      Sort @ FindInfraBisectingHyperplane[ g, 1, 25, {0, 0}, All ][[ 1, 1, 1 ]] ],
  True,
  TestID -> "FindInfraEquidistantSet-n2-equals-bisector"
]

(* Three corners of the square grid meet at the centre. *)
VerificationTest[
  FindInfraEquidistantSet[ GridGraph[ {5, 5} ], {1, 5, 21} ][ "Vertices" ],
  {13},
  TestID -> "FindInfraEquidistantSet-three-corners-centre"
]

(* Output head is InfraSet. *)
VerificationTest[
  Head @ FindInfraEquidistantSet[ GridGraph[ {5, 5} ], {1, 5, 21} ],
  InfraSet,
  TestID -> "FindInfraEquidistantSet-head"
]

(* Widening the window can only grow the set (the strict set is a subset). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    SubsetQ[ FindInfraEquidistantSet[ g, {1, 25}, {-1, 1} ][ "Vertices" ],
             FindInfraEquidistantSet[ g, {1, 25} ][ "Vertices" ] ] ],
  True,
  TestID -> "FindInfraEquidistantSet-window-monotone"
]

(* ===== FindAdvancingInfraFront ===== *)

(* The bouncing front moves each vertex one step outward from the previous front
   and reflects it inward where it cannot advance. On a triangle from 1 it expands
   to {2,3} then turns straight back to {1}, oscillating with no dwell -- never
   empties, unlike the metric sphere. *)
VerificationTest[
  Sort /@ ( #[ "Vertices" ] & /@ FindAdvancingInfraFront[ CompleteGraph[ 3 ], 1, 5 ] ),
  {{1}, {2, 3}, {1}, {2, 3}, {1}, {2, 3}},
  TestID -> "FindAdvancingInfraFront-triangle-bounces"
]

(* The defining property: on a finite connected graph the front never empties. *)
VerificationTest[
  Min[ #[ "Length" ] & /@ FindAdvancingInfraFront[ GridGraph[ {4, 30} ], 2, 120 ] ] >= 1,
  True,
  TestID -> "FindAdvancingInfraFront-never-empties"
]

(* steps + 1 fronts, each an InfraSet, the seed front S_0 = {origin}. *)
VerificationTest[
  With[ { f = FindAdvancingInfraFront[ CycleGraph[ 9 ], 1, 4 ] },
    Length[ f ] === 5 && AllTrue[ f, MatchQ[ #, InfraSet[ _List ] ] & ] && First[ f ] === InfraSet[ {1} ] ],
  True,
  TestID -> "FindAdvancingInfraFront-shape-and-seed"
]

(* Locality: S_{i+1} subset of S_i union N(S_i) (each vertex moves to a neighbour). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    AllTrue[
      Partition[ #[ "Vertices" ] & /@ FindAdvancingInfraFront[ g, 13, 6 ], 2, 1 ],
      SubsetQ[ Union[ #[[ 1 ]], Union @@ ( AdjacencyList[ g, # ] & /@ #[[ 1 ]] ) ], #[[ 2 ]] ] & ] ],
  True,
  TestID -> "FindAdvancingInfraFront-local-step"
]

(* It bounces: from the centre of a path the wave runs to the ends, reflects, and
   refocuses back at the origin, so {5} recurs as a later front. *)
VerificationTest[
  MemberQ[ Rest[ #[ "Vertices" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 12 ] ], {5} ],
  True,
  TestID -> "FindAdvancingInfraFront-refocuses-at-origin"
]

(* Immediate bounce: no two consecutive fronts are equal (longest run is 1). *)
VerificationTest[
  Max[ Length /@ Split[ Sort /@ ( #[ "Vertices" ] & /@ FindAdvancingInfraFront[ PathGraph @ Range @ 9, 5, 30 ] ) ] ],
  1,
  TestID -> "FindAdvancingInfraFront-immediate-bounce"
]

(* InfraPoint origin seeds a multi-source front: S_0 is the source set. *)
VerificationTest[
  Sort @ First[ FindAdvancingInfraFront[ GridGraph[ {6, 6} ], InfraPoint[ {1, 36} ], 5 ] ][ "Vertices" ],
  {1, 36},
  TestID -> "FindAdvancingInfraFront-multi-source-seed"
]

EndTestSection[]
