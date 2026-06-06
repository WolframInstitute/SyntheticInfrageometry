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

(* Orientation forbids the immediate reversal: reaching vertex 1 of a triangle,
   the next front is {2,3}, and the step after stays on {2,3} (does NOT fall back
   to 1) -- only the third step closes the loop. The front never empties: it is
   period-3 and propagates indefinitely, distinguishing it from the metric sphere
   (which would die after {2,3}). *)
VerificationTest[
  Sort /@ ( #[ "Vertices" ] & /@ FindAdvancingInfraFront[ CompleteGraph[ 3 ], 1, 5 ] ),
  {{1}, {2, 3}, {2, 3}, {1}, {2, 3}, {2, 3}},
  TestID -> "FindAdvancingInfraFront-triangle-orientation"
]

(* On a finite connected graph the oriented front never empties. *)
VerificationTest[
  AllTrue[ FindAdvancingInfraFront[ GridGraph[ {4, 30} ], 2, 120 ], #[ "Length" ] >= 1 & ],
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

(* Locality: the front advances by adjacency, S_{i+1} subset of N(S_i). *)
VerificationTest[
  With[ { g = GridGraph[ {5, 5} ] },
    AllTrue[
      Partition[ #[ "Vertices" ] & /@ FindAdvancingInfraFront[ g, 13, 6 ], 2, 1 ],
      SubsetQ[ Union @@ ( AdjacencyList[ g, # ] & /@ #[[ 1 ]] ), #[[ 2 ]] ] & ] ],
  True,
  TestID -> "FindAdvancingInfraFront-adjacency-local"
]

(* On a 1D band (ring) the front stays a thin circulating pulse: at most two
   counter-rotating arrowheads, never zero. *)
VerificationTest[
  With[ { sizes = #[ "Length" ] & /@ FindAdvancingInfraFront[ CycleGraph[ 12 ], 1, 40 ] },
    Min[ sizes ] >= 1 && Max[ sizes ] <= 2 ],
  True,
  TestID -> "FindAdvancingInfraFront-ring-coherent"
]

(* InfraPoint origin seeds a multi-source front: S_0 is the source set. List
   vertices (TorusTessellation, {i,j}) are handled by the {a,b}-pair encoding. *)
VerificationTest[
  Sort @ First[ FindAdvancingInfraFront[ GridGraph[ {6, 6} ], InfraPoint[ {1, 36} ], 5 ] ][ "Vertices" ],
  {1, 36},
  TestID -> "FindAdvancingInfraFront-multi-source-seed"
]

VerificationTest[
  With[ { g = TorusTessellation[ {4, 4}, "Triangular" ] },
    With[ { o = First @ VertexList[ g ] },
      AllTrue[ FindAdvancingInfraFront[ g, o, 12 ], #[ "Length" ] >= 1 & ] &&
      First[ FindAdvancingInfraFront[ g, o, 12 ] ] === InfraSet[ {o} ] ] ],
  True,
  TestID -> "FindAdvancingInfraFront-list-vertices"
]

(* "SelfAvoidanceDepth" -> 2 forbids closing a 3-cycle: at step 3 the walk
   1->2->3 cannot return to 1, so the front dies there instead of looping back to
   {1} as the default (period-3) does. *)
VerificationTest[
  Sort /@ ( #[ "Vertices" ] & /@ FindAdvancingInfraFront[ CompleteGraph[ 3 ], 1, 4, "SelfAvoidanceDepth" -> 2 ] ),
  {{1}, {2, 3}, {2, 3}, {}, {}},
  TestID -> "FindAdvancingInfraFront-self-avoidance-depth-2"
]

(* "GeodesicSprayDepth" -> Infinity keeps the wave radial about the origin: on a
   surface it stays a band (never fills) yet never empties -- the global frame.
   The default front (no reference) floods to the whole vertex set. *)
VerificationTest[
  With[ { g = TorusTessellation[ {8, 8}, "Triangular" ] },
    With[ { o = First @ VertexList[ g ] },
      With[ { plain = #[ "Length" ] & /@ FindAdvancingInfraFront[ g, o, 12 ],
              band  = #[ "Length" ] & /@ FindAdvancingInfraFront[ g, o, 12, "GeodesicSprayDepth" -> Infinity ] },
        Max[ plain ] === VertexCount[ g ] && Max[ band ] < VertexCount[ g ] && Min[ band ] >= 1 ] ] ],
  True,
  TestID -> "FindAdvancingInfraFront-radial-origin-stays-band"
]

(* A LOCAL radial frame (k-th predecessor) imposes transversality but, unlike the
   origin frame, only delays flooding -- a surface still fills. *)
VerificationTest[
  With[ { g = TorusTessellation[ {8, 8}, "Triangular" ] },
    With[ { o = First @ VertexList[ g ] },
      Max[ #[ "Length" ] & /@ FindAdvancingInfraFront[ g, o, 12, "GeodesicSprayDepth" -> 1 ] ] === VertexCount[ g ] ] ],
  True,
  TestID -> "FindAdvancingInfraFront-radial-local-floods"
]

EndTestSection[]
