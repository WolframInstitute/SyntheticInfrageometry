BeginTestSection["InfraPolygon"]

(* ===================== InfraPolygon wrapper ===================== *)

(* Length = perimeter edge count (sum of leg edge counts) per realisation. *)
VerificationTest[
  InfraPolygon[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 4 } } ], InfraSegment[ { { 4, 1 } } ] } } ][ "Length" ],
  { 4 },
  TestID -> "InfraPolygon-Length-single"
]

VerificationTest[
  InfraPolygon[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 1 } } ] },
                  { InfraSegment[ { { 1, 2 } } ], InfraSegment[ { { 2, 3 } } ], InfraSegment[ { { 3, 1 } } ] } } ][ "Length" ],
  { 3, 3 },
  TestID -> "InfraPolygon-Length-multi"
]

(* Sides returns the leg list per realisation; Vertices the corner points. *)
VerificationTest[
  InfraPolygon[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 4 } } ], InfraSegment[ { { 4, 1 } } ] } } ][ "Vertices" ],
  { { InfraPoint[ { 1 } ], InfraPoint[ { 3 } ], InfraPoint[ { 4 } ] } },
  TestID -> "InfraPolygon-Vertices-single"
]

VerificationTest[
  InfraPolygon[ { InfraPolygon[ { { 1, 2, 3 } } ], InfraPolygon[ { { 4, 5, 6 } } ] } ],
  InfraPolygon[ { { 1, 2, 3 }, { 4, 5, 6 } } ],
  TestID -> "InfraPolygon-auto-flatten"
]


(* ===================== FindInfraRegularPolygon: equilateral case ===================== *)

(* The cycle graph itself: with A_1 = 1, the distance-1 graph IS CycleGraph[6],
   so the unique 6-cycle is found. *)

VerificationTest[
  With[ { res = FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1 }, 6 ] },
    Head /@ res === { InfraPolygon } && Length[ ( First @ res )[[ 1, 1 ]] ] == 6
  ],
  True,
  TestID -> "FindInfraRegularPolygon-cycle6-equilateral"
]

VerificationTest[
  Length @ FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1 }, 6, All ],
  1,
  TestID -> "FindInfraRegularPolygon-cycle6-count-All"
]

VerificationTest[
  Length @ FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1 }, 6, UpTo[ 5 ] ],
  1,
  TestID -> "FindInfraRegularPolygon-cycle6-count-UpTo"
]


(* ===================== Full metric profile ===================== *)

(* The 6-cycle satisfies the full {1, 2, 3} profile: consecutive at distance 1,
   2-diagonals at distance 2, 3-diagonals (antipodes) at distance 3. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1, 2, 3 }, 6, All ],
  1,
  TestID -> "FindInfraRegularPolygon-cycle6-full-profile"
]

(* Asking for a 2-diagonal at distance 5 in CycleGraph[6] cannot be satisfied
   (max distance is 3); strict count returns $Failed. *)

VerificationTest[
  FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1, 5 }, 6 ],
  $Failed,
  TestID -> "FindInfraRegularPolygon-cycle6-impossible-diagonal"
]


(* ===================== GridGraph ===================== *)

(* Unit squares in a 3x3 grid: 4 of them. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 3, 3 } ], { 1 }, 4, All ],
  4,
  TestID -> "FindInfraRegularPolygon-grid3x3-unit-squares"
]


(* ===================== PetersenGraph: 12 pentagons ===================== *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ PetersenGraph[ ], { 1 }, 5, All ],
  12,
  TestID -> "FindInfraRegularPolygon-petersen-12-pentagons"
]


(* ===================== No 4-cycles in a path graph ===================== *)

VerificationTest[
  FindInfraRegularPolygon[ PathGraph[ Range[ 5 ] ], { 1 }, 4 ],
  $Failed,
  TestID -> "FindInfraRegularPolygon-pathgraph-no-4-cycle"
]

VerificationTest[
  FindInfraRegularPolygon[ PathGraph[ Range[ 5 ] ], { 1 }, 4, All ],
  { },
  TestID -> "FindInfraRegularPolygon-pathgraph-no-4-cycle-All"
]


(* ===================== Wider distance A_1 ===================== *)

(* On PathGraph[Range[5]], the distance-2 graph has edges {1-3, 2-4, 3-5}.
   No 3-cycles or longer cycles, so {2}, n = 3 returns $Failed. *)

VerificationTest[
  FindInfraRegularPolygon[ PathGraph[ Range[ 5 ] ], { 2 }, 3 ],
  $Failed,
  TestID -> "FindInfraRegularPolygon-pathgraph-distance2-no-triangle"
]

(* On CycleGraph[8] with A_1 = 2: distance-2 graph is two disjoint CycleGraph[4]s
   ({1,3,5,7} and {2,4,6,8}), so 2 square cycles. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ CycleGraph[ 8 ], { 2 }, 4, All ],
  2,
  TestID -> "FindInfraRegularPolygon-cycle8-distance2-squares"
]


(* ===================== InfraRegularPolygonQ ===================== *)

VerificationTest[
  InfraRegularPolygonQ[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6 }, { 1, 2, 3 } ],
  True,
  TestID -> "InfraRegularPolygonQ-cycle6-full-profile"
]

VerificationTest[
  InfraRegularPolygonQ[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6, 1 }, { 1 } ],
  True,
  TestID -> "InfraRegularPolygonQ-cycle6-closed-input"
]

VerificationTest[
  InfraRegularPolygonQ[ CycleGraph[ 6 ], { 1, 2, 3, 4, 5, 6 }, { 1, 5 } ],
  False,
  TestID -> "InfraRegularPolygonQ-cycle6-wrong-diagonal"
]

VerificationTest[
  InfraRegularPolygonQ[ PathGraph[ Range[ 5 ] ], { 1, 2, 3, 4 }, { 1 } ],
  False,
  TestID -> "InfraRegularPolygonQ-pathgraph-not-a-cycle"
]

VerificationTest[
  InfraRegularPolygonQ[ CycleGraph[ 6 ], { 1, 2 }, { 1 } ],
  False,
  TestID -> "InfraRegularPolygonQ-too-short"
]


(* ===================== Error messages ===================== *)

VerificationTest[
  FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1 }, 6, Properties -> { "Convex" } ],
  $Failed,
  { FindInfraRegularPolygon::badproperty },
  TestID -> "FindInfraRegularPolygon-badproperty"
]

VerificationTest[
  FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1 }, 6, Method -> "Greedy" ],
  $Failed,
  { FindInfraRegularPolygon::badmethod },
  TestID -> "FindInfraRegularPolygon-badmethod"
]

VerificationTest[
  FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1, 2, 3, 4 }, 6 ],
  $Failed,
  { FindInfraRegularPolygon::badcount },
  TestID -> "FindInfraRegularPolygon-too-many-diagonals"
]

VerificationTest[
  FindInfraRegularPolygon[ CycleGraph[ 6 ], { "foo" }, 6 ],
  $Failed,
  { FindInfraRegularPolygon::badslot },
  TestID -> "FindInfraRegularPolygon-badslot"
]


(* ===================== Slot grammar: Automatic ===================== *)

(* GridGraph[{3, 3}] unit squares: sides 1, 2-diagonals constant 2.
   {1, Automatic} (any constant 2-diagonal) and {1, {2, 2}} (constant in [2, 2])
   both find them; {1, {3, 3}} finds none. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 3, 3 } ], { 1, Automatic }, 4, All ],
  4,
  TestID -> "FindInfraRegularPolygon-grid-Automatic-2diagonal"
]

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 3, 3 } ], { 1, { 2, 2 } }, 4, All ],
  4,
  TestID -> "FindInfraRegularPolygon-grid-range-2diagonal"
]

VerificationTest[
  FindInfraRegularPolygon[ GridGraph[ { 3, 3 } ], { 1, { 3, 3 } }, 4 ],
  $Failed,
  TestID -> "FindInfraRegularPolygon-grid-range-2diagonal-impossible"
]

(* PetersenGraph is 3-regular with girth 5; its 12 pentagons are all
   distance-1 5-cycles, so {Automatic} finds the same 12 as {1}. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ PetersenGraph[ ], { Automatic }, 5, All ],
  12,
  TestID -> "FindInfraRegularPolygon-petersen-Automatic-equilateral"
]

(* {Automatic, Automatic}: 2-diagonal must also be constant.  In Petersen
   every 2-diagonal in a 5-cycle is at distance 2, so all 12 pass. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ PetersenGraph[ ], { Automatic, Automatic }, 5, All ],
  12,
  TestID -> "FindInfraRegularPolygon-petersen-Automatic-pair"
]


(* ===================== InfraRegularPolygonQ slot grammar ===================== *)

(* A 3x3 grid unit square satisfies {1, Automatic} (sides 1, 2-diagonal
   constant = 2) but not {1, {3, 3}}. *)

VerificationTest[
  InfraRegularPolygonQ[ GridGraph[ { 3, 3 } ], { 1, 2, 5, 4 }, { 1, Automatic } ],
  True,
  TestID -> "InfraRegularPolygonQ-grid-Automatic-2diagonal"
]

VerificationTest[
  InfraRegularPolygonQ[ GridGraph[ { 3, 3 } ], { 1, 2, 5, 4 }, { 1, { 2, 2 } } ],
  True,
  TestID -> "InfraRegularPolygonQ-grid-range-2diagonal"
]

VerificationTest[
  InfraRegularPolygonQ[ GridGraph[ { 3, 3 } ], { 1, 2, 5, 4 }, { 1, { 3, 3 } } ],
  False,
  TestID -> "InfraRegularPolygonQ-grid-range-2diagonal-false"
]


(* ===================== "From" localization ===================== *)

(* GridGraph[{5, 5}] has 16 unit squares.  Center vertex is 13; the four
   unit squares incident to vertex 13 are {7, 8, 13, 12}, {8, 9, 14, 13},
   {12, 13, 18, 17}, {13, 14, 19, 18}. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All ],
  16,
  TestID -> "FindInfraRegularPolygon-grid5-default-From-All"
]

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All, "From" -> 13 ],
  4,
  TestID -> "FindInfraRegularPolygon-grid5-From-center-membership"
]

(* "From" -> v -> 1: squares whose vertices all lie in N_1(13) = {8, 12, 13, 14, 18}.
   No 4-cycle fits in this 5-vertex star, so the result is empty. *)

VerificationTest[
  FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All, "From" -> 13 -> 1 ],
  { },
  TestID -> "FindInfraRegularPolygon-grid5-From-radius1-empty"
]

(* "From" -> v -> 2: N_2(13) is large enough for unit squares around 13 to fit. *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All, "From" -> 13 -> 2 ] >= 4,
  True,
  TestID -> "FindInfraRegularPolygon-grid5-From-radius2-localized"
]

VerificationTest[
  FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, "From" -> 13 -> "bar" ],
  $Failed,
  { FindInfraRegularPolygon::badfrom },
  TestID -> "FindInfraRegularPolygon-badfrom"
]

(* "From" accepts InfraPoint[{v}] wrapper, unwrapping to a bare vertex. *)

VerificationTest[
  FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All,
    "From" -> InfraPoint[ { 13 } ] ] ===
  FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All, "From" -> 13 ],
  True,
  TestID -> "FindInfraRegularPolygon-From-InfraPoint-unary"
]

(* "From" accepts the list-of-unaries shape returned by FindInfraPoint. *)

VerificationTest[
  FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All,
    "From" -> FindInfraPoint[ GridGraph[ { 5, 5 } ], "From" -> "Center" ] -> 2 ] ===
  FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All,
    "From" -> 13 -> 2 ],
  True,
  TestID -> "FindInfraRegularPolygon-From-FindInfraPoint-pipe"
]

(* Multi-anchor InfraPoint[{v1, v2}] in localization: NeighborhoodGraph
   accepts a list, giving N_r(v1) union N_r(v2). *)

VerificationTest[
  Sort @ FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All,
    "From" -> InfraPoint[ { 1, 25 } ] -> 1 ] ===
  Sort @ FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All,
    "From" -> { 1, 25 } -> 1 ],
  True,
  TestID -> "FindInfraRegularPolygon-From-InfraPoint-multi-radius"
]

(* Multi-anchor membership: cycles containing at least one of the listed
   vertices.  In GridGraph[{5,5}], "From" -> InfraPoint[{1, 25}] should
   pick squares incident to corner 1 OR corner 25 = 2 squares total
   (one per corner). *)

VerificationTest[
  Length @ FindInfraRegularPolygon[ GridGraph[ { 5, 5 } ], { 1 }, 4, All,
    "From" -> InfraPoint[ { 1, 25 } ] ],
  2,
  TestID -> "FindInfraRegularPolygon-From-InfraPoint-multi-membership"
]


(* ===================== FindInfraPolygon (through corners) ===================== *)

VerificationTest[
  With[ { res = FindInfraPolygon[ GridGraph[ { 4, 4 } ], { 1, 4, 16, 13 } ] },
    Head /@ res === { InfraPolygon } &&
    Length[ ( First @ res )[ "Sides" ][[ 1 ]] ] == 4 &&
    ( First @ res )[ "Length" ] === { 12 }
  ],
  True,
  TestID -> "FindInfraPolygon-grid4x4-square"
]

(* Default count = 1 returns a single polygon. *)
VerificationTest[
  Length @ FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 } ],
  1,
  TestID -> "FindInfraPolygon-default-one"
]

(* All enumerates the Cartesian product of per-side geodesics; the diagonal
   side 9 -> 1 of GridGraph[{3,3}] has 6 geodesics, the other two are unique. *)
VerificationTest[
  Length @ FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All ],
  6,
  TestID -> "FindInfraPolygon-grid3x3-cartesian"
]

VerificationTest[
  AllTrue[ FindInfraPolygon[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All ],
    InfraPolygonQ[ GridGraph[ { 3, 3 } ], # ] & ],
  True,
  TestID -> "FindInfraPolygon-all-valid"
]

(* InfraPolygonQ rejects an open (non-closed) leg chain. *)
VerificationTest[
  InfraPolygonQ[ PathGraph[ Range[ 4 ] ],
    { InfraSegment[ { { 1, 2 } } ], InfraSegment[ { { 2, 3 } } ] } ],
  False,
  TestID -> "InfraPolygonQ-open-chain-False"
]

(* Migrated FindInfraRegularPolygon stores geodesic sides; the result is a
   valid InfraPolygon. *)
VerificationTest[
  With[ { res = FindInfraRegularPolygon[ CycleGraph[ 6 ], { 1 }, 6 ] },
    InfraPolygonQ[ CycleGraph[ 6 ], First @ res ]
  ],
  True,
  TestID -> "FindInfraRegularPolygon-segment-sides-valid"
]


EndTestSection[]
