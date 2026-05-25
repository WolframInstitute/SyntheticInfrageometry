BeginTestSection["InfraPolygon"]

(* ===================== InfraPolygon wrapper ===================== *)

VerificationTest[
  InfraPolygon[ { { 1, 2, 3, 4, 5, 6 } } ][ "Length" ],
  { 6 },
  TestID -> "InfraPolygon-Length-single"
]

VerificationTest[
  InfraPolygon[ { { 1, 2, 3 }, { 4, 5, 6, 7 } } ][ "Length" ],
  { 3, 4 },
  TestID -> "InfraPolygon-Length-multi"
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


EndTestSection[]
