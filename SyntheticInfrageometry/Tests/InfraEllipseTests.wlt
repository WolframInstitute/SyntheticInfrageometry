BeginTestSection["InfraEllipse"]

(* ===== FindInfraEllipse: default Properties -> {"Separating"}, Method -> "Peel" =====

   Default returns the shortest separating cycle around both foci.  A
   separating cycle requires a non-empty near region {sum < cMin}, so c
   must be > d(p1, p2).  On the 7x7 grid, foci {25, 12} are at distance 3
   and band {4, 8} gives a non-degenerate level surface that supports
   genuine elliptic loops. *)

VerificationTest[
  With[ { g = GridGraph[ { 7, 7 } ] },
    With[ { cyc = First @ First @ First @ FindInfraEllipse[ g, { 25, 12 }, { 4, 8 } ] },
      Length[ cyc ] >= 4 &&
      AllTrue[ Partition[ Append[ cyc, First @ cyc ], 2, 1 ],
        EdgeQ[ g, UndirectedEdge @@ # ] & ]
    ]
  ],
  True,
  TestID -> "FindInfraEllipse-default-returns-separating-cycle"
]

(* Cycle vertices lie inside the level band *)
VerificationTest[
  With[ {
      g = GridGraph[ { 7, 7 } ],
      dm = GraphDistanceMatrix @ GridGraph[ { 7, 7 } ] },
    With[ { cyc = First @ First @ First @ FindInfraEllipse[ g, { 25, 12 }, { 4, 8 } ] },
      AllTrue[ cyc, 4 <= dm[[ 25, # ]] + dm[[ 12, # ]] <= 8 & ]
    ]
  ],
  True,
  TestID -> "FindInfraEllipse-default-cycle-in-level-band"
]

(* Cycle separates near region from far region in the original graph *)
VerificationTest[
  With[ {
      g = GridGraph[ { 7, 7 } ],
      dm = GraphDistanceMatrix @ GridGraph[ { 7, 7 } ] },
    With[ { cyc = First @ First @ First @ FindInfraEllipse[ g, { 25, 12 }, { 4, 8 } ] },
      With[ {
          near = Select[ VertexList @ g, dm[[ 25, # ]] + dm[[ 12, # ]] < 4 & ],
          far  = Select[ VertexList @ g, dm[[ 25, # ]] + dm[[ 12, # ]] > 8 & ] },
        near =!= { } && far =!= { } &&
        GraphDistance[ VertexDelete[ g, cyc ], First @ near, First @ far ] === Infinity
      ]
    ]
  ],
  True,
  TestID -> "FindInfraEllipse-default-cycle-separates-near-from-far"
]

(* Properties -> {} reverts to "any simple cycle in level set" *)
VerificationTest[
  Length @ First @ First @ First @
    FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, Properties -> { } ],
  4,
  TestID -> "FindInfraEllipse-NoProperties-Grid4x4-shortest-cycle-length-4"
]

VerificationTest[
  SubsetQ[
    { 2, 3, 6, 7, 10, 11, 14, 15 },
    First @ First @ First @
      FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, Properties -> { } ]
  ],
  True,
  TestID -> "FindInfraEllipse-NoProperties-Grid4x4-cycle-in-level-set"
]

(* All cycles returned with All are within the level set *)
VerificationTest[
  AllTrue[
    #[[ 1, 1 ]] & /@ FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, All,
      Properties -> { } ],
    SubsetQ[ { 2, 3, 6, 7, 10, 11, 14, 15 }, # ] &
  ],
  True,
  TestID -> "FindInfraEllipse-NoProperties-Grid4x4-all-cycles-in-level-set"
]

(* Cycles sorted by length ascending *)
VerificationTest[
  With[ {
      shortest = Length @ First @ First @ First @
        FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, Properties -> { } ],
      allLengths = Length /@ (#[[ 1, 1 ]] & /@
        FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, All, Properties -> { } ]) },
    shortest <= Min @ allLengths
  ],
  True,
  TestID -> "FindInfraEllipse-NoProperties-Grid4x4-sorted-by-length"
]

(* Empty level set -> $Failed for count=1 *)
VerificationTest[
  FindInfraEllipse[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 100 ],
  $Failed,
  TestID -> "FindInfraEllipse-empty-level-set"
]

(* Level set with no cycle (path, not a cycle) -> $Failed for count=1 *)
VerificationTest[
  FindInfraEllipse[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2 ],
  $Failed,
  TestID -> "FindInfraEllipse-PathGraph-no-cycle-in-level-set"
]

(* ===== InfraEllipse wrapper ===== *)

VerificationTest[
  InfraEllipse[ { InfraEllipse[ { { 1, 2, 3 } } ], InfraEllipse[ { { 4, 5, 6 } } ] } ],
  InfraEllipse[ { { 1, 2, 3 }, { 4, 5, 6 } } ],
  TestID -> "InfraEllipse-auto-flatten-nested"
]

VerificationTest[
  InfraEllipse[ { { 1, 2, 3 } } ],
  InfraEllipse[ { { 1, 2, 3 } } ],
  TestID -> "InfraEllipse-unary-no-flatten"
]

(* ===== InfraEllipseQ ===== *)

(* A 4-cycle in the inner strip of GridGraph[{4,4}] is an ellipse *)
VerificationTest[
  With[ { cycle = First @ First @ First @
      FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, Properties -> { } ] },
    InfraEllipseQ[ GridGraph[ { 4, 4 } ], cycle ]
  ],
  True,
  TestID -> "InfraEllipseQ-Grid4x4-found-cycle-true"
]

(* A path is not an ellipse *)
VerificationTest[
  InfraEllipseQ[ GridGraph[ { 4, 4 } ], { 1, 2, 3, 4 } ],
  False,
  TestID -> "InfraEllipseQ-Grid4x4-path-false"
]

(* Short cycle: length < 3 is not an ellipse *)
VerificationTest[
  InfraEllipseQ[ CycleGraph[ 6 ], { 1, 2 } ],
  False,
  TestID -> "InfraEllipseQ-too-short-false"
]

EndTestSection[]
