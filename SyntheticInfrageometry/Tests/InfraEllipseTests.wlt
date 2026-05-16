BeginTestSection["InfraEllipse"]

(* ===== FindInfraEllipse: basic cycle finding ===== *)

(* GridGraph[{4,4}], foci {2, 15}: level set at sum=4 is the 2x4 inner strip
   {2,3,6,7,10,11,14,15}.  The shortest simple cycles in the induced subgraph
   are 4-cycles: {2,3,7,6}, {6,7,11,10}, {10,11,15,14}. *)

VerificationTest[
  Length @ First @ First @ First @ FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4 ],
  4,
  TestID -> "FindInfraEllipse-Grid4x4-shortest-cycle-length-4"
]

(* Cycle vertices lie inside the level set *)
VerificationTest[
  SubsetQ[
    { 2, 3, 6, 7, 10, 11, 14, 15 },
    First @ First @ First @ FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4 ]
  ],
  True,
  TestID -> "FindInfraEllipse-Grid4x4-cycle-in-level-set"
]

(* All cycles returned with All are within the level set *)
VerificationTest[
  AllTrue[
    #[[ 1, 1 ]] & /@ FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, All ],
    SubsetQ[ { 2, 3, 6, 7, 10, 11, 14, 15 }, # ] &
  ],
  True,
  TestID -> "FindInfraEllipse-Grid4x4-all-cycles-in-level-set"
]

(* Cycles sorted by length ascending *)
VerificationTest[
  With[ {
      shortest = Length @ First @ First @ First @
        FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4 ],
      allLengths = Length /@ (#[[ 1, 1 ]] & /@
        FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4, All ]) },
    shortest <= Min @ allLengths
  ],
  True,
  TestID -> "FindInfraEllipse-Grid4x4-sorted-by-length"
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
      FindInfraEllipse[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4 ] },
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
