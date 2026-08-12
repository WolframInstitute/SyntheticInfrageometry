BeginTestSection["InfraTriangle"]

(* ===================== InfraTriangle wrapper ===================== *)

VerificationTest[
  InfraTriangle[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 4 } } ], InfraSegment[ { { 4, 1 } } ] } } ][ "Length" ],
  { 4 },
  TestID -> "InfraTriangle-Length-single"
]

VerificationTest[
  InfraTriangle[ { { InfraSegment[ { { 1, 2, 3 } } ], InfraSegment[ { { 3, 4 } } ], InfraSegment[ { { 4, 1 } } ] } } ][ "Vertices" ],
  { { InfraPoint[ { 1 } ], InfraPoint[ { 3 } ], InfraPoint[ { 4 } ] } },
  TestID -> "InfraTriangle-Vertices-single"
]

VerificationTest[
  InfraTriangle[ { InfraTriangle[ { { InfraSegment[ { { 1, 2 } } ] } } ], InfraTriangle[ { { InfraSegment[ { { 3, 4 } } ] } } ] } ],
  InfraTriangle[ { { InfraSegment[ { { 1, 2 } } ] }, { InfraSegment[ { { 3, 4 } } ] } } ],
  TestID -> "InfraTriangle-auto-flatten"
]


(* ===================== FindInfraTriangle ===================== *)

VerificationTest[
  With[ { res = FindInfraTriangle[ GridGraph[ { 4, 4 } ], { 1, 4, 13 } ] },
    Head[ res ] === InfraTriangle &&
    Length[ res[ "Sides" ][[ 1 ]] ] == 3
  ],
  True,
  TestID -> "FindInfraTriangle-grid-basic"
]

VerificationTest[
  Length @ FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, 1 ][ "Realizations" ],
  1,
  TestID -> "FindInfraTriangle-default-one"
]

(* Cartesian product over the 6 geodesics of the diagonal side 9 -> 1. *)
VerificationTest[
  Length @ FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All ][ "Realizations" ],
  6,
  TestID -> "FindInfraTriangle-cartesian"
]

VerificationTest[
  AllTrue[ FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All ][ "Realizations" ],
    InfraTriangleQ[ GridGraph[ { 3, 3 } ], # ] & ],
  True,
  TestID -> "FindInfraTriangle-all-valid"
]

(* InfraTriangleQ rejects a four-sided chain. *)
VerificationTest[
  InfraTriangleQ[ GridGraph[ { 4, 4 } ], FindInfraPolygon[ GridGraph[ { 4, 4 } ], { 1, 4, 16, 13 } ][ "Realizations" ][[ 1 ]] ],
  False,
  TestID -> "InfraTriangleQ-square-False"
]


EndTestSection[]
