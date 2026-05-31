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

EndTestSection[]
