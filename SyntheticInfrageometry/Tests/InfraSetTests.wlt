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

EndTestSection[]
