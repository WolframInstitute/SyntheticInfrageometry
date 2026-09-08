BeginTestSection["InfraQuadric"]

(* PathGraph 1-2-3-4-5; foci {1, 3}: d(1,v)+d(3,v) = {2, 2, 2, 4, 6} *)

(* ===== Scalar c: solid interior { v : sum d <= c } ===== *)

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2 ],
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-ellipsoid-c2"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 3 ],
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-ellipsoid-c3"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 4 ],
  InfraSet[ { 1, 2, 3, 4 } ],
  TestID -> "FindInfraQuadric-PathGraph-ellipsoid-c4"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 6 ],
  InfraSet[ { 1, 2, 3, 4, 5 } ],
  TestID -> "FindInfraQuadric-PathGraph-ellipsoid-c6-all"
]

(* ===== Band c = {cMin, cMax} ===== *)

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, { 4, 6 } ],
  InfraSet[ { 4, 5 } ],
  TestID -> "FindInfraQuadric-PathGraph-band-4-6"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, { 2, 2 } ],
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-band-2-2-level-set"
]

(* ===== Explicit weights {1, 1} match default ===== *)

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2, { 1, 1 } ],
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2 ],
  TestID -> "FindInfraQuadric-PathGraph-explicit-unit-weights"
]

(* ===== Hyperboloid: weights {1, -1} =====
   d(1,v) - d(5,v) on PathGraph 1..5 = { -4, -2, 0, 2, 4 } *)

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 5 }, 0, { 1, -1 } ],
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-hyperboloid-half-c0"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 5 }, { -1, 1 }, { 1, -1 } ],
  InfraSet[ { 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-hyperboloid-twosided-c1"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 5 }, { -2, 2 }, { 1, -1 } ],
  InfraSet[ { 2, 3, 4 } ],
  TestID -> "FindInfraQuadric-PathGraph-hyperboloid-twosided-c2"
]

(* ===== InfraPoint foci ===== *)

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ],
    { InfraPoint[1], InfraPoint[3] }, 2 ],
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-InfraPoint-foci"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ],
    { InfraSet[ { 1, 4 } ], 3 }, 2 ],
  InfraSet[ { 1, 2, 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-multirealisation-first-only"
]

(* ===== Three foci (k >= 3) =====
   PathGraph 1..5, foci {1, 3, 5}: d-sums = { 6, 5, 4, 5, 6 } *)

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3, 5 }, 4 ],
  InfraSet[ { 3 } ],
  TestID -> "FindInfraQuadric-PathGraph-three-foci-c4"
]

VerificationTest[
  FindInfraQuadric[ PathGraph[ Range[ 5 ] ], { 1, 3, 5 }, 5 ],
  InfraSet[ { 2, 3, 4 } ],
  TestID -> "FindInfraQuadric-PathGraph-three-foci-c5"
]

(* ===== GridGraph sanity =====
   GridGraph[{4,4}], foci {1, 16} (opposite corners): sums == 6 along the
   anti-diagonal strip; solid c = 6 covers the whole graph. *)

VerificationTest[
  FindInfraQuadric[ GridGraph[ { 4, 4 } ], { 1, 16 }, 6 ][ "Length" ],
  16,
  TestID -> "FindInfraQuadric-GridGraph-corner-corner-solid"
]

EndTestSection[]
