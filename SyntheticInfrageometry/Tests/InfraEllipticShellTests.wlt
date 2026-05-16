BeginTestSection["InfraEllipticShell"]

(* ===== FindInfraEllipticShell: basic level sets ===== *)

(* PathGraph 1-2-3-4-5, foci {1, 3}:
   d(1,v)+d(3,v) = 2,2,2,4,6 for v=1..5 *)

VerificationTest[
  Sort @ First @ First @ First @ FindInfraEllipticShell[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2 ],
  { 1, 2, 3 },
  TestID -> "FindInfraEllipticShell-PathGraph-c2-metric-interval"
]

VerificationTest[
  First @ First @ First @ FindInfraEllipticShell[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 4 ],
  { 4 },
  TestID -> "FindInfraEllipticShell-PathGraph-c4-singleton"
]

VerificationTest[
  First @ First @ First @ FindInfraEllipticShell[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 6 ],
  { 5 },
  TestID -> "FindInfraEllipticShell-PathGraph-c6-endpoint"
]

(* ===== Range form {cMin, cMax} ===== *)

VerificationTest[
  Sort @ First @ First @ First @
    FindInfraEllipticShell[ PathGraph[ Range[ 5 ] ], { 1, 3 }, { 4, 6 } ],
  { 4, 5 },
  TestID -> "FindInfraEllipticShell-PathGraph-range-c4-c6"
]

(* ===== GridGraph level set: 2x4 inner strip ===== *)

(* GridGraph[{4,4}], foci 2=(1,2) and 15=(4,3):
   d(2,v)+d(15,v) = 4 for vertices in the 2-column inner strip *)

VerificationTest[
  Sort @ First @ First @ First @
    FindInfraEllipticShell[ GridGraph[ { 4, 4 } ], { 2, 15 }, 4 ],
  Sort[ { 2, 3, 6, 7, 10, 11, 14, 15 } ],
  TestID -> "FindInfraEllipticShell-Grid4x4-inner-strip"
]

(* ===== Count forms ===== *)

VerificationTest[
  Length @ FindInfraEllipticShell[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2, All ] >= 1,
  True,
  TestID -> "FindInfraEllipticShell-PathGraph-All-nonempty"
]

(* Properties empty → Method ignored, returns single level set *)
VerificationTest[
  Length @ FindInfraEllipticShell[ PathGraph[ Range[ 5 ] ], { 1, 3 }, 2, All,
    Properties -> { } ],
  1,
  TestID -> "FindInfraEllipticShell-empty-properties-one-realisation"
]

(* ===== InfraEllipticShell wrapper ===== *)

VerificationTest[
  InfraEllipticShell[ { InfraEllipticShell[ { { 1, 2 } } ], InfraEllipticShell[ { { 3, 4 } } ] } ],
  InfraEllipticShell[ { { 1, 2 }, { 3, 4 } } ],
  TestID -> "InfraEllipticShell-auto-flatten-nested"
]

VerificationTest[
  InfraEllipticShell[ { { 1, 2, 3 } } ],
  InfraEllipticShell[ { { 1, 2, 3 } } ],
  TestID -> "InfraEllipticShell-unary-no-flatten"
]

(* ===== InfraEllipticShellQ ===== *)

(* {1,2,3} is the level set of foci {1,3} at sum 2 on PathGraph[Range[5]] *)
VerificationTest[
  InfraEllipticShellQ[ PathGraph[ Range[ 5 ] ], { 1, 2, 3 } ],
  True,
  TestID -> "InfraEllipticShellQ-PathGraph-metric-interval-true"
]

(* {1,3,5} cannot be an elliptic shell on PathGraph[Range[5]] *)
VerificationTest[
  InfraEllipticShellQ[ PathGraph[ Range[ 5 ] ], { 1, 3, 5 } ],
  False,
  TestID -> "InfraEllipticShellQ-PathGraph-alternating-false"
]

(* {2,3,6,7,10,11,14,15} is the level set of foci {2,15} at sum 4 on GridGraph[{4,4}] *)
VerificationTest[
  InfraEllipticShellQ[ GridGraph[ { 4, 4 } ], { 2, 3, 6, 7, 10, 11, 14, 15 } ],
  True,
  TestID -> "InfraEllipticShellQ-Grid4x4-inner-strip-true"
]

EndTestSection[]
