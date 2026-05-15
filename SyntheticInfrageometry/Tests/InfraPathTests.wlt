BeginTestSection["InfraPath"]

(* ===================== InfraPath wrapper ===================== *)

VerificationTest[
  InfraPath[ { InfraPath[ { { 1, 2, 3 } } ], InfraPath[ { { 1, 3 } } ] } ],
  InfraPath[ { { 1, 2, 3 }, { 1, 3 } } ],
  TestID -> "InfraPath-auto-flatten"
]

VerificationTest[
  InfraPath[ { { 1, 2, 3 } } ],
  InfraPath[ { { 1, 2, 3 } } ],
  TestID -> "InfraPath-singleton-unchanged"
]

(* ===================== FindInfraPath ===================== *)

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5 ],
  { InfraPath[ { { 1, 2, 3, 4, 5 } } ] },
  TestID -> "FindInfraPath-default-single-path"
]

VerificationTest[
  Length @ FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
  TestID -> "FindInfraPath-All-matches-Wolfram-FindPath"
]

VerificationTest[
  AllTrue[
    FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
    MatchQ[ InfraPath[ { _List } ] ] ],
  True,
  TestID -> "FindInfraPath-output-shape"
]

VerificationTest[
  AllTrue[
    FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, 4, All ],
    p |-> InfraPathQ[ GridGraph[ { 3, 3 } ], First @ p[[ 1 ]] ] ],
  True,
  TestID -> "FindInfraPath-all-paths-pass-InfraPathQ"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5, { 4 }, 1 ],
  { InfraPath[ { { 1, 2, 3, 4, 5 } } ] },
  TestID -> "FindInfraPath-exact-length-spec"
]

VerificationTest[
  Length @ FindInfraPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  Length @ FindPath[ GridGraph[ { 3, 3 } ], 1, 9, { 4, 6 }, All ],
  TestID -> "FindInfraPath-range-length-spec"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, UpTo[ 10 ] ],
  { InfraPath[ { { 1, 2, 3, 4, 5 } } ] },
  TestID -> "FindInfraPath-UpTo-no-failure"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 1, 5, Infinity, 7 ],
  $Failed,
  TestID -> "FindInfraPath-strict-shortfall-Failed"
]

VerificationTest[
  FindInfraPath[ PathGraph[ Range[ 5 ] ], 3, 3 ],
  $Failed,
  TestID -> "FindInfraPath-degenerate-same-endpoints"
]

(* ===================== Multi-anchor spread ===================== *)

VerificationTest[
  Sort[ #[[ 1, 1, { 1, -1 } ]] & /@
    FindInfraPath[ PathGraph[ Range[ 5 ] ], InfraPoint[ { 1, 2 } ], 5, Infinity, All ] ],
  Sort[ { { 1, 5 }, { 2, 5 } } ],
  TestID -> "FindInfraPath-multi-source-spread"
]

EndTestSection[]
