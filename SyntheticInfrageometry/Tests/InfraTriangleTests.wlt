BeginTestSection["InfraTriangle"]

polylineToKnots = WolframInstitute`SyntheticInfrageometry`PackageScope`polylineToKnots;

(* a triangle is its three geodesic sides, one directed path graph each, closing on the first corner *)

(* ===================== FindInfraTriangle ===================== *)

VerificationTest[
  With[ { res = FindInfraTriangle[ GridGraph[ { 4, 4 } ], { 1, 4, 13 } ] },
    MatchQ[ res, { _Graph, _Graph, _Graph } ] && Most @ polylineToKnots @ res === { 1, 4, 13 }
  ],
  True,
  TestID -> "FindInfraTriangle-grid-basic"
]

VerificationTest[
  Length @ FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, 1 ],
  1,
  TestID -> "FindInfraTriangle-default-one"
]

(* Cartesian product over the 6 geodesics of the diagonal side 9 -> 1. *)
VerificationTest[
  Length @ FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All ],
  6,
  TestID -> "FindInfraTriangle-cartesian"
]

VerificationTest[
  AllTrue[ FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, All ],
    InfraTriangleQ[ GridGraph[ { 3, 3 } ], # ] & ],
  True,
  TestID -> "FindInfraTriangle-all-valid"
]

(* the whole family passes at once, and is a legal HighlightGraph argument *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { InfraTriangleQ[ g, FindInfraTriangle[ g, { 1, 3, 9 }, All ] ],
      Head @ HighlightGraph[ g, FindInfraTriangle[ g, { 1, 3, 9 } ] ] } ],
  { True, Graph },
  TestID -> "FindInfraTriangle-family-and-highlight"
]

VerificationTest[
  FindInfraTriangle[ GridGraph[ { 3, 3 } ], { 1, 3, 9 }, Method -> "Bogus" ],
  $Failed,
  { FindInfraTriangle::badmethod },
  TestID -> "FindInfraTriangle-badmethod"
]

(* InfraTriangleQ rejects a four-sided chain. *)
VerificationTest[
  InfraTriangleQ[ GridGraph[ { 4, 4 } ], FindInfraPolygon[ GridGraph[ { 4, 4 } ], { 1, 4, 16, 13 } ] ],
  False,
  TestID -> "InfraTriangleQ-square-False"
]


EndTestSection[]
