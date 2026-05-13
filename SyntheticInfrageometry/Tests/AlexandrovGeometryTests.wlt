BeginTestSection["AlexandrovGeometry"]


(* ===== InfraAngle Method dispatch ===== *)

(* PunchOut at p = 2 in C_5: radius = 1, the open ball deletes only vertex 2
   itself (distance 0 < 1).  In the remaining graph on {1, 3, 4, 5},
   d(1, 3) = 3 (via 1-5-4-3), so the angle is 3 / 1 = 3. *)
VerificationTest[
  InfraAngle[ CycleGraph[ 5 ], { 1, 2, 3 } ],
  3,
  TestID -> "InfraAngle-CycleGraph5-PunchOut-default-unchanged"
]

VerificationTest[
  InfraAngle[ CompleteGraph[ 3 ], { 1, 2, 3 }, Method -> "Comparison" ],
  Pi / 3,
  SameTest -> ( Abs[ N[ #1 ] - N[ #2 ] ] < 10^-10 & ),
  TestID -> "InfraAngle-K3-Comparison-Pi-over-3"
]

VerificationTest[
  InfraAngle[ PathGraph[ Range[ 3 ] ], { 1, 2, 3 }, Method -> "Comparison" ],
  Pi,
  SameTest -> ( Abs[ N[ #1 ] - N[ #2 ] ] < 10^-10 & ),
  TestID -> "InfraAngle-P3-Comparison-degenerate-Pi"
]

VerificationTest[
  N @ InfraAngle[ CompleteGraph[ 3 ], { 1, 2, 3 },
        Method -> { "Comparison", "Curvature" -> 0 } ],
  N[ Pi / 3 ],
  SameTest -> ( Abs[ #1 - #2 ] < 10^-10 & ),
  TestID -> "InfraAngle-Comparison-Curvature0-matches-Euclidean"
]


(* ===== ComparisonTriangle ===== *)

VerificationTest[
  Head @ ComparisonTriangle[ 3, 4, 5 ],
  Triangle,
  TestID -> "ComparisonTriangle-Euclidean-default-Triangle-head"
]

(* Law-of-cosines angles of the 3-4-5 right triangle sum to Pi.  We Chop the
   numerical residual to 0 because the symbolic difference
   ArcCos[3/5] + ArcCos[4/5] - Pi/2 is exactly zero, which triggers
   N::meprec under a naive Abs[...] < eps check. *)
VerificationTest[
  With[ { angles = ArcCos /@ {
        ( 4^2 + 5^2 - 3^2 ) / ( 2 4 5 ),
        ( 3^2 + 5^2 - 4^2 ) / ( 2 3 5 ),
        ( 3^2 + 4^2 - 5^2 ) / ( 2 3 4 )
      } },
    Chop[ N[ Total[ angles ] - Pi ] ] == 0
  ],
  True,
  TestID -> "ComparisonTriangle-345-angles-sum-Pi"
]

VerificationTest[
  Head @ ComparisonTriangle[ 2, 2, 2, "Curvature" -> -1 ],
  InfraComparisonTriangle,
  TestID -> "ComparisonTriangle-hyperbolic-wrapper"
]

VerificationTest[
  ComparisonTriangle[ 2, 2, 2, "Curvature" -> -1 ][ "Curvature" ],
  -1,
  TestID -> "InfraComparisonTriangle-Curvature-accessor"
]

VerificationTest[
  ComparisonTriangle[ 2, 2, 2, "Curvature" -> -1 ][ "Sides" ],
  { 2, 2, 2 },
  TestID -> "InfraComparisonTriangle-Sides-accessor"
]


(* ===== CATInequalityQ ===== *)

VerificationTest[
  CATInequalityQ[ PathGraph[ Range[ 5 ] ], { 1, 3, 5 }, 0 ],
  True,
  TestID -> "CATInequalityQ-Path-degenerate-true"
]

VerificationTest[
  CATInequalityQ[ CompleteGraph[ 3 ], { 1, 2, 3 }, 0 ],
  True,
  TestID -> "CATInequalityQ-K3-vacuous-true"
]

(* C6 with the equilateral triangle {1, 3, 5}: sides 2-2-2. Apex 1 sees the
   opposite-side midpoint 4 at graph distance 3, while the Euclidean
   comparison gives d_bar^2 = 3 < 9.  So C6 is not CAT(0) and the inequality
   correctly fails -- this is the standard "cycles of length >= 5 fail CAT(0)"
   counterexample. *)
VerificationTest[
  CATInequalityQ[ CycleGraph[ 6 ], { 1, 3, 5 }, 0 ],
  False,
  TestID -> "CATInequalityQ-C6-equilateral-2-2-2-not-CAT0"
]

(* Default Method equals "ApexSide" (preserves backward compatibility). *)
VerificationTest[
  CATInequalityQ[ CycleGraph[ 6 ], { 1, 3, 5 }, 0 ] ===
    CATInequalityQ[ CycleGraph[ 6 ], { 1, 3, 5 }, 0, Method -> "ApexSide" ],
  True,
  TestID -> "CATInequalityQ-default-Method-equals-ApexSide"
]

(* TwoRays branch -- basic plumbing. *)

VerificationTest[
  CATInequalityQ[ PathGraph[ Range[ 5 ] ], { 1, 3, 5 }, 0, Method -> "TwoRays" ],
  True,
  TestID -> "CATInequalityQ-Path-degenerate-TwoRays-true"
]

VerificationTest[
  CATInequalityQ[ CompleteGraph[ 3 ], { 1, 2, 3 }, 0, Method -> "TwoRays" ],
  True,
  TestID -> "CATInequalityQ-K3-vacuous-TwoRays-true"
]

(* C6 {1, 3, 5} is rejected by both formulations (not CAT(0)). *)
VerificationTest[
  CATInequalityQ[ CycleGraph[ 6 ], { 1, 3, 5 }, 0, Method -> "TwoRays" ],
  False,
  TestID -> "CATInequalityQ-C6-equilateral-2-2-2-TwoRays-not-CAT0"
]

(* The k > 0 perimeter guard fires the same way for either Method. *)
VerificationTest[
  CATInequalityQ[ CycleGraph[ 6 ], { 1, 3, 5 }, 4, Method -> "TwoRays" ],
  Indeterminate,
  TestID -> "CATInequalityQ-spherical-perimeter-too-large-Indeterminate"
]


(* ===== InfraCurvature ===== *)

VerificationTest[
  InfraCurvature[ PathGraph[ Range[ 5 ] ], 3 ],
  -Infinity,
  TestID -> "InfraCurvature-Path-tree-no-triangles-MinusInfinity"
]

VerificationTest[
  InfraCurvature[ CompleteGraph[ 4 ], 1, "Radius" -> 1 ],
  -Infinity,
  TestID -> "InfraCurvature-K4-1-1-1-only-discrete-vacuous-MinusInfinity"
]

VerificationTest[
  AssociationQ @ InfraCurvature[ CycleGraph[ 6 ] ],
  True,
  TestID -> "InfraCurvature-bulk-form-returns-Association"
]

VerificationTest[
  Length @ InfraCurvature[ CycleGraph[ 6 ] ],
  6,
  TestID -> "InfraCurvature-bulk-form-keys-all-vertices"
]

VerificationTest[
  Length @ DeleteDuplicates[ Values @ InfraCurvature[ CycleGraph[ 6 ] ] ],
  1,
  TestID -> "InfraCurvature-CycleGraph6-vertex-transitive-constant"
]


EndTestSection[]
