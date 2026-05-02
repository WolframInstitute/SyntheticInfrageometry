BeginTestSection["Curvatures"]

(* ===== FormanRicciCurvature ===== *)

VerificationTest[
  Values @ FormanRicciCurvature[ CycleGraph[ 6 ] ],
  ConstantArray[ 0, 6 ],
  TestID -> "FormanRicciCurvature-CycleGraph6-zero"
]

VerificationTest[
  Values @ FormanRicciCurvature[ PathGraph[ Range[ 5 ] ] ],
  { 1, 0, 0, 1 },
  TestID -> "FormanRicciCurvature-PathGraph5-leaf-one-interior-zero"
]

VerificationTest[
  Values @ FormanRicciCurvature[ CompleteGraph[ 4 ], Method -> "Triangles" ],
  ConstantArray[ 4, 6 ],
  TestID -> "FormanRicciCurvature-CompleteGraph4-Triangles"
]

VerificationTest[
  Values @ FormanRicciCurvature[ CompleteGraph[ 4 ], Method -> "Simple" ],
  ConstantArray[ -2, 6 ],
  TestID -> "FormanRicciCurvature-CompleteGraph4-Simple"
]

VerificationTest[
  FormanRicciCurvature[ CycleGraph[ 6 ], Method -> "Bogus" ],
  $Failed,
  { FormanRicciCurvature::badmethod },
  TestID -> "FormanRicciCurvature-bad-method"
]


(* ===== OllivierRicciCurvature ===== *)

VerificationTest[
  Values @ OllivierRicciCurvature[ CompleteGraph[ 4 ] ],
  ConstantArray[ 2 / 3, 6 ],
  SameTest -> ( Max @ Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "OllivierRicciCurvature-CompleteGraph4"
]

VerificationTest[
  Values @ OllivierRicciCurvature[ PathGraph[ Range[ 5 ] ] ],
  ConstantArray[ 0, 4 ],
  SameTest -> ( Max @ Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "OllivierRicciCurvature-PathGraph5-zero"
]

VerificationTest[
  Values @ OllivierRicciCurvature[ CycleGraph[ 6 ] ],
  ConstantArray[ 0, 6 ],
  SameTest -> ( Max @ Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "OllivierRicciCurvature-CycleGraph6-zero"
]


(* ===== WolframRicciScalar ===== *)

VerificationTest[
  WolframRicciScalar[ CycleGraph[ 12 ], 1, { 1, 3 }, "Dimension" -> 1 ],
  <| 1 -> -9., 2 -> -1.125, 3 -> -1./3 |>,
  SameTest -> ( Max @ Abs[ Values[ #1 ] - Values[ #2 ] ] < 10^-8 & ),
  TestID -> "WolframRicciScalar-CycleGraph12-d1"
]

VerificationTest[
  AssociationQ @ WolframRicciScalar[ GridGraph[ { 5, 5 } ], 13 ],
  True,
  TestID -> "WolframRicciScalar-Grid5x5-default-range-association"
]

VerificationTest[
  Length @ WolframRicciScalar[ HypercubeGraph[ 3 ], 1, { 1, 2 }, "Dimension" -> 3 ],
  2,
  TestID -> "WolframRicciScalar-Hypercube3-d3-length"
]

VerificationTest[
  KeyExistsQ[ WolframRicciScalar[ PathGraph[ Range[ 7 ] ], All ], 1 ],
  True,
  TestID -> "WolframRicciScalar-Path7-All-keys-by-vertex"
]


EndTestSection[]
