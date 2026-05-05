BeginTestSection["Curvatures"]

(* ===== FormanRicci ===== *)

VerificationTest[
  Values @ FormanRicci[ CycleGraph[ 6 ] ],
  ConstantArray[ 0, 6 ],
  TestID -> "FormanRicci-CycleGraph6-zero"
]

VerificationTest[
  Values @ FormanRicci[ PathGraph[ Range[ 5 ] ] ],
  { 1, 0, 0, 1 },
  TestID -> "FormanRicci-PathGraph5-leaf-one-interior-zero"
]

VerificationTest[
  Values @ FormanRicci[ CompleteGraph[ 4 ], Method -> "Triangles" ],
  ConstantArray[ 4, 6 ],
  TestID -> "FormanRicci-CompleteGraph4-Triangles"
]

VerificationTest[
  Values @ FormanRicci[ CompleteGraph[ 4 ], Method -> "Simple" ],
  ConstantArray[ -2, 6 ],
  TestID -> "FormanRicci-CompleteGraph4-Simple"
]

VerificationTest[
  FormanRicci[ CycleGraph[ 6 ], Method -> "Bogus" ],
  $Failed,
  { FormanRicci::badmethod },
  TestID -> "FormanRicci-bad-method"
]


(* ===== OllivierRicci ===== *)

VerificationTest[
  Values @ OllivierRicci[ CompleteGraph[ 4 ] ],
  ConstantArray[ 2 / 3, 6 ],
  SameTest -> ( Max @ Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "OllivierRicci-CompleteGraph4"
]

VerificationTest[
  Values @ OllivierRicci[ PathGraph[ Range[ 5 ] ] ],
  ConstantArray[ 0, 4 ],
  SameTest -> ( Max @ Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "OllivierRicci-PathGraph5-zero"
]

VerificationTest[
  Values @ OllivierRicci[ CycleGraph[ 6 ] ],
  ConstantArray[ 0, 6 ],
  SameTest -> ( Max @ Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "OllivierRicci-CycleGraph6-zero"
]


(* ===== WolframRicci ===== *)

VerificationTest[
  WolframRicci[ CycleGraph[ 12 ], { 1, 3 }, "Dimension" -> 1 ][ 1 ],
  Mean[ { -9., -1.125, -1./3 } ],
  SameTest -> ( Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "WolframRicci-CycleGraph12-d1-interval-mean"
]

VerificationTest[
  WolframRicci[ CycleGraph[ 12 ], 2, "Dimension" -> 1 ][ 1 ],
  -1.125,
  SameTest -> ( Abs[ #1 - #2 ] < 10^-8 & ),
  TestID -> "WolframRicci-CycleGraph12-d1-single-radius"
]

VerificationTest[
  AssociationQ @ WolframRicci[ GridGraph[ { 5, 5 } ] ],
  True,
  TestID -> "WolframRicci-Grid5x5-default-association-by-vertex"
]

VerificationTest[
  Length @ WolframRicci[ GridGraph[ { 5, 5 } ] ],
  25,
  TestID -> "WolframRicci-Grid5x5-default-keys-all-vertices"
]

VerificationTest[
  KeyExistsQ[ WolframRicci[ PathGraph[ Range[ 7 ] ] ], 1 ],
  True,
  TestID -> "WolframRicci-Path7-keys-by-vertex"
]

VerificationTest[
  WolframRicci[ HypercubeGraph[ 3 ], { 5, 7 }, "Dimension" -> 3 ][ 1 ],
  Indeterminate,
  TestID -> "WolframRicci-Hypercube3-empty-window-indeterminate"
]


EndTestSection[]
