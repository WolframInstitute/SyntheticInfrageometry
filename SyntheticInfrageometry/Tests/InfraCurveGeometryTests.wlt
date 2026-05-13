BeginTestSection["InfraCurveGeometry"]

(* ===== TurningAngles: open paths ===== *)

VerificationTest[
  TurningAngles[CycleGraph[6], {1, 2, 3}],
  {Pi - 4},
  TestID -> "TurningAngles-open-single-corner-on-C6"
]

VerificationTest[
  TurningAngles[GridGraph[{3, 3}], {2, 1, 4}],
  {Pi - 2},
  TestID -> "TurningAngles-open-grid-corner"
]

VerificationTest[
  TurningAngles[CycleGraph[6], {1, 2}],
  {},
  TestID -> "TurningAngles-empty-on-single-edge"
]

(* ===== TurningAngles: closed cycles ===== *)

VerificationTest[
  TurningAngles[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}],
  ConstantArray[Pi - 4, 6],
  TestID -> "TurningAngles-closed-C6-six-equal-corners"
]

VerificationTest[
  TurningAngles[CycleGraph[3], {1, 2, 3, 1}],
  ConstantArray[Pi - 1, 3],
  TestID -> "TurningAngles-closed-C3-three-equal-corners"
]

VerificationTest[
  TurningAngles[GridGraph[{3, 3}], {1, 2, 3, 6, 9, 8, 7, 4, 1}],
  {Pi - 4, Pi - 2, Pi - 4, Pi - 2, Pi - 4, Pi - 2, Pi - 4, Pi - 2},
  TestID -> "TurningAngles-closed-grid-3x3-boundary-alternates"
]

(* ===== TotalCurvature ===== *)

VerificationTest[
  Simplify @ TotalCurvature[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}],
  6 (Pi - 4),
  TestID -> "TotalCurvature-closed-C6"
]

VerificationTest[
  Simplify @ TotalCurvature[CycleGraph[3], {1, 2, 3, 1}],
  3 (Pi - 1),
  TestID -> "TotalCurvature-closed-C3"
]

VerificationTest[
  Simplify[ TotalCurvature[GridGraph[{3, 3}], {1, 2, 3, 6, 9, 8, 7, 4, 1}] - (8 Pi - 24) ],
  0,
  TestID -> "TotalCurvature-closed-grid-3x3-boundary"
]

VerificationTest[
  Simplify @ TotalCurvature[CycleGraph[6], {1, 2, 3}],
  Pi - 4,
  TestID -> "TotalCurvature-open-single-corner"
]

(* ===== TotalAbsoluteCurvature ===== *)

VerificationTest[
  Simplify[ TotalAbsoluteCurvature[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}] - 6 (4 - Pi) ],
  0,
  TestID -> "TotalAbsoluteCurvature-C6-equals-negation-of-total"
]

VerificationTest[
  Simplify @ TotalAbsoluteCurvature[CycleGraph[3], {1, 2, 3, 1}],
  3 (Pi - 1),
  TestID -> "TotalAbsoluteCurvature-C3-equals-total-when-all-positive"
]

VerificationTest[
  TotalAbsoluteCurvature[CycleGraph[6], {1, 2}],
  0,
  TestID -> "TotalAbsoluteCurvature-zero-on-edge"
]

(* ===== TurningNumber ===== *)

VerificationTest[
  Simplify @ TurningNumber[CycleGraph[6], {1, 2, 3, 4, 5, 6, 1}],
  (6 (Pi - 4)) / (2 Pi),
  TestID -> "TurningNumber-C6-closed"
]

VerificationTest[
  Simplify @ TurningNumber[CycleGraph[3], {1, 2, 3, 1}],
  (3 (Pi - 1)) / (2 Pi),
  TestID -> "TurningNumber-C3-closed"
]

VerificationTest[
  Simplify @ ( TurningNumber[GridGraph[{3, 3}], {1, 2, 3, 6, 9, 8, 7, 4, 1}] - (8 Pi - 24) / (2 Pi) ),
  0,
  TestID -> "TurningNumber-grid-boundary-matches-total-over-2-pi"
]

EndTestSection[]
