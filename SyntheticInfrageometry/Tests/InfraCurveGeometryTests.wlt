BeginTestSection["InfraCurveGeometry"]

geodesicGraph      = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
geodesicCycleGraph = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicCycleGraph;

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

(* ===== TurningAngles: walk graphs ===== *)

(* a path graph is read as its vertex sequence, a cycle graph as its closed walk *)
VerificationTest[
  { TurningAngles[GridGraph[{3, 3}], geodesicGraph @ {2, 1, 4}],
    TurningAngles[CycleGraph[6], geodesicCycleGraph @ Range[6]] },
  { {Pi - 2}, ConstantArray[Pi - 4, 6] },
  TestID -> "TurningAngles-walk-graphs"
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

VerificationTest[
  Simplify @ TotalCurvature[CycleGraph[6], geodesicCycleGraph @ Range[6]],
  6 (Pi - 4),
  TestID -> "TotalCurvature-cycle-graph"
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

(* ===== TurningAngles: polylines turn only at their knots ===== *)

(* Two-leg open polyline 1-2-3 then 3-6-9 on the 3x3 grid: one corner at
   the knot 3 with arms 1 and 9.  Radius = Min[d(3, 1), d(3, 9)] = 2; the
   punched-out ball B(3, 2) deletes {2, 3, 6}, leaving d(1, 9) = 4 in the
   residual graph.  Angle = 4 / 2 = 2, turning = Pi - 2. *)
VerificationTest[
  TurningAngles[GridGraph[{3, 3}], geodesicGraph /@ {{1, 2, 3}, {3, 6, 9}}],
  {Pi - 2},
  TestID -> "TurningAngles-polyline-open-two-legs-on-grid"
]

(* Single-leg polyline has no interior knot. *)
VerificationTest[
  TurningAngles[GridGraph[{3, 3}], { geodesicGraph @ {1, 2, 3} }],
  {},
  TestID -> "TurningAngles-polyline-single-leg-empty"
]

(* Closed four-leg polyline traversing the 3x3 grid boundary clockwise.
   Knot vertex list {1, 3, 9, 7, 1} is closed (first === last); each of the
   four corner triples is a 90-degree grid corner giving angle 2. *)
VerificationTest[
  TurningAngles[GridGraph[{3, 3}], geodesicGraph /@ {{1, 2, 3}, {3, 6, 9}, {9, 8, 7}, {7, 4, 1}}],
  ConstantArray[Pi - 2, 4],
  TestID -> "TurningAngles-polyline-closed-grid-boundary"
]

(* a family of polylines is read one at a time *)
VerificationTest[
  TurningAngles[GridGraph[{3, 3}], #] & /@ {
    geodesicGraph /@ {{1, 2, 3}, {3, 6, 9}},
    geodesicGraph /@ {{1, 4, 7}, {7, 8, 9}} },
  {{Pi - 2}, {Pi - 2}},
  TestID -> "TurningAngles-polyline-family"
]

(* the polyline reading agrees with the knot sequence read bare *)
VerificationTest[
  TurningAngles[GridGraph[{3, 3}], FindInfraPolylineSubdivision[GridGraph[{3, 3}], {1, 2, 3, 6, 9}]] ===
    TurningAngles[GridGraph[{3, 3}], {1, 3, 9}],
  True,
  TestID -> "TurningAngles-polyline-equals-knot-sequence"
]

EndTestSection[]
