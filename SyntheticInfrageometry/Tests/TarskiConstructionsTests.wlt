BeginTestSection["TarskiConstructions"]

(* ===== FindTarskiMidpoint ===== *)

VerificationTest[
  FindTarskiMidpoint[PathGraph[Range[5]], 1, 5, All],
  InfraPoint[{3}],
  TestID -> "FindTarskiMidpoint-PathGraph-even-distance-hit"
]

VerificationTest[
  FindTarskiMidpoint[PathGraph[Range[5]], 1, 4, All],
  InfraPoint[{}],
  TestID -> "FindTarskiMidpoint-PathGraph-odd-distance-miss"
]

VerificationTest[
  FindTarskiMidpoint[PathGraph[Range[5]], 1, 4],
  $Failed,
  TestID -> "FindTarskiMidpoint-PathGraph-odd-distance-strict-fails"
]

VerificationTest[
  FindTarskiMidpoint[PathGraph[Range[5]], 1, 4, UpTo[1]],
  InfraPoint[{}],
  TestID -> "FindTarskiMidpoint-PathGraph-odd-distance-UpTo-empty"
]

(* CycleGraph[4] with antipodes 1 and 3: midpoints are 2 and 4 *)
VerificationTest[
  Sort @ FindTarskiMidpoint[CycleGraph[4], 1, 3, All]["Realisations"],
  {2, 4},
  TestID -> "FindTarskiMidpoint-CycleGraph4-antipodes-two-midpoints"
]

(* Calling-triple cap *)
VerificationTest[
  FindTarskiMidpoint[CycleGraph[4], 1, 3, UpTo[1]]["Length"],
  1,
  TestID -> "FindTarskiMidpoint-CycleGraph4-UpTo-caps"
]

(* ===== FindTarskiReflection ===== *)

(* On PathGraph[Range[5]], reflecting 1 through 2 gives 3 *)
VerificationTest[
  FindTarskiReflection[PathGraph[Range[5]], 1, 2, All],
  InfraPoint[{3}],
  TestID -> "FindTarskiReflection-PathGraph-adjacent"
]

(* Reflecting 1 through 3 gives 5 *)
VerificationTest[
  FindTarskiReflection[PathGraph[Range[5]], 1, 3, All],
  InfraPoint[{5}],
  TestID -> "FindTarskiReflection-PathGraph-distance-two"
]

(* Reflecting through a point too close to the boundary returns {} *)
VerificationTest[
  FindTarskiReflection[PathGraph[Range[5]], 1, 4, All],
  InfraPoint[{}],
  TestID -> "FindTarskiReflection-PathGraph-no-room"
]

VerificationTest[
  FindTarskiReflection[PathGraph[Range[5]], 1, 4],
  $Failed,
  TestID -> "FindTarskiReflection-PathGraph-no-room-strict-fails"
]

(* On CycleGraph[6], 1 through 2 reflects to 3 *)
VerificationTest[
  MemberQ[FindTarskiReflection[CycleGraph[6], 1, 2, All]["Realisations"], 3],
  True,
  TestID -> "FindTarskiReflection-CycleGraph6-includes-3"
]

(* On HypercubeGraph[3], reflection through an adjacent vertex is multi-valued *)
VerificationTest[
  FindTarskiReflection[HypercubeGraph[3], 1, 2, All]["Length"] >= 2,
  True,
  TestID -> "FindTarskiReflection-HypercubeGraph-multi-valued"
]

EndTestSection[]
