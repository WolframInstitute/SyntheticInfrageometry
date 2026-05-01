BeginTestSection["TarskiPostulates"]

(* ===== FindTarskiSegmentExtension ===== *)

VerificationTest[
  FindTarskiSegmentExtension[PathGraph[Range[5]], 1, 2, 1, 2, All],
  {3},
  TestID -> "FindTarskiSegmentExtension-PathGraph-extends-by-one"
]

VerificationTest[
  FindTarskiSegmentExtension[PathGraph[Range[5]], 1, 2, 1, 3, All],
  {4},
  TestID -> "FindTarskiSegmentExtension-PathGraph-extends-by-two"
]

VerificationTest[
  FindTarskiSegmentExtension[PathGraph[Range[5]], 1, 2, 1, 5, All],
  {},
  TestID -> "FindTarskiSegmentExtension-PathGraph-no-room"
]

VerificationTest[
  FindTarskiSegmentExtension[PathGraph[Range[5]], 1, 2, 1, 5],
  $Failed,
  TestID -> "FindTarskiSegmentExtension-PathGraph-strict-fails"
]

VerificationTest[
  FindTarskiSegmentExtension[PathGraph[Range[5]], 1, 2, 1, 5, UpTo[1]],
  {},
  TestID -> "FindTarskiSegmentExtension-PathGraph-UpTo-empty-not-failed"
]

VerificationTest[
  Length @ FindTarskiSegmentExtension[CycleGraph[6], 1, 2, 1, 2, All] >= 1,
  True,
  TestID -> "FindTarskiSegmentExtension-CycleGraph-has-extension"
]

(* ===== FindTarskiCounterexample ===== *)

VerificationTest[
  FindTarskiCounterexample[PathGraph[Range[5]], TarskiCongruenceReflexivityQ, All],
  {},
  TestID -> "FindTarskiCounterexample-AlwaysTrue-empty"
]

VerificationTest[
  FindTarskiCounterexample[PathGraph[Range[5]], TarskiCongruenceReflexivityQ],
  $Failed,
  TestID -> "FindTarskiCounterexample-AlwaysTrue-strict-fails"
]

VerificationTest[
  Length @ FindTarskiCounterexample[PathGraph[Range[5]], TarskiSegmentConstructionQ, All] >= 1,
  True,
  TestID -> "FindTarskiCounterexample-SegmentConstruction-PathGraph-has-witness"
]

VerificationTest[
  Length @ FindTarskiCounterexample[PathGraph[Range[5]], TarskiSegmentConstructionQ, UpTo[3]] <= 3,
  True,
  TestID -> "FindTarskiCounterexample-SegmentConstruction-PathGraph-UpTo-respects-cap"
]

VerificationTest[
  Quiet @ FindTarskiCounterexample[PathGraph[Range[5]], TarskiContinuityQ, All],
  $Failed,
  TestID -> "FindTarskiCounterexample-Continuity-no-finite-witness"
]

VerificationTest[
  Length @ FindTarskiCounterexample[PetersenGraph[], TarskiInnerPaschQ, UpTo[1]] >= 1,
  True,
  TestID -> "FindTarskiCounterexample-InnerPasch-PetersenGraph-fails"
]

EndTestSection[]
