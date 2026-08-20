$pacletDir = DirectoryName[DirectoryName[$InputFileName]];

(* FindBallHull / BallHullQ delegate to BallHull (WolframInstitute`Infrageometry`);
   load the sibling submodule source if present so the dev BallHull resolves
   instead of a stale installed paclet. *)
$infraDir = FileNameJoin[{DirectoryName[DirectoryName[$pacletDir]], "Infrageometry", "Infrageometry"}];
If[DirectoryQ[$infraDir], PacletDirectoryLoad[$infraDir]];
Needs["WolframInstitute`Infrageometry`"];

PacletDirectoryLoad[$pacletDir];
Needs["WolframInstitute`SyntheticInfrageometry`"];

$testDir = DirectoryName[$InputFileName];


(* Every .wlt in this directory, discovered rather than listed: the hand-kept
   list had gone stale in both directions -- five test files unrun, four named
   files gone. *)

Scan[
  file |-> ( Print["Running ", FileBaseName[file], "..."]; Print[TestReport[file]] ),
  FileNames["*.wlt", $testDir]
]
