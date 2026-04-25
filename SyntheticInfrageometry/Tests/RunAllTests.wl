$pacletDir = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[$pacletDir];
Needs["WolframInstitute`SyntheticInfrageometry`"];

$testDir = DirectoryName[$InputFileName];

Print["Running ToolsTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ToolsTests.wlt"}]]]

Print["Running EuclideanPostulatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "EuclideanPostulatesTests.wlt"}]]]

Print["Running EuclideanPredicatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "EuclideanPredicatesTests.wlt"}]]]

Print["Running ScenesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ScenesTests.wlt"}]]]

Print["Running CoordinatizationTests..."]
Print[TestReport[FileNameJoin[{$testDir, "CoordinatizationTests.wlt"}]]]

Print["Running ViewersTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ViewersTests.wlt"}]]]

Print["Running ProjectivePostulatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ProjectivePostulatesTests.wlt"}]]]

Print["Running ProjectivePredicatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ProjectivePredicatesTests.wlt"}]]]
