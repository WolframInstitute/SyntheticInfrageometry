$pacletDir = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[$pacletDir];
Needs["WolframInstitute`SyntheticInfrageometry`"];

$testDir = DirectoryName[$InputFileName];

Print["Running ToolsTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ToolsTests.wlt"}]]]

Print["Running PostulatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "PostulatesTests.wlt"}]]]

Print["Running PredicatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "PredicatesTests.wlt"}]]]

Print["Running ScenesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ScenesTests.wlt"}]]]
