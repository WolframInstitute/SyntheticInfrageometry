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

Print["Running EuclideanConstructionsTests..."]
Print[TestReport[FileNameJoin[{$testDir, "EuclideanConstructionsTests.wlt"}]]]

Print["Running PathSpaceTests..."]
Print[TestReport[FileNameJoin[{$testDir, "PathSpaceTests.wlt"}]]]

Print["Running MetricAlgebraTests..."]
Print[TestReport[FileNameJoin[{$testDir, "MetricAlgebraTests.wlt"}]]]

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

Print["Running TarskiPostulatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TarskiPostulatesTests.wlt"}]]]

Print["Running TarskiPredicatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TarskiPredicatesTests.wlt"}]]]

Print["Running TarskiConstructionsTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TarskiConstructionsTests.wlt"}]]]

Print["Running TropicalPostulatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TropicalPostulatesTests.wlt"}]]]

Print["Running TropicalPredicatesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TropicalPredicatesTests.wlt"}]]]

Print["Running EnumerationTests..."]
Print[TestReport[FileNameJoin[{$testDir, "EnumerationTests.wlt"}]]]

Print["Running TessellationsTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TessellationsTests.wlt"}]]]

Print["Running CurvaturesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "CurvaturesTests.wlt"}]]]
