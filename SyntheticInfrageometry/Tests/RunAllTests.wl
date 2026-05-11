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

Print["Running EuclideanSpaceTests..."]
Print[TestReport[FileNameJoin[{$testDir, "EuclideanSpaceTests.wlt"}]]]

Print["Running PathSpaceTests..."]
Print[TestReport[FileNameJoin[{$testDir, "PathSpaceTests.wlt"}]]]

Print["Running MetricGeometryTests..."]
Print[TestReport[FileNameJoin[{$testDir, "MetricGeometryTests.wlt"}]]]

Print["Running LaplacianAlgebraTests..."]
Print[TestReport[FileNameJoin[{$testDir, "LaplacianAlgebraTests.wlt"}]]]

Print["Running ResistanceGeometryTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ResistanceGeometryTests.wlt"}]]]

Print["Running SpectralGeometryTests..."]
Print[TestReport[FileNameJoin[{$testDir, "SpectralGeometryTests.wlt"}]]]

Print["Running InfraPrimitivesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "InfraPrimitivesTests.wlt"}]]]

Print["Running InfraSceneTests..."]
Print[TestReport[FileNameJoin[{$testDir, "InfraSceneTests.wlt"}]]]

Print["Running CoordinatizationTests..."]
Print[TestReport[FileNameJoin[{$testDir, "CoordinatizationTests.wlt"}]]]

Print["Running InfraSceneVisualizationTests..."]
Print[TestReport[FileNameJoin[{$testDir, "InfraSceneVisualizationTests.wlt"}]]]

Print["Running InfraSceneInteractiveTests..."]
Print[TestReport[FileNameJoin[{$testDir, "InfraSceneInteractiveTests.wlt"}]]]

Print["Running ProjectiveGeometryTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ProjectiveGeometryTests.wlt"}]]]

Print["Running TarskiGeometryTests..."]
Print[TestReport[FileNameJoin[{$testDir, "TarskiGeometryTests.wlt"}]]]

Print["Running GraphEnumerationTests..."]
Print[TestReport[FileNameJoin[{$testDir, "GraphEnumerationTests.wlt"}]]]

Print["Running ExampleGraphsTests..."]
Print[TestReport[FileNameJoin[{$testDir, "ExampleGraphsTests.wlt"}]]]

Print["Running CurvaturesTests..."]
Print[TestReport[FileNameJoin[{$testDir, "CurvaturesTests.wlt"}]]]
