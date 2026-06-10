Package["WolframInstitute`SyntheticInfrageometry`"]

(* Tools.wl *)
PackageExport[InfraMeasure]

(* InfraPoint.wl *)
PackageExport[InfraPoint]
PackageExport[FindInfraPoint]
PackageExport[FindInfraMidpoint]
PackageExport[FindInfraGoldenSection]
PackageExport[FindInfraReflection]
PackageExport[CompleteInfraEquilateralTriangle]
PackageExport[FindInfraCommonPoint]
PackageExport[FindClosestInfraPoint]
PackageExport[SelectInfraPoint]
PackageExport[InfraReachableQ]

(* InfraSegment.wl *)
PackageExport[InfraSegment]
PackageExport[FindInfraSegment]
PackageExport[ExtendInfraSegment]
PackageExport[InfraPathQ]
PackageExport[InfraSegmentQ]
PackageExport[UniqueInfraSegmentQ]

(* InfraPath.wl *)
PackageExport[InfraPath]
PackageExport[FindInfraPath]
PackageExport[ExtendInfraPath]
PackageExport[ConcatenateInfraPath]

(* InfraLoop.wl *)
PackageExport[InfraLoop]

(* InfraString.wl *)
PackageExport[InfraString]

(* InfraLine.wl *)
PackageExport[FindInfraLine]
PackageExport[FindInfraParallel]
PackageExport[FindInfraPerpendicular]
PackageExport[FindInfraCommonLine]
PackageExport[InfraLineQ]
PackageExport[InfraParallelQ]
PackageExport[InfraPerpendicularQ]
PackageExport[PencilDirections]
PackageExport[PencilCardinality]
PackageExport[LineCount]
PackageExport[FindLineHull]
PackageExport[LineHullQ]
PackageExport[UniversalLineQ]

(* InfraLineStructure.wl *)
PackageExport[FindLineStructure]
PackageExport[InfraLineStructure]
PackageExport[ConsistentPathSystemQ]

(* InfraShell.wl *)
PackageExport[InfraShell]
PackageExport[FindInfraShell]
PackageExport[FindInfraOsculatingShell]
PackageExport[FindInfraShellCenter]
PackageExport[InfraShellQ]
PackageExport[SeparatesQ]

(* InfraEllipticShell.wl *)
PackageExport[InfraEllipticShell]
PackageExport[FindInfraEllipticShell]
PackageExport[InfraEllipticShellQ]

(* InfraQuadric.wl *)
PackageExport[FindInfraQuadric]

(* InfraBall.wl *)
PackageExport[InfraBall]
PackageExport[FindInfraBall]
PackageExport[InfraBallQ]
PackageExport[FindBallHull]
PackageExport[BallHullQ]

(* InfraCircle.wl *)
PackageExport[InfraCircle]
PackageExport[FindInfraCircle]
PackageExport[FindInfraCycle]
PackageExport[InfraCircleQ]

(* InfraPolygon.wl *)
PackageExport[InfraPolygon]
PackageExport[FindInfraPolygon]
PackageExport[FindInfraRegularPolygon]
PackageExport[InfraPolygonQ]
PackageExport[InfraRegularPolygonQ]

(* InfraTriangle.wl *)
PackageExport[InfraTriangle]
PackageExport[FindInfraTriangle]
PackageExport[InfraTriangleQ]

(* InfraEllipse.wl *)
PackageExport[InfraEllipse]
PackageExport[FindInfraEllipse]
PackageExport[InfraEllipseQ]

(* InfraPlane.wl *)
PackageExport[InfraPlane]
PackageExport[FindInfraBisectingHyperplane]

(* InfraRay.wl *)
PackageExport[InfraRay]
PackageExport[FindInfraRay]

(* InfraPolyline.wl *)
PackageExport[InfraPolyline]
PackageExport[FindInfraPolylineSubdivision]
PackageExport[InfraPolylineQ]

(* InfraRevolution.wl *)
PackageExport[InfraObject]
PackageExport[InfraRevolution]
PackageExport[FindInfraRevolution]
PackageExport[FindInfraCylinder]
PackageExport[FindInfraCone]
PackageExport[InfraRevolutionQ]

(* EuclideanSpace.wl *)
PackageExport[InfraScalarProduct]
PackageExport[FindInfraLinearCombination]
PackageExport[InfraAngle]

(* InfraCurveGeometry.wl *)
PackageExport[TurningAngles]
PackageExport[TotalCurvature]
PackageExport[TotalAbsoluteCurvature]
PackageExport[TurningNumber]

(* AlexandrovGeometry.wl *)
PackageExport[ComparisonTriangle]
PackageExport[InfraComparisonTriangle]
PackageExport[CATInequalityQ]
PackageExport[InfraCurvature]

(* PathSpace.wl *)
PackageExport[SelectInfraPath]
PackageExport[SelectInfraCycle]
PackageExport[EmbeddingClosest]
PackageExport[FindEmbeddingClosestPath]
PackageExport[GeodesicSprayGraph]
PackageExport[PathSubgraph]
PackageExport[FindForwardDeformation]

(* Homotopy.wl *)
PackageExport[InfraHomotopy]
PackageExport[FindInfraHomotopy]
PackageExport[FindInfraHomotopyRepresentative]
PackageExport[FindInfraHomotopyRepresentativeHomotopy]
PackageExport[HomotopicQ]
PackageExport[NullHomotopicQ]
PackageExport[HomotopyMoveType]
PackageExport[HomotopyMoveTypes]

(* MetricAlgebra.wl *)
PackageExport[MetricInterval]
PackageExport[GeodesicMultiplicity]
PackageExport[GeodesicMultiplicityMatrix]
PackageExport[MedianVertices]
PackageExport[FindSegmentHull]
PackageExport[SegmentHullQ]

(* InfraSet.wl *)
(* The Alexandrov-topology layer (InfraTopologicalSpace / InfraBallTopology /
   ContinuousMapQ) was relocated to the Infrageometry paclet as BallTopology /
   Topological* / ContinuousMapQ on bare vertex lists. InfraSet stays here -- it is
   a scene primitive. InfraBoundary / InfraInterior are the synthetic front-ends:
   they accept any Infra* object, dispatch over Method to GraphBoundary/GraphInterior
   ("Combinatorial") or Topological{Boundary,Interior} ("Alexandrov") from
   Infrageometry, and return an InfraSet. *)
PackageExport[InfraSet]
PackageExport[FindInfraEquidistantSet]
PackageExport[FindAdvancingInfraFront]
PackageExport[InfraBoundary]
PackageExport[InfraInterior]

(* Coordinatization.wl -- RadarCoordinates / ResistanceCoordinates relocated to
   the Infrageometry paclet; Synthetic adds InfraObject overloads to them and
   keeps FindInfraRadarBasis / InfraRadarBasisQ as deprecation aliases. *)
PackageExport[FindInfraRadarBasis]
PackageExport[InfraRadarBasisQ]
PackageExport[OrthogonalCoordinates]
PackageExport[FindInfraOrthogonalFrame]
PackageExport[FindInfraSpanningAxes]

(* TarskiGeometry.wl *)
PackageExport[BetweennessQ]
PackageExport[EquidistanceQ]
PackageExport[TarskiStructure]
PackageExport[TarskiBetweennessTensor]
PackageExport[TarskiEquidistanceClasses]
PackageExport[TarskiCongruenceReflexivityQ]
PackageExport[TarskiCongruenceTransitivityQ]
PackageExport[TarskiCongruenceIdentityQ]
PackageExport[TarskiSegmentConstructionQ]
PackageExport[TarskiFiveSegmentsQ]
PackageExport[TarskiBetweennessIdentityQ]
PackageExport[TarskiInnerPaschQ]
PackageExport[TarskiLowerDimensionQ]
PackageExport[TarskiUpperDimensionQ]
PackageExport[TarskiEuclidAxiomQ]
PackageExport[TarskiContinuityQ]
PackageExport[TarskiAxiomQ]
PackageExport[FindTarskiCounterexample]

(* ProjectiveGeometry.wl *)
PackageExport[SameDirectionQ]
PackageExport[CollinearQ]
PackageExport[ConcurrentQ]
PackageExport[UniquePencilQ]
PackageExport[UniqueCollinearQ]
PackageExport[UniqueConcurrentQ]
PackageExport[WhiteheadW1Q]
PackageExport[WhiteheadW2Q]
PackageExport[WhiteheadW3Q]
PackageExport[ProjectivePlaneGraphQ]

(* GraphEnumeration.wl *)
PackageExport[EnumerateGraphs]

(* Graph generation was relocated to the Infrageometry paclet (graph-generation layer):
   the single TessellationGraph generator (regular maps by {p, q}, uniform/Archimedean maps
   by vertex configuration, sized by n / {m, n} / a carrying group) plus PunchHole. Load it
   separately via Needs["WolframInstitute`Infrageometry`"]. *)

(* InfraScene.wl *)
PackageExport[InfraScene]
PackageExport[FindInfraScene]
PackageExport[InfraInstance]
PackageExport[InfraGeometricStep]
PackageExport[InfraLine]
PackageExport[InfraIntersection]
PackageExport[InfraUnion]
PackageExport[InfraDistance]
PackageExport[InfraPlaneQ]
PackageExport[InfraIntersectQ]

(* InfraSceneVisualization.wl *)
PackageExport[InfraSceneHighlight]
PackageExport[$InfraPointColor]
PackageExport[$InfraSegmentColor]
PackageExport[$InfraShellColor]
PackageExport[$InfraPlaneColor]
PackageExport[$InfraCircleColor]
PackageExport[$InfraRayColor]
PackageExport[$InfraPathColor]
PackageExport[$InfraLineColor]
PackageExport[$InfraObjectColor]

(* InfraSceneInteractive.wl *)
PackageExport[PointViewer]
PackageExport[SegmentViewer]
PackageExport[ShellViewer]
PackageExport[CircleViewer]
PackageExport[InfraSceneViewer]

(* InfraEquality.wl *)
PackageExport[InfraEqualQ]


ClearAll["WolframInstitute`SyntheticInfrageometry`**`*", "WolframInstitute`SyntheticInfrageometry`*"]
