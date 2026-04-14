Package["WolframInstitute`SyntheticInfrageometry`"]

(* Tools.wl — distance metrics, centrality, segment utilities, separating cycles *)
PackageExport[HausdorffDistance]
PackageExport[FrechetDistance]
PackageExport[MinimalSeparationDistance]
PackageExport[EmbeddingHausdorffDistance]
PackageExport[EmbeddingCircleDistance]
PackageExport[CentralElement]
PackageExport[PeripheralElement]
PackageExport[SegmentEndpoints]
PackageExport[SeparatingCycleQ]
PackageExport[FindSeparatingCycles]

(* Postulates.wl — primitive existence postulates *)
PackageExport[FindPoint]
PackageExport[FindSegment]
PackageExport[FindLine]
PackageExport[FindCircle]

(* Predicates.wl — geometric predicates *)
PackageExport[SegmentQ]
PackageExport[CircleQ]
PackageExport[LineQ]
PackageExport[IntersectQ]
PackageExport[ParallelQ]
PackageExport[SegmentLineAngle]

(* Coordinatization.wl — metric bases and coordinates *)
PackageExport[FindMetricBasis]
PackageExport[MetricBasisQ]
PackageExport[MetricCoordinates]
PackageExport[MetricBisector]

(* Scenes.wl — declarative scene engine *)
PackageExport[InfraScene]
PackageExport[FindInfraScene]
PackageExport[InfraInstance]
PackageExport[InfraPoint]
PackageExport[InfraSegment]
PackageExport[InfraLine]
PackageExport[InfraCircle]
PackageExport[InfraIntersection]
PackageExport[InfraIntersectionPoint]
PackageExport[InfraDistance]
PackageExport[InfraSegmentQ]
PackageExport[InfraCircleQ]
PackageExport[InfraLineQ]
PackageExport[InfraParallelQ]
PackageExport[InfraIntersectQ]

(* Shared type colors for all viewers *)
PackageScope[$InfraPointColor]
PackageScope[$InfraSegmentColor]
PackageScope[$InfraCircleColor]

(* Viewers.wl — interactive visualization tools *)
PackageExport[PointViewer]
PackageExport[SegmentViewer]
PackageExport[CircleViewer]

(* InteractiveViewers.wl — advanced interactive viewers *)
PackageExport[InfraSceneViewer]


ClearAll["WolframInstitute`SyntheticInfrageometry`**`*", "WolframInstitute`SyntheticInfrageometry`*"]
