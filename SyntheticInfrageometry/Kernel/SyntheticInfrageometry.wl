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


ClearAll["WolframInstitute`SyntheticInfrageometry`**`*", "WolframInstitute`SyntheticInfrageometry`*"]
