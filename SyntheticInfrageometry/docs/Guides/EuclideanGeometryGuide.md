---
Template: Guide
Name: EuclideanGeometryGuide
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/guide/EuclideanGeometryGuide
Keywords: [Euclidean geometry, graph, geodesic, synthetic, Euclid, Tarski]
RelatedGuides: [SyntheticInfrageometryGuide, TarskiGeometryGuide, VisualizationGuide]
---

## Abstract

Euclidean geometry rebuilt inside a graph. The graph is all there is: no ambient space, no coordinates. Each Euclidean notion is redefined using graph properties alone, and the shortest-path metric is the layer these definitions sit on. Two things change from the plane. A construction returns a *set* of admissible answers rather than one, so uniqueness fails generically. And some objects fail to exist at all — a circle of a single radius is empty on a lattice, and a perpendicular bisector is empty at odd distance. Both are results, not defects.

## Functions

### Points

- `InfraPoint` a point given by a set of candidate vertices, optionally weighted
- `FindInfraPoint` the whole vertex set as one candidate family, narrowed by `"From"` and `"Distance"`
- `SelectInfraPoint` the same narrowing applied to a bundle you already hold
- `FindInfraMidpoint` vertices *m* with *d(a,m) = d(m,b) = d(a,b)/2*, over all geodesics
- `FindClosestInfraPoint` the vertices of a line nearest a given point
- `FindInfraReflection` the reflection of a point in a line
- `InfraSet` an arbitrary vertex subset, coercing any wrapper to its vertices

### Segments and lines

- `InfraSegment` the set of all geodesics between two vertices
- `FindInfraSegment` that set, as a compact interval DAG or enumerated
- `MetricInterval` the vertices lying on some geodesic between two points
- `InfraLine` an inclusion-maximal geodesic
- `FindInfraLine` the maximal geodesics through two points, or containing a segment
- `ExtendInfraSegment` Tarski's segment-construction step, laying off a segment beyond a point
- `InfraPath`, `FindInfraPath`, `ExtendInfraPath` walks, where revisiting a vertex is allowed
- `InfraRay`, `FindInfraRay` the pointed half of a maximal geodesic, which is how direction is expressed
- `InfraPolyline`, `FindInfraPolylineSubdivision` a walk cut into geodesic legs

### Circles, shells and balls

- `InfraShell`, `FindInfraShell` the level surface *{v : d(c,v) = r}*, a vertex set
- `InfraBall`, `FindInfraBall` the closed ball *{v : d(c,v) <= r}*, whose volume is an exact polynomial on a lattice
- `InfraCircle`, `FindInfraCircle` a simple cycle inside a level surface, empty at single radius on a lattice
- `InfraEllipse`, `FindInfraEllipse` the sum-of-distances band around two foci
- `InfraPlane`, `FindInfraBisectingHyperplane` the perpendicular bisector, empty at odd distance

### Polygons and triangles

- `InfraTriangle`, `FindInfraTriangle` three vertices with their connecting segments
- `InfraPolygon`, `FindInfraPolygon` a cyclic vertex sequence
- `FindInfraRegularPolygon` cycles whose diagonals all have prescribed lengths
- `CompleteInfraEquilateralTriangle` the apexes completing two given vertices to an equilateral triangle

### Measurement

- `InfraAngle` an angle at a vertex, by arclength on the punched-out boundary or by comparison triangle
- `InfraScalarProduct` the polar form of the metric at a base point
- `InfraCurvature` a local curvature from the growth of balls
- `ComparisonTriangle`, `InfraComparisonTriangle`, `CATInequalityQ` the CAT(k) comparison layer
- `TurningAngles`, `TotalCurvature`, `TurningNumber` curvature along a walk

### Predicates

- `InfraPathQ`, `InfraSegmentQ`, `InfraLineQ` a strict hierarchy: simple path, then geodesic, then maximal geodesic
- `UniqueInfraSegmentQ` whether the geodesic between two points is unique, so whether Euclid's first postulate holds sharply
- `InfraParallelQ`, `FindInfraParallel` constant distance to a line
- `InfraPerpendicularQ`, `FindInfraPerpendicular` right angles, with several inequivalent methods
- `InfraShellQ`, `InfraBallQ`, `InfraCircleQ`, `InfraEllipseQ`, `InfraPolygonQ`, `InfraTriangleQ` membership tests for each shape
- `SeparatesQ` whether a vertex set disconnects one point from another
- `InfraEqualQ` equality of two infra-objects, under a choice of set or multiset semantics

### Axioms

- `BetweennessQ` Tarski's *B(u,w,v)*: *d(u,w) + d(w,v) = d(u,v)*
- `EquidistanceQ` Tarski's congruence: *d(a,b) = d(c,d)*
- `TarskiAxiomQ` all eleven axioms tested at once on a graph
- `TarskiStructure`, `TarskiBetweennessTensor`, `TarskiEquidistanceClasses` the two primitives as explicit data
- `FindTarskiCounterexample` a witness to an axiom failing

### Substrates and drawing

- `InfraSceneHighlight` the one rendering primitive; intensity follows multiplicity
- `InfraScene`, `FindInfraScene`, `InfraGeometricStep` a construction stated as constraints and then solved
- `InfraSceneViewer` a construction stepped through interactively
- `$InfraPalette` the colour of each object head, in one place
