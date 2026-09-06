---
Template: Symbol
Name: FindInfraSegment
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraSegment
Keywords: [segment, geodesic, shortest path, Euclid Postulate 1]
SeeAlso: [InfraSegment, FindInfraLine, FindInfraMidpoint, UniqueInfraSegmentQ, MetricInterval]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraSegment]()[*g*, *a*, *b*]</code> gives one geodesic — a shortest path — from *a* to *b* in *g*, as an [InfraSegment]() wrapper.

<code>[FindInfraSegment]()[*g*, *a*, *b*, *n*]</code> gives exactly *n* geodesics or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives the whole class as the geodesic interval DAG <code>[InfraSegment]()[*dag*]</code>.

## Details & Options

A segment from *a* to *b* is a geodesic: a path whose length realizes $d(a,b)$.

In the Euclidean plane the segment between two points is unique. On a graph it is a **set** of paths, and uniqueness fails generically — a square grid has many geodesics between two vertices, because any interleaving of the horizontal and vertical steps is one.

`All` returns the geodesic interval DAG rather than an enumeration, because the number of geodesics grows combinatorially: it is the compact object that represents all of them at once, and `["Multiplicity"]`, `["Measure"]` and `["Length"]` are read off it without enumerating a path. A bounded count enumerates that many geodesics, and the count-less call is one, deterministic.

[UniqueInfraSegmentQ]() tests whether the segment is unique, which is the graph-intrinsic shadow of Euclid's first postulate holding sharply.

There is no `Properties` option: the symbol is the whole geodesic class, and a rule narrowing it is a local law at an infra-scale, which is a `FindInfraGeodesic` call. Option `Method` takes `Automatic` (default), `"Exhaustive"`, `{"Exhaustive", "Pruning" -> spec}`, `"Greedy"` or `"RandomGreedy"`. `Automatic` resolves by the count — `All` to `"Exhaustive"`, the DAG; a bounded or absent count to `"Greedy"`, the DAG descended in candidate order — and the class is the same under every value. `"RandomGreedy"` descends in random order, seeded by an ambient `SeedRandom`; `"Pruning"` is accepted and inert, the DAG having no frontier to cap.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 1 | To draw a straight-line from any point to any point. |
| Hilbert | I.1, I.2 | Two distinct points determine one and only one line. |
| Tarski | A4, segment construction | Given points, a point can be laid off at a prescribed distance along the segment. |
| Birkhoff | Ruler postulate | The points of a line correspond to the reals, and the segment is a closed coordinate interval. |

## Basic Examples

Every geodesic between two vertices at distance 6, on the discretized plane, the square grid and the hexagonal tiling. The count is the sharpest difference between the substrates: the irregular mesh has four, the square grid fifteen, the hexagonal tiling three.

```wl
Row[Table[
   With[
     {g = InfraSubstrate[name, "Large", "Gray", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
     {segs = FindInfraSegment[g, a, b, All]},
     Labeled[
       InfraSceneHighlight[g,
         {segs -> $InfraSegmentColor, InfraSet[{a, b}] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[name <> ": " <> ToString[segs["Multiplicity"]] <> " geodesics"]]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]]
```

The intensity in that picture is the multiplicity: an edge lying on many geodesics is drawn more strongly than one lying on few, so the bundle shows where the segment is concentrated.

`All` is the interval DAG, not a list of paths; the count-less call is one path.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Large", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
  {Head @ First @ FindInfraSegment[g, a, b, All], Length @ FindInfraSegment[g, a, b]["Realizations"]}]
```

## Properties and Relations

The vertices covered by the geodesic bundle are exactly the metric interval between the endpoints, which is what [MetricInterval]() computes directly.

```wl
With[
  {g = InfraSubstrate["HexagonalTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  Sort @ FindInfraSegment[g, a, b, All]["Vertices"] === Sort @ MetricInterval[g, a, b]]
```
