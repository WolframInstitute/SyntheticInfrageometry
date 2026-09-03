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

<code>[FindInfraSegment]()[*g*, *a*, *b*]</code> gives the compact canonical form <code>[InfraSegment]()[*dag*]</code>, the geodesic interval DAG of all shortest paths from *a* to *b* in *g*.

<code>[FindInfraSegment]()[*g*, *a*, *b*, *n*]</code> gives one [InfraSegment]() with exactly *n* enumerated paths or `$Failed`; `UpTo[n]` up to *n*; `All` the fully enumerated bundle.

## Details & Options

A segment from *a* to *b* is a geodesic: a path whose length realizes $d(a,b)$.

In the Euclidean plane the segment between two points is unique. On a graph it is a **set** of paths, and uniqueness fails generically — a square grid has many geodesics between two vertices, because any interleaving of the horizontal and vertical steps is one.

The default return is the geodesic interval DAG rather than an enumeration, because the number of geodesics grows combinatorially: it is the compact object that represents all of them at once. Ask for `All` only when the individual paths are what you need.

[UniqueInfraSegmentQ]() tests whether the segment is unique, which is the graph-intrinsic shadow of Euclid's first postulate holding sharply.

Option `Properties` conjoins filters on the geodesic bundle: `{"EdgeMin", f}` and `{"EdgeMax", f}` for a user-supplied edge function *f*, and `"LongestPath"`. `"ShortestPath"` is implicit and is not a valid property. Option `Method` takes `"Exhaustive"` (default) or `"Greedy"`.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 1 | To draw a straight-line from any point to any point. |
| Hilbert | I.1, I.2 | Two distinct points determine one and only one line. |
| Tarski | A4, segment construction | Given points, a point can be laid off at a prescribed distance along the segment. |
| Birkhoff | Ruler postulate | The points of a line correspond to the reals, and the segment is a closed coordinate interval. |

## Basic Examples

Every geodesic between two vertices at distance 6, on the discretized plane, the square grid and the hexagonal tiling. The count is the sharpest difference between the substrates: the irregular mesh happens to have exactly one, the square grid has fifteen, the hexagonal tiling three.

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
       Text[name <> ": " <> ToString[Length @ First @ segs] <> " geodesics"]]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]]
```

The intensity in that picture is the multiplicity: an edge lying on many geodesics is drawn more strongly than one lying on few, so the bundle shows where the segment is concentrated.

The default form is the interval DAG, not a list of paths.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Large", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
  Head @ First @ FindInfraSegment[g, a, b]]
```

## Properties and Relations

The vertices covered by the geodesic bundle are exactly the metric interval between the endpoints, which is what [MetricInterval]() computes directly.

```wl
With[
  {g = InfraSubstrate["HexagonalTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  Sort @ Union @ Flatten @ First @ FindInfraSegment[g, a, b, All] === Sort @ MetricInterval[g, a, b]]
```
