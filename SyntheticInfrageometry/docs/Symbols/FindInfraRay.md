---
Template: Symbol
Name: FindInfraRay
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraRay
Keywords: [ray, half-line, direction, Euclid Postulate 2]
SeeAlso: [InfraRay, FindInfraLine, FindInfraSegment, SameDirectionQ, PencilDirections]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraRay]()[*g*, *O*, *v*]</code> gives one pointed half-line from *O* in *v*'s direction as an [InfraRay]() wrapper.

<code>[FindInfraRay]()[*g*, *O*, *v*, *n*]</code> gives *n* realisations or `$Failed`; `UpTo[n]` up to *n*; `All` all of them.

## Details & Options

A ray from *O* through *v* is the pointed half of a maximal geodesic line: the vertex sequence $O, \ldots, v, \ldots, w$ with *w* an inextensible endpoint. The first vertex of every realisation is the origin, which is what makes it a ray rather than a line.

Rays are how direction is expressed without a vector space. There is no tangent space on a graph, so "the direction from *O* towards *v*" is not a vector but the *family* of rays from *O* containing *v* — and like every other family here it is large: on a square grid a single origin and target admit many maximal continuations. [SameDirectionQ]() compares two rays, and [PencilDirections]() counts the directions at a vertex, which is the graph's stand-in for the circle of directions.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Postulate 2 | To produce a finite straight-line continuously; the ray is the one-sided production. |
| Hilbert | Group II, order | Rays are defined from betweenness on a line. |
| Tarski | A4, segment construction | Laying off a segment from a point in a given direction. |
| Birkhoff | Ruler postulate | A half-open coordinate interval on a line. |

## Basic Examples

How many maximal rays leave a vertex through a given neighbour-direction, on each substrate.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Length @ First @ FindInfraRay[g, a, b, All]],
   {name, {"PlanePatch", "SquarePatch", "HexagonalPatch"}}]
```

Three rays from the centre of each substrate. Each starts at the origin and runs to the far edge of the patch.

```wl
Row[Table[
   With[
     {g = InfraSubstrate[name, "Medium", "Gray", "KeepCoordinates" -> True]},
     {a = First @ GraphCenter[g]},
     {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
     Labeled[
       InfraSceneHighlight[g,
         {FindInfraRay[g, a, b, UpTo[3]] -> $InfraRayColor,
          InfraPoint[a] -> $InfraPointColor},
         "PointSizeRange" -> 15,
         VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
         ImageSize -> 250],
       Text[name]]],
   {name, {"PlanePatch", "SquarePatch", "HexagonalPatch"}}]]
```

## Properties and Relations

Every ray begins at its origin, and its vertex sequence is a geodesic, so the distance along it grows by one at each step.

```wl
With[
  {g = InfraSubstrate["HexagonalPatch", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {ray = First @ First @ FindInfraRay[g, a, b, 1]},
  {First[ray] === a, GraphDistance[g, a, #] & /@ ray === Range[0, Length[ray] - 1]}]
```
