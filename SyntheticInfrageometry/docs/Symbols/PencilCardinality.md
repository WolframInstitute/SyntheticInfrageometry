---
Template: Symbol
Name: PencilCardinality
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/PencilCardinality
Keywords: [pencil, ray, direction, count]
SeeAlso: [PencilDirections, FindInfraRay, InfraRay, LineCount, UniquePencilQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[PencilCardinality]()[*g*, *O*]</code> gives the number of rays from *O* in *g*.

## Details & Options

The pencil at *O* is the set of rays from *O* ([PencilDirections]()). A ray leaves *O* through exactly one neighbour, so its size is the sum over the neighbours *w* of the path count of the ray pool <code>[FindInfraRay]()[*g*, *O*, *w*, All]</code> — read off each pool's `["Multiplicity"]` by dynamic programming, never by enumerating rays.

The count is at least the degree of *O*, with equality when every direction continues uniquely to a unique end. Its excess over the degree measures how much the graph branches ahead of *O*; on a lattice it is large.

## Basic Examples

Two rays leave the middle of a path.

```wl
PencilCardinality[PathGraph[Range[7]], 4]
```

Two leave a vertex of the 6-cycle — both end at the antipode — and eight leave the centre of a 3 × 3 grid.

```wl
{PencilCardinality[CycleGraph[6], 1], PencilCardinality[GridGraph[{3, 3}], 5]}
```

On the 3-cube every geodesic from a vertex runs to the antipode, so the pencil has 3! = 6 rays.

```wl
PencilCardinality[HypercubeGraph[3], 1]
```

The pencil at the centre of each substrate, counted without enumerating a ray.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     PencilCardinality[g, First @ GraphCenter[g]]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

## Properties and Relations

The count equals the length of the enumerated pencil.

```wl
With[
  {g = GridGraph[{3, 3}]},
  {PencilCardinality[g, 5], Length @ PencilDirections[g, 5]}]
```

It is at least the degree.

```wl
With[
  {g = PetersenGraph[]},
  {PencilCardinality[g, 1], VertexDegree[g, 1]}]
```
